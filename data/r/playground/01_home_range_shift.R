# 01_home_range_shift.R
# Habitat Selection & Home Range Shift Analysis for Elephants E3 & E4
# Stages: Pre-Removal, Interim, Post-Removal


suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate)
    library(sf)
    library(terra)
    library(amt)
    library(moveHMM)
    library(survival)
    library(viridis)
    library(patchwork)
})

# ==============================================================================
# 1. Config & Paths
# ==============================================================================

root_dir <- dirname(dirname(getwd())) # Assuming running from BioHabs/playground
clean_dir <- file.path(root_dir, "BioHabs", "data", "clean")
env_dir_A <- file.path(root_dir, "BioHabs", "data", "envi", "A", "stack")
env_dir_B <- file.path(root_dir, "BioHabs", "data", "envi", "B", "stack")
dist_dir <- file.path(root_dir, "BioHabs", "playground", "data", "dist_rasters")
out_dir <- file.path(root_dir, "BioHabs", "playground", "E3_E4_HabSel")

if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Time Periods
periods <- list(
    pre = interval(dmy("04-08-2022"), dmy("09-12-2023")),
    interim = interval(dmy("10-12-2023"), dmy("08-01-2024")),
    post = interval(dmy("09-01-2024"), dmy("14-07-2025"))
)

stack_list <- list(
    pre = file.path(env_dir_A, "stack.tif"),
    interim = file.path(env_dir_B, "stack.tif"),
    post = file.path(env_dir_B, "stack.tif")
)

# ==============================================================================
# 2. Data Loading & Prep
# ==============================================================================

csv_files <- list.files(clean_dir, pattern = "^E[34][AB]?\\.csv$", full.names = TRUE, recursive = TRUE)

ele_data <- map_df(csv_files, function(f) {
  read_csv(f, show_col_types = FALSE, col_types = cols(collar_id = col_character())) %>%
    mutate(
      Elephant = str_extract(basename(f), "E[0-9]+"),
      timestamp = dmy_hm(timestamp)
    ) %>%
    select(Elephant, timestamp, lon, lat, collar_id)
}) %>%
  filter(!is.na(timestamp), !is.na(lon), !is.na(lat)) %>%
  distinct(Elephant, timestamp, .keep_all = TRUE)

ele_data <- ele_data %>%
    mutate(
        Stage = case_when(
            timestamp %within% periods$pre ~ "pre",
            timestamp %within% periods$interim ~ "interim",
            timestamp %within% periods$post ~ "post",
            TRUE ~ NA_character_
        )
    ) %>%
    filter(!is.na(Stage)) %>%
    arrange(Elephant, timestamp)

# ==============================================================================
# 3. Analysis Loop
# ==============================================================================

models_list <- list()
micro_hab_list <- list()

# Define Plot Helper Function globally (or here)
plot_dist <- function(variable, data, params, dist_name, unit, state_names) {
    x_grid <- seq(min(data[[variable]], na.rm=TRUE), max(data[[variable]], na.rm=TRUE), length.out=200)
    dens_df <- map_df(1:2, function(k) {
        st_name <- state_names[k]
        if(dist_name == "gamma") {
            mu <- params$step[1, k]
            sigma <- params$step[2, k]
            shape <- (mu^2) / (sigma^2)
            scale <- (sigma^2) / mu
            y <- dgamma(x_grid, shape=shape, scale=scale)
        } else if(dist_name == "vonmises") {
            mu <- params$angle[1, k]
            kappa <- params$angle[2, k]
            y <- exp(kappa * cos(x_grid - mu)) / (2 * pi * besselI(kappa, 0))
        }
        tibble(x = x_grid, y = y, State = st_name)
    })
    
    ggplot() +
        geom_histogram(data = data, aes(x = .data[[variable]], y = after_stat(density), fill = State), 
                       alpha = 0.4, bins = 30, position="identity") +
        geom_line(data = dens_df, aes(x = x, y = y, color = State), linewidth = 1) +
        scale_fill_manual(name = "State", values = c("Foraging"="#E69F00", "Movement"="#56B4E9")) +
        scale_color_manual(name = "State", values = c("Foraging"="#E69F00", "Movement"="#56B4E9")) +
        labs(title = paste(tools::toTitleCase(gsub("_", " ", variable)), "-", tag),
             x = paste0(tools::toTitleCase(gsub("_", " ", variable)), " (", unit, ")"),
             y = "Density") +
        theme_minimal()
}

jobs <- expand.grid(Elephant = c("E3", "E4"), Stage = c("pre", "post", "interim"))

for(i in 1:nrow(jobs)) {
    job <- jobs[i,]
    ele_id <- job$Elephant
    stg <- job$Stage
    tag <- paste(ele_id, stg, sep = "_")
    
    cat("\n------------------------------------------------------------\n")
    cat("Processing:", tag, "\n")
    
    # 3.1 Subset Data
    dat_sub <- ele_data %>% filter(Elephant == ele_id, Stage == stg)
    
    if(nrow(dat_sub) < 100) {
        cat("Skipping", tag, "- Not enough data (n =", nrow(dat_sub), ")\n")
        next
    }
    
    # 3.2 Load Raster
    r_path <- stack_list[[stg]]
    if(!file.exists(r_path)) {
        cat("Raster not found for", stg, "\n")
        next
    }
    r_stack <- terra::rast(r_path)
    
    # Load Distances
    if(dir.exists(dist_dir)) {
        dist_files <- list.files(dist_dir, pattern = "dist_.*\\.tif$", full.names = TRUE)
        if(length(dist_files) > 0) {
             cat("Loading", length(dist_files), "distance layers... ")
             for(dfile in dist_files) {
                 r_d <- rast(dfile)
                 # Resample to match stack if needed
                 if(!compareGeom(r_stack, r_d, stopOnError = FALSE, ext=TRUE, rowcol=TRUE)) {
                     r_d <- resample(r_d, r_stack)
                 }
                 add(r_stack) <- r_d
             }
             cat("Done.\n")
        }
    }
    names(r_stack) <- make.names(names(r_stack), unique = TRUE)
    
    # 3.3 Create Track & HMM (Strict Metric/UTM Logic)
    
    # 1. Project Raw Data to UTM (Zone 35S) immediately
    # This ensures all step calculations (HMM and iSSA) are in Meters
    dat_sf <- st_as_sf(dat_sub, coords = c("lon", "lat"), crs = 4326) %>%
        st_transform(32735) # UTM Zone 35S
    
    coords <- st_coordinates(dat_sf)
    dat_sub$x <- coords[,1]
    dat_sub$y <- coords[,2]
    
    # 2. Resample Track (amt)
    # Use 60 min as primary trial (User: "clear cut difference" usually robust at 1h)
    trk_raw <- make_track(dat_sub, x, y, timestamp, crs = 32735)
    trk_res <- track_resample(trk_raw, rate = minutes(30), tolerance = minutes(10))
    cadence <- "30 min"
    
    if(nrow(trk_res) < 200) {
        cat("Insufficient points at 30 min, trying 60 min...\n")
        trk_res <- track_resample(trk_raw, rate = minutes(60), tolerance = minutes(15))
        cadence <- "60 min"
    }
    cat("Using cadence:", cadence, "Rows:", nrow(trk_res), "\n")
    
    if(nrow(trk_res) < 50) next
    
    # 3. Explicitly Handle Bursts & IDs for HMM
    if(!"burst_" %in% names(trk_res)) {
        trk_res <- filter_min_n_burst(trk_res, min_n = 3)
    }
    trk_res$row_id <- 1:nrow(trk_res) # For robust mapping back
    
    # 4. Prepare Data for moveHMM (Meters)
    # type="UTM" assumes Euclidean distance (Meters) given our input
    hmm_data <- data.frame(
        ID = paste0(ele_id, "_", trk_res$burst_), 
        x = trk_res$x_, 
        y = trk_res$y_,
        row_id = trk_res$row_id
    )
    
    # prepData
    hmm_prep <- prepData(hmm_data, type = "UTM", coordNames = c("x", "y"))
    
    # Fix Zero Steps (epsilon)
    hmm_prep$step[hmm_prep$step == 0] <- 1e-5
    
    # 5. Fit HMM (2-State)
    # Init Params based on data summary (Meters)
    steps_vec <- hmm_prep$step[!is.na(hmm_prep$step)]
    if(length(steps_vec) < 10) next
    
    # Expect: State 1 (Foraging) ~ Short (e.g., 50m), State 2 (Movement) ~ Long (e.g., 500m)
    # Distributions: Gamma (Step), VonMises (Angle)
    mu0 <- c(quantile(steps_vec, 0.25, na.rm=TRUE), quantile(steps_vec, 0.90, na.rm=TRUE))
    sd0 <- c(sd(steps_vec)/2, sd(steps_vec))
    angle0 <- c(0, 0, 0.5, 5) # Concentration 0.5 (random), 5 (directed)
    
    # Ensure starting mus are distinct to avoid convergence to single state
    if(mu0[1] == mu0[2]) mu0 <- c(mean(steps_vec)*0.5, mean(steps_vec)*2)
    
    cat("Init HMM (Meters) - Mu:", round(mu0, 1), "\n")
    
    m_hmm <- tryCatch({
        fitHMM(hmm_prep, nbStates = 2, stepPar0 = c(mu0, sd0), anglePar0 = angle0)
    }, error = function(e) {
        cat("HMM Error:", e$message, "\n")
        return(NULL)
    })
    
    if(is.null(m_hmm)) next
    
    # 6. Decode & Assign States
    states <- viterbi(m_hmm)
    
    mus <- m_hmm$mle$stepPar[1,]
    if(mus[1] < mus[2]) {
        state_labels <- c("Foraging", "Movement")
    } else {
        state_labels <- c("Movement", "Foraging") 
    }
    
    hmm_prep$State <- factor(states, levels = 1:2, labels = state_labels)
    
    # 7. Map States back to Track (Robust Join)
    # Using match on row_id to ensure strict alignment
    matches <- match(trk_res$row_id, hmm_prep$row_id)
    trk_res$State <- hmm_prep$State[matches]
    
    # Define state_ column definitely for downstream
    trk_res$state_ <- trk_res$State
    
    # --- VISUALIZATION: HMM Distributions ---
    # (Plot functions would go here, adapted to use hmm_prep with State)
    # Re-using previous 'plot_dist' logic but ensuring it uses the new hmm_prep
    par_list <- list(step = m_hmm$mle$stepPar, angle = m_hmm$mle$anglePar)
    p_sl <- plot_dist("step", hmm_prep, par_list, "gamma", "m", state_labels)
    p_ta <- plot_dist("angle", hmm_prep, par_list, "vonmises", "rad", state_labels)
    
    p_comb <- p_sl + p_ta + plot_layout(guides = "collect")
    ggsave(file.path(out_dir, paste0("HMM_Distributions_", tag, ".png")), p_comb, width = 10, height = 5)
    
    # --- VISUALIZATION: Trajectory Map ---
    p_traj <- ggplot(trk_res, aes(x=x_, y=y_, color=State)) +
        geom_path(alpha=0.3) +
        geom_point(size=0.5) +
        scale_color_manual(values = c("Foraging"="#E69F00", "Movement"="#56B4E9")) +
        labs(title = paste("Trajectory -", tag)) +
        coord_equal() +
        theme_minimal()
    ggsave(file.path(out_dir, paste0("HMM_Map_", tag, ".png")), p_traj, width = 8, height = 6)
    
    # 3.4 iSSA Prep and Covariate Extraction
    # Make track from trk_res (which is already UTM 32735 and has state_)
    trk <- make_track(trk_res, x_, y_, t_, crs = 32735, state_ = state_, burst_ = burst_)
    
    # Generate Steps (Meters)
    steps <- steps_by_burst(trk, keep_cols = "start")
    
    # Filter valid steps (consistent with HMM support)
    steps <- steps %>% filter(sl_ > 1)
    
    if(nrow(steps) < 50) { cat("Not enough steps for iSSA\n"); next }
    
    # Generate Random Steps
    rnd <- steps %>% random_steps(n_control = 15)
    
    # Extract Covariates (Robust Dynamic CRS)
    # Using endpoints (x2_, y2_)
    rnd_sf <- st_as_sf(rnd, coords = c("x2_", "y2_"), crs = 32735)
    
    # Project points to match the raster stack's CRS exactly
    rnd_prj <- st_transform(rnd_sf, st_crs(r_stack))
    
    # DEBUG: Check Extents
    e_rst <- ext(r_stack)
    e_pts <- ext(vect(rnd_prj))
    cat("Raster Extent:", as.character(e_rst), "\n")
    cat("Points Extent:", as.character(e_pts), "\n")
    
    # Extract using terra::extract with matrices (fastest)
    coords_prj <- st_coordinates(rnd_prj)
    cov_vals <- terra::extract(r_stack, coords_prj)
    
    if("ID" %in% names(cov_vals)) cov_vals$ID <- NULL
    
    # Bind Raster Data
    rnd_cov <- bind_cols(rnd, cov_vals)
    
    # --- VEGETATION CLASS ANALYSIS (Using Shapefiles) ---
    # Intersect Used Steps (case_=1) with Veg Shapefiles to get Class
    cat("Analyzing Vegetation Class Usage...\n")
    
    veg_dir <- "d:/PhD/Projects/SDM_projects/BTEH/SDM_ele/BTEH/BTEH/BioHabs/data/envi/raw/Veg class"
    veg_shps <- list.files(veg_dir, pattern="\\.shp$", full.names=TRUE)
    
    # Load all veg shapes into one SF (if not already loaded)
    # This might be slow if done every loop iteration. Ideally done once outside.
    # We will do it locally here for robustness or check if exists.
    
    # Filter for non-xml
    veg_shps <- veg_shps[!grepl("xml", veg_shps)]
    
    veg_poly_list <- lapply(veg_shps, function(x) {
        v <- read_sf(x, quiet=TRUE) %>% select(geometry)
        v$VegClass <- tools::file_path_sans_ext(basename(x))
        if(st_crs(v) != st_crs(32735)) v <- st_transform(v, 32735)
        # Ensure it is POLYGON/MULTIPOLYGON
        v <- st_cast(v, "MULTIPOLYGON")
        return(v)
    })
    veg_all <- do.call(bind_rows, veg_poly_list)
    
    # Intersect with Used Steps (in UTM)
    used_steps <- rnd %>% filter(case_ == 1) %>% 
        st_as_sf(coords=c("x2_", "y2_"), crs=32735)
    
    used_veg <- st_join(used_steps, veg_all, join=st_within)
    
    # Summarize & Plot
    veg_stats <- used_veg %>%
        st_drop_geometry() %>%
        group_by(state_, VegClass) %>%
        summarise(count = n(), .groups="drop") %>%
        mutate(VegClass = replace_na(VegClass, "Unknown"))
        
    p_veg <- ggplot(veg_stats, aes(x = reorder(VegClass, count), y = count, fill = state_)) +
        geom_bar(stat = "identity", position = "dodge") +
        coord_flip() +
        scale_fill_manual(values = c("Foraging"="#E69F00", "Movement"="#56B4E9")) +
        labs(title = paste("Vegetation Class Usage -", tag), x = NULL, y = "Used Steps", fill = "State") +
        theme_minimal()
    ggsave(file.path(out_dir, paste0("Veg_Usage_", tag, ".png")), p_veg, width=10, height=8)
    
    # --- Micro-Habitat Data Collection (Boxplots) ---
    # Use rnd_cov (includes randoms, but we filter case_=1 for usage boxplots)
    used_cov <- rnd_cov %>% filter(case_ == 1) 
    # Attach veg info if needed, but boxplots use raster vars usually
    
    used_cov$Elephant <- ele_id
    used_cov$Stage <- job$Stage
    
    micro_hab_list[[length(micro_hab_list) + 1]] <- used_cov
    
    # 4. Fit iSSA
    # Scale variables
    rnd_cov_sc <- rnd_cov %>% 
        mutate(across(names(r_stack), scale)) %>%
        mutate(step_id_ = factor(step_id_),
               sl_ = as.numeric(sl_),
               log_sl_ = log(sl_),
               cos_ta_ = cos(ta_))
    
    # Formula: Interaction with State
    env_vars <- names(r_stack)
    form_str <- paste("case_ ~ sl_ + log_sl_ + cos_ta_ + (", paste(env_vars, collapse = " + "), ") : state_ + strata(step_id_)")
    
    m_issa <- tryCatch({
        fit_clogit(rnd_cov_sc, as.formula(form_str))
    }, error = function(e) {
        cat("iSSA Error:", e$message, "\n")
        return(NULL)
    })
    
    if(!is.null(m_issa)) {
        res <- tidy(m_issa$model) %>% mutate(Elephant = ele_id, Stage = job$Stage)
        models_list[[length(models_list) + 1]] <- res
        
        # Save individuals
        write_csv(res, file.path(out_dir, paste0("Coefs_", tag, ".csv")))
    }
    # --- VISUALIZATION: Dist Plots ---
    # (Plot helpers defined above...)
    # Ensure hmm_prep$State is consistent with trk_res (it is, see previous block)
    
    par_list <- list(step = m_hmm$mle$stepPar, angle = m_hmm$mle$anglePar)
    p_sl <- plot_dist("step", hmm_prep, par_list, "gamma", "m", state_labels)
    p_ta <- plot_dist("angle", hmm_prep, par_list, "vonmises", "rad", state_labels)
    
    p_comb <- p_sl + p_ta + plot_layout(guides = "collect")
    ggsave(file.path(out_dir, paste0("HMM_Distributions_", tag, ".png")), p_comb, width = 10, height = 5)
    
    # --- VISUALIZATION: Trajectory Map (Using trk_res) ---
    p_traj <- ggplot(trk_res, aes(x=x_, y=y_, color=State)) +
        geom_path(alpha=0.3) +
        geom_point(size=0.5) +
        scale_color_manual(values = c("Foraging"="#E69F00", "Movement"="#56B4E9")) +
        labs(title = paste("Trajectory -", tag)) +
        coord_equal() +
        theme_minimal()
    ggsave(file.path(out_dir, paste0("HMM_Map_", tag, ".png")), p_traj, width = 8, height = 6)
    


    # --- VISUALIZATION: Dist Plots ---
    # ... (Plotting code exists above)

    # Valid loop end
    cat("Done with iteration.\n")
}

# ==============================================================================
    if(length(vars_plot) == 0) {
        cat("No environmental variables found in micro_hab_list to plot.\n")
    } else {
        # Plot per elephant
        for(ele in unique(all_micro$Elephant)) {
            sub_micro <- all_micro %>% filter(Elephant == ele) %>%
               select(state_, all_of(vars_plot)) %>%
               pivot_longer(cols = all_of(vars_plot), names_to = "Variable", values_to = "Value")
            
            p_box <- ggplot(sub_micro, aes(x = state_, y = Value, fill = state_)) +
                geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
                facet_wrap(~ Variable, scales = "free_y", ncol = 5) +
                scale_fill_manual(values = c("Foraging"="#E69F00", "Movement"="#56B4E9")) +
                labs(title = paste("Micro-Habitat Selection (Used Steps) -", ele), y = "Value", x = NULL) +
                theme_minimal() + 
                theme(legend.position = "top")
                
            ggsave(file.path(out_dir, paste0("Micro_Habitat_Boxplots_", ele, ".png")), p_box, width = 16, height = 12)
        }
    }
}

if(length(models_list) > 0) {
    all_coefs <- bind_rows(models_list)
    write_csv(all_coefs, file.path(out_dir, "All_E3_E4_Coefficients.csv"))
    
    # Summary Plot
    env_coefs <- all_coefs %>% filter(str_detect(term, "_z")) %>%
        mutate(
            State = str_extract(term, "Foraging|Movement"),
            Variable = str_remove(str_remove(term, ":state_(Foraging|Movement)"), "_z")
        )
        
    p_summ <- ggplot(env_coefs, aes(x = term, y = estimate, color = Stage)) +
        geom_point(position = position_dodge(width = 0.5), size = 2) +
        geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2, position = position_dodge(width = 0.5)) +
# 5. Hypothesis 3 Check: Time Budget Shift (Graph D)
# ==============================================================================
if(length(micro_hab_list) > 0) {
    cat("Generating Time Budget Comparison (Pre vs Post)...\n")
    all_res <- bind_rows(micro_hab_list) %>%
        filter(!is.na(state_)) %>%
        group_by(Elephant, Stage, state_) %>%
        summarise(Count = n(), .groups = "drop") %>%
        group_by(Elephant, Stage) %>%
        mutate(Proportion = Count / sum(Count))
    
    # Order stages logically
    all_res$Stage <- factor(all_res$Stage, levels = c("pre", "interim", "post"))
    
    p_budget <- ggplot(all_res, aes(x = Stage, y = Proportion, fill = state_)) +
        geom_bar(stat = "identity", position = "fill", width = 0.7) +
        geom_text(aes(label = scales::percent(Proportion, accuracy = 1)), 
                  position = position_fill(vjust = 0.5), color = "white", size = 4) +
        facet_wrap(~ Elephant) +
        scale_fill_manual(values = c("Foraging"="#E69F00", "Movement"="#56B4E9")) +
        labs(title = "Hypothesis 3: Behavioral Time Budget Shift (Graph D)",
             subtitle = "Does fence removal reduce Movement time (cost) and increase Foraging (efficiency)?",
             y = "Proportion of Time Budget", x = "Stage", fill = "State") +
        theme_minimal() +
        theme(text = element_text(size = 14))
        
    ggsave(file.path(out_dir, "Time_Budget_Shift.png"), p_budget, width = 10, height = 6)
}

# ==============================================================================
# Micro-Habitat Boxplots (Graph C) & Coefficient Summary
# ==============================================================================

# ==============================================================================
# Micro-Habitat Boxplots (Graph C) & Coefficient Summary
# ==============================================================================
# Temporarily disabled to prevent crash due to missing Raster Data (NAs)
# Graph D (Time Budget) is the priority.

# if(length(micro_hab_list) > 0) {
#     cat("Generating Micro-Habitat Boxplots...\n")
#     all_micro <- bind_rows(micro_hab_list)
#     
#     # vars_plot should ONLY be the raster layers + extracted veg if any
#     # Identify variables to plot (raster layers + any veg)
#     known_env <- names(r_stack)
#     vars_to_check <- intersect(names(all_micro), known_env)
#     vars_plot <- setdiff(vars_to_check, "dist_old_lands") 
#     
#     if(length(vars_plot) == 0) {
#         cat("No environmental variables found for boxplots (Skipping Graph C).\n")
#     } else {
#         cat("Plotting variables:", paste(vars_plot, collapse=", "), "\n")
#         # Plot per elephant
#         for(ele in unique(all_micro$Elephant)) {
#             sub_micro <- all_micro %>% 
#                 filter(Elephant == ele) %>%
#                 select(state_, all_of(vars_plot)) %>%
#                 pivot_longer(cols = all_of(vars_plot), names_to = "Variable", values_to = "Value") %>%
#                 filter(is.finite(Value)) 
#             
#             if(nrow(sub_micro) > 0) {
#                 p_box <- ggplot(sub_micro, aes(x = state_, y = Value, fill = state_)) +
#                     geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
#                     facet_wrap(~ Variable, scales = "free_y", ncol = 5) +
#                     scale_fill_manual(values = c("Foraging"="#E69F00", "Movement"="#56B4E9")) +
#                     labs(title = paste("Micro-Habitat Constraints (Graph C) -", ele), y = "Value", x = NULL) +
#                     theme_minimal() + 
#                     theme(legend.position = "top")
#                     
#                 ggsave(file.path(out_dir, paste0("Micro_Habitat_Boxplots_", ele, ".png")), p_box, width = 16, height = 12)
#             }
#         }
#     }
# }

# if(length(models_list) > 0) {
#     all_coefs <- bind_rows(models_list)
#     write_csv(all_coefs, file.path(out_dir, "All_E3_E4_Coefficients.csv"))
#     
#     # Summary Plot
#     env_coefs <- all_coefs %>% filter(str_detect(term, "_z")) %>%
#         mutate(
#             State = str_extract(term, "Foraging|Movement"),
#             Variable = str_remove(str_remove(term, ":state_(Foraging|Movement)"), "_z")
#         )
#         
#     p_summ <- ggplot(env_coefs, aes(x = term, y = estimate, color = Stage)) +
#         geom_point(position = position_dodge(width = 0.5), size = 2) +
#         geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2, position = position_dodge(width = 0.5)) +
#         coord_flip() +
#         facet_grid(Elephant ~ State, scales = "free_y", space = "free_y") +
#         theme_bw() +
#         labs(title = "Habitat Selection Coefficients", x = "Variable", y = "Selection Strength (Log-Odds)")
#         
#     ggsave(file.path(out_dir, "Coefficient_Summary.png"), p_summ, width = 12, height = 10)
# }

