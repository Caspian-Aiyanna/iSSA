# 02_hypothesis_testing.R
# Focused script to test Core Hypotheses with High-Quality Visuals
# Hypothesis 1: Functional Axis (Trajectory Maps with Study Boundary)
# Hypothesis 3: Behavioral Plasticity (Time Budget Shift)

suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate)
    library(sf)
    library(terra)
    library(amt)
    library(moveHMM)
    library(viridis)
    library(patchwork)
    library(scales)
})

# ==============================================================================
# 1. Config & Paths
# ==============================================================================

root_dir <- dirname(dirname(getwd())) # BioHabs (assuming running from playground)
clean_dir <- file.path(root_dir, "BioHabs", "data", "clean")
shp_dir <- file.path(root_dir, "BioHabs", "data", "shp")
out_dir <- file.path(root_dir, "BioHabs", "playground", "02_Hypothesis_Results")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Time Periods
periods <- list(
    pre = interval(dmy("04-08-2022"), dmy("09-12-2023")),
    interim = interval(dmy("10-12-2023"), dmy("08-01-2024")),
    post = interval(dmy("09-01-2024"), dmy("14-07-2025"))
)

# Load Boundary Shapefile
# Try to find the best candidate for "Full Study Area"
bound_path <- file.path(shp_dir, "HV20233.shp") # Assuming this is the latest
if (!file.exists(bound_path)) {
    # Fallback to similar
    cands <- list.files(shp_dir, pattern = "HV.*\\.shp", full.names = TRUE)
    if (length(cands) > 0) bound_path <- cands[length(cands)] # Take last (likely newest)
}

boundary_sf <- NULL
if (file.exists(bound_path)) {
    cat("Loading Study Area Boundary:", bound_path, "\n")
    boundary_sf <- read_sf(bound_path)
} else {
    warning("Study Area Boundary shapefile not found in:", shp_dir)
}

# ==============================================================================
# 2. Main Analysis Loop
# ==============================================================================

time_budget_data <- list()

ele_files <- list.files(clean_dir, pattern = "^E[34][AB]?\\.csv$", full.names = TRUE, recursive = TRUE)

# Robust Data Loading
ele_data <- map_df(ele_files, function(f) {
    df <- read_csv(f, show_col_types = FALSE)
    ele_id <- str_extract(basename(f), "^E[0-9]+")

    if ("individual.local.identifier" %in% names(df)) {
        df <- df %>% rename(collar_id = individual.local.identifier, lon = location.long, lat = location.lat)
    }

    df %>%
        mutate(
            Elephant = ele_id,
            timestamp = tryCatch(dmy_hm(timestamp), error = function(e) ymd_hms(timestamp))
        ) %>%
        mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>%
        select(Elephant, timestamp, lon, lat)
}) %>%
    arrange(Elephant, timestamp) %>%
    filter(!is.na(lon) & !is.na(lat) & !is.na(timestamp))

# For each elephant
for (ele_id in unique(ele_data$Elephant)) {
    ele_sub <- ele_data %>% filter(Elephant == ele_id)

    # For each stage
    for (stage_name in names(periods)) {
        cat("\nPROCESSING:", ele_id, "-", stage_name, "\n")

        # 1. Filter Data
        p_int <- periods[[stage_name]]
        trk <- ele_sub %>% filter(timestamp %within% p_int)

        if (nrow(trk) < 500) {
            cat("  Skipping (Not enough data - <500 pts)\n")
            next
        }

        # 2. Prepare HMM Data
        trk_sf <- make_track(trk, lon, lat, timestamp, crs = 4326)
        trk_utm <- transform_coords(trk_sf, crs_to = 32735)

        # Resample
        trk_res <- NULL
        try(
            {
                trk_res <- trk_utm %>%
                    track_resample(rate = minutes(30), tolerance = minutes(5))
            },
            silent = TRUE
        )

        if (is.null(trk_res)) {
            cat("  Resampling failed\n")
            next
        }

        # 3. Fit HMM
        df_hmm <- data.frame(ID = ele_id, x = trk_res$x_, y = trk_res$y_, date = trk_res$t_)
        hmm_dat <- prepData(df_hmm, type = "UTM")
        hmm_dat <- hmm_dat %>% filter(!is.na(step) & !is.na(angle) & step > 0)

        if (nrow(hmm_dat) < 100) next

        # Init Params
        mean_sl <- mean(hmm_dat$step, na.rm = TRUE)
        step_mean <- c(mean_sl * 0.2, mean_sl * 2.0)
        step_sd <- c(mean_sl * 0.2, mean_sl * 1.5)
        angle_mean <- c(pi, 0)
        angle_conc <- c(0.5, 2.0)

        m_hmm <- try(fitHMM(
            data = hmm_dat, nbStates = 2,
            stepPar0 = c(step_mean, step_sd),
            anglePar0 = c(angle_mean, angle_conc),
            formula = ~1, verbose = 0
        ))

        if (inherits(m_hmm, "try-error")) {
            cat("  HMM Fit Failed\n")
            next
        }

        # Decode and Label
        states <- viterbi(m_hmm)
        mus <- m_hmm$mle$stepPar[1, ]
        state_labels <- if (mus[1] > mus[2]) c("Movement", "Foraging") else c("Foraging", "Movement")

        dat_states <- hmm_dat %>% mutate(
            state_idx = states,
            state_label = factor(state_labels[states], levels = c("Foraging", "Movement"))
        )

        # Store for H3
        time_budget_data[[paste(ele_id, stage_name)]] <- dat_states %>%
            count(state_label) %>%
            mutate(Elephant = ele_id, Stage = stage_name)

        # ======================================================================
        # HYPOTHESIS 1: Trajectory Map (With Boundary)
        # ======================================================================
        cat("  Generating H1 Map...\n")

        # Setup Plot
        p_map <- ggplot()

        # Add Boundary (if exists and CRS aligned)
        if (!is.null(boundary_sf)) {
            try(
                {
                    # Ensure Boundary has CRS (Assume WGS84 if missing)
                    if (is.na(st_crs(boundary_sf))) {
                        st_crs(boundary_sf) <- 4326
                    }
                    # Transform to UTM 35S
                    b_prj <- st_transform(boundary_sf, 32735)

                    p_map <- p_map +
                        geom_sf(data = b_prj, fill = NA, color = "black", linewidth = 0.8, linetype = "solid")
                },
                silent = FALSE
            )
        }


        # Add Track (Lines AND Dots)
        p_map <- p_map +
            geom_path(data = dat_states, aes(x = x, y = y, color = state_label), alpha = 0.5, linewidth = 0.3) +
            geom_point(data = dat_states, aes(x = x, y = y, color = state_label), size = 0.5, alpha = 0.7) +
            scale_color_manual(values = c("Foraging" = "#E69F00", "Movement" = "#56B4E9")) +
            coord_sf() + # Ensure SF coords handle aspect ratio
            theme_minimal() +
            labs(
                title = paste("H1: Functional Trajectory -", ele_id, stage_name),
                subtitle = "Map showing Foraging (Gold) vs Movement (Blue) relative to Study Area",
                x = "Easting", y = "Northing", color = "Behavior"
            )

        ggsave(file.path(out_dir, paste0("H1_Map_", ele_id, "_", stage_name, ".png")), p_map, width = 8, height = 8, bg = "white")
    }
}

# ==============================================================================
# H3 AGGREGATE: Time Budget Shift (THE PERFECT CHART)
# ==============================================================================
if (length(time_budget_data) > 0) {
    cat("\nGenerating Perfect Time Budget Graph...\n")
    tb_df <- bind_rows(time_budget_data)
    tb_df$Stage <- factor(tb_df$Stage, levels = c("pre", "interim", "post"))

    tb_summ <- tb_df %>%
        group_by(Elephant, Stage) %>%
        mutate(Total = sum(n), Prop = n / Total) %>%
        ungroup() %>%
        mutate(state_label = factor(state_label, levels = c("Foraging", "Movement")))

    p_budget <- ggplot(tb_summ, aes(x = Stage, y = Prop, fill = state_label)) +
        geom_bar(stat = "identity", width = 0.7, position = "fill", color = "white", linewidth = 0.5) +
        geom_text(aes(label = scales::percent(Prop, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            color = "white", size = 5, fontface = "bold"
        ) +
        scale_fill_manual(values = c("Foraging" = "#E69F00", "Movement" = "#56B4E9")) +
        facet_wrap(~Elephant, scales = "free_x") +
        labs(
            title = "Hypothesis 3: Behavioral Time Budget Shift",
            subtitle = "Proportion of Time Spent in Foraging vs. Movement States (Pre- vs. Post-Fence Removal)",
            y = "Time Budget (%)", x = NULL, fill = "Behavioral State"
        ) +
        theme_minimal(base_size = 14) +
        theme(
            plot.title = element_text(face = "bold", size = 16),
            strip.text = element_text(face = "bold", size = 14),
            legend.position = "top",
            panel.grid = element_blank(),
            axis.text.y = element_text(color = "gray50"),
            axis.text.x = element_text(face = "bold", size = 12)
        ) +
        scale_y_continuous(labels = scales::percent_format())

    out_file <- file.path(out_dir, "H3_Time_Budget_Shift_Perfect.png")
    ggsave(out_file, p_budget, width = 10, height = 7, bg = "white", dpi = 300)
    cat("Saved Perfect Time Budget Graph to:", out_file, "\n")
} else {
    cat("No Time Budget Data Generated.\n")
}
