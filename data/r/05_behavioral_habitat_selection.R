# 05_behavioral_habitat_selection.R
# Final BACI-Design Script: Behavioral States, Temporal Predictions, and Ecological Trends
# Integrated Logic: 04_v3, 02_hypothesis, and 05_RSF
# 3-State HMM: Resting (5m), Foraging (50m), Movement (150m)

suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate)
    library(sf)
    library(amt)
    library(moveHMM)
    library(terra)
    library(patchwork)
    library(scales)
    library(ggplot2)
})

cat("Starting Integrated Behavioral Habitat Selection Analysis (BACI Design)...\n")

# ==============================================================================
# 1. Configuration & Paths
# ==============================================================================

root_dir <- getwd()
clean_dir <- file.path(root_dir, "data", "clean")
shp_dir <- file.path(root_dir, "data", "shp")
env_dir <- file.path(root_dir, "data", "envi")
out_dir <- file.path(root_dir, "results", "RSF")
pts_out_dir <- file.path(out_dir, "behavioral_points")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
if (!dir.exists(pts_out_dir)) dir.create(pts_out_dir, recursive = TRUE)

target_crs <- 32735 # UTM 35S
cache_file <- file.path(out_dir, "hmm_results_all_elephants.rds")

# BACI Time Periods
# Synchronized with 04_ecological_behavior_analysis_v5.R
periods <- list(
    pre     = interval(ymd("2020-01-01"), ymd("2023-12-09")),
    interim = interval(ymd("2023-12-10"), ymd("2024-02-09")),
    post    = interval(ymd("2024-02-10"), ymd("2026-12-31"))
)

# Custom Color Scale (Blue = Low, Red = High)
br_colors <- c("#0571b0", "#f7f7f7", "#ca0020")

# ==============================================================================
# 2. Loading Contextual Shapefiles
# ==============================================================================

cat("Loading Spatial Context (HV, KW, Study Area, Fence)...\n")
hv_sf <- read_sf(file.path(shp_dir, "HV", "HV.shp")) %>% st_transform(target_crs)
kw_sf <- read_sf(file.path(shp_dir, "KW", "KW.shp")) %>% st_transform(target_crs)
study_area_sf <- read_sf(file.path(shp_dir, "study area boundary", "HV20233.shp")) %>% st_transform(target_crs)
fence_sf <- read_sf(file.path(shp_dir, "Fence", "fence_2024.shp")) %>% st_transform(target_crs)
fence_buffer_100m <- st_buffer(fence_sf, dist = 100) # 100m buffer as requested

# ==============================================================================
# 3. Environmental Prediction Stack (41 Layers)
# ==============================================================================

cat("Loading Environmental Stack (39 layers)...\n")
r_stack <- terra::rast(list.files(env_dir, pattern = "\\.tif$", full.names = TRUE))
safe_names <- make.names(names(r_stack), unique = TRUE)
names(r_stack) <- safe_names
stack_crs <- terra::crs(r_stack)

# ==============================================================================
# 4. Loading Master Calibrated Behavioral Points (V5 - 4-State HMM)
# ==============================================================================

cat("Loading Calibrated 4-State Behavioral Points (V5)...\n")
master_behavior_file <- file.path(root_dir, "results", "Ecological_Behavior_V5", "Elephant_Behavioral_Points_Final_V5.csv")

if (!file.exists(master_behavior_file)) {
    stop("Master behavioral file not found. Please run 04_ecological_behavior_analysis_v5.R first.")
}

full_df_master <- read_csv(master_behavior_file, show_col_types = FALSE) %>%
    mutate(behavior = factor(behavior, levels = c("Sleeping", "Resting", "Foraging", "Movement", "Bounce")))

# Convert to SF and transform to environmental stack CRS
full_results_sf <- full_df_master %>%
    st_as_sf(coords = c("x_m", "y_m"), crs = target_crs, remove = FALSE) %>%
    st_transform(stack_crs) %>%
    split(.$Elephant)

cat(sprintf("Loaded behavioral data for %d elephants.\n", length(full_results_sf)))


# ==============================================================================
# 5. Temporal RSF Mapping (Realized BACI Selection)
# ==============================================================================

cat("\nExecuting Realized Behavioral habitat Selection (BACI)...\n")
accuracy_report <- list()
master_extractions <- list()

# Custom AUC function
calc_auc <- function(probs, obs) {
    n1 <- sum(obs == 1)
    n0 <- sum(obs == 0)
    if (n1 == 0 | n0 == 0) {
        return(NA)
    }
    r <- rank(probs)
    (sum(r[obs == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# Study Area Union for Post-Removal (used by E3, E4, E5 for INTERIM/POST)
study_area_union <- st_make_valid(study_area_sf) %>%
    st_collection_extract("POLYGON") %>%
    st_union()

for (stg in names(periods)) {
    stg_dir <- file.path(out_dir, "rasters", stg)
    if (!dir.exists(stg_dir)) dir.create(stg_dir, recursive = TRUE)

    for (ele_id in names(full_results_sf)) {
        # Check data availability: E1, E2, E6 have no POST data
        if (ele_id %in% c("E1", "E2", "E6") && stg == "post") {
            cat(sprintf("  Skipping %s for POST period: No data available\n", ele_id))
            next
        }

        pts_stg <- full_results_sf[[ele_id]] %>% filter(Stage == stg)
        if (nrow(pts_stg) < 20) {
            cat(sprintf("  Skipping Stage %s for %s: Too few points.\n", stg, ele_id))
            next
        }

        # Define availability mask based on elephant ID and period
        # E1, E2, E3, E4: Use KW for all periods
        # E5, E6: Use HV for PRE, study area for INTERIM/POST
        # E3, E4, E5: Use study area for INTERIM/POST

        if (ele_id %in% c("E1", "E2")) {
            # E1, E2: Always use KW
            mask_poly <- st_make_valid(kw_sf)
            cat(sprintf("  %s | %s: Using KW boundary for RSF masking\n", ele_id, toupper(stg)))
        } else if (ele_id %in% c("E3", "E4")) {
            # E3, E4: Use KW for PRE, study area for INTERIM/POST
            if (stg == "pre") {
                mask_poly <- st_make_valid(kw_sf)
                cat(sprintf("  %s | %s: Using KW boundary for RSF masking\n", ele_id, toupper(stg)))
            } else {
                mask_poly <- study_area_union
                cat(sprintf("  %s | %s: Using study area boundary for RSF masking\n", ele_id, toupper(stg)))
            }
        } else if (ele_id %in% c("E5", "E6")) {
            # E5, E6: Use HV for PRE, study area for INTERIM/POST
            if (stg == "pre") {
                mask_poly <- st_make_valid(hv_sf)
                cat(sprintf("  %s | %s: Using HV boundary for RSF masking\n", ele_id, toupper(stg)))
            } else {
                mask_poly <- study_area_union
                cat(sprintf("  %s | %s: Using study area boundary for RSF masking\n", ele_id, toupper(stg)))
            }
        }

        mask_poly <- st_collection_extract(st_make_valid(mask_poly), "POLYGON") %>% st_union()
        mask_vect <- terra::vect(st_transform(mask_poly, stack_crs))

        # Stage-Specific Background Sampling once per elephant/stage
        set.seed(42)
        bg_pts <- st_sample(mask_poly, size = nrow(pts_stg) * 2)
        ext_bg <- terra::extract(r_stack, terra::vect(st_transform(st_as_sf(bg_pts), stack_crs))) %>% mutate(case = 0)
        if ("ID" %in% names(ext_bg)) ext_bg$ID <- NULL

        for (bh in c("Sleeping", "Resting", "Foraging", "Movement")) {
            # Skip if output already exists to save time/memory
            final_tif <- file.path(stg_dir, sprintf("Realized_%s_%s_%s.tif", ele_id, stg, bh))
            if (file.exists(final_tif)) {
                cat(sprintf("  Skipping finished model: %s | %s | %s\n", ele_id, stg, bh))
                next
            }

            bh_pts <- pts_stg %>% filter(behavior == bh)
            if (nrow(bh_pts) < 10) next

            cat(sprintf("  RSF Processing: %s | %s | %s (%d points)\n", ele_id, stg, bh, nrow(bh_pts)))

            ext_used <- terra::extract(r_stack, terra::vect(st_transform(bh_pts, stack_crs))) %>%
                mutate(case = 1, behavior = bh, Stage = stg, Elephant = ele_id)
            if ("ID" %in% names(ext_used)) ext_used$ID <- NULL
            master_extractions[[length(master_extractions) + 1]] <- ext_used

            # 2. Predictor Filtering
            full_df <- bind_rows(ext_used, ext_bg)
            na_prop <- colMeans(is.na(full_df))
            valid_covs <- names(na_prop)[na_prop < 0.3 & names(na_prop) %in% safe_names]

            sds <- sapply(full_df[, valid_covs, drop = FALSE], function(x) sd(x, na.rm = TRUE))
            valid_covs <- valid_covs[is.finite(sds) & sds > 1e-6]

            model_df <- full_df %>%
                select(case, all_of(valid_covs)) %>%
                na.omit()
            if (nrow(model_df) < 50) next

            cor_mat <- try(cor(model_df %>% select(-case), use = "complete.obs"), silent = TRUE)
            final_covs <- valid_covs
            if (!inherits(cor_mat, "try-error")) {
                to_remove <- c()
                for (i in 1:(ncol(cor_mat) - 1)) {
                    for (j in (i + 1):ncol(cor_mat)) {
                        if (abs(cor_mat[i, j]) > 0.8) to_remove <- c(to_remove, colnames(cor_mat)[j])
                    }
                }
                final_covs <- setdiff(valid_covs, unique(to_remove))
            }

            # 3. Fit Model
            m_rsf <- try(glm(as.formula(paste("case ~", paste(final_covs, collapse = " + "))), data = model_df, family = binomial()), silent = TRUE)
            if (inherits(m_rsf, "try-error")) next

            # 4. AUC & Predict
            auc_val <- calc_auc(predict(m_rsf, type = "response"), model_df$case)
            accuracy_report[[paste(ele_id, stg, bh)]] <- data.frame(Elephant = ele_id, Stage = stg, Behavior = bh, AUC = auc_val)

            # Use only relevant layers for prediction to save memory
            p_rast <- terra::predict(r_stack[[final_covs]], m_rsf, type = "response")
            p_rast_masked <- terra::mask(p_rast, mask_vect)

            # Normalize & Write
            q_high_df <- terra::global(p_rast_masked, function(x) quantile(x, 0.99, na.rm = T))
            if (!is.na(q_high_df[1, 1])) {
                q_high <- q_high_df[1, 1]
                p_clamped <- terra::clamp(p_rast_masked, upper = q_high)
                min_val <- terra::global(p_clamped, "min", na.rm = T)[1, 1]
                max_val <- terra::global(p_clamped - min_val, "max", na.rm = T)[1, 1]

                if (!is.na(max_val) && max_val > 0) {
                    rsf_01 <- (p_clamped - min_val) / max_val
                    terra::writeRaster(rsf_01, final_tif, overwrite = TRUE)

                    # Generate Map Plot
                    rsf_plot_rast <- terra::project(rsf_01, paste0("EPSG:", target_crs))
                    df_plot <- as.data.frame(rsf_plot_rast, xy = TRUE)
                    names(df_plot)[3] <- "Selection"

                    p_map <- ggplot() +
                        geom_sf(data = hv_sf, fill = "#f7f7f7", color = NA) +
                        geom_sf(data = kw_sf, fill = "#f7f7f7", color = NA) +
                        geom_raster(data = df_plot, aes(x = x, y = y, fill = Selection)) +
                        geom_sf(data = hv_sf, fill = NA, color = "grey30", linewidth = 0.3) +
                        geom_sf(data = kw_sf, fill = NA, color = "grey30", linewidth = 0.3) +
                        {
                            if (stg %in% c("pre", "interim")) geom_sf(data = fence_sf, color = "red", linetype = "solid", linewidth = 0.8)
                        } +
                        scale_fill_gradientn(
                            colors = br_colors, limits = c(0, 1),
                            breaks = c(0, 0.5, 1), labels = c("Low", "Mid", "High"),
                            name = "Selection\nProb."
                        ) +
                        labs(
                            title = sprintf("%s RSF Selection: %s", ele_id, bh),
                            subtitle = sprintf("Stage: %s | AUC: %.2f", toupper(stg), auc_val),
                            x = "Easting", y = "Northing"
                        ) +
                        theme_bw() +
                        theme(panel.grid = element_blank()) +
                        coord_sf()

                    ggsave(file.path(stg_dir, sprintf("Realized_RSF_%s_%s_%s.png", ele_id, stg, bh)), p_map, width = 10, height = 9, dpi = 300, bg = "white", create.dir = TRUE)
                }
            }

            # OCCUPANCY INTENSITY (Realized Usage Heatmap)
            r_points <- terra::rasterize(terra::vect(st_transform(bh_pts, stack_crs)), r_stack[[1]], fun = "count")
            r_smooth <- terra::focal(r_points, w = 11, fun = "mean", na.rm = TRUE)
            r_usage_masked <- terra::mask(r_smooth, mask_vect)

            u_min <- terra::global(r_usage_masked, "min", na.rm = TRUE)[1, 1]
            u_max <- terra::global(r_usage_masked, "max", na.rm = TRUE)[1, 1]
            if (!is.na(u_max) && u_max > (u_min + 1e-6)) {
                usage_01 <- (r_usage_masked - u_min) / (u_max - u_min)
                terra::writeRaster(usage_01, file.path(stg_dir, sprintf("Usage_%s_%s_%s.tif", ele_id, stg, bh)), overwrite = TRUE)
            }

            # Clean memory for this behavior
            rm(ext_used, full_df, model_df, m_rsf, p_rast, p_rast_masked, rsf_01, r_points, r_smooth, r_usage_masked)
            gc()
        }
        # Clean memory for this elephant/stage
        rm(pts_stg, ext_bg)
        gc()
    }
}

# ==============================================================================
# 6. Comparative Visual panels: Occupancy, Realized Niche & RSF Selection
# ==============================================================================

cat("\nGenerating Comparative Panels (Vigour check with Occupancy plots)...\n")
all_pts_sf <- bind_rows(full_results_sf)

# 1. Behavioral Trajectory Panels (Specific per Behavior & Stage)
for (ele_id in names(full_results_sf)) {
    for (bh in c("Sleeping", "Resting", "Foraging", "Movement")) {
        cat(sprintf("  Creating Specific Trajectory Panel: %s | %s\n", ele_id, bh))
        pts_ele_bh <- all_pts_sf %>% filter(Elephant == ele_id, behavior == bh)

        p_list <- list()
        for (stg in c("pre", "interim", "post")) {
            p_sub <- pts_ele_bh %>% filter(Stage == stg)

            p <- ggplot() +
                geom_sf(data = hv_sf, fill = "gray98", color = "gray90", linewidth = 0.1) +
                geom_sf(data = kw_sf, fill = "gray98", color = "gray90", linewidth = 0.1) +
                geom_sf(data = p_sub, color = case_when(
                    bh == "Sleeping" ~ "#1A1A1A",
                    bh == "Resting" ~ "#999999",
                    bh == "Foraging" ~ "#E69F00",
                    bh == "Movement" ~ "#56B4E9"
                ), size = 0.4, alpha = 0.5) +
                labs(title = toupper(stg), subtitle = sprintf("Points: %d", nrow(p_sub))) +
                theme_minimal() +
                theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank()) +
                coord_sf()

            # Add fence only for Pre/Interim
            if (stg %in% c("pre", "interim")) p <- p + geom_sf(data = fence_sf, color = "red", linewidth = 0.6)
            p_list[[stg]] <- p
        }

        combined_bh_traj <- (p_list[["pre"]] | p_list[["interim"]] | p_list[["post"]]) +
            plot_annotation(
                title = sprintf("Behavioral Trajectory Map: %s - %s", ele_id, bh),
                subtitle = "Comparison across BACI stages for specific behavior state",
                theme = theme(plot.title = element_text(face = "bold", size = 16))
            )

        ggsave(file.path(out_dir, sprintf("Panel_Trajectory_Specific_%s_%s.png", ele_id, bh)), combined_bh_traj, width = 15, height = 6, bg = "white", create.dir = TRUE)
    }
}

# 2. RSF Predicted Selection Panels (The "Potential" Niche from GLM)
for (ele_id in names(full_results_sf)) {
    for (bh in c("Sleeping", "Resting", "Foraging", "Movement")) {
        cat(sprintf("  Creating RSF Selection Panel: %s | %s\n", ele_id, bh))
        rsf_panel_list <- list()
        for (stg in c("pre", "interim", "post")) {
            tif_path <- file.path(out_dir, "rasters", stg, sprintf("Realized_%s_%s_%s.tif", ele_id, stg, bh))
            if (file.exists(tif_path)) {
                r_val <- terra::rast(tif_path)
                r_prj <- terra::project(r_val, paste0("EPSG:", target_crs))
                df_p <- as.data.frame(r_prj, xy = TRUE)
                names(df_p)[3] <- "Selection"

                p_rsf <- ggplot() +
                    geom_sf(data = hv_sf, fill = "#f7f7f7", color = NA) +
                    geom_sf(data = kw_sf, fill = "#f7f7f7", color = NA) +
                    geom_raster(data = df_p, aes(x = x, y = y, fill = Selection)) +
                    geom_sf(data = hv_sf, fill = NA, color = "grey35", linewidth = 0.2) +
                    geom_sf(data = kw_sf, fill = NA, color = "grey35", linewidth = 0.2) +
                    scale_fill_gradientn(colors = br_colors, limits = c(0, 1), name = "Selection Prob.") +
                    labs(title = toupper(stg)) +
                    theme_minimal() +
                    theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank()) +
                    coord_sf()
                if (stg %in% c("pre", "interim")) p_rsf <- p_rsf + geom_sf(data = fence_sf, color = "red", linewidth = 0.6)
                rsf_panel_list[[stg]] <- p_rsf
            } else {
                rsf_panel_list[[stg]] <- ggplot() +
                    theme_void() +
                    labs(title = toupper(stg), subtitle = "Model skipped")
            }
        }
        combined_rsf <- (rsf_panel_list[["pre"]] | rsf_panel_list[["interim"]] | rsf_panel_list[["post"]]) +
            plot_annotation(
                title = sprintf("Predicted Habitat Selection (RSF): %s (%s)", ele_id, bh),
                subtitle = "Environmental Habituation across BACI stages (Blue=Low, Red=High Selection)",
                theme = theme(plot.title = element_text(face = "bold", size = 16))
            ) +
            plot_layout(guides = "collect") & theme(legend.position = "bottom")
        ggsave(file.path(out_dir, sprintf("Panel_RSF_Selection_%s_%s.png", ele_id, bh)), combined_rsf, width = 15, height = 6, bg = "white", create.dir = TRUE)
    }
}

# 3. Behavioral Occupancy Intensity Panels (The "Realized" Niche Heatmaps)
for (ele_id in names(full_results_sf)) {
    for (bh in c("Sleeping", "Resting", "Foraging", "Movement")) {
        cat(sprintf("  Creating Occupancy Intensity Panel: %s | %s\n", ele_id, bh))
        bh_list <- list()
        for (stg in c("pre", "interim", "post")) {
            tif_path <- file.path(out_dir, "rasters", stg, sprintf("Usage_%s_%s_%s.tif", ele_id, stg, bh))
            if (file.exists(tif_path)) {
                r_usage <- terra::rast(tif_path)
                r_prj <- terra::project(r_usage, paste0("EPSG:", target_crs))

                # Use same masking logic as RSF
                if (ele_id %in% c("E1", "E2")) {
                    m_poly <- st_make_valid(kw_sf)
                } else if (ele_id %in% c("E3", "E4")) {
                    m_poly <- if (stg == "pre") st_make_valid(kw_sf) else study_area_union
                } else if (ele_id %in% c("E5", "E6")) {
                    m_poly <- if (stg == "pre") st_make_valid(hv_sf) else study_area_union
                }

                r_msk <- terra::mask(r_prj, terra::vect(m_poly))

                df_u <- as.data.frame(r_msk, xy = TRUE)
                names(df_u)[3] <- "Intensity"
                p_occ <- ggplot() +
                    geom_sf(data = hv_sf, fill = "#f7f7f7", color = NA) +
                    geom_sf(data = kw_sf, fill = "#f7f7f7", color = NA) +
                    geom_raster(data = df_u, aes(x = x, y = y, fill = Intensity)) +
                    geom_sf(data = hv_sf, fill = NA, color = "grey35", linewidth = 0.2) +
                    geom_sf(data = kw_sf, fill = NA, color = "grey35", linewidth = 0.2) +
                    scale_fill_gradientn(colors = br_colors, limits = c(0, 1), name = "Usage Intensity") +
                    labs(title = toupper(stg)) +
                    theme_minimal() +
                    theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank()) +
                    coord_sf()
                if (stg %in% c("pre", "interim")) p_occ <- p_occ + geom_sf(data = fence_sf, color = "red", linewidth = 0.6)
                bh_list[[stg]] <- p_occ
            } else {
                bh_list[[stg]] <- ggplot() +
                    theme_void() +
                    labs(title = toupper(stg), subtitle = "No data")
            }
        }
        combined_niche <- (bh_list[["pre"]] | bh_list[["interim"]] | bh_list[["post"]]) +
            plot_annotation(
                title = sprintf("Realized Occupancy Intensity: %s (%s)", ele_id, bh),
                subtitle = "Visual agreement with occupancy density (Blue=Low, Red=High Concentration)",
                theme = theme(plot.title = element_text(face = "bold", size = 16))
            ) +
            plot_layout(guides = "collect") & theme(legend.position = "bottom")
        ggsave(file.path(out_dir, sprintf("Panel_Realized_Occupancy_%s_%s.png", ele_id, bh)), combined_niche, width = 15, height = 6, bg = "white", create.dir = TRUE)
    }
}

# ==============================================================================
# 7. Core Behavioral Synthesis Graphs (Recreating figures)
# ==============================================================================

cat("\nGenerating Final Synthesis Graphs...\n")
df_all <- bind_rows(map(full_results_sf, st_drop_geometry)) %>%
    mutate(
        Season = case_when(
            month(date) %in% c(9, 10, 11) ~ "Spring",
            month(date) %in% c(12, 1, 2) ~ "Summer",
            month(date) %in% c(3, 4, 5) ~ "Autumn",
            month(date) %in% c(6, 7, 8) ~ "Winter"
        ),
        Season = factor(Season, levels = c("Spring", "Summer", "Autumn", "Winter")),
        Stage = factor(Stage, levels = c("pre", "interim", "post"))
    )

# --- Graph 1: Behavioral Time Budget Shift ---
cat("  Plotting: Behavioral_Time_Budget_Shift.png\n")
df_budget <- df_all %>%
    group_by(Elephant, Stage, behavior) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(Prop = n / sum(n))

p1 <- ggplot(df_budget, aes(x = Stage, y = Prop, fill = behavior)) +
    geom_bar(stat = "identity", color = "white", linewidth = 0.5) +
    geom_text(aes(label = scales::percent(Prop, accuracy = 1)),
        position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 4
    ) +
    facet_wrap(~Elephant) +
    scale_fill_manual(values = c("Sleeping" = "#1A1A1A", "Resting" = "#969696", "Foraging" = "#D95F02", "Movement" = "#377EB8", "Bounce" = "#E41A1C")) +
    labs(title = "Behavioral Time Budget Shift", x = "Period", y = "Proportion", fill = "behavior") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

ggsave(file.path(out_dir, "01_Behavioral_Time_Budget_Shift.png"), p1, width = 12, height = 8, bg = "white", create.dir = TRUE)

# --- Graph 2: Seasonal Behavioral Usage ---
cat("  Plotting: Seasonal_Behavioral_Usage.png\n")
df_seasonal <- df_all %>%
    group_by(Elephant, Stage, Season, behavior) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(Prop = n / sum(n))

p2 <- ggplot(df_seasonal, aes(x = Season, y = Prop, fill = behavior)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(Elephant ~ Stage) +
    scale_fill_manual(values = c("Sleeping" = "#1A1A1A", "Resting" = "#969696", "Foraging" = "#D95F02", "Movement" = "#377EB8", "Bounce" = "#E41A1C")) +
    labs(
        title = "Seasonal Behavioral Usage", subtitle = "Comparison across 4 seasons (Spring, Summer, Autumn, Winter)",
        x = "Season", y = "Prop", fill = "behavior"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_dir, "02_Seasonal_Behavioral_Usage.png"), p2, width = 14, height = 9, bg = "white", create.dir = TRUE)

# --- Graph 3: Ghost Fence & Interaction Trend ---
cat("  Plotting: Ghost_Fence_Trend.png\n")
interaction_events <- list()

for (ele_id in names(full_results_sf)) {
    ele_sf <- full_results_sf[[ele_id]]
    this_crs <- st_crs(ele_sf)
    f_buf_prj <- st_transform(fence_buffer_100m, this_crs) %>% st_make_valid()
    kw_sf_prj <- st_transform(kw_sf, this_crs) %>% st_make_valid()

    is_buffer <- st_intersects(st_make_valid(ele_sf), f_buf_prj, sparse = F)[, 1]
    is_kw <- st_intersects(st_make_valid(ele_sf), kw_sf_prj, sparse = F)[, 1]

    # 1. Behavioral Interactions (Points within buffer)
    bh_in_buf <- ele_sf[is_buffer, ] %>%
        st_drop_geometry() %>%
        mutate(Type = as.character(behavior)) %>%
        select(Elephant, Date = date, Type, Stage)
    interaction_events[[length(interaction_events) + 1]] <- bh_in_buf

    # 2. Bounce Logic (Physical response to the fence)
    buf_idx <- which(is_buffer)
    if (length(buf_idx) > 0) {
        bursts <- split(buf_idx, cumsum(c(1, diff(buf_idx) != 1)))
        for (b in bursts) {
            f <- min(b)
            l <- max(b)
            if (f > 1 && l < nrow(ele_sf)) {
                if (is_kw[f - 1] && is_kw[l + 1]) {
                    interaction_events[[length(interaction_events) + 1]] <- data.frame(
                        Elephant = ele_id, Date = ele_sf$date[f], Type = "Bounce", Stage = ele_sf$Stage[f]
                    )
                }
            }
        }
    }
}

df_int <- bind_rows(interaction_events) %>%
    filter(Stage == "post") %>%
    mutate(Month = floor_date(Date, "month")) %>%
    group_by(Elephant, Month, Type) %>%
    summarise(Events = n(), .groups = "drop")

p3 <- ggplot(df_int, aes(x = Month, y = Events, color = Type)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    facet_wrap(~Elephant, scales = "free_y") +
    scale_color_manual(values = c("Sleeping" = "#1A1A1A", "Resting" = "#969696", "Foraging" = "#D95F02", "Movement" = "#377EB8", "Bounce" = "#E41A1C")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    labs(
        title = "Ghost Fence Dynamics: Monthly Trends (Post-Removal)",
        subtitle = "Comparison of Crossings (Foraging/Movement) and Bounces over time",
        x = "Time", y = "Events per Month", color = "Behavior"
    ) +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(face = "bold")
    )

ggsave(file.path(out_dir, "03_Ghost_Fence_Trend.png"), p3, width = 14, height = 8, bg = "white", create.dir = TRUE)

# Microhabitat Analysis
if (length(master_extractions) > 0) {
    df_box <- bind_rows(master_extractions) %>%
        pivot_longer(cols = starts_with("dist_"), names_to = "Veg_Type", values_to = "Distance") %>%
        filter(!is.na(Distance))
    p_box <- ggplot(df_box, aes(x = behavior, y = Distance, fill = behavior)) +
        geom_boxplot(outlier.alpha = 0.05) +
        facet_grid(Veg_Type ~ Stage, scales = "free_y") +
        scale_fill_manual(values = c("Sleeping" = "#1A1A1A", "Resting" = "#969696", "Foraging" = "#D95F02", "Movement" = "#377EB8", "Bounce" = "#E41A1C")) +
        theme_minimal(base_size = 9) +
        theme(strip.text.y = element_text(angle = 0))
    ggsave(file.path(out_dir, "04_Microhabitat_BACI.png"), p_box, width = 12, height = 18, bg = "white", create.dir = TRUE)
}

write_csv(bind_rows(accuracy_report), file.path(out_dir, "Accuracy_Summary.csv"))

# ==============================================================================
# 8. Request: Journal Style Trajectory Maps (WGS84)
# ==============================================================================
cat("\nGenerating Journal-Style Trajectory Maps (WGS84)...\n")
for (ele_id in names(full_results_sf)) {
    # Transform to WGS84 for degree labels
    data_ele <- full_results_sf[[ele_id]] %>%
        st_transform(4326) %>%
        mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

    # Filter for Foraging, Movement, and Resting
    data_plot <- data_ele %>% filter(behavior %in% c("Foraging", "Movement", "Resting"))

    for (stg in c("pre", "post")) {
        data_stg <- data_plot %>% filter(Stage == stg)
        if (nrow(data_stg) < 5) next

        cat(sprintf("  Plotting Journal Map: %s | %s\n", ele_id, stg))

        p_journal <- ggplot() +
            # Trajectories (connected points)
            geom_path(data = data_stg, aes(x = lon, y = lat, color = behavior), alpha = 0.4, linewidth = 0.3) +
            geom_point(data = data_stg, aes(x = lon, y = lat, color = behavior), size = 0.5, alpha = 0.6) +
            # Fence Line - ONLY for "pre" as requested
            {
                if (stg == "pre") geom_sf(data = fence_sf %>% st_transform(4326), color = "black", linetype = "dashed", linewidth = 1.1)
            } +
            # Scales & Styling
            scale_color_manual(values = c("Sleeping" = "#1A1A1A", "Resting" = "#969696", "Foraging" = "#D95F02", "Movement" = "#377EB8", "Bounce" = "#E41A1C")) +
            scale_x_continuous(labels = function(x) paste0(round(x, 2), "°E")) +
            scale_y_continuous(labels = function(y) paste0(abs(round(y, 2)), "°S")) +
            labs(
                title = sprintf("BACI Spatial Design: %s-Fence (%s)", toupper(stg), ele_id),
                subtitle = "Elephant Trajectory vs Fence Line (dashed)",
                x = "Easting", y = "Northing",
                color = "Behavior"
            ) +
            theme_minimal() +
            theme(
                legend.position = "bottom",
                panel.grid.major = element_line(color = "gray95"),
                panel.grid.minor = element_blank(),
                plot.title = element_text(size = 16, face = "plain"),
                axis.title = element_text(face = "plain"),
                panel.background = element_rect(fill = "white", color = NA),
                plot.background = element_rect(fill = "white", color = NA)
            ) +
            coord_sf()

        ggsave(file.path(out_dir, sprintf("Journal_Trajectory_%s_%s.png", ele_id, stg)), p_journal, width = 10, height = 10, dpi = 300, create.dir = TRUE)
    }
}

cat("\nBACI Analysis Complete. Results in:", out_dir, "\n")
