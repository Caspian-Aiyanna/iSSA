#!/usr/bin/env Rscript
# ==============================================================================
# H2O SDM per Elephant and Behavior (10 Repetitions, 30 Models)
# Task:
# 1. Save behavioral points as CSVs per elephant/behavior.
# 2. Train 10 repetitions of H2O AutoML (30 models each) per group.
# 3. Calculate ensemble mean for each elephant/behavioral niche.
# ==============================================================================

suppressPackageStartupMessages({
    library(h2o)
    library(terra)
    library(sf)
    library(dplyr)
    library(readr)
    library(tidyr)
})

# --- setup paths ---
root_dir <- getwd()
cache_file <- file.path(root_dir, "BioHabs", "results", "RSF", "hmm_results_all_elephants.rds")
env_a_dir <- file.path(root_dir, "BioHabs", "data", "envi", "A")
veg_dir <- file.path(root_dir, "BioHabs", "data", "envi", "raw", "Veg class", "dist_rasters")
out_root <- file.path(root_dir, "BioHabs", "results", "Behavioral_SDM_Simple")

if (!dir.exists(file.path(out_root, "data"))) dir.create(file.path(out_root, "data"), recursive = TRUE)

# --- load utils ---
source(file.path(root_dir, "R", "utils_io.R"))
source(file.path(root_dir, "R", "utils_kendall.R"))
source(file.path(root_dir, "R", "utils_h2o.R"))

# --- 1. Load Data ---
cat(">>> Loading Behavioral Data...\n")
if (!file.exists(cache_file)) stop("Cache not found. Run script 05 first.")
full_results_list <- readRDS(cache_file)

# --- 2. Load Environmental Stack (41 layers) ---
cat(">>> Loading Environmental Stack (41 layers)...\n")
Renv <- c(
    terra::rast(list.files(veg_dir, pattern = "\\.tif$", full.names = TRUE)),
    terra::rast(list.files(env_a_dir, pattern = "\\.tif$", full.names = TRUE))
)
names(Renv) <- make.names(names(Renv), unique = TRUE)

# --- Quick NA Check ---
cat("Checking for empty/low-coverage layers...\n")
samp_pts <- spatSample(Renv[[1]], 1000, "random", as.points = TRUE, na.rm = TRUE)
samp_vals <- terra::extract(Renv, samp_pts)[, -1]
bad_layers <- names(samp_vals)[colSums(!is.na(samp_vals)) < 100]
if (length(bad_layers) > 0) {
    cat(sprintf("  Dropping %d layers with insufficient data: %s\n", length(bad_layers), paste(bad_layers, collapse = ", ")))
    Renv <- Renv[[setdiff(names(Renv), bad_layers)]]
}
cat(sprintf("  Successfully loaded %d layers.\n", nlyr(Renv)))

# --- 2. Variable Selection (Global) ---
cat(">>> Performing Kendall Pruning (cutoff = 0.8)...\n")
keep_vars <- kendall_plan(run = "Simple_Behavioral", r = Renv, out_dir = file.path(out_root, "kendall_plan"))
Renv_filt <- Renv[[keep_vars]]

# --- 3. Initialize H2O ---
h2o.init(nthreads = -1, max_mem_size = "16G")
h2o.removeAll()

# --- 4. Main Processing Loop ---
ele_ids <- names(full_results_list)
behaviors <- c("Resting", "Foraging", "Movement")
n_reps <- 10

for (ele_id in ele_ids) {
    cat(sprintf("\n--- Processing Elephant: %s ---\n", ele_id))
    df_ele <- full_results_list[[ele_id]]
    df_proj <- st_transform(df_ele, crs(Renv))

    for (bh in behaviors) {
        cat(sprintf("  Behavior: %s\n", bh))
        bh_pts <- df_proj %>% filter(behavior == bh)
        if (nrow(bh_pts) < 30) {
            cat(sprintf("    Skipping %s: Too few points.\n", bh))
            next
        }

        # Save Base CSV
        pres_df_base <- bh_pts %>%
            mutate(lon = as.numeric(st_coordinates(.)[, 1]), lat = as.numeric(st_coordinates(.)[, 2])) %>%
            st_drop_geometry() %>%
            as.data.frame() %>%
            select(lon, lat) %>%
            mutate(pa = 1)
        write_csv(pres_df_base, file.path(out_root, "data", sprintf("%s_%s.csv", ele_id, bh)))

        mod_dir <- file.path(out_root, ele_id, bh)
        if (!dir.exists(file.path(mod_dir, "reps"))) dir.create(file.path(mod_dir, "reps"), recursive = TRUE)

        cat(sprintf("    Training %d repetitions with 30 models each...\n", n_reps))

        for (rep_i in 1:n_reps) {
            rep_path <- file.path(mod_dir, "reps", sprintf("suitability_rep_%02d.tif", rep_i))
            if (file.exists(rep_path)) next

            # Rep-specific background sampling
            bg_pts <- terra::spatSample(Renv_filt[[1]], size = 5000, method = "random", as.points = TRUE, na.rm = TRUE)
            bg_df <- as.data.frame(crds(bg_pts)) %>%
                rename(lon = x, lat = y) %>%
                mutate(pa = 0)
            train_coords <- as.data.frame(bind_rows(pres_df_base, bg_df))

            # Extract predictors
            v_train <- terra::extract(Renv_filt, vect(train_coords, geom = c("lon", "lat"), crs = crs(Renv_filt)))
            v_train <- v_train[, -1, drop = FALSE]
            train_final <- bind_cols(train_coords, v_train) %>% drop_na()

            if (sum(train_final$pa == 1) < 20) next

            hf <- as.h2o(train_final)
            hf[["pa"]] <- h2o.asfactor(hf[["pa"]])

            aml <- h2o.automl(
                x = keep_vars, y = "pa",
                training_frame = hf,
                max_models = 30,
                max_runtime_secs = 120, # Fast limit
                seed = rep_i,
                sort_metric = "AUC"
            )

            leader <- aml@leader
            if (!is.null(leader)) {
                # Predict and Save Rep
                pred_r <- predict_raster_h2o(Renv_filt, leader, block_rows = 50000)
                writeRaster(pred_r, rep_path, overwrite = TRUE)
                h2o.rm(hf)
            }
        }

        # Calculate Ensemble Mean for this niche
        cat(sprintf("    Calculating Ensemble Mean for %s %s...\n", ele_id, bh))
        rep_files <- list.files(file.path(mod_dir, "reps"), pattern = "\\.tif$", full.names = TRUE)
        if (length(rep_files) > 0) {
            ens_mean <- mean(terra::rast(rep_files), na.rm = TRUE)
            writeRaster(ens_mean, file.path(mod_dir, sprintf("suitability_%s_%s_mean.tif", ele_id, bh)), overwrite = TRUE)

            # Visual check
            png(file.path(mod_dir, "ensemble_preview.png"), width = 800, height = 800)
            plot(ens_mean, main = sprintf("Mean: %s %s", ele_id, bh), col = viridis::viridis(100))
            dev.off()
        }
    }
}

cat("\nBehavioral H2O Ensemble Analysis (10 reps) Complete.\n")
h2o.shutdown(prompt = FALSE)
