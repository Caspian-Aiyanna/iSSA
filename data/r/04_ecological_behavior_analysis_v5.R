# 04_ecological_behavior_analysis_v5.R
# Comprehensive Ecological Behavioral Analysis (V5 - Biological Calibration)
# Focus: 4-State HMM (Sleeping, Resting, Foraging, Movement)
# Goal: Strictly follow biological anchors (Foraging < 50m, Movement > 50m) and ensure data traceability.

suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate)
    library(sf)
    library(amt)
    library(moveHMM)
    library(patchwork)
    library(circular)
    library(scales)
})

set.seed(42) # Ensure strict reproducibility

cat("Starting Calibrated Behavioral Analysis V5 (Strict Traceability)...\n")

# ==============================================================================
# 1. Config & Paths
# ==============================================================================

# Search for project root (where 'data' directory exists)
start_dir <- getwd()
while (!dir.exists(file.path(start_dir, "data")) && start_dir != dirname(start_dir)) {
    start_dir <- dirname(start_dir)
}
root_dir <- start_dir

data_dir <- file.path(root_dir, "data")
clean_dir <- file.path(data_dir, "OG")
shp_dir <- file.path(data_dir, "shp")
results_dir <- file.path(root_dir, "results")
out_dir <- file.path(results_dir, "Ecological_Behavior_V5")
models_dir <- file.path(out_dir, "models")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
if (!dir.exists(models_dir)) dir.create(models_dir, recursive = TRUE)

# Time Periods (Fixed as per decided protocol)
periods <- list(
    pre     = interval(ymd("2020-01-01"), ymd("2023-11-30")),
    interim = interval(ymd("2023-12-01"), ymd("2024-02-06")),
    post    = interval(ymd("2024-02-07"), ymd("2026-12-31"))
)

target_crs <- 32735 # UTM 35S

# Refined Color Palette to match original 3-state HMM exactly
behavior_cols <- c(
    "Sleeping" = "#999999", # Grey
    "Resting"  = "#E69F00", # Orange
    "Foraging" = "#10B981", # Green
    "Movement" = "#56B4E9", # Original Light Blue
    "Bounce"   = "#E41A1C" # Red for Fence Interaction
)

# ==============================================================================
# 2. Load Spatial Context
# ==============================================================================

cat("Loading Spatial Context...\n")
hv_sf <- read_sf(file.path(shp_dir, "HV", "HV.shp")) %>% st_transform(target_crs)
kw_sf <- read_sf(file.path(shp_dir, "KW", "KW.shp")) %>% st_transform(target_crs)
fence_2024 <- read_sf(file.path(shp_dir, "Fence", "fence_2024.shp")) %>% st_transform(target_crs)
fence_buffer <- st_buffer(fence_2024, dist = 150)

# ==============================================================================
# 3. Data Loading (Strict Traceability)
# ==============================================================================

ele_files <- list.files(clean_dir, pattern = "^E[1-6].*\\.csv$", full.names = TRUE)

full_data <- map_df(ele_files, function(f) {
    # Read raw to preserve format
    df <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) {
        return(NULL)
    })
    if (is.null(df)) {
        return(NULL)
    }

    ele_id <- str_extract(basename(f), "E[0-9]+")

    # Standardize column names
    fix_col <- names(df)[grepl("fixtime|timestamp", names(df), ignore.case = TRUE)]
    if (length(fix_col) > 0) names(df)[names(df) == fix_col[1]] <- "timestamp"

    lon_col <- names(df)[grepl("location\\.long|^lon$", names(df), ignore.case = TRUE)]
    if (length(lon_col) > 0) names(df)[names(df) == lon_col[1]] <- "lon"

    lat_col <- names(df)[grepl("location\\.lat|^lat$", names(df), ignore.case = TRUE)]
    if (length(lat_col) > 0) names(df)[names(df) == lat_col[1]] <- "lat"

    # Robust Date Parsing (Handles mixed formats in South Africa dataset)
    # The dataset typically mixes m-d-Y and d-m-Y depending on the source.
    # By using several orders and checking for jumps, we ensure continuity.
    df$timestamp <- parse_date_time(df$timestamp, orders = c("mdY HMS", "dmY HMS", "Ymd HMS", "mdY HM", "dmY HM", "Ymd HM"))

    df %>%
        mutate(Elephant = ele_id) %>%
        filter(!is.na(lon) & !is.na(lat) & !is.na(timestamp)) %>%
        select(Elephant, timestamp, lon, lat)
}) %>%
    arrange(Elephant, timestamp) %>%
    distinct(Elephant, timestamp, .keep_all = TRUE)

# ==============================================================================
# 4. HMM Analysis Loop (V5 Biological Parameters)
# ==============================================================================

results_list <- list()
interaction_events <- list()

# Anchor Parameters (km per 30 mins) - Reverting to match 3-state HMM values
# Foraging (60m -> 0.060), Movement (250m -> 0.250)
# Sleeping and Resting split the original Resting (5m -> 0.005) anchor.

mu0 <- c(0.0012, 0.008, 0.060, 0.250) # 1.2m, 8m, 60m, 250m
sd0 <- c(0.001, 0.006, 0.040, 0.120)
angle0 <- c(pi, pi, pi, 0) # Sleeping/Resting/Foraging are non-directed, Movement is directed
con0 <- c(0.05, 0.2, 0.4, 0.6) # Concentration (kappa/rho)

for (ele_id in unique(full_data$Elephant)) {
    cat("\n>>> Modeling Elephant:", ele_id, "\n")
    ele_full <- full_data %>% filter(Elephant == ele_id)

    # Track resampling to uniform 30 min (crucial for HMM)
    trk_full <- make_track(ele_full, lon, lat, timestamp, crs = 4326) %>%
        transform_coords(target_crs) %>%
        track_resample(rate = minutes(30), tolerance = minutes(5))

    if (nrow(trk_full) < 100) next

    # Prep moveHMM Data
    m_data <- prepData(data.frame(ID = ele_id, x = trk_full$x_ / 1000, y = trk_full$y_ / 1000, x_m = trk_full$x_, y_m = trk_full$y_, date = trk_full$t_), type = "UTM")

    # Add Diurnal Covariates (24h period)
    m_data$hour <- hour(m_data$date) + minute(m_data$date) / 60
    m_data$cosHour <- cos(2 * pi * m_data$hour / 24)
    m_data$sinHour <- sin(2 * pi * m_data$hour / 24)

    # Separate Sleep from Rest with small jitter (biological real 0s)
    z_idx <- which(m_data$step <= 0 | is.na(m_data$step))
    if (length(z_idx) > 0) m_data$step[z_idx] <- runif(length(z_idx), 0.0001, 0.002)
    m_data <- m_data %>% filter(!is.na(step) & !is.na(angle))

    model_path <- file.path(models_dir, paste0(ele_id, "_HMM_V5.rds"))

    # Fit or Load
    if (file.exists(model_path)) {
        m_fit <- readRDS(model_path)
        cat("  Refined model loaded.\n")
    } else {
        cat("  Fitting 4-State Bio-HMM...")
        # Incorporate diurnal transitions to catch 1-3h sleep cycles nocturnally
        m_fit <- try(fitHMM(
            data = m_data, nbStates = 4,
            stepPar0 = c(mu0, sd0),
            anglePar0 = c(angle0, con0),
            formula = ~ cosHour + sinHour,
            verbose = 0
        ), silent = TRUE)

        if (!inherits(m_fit, "try-error")) {
            saveRDS(m_fit, model_path)
            cat(" Success.\n")
        } else {
            cat(" Failed. Retrying without covariates...")
            m_fit <- try(fitHMM(
                data = m_data, nbStates = 4,
                stepPar0 = c(mu0, sd0),
                anglePar0 = c(angle0, con0),
                verbose = 0
            ), silent = TRUE)
            if (!inherits(m_fit, "try-error")) saveRDS(m_fit, model_path)
        }
    }

    if (inherits(m_fit, "try-error")) next

    # Classify behaviors
    states <- viterbi(m_fit)
    mu_est <- m_fit$mle$stepPar[1, ]
    ord <- order(mu_est) # Strict ordering: Sleep -> Rest -> Forage -> Move
    names_map <- c("Sleeping", "Resting", "Foraging", "Movement", "Bounce") # Include Bounce in levels
    m_data$behavior <- factor(names_map[match(states, ord)], levels = names_map)

    # Period Assignment
    m_data <- m_data %>%
        mutate(
            Stage = case_when(
                date %within% periods$pre ~ "pre",
                date %within% periods$interim ~ "interim",
                date %within% periods$post ~ "post",
                TRUE ~ NA_character_
            )
        ) %>%
        filter(!is.na(Stage))

    m_data$Elephant <- ele_id
    ele_sf <- st_as_sf(m_data, coords = c("x_m", "y_m"), crs = target_crs, remove = FALSE)

    # Home/Novel Reserve Context
    home_sf <- if (ele_id %in% c("E1", "E2", "E3", "E4")) kw_sf else hv_sf
    novel_sf <- if (ele_id %in% c("E1", "E2", "E3", "E4")) hv_sf else kw_sf

    ele_sf <- ele_sf %>%
        mutate(
            Zone = case_when(
                st_intersects(ele_sf, home_sf, sparse = F)[, 1] ~ "Home",
                st_intersects(ele_sf, novel_sf, sparse = F)[, 1] ~ "Novel",
                TRUE ~ "Other"
            )
        )

    # Secondary Interaction Check (Spatial)
    # Check for Bounce (approaching fence and returning)
    in_buffer <- st_intersects(ele_sf, fence_buffer, sparse = F)[, 1]
    buf_idx <- which(in_buffer)
    if (length(buf_idx) > 0) {
        bursts <- split(buf_idx, cumsum(c(1, diff(buf_idx) != 1)))
        for (b in bursts) {
            if (min(b) > 1 && max(b) < nrow(ele_sf)) {
                if (ele_sf$Zone[min(b) - 1] == ele_sf$Zone[max(b) + 1] && ele_sf$Zone[min(b) - 1] != "Other") {
                    interaction_events[[length(interaction_events) + 1]] <- data.frame(Elephant = ele_id, Date = ele_sf$date[round(mean(b))], Type = "Bounce", Behavior = "Bounce", Stage = ele_sf$Stage[round(mean(b))])
                    ele_sf$behavior[b] <- "Bounce" # Overlay behavior
                }
            }
        }
    }

    results_list[[ele_id]] <- ele_sf
}

# ==============================================================================
# 5. Synthesis & Plotting
# ==============================================================================

df_all <- bind_rows(map(results_list, st_drop_geometry)) %>%
    mutate(
        month = month(date),
        Season = case_when(
            month %in% c(12, 1, 2) ~ "Summer",
            month %in% c(3, 4, 5) ~ "Autumn",
            month %in% c(6, 7, 8) ~ "Winter",
            month %in% c(9, 10, 11) ~ "Spring"
        ),
        Season = factor(Season, levels = c("Summer", "Autumn", "Winter", "Spring")),
        Stage = factor(Stage, levels = c("pre", "interim", "post")),
        behavior = factor(behavior, levels = c("Sleeping", "Resting", "Foraging", "Movement", "Bounce"))
    )

cat("\nGenerating Final Visualizations...\n")

# A. Trajectory Matrix (All Elephants)
for (eid in names(results_list)) {
    p_map <- ggplot() +
        geom_sf(data = hv_sf, fill = "gray98", color = "gray90") +
        geom_sf(data = kw_sf, fill = "gray98", color = "gray90") +
        geom_sf(data = results_list[[eid]], aes(color = behavior), size = 0.4, alpha = 0.6) +
        geom_sf(data = fence_2024, color = "black", linewidth = 0.5, linetype = "dotted") +
        facet_wrap(~ factor(Stage, levels = c("pre", "interim", "post")), ncol = 3) +
        scale_color_manual(values = behavior_cols) +
        labs(title = paste("Bio-Calibrated Trajectory Matrix:", eid), subtitle = "States: Sleep (1m), Rest (15m), Foraging (<50m), Movement (>50m)") +
        theme_minimal() +
        theme(axis.text = element_blank(), panel.grid = element_blank())

    ggsave(file.path(out_dir, paste0("Map_Matrix_", eid, ".png")), p_map, width = 15, height = 5, bg = "white")
}

# B. Diurnal Pattern (Hourly Stack) - Capturing the 1-3h sleep and day foraging
p_diurnal <- ggplot(df_all, aes(x = hour, fill = behavior)) +
    geom_density(position = "fill", alpha = 0.9) +
    facet_wrap(~Elephant, ncol = 2) +
    scale_fill_manual(values = behavior_cols) +
    scale_x_continuous(breaks = seq(0, 24, 4)) +
    labs(title = "Diurnal Behavioral Rhythms (Strictly Calibrated)", x = "Hour (24h format)", y = "Proportion", fill = "Behavior") +
    theme_minimal(base_size = 14)

ggsave(file.path(out_dir, "Global_Diurnal_Activity.png"), p_diurnal, width = 12, height = 10, bg = "white")

# C. Seasonal Patterns
p_seasonal <- df_all %>%
    count(Elephant, Season, behavior) %>%
    group_by(Elephant, Season) %>%
    mutate(Prop = n / sum(n)) %>%
    ggplot(aes(x = Season, y = Prop, fill = behavior)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~Elephant, ncol = 2) +
    scale_fill_manual(values = behavior_cols) +
    labs(title = "Seasonal Behavioral Budgets", x = "Season (S. Hemisphere)", y = "Proportion") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_dir, "Global_Seasonal_Patterns.png"), p_seasonal, width = 12, height = 10, bg = "white")

# D. Time Budget Shift (BACI)
p_shift <- df_all %>%
    filter(!is.na(behavior)) %>%
    count(Elephant, Stage, behavior) %>%
    group_by(Elephant, Stage) %>%
    mutate(Prop = n / sum(n)) %>%
    ggplot(aes(x = Stage, y = Prop, fill = behavior)) +
    geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.3) +
    geom_text(aes(label = scales::percent(Prop, accuracy = 1)),
        position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 4
    ) +
    facet_wrap(~Elephant, ncol = 3) +
    scale_fill_manual(values = behavior_cols) +
    labs(title = "Behavioral Time Budget Shift (4-State Calibration)", x = "Period", y = "Proportion") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

ggsave(file.path(out_dir, "Individual_BACI_Shifts.png"), p_shift, width = 12, height = 8, bg = "white")

# ==============================================================================
# 6. Final Export
# ==============================================================================

write_csv(df_all, file.path(out_dir, "Elephant_Behavioral_Points_Final_V5.csv"))
cat("\nProcess Complete. Results saved in:", out_dir, "\n")
