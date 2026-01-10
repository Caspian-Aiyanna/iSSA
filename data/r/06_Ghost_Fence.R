# 06_Ghost_Fence.R
# Core Idea: Analyse the presence of Ghost Fence effect for E3 and E4 by comparing PRE and POST periods.
# Logic: Create three 50m, 100m, and 200m buffer zones around the fence line and collect all GPS telemetry points.
# Compare interaction rates (crossings and bounces) and behavioral composition between PRE and POST.

suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate)
    library(sf)
    library(patchwork)
    library(scales)
})

# ==============================================================================
# 1. Config & Paths
# ==============================================================================

root_dir <- getwd()
data_dir <- file.path(root_dir, "data")
clean_dir <- file.path(data_dir, "clean")
shp_dir <- file.path(data_dir, "shp")
results_dir <- file.path(root_dir, "results")
ghost_results_dir <- file.path(results_dir, "Ghost_Fence")
behavior_dir <- file.path(results_dir, "RSF", "behavioral_points")

if (!dir.exists(ghost_results_dir)) dir.create(ghost_results_dir, recursive = TRUE)

# CRS
target_crs <- 32735 # UTM 35S

# Define Time Periods
# PRE: Before Dec 9, 2023
# POST: After Feb 9, 2024
periods <- list(
    PRE = list(start = ymd("2022-01-01"), end = ymd("2023-12-08")),
    POST = list(start = ymd("2024-02-10"), end = ymd("2026-01-01"))
)

# ==============================================================================
# 2. Load Spatial Data
# ==============================================================================

cat("Loading Spatial Data...\n")

# Reserve Boundaries
hv_sf <- read_sf(file.path(shp_dir, "HV", "HV.shp")) %>% st_transform(target_crs)
kw_sf <- read_sf(file.path(shp_dir, "KW", "KW.shp")) %>% st_transform(target_crs)

# Fence Line
fence_path <- file.path(shp_dir, "Fence", "fence_2024.shp")
if (!file.exists(fence_path)) stop("Fence shapefile not found at: ", fence_path)

fence_line <- read_sf(fence_path) %>%
    st_transform(target_crs) %>%
    st_cast("MULTILINESTRING")

# Create Buffers
buffers <- list(
    buf_50 = st_buffer(fence_line, dist = 50),
    buf_100 = st_buffer(fence_line, dist = 100),
    buf_200 = st_buffer(fence_line, dist = 200)
)

# ==============================================================================
# 3. Load & Process Telemetry Data
# ==============================================================================

elephants <- c("E3", "E4")
all_event_data <- list()
all_point_data <- list()

for (ele_id in elephants) {
    cat("\n=== Processing Elephant:", ele_id, "===\n")

    file_path <- file.path(behavior_dir, paste0(ele_id, "_behavioral_points.csv"))
    if (!file.exists(file_path)) {
        cat("  Behavioral points file not found for", ele_id, ". Skipping.\n")
        next
    }

    # Load and classify points into periods
    ele_data_raw <- read_csv(file_path, show_col_types = FALSE) %>%
        mutate(date = as_datetime(date))

    ele_data <- ele_data_raw %>%
        mutate(Period = case_when(
            date >= periods$PRE$start & date <= periods$PRE$end ~ "PRE",
            date >= periods$POST$start & date <= periods$POST$end ~ "POST",
            TRUE ~ NA_character_
        )) %>%
        filter(!is.na(Period)) %>%
        arrange(date)

    if (nrow(ele_data) == 0) {
        cat("  No data found in defined periods for", ele_id, ". Skipping.\n")
        next
    }

    # Convert to SF
    ele_sf <- st_as_sf(ele_data, coords = c("x_m", "y_m"), crs = target_crs, remove = FALSE)

    # Identify Side (KW or HV)
    is_kw <- st_intersects(ele_sf, kw_sf, sparse = FALSE)[, 1]
    is_hv <- st_intersects(ele_sf, hv_sf, sparse = FALSE)[, 1]

    ele_sf$side <- "Other"
    ele_sf$side[is_kw] <- "KW_Home"
    ele_sf$side[is_hv] <- "HV_New"

    # Point classification for buffers
    ele_sf$in_50m <- st_intersects(ele_sf, buffers$buf_50, sparse = FALSE)[, 1]
    ele_sf$in_100m <- st_intersects(ele_sf, buffers$buf_100, sparse = FALSE)[, 1]
    ele_sf$in_200m <- st_intersects(ele_sf, buffers$buf_200, sparse = FALSE)[, 1]

    all_point_data[[ele_id]] <- ele_sf %>% st_drop_geometry()

    # Event Analysis for each Period and Distance
    for (p_name in c("PRE", "POST")) {
        ele_p <- ele_sf %>% filter(Period == p_name)
        if (nrow(ele_p) < 10) next

        for (dist in c(50, 100, 200)) {
            buf_col <- paste0("in_", dist, "m")
            in_buf <- ele_p[[buf_col]]
            sides <- ele_p$side

            # 1. Crossing Analysis
            crossings <- which(sides[-1] != sides[-length(sides)] &
                sides[-1] %in% c("KW_Home", "HV_New") &
                sides[-length(sides)] %in% c("KW_Home", "HV_New"))

            if (length(crossings) > 0) {
                all_event_data[[length(all_event_data) + 1]] <- data.frame(
                    Elephant = ele_id,
                    Period = p_name,
                    BufferDistance = dist,
                    Type = "Crossing",
                    Date = ele_p$date[crossings + 1],
                    Behavior = ele_p$behavior[crossings + 1]
                )
            }

            # 2. Bounce Analysis
            bursts <- which(in_buf)
            if (length(bursts) > 0) {
                burst_ids <- cumsum(c(1, diff(bursts) != 1))
                burst_list <- split(bursts, burst_ids)

                for (b in burst_list) {
                    first <- min(b)
                    last <- max(b)

                    if (first > 1 && last < nrow(ele_p)) {
                        side_before <- sides[first - 1]
                        side_after <- sides[last + 1]

                        if (side_before == side_after && side_before %in% c("KW_Home", "HV_New")) {
                            all_event_data[[length(all_event_data) + 1]] <- data.frame(
                                Elephant = ele_id,
                                Period = p_name,
                                BufferDistance = dist,
                                Type = "Bounce",
                                Date = ele_p$date[round(mean(c(first, last)))],
                                Behavior = ele_p$behavior[first]
                            )
                        }
                    }
                }
            }
        }
    }
}

# ==============================================================================
# 4. Aggregation and Summarization
# ==============================================================================

if (length(all_event_data) == 0) {
    stop("No crossing or bounce events detected.")
}

df_events <- bind_rows(all_event_data)

# Calculate Daily Rates (Events per Day)
period_durations <- bind_rows(all_point_data) %>%
    group_by(Elephant, Period) %>%
    summarise(
        Days = as.numeric(difftime(max(date), min(date), units = "days")),
        .groups = "drop"
    ) %>%
    filter(Days > 0)

df_total_rates <- df_events %>%
    group_by(Elephant, Period, BufferDistance, Type) %>%
    summarise(EventCount = n(), .groups = "drop") %>%
    left_join(period_durations, by = c("Elephant", "Period")) %>%
    mutate(EventRate = EventCount / Days)

# Behavioral composition in interaction zones
df_behavior <- bind_rows(all_point_data) %>%
    pivot_longer(cols = starts_with("in_"), names_to = "BufferRange", values_to = "Inside") %>%
    filter(Inside) %>%
    mutate(BufferDistance = as.numeric(str_extract(BufferRange, "\\d+"))) %>%
    group_by(Elephant, Period, BufferDistance, behavior) %>%
    summarise(PointCount = n(), .groups = "drop") %>%
    group_by(Elephant, Period, BufferDistance) %>%
    mutate(Proportion = PointCount / sum(PointCount))

# ==============================================================================
# 5. Visualizations
# ==============================================================================

cat("\nGenerating Visualizations...\n")

# Plot 1: Interaction Rate Comparison (PRE vs POST)
p_interaction <- ggplot(
    df_total_rates %>% filter(BufferDistance == 100),
    aes(x = Period, y = EventRate, fill = Type)
) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Elephant) +
    labs(
        title = "Ghost Fence Effect: Interaction Rates (PRE vs POST)",
        subtitle = "Events per Day within 100m of the fence line",
        y = "Events per Day",
        x = "Period",
        fill = "Event Type"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Crossing" = "#009E73", "Bounce" = "#D55E00"))

# Plot 2: Behavioral Budget Shift (PRE vs POST)
p_behavior_shift <- ggplot(
    df_behavior %>% filter(BufferDistance == 100),
    aes(x = Period, y = Proportion, fill = behavior)
) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~Elephant) +
    scale_y_continuous(labels = percent) +
    labs(
        title = "Behavioral Shift in Interaction Zone (100m)",
        subtitle = "Composition of states before and after fence removal",
        y = "Proportion of Points",
        x = "Period",
        fill = "Behavioral State"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Resting" = "#999999", "Foraging" = "#E69F00", "Movement" = "#56B4E9"))

# Plot 3: Distance Sensitivity Comparison
p_dist_compare <- ggplot(df_total_rates, aes(x = factor(BufferDistance), y = EventRate, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(Elephant ~ Period) +
    labs(
        title = "Sensitivity to Buffer Distance",
        subtitle = "Daily rate of interactions across different buffer widths",
        x = "Buffer Distance (meters)",
        y = "Events per Day",
        fill = "Event Type"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Crossing" = "#009E73", "Bounce" = "#D55E00"))

# Save Plots
ggsave(file.path(ghost_results_dir, "01_Interaction_Rate_Comparison.png"), p_interaction, width = 10, height = 6, bg = "white")
ggsave(file.path(ghost_results_dir, "02_Behavioral_Shift_Buffers.png"), p_behavior_shift, width = 10, height = 6, bg = "white")
ggsave(file.path(ghost_results_dir, "03_Distance_Sensitivity_BACI.png"), p_dist_compare, width = 12, height = 8, bg = "white")

# Spatial Point Comparison Map
ele_sf_all <- bind_rows(all_point_data) %>%
    filter(in_200m) %>%
    st_as_sf(coords = c("x_m", "y_m"), crs = target_crs)

p_map_baci <- ggplot() +
    geom_sf(data = kw_sf, fill = "grey95", color = "grey80") +
    geom_sf(data = hv_sf, fill = "grey90", color = "grey80") +
    geom_sf(data = fence_line, color = "red", linewidth = 1) +
    geom_sf(data = ele_sf_all, aes(color = behavior), size = 0.5, alpha = 0.4) +
    facet_grid(Elephant ~ Period) +
    coord_sf(xlim = st_bbox(buffers$buf_200)[c(1, 3)], ylim = st_bbox(buffers$buf_200)[c(2, 4)]) +
    scale_color_manual(values = c("Resting" = "#999999", "Foraging" = "#E69F00", "Movement" = "#56B4E9")) +
    labs(
        title = "Spatial Point Comparison: Near Fence Line",
        subtitle = "PRE (Fixed Fence) vs POST (Ghost Fence) - 200m Buffer",
        color = "Behavior"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

ggsave(file.path(ghost_results_dir, "04_Spatial_Point_Comparison_BACI.png"), p_map_baci, width = 12, height = 10, bg = "white")

# ==============================================================================
# 6. Save Data Results
# ==============================================================================

write_csv(df_total_rates, file.path(ghost_results_dir, "Ghost_Fence_Rates_Comparison.csv"))
write_csv(df_behavior, file.path(ghost_results_dir, "Ghost_Fence_Behavior_BACI.csv"))

cat("\nBACI Analysis complete. Results saved in:", ghost_results_dir, "\n")
