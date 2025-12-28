# 03_ghost_fence_analysis.R
# BACI Design for "Ghost Fence" Permeability Hypothesis
# Investigates if elephants exhibit behavioral lag (bounces vs crossings) after fence removal.
# Also tests if the "New Zone" is treated as a transit corridor (Movement state) vs resource hub (Foraging state).

suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate)
    library(sf)
    library(terra)
    library(amt)
    library(moveHMM)
    library(patchwork)
})

# ==============================================================================
# 1. Config & Paths
# ==============================================================================

# ==============================================================================
# 1. Config & Paths
# ==============================================================================

# Robust Path Detection
if (dir.exists("BioHabs")) {
    root_dir <- getwd()
} else {
    # Assume running from playground or deeper
    root_dir <- dirname(dirname(getwd()))
}

clean_dir <- file.path(root_dir, "BioHabs", "data", "clean")
shp_dir <- file.path(root_dir, "BioHabs", "data", "shp")
out_dir <- file.path(root_dir, "BioHabs", "playground", "03_Ghost_Fence_Results")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Time Periods
periods <- list(
    pre = interval(dmy("04-08-2022"), dmy("09-12-2023")),
    interim = interval(dmy("10-12-2023"), dmy("08-01-2024")),
    post = interval(dmy("09-01-2024"), dmy("14-07-2025"))
)

# CRS
target_crs <- 32735 # UTM 35S

# ==============================================================================
# 2. Load & Prepare Geometry (Fence Line)
# ==============================================================================

cat("Loading Spatial Context...\n")

# Load Polygons for Zone Definition
hv_path <- file.path(shp_dir, "HV", "HV.shp")
kw_path <- file.path(shp_dir, "KW", "KW.shp")

if (!file.exists(hv_path) || !file.exists(kw_path)) {
    stop("Shapefiles KW or HV not found. Please check paths.")
}

hv_sf <- read_sf(hv_path) %>% st_transform(target_crs)
kw_sf <- read_sf(kw_path) %>% st_transform(target_crs)

cat("Loading Specific Fence Line...\n")
fence_path <- file.path(shp_dir, "Fence", "fence_2024.shp")
if (!file.exists(fence_path)) stop("Fence not found")

fence_line <- read_sf(fence_path) %>%
    st_transform(target_crs) %>%
    st_cast("MULTILINESTRING")

# Define Fence Buffer (Interaction Zone)
buffer_dist <- 250 # meters (Increased to capture interactions at 30min resolution)
fence_buffer <- st_buffer(fence_line, dist = buffer_dist)

# ==============================================================================
# 3. Data Loading & HMM
# ==============================================================================

time_budget_data <- list()
interaction_data <- list()

# Fix Duplicate Files
all_files <- list.files(clean_dir, pattern = "^E[34][AB]?\\.csv$", full.names = TRUE, recursive = TRUE)
ele_files <- all_files[!duplicated(basename(all_files))]
cat("Unique Files Found:", length(ele_files), "\n")

# Load All Data First
cat("Loading Telemetry Data...\n")
ele_data_all <- map_df(ele_files, function(f) {
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
    filter(!is.na(lon) & !is.na(lat) & !is.na(timestamp)) %>%
    distinct(Elephant, timestamp, .keep_all = TRUE) # Exact dedup

# Process Each Elephant
for (ele_id in unique(ele_data_all$Elephant)) {
    cat("\n=== Processing Elephant:", ele_id, "===\n")
    ele_sub <- ele_data_all %>% filter(Elephant == ele_id)

    # ---------------------------------------------------------
    # A. HMM Fitting on FULL dataset (Better stability)
    # ---------------------------------------------------------
    trk_sf <- make_track(ele_sub, lon, lat, timestamp, crs = 4326)
    trk_utm <- transform_coords(trk_sf, crs_to = target_crs)

    # Resample
    trk_res <- tryCatch(
        {
            trk_utm %>% track_resample(rate = minutes(30), tolerance = minutes(5))
        },
        error = function(e) NULL
    )

    if (is.null(trk_res)) {
        cat("  Resampling failed for entire track. Skipping.\n")
        next
    }

    # Fit HMM
    df_hmm <- data.frame(ID = ele_id, x = trk_res$x_, y = trk_res$y_, date = trk_res$t_)
    hmm_dat <- prepData(df_hmm, type = "UTM")
    hmm_dat <- hmm_dat %>% filter(!is.na(step) & !is.na(angle) & step > 0)

    # Simple defaults
    mean_sl <- mean(hmm_dat$step, na.rm = TRUE)
    m_hmm <- try(fitHMM(
        data = hmm_dat, nbStates = 2,
        stepPar0 = c(mean_sl * 0.2, mean_sl * 2.0, mean_sl * 0.2, mean_sl * 1.5),
        anglePar0 = c(pi, 0, 0.5, 2.0),
        formula = ~1, verbose = 0
    ))

    if (inherits(m_hmm, "try-error")) {
        cat("  HMM Fit Failed. Skipping.\n")
        next
    }

    states <- viterbi(m_hmm)
    mus <- m_hmm$mle$stepPar[1, ]
    state_labels <- if (mus[1] > mus[2]) c("Movement", "Foraging") else c("Foraging", "Movement")

    hmm_dat$state <- factor(state_labels[states], levels = c("Foraging", "Movement"))

    # Re-convert to SF for spatial ops (Keep Coords for Plotting)
    hmm_sf <- st_as_sf(hmm_dat, coords = c("x", "y"), crs = target_crs, remove = FALSE)

    # ---------------------------------------------------------
    # B. Identify Home Zone vs New Zone
    # ---------------------------------------------------------
    # Get Pre-period points
    pre_pts <- hmm_sf %>% filter(date %within% periods$pre)

    if (nrow(pre_pts) == 0) {
        cat("  No Pre-period data. Cannot determine Home Zone.\n")
        next
    }

    counts_hv <- st_intersects(pre_pts, hv_sf, sparse = FALSE) %>% sum()
    counts_kw <- st_intersects(pre_pts, kw_sf, sparse = FALSE) %>% sum()

    if (counts_hv > counts_kw) {
        home_zone <- hv_sf
        new_zone <- kw_sf
        home_name <- "HV"
        new_name <- "KW"
    } else {
        home_zone <- kw_sf
        new_zone <- hv_sf
        home_name <- "KW"
        new_name <- "HV"
    }
    cat("  Home Zone identified as:", home_name, "\n")

    # ---------------------------------------------------------
    # C. Interaction Analysis (Event-Based with Timestamps)
    # ---------------------------------------------------------
    # We iterate by period
    for (stage_name in c("pre", "post")) {
        p_int <- periods[[stage_name]]
        stage_pts <- hmm_sf %>% filter(date %within% p_int)

        if (nrow(stage_pts) < 10) next

        # Spatial Classification
        is_home <- st_intersects(stage_pts, home_zone, sparse = FALSE)[, 1]
        is_new <- st_intersects(stage_pts, new_zone, sparse = FALSE)[, 1]
        is_buffer <- st_intersects(stage_pts, fence_buffer, sparse = FALSE)[, 1]

        # State Sequence (H = Home, N = New, O = Buffer/Other)
        z_state <- rep("O", nrow(stage_pts))
        z_state[is_home] <- "H"
        z_state[is_new] <- "N"

        # 1. CROSSINGS: Robust Transition Detection
        valid_idx <- which(z_state %in% c("H", "N"))

        if (length(valid_idx) > 1) {
            v_zones <- z_state[valid_idx]
            switch_locs <- which(v_zones[-1] != v_zones[-length(v_zones)])

            if (length(switch_locs) > 0) {
                # We attribute the event time to the point *after* the switch
                event_indices <- valid_idx[switch_locs + 1]

                crossing_events <- data.frame(
                    Elephant = ele_id,
                    Stage = stage_name,
                    Type = "Crossing",
                    # Capture the behavioral state associated with the crossing step
                    Behavior = stage_pts$state[event_indices], # Foraging or Movement
                    Date = stage_pts$date[event_indices]
                )
                interaction_data[[paste(ele_id, stage_name, "cross")]] <- crossing_events
            }
        }

        # 2. BOUNCES: Buffer Bursts returning to Home
        burst_idx <- which(is_buffer)

        if (length(burst_idx) > 0) {
            bursts <- split(burst_idx, cumsum(c(1, diff(burst_idx) != 1)))

            bounce_dates <- c()
            for (b in bursts) {
                first <- min(b)
                last <- max(b)

                if (first > 1 && last < nrow(stage_pts)) {
                    prev_z <- z_state[first - 1]
                    next_z <- z_state[last + 1]

                    if (prev_z == "H" && next_z == "H") {
                        mid_pt <- round(mean(c(first, last)))
                        bounce_dates <- c(bounce_dates, stage_pts$date[mid_pt])
                    }
                }
            }
            if (length(bounce_dates) > 0) {
                bounce_events <- data.frame(
                    Elephant = ele_id,
                    Stage = stage_name,
                    Type = "Bounce",
                    Behavior = "Bounce", # Bounces are their own behavior class for now
                    Date = as_datetime(bounce_dates)
                )
                interaction_data[[paste(ele_id, stage_name, "bounce")]] <- bounce_events
            }
        }

        # State Analysis in New Zone
        pts_new_zone <- stage_pts[is_new, ]
        if (nrow(pts_new_zone) > 0) {
            state_counts <- table(pts_new_zone$state)
            time_budget_data[[paste(ele_id, stage_name)]] <- data.frame(
                Elephant = ele_id,
                Stage = stage_name,
                Zone = "New_Zone",
                State = names(state_counts),
                N = as.numeric(state_counts)
            )
        }

        # ---------------------------------------------------------
        # 3. BACI Trajectory Visualization (Map)
        # ---------------------------------------------------------
        cat("  Generating BACI Map for", stage_name, "...\n")

        # Define Frame around Fence Line (2km buffer)
        bbox_fence <- st_bbox(st_buffer(fence_line, dist = 2000))

        p_map <- ggplot() +
            # Trajectory
            geom_path(data = stage_pts, aes(x = x, y = y, color = state), linewidth = 0.3, alpha = 0.6) +
            geom_point(data = stage_pts, aes(x = x, y = y, color = state), size = 0.5, alpha = 0.6) +

            # Fence Line (Overlaid clearly)
            geom_sf(data = fence_line, color = "black", linewidth = 1.0, linetype = "dashed") +

            # Styling
            scale_color_manual(values = c("Foraging" = "#E69F00", "Movement" = "#56B4E9")) +
            coord_sf(
                xlim = c(bbox_fence["xmin"], bbox_fence["xmax"]),
                ylim = c(bbox_fence["ymin"], bbox_fence["ymax"]),
                expand = FALSE
            ) +
            labs(
                title = paste0("BACI Spatial Design: ", toupper(stage_name), "-Fence (", ele_id, ")"),
                subtitle = "Elephant Trajectory vs Fence Line (dashed)",
                color = "Behavior",
                x = "Easting", y = "Northing"
            ) +
            theme_minimal() +
            theme(legend.position = "bottom")

        ggsave(file.path(out_dir, paste0("BACI_Map_", ele_id, "_", stage_name, ".png")), p_map, width = 8, height = 8, bg = "white")
    }
}

# ==============================================================================
# 4. Aggregation, Traceability & Visualization
# ==============================================================================

# Create Traceability Report
timestamp_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
report_file <- file.path(out_dir, "Ghost_Fence_Traceability_Report.txt")

sink(report_file)
cat("==============================================================================\n")
cat("GHOST FENCE ANALYSIS - TRACEABILITY REPORT\n")
cat("Generated:", timestamp_str, "\n")
cat("Script:", "03_ghost_fence_analysis.R", "\n")
cat("==============================================================================\n\n")

cat("INPUT DATA FILES:\n")
for (f in ele_files) {
    cat("  - ", basename(f), "\n")
}
cat("\n")

if (length(interaction_data) > 0) {
    df_events <- bind_rows(interaction_data)

    # Summary Table for Report
    df_summ <- df_events %>%
        group_by(Elephant, Stage, Type, Behavior) %>%
        summarise(Count = n(), .groups = "drop")

    cat("INTERACTION SUMMARY (EVENTS):\n")
    print(df_summ)
    cat("\n")

    # ---------------------------------------------------------
    # Plot 1: Monthly Trend in Post-Fence Period (SPLIT BY BEHAVIOR)
    # ---------------------------------------------------------
    cat("Generating Monthly Trend Analysis...\n")
    df_post <- df_events %>%
        filter(Stage == "post")
    # Included all types (Crossing & Bounce)

    if (nrow(df_post) > 0) {
        # Aggregate by Month
        df_monthly <- df_post %>%
            mutate(Month = floor_date(Date, "month")) %>%
            group_by(Elephant, Month, Behavior) %>%
            summarise(Events = n(), .groups = "drop") %>%
            complete(Elephant, Month, Behavior, fill = list(Events = 0))

        p_trend <- ggplot(df_monthly, aes(x = Month, y = Events, color = Behavior, group = Behavior)) +
            geom_line(linewidth = 1) +
            geom_point(size = 2) +
            facet_wrap(~Elephant, scales = "free_y") +
            scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
            labs(
                title = "Ghost Fence Dynamics: Monthly Trends (Post-Removal)",
                subtitle = "Comparison of Crossings (Foraging/Movement) and Bounces over time",
                y = "Events per Month",
                x = "Time",
                color = "Behavior"
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_color_manual(values = c("Bounce" = "#D55E00", "Foraging" = "#E69F00", "Movement" = "#56B4E9"))

        ggsave(file.path(out_dir, "Ghost_Fence_Monthly_Trend_Behavior.png"), p_trend, width = 10, height = 6, bg = "white")
    } else {
        cat("[WARN] No events found in Post period for trending.\n")
    }

    # ---------------------------------------------------------
    # Plot 2: Total Interaction Rates (Pre vs Post)
    # ---------------------------------------------------------
    # Re-calculate durations
    durations <- data.frame(
        Stage = c("pre", "post"),
        Days = c(
            as.numeric(as.duration(periods$pre), "days"),
            as.numeric(as.duration(periods$post), "days")
        )
    )

    df_rates <- df_summ %>%
        left_join(durations, by = "Stage") %>%
        mutate(Rate = Count / Days)

    p_inter <- ggplot(df_rates, aes(x = Stage, y = Rate, fill = Behavior)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(~Elephant) +
        labs(
            title = "Ghost Fence Analysis: Interaction Types",
            subtitle = "Comparison of Interaction Rates by Behavioral Type",
            y = "Events per Day",
            x = "Period"
        ) +
        theme_minimal() +
        scale_fill_manual(values = c("Bounce" = "#D55E00", "Foraging" = "#E69F00", "Movement" = "#56B4E9"))

    ggsave(file.path(out_dir, "Ghost_Fence_Interactions_Behavior.png"), p_inter, width = 8, height = 6, bg = "white")
} else {
    cat("[ERROR] No interaction data generated.\n")
}

if (length(time_budget_data) > 0) {
    df_budget <- bind_rows(time_budget_data)

    cat("NEW ZONE BEHAVIOR SUMMARY:\n")
    # Verify aggregation
    summ <- df_budget %>%
        group_by(Elephant, Stage, Zone) %>%
        summarise(Total_Pts = sum(N), .groups = "drop")
    print(summ)
    cat("\n")

    df_budget_post <- df_budget %>% filter(Stage == "post")

    if (nrow(df_budget_post) > 0) {
        p_budget <- ggplot(df_budget_post, aes(x = Elephant, y = N, fill = State)) +
            geom_bar(stat = "identity", position = "fill") +
            geom_text(aes(label = scales::percent(after_stat(y), accuracy = 1)),
                position = position_fill(vjust = 0.5),
                stat = "identity",
                color = "white", size = 3
            ) +
            labs(
                title = "Behavior in New Zone (Post-Removal)",
                subtitle = "Proportion of Time Spent in Foraging vs Movement in newly accessible areas",
                y = "Proportion",
                x = "Elephant"
            ) +
            scale_y_continuous(labels = scales::percent) +
            theme_minimal() +
            scale_fill_manual(values = c("Foraging" = "#E69F00", "Movement" = "#56B4E9"))

        ggsave(file.path(out_dir, "New_Zone_Behavior.png"), p_budget, width = 6, height = 6, bg = "white")
    }
} else {
    cat("[WARN] No New Zone data generated.\n")
}

sink()
cat("Traceability Report saved to:", report_file, "\n")
