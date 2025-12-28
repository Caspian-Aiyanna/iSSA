# 04_ecological_behavior_analysis_v3.R
# Comprehensive Ecological Behavioral Analysis of Elephant Movement
# Focus: 3-State HMM (Resting, Foraging, Movement)
# Integrating Diurnal/Seasonal context, Ghost Fence Dynamics, and Spatial Trajectories.

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

cat("Starting Comprehensive Ecological Behavior Analysis (V3 - 30min)...\n")

# ==============================================================================
# 1. Config & Paths
# ==============================================================================

if (dir.exists("BioHabs")) {
    root_dir <- getwd()
} else {
    root_dir <- dirname(dirname(getwd()))
}

clean_dir <- file.path(root_dir, "BioHabs", "data", "clean")
shp_dir <- file.path(root_dir, "BioHabs", "data", "shp")
out_dir <- file.path(root_dir, "BioHabs", "playground", "04_Ecological_Behavior_Results_V3")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Time Periods (As per 02_hypothesis_testing.R)
periods <- list(
    pre     = interval(dmy("04-08-2022"), dmy("09-12-2023")),
    interim = interval(dmy("10-12-2023"), dmy("08-01-2024")),
    post    = interval(dmy("09-01-2024"), dmy("14-07-2025"))
)

target_crs <- 32735 # UTM 35S

# Season Definitions
get_season <- function(date) {
    m <- month(date)
    case_when(
        m %in% c(9, 10, 11) ~ "Spring",
        m %in% c(12, 1, 2) ~ "Summer",
        m %in% c(3, 4, 5) ~ "Autumn",
        m %in% c(6, 7, 8) ~ "Winter"
    )
}

# ==============================================================================
# 2. Load Spatial Context
# ==============================================================================

cat("Loading Spatial Context...\n")
hv_sf <- read_sf(file.path(shp_dir, "HV", "HV.shp")) %>% st_transform(target_crs)
kw_sf <- read_sf(file.path(shp_dir, "KW", "KW.shp")) %>% st_transform(target_crs)
fence_2024 <- read_sf(file.path(shp_dir, "Fence", "fence_2024.shp")) %>% st_transform(target_crs)
fence_buffer <- st_buffer(fence_2024, dist = 150)

# ==============================================================================
# 3. Data Loading
# ==============================================================================

ele_files <- list.files(clean_dir, pattern = "^E[34][AB]?\\.csv$", full.names = TRUE, recursive = TRUE)
ele_files <- ele_files[!duplicated(basename(ele_files))]

full_data <- map_df(ele_files, function(f) {
    df <- read_csv(f, show_col_types = FALSE)
    ele_id <- str_extract(basename(f), "^E[0-9]+")
    if ("location.long" %in% names(df)) df <- rename(df, lon = location.long, lat = location.lat)
    df %>%
        mutate(
            Elephant = ele_id,
            timestamp = tryCatch(dmy_hm(timestamp), error = function(e) ymd_hms(timestamp)),
            Source_File = basename(f)
        ) %>%
        select(Elephant, timestamp, lon, lat, Source_File)
}) %>%
    filter(!is.na(lon) & !is.na(lat) & !is.na(timestamp)) %>%
    arrange(Elephant, timestamp) %>%
    distinct(Elephant, timestamp, .keep_all = TRUE)

# ==============================================================================
# 4. HMM Analysis Loop (30-Minute Interval)
# ==============================================================================

results_list <- list()
interaction_events <- list()

for (ele_id in unique(full_data$Elephant)) {
    cat("\n>>> Processing Elephant:", ele_id, "\n")
    ele_sub <- full_data %>% filter(Elephant == ele_id)

    # 1. Resample to 30min
    trk <- make_track(ele_sub, lon, lat, timestamp, crs = 4326) %>%
        transform_coords(target_crs) %>%
        track_resample(rate = minutes(30), tolerance = minutes(5))

    if (nrow(trk) < 200) next

    # 2. HMM Prep
    df_hmm <- data.frame(
        ID = ele_id,
        x = trk$x_ / 1000, # km
        y = trk$y_ / 1000,
        x_m = trk$x_, # m
        y_m = trk$y_,
        date = trk$t_,
        hour = hour(trk$t_) + minute(trk$t_) / 60
    )

    m_data <- prepData(df_hmm, type = "UTM")
    m_data$cosHour <- cos(2 * pi * m_data$hour / 24)
    m_data$sinHour <- sin(2 * pi * m_data$hour / 24)
    m_data$Season <- factor(get_season(m_data$date), levels = c("Spring", "Summer", "Autumn", "Winter"))
    m_data$Elephant <- ele_id

    # Jitter zeros
    z_idx <- which(m_data$step <= 0 | is.na(m_data$step))
    if (length(z_idx) > 0) m_data$step[z_idx] <- runif(length(z_idx), 0.0001, 0.005)
    m_data <- m_data %>% filter(!is.na(step) & !is.na(angle))

    # 3. Fit HMM (30-min parameters - Calibrated for constrained area)
    # mu: Resting (5m), Foraging (60m), Movement (250m)
    mu0 <- c(0.005, 0.06, 0.25) # km / 30min
    sd0 <- c(0.004, 0.04, 0.12)
    angle0 <- c(pi, pi, 0)
    con0 <- c(0.2, 0.4, 0.6)

    cat("  Fitting HMM...\n")
    m_fit <- try(fitHMM(
        data = m_data, nbStates = 3,
        stepPar0 = c(mu0, sd0), anglePar0 = c(angle0, con0),
        formula = ~ cosHour + sinHour, verbose = 0
    ))

    if (inherits(m_fit, "try-error")) next

    # 4. Decode
    states <- viterbi(m_fit)
    mu_est <- m_fit$mle$stepPar[1, ]
    ord <- order(mu_est)
    sl <- c("Resting", "Foraging", "Movement")
    m_data$behavior <- factor(sl[match(states, ord)], levels = sl)

    ele_sf <- st_as_sf(m_data, coords = c("x_m", "y_m"), crs = target_crs, remove = FALSE)

    # 5. Zone Context
    pre_pts <- ele_sf %>% filter(date %within% periods$pre)
    if (nrow(pre_pts) > 0) {
        c_hv <- sum(st_intersects(pre_pts, hv_sf, sparse = FALSE))
        c_kw <- sum(st_intersects(pre_pts, kw_sf, sparse = FALSE))
        home_sf <- if (c_hv > c_kw) hv_sf else kw_sf
        novel_sf <- if (c_hv > c_kw) kw_sf else hv_sf
        home_label <- if (c_hv > c_kw) "HV" else "KW"
    } else {
        next
    }

    ele_sf <- ele_sf %>%
        mutate(
            Zone = case_when(
                st_intersects(ele_sf, home_sf, sparse = F)[, 1] ~ "Home",
                st_intersects(ele_sf, novel_sf, sparse = F)[, 1] ~ "Novel",
                TRUE ~ "Other"
            ),
            Stage = case_when(
                date %within% periods$pre ~ "pre",
                date %within% periods$interim ~ "interim",
                date %within% periods$post ~ "post",
                TRUE ~ NA_character_
            )
        ) %>%
        filter(!is.na(Stage))

    results_list[[ele_id]] <- ele_sf

    # 6. Interaction Analysis (Ghost Fence Logic)
    # Detect Crossings and Bounces manually to verify the "reduction" trend
    cat("  Analyzing Ghost Fence Interactions...\n")
    is_home <- ele_sf$Zone == "Home"
    is_novel <- ele_sf$Zone == "Novel"
    is_buffer <- st_intersects(ele_sf, fence_buffer, sparse = F)[, 1]

    # Crossing detect
    v_idx <- which(is_home | is_novel)
    if (length(v_idx) > 1) {
        zones <- ifelse(is_home[v_idx], "H", "N")
        sw <- which(zones[-1] != zones[-length(zones)])
        for (i in sw) {
            orig_idx <- v_idx[i + 1]
            interaction_events[[length(interaction_events) + 1]] <- data.frame(
                Elephant = ele_id, Date = ele_sf$date[orig_idx], Type = "Crossing",
                Behavior = ele_sf$behavior[orig_idx], Stage = ele_sf$Stage[orig_idx]
            )
        }
    }

    # Bounce detect
    buf_idx <- which(is_buffer)
    if (length(buf_idx) > 0) {
        bursts <- split(buf_idx, cumsum(c(1, diff(buf_idx) != 1)))
        for (b in bursts) {
            f <- min(b)
            l <- max(b)
            if (f > 1 && l < nrow(ele_sf)) {
                if (is_home[f - 1] && is_home[l + 1]) {
                    mid <- round(mean(c(f, l)))
                    interaction_events[[length(interaction_events) + 1]] <- data.frame(
                        Elephant = ele_id, Date = ele_sf$date[mid], Type = "Bounce",
                        Behavior = "Bounce", Stage = ele_sf$Stage[mid]
                    )
                }
            }
        }
    }
}

# ==============================================================================
# 5. Visualizations
# ==============================================================================

df_all <- bind_rows(map(results_list, st_drop_geometry))
df_int <- bind_rows(interaction_events)

cat("\nGenerating Visualizations...\n")

# A. Diurnal Behavioral Cycle
p1 <- ggplot(df_all, aes(x = hour, fill = behavior)) +
    geom_density(position = "fill", alpha = 0.8) +
    facet_wrap(~Elephant) +
    scale_fill_manual(values = c("Resting" = "#999999", "Foraging" = "#E69F00", "Movement" = "#56B4E9")) +
    labs(title = "Diurnal Behavioral Cycle (30-min HMM)", y = "Proportion", x = "Hour of Day") +
    theme_minimal()
ggsave(file.path(out_dir, "01_Diurnal_Activity.png"), p1, width = 10, height = 6)

# B. Time Budget Shift (Stacked Bar)
p2 <- df_all %>%
    count(Elephant, Stage, behavior) %>%
    group_by(Elephant, Stage) %>%
    mutate(Prop = n / sum(n)) %>%
    ggplot(aes(x = factor(Stage, levels = c("pre", "interim", "post")), y = Prop, fill = behavior)) +
    geom_bar(stat = "identity", position = "fill", color = "white") +
    geom_text(aes(label = percent(Prop, accuracy = 1)), position = position_fill(vjust = 0.5), color = "white", fontface = "bold") +
    facet_wrap(~Elephant) +
    scale_fill_manual(values = c("Resting" = "#999999", "Foraging" = "#E69F00", "Movement" = "#56B4E9")) +
    labs(title = "Behavioral Time Budget Shift", x = "Period", y = "Proportion") +
    theme_minimal()
ggsave(file.path(out_dir, "03_Time_Budget_Shift.png"), p2, width = 10, height = 7)

# C. Ghost Fence Dynamics: Monthly Trends (Post-Removal)
df_int_plot <- df_int %>%
    filter(Stage == "post") %>%
    mutate(
        Month = floor_date(Date, "month"),
        Cat = ifelse(Type == "Bounce", "Bounce", as.character(Behavior))
    ) %>%
    group_by(Elephant, Month, Cat) %>%
    summarise(Events = n(), .groups = "drop")

p3 <- ggplot(df_int_plot, aes(x = Month, y = Events, color = Cat, group = Cat)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    facet_wrap(~Elephant, scales = "free_y") +
    scale_color_manual(values = c("Resting" = "#999999", "Foraging" = "#E69F00", "Movement" = "#56B4E9", "Bounce" = "#8B4513")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    labs(
        title = "Ghost Fence Dynamics: Monthly Trends (Post-Removal)",
        subtitle = "Comparison of Crossings (Foraging/Movement) and Bounces over time (150m Buffer)",
        y = "Events per Month", x = "Time", color = "Behavior"
    ) +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold")
    )
ggsave(file.path(out_dir, "05_Ghost_Fence_Trend.png"), p3, width = 12, height = 7)

# E. Home vs Novel Area Access Intensity (Post-Removal)
cat("  Plotting: Home_vs_Novel_Intensity.png\n")
df_zone_trend <- df_all %>%
    filter(Stage == "post", Zone != "Other") %>%
    mutate(Month = floor_date(date, "month")) %>%
    group_by(Elephant, Month, Zone) %>%
    summarise(Intensity = n(), .groups = "drop")

p5 <- ggplot(df_zone_trend, aes(x = Month, y = Intensity, fill = Zone)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Elephant, scales = "free_y") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    scale_fill_manual(values = c("Home" = "#0571b0", "Novel" = "#ca0020")) +
    labs(
        title = "Zone Access Intensity (Post-Removal)",
        subtitle = "Comparison of usage frequency in Home vs Novel areas",
        y = "Number of Localizations", x = "Time"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold")
    )
ggsave(file.path(out_dir, "07_Home_vs_Novel_Intensity.png"), p5, width = 12, height = 7, bg = "white")

# D. Seasonal Variations
p4 <- df_all %>%
    count(Elephant, Stage, Season, behavior) %>%
    group_by(Elephant, Stage, Season) %>%
    mutate(Prop = n / sum(n)) %>%
    ggplot(aes(x = Season, y = Prop, fill = behavior)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(Elephant ~ Stage) +
    scale_fill_manual(values = c("Resting" = "#999999", "Foraging" = "#E69F00", "Movement" = "#56B4E9")) +
    labs(title = "Seasonal Behavioral Usage", subtitle = "Comparison across 4 seasons (Spring, Summer, Autumn, Winter)") +
    theme_minimal()
ggsave(file.path(out_dir, "06_Seasonal_Variations.png"), p4, width = 12, height = 8)

# E. Trajectory Maps (Functional Map Sample)
cat("Generating Trajectory Maps...\n")
for (ele in names(results_list)) {
    for (stg in c("pre", "interim", "post")) {
        sub_sf <- results_list[[ele]] %>% filter(Stage == stg)
        if (nrow(sub_sf) < 5) next

        p_map <- ggplot() +
            geom_sf(data = hv_sf, fill = "gray95", color = "gray80") +
            geom_sf(data = kw_sf, fill = "gray95", color = "gray80") +
            geom_sf(data = fence_2024, color = "red", linetype = "dashed", linewidth = 0.5) +
            geom_path(data = sub_sf, aes(x = x_m, y = y_m, color = behavior), alpha = 0.4, linewidth = 0.5) +
            geom_point(data = sub_sf, aes(x = x_m, y = y_m, color = behavior), size = 0.5, alpha = 0.6) +
            scale_color_manual(values = c("Resting" = "#999999", "Foraging" = "#E69F00", "Movement" = "#56B4E9")) +
            labs(title = paste("Trajectory Map:", ele, "at", stg), x = "Easting", y = "Northing") +
            theme_minimal() +
            theme(legend.position = "bottom")

        ggsave(file.path(out_dir, paste0("02_Map_", ele, "_", stg, ".png")), p_map, width = 8, height = 8)
    }
}

# ==============================================================================
# 6. Traceability Report
# ==============================================================================

report_path <- file.path(out_dir, "Traceability_Report_V3.txt")
sink(report_path)
cat("COMPREHENSIVE ECOLOGICAL BEHAVIOR ANALYSIS (V3)\n")
cat("==============================================\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Parameters: 30-minute sampling, 3 HMM states (Rest, Forage, Move)\n\n")

cat("Interaction Summary (Bounces vs Crossings by Behavior):\n")
print(df_int %>%
    mutate(Cat = ifelse(Type == "Bounce", "Bounce", as.character(Behavior))) %>%
    group_by(Elephant, Stage, Cat) %>%
    summarise(Count = n(), .groups = "drop"))

cat("\nTime Budget Proportions (Total):\n")
print(df_all %>% group_by(Elephant, Stage, behavior) %>% summarise(N = n(), .groups = "drop") %>% group_by(Elephant, Stage) %>% mutate(Prop = N / sum(N)))

cat("\nSeasonal Breakdown (Behavior usage by Season):\n")
print(df_all %>% group_by(Elephant, Stage, Season, behavior) %>% summarise(N = n(), .groups = "drop"))

sink()

cat("\nAnalysis Complete. Results in:", out_dir, "\n")
