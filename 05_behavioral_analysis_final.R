# 05_behavioral_analysis_final.R
# BACI Design: 3-State HMM and Behavioral Habitat Selection
# Threshold Date: 2024-01-23 (Fence Removal)

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

cat("Starting Final Behavioral Analysis (BACI Design: Jan 23, 2024)...\n")

# ==============================================================================
# 1. Configuration & Paths
# ==============================================================================

root_dir <- getwd()
clean_dir <- file.path(root_dir, "data", "clean")
shp_dir <- file.path(root_dir, "data", "shp")
envi_dir <- file.path(root_dir, "data", "envi")
out_dir <- file.path(root_dir, "results")
pts_out_dir <- file.path(out_dir, "behavioral_points")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
if (!dir.exists(pts_out_dir)) dir.create(pts_out_dir, recursive = TRUE)

target_crs <- 32735 # UTM 35S
removal_date <- as.POSIXct("2024-01-23", tz = "UTC")

# Custom Color Scale
br_colors <- c("#0571b0", "#f7f7f7", "#ca0020")
bh_colors <- c("Resting" = "#999999", "Foraging" = "#E69F00", "Movement" = "#56B4E9", "Bounce" = "#8B4513")

# ==============================================================================
# 2. Loading Spatial Context
# ==============================================================================

cat("Loading Shapefiles (HV, KW, Fence)...\n")
hv_sf <- read_sf(file.path(shp_dir, "HV", "HV.shp")) %>% st_transform(target_crs)
kw_sf <- read_sf(file.path(shp_dir, "KW", "KW.shp")) %>% st_transform(target_crs)
fence_sf <- read_sf(file.path(shp_dir, "Fence", "fence_2024.shp")) %>% st_transform(target_crs)
fence_buffer_100m <- st_buffer(fence_sf, dist = 100)

# ==============================================================================
# 3. Environmental Prediction Stack (39 Layers)
# ==============================================================================

cat("Loading Environmental Stack (39 layers)...\n")
tif_files <- list.files(envi_dir, pattern = "\\.tif$", full.names = TRUE)
r_stack <- terra::rast(tif_files)
safe_names <- make.names(names(r_stack), unique = TRUE)
names(r_stack) <- safe_names
stack_crs <- terra::crs(r_stack)

# ==============================================================================
# 4. HMM & Behavioral Decoding
# ==============================================================================

ele_files <- list.files(clean_dir, pattern = "\\.csv$", full.names = TRUE)
full_results_list <- list()

for (f in ele_files) {
  ele_name <- sub("_.*", "", basename(f))
  cat("\nProcessing Elephant:", ele_name, "\n")
  
  d <- read.csv(f)
  
  # Standardize column names
  names(d) <- gsub("\\.\\.GMT\\.0\\.0\\.", "", names(d))
  
  # Parse timestamp
  d$timestamp <- as.POSIXct(d$fixtime, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  if (all(is.na(d$timestamp))) {
    d$timestamp <- as.POSIXct(d$fixtime, format="%d-%m-%Y %H:%M", tz="UTC")
  }
  
  d <- d %>% 
    filter(!is.na(lon), !is.na(lat), !is.na(timestamp)) %>%
    arrange(timestamp) %>%
    distinct(timestamp, .keep_all = TRUE)
  
  # Resample to 30-min
  trk <- make_track(d, lon, lat, timestamp, crs = 4326) %>%
    transform_coords(target_crs) %>%
    track_resample(rate = minutes(30), tolerance = minutes(5))
  
  if (nrow(trk) < 50) {
    cat("  Skipping: Not enough resampled points.\n")
    next
  }
  
  # HMM Prep
  m_data <- prepData(data.frame(ID = ele_name, x = trk$x_ / 1000, y = trk$y_ / 1000, x_m = trk$x_, y_m = trk$y_, date = trk$t_), type = "UTM")
  m_data$hour <- hour(m_data$date) + minute(m_data$date) / 60
  m_data$cosHour <- cos(2 * pi * m_data$hour / 24)
  m_data$sinHour <- sin(2 * pi * m_data$hour / 24)
  
  # Jitter stationary steps
  z_idx <- which(m_data$step <= 0 | is.na(m_data$step))
  if (length(z_idx) > 0) m_data$step[z_idx] <- runif(length(z_idx), 0.0001, 0.005)
  m_data <- m_data %>% filter(!is.na(step), !is.na(angle))
  
  # Params: Rest(5m), Forage(60m), Move(250m)
  mu0 <- c(0.005, 0.06, 0.25)
  sd0 <- c(0.004, 0.04, 0.12)
  ang0 <- c(pi, pi, 0)
  con0 <- c(0.2, 0.4, 0.6)
  
  cat("  Fitting 3-state HMM...\n")
  m_fit <- try(fitHMM(data = m_data, nbStates = 3, stepPar0 = c(mu0, sd0), anglePar0 = c(ang0, con0), formula = ~ cosHour + sinHour, verbose = 0))
  
  if (inherits(m_fit, "try-error")) next
  
  states <- viterbi(m_fit)
  mu_est <- m_fit$mle$stepPar[1, ]
  ord <- order(mu_est)
  sl <- c("Resting", "Foraging", "Movement")
  
  m_data <- m_data %>%
    mutate(
      behavior = factor(sl[match(states, ord)], levels = sl),
      Elephant = ID,
      Stage = ifelse(date < removal_date, "pre", "post")
    )
  
  # Spatial Context
  ele_sf <- st_as_sf(m_data, coords = c("x_m", "y_m"), crs = target_crs)
  m_data$in_HV <- st_intersects(ele_sf, hv_sf, sparse = FALSE)[, 1]
  m_data$in_KW <- st_intersects(ele_sf, kw_sf, sparse = FALSE)[, 1]
  m_data$Zone <- ifelse(m_data$in_KW, "KW_Home", ifelse(m_data$in_HV, "HV_Novel", "Other"))
  
  # Save individual behavioral CSV
  write.csv(m_data, file.path(pts_out_dir, paste0(ele_name, "_behavioral.csv")), row.names = FALSE)
  
  full_results_list[[ele_name]] <- m_data
}

all_data <- bind_rows(full_results_list)

# ==============================================================================
# 5. Visualizations
# ==============================================================================

cat("\nGenerating Visualizations...\n")

# 1. Trajectory Maps (Journal Style)
for (ele in names(full_results_list)) {
  d_ele <- full_results_list[[ele]]
  d_ele_sf <- st_as_sf(d_ele, coords = c("x_m", "y_m"), crs = target_crs) %>% st_transform(4326)
  d_ele_sf$lon_wgs <- st_coordinates(d_ele_sf)[,1]
  d_ele_sf$lat_wgs <- st_coordinates(d_ele_sf)[,2]
  
  for (stg in unique(d_ele$Stage)) {
    d_stg <- d_ele_sf %>% filter(Stage == stg)
    
    p <- ggplot() +
      geom_path(data = d_stg, aes(x = lon_wgs, y = lat_wgs, color = behavior), alpha = 0.4) +
      geom_point(data = d_stg, aes(x = lon_wgs, y = lat_wgs, color = behavior), size = 0.6) +
      scale_color_manual(values = bh_colors) +
      labs(title = paste("Trajectory:", ele, "-", toupper(stg)), x = "Longitude", y = "Latitude") +
      theme_minimal() +
      coord_sf()
    
    if (stg == "pre") {
      p <- p + geom_sf(data = fence_sf %>% st_transform(4326), color = "black", linetype = "dashed", linewidth = 1)
    }
    
    ggsave(file.path(out_dir, paste0("Trajectory_", ele, "_", stg, ".png")), p, width = 10, height = 8, bg = "white")
  }
}

# 2. Time Budget Shift
cat("  Plotting: Behavioral_Time_Budget_Shift.png\n")
df_budget <- all_data %>%
  group_by(Elephant, Stage, behavior) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(Prop = n / sum(n))

p_budget <- ggplot(df_budget, aes(x = Stage, y = Prop, fill = behavior)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(Prop, accuracy = 1)), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  facet_wrap(~Elephant) +
  scale_fill_manual(values = bh_colors) +
  labs(title = "Behavioral Time Budget Shift (Fence Removal: Jan 23 2024)", y = "Proportion") +
  theme_minimal()

ggsave(file.path(out_dir, "01_Behavioral_Time_Budget_Shift.png"), p_budget, width = 12, height = 8, bg = "white")

# 3. Ghost Fence Dynamics (Bounce logic)
cat("  Plotting: Ghost_Fence_Trend.png\n")
df_int_list <- list()

for (ele in names(full_results_list)) {
  d_ele <- full_results_list[[ele]]
  d_sf <- st_as_sf(d_ele, coords = c("x_m", "y_m"), crs = target_crs)
  
  is_buffer <- st_intersects(d_sf, fence_buffer_100m, sparse = FALSE)[, 1]
  
  # Intersection events
  events <- d_ele[is_buffer, ] %>% mutate(Type = as.character(behavior))
  
  # Bounce logic
  buf_idx <- which(is_buffer)
  if (length(buf_idx) > 1) {
    bursts <- split(buf_idx, cumsum(c(1, diff(buf_idx) != 1)))
    for (b in bursts) {
      st <- min(b)
      en <- max(b)
      if (st > 1 && en < nrow(d_ele)) {
        if (d_ele$in_KW[st-1] && d_ele$in_KW[en+1]) {
           events <- bind_rows(events, d_ele[st, ] %>% mutate(Type = "Bounce"))
        }
      }
    }
  }
  df_int_list[[ele]] <- events
}

df_int <- bind_rows(df_int_list) %>%
  filter(Stage == "post") %>%
  mutate(Month = floor_date(date, "month")) %>%
  group_by(Elephant, Month, Type) %>%
  summarise(Events = n(), .groups = "drop")

p_int <- ggplot(df_int, aes(x = Month, y = Events, color = Type)) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~Elephant, scales = "free_y") +
  scale_color_manual(values = bh_colors) +
  labs(title = "Ghost Fence Dynamics: Monthly Trends (Post-Removal)", x = "Date", y = "Events") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_dir, "03_Ghost_Fence_Trend.png"), p_int, width = 12, height = 8, bg = "white")

cat("\nAnalysis Complete. Results in:", out_dir, "\n")
