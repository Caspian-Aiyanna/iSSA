library(terra)
library(readxl)
library(dplyr)

envi_dir <- "data/envi"
tif_files <- list.files(envi_dir, pattern = "\\.tif$", full.names = TRUE)

# Read names for better titles
envi_names <- read_xlsx(file.path(envi_dir, "envi_names.xlsx"))

# Match files to names
file_info <- data.frame(path = tif_files) %>%
  mutate(base = sub("\\.tif$", "", basename(path))) %>%
  left_join(envi_names, by = c("base" = "original")) %>%
  mutate(title = ifelse(is.na(safe), base, safe))

# Total of 39 rasters
n_rasters <- nrow(file_info)

# Output image size
# 40 rasters -> say 5 cols, 8 rows
png("all_envi_rasters_visual_check.png", width = 10 * 400, height = 8 * 500, res = 150)
par(mfrow = c(8, 5), mar = c(2, 2, 3, 1))

# Choose a "biclimatic" scale
# Temperature usually red/yellow, Precip usually blue/green
# But for a consistent visual check for 'awkward corners', Viridis is best for contrast
cols <- viridis::viridis(100)

for (i in 1:n_rasters) {
  r <- rast(file_info$path[i])
  # Use a slight buffer for the title
  plot(r, main = file_info$title[i], col = cols, axes = FALSE, box = TRUE)
}

dev.off()

cat("Saved multipanel plot to all_envi_rasters_visual_check.png\n")
