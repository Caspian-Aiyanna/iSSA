library(terra)
library(dplyr)

envi_dir <- "data/envi"
tif_files <- list.files(envi_dir, pattern = "\\.tif$", full.names = TRUE)

check_results <- list()

for (f in tif_files) {
  name <- basename(f)
  r <- rast(f)
  
  nr <- nrow(r)
  nc <- ncol(r)
  
  # Top right 200x200
  vals <- values(r, row=1, nrows=200, col=nc-199, ncols=200)
  
  non_na_vals <- vals[!is.na(vals)]
  
  if (length(non_na_vals) > 0) {
    check_results[[name]] <- data.frame(
      File = name,
      Count = length(non_na_vals),
      Mean = mean(non_na_vals),
      SD = sd(non_na_vals),
      Min = min(non_na_vals),
      Max = max(non_na_vals)
    )
  }
}

results_df <- bind_rows(check_results)
print("--- Top Right Corner (200x200) Stats ---")
print(results_df)

# Compare bio rasters specifically
bio_only <- results_df[grepl("^bio", results_df$File), ]
print("--- Bio Rasters Corner Stats ---")
print(bio_only)

# Identify the one that stands out
# Many have 2765 pixels. If one has different count or weird SD, that's it.
# Actually, I already saw all bio had 2765. 
# But maybe one has different Mean or SD relative to its global stats?

# Let's get global stats for bio files to compare
cat("\n--- Bio Rasters Global vs Corner SD ---\n")
for (f in list.files(envi_dir, pattern = "^bio.*\\.tif$", full.names = TRUE)) {
  name <- basename(f)
  r <- rast(f)
  glob_sd <- global(r, "sd", na.rm=TRUE)[1,1]
  corner_vals <- values(r, row=1, nrows=200, col=ncol(r)-199, ncols=200)
  corn_sd <- sd(corner_vals, na.rm=TRUE)
  cat(name, " Global SD:", glob_sd, " Corner SD:", corn_sd, "\n")
}
