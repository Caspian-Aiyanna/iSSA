library(terra)
library(dplyr)

envi_dir <- "data/envi"
bio_files <- list.files(envi_dir, pattern = "^bio.*\\.tif$", full.names = TRUE)

if (length(bio_files) == 0) {
  cat("No bio files found.")
  quit()
}

# Use bio1.tif as reference mask
ref_r <- rast(file.path(envi_dir, "bio1.tif"))
nr <- nrow(ref_r)
nc <- ncol(ref_r)

ref_na <- is.na(ref_r)

mismatches <- list()

for (f in bio_files) {
  name <- basename(f)
  if (name == "bio1.tif") next
  
  r <- rast(f)
  r_na <- is.na(r)
  
  # Compare masks
  diff_mask <- ref_na != r_na
  num_diff <- global(diff_mask, "sum", na.rm=TRUE)[1,1]
  
  if (num_diff > 0) {
    mismatches[[name]] <- num_diff
  }
}

cat("\n--- Mask Mismatch Check (vs bio1.tif) ---\n")
if (length(mismatches) > 0) {
  print(mismatches)
} else {
  cat("All bio rasters have the same NA mask.\n")
}

# Now check for "awkward" top right corner values
# Maybe the values are weird in the top right?
# Let's check the max value in the top right 100x100 block for all
corner_stats <- list()

for (f in bio_files) {
  name <- basename(f)
  r <- rast(f)
  
  # Top right coords:
  # row 1:100, col (nc-99):nc
  row_idx <- 1:min(100, nr)
  col_idx <- max(1, nc-99):nc
  
  corner_vals <- values(r, row=min(row_idx), nrows=length(row_idx), col=min(col_idx), ncols=length(col_idx))
  
  # Check if any value is weird (e.g. extremely high or 0 when it should be NA)
  corner_stats[[name]] <- data.frame(
    File = name,
    Corner_Mean = mean(corner_vals, na.rm=TRUE),
    Corner_Max = max(corner_vals, na.rm=TRUE),
    Corner_Min = min(corner_vals, na.rm=TRUE),
    Corner_NonNA_Count = sum(!is.na(corner_vals))
  )
}

corner_df <- bind_rows(corner_stats)
print("\n--- Corner Stats ---")
print(corner_df)
