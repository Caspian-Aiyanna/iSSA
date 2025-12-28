library(terra)
library(dplyr)

envi_dir <- "data/envi"
tif_files <- list.files(envi_dir, pattern = "\\.tif$", full.names = TRUE)

# Use Elevation.tif as base if available, else first one
base_f <- file.path(envi_dir, "Elevation.tif")
if(!file.exists(base_f)) base_f <- tif_files[1]

ref_r <- rast(base_f)
ref_na <- is.na(ref_r)

results <- list()

for (f in tif_files) {
  name <- basename(f)
  r <- rast(f)
  
  # Check mask
  r_na <- is.na(r)
  
  # Diff
  diff_mask <- ref_na != r_na
  num_diff <- global(diff_mask, "sum", na.rm=TRUE)[1,1]
  
  # Also check if it has values where ref has NA
  # This would be an "awkward corner"
  extra_values <- global(!is.na(r) & is.na(ref_r), "sum", na.rm=TRUE)[1,1]
  missing_values <- global(is.na(r) & !is.na(ref_r), "sum", na.rm=TRUE)[1,1]
  
  results[[name]] <- data.frame(
    File = name,
    Total_Pixels = nrow(r)*ncol(r),
    Mask_Diff = num_diff,
    Extra_Values = extra_values,
    Missing_Values = missing_values
  )
}

results_df <- bind_rows(results)
print(results_df %>% filter(Mask_Diff > 0))

# Also check for any file with "awkward" in the name or similar? No.
# Check for any value in top-right that is NOT NA
# Maybe the corner is around (row 1-100, col nc-100:nc)
# I'll check a larger corner: 20% of the width/height
cat("\n--- Checking Top-Right 200x200 block ---\n")
corner_results <- list()
for (f in tif_files) {
  name <- basename(f)
  r <- rast(f)
  nr <- nrow(r)
  nc <- ncol(r)
  
  # Top right 200x200
  vals <- values(r, row=1, nrows=200, col=nc-199, ncols=200)
  non_na <- sum(!is.na(vals))
  
  if (non_na > 0) {
     corner_results[[name]] <- non_na
  }
}

print(corner_results)
