library(terra)
library(dplyr)

envi_dir <- "data/envi"
tif_files <- list.files(envi_dir, pattern = "\\.tif$", full.names = TRUE)

check_results <- list()

for (f in tif_files) {
  name <- basename(f)
  r <- try(rast(f), silent = TRUE)
  
  if (inherits(r, "try-error")) next
  
  ext_v <- as.vector(ext(r))
  res_v <- res(r)
  
  # Get corner values
  # Top right 10x10
  nr <- nrow(r)
  nc <- ncol(r)
  corner_vals <- values(r, row=1, nrows=10, col=nc-9, ncols=10)
  
  check_results[[name]] <- data.frame(
    File = name,
    Xmin = ext_v[1],
    Xmax = ext_v[2],
    Ymin = ext_v[3],
    Ymax = ext_v[4],
    ResX = res_v[1],
    ResY = res_v[2],
    Mean = global(r, "mean", na.rm=TRUE)[1,1],
    SD = global(r, "sd", na.rm=TRUE)[1,1],
    Corner_Any_Value = any(!is.na(corner_vals))
  )
}

results_df <- bind_rows(check_results)

# Compare bio rasters specifically
bio_stats <- results_df[grepl("^bio", results_df$File), ]

print("--- Detailed Metadata for Bioclimatic Rasters ---")
print(bio_stats)

# Check for outlier in metadata
# Many bioclim rasters should have the same extent/res
print("--- Extent Summary ---")
print(results_df %>% group_by(Xmin, Xmax, Ymin, Ymax) %>% summarise(Count = n()))

# Identify the one with 'awkward' corner
awkward <- results_df %>% filter(Corner_Any_Value == TRUE)
if(nrow(awkward) > 0) {
  cat("\n--- Rasters with data in the top-right corner (others are NA) ---\n")
  print(awkward$File)
}

write.csv(results_df, "raster_detailed_metadata.csv", row.names = FALSE)
