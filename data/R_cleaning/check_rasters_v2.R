library(terra)
library(dplyr)

envi_dir <- "data/envi"
tif_files <- list.files(envi_dir, pattern = "\\.tif$", full.names = TRUE)

check_results <- list()

for (f in tif_files) {
  name <- basename(f)
  r <- try(rast(f), silent = TRUE)
  
  if (inherits(r, "try-error")) next
  
  nr <- nrow(r)
  nc <- ncol(r)
  
  # Check top right corner (100x100 block)
  # Rows 1 to 100, Cols (nc-99) to nc
  row_idx <- 1:min(100, nr)
  col_idx <- max(1, nc-99):nc
  
  # Extract values
  v <- values(r, row=min(row_idx), nrows=length(row_idx), col=min(col_idx), ncols=length(col_idx))
  
  na_count <- sum(is.na(v))
  total_cells <- length(v)
  na_perc <- (na_count / total_cells) * 100
  
  # Check for "top right corner awkwardness"
  # If the corner is valid but the rest is NA, or vice versa?
  # Or if it's mostly NA.
  
  check_results[[name]] <- data.frame(
    File = name,
    Rows = nr,
    Cols = nc,
    TopRight_NA_Percent = na_perc,
    Global_NA_Percent = (global(is.na(r), "sum", na.rm=TRUE)[1,1] / (nr*nc)) * 100
  )
}

results_df <- bind_rows(check_results)

# Bioclimatic rasters are bio1.tif to bio19.tif
bio_rasters <- results_df[grepl("^bio", results_df$File), ]

print("--- Bioclimatic Rasters Top-Right Check ---")
print(bio_rasters %>% select(File, TopRight_NA_Percent, Global_NA_Percent))

# Look for ones where TopRight is significantly different or all NAs
anomalies <- results_df %>% 
  filter(TopRight_NA_Percent == 100 & Global_NA_Percent < 100)

cat("\n--- Potential Anomalies (Corner is 100% NA) ---\n")
print(anomalies)

# Write out the results
write.csv(results_df, "raster_anomaly_report_v2.csv", row.names = FALSE)
