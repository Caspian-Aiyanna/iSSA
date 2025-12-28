library(terra)
library(dplyr)

envi_dir <- "data/envi"
tif_files <- list.files(envi_dir, pattern = "\\.tif$", full.names = TRUE)

# Try to find a CSV or XLSX for names
# Since I only saw .xlsx, I'll try to use the filenames for now
# or see if I can list the sheets if I had readxl, but I'll stick to filenames

check_results <- list()

# Define a function to check the top right corner
check_top_right <- function(r) {
  # Get dimensions
  nr <- nrow(r)
  nc <- ncol(r)
  
  # Check a 50x50 block in the top right
  # Top right indices: rows 1:50, cols (nc-49):nc
  row_range <- 1:min(50, nr)
  col_range <- max(1, nc-49):nc
  
  cells <- cellFromRowColCombine(r, row_range, col_range)
  vals <- r[cells]
  
  na_count <- sum(is.na(vals))
  total_checked <- length(vals)
  
  return(list(na_percent = (na_count / total_checked) * 100, na_count = na_count))
}

base_extent <- NULL
base_res <- NULL

for (f in tif_files) {
  name <- basename(f)
  r <- try(rast(f), silent = TRUE)
  
  if (inherits(r, "try-error")) {
    check_results[[name]] <- data.frame(
      File = name,
      Status = "Error loading",
      Extent_Match = FALSE,
      TopRight_NA_Percent = NA
    )
    next
  }
  
  # Check extent consistency (compare with first valid raster)
  if (is.null(base_extent)) {
    base_extent <- ext(r)
    base_res <- res(r)
  }
  
  ext_match <- (ext(r) == base_extent)
  res_match <- all(res(r) == base_res)
  
  # Check top right corner
  tr_check <- check_top_right(r)
  
  check_results[[name]] <- data.frame(
    File = name,
    Rows = nrow(r),
    Cols = ncol(r),
    Extent = paste(as.vector(ext(r)), collapse=","),
    Extent_Match = ext_match,
    Res_Match = res_match,
    TopRight_NA_Percent = tr_check$na_percent,
    TopRight_NA_Count = tr_check$na_count,
    Global_NA_Percent = (global(is.na(r), "sum", na.rm=TRUE)[1,1] / (nrow(r)*ncol(r))) * 100
  )
}

results_df <- bind_rows(check_results)

# Print summary of anomalies
cat("\n--- Raster Consistency & Anomaly Check ---\n")
print(results_df %>% select(File, Extent_Match, Res_Match, TopRight_NA_Percent))

anomalies <- results_df %>% 
  filter(!Extent_Match | !Res_Match | TopRight_NA_Percent > 10) # 10% NA in corner as threshold

cat("\n--- Identified Anomalies ---\n")
if (nrow(anomalies) > 0) {
  print(anomalies)
} else {
  print("No major anomalies detected based on current heuristics.")
}

write.csv(results_df, "raster_check_report.csv", row.names = FALSE)
