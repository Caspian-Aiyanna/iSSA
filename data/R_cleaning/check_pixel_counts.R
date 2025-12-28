library(terra)
library(dplyr)

envi_dir <- "data/envi"
tif_files <- list.files(envi_dir, pattern = "\\.tif$", full.names = TRUE)

global_counts <- list()

for (f in tif_files) {
  name <- basename(f)
  r <- rast(f)
  
  # Valid pixel count
  valid_count <- global(!is.na(r), "sum", na.rm=TRUE)[1,1]
  
  global_counts[[name]] <- data.frame(
    File = name,
    Valid_Pixels = valid_count
  )
}

counts_df <- bind_rows(global_counts)
print(counts_df)

# Group by pixel count to see outliers
print(counts_df %>% group_by(Valid_Pixels) %>% summarise(Files = paste(File, collapse=", ")))
