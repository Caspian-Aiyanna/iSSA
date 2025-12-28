library(terra)
library(dplyr)

envi_dir <- "data/envi"
tif_files <- list.files(envi_dir, pattern = "\\.tif$", full.names = TRUE)

valid_extents <- list()

for (f in tif_files) {
  name <- basename(f)
  r <- rast(f)
  
  # Trim NAs to find the actual data extent
  rt <- try(trim(r), silent=TRUE)
  
  if (inherits(rt, "try-error")) {
    valid_extents[[name]] <- data.frame(File = name, Xmin=NA, Xmax=NA, Ymin=NA, Ymax=NA)
  } else {
    ev <- as.vector(ext(rt))
    valid_extents[[name]] <- data.frame(File = name, Xmin=ev[1], Xmax=ev[2], Ymin=ev[3], Ymax=ev[4])
  }
}

ext_df <- bind_rows(valid_extents)
print(ext_df)

# Find unique data extents
print(ext_df %>% group_by(Xmin, Xmax, Ymin, Ymax) %>% summarise(Count = n(), Files = paste(File, collapse=", ")))
