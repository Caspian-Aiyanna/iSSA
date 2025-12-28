# 00_fence_occupancy_check.R
# Check Elephant Occupancy Across Fence Removal Stages

# Date: 2025-12-16

library(sf)
library(tidyverse)
library(lubridate)
library(terra)
# library(tidyterra)

# ==============================================================================
# 1. Define Paths and Periods
# ==============================================================================

shapefile_paths <- list(
    pre = "d:/PhD/Projects/SDM_projects/BTEH/SDM_ele/BTEH/BTEH/BioHabs/data/shp/HV2021/HV_before_fenceRM.shp",
    interim = "d:/PhD/Projects/SDM_projects/BTEH/SDM_ele/BTEH/BTEH/BioHabs/data/shp/HV2022/HV202222.shp", # Assumption based on folder structure
    post = "d:/PhD/Projects/SDM_projects/BTEH/SDM_ele/BTEH/BTEH/BioHabs/data/shp/HV2023/HV20233.shp" # Assumption based on folder structure
)

data_dir <- "d:/PhD/Projects/SDM_projects/BTEH/SDM_ele/BTEH/BTEH/BioHabs/data/clean"

periods <- list(
    pre = interval(dmy("04-08-2022"), dmy("09-12-2023")),
    interim = interval(dmy("10-12-2023"), dmy("08-01-2024")),
    post = interval(dmy("09-01-2024"), dmy("14-07-2025"))
)

# ==============================================================================
# 2. Load Shapefiles
# ==============================================================================

shapes <- list()
for (stage in names(shapefile_paths)) {
    if (file.exists(shapefile_paths[[stage]])) {
        shapes[[stage]] <- st_read(shapefile_paths[[stage]], quiet = TRUE) %>%
            st_make_valid()

        # Ensure CRS is WGS84 for consistency with GPS data first
        if (st_crs(shapes[[stage]])$epsg != 4326) {
            shapes[[stage]] <- st_transform(shapes[[stage]], 4326)
        }

        # Add a column for the stage to the shapefile
        shapes[[stage]]$Stage_Map <- stage
        shapes[[stage]]$Source_Shapefile <- basename(shapefile_paths[[stage]])
    } else {
        warning(paste("Shapefile not found:", shapefile_paths[[stage]]))
    }
}

# ==============================================================================
# 3. Load Telemetry Data
# ==============================================================================

# Search recursively for CSV files in valid subdirectories
csv_files <- list.files(data_dir, pattern = "^E[1-6][AB]?\\.csv$", full.names = TRUE, recursive = TRUE)

ele_data_list <- lapply(csv_files, function(f) {
    # Extract Elephant ID from filename (E1, E2, etc.)
    fname <- basename(f)
    eid <- str_extract(fname, "E[0-9]+")

    df <- read_csv(f, show_col_types = FALSE, col_types = cols(collar_id = col_character())) %>%
        mutate(
            Elephant = eid,
            timestamp = dmy_hm(timestamp), # Format: 01-01-2024 00:13
            source_file = fname
        ) %>%
        select(Elephant, timestamp, lon, lat, collar_id, source_file)

    return(df)
})

all_ele_data <- bind_rows(ele_data_list) %>%
    filter(!is.na(lon), !is.na(lat), !is.na(timestamp))

# ==============================================================================
# 4. Assign Stages and Zones
# ==============================================================================

# Assign Stage based on timestamp
all_ele_data <- all_ele_data %>%
    mutate(
        Stage = case_when(
            timestamp %within% periods$pre ~ "pre",
            timestamp %within% periods$interim ~ "interim",
            timestamp %within% periods$post ~ "post",
            TRUE ~ NA_character_
        )
    ) %>%
    filter(!is.na(Stage))

# Convert to sf object
ele_sf <- st_as_sf(all_ele_data, coords = c("lon", "lat"), crs = 4326)

# Perform Spatial Join for each stage to see which zone (polygon) they are in
# We do this separately for each stage because the map changes
results_list <- list()

for (stg in c("pre", "interim", "post")) {
    points_in_stage <- ele_sf %>% filter(Stage == stg)
    map_in_stage <- shapes[[stg]]

    if (nrow(points_in_stage) > 0 && !is.null(map_in_stage)) {
        # Add a unique ID to the map polygons for easy tracking if not present
        map_in_stage <- map_in_stage %>%
            mutate(Zone_Index = as.character(1:n()))

        # Spatial join
        joined <- st_join(points_in_stage, map_in_stage)

        # Store result
        results_list[[stg]] <- joined %>%
            st_drop_geometry() %>%
            mutate(Stage_Map_Used = stg)
    }
}

final_results <- bind_rows(results_list)

# ==============================================================================
# 5. Summarize
# ==============================================================================

# Create a detailed summary
summary_detailed <- final_results %>%
    group_by(Elephant, Stage, Zone_Index) %>%
    summarise(
        Points = n(),
        Min_Date = min(timestamp),
        Max_Date = max(timestamp),
        .groups = "drop"
    )

# Create a readable wide-format summary for reporting
summary_wide <- summary_detailed %>%
    select(Elephant, Stage, Zone_Index, Points) %>%
    mutate(Zone_Index = replace_na(Zone_Index, "Outside")) %>%
    pivot_wider(
        names_from = Zone_Index,
        values_from = Points,
        names_prefix = "Points_Zone_",
        values_fill = 0
    ) %>%
    mutate(
        Total_Points = rowSums(across(starts_with("Points_Zone_")))
    ) %>%
    arrange(Elephant, factor(Stage, levels = c("pre", "interim", "post")))

print("Occupancy Summary (Wide):")
print(summary_wide)

# Save to a new file to avoid permission issues
out_file <- file.path(dirname(dirname(data_dir)), "playground", "elephant_occupancy_summary_clean.csv")
# write_csv(summary_wide, out_file)
print(paste("Summary table saved to:", out_file))


# ==============================================================================
# 6. Plotting
# ==============================================================================

library(patchwork)

# Create output directory for plots
plot_dir <- file.path(dirname(dirname(data_dir)), "playground", "occupancy_plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

# Determine global bounding box (Extent of Post/Maximal shapefile)
# We assume 'post' (HV20233) covers the largest extent.
global_bbox <- st_bbox(shapes[["post"]])

# Function to create a plot for a specific Elephant and Stage
create_stage_plot <- function(ele_id, stage_name, map_sf, points_sf, period_label) {
    # Filter points
    pts <- points_sf %>%
        filter(Elephant == ele_id, Stage == stage_name)

    # Base plot with Map
    p <- ggplot() +
        geom_sf(data = map_sf, fill = "gray95", color = "black", alpha = 0.5)

    # Add points if they exist
    if (nrow(pts) > 0) {
        p <- p + geom_sf(data = pts, color = "#E69F00", size = 0.8, alpha = 0.6)
    }

    p <- p +
        coord_sf(
            xlim = c(global_bbox["xmin"], global_bbox["xmax"]),
            ylim = c(global_bbox["ymin"], global_bbox["ymax"]),
            expand = FALSE
        ) +
        labs(
            title = paste(tools::toTitleCase(stage_name)),
            subtitle = period_label
        ) +
        theme_minimal() +
        theme(
            axis.title = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            plot.subtitle = element_text(size = 8)
        )

    return(p)
}

# Iterate over elephants and generate panels
elephants <- unique(ele_sf$Elephant)

for (ele in elephants) {
    # Create 3 plots: Pre, Interim, Post
    p1 <- create_stage_plot(ele, "pre", shapes[["pre"]], ele_sf, "Aug 2022 - Dec 2023")
    p2 <- create_stage_plot(ele, "interim", shapes[["interim"]], ele_sf, "Dec 2023 - Jan 2024")
    p3 <- create_stage_plot(ele, "post", shapes[["post"]], ele_sf, "Jan 2024 - Jul 2025")

    # Combine using patchwork
    combined_plot <- (p1 | p2 | p3) +
        plot_annotation(
            title = paste("Temporal Distribution - Elephant", ele),
            theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
        )

    # Save
    outfile <- file.path(plot_dir, paste0("Occupancy_", ele, ".png"))
    ggsave(outfile, combined_plot, width = 12, height = 5, dpi = 300)
    print(paste("Saved:", outfile))
}
