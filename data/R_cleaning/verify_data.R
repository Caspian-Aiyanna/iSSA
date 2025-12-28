library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)

og_dir <- "data/OG"
clean_dir <- "data/clean"

og_files <- list.files(og_dir, pattern = "\\.csv$", full.names = TRUE)
clean_files <- list.files(clean_dir, pattern = "\\.csv$", full.names = TRUE)

tally_results <- list()

get_season <- function(month) {
  case_when(
    month %in% c(12, 1, 2) ~ "Summer",
    month %in% c(3, 4, 5) ~ "Autumn",
    month %in% c(6, 7, 8) ~ "Winter",
    month %in% c(9, 10, 11) ~ "Spring"
  )
}

all_data_summary <- list()

for (f in og_files) {
  name <- basename(f)
  cl_f <- file.path(clean_dir, name)
  
  if (file.exists(cl_f)) {
    og_data <- read.csv(f)
    cl_data <- read.csv(cl_f)
    
    tally_results[[name]] <- data.frame(
      Elephant = name,
      OG_Rows = nrow(og_data),
      Clean_Rows = nrow(cl_data),
      Match = nrow(og_data) == nrow(cl_data)
    )
    
    # Process cleaned data for seasonal/temporal summary
    # Format: 2022-01-09 00:08:00
    cl_data$fixtime <- as.POSIXct(cl_data$fixtime..GMT.0.0., format="%Y-%m-%d %H:%M:%S")
    
    # If NA, try other formats (OG had DD-MM-YYYY)
    if (all(is.na(cl_data$fixtime))) {
       cl_data$fixtime <- as.POSIXct(cl_data$fixtime..GMT.0.0., format="%d-%m-%Y %H:%M")
    }
    
    cl_data$Month <- month(cl_data$fixtime)
    cl_data$Year <- year(cl_data$fixtime)
    cl_data$Season <- get_season(cl_data$Month)
    cl_data$Period <- ifelse(cl_data$fixtime <= as.POSIXct("2024-01-31"), "Before Jan 31 2024", "After Jan 31 2024")
    
    summary <- cl_data %>%
      group_by(Season, Period) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Elephant = name)
    
    all_data_summary[[name]] <- summary
  }
}

tally_df <- bind_rows(tally_results)
summary_df <- bind_rows(all_data_summary)

print("--- Tally Results ---")
print(tally_df)

cat("\n--- Seasonal and Temporal Summary ---\n")
final_summary <- summary_df %>%
  group_by(Season, Period) %>%
  summarise(Total_Fixes = sum(Count), Elephants_Represented = n_distinct(Elephant), .groups = 'drop')

print(final_summary)

# Also print by elephant for more detail
cat("\n--- Summary by Elephant ---\n")
elephant_summary <- summary_df %>%
  pivot_wider(names_from = c(Season, Period), values_from = Count)
print(elephant_summary)
