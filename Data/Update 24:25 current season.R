# Update 24/25 current season

# Packages
library(tidyverse)
library(rvest)
library(purrr)
library(lubridate)

# Source functions from GitHub
source("https://raw.githubusercontent.com/SirKiels/d_bois/main/Functions/Functions.R")
# Load existing data from GitHub
load(url("https://raw.githubusercontent.com/SirKiels/d_bois/main/Data/results_list.RData"))

# Define current season URL
current_season_url <- "https://www.football-data.co.uk/mmz4281/2425/N1.csv"

# Import latest season data
latest_data <- F1_import_csv(current_season_url)[[1]]

# Fix dates if necessary
latest_data <- F7_fix_dates(latest_data)

# Add season column
latest_data <- latest_data |> 
  mutate(season = "2024-2025")

# Scrape match dates
match_dates <- F6_scrape_match_dates_multiple_seasons("2024-2025", 34) |> 
  as_tibble() |> 
  mutate(date = dmy(date)) |> 
  rename(scraped_date = date)

# Merge match dates
latest_data <- left_join(
  latest_data, match_dates, 
  by = c("Date" = "scraped_date"),
  relationship = "many-to-many")

# Drop rows with NA dates
latest_data <- latest_data |> drop_na(Date)

# Identify existing dates in stored results
existing_dates <- results_list$season_2425$Date

# Filter only new rows
new_rows <- latest_data |> filter(!Date %in% existing_dates)

# Append new rows to existing data
results_list$season_2425 <- bind_rows(results_list$season_2425, new_rows)

# Cleanup: remove everything except for "results_list"
rm(list = setdiff(ls(), "results_list"))

# Save updated object
save(results_list, file = "Data/results_list.RData")


