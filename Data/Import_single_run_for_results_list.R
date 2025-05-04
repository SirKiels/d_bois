# Todo:
# Turn current season in run to update

# Packages
library(tidyverse)
library(rvest)
library(purrr)
library(lubridate)
library(pointblank)
library(skimmer)

# Source functions from GitHub
source("https://raw.githubusercontent.com/SirKiels/d_bois/main/Functions/Functions.R")

# Import data sets
# concatenate addresses for function
{
  file_addresses <- c(
  "https://www.football-data.co.uk/mmz4281/2425/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/2324/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/2223/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/2122/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/2021/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/1920/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/1819/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/1718/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/1617/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/1516/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/1415/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/1314/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/1213/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/1112/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/1011/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/0910/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/0809/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/0708/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/0607/N1.csv",  
  "https://www.football-data.co.uk/mmz4281/0506/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/0405/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/0304/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/0203/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/0102/N1.csv", 
  "https://www.football-data.co.uk/mmz4281/0001/N1.csv"
  )
}

# Call the function to process the files and store them in a list
results_list <- F1_import_csv(file_addresses)
remove(file_addresses)

# Replace weird years in dfs 8:25
results_list[8:25] <- map(results_list[8:25], F7_fix_dates)

# Rename objects in list
{
  names(results_list) <- c(
  "season_2425", 
  "season_2324", 
  "season_2223", 
  "season_2122", 
  "season_2021", 
  "season_1920",
  "season_1819", 
  "season_1718", 
  "season_1617", 
  "season_1516", 
  "season_1415", 
  "season_1314", 
  "season_1213", 
  "season_1112", 
  "season_1011", 
  "season_0910", 
  "season_0809", 
  "season_0708", 
  "season_0607", 
  "season_0506", 
  "season_0405", 
  "season_0304", 
  "season_0203", 
  "season_0102", 
  "season_0001"
  )
}

# Add season names as column
results_list <- F1_1_add_season_column(results_list)

# Scrape dates and merge
# Scrape
{
seasons_25 <- c(
  "2024-2025", 
  "2023-2024", 
  "2022-2023", 
  "2021-2022", 
  "2020-2021",
  "2019-2020",
  "2018-2019",
  "2017-2018",
  "2016-2017",
  "2015-2016",
  "2014-2015", 
  "2013-2014", 
  "2012-2013", 
  "2011-2012", 
  "2010-2011",
  "2009-2010",
  "2008-2009",
  "2007-2008",
  "2006-2007",
  "2005-2006",
  "2004-2005",
  "2003-2004",
  "2002-2003",
  "2001-2002",
  "2000-2001"
)
}
num_rounds <- 34
match_dates <- F6_scrape_match_dates_multiple_seasons(seasons_25, num_rounds)
match_dates <- match_dates |>  as_tibble()

match_dates <- match_dates |>
  mutate(date = dmy(date)) |> 
  rename(scraped_date = date)

# Merge over tables in list
results_list <- 
  map(results_list, ~ left_join(.x, match_dates, by = c("Date" = "scraped_date")))

# Quick column quality check
QA_Date <- map_df(results_list, ~ summarise(.x, 
                                 min_date = min(Date, na.rm = TRUE),
                                 max_date = max(Date, na.rm = TRUE),
                                 n_na = sum(is.na(Date))),
       .id = "dataframe")

# Problems with: season_0203, season_0708, season_0405, season_1213
{
#season_0203
#2002-08-16
#2003-05-29
#176
#18
#season_0708
#2007-08-17
#2008-04-20
#32
#21
#season_0405
#2004-08-13
#2005-05-22
#16
#13
#season_1213
#2012-08-10
#2013-05-12
#3
}
# Turns out there are empty rows and any row that has Date empty can be removed
# So all dfs, remove row if Date is NA
results_list <- F8_drop_na_rows(results_list, "Date")

# Quick clean
remove(num_rounds, seasons_25, match_dates, QA_Date)

# Save as R object
save(results_list, file = "results_list.RData")

