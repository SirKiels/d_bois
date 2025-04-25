# Functions
F1_import_csv <- function(addresses) {
  addresses %>%
    map(~ as_tibble(read.csv(.))) %>%
    map(~ mutate(., 
                 Date = as.Date(Date, format = "%d/%m/%Y"), 
                 SHHG = (FTHG - HTHG) , # Second half home goal
                 SHAG = (FTAG - HTAG))) %>% # Second half away goal)) 
    set_names(basename(addresses))  # Names the list elements after the file names
}

F1_1_add_season_column <- function(list_input) {
  map2(list_input, names(list_input), ~ mutate(.x, Season = .y))
}
  
F2_team_results_history <- function(team_name, df_list) {
  df_list %>%
    map(~ .x %>%
           filter(HomeTeam == team_name | AwayTeam == team_name) %>%
           mutate(
             Result = case_when(
               (HomeTeam == team_name & FTR == "H") | (AwayTeam == team_name & FTR == "A") ~ "W",
               FTR == "D" ~ "D",
               TRUE ~ "L"
             )
           ) %>%
           select(Date, HomeTeam, AwayTeam, FTHG, FTAG, Result) %>%
           arrange(Date))
}

# Team1 v Team2 filtered results
F3_filter_team1_team2_results <- function(Team1, Team2, df_list) {
  df_list %>%
    map(~ .x %>%
           filter((HomeTeam == Team1 & AwayTeam == Team2) | 
                  (HomeTeam == Team2 & AwayTeam == Team1)) %>%
           select(Date, HomeTeam, AwayTeam, FTHG, FTAG, Result) %>%
           arrange(Date))
}

# Generate standings for a season
F4_standings <- function(df_in_list = results_list[[1]]){
  df <- df_in_list %>%
    mutate(
      HomePoints = case_when(FTR == "H" ~ 3, FTR == "D" ~ 1, TRUE ~ 0),
      AwayPoints = case_when(FTR == "A" ~ 3, FTR == "D" ~ 1, TRUE ~ 0)
    ) %>%
    select(HomeTeam, AwayTeam, FTHG, FTAG, HomePoints, AwayPoints) %>%
    pivot_longer(cols = c(HomeTeam, AwayTeam), names_to = "Location", values_to = "Team") %>%
    mutate(
      Points = if_else(Location == "HomeTeam", HomePoints, AwayPoints),
      GoalsFor = if_else(Location == "HomeTeam", FTHG, FTAG),
      GoalsAgainst = if_else(Location == "HomeTeam", FTAG, FTHG)
    ) %>%
    group_by(Team) %>%
    summarise(
      Played = n(),
      Wins = sum(Points == 3),
      Draws = sum(Points == 1),
      Losses = sum(Points == 0),
      GoalsFor = sum(GoalsFor),
      GoalsAgainst = sum(GoalsAgainst),
      GoalDiff = GoalsFor - GoalsAgainst,
      Points = sum(Points)
    ) %>%
    arrange(desc(Points), desc(GoalDiff), desc(GoalsFor))
  
  df
}

F5_LM_Team <- function(formula, data, team = NULL, HG = TRUE, AG = TRUE) {
  # Filter data only if a team is specified
  if (!is.null(team)) {
    if (HG & AG) {
      # Include both home and away matches for the team
      data <- data %>%
        filter(HomeTeam == team | AwayTeam == team)
    } else if (HG) {
      # Only include home matches for the team
      data <- data %>%
        filter(HomeTeam == team)
    } else if (AG) {
      # Only include away matches for the team
      data <- data %>%
        filter(AwayTeam == team)
    } else {
      stop("Both HG and AG are FALSE, no matches to analyze.")
    }
    
    # Check if enough data remains after filtering
    if (nrow(data) < 2) {
      stop("Not enough data for regression after filtering by team and home/away games.")
    }
  }
  
  # Fit the linear model
  model <- lm(formula, data = data)
  
  return(summary(model))
}

F6_avg_goals_1st_2nd_FT <- function(data, teamname) {
  # Filter for the specified home team
  team_data <- subset(data, teamname == HomeTeam)
  
  # Calculate mean goals for home team
  mean_home_1st_half <- mean(team_data$HTHG, na.rm = TRUE)
  mean_home_2nd_half <- mean(team_data$SHHG, na.rm = TRUE)
  mean_home_FT       <- mean(team_data$FTHG, na.rm = TRUE)
  
  # Calculate mean goals for away team
  mean_away_1st_half <- mean(team_data$HTAG, na.rm = TRUE)
  mean_away_2nd_half <- mean(team_data$SHAG, na.rm = TRUE)
  mean_away_FT       <- mean(team_data$FTAG, na.rm = TRUE)
  
  # Create summary table
  result <- data.frame(
    Half = c("1st Half", "2nd Half", "Full Time"),
    Mean_Home_Goals = c(mean_home_1st_half, mean_home_2nd_half, mean_home_FT),
    Mean_Goals_Against = c(mean_away_1st_half, mean_away_2nd_half, mean_away_FT)
  )
  
  return(result)
}

# Function to scrape dates based on season and number of rounds
F6_scrape_match_dates_multiple_seasons <- function(seasons, num_rounds) {
  all_dates <- data.frame()

  # Loop over each season
  for (season in seasons) {
    for (round_number in 1:num_rounds) {
      url <- paste0("https://www.voetbal.com/wedstrijdgegevens/ned-eredivisie-", season, "-spieltag/", round_number, "/")
      page <- read_html(url)
      
      fixture_table <- page %>%
        html_elements("table") %>%
        .[[2]]
      
      all_cells <- fixture_table %>%
        html_elements("td") %>%
        html_text2() %>%
        trimws()
      
      dates <- all_cells[grepl("^\\d{2}\\.\\d{2}\\.\\d{4}$", all_cells)]
      
      round_df <- data.frame(season = season, round = round_number, date = dates)
      all_dates <- rbind(all_dates, round_df)
    }
  }

  return(all_dates)
}
#library(rvest)
#seasons <- c("2021-2022", "2022-2023", "2023-2024")
#num_rounds <- 34
#match_dates <- F6_scrape_match_dates_multiple_seasons(seasons, num_rounds)

F7_fix_dates <- function(df) {
  date_parts <- as.POSIXlt(df$Date)
  valid_idx <- !is.na(date_parts$year) & date_parts$year < 100
  date_parts$year[valid_idx] <- date_parts$year[valid_idx] + 2000
  df$Date <- as.Date(date_parts)
  return(df)
}

F8_drop_na_rows <- function(List_Name, Name_DF_in_list) {
  cleaned_list <- map(List_Name, ~ .x %>% filter(!is.na(.data[[Name_DF_in_list]])))
  return(cleaned_list)
}
