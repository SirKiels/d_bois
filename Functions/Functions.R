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

F9_add_team_ranks <- function(matches) {
  matches <- matches %>%
    arrange(Date, Time) %>%
    mutate(match_id = row_number())
  
  match_list <- vector("list", nrow(matches))
  
  for (i in seq_len(nrow(matches))) {
    past_results <- matches[1:(i - 1), ]
    
    if (nrow(past_results) == 0) {
      match_list[[i]] <- tibble(HomeRank = NA_integer_, AwayRank = NA_integer_)
    } else {
      long_results <- past_results %>%
        select(HomeTeam, AwayTeam, FTHG, FTAG) %>%
        mutate(
          HomePoints = case_when(
            FTHG > FTAG ~ 3,
            FTHG == FTAG ~ 1,
            TRUE ~ 0
          ),
          AwayPoints = case_when(
            FTAG > FTHG ~ 3,
            FTHG == FTAG ~ 1,
            TRUE ~ 0
          )
        ) %>%
        pivot_longer(cols = c(HomeTeam, AwayTeam),
                     names_to = "Side", values_to = "Team") %>%
        mutate(
          GF = if_else(Side == "HomeTeam", FTHG, FTAG),
          GA = if_else(Side == "HomeTeam", FTAG, FTHG),
          Pts = if_else(Side == "HomeTeam", HomePoints, AwayPoints)
        ) %>%
        group_by(Team) %>%
        summarise(
          Pts = sum(Pts),
          GD = sum(GF - GA),
          GF = sum(GF),
          .groups = "drop"
        ) %>%
        arrange(desc(Pts), desc(GD), desc(GF)) %>%
        mutate(Rank = row_number())
      
      match_row <- matches[i, ]
      home_rank <- long_results %>% filter(Team == match_row$HomeTeam) %>% pull(Rank)
      away_rank <- long_results %>% filter(Team == match_row$AwayTeam) %>% pull(Rank)
      
      match_list[[i]] <- tibble(
        HomeRank = ifelse(length(home_rank) == 0, NA_integer_, home_rank),
        AwayRank = ifelse(length(away_rank) == 0, NA_integer_, away_rank)
      )
    }
  }
  
  matches %>%
    bind_cols(bind_rows(match_list)) %>%
    select(-match_id) %>%
    mutate(
      HomeRank = if_else(row_number() == 1, NA, HomeRank), # first row should be NA
      AwayRank = if_else(row_number() == 1, NA, AwayRank)
    )
}

F10_plot_logistic_effect <- function(model, predictor, n_points = 100) {
  # Extract original data from the model
  model_data <- model$model
  
  # Create a sequence of values for the predictor
  predictor_seq <- seq(
    min(model_data[[predictor]], na.rm = TRUE),
    max(model_data[[predictor]], na.rm = TRUE),
    length.out = n_points
  )
  
  # Build new data frame for prediction
  new_data <- model_data[1, , drop = FALSE]  # clone structure
  new_data <- new_data[rep(1, n_points), ]
  new_data[[predictor]] <- predictor_seq
  
  # Zero out all other predictors if needed
  for (var in names(new_data)) {
    if (var != predictor) {
      if (is.numeric(new_data[[var]])) {
        new_data[[var]] <- mean(model_data[[var]], na.rm = TRUE)
      } else if (is.factor(new_data[[var]])) {
        new_data[[var]] <- levels(model_data[[var]])[1]
      }
    }
  }
  
  # Get predicted probabilities
  new_data$predicted_prob <- predict(model, newdata = new_data, type = "response")
  
  # Plot
  ggplot(new_data, aes_string(x = predictor, y = "predicted_prob")) +
    geom_line(color = "blue", size = 1) +
    labs(
      title = paste("Predicted Probability vs.", predictor),
      x = predictor,
      y = "Predicted Probability"
    ) +
    theme_minimal()
}

F11_table_ranks_per_game <- function(List_input = results_list[[1]]){
  List_input |>
    select(2, 4:8, 45:47, 122:123) |>  # keep essentials
    arrange(Date) |>
    mutate(GameID = row_number()) |>  # Add unique game ID for tracking
    group_by(season) |>
    group_modify(~ {
      results <- .x
      league_table <- tibble(Team = character(), Points = numeric(), GD = numeric(), GoalsFor = numeric(), Played = numeric())
      
      # Storage for ranks per match
      home_ranks <- integer(nrow(results))
      away_ranks <- integer(nrow(results))
      
      for (i in seq_len(nrow(results))) {
        match <- results[i, ]
        
        # Calculate rank before this game
        if (nrow(league_table) > 0) {
          standings <- league_table |>
            arrange(desc(Points), desc(GD), desc(GoalsFor)) |>
            mutate(Rank = row_number())
          
          home_rank <- standings |> filter(Team == match$HomeTeam) |> pull(Rank)
          away_rank <- standings |> filter(Team == match$AwayTeam) |> pull(Rank)
          
          home_ranks[i] <- ifelse(length(home_rank) == 0, NA_integer_, home_rank)
          away_ranks[i] <- ifelse(length(away_rank) == 0, NA_integer_, away_rank)
        } else {
          home_ranks[i] <- NA_integer_
          away_ranks[i] <- NA_integer_
        }
        
        # Update league table after this game
        home_team <- match$HomeTeam
        away_team <- match$AwayTeam
        FTHG <- match$FTHG
        FTAG <- match$FTAG
        
        home_points <- if (FTHG > FTAG) 3 else if (FTHG == FTAG) 1 else 0
        away_points <- if (FTAG > FTHG) 3 else if (FTHG == FTAG) 1 else 0
        
        update_team <- function(tbl, team, points, gf, ga) {
          existing <- tbl |> filter(Team == team)
          if (nrow(existing) == 0) {
            tibble(Team = team, Points = points, GD = gf - ga, GoalsFor = gf, Played = 1)
          } else {
            tbl |> mutate(
              Points = if_else(Team == team, Points + points, Points),
              GD = if_else(Team == team, GD + (gf - ga), GD),
              GoalsFor = if_else(Team == team, GoalsFor + gf, GoalsFor),
              Played = if_else(Team == team, Played + 1, Played)
            )
          }
        }
        
        league_table <- league_table |>
          rows_upsert(update_team(league_table, home_team, home_points, FTHG, FTAG), by = "Team") |>
          rows_upsert(update_team(league_table, away_team, away_points, FTAG, FTHG), by = "Team")
      }
      
      # Add ranks to results
      results |> mutate(
        Draw_1_No_Draw_0 = case_when(FTR == "D" ~ 1, TRUE ~ 0),
        HomeRank = home_ranks,
        AwayRank = away_ranks,
        Distance_Teams = sqrt((HomeRank - AwayRank)^2),
        Distance_Teams_Higher_Lower = 
          case_when(
            HomeRank - AwayRank < 0 ~ "Home Higher",
            HomeRank - AwayRank > 0 ~ "Away Higher",
            TRUE ~ NA
          ),
        CategoriesRankHome = case_when(
          home_ranks <= 3 ~ "Top3",
          home_ranks >= 16 ~ "Bottom3",
          home_ranks <= 9 & home_ranks >= 4 ~ "Middle4to9",
          home_ranks <= 15 & home_ranks >= 10 ~ "Middle10to15",
          TRUE        ~ NA
        ),
        CategoriesRankAway = case_when(
          away_ranks <= 3 ~ "Top3",
          away_ranks >= 16 ~ "Bottom3",
          away_ranks <= 9 & away_ranks >= 4 ~ "Middle4to9",
          away_ranks <= 15 & away_ranks >= 10 ~ "Middle10to15",
          TRUE        ~ NA
        )
      )
    }) |>
    ungroup() |> 
    arrange(GameID) |> 
    select(-GameID)
}

F12_plot_random_effect_lines <- function(model, data, distance_var, group_var, 
                                         num_points = 100, max_groups = NULL) {
  # Limit to top N groups by frequency if requested
  if (!is.null(max_groups)) {
    top_groups <- data %>%
      count(!!sym(group_var), sort = TRUE) %>%
      slice_head(n = max_groups) %>%
      pull(!!sym(group_var))
  } else {
    top_groups <- unique(data[[group_var]])
  }
  
  # Generate prediction grid
  distance_seq <- seq(min(data[[distance_var]], na.rm = TRUE),
                      max(data[[distance_var]], na.rm = TRUE),
                      length.out = num_points)
  
  newdata <- expand.grid(
    distance = distance_seq,
    group = top_groups
  )
  
  # Rename columns to match model input
  names(newdata) <- c(distance_var, group_var)
  
  # Predict
  newdata$pred <- predict(model, newdata = newdata, type = "response", allow.new.levels = TRUE)
  
  # Plot
  ggplot(newdata, aes_string(x = distance_var, y = "pred", color = group_var)) +
    geom_line() +
    labs(
      x = distance_var,
      y = "Predicted probability of draw",
      color = group_var
    ) +
    theme_minimal()
}
