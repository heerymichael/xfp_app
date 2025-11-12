# R/data_processing.R - Data processing functions (NO downloading/reading files)

library(dplyr)
library(tidyr)

# Standardize team names (fix LA -> LAR issue)
standardize_team_names <- function(team) {
  case_when(
    team == "LA" ~ "LAR",
    team == "STL" ~ "LAR",
    team == "SD" ~ "LAC",
    team == "OAK" ~ "LV",
    TRUE ~ team
  )
}

# Process xFP data
process_xfp_data <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data)
  
  data %>%
    mutate(
      team = standardize_team_names(team),
      xFP = as.numeric(xFP),
      actual_FP = as.numeric(actual_FP),
      fp_diff = actual_FP - xFP
    )
}

# Get team name mapping
get_team_name_mapping <- function() {
  data.frame(
    team_abbr = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", 
                  "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", 
                  "LAC", "LAR", "LV", "MIA", "MIN", "NE", "NO", "NYG", 
                  "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS"),
    team_name = c("ARIZONA CARDINALS", "ATLANTA FALCONS", "BALTIMORE RAVENS", 
                  "BUFFALO BILLS", "CAROLINA PANTHERS", "CHICAGO BEARS", 
                  "CINCINNATI BENGALS", "CLEVELAND BROWNS", "DALLAS COWBOYS", 
                  "DENVER BRONCOS", "DETROIT LIONS", "GREEN BAY PACKERS", 
                  "HOUSTON TEXANS", "INDIANAPOLIS COLTS", "JACKSONVILLE JAGUARS", 
                  "KANSAS CITY CHIEFS", "LOS ANGELES CHARGERS", "LOS ANGELES RAMS", 
                  "LAS VEGAS RAIDERS", "MIAMI DOLPHINS", "MINNESOTA VIKINGS", 
                  "NEW ENGLAND PATRIOTS", "NEW ORLEANS SAINTS", "NEW YORK GIANTS", 
                  "NEW YORK JETS", "PHILADELPHIA EAGLES", "PITTSBURGH STEELERS", 
                  "SEATTLE SEAHAWKS", "SAN FRANCISCO 49ERS", "TAMPA BAY BUCCANEERS", 
                  "TENNESSEE TITANS", "WASHINGTON COMMANDERS"),
    stringsAsFactors = FALSE
  )
}

# Prepare data for Top Performers table
prepare_top_performers_data <- function(data, position, weeks, min_games, num_players, 
                                        rookies_only = FALSE, rookie_list = NULL,
                                        view_mode = NULL, min_xfp = NULL, 
                                        excluded_players = NULL) {
  
  if (is.null(data) || nrow(data) == 0) return(data.frame())
  
  if (is.null(num_players) || is.na(num_players) || !is.numeric(num_players)) {
    num_players <- 12
  }
  num_players <- as.integer(num_players)
  if (num_players < 1) num_players <- 12
  
  team_names <- get_team_name_mapping()
  
  # Filter by position only if not "ALL"
  filtered_data <- data %>%
    filter(week %in% !!weeks)
  
  if (position != "ALL") {
    # Handle WR_TE combination
    if (position == "WR_TE") {
      filtered_data <- filtered_data %>%
        filter(position %in% c("WR", "TE"))
    } else {
      filtered_data <- filtered_data %>%
        filter(position == !!position)
    }
  }
  
  # Apply rookies filter if enabled
  if (rookies_only && !is.null(rookie_list)) {
    filtered_data <- filtered_data %>%
      filter(player %in% rookie_list)
  }
  
  # Apply excluded players filter
  if (!is.null(excluded_players) && length(excluded_players) > 0) {
    filtered_data <- filtered_data %>%
      filter(!(player %in% excluded_players))
  }
  
  result <- filtered_data %>%
    group_by(player, position, team) %>%
    summarise(
      games_played = n_distinct(week),
      weeks_played_list = list(sort(unique(week))),
      total_expected = sum(xFP, na.rm = TRUE),
      total_actual = sum(actual_FP, na.rm = TRUE),
      expected_points = mean(xFP, na.rm = TRUE),
      actual_points = mean(actual_FP, na.rm = TRUE),
      receiver_id = first(receiver_id),
      .groups = "drop"
    ) %>%
    left_join(team_names, by = c("team" = "team_abbr")) %>%
    mutate(
      expected_points = coalesce(expected_points, 0),
      actual_points = coalesce(actual_points, 0),
      team_name = coalesce(team_name, paste(team, "TEAM")),
      fp_diff_per_game = actual_points - expected_points
    ) %>%
    filter(games_played >= !!min_games)
  
  # Apply minimum xFP filter if provided
  if (!is.null(min_xfp) && !is.na(min_xfp)) {
    result <- result %>%
      filter(expected_points >= !!min_xfp)
  } else {
    result <- result %>%
      filter(expected_points > 0)
  }
  
  # Sort based on view_mode
  if (!is.null(view_mode)) {
    if (view_mode == "underperformers") {
      # For underperformers: sort by highest expected xFP
      result <- result %>%
        arrange(desc(expected_points))
    } else if (view_mode == "overperformers") {
      # For overperformers: sort by highest actual FP
      result <- result %>%
        arrange(desc(actual_points))
    } else {
      # Default sorting by expected points
      result <- result %>%
        arrange(desc(expected_points))
    }
  } else {
    # Default sorting by expected points
    result <- result %>%
      arrange(desc(expected_points))
  }
  
  result %>%
    slice_head(n = num_players)
}

# Prepare data for Team Summaries
prepare_team_summary_data <- function(data, team, position, weeks, min_games) {
  
  if (is.null(data) || nrow(data) == 0) return(data.frame())
  
  filtered_data <- data %>%
    filter(week %in% !!weeks)
  
  if (team != "" && team != "ALL") {
    filtered_data <- filtered_data %>%
      filter(team == !!team)
  }
  
  if (position != "ALL") {
    filtered_data <- filtered_data %>%
      filter(position == !!position)
  }
  
  filtered_data %>%
    group_by(player, position) %>%
    summarise(
      games_played = n_distinct(week),
      total_expected = sum(xFP, na.rm = TRUE),
      total_actual = sum(actual_FP, na.rm = TRUE),
      avg_expected = mean(xFP, na.rm = TRUE),
      avg_actual = mean(actual_FP, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(games_played >= !!min_games) %>%
    arrange(desc(avg_expected))
}

# Get team players for Team Detail
get_team_players <- function(data, team, min_games = 3) {
  
  if (is.null(data) || nrow(data) == 0 || length(team) == 0) {
    return(data.frame())
  }
  
  data %>%
    filter(team == !!team) %>%
    group_by(player, position) %>%
    summarise(
      avg_xfp = mean(xFP, na.rm = TRUE),
      games = n(),
      .groups = "drop"
    ) %>%
    filter(games >= !!min_games) %>%
    arrange(desc(avg_xfp))
}

# Prepare data for Team Detail facet plot
prepare_team_facet_data <- function(data, team, included_players, min_games = 3) {
  
  if (is.null(data) || nrow(data) == 0 || length(team) == 0) {
    return(NULL)
  }
  
  team_data <- data %>%
    filter(
      team == !!team,
      player %in% !!included_players
    )
  
  if (nrow(team_data) == 0) {
    return(NULL)
  }
  
  player_order <- team_data %>%
    group_by(player, position) %>%
    summarise(
      avg_xfp = mean(xFP, na.rm = TRUE),
      games_played = n(),
      .groups = "drop"
    ) %>%
    filter(games_played >= !!min_games) %>%
    arrange(desc(avg_xfp)) %>%
    pull(player)
  
  team_data %>%
    filter(player %in% player_order) %>%
    mutate(player = factor(player, levels = player_order))
}

# Get position color
get_position_color <- function(position) {
  switch(position,
         "QB" = "#E91E63",
         "RB" = "#2196F3",
         "WR" = "#4CAF50",
         "TE" = "#FF9800",
         "#999999"
  )
}

# Calculate week range text
get_week_range_text <- function(selected_weeks, all_weeks) {
  if (length(selected_weeks) == 0) {
    "no weeks selected"
  } else if (length(selected_weeks) == 1) {
    paste0("Week ", selected_weeks[1])
  } else if (length(selected_weeks) == length(all_weeks) && 
             all(selected_weeks == all_weeks)) {
    "the entire 2025 season"
  } else if (length(selected_weeks) == 4 && 
             all(selected_weeks == tail(all_weeks, 4))) {
    "the last four weeks"
  } else {
    paste0("Weeks ", paste(selected_weeks, collapse = ", "))
  }
}

# Format subtitle text for tables
format_table_subtitle <- function(position, selected_weeks, all_weeks) {
  pos_name <- switch(position,
                     "RB" = "running backs",
                     "WR" = "wide receivers", 
                     "TE" = "tight ends",
                     "ALL" = "all positions"
  )
  
  week_text <- get_week_range_text(selected_weeks, all_weeks)
  
  if (length(selected_weeks) == 0) {
    paste0("Table shows actual and expected fantasy points for ", pos_name, " for ", week_text, ".")
  } else if (length(selected_weeks) == 1) {
    paste0("Table shows actual and expected fantasy points for ", pos_name, " in ", week_text, " of the 2025 season.")
  } else {
    paste0("Table shows actual and expected fantasy points for ", pos_name, " during ", week_text, ".")
  }
}