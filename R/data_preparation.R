# R/data_preparation.R - Prepare and combine xFP data from source files

library(tidyverse)
library(janitor)

#' Prepare xFP data from source files
#' @return List containing combined_xfp_data and all_players_xfp
prepare_xfp_data_from_source <- function() {
  
  # Check if source files exist
  wr_te_file <- "data/NFL Weekly Volume - WeeklyAirYards.csv"
  rb_file <- "data/NFL Weekly Volume - WeeklyRBXP.csv"
  
  if (!file.exists(wr_te_file) || !file.exists(rb_file)) {
    stop("Source data files not found. Please ensure both files exist:
         - data/NFL Weekly Volume - WeeklyAirYards.csv
         - data/NFL Weekly Volume - WeeklyRBXP.csv")
  }
  
  # Process WR/TE data
  wr_te_xfp <- read_csv(wr_te_file, show_col_types = FALSE) %>%
    select(receiver, position, team, week, season, knn_fps) %>%
    rename(player = receiver) %>%
    filter(position != "RB")
  
  # Process RB data - Note the capital T in Team
  rb_xfp <- read_csv(rb_file, show_col_types = FALSE) %>%
    rename(team = Team) %>%  # Rename Team to team for consistency
    select(player, week, season, team, knn_fps) %>%
    mutate(position = "RB")
  
  # Combine all players
  all_players_xfp <- bind_rows(wr_te_xfp, rb_xfp) %>%
    # Standardize team names
    mutate(
      team = case_when(
        team == "LA" ~ "LAR",
        team == "STL" ~ "LAR",
        team == "SD" ~ "LAC",
        team == "OAK" ~ "LV",
        TRUE ~ team
      )
    )
  
  # Save processed file for future use
  write_csv(all_players_xfp, "data/all_players_xfp.csv")
  
  # Create combined xFP data for the main analysis
  combined_xfp_data <- create_combined_xfp_dataset()
  
  return(list(
    combined_xfp_data = combined_xfp_data,
    all_players_xfp = all_players_xfp
  ))
}

#' Create combined xFP dataset with actual points
#' @return Data frame with combined xFP and actual points
create_combined_xfp_dataset <- function() {
  
  # Read source files
  air_yards_data <- read_csv("data/NFL Weekly Volume - WeeklyAirYards.csv", 
                             show_col_types = FALSE)
  rb_data <- read_csv("data/NFL Weekly Volume - WeeklyRBXP.csv", 
                      show_col_types = FALSE)
  
  # Process WR/TE/QB data (exclude RBs to avoid double counting)
  wr_te_qb_data <- air_yards_data %>%
    filter(position != "RB") %>%
    mutate(
      player = receiver,
      xFP = ifelse(!is.na(knn_fps), knn_fps, 0),
      actual_FP = ppr_points,
      # Add rushing columns as 0 for WR/TE
      carries = 0,
      rush_yards = 0,
      rush_touchdowns = 0,
      goal_line_carry = 0
    ) %>%
    select(
      player, position, team, week, season, receiver_id,
      xFP, actual_FP,
      # Receiving stats
      target, air_yards, rz_target, receptions,
      receiving_yards, receiving_touchdowns,
      # Rushing stats (all 0 for WR/TE)
      carries, rush_yards, rush_touchdowns, goal_line_carry,
      # Other useful fields
      adot, catch_rate, ypt
    )
  
  # Process RB data - handling the capital T in Team
  rb_combined_data <- rb_data %>%
    rename(team = Team) %>%  # Fix the capital T
    mutate(
      player = player,
      position = "RB",
      xFP = knn_fps,  # Already total (rushing + receiving)
      actual_FP = actual_fps,  # Already total
      # Map the existing columns correctly
      target = targets,
      rz_target = NA_real_,
      receptions = NA_real_,
      receiving_yards = NA_real_,
      receiving_touchdowns = NA_real_,
      receiver_id = paste0(player, "-", team),
      adot = NA_real_,
      catch_rate = NA_real_,
      ypt = NA_real_
    ) %>%
    select(
      player, position, team, week, season, receiver_id,
      xFP, actual_FP,
      # Receiving stats
      target, air_yards, rz_target, receptions,
      receiving_yards, receiving_touchdowns,
      # Rushing stats
      carries, rush_yards, rush_touchdowns, goal_line_carry,
      # Other fields
      adot, catch_rate, ypt
    )
  
  # Combine the datasets
  combined_data <- bind_rows(wr_te_qb_data, rb_combined_data) %>%
    # Standardize team names
    mutate(
      team = case_when(
        team == "LA" ~ "LAR",
        team == "STL" ~ "LAR",
        team == "SD" ~ "LAC",
        team == "OAK" ~ "LV",
        TRUE ~ team
      )
    ) %>%
    arrange(team, week, desc(xFP))
  
  # Add calculated fields
  combined_data <- combined_data %>%
    mutate(
      xFP = as.numeric(xFP),
      actual_FP = as.numeric(actual_FP),
      fp_diff = actual_FP - xFP,
      games = 1
    )
  
  # Save for future use
  write_csv(combined_data, "data/combined_xfp_data.csv")
  
  return(combined_data)
}

#' Force regenerate all data files
#' Use this when source data changes
force_regenerate_data <- function() {
  # Delete existing processed files
  if (file.exists("data/combined_xfp_data.csv")) {
    unlink("data/combined_xfp_data.csv")
    cat("Deleted existing combined_xfp_data.csv\n")
  }
  
  if (file.exists("data/all_players_xfp.csv")) {
    unlink("data/all_players_xfp.csv")
    cat("Deleted existing all_players_xfp.csv\n")
  }
  
  # Regenerate
  cat("Regenerating data files...\n")
  prepare_xfp_data_from_source()
  cat("Data regeneration complete!\n")
}
