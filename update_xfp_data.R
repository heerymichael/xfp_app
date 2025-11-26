# update_xfp_data.R - Run this script weekly to update the data
# This script runs SEPARATELY from your Shiny app

library(googlesheets4)
library(tidyverse)
library(janitor)

cat("====================================\n")
cat("XFP DATA UPDATE SCRIPT\n")
cat("Run this weekly to update app data\n")
cat("====================================\n\n")


# Source sheet URL (your main data source)
source_sheet_url <- "https://docs.google.com/spreadsheets/d/1gULbmQ5vmaeoxovra-I1pWP6GlI0tLhqx6NuUCG-uEc/edit?gid=1280900748#gid=1280900748"

# Target PUBLIC sheet URL (where processed data goes)
target_sheet_url <- "https://docs.google.com/spreadsheets/d/1N8Dn8d-3OiqjqATvsz9d79buHBSdz3chjp4wMtd47es/edit?gid=0#gid=0"

# Download source data ====
cat("Step 1: Downloading source data from Google Sheets...\n")

weekly_air_yards <- read_sheet(source_sheet_url, sheet = "WeeklyAirYards") %>% 
  clean_names()

weekly_rb_xp <- read_sheet(source_sheet_url, sheet = "WeeklyRBXP") %>% 
  clean_names()

cat("✓ Source data downloaded successfully.\n\n")

# Process combined_xfp_data ====
cat("Step 2: Processing combined xFP data...\n")

# Process WR/TE/QB data
wr_te_qb_data <- weekly_air_yards %>%
  filter(position != "RB") %>%
  mutate(
    player = receiver,
    xFP = ifelse(!is.na(knn_fps), knn_fps, 0),
    actual_FP = ppr_points,
    carries = 0,
    rush_yards = 0,
    rush_touchdowns = 0,
    goal_line_carry = 0
  ) %>%
  select(
    player, position, team, week, season, receiver_id,
    xFP, actual_FP,
    target, air_yards, rz_target, receptions,
    receiving_yards, receiving_touchdowns,
    carries, rush_yards, rush_touchdowns, goal_line_carry,
    adot, catch_rate, ypt
  )

# Process RB data
rb_combined_data <- weekly_rb_xp %>%
  mutate(
    player = player,
    position = "RB",
    team = team,
    xFP = knn_fps,
    actual_FP = actual_fps,
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
    target, air_yards, rz_target, receptions,
    receiving_yards, receiving_touchdowns,
    carries, rush_yards, rush_touchdowns, goal_line_carry,
    adot, catch_rate, ypt
  )

# Combine and standardize
combined_xfp_data <- bind_rows(wr_te_qb_data, rb_combined_data) %>%
  mutate(
    # Standardize team names
    team = case_when(
      team == "LA" ~ "LAR",
      team == "STL" ~ "LAR",
      team == "SD" ~ "LAC",
      team == "OAK" ~ "LV",
      TRUE ~ team
    ),
    # Add calculated fields
    xFP = as.numeric(xFP),
    actual_FP = as.numeric(actual_FP),
    fp_diff = actual_FP - xFP,
    games = 1
  ) %>%
  arrange(team, week, desc(xFP))

cat("✓ Combined xFP data processed.\n")
cat("  - Total rows:", nrow(combined_xfp_data), "\n")
cat("  - Unique players:", n_distinct(combined_xfp_data$player), "\n")
cat("  - Weeks included:", paste(sort(unique(combined_xfp_data$week)), collapse = ", "), "\n\n")

# Upload to public Google Sheet ====
cat("Step 3: Uploading processed data to public Google Sheet...\n")

# Clear existing sheet and write new data
sheet_write(
  combined_xfp_data,
  ss = target_sheet_url,
  sheet = "combined_xfp_data"
)

cat("✓ Data successfully uploaded to Google Sheet.\n\n")

# Process QB/Offense data ====
cat("Step 4: Processing QB/Offense data...\n")

# Source sheet URL for Offenses data
offenses_source_url <- "https://docs.google.com/spreadsheets/d/1St_fXXt15YHSu92ombzmXnQAxX8rmjQKhYfSnMMUuuQ/edit?gid=0#gid=0"

# Target sheet URL for QB data
qb_target_url <- "https://docs.google.com/spreadsheets/d/10MbQBNY1fNJ1pp5VnNXmEtX-TdAsaN_hOE6DpVy-krI/edit?gid=0#gid=0"

# Download Offenses data
offenses_data <- read_sheet(offenses_source_url, sheet = "Offenses") %>%
  clean_names()

cat("✓ Offenses data downloaded successfully.\n")

# Process and filter QB data
qb_data <- offenses_data %>%
  filter(!is.na(as.numeric(week))) %>%  # Filter out non-numeric weeks
  select(season, week, posteam, team_qb)

cat("✓ QB data processed.\n")
cat("  - Total rows:", nrow(qb_data), "\n")
cat("  - Seasons included:", paste(sort(unique(qb_data$season)), collapse = ", "), "\n")
cat("  - Weeks included:", paste(sort(unique(as.numeric(qb_data$week))), collapse = ", "), "\n\n")

# Upload QB data to target sheet
cat("Uploading QB data to target Google Sheet...\n")

sheet_write(
  qb_data,
  ss = qb_target_url,
  sheet = "Sheet1"
)

cat("✓ QB data successfully uploaded to Google Sheet.\n\n")

# Optional: Save local backup
cat("Step 5: Saving local backup...\n")
dir.create("data", showWarnings = FALSE)
write_csv(combined_xfp_data, "data/combined_xfp_data_backup.csv")
write_csv(qb_data, "data/qb_data_backup.csv")
cat("✓ Local backups saved to data/ folder\n\n")

# Summary
cat("====================================\n")
cat("UPDATE COMPLETE!\n")
cat("====================================\n")
cat("Processed", nrow(combined_xfp_data), "rows of xFP data\n")
cat("Processed", nrow(qb_data), "rows of QB data\n\n")
cat("Data is now available at:\n")
cat("xFP Data:", target_sheet_url, "\n")
cat("QB Data:", qb_target_url, "\n\n")
cat("Your Shiny app will automatically use this updated data.\n")
cat("No need to redeploy the app!\n")

