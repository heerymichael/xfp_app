# global.R - Load all dependencies and configurations
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(janitor)
library(reactable)
library(htmltools)
library(shinyjs)
library(showtext)
library(base64enc)
library(googlesheets4)

# Configure Google Sheets to work without authentication for public sheets
gs4_deauth()

# URL of your public Google Sheet with processed data
PUBLIC_DATA_SHEET_URL <- "https://docs.google.com/spreadsheets/d/1N8Dn8d-3OiqjqATvsz9d79buHBSdz3chjp4wMtd47es/edit?gid=0#gid=0"

# Rookies list 
rookie_list = c(
  "Ashton Jeanty", "Omarion Hampton", "Tetairoa McMillan", "Travis Hunter", 
  "TreVeyon Henderson", "Emeka Egbuka", "Tyler Warren", "Kaleb Johnson", 
  "Colston Loveland", "Matthew Golden", "Quinshon Judkins", "Luther Burden III", 
  "Cam Ward", "RJ Harvey", "Tre' Harris", "Jayden Higgins", "Cam Skattebo", 
  "Jaxson Dart", "Jack Bech", "Bhayshul Tuten", "Jaylin Noel", "Dylan Sampson", 
  "Jalen Milroe", "Mason Taylor", "Kyle Williams", "Jalen Royals", 
  "Harold Fannin Jr.", "Elic Ayomanor", "Elijah Arroyo", "Devin Neal", 
  "Shedeur Sanders", "Tyler Shough", "Pat Bryant", "Jaydon Blue", "DJ Giddens", 
  "Terrance Ferguson", "Ollie Gordon II", "Tory Horton", "Trevor Etienne", 
  "Savion Williams", "Jordan James", "Tahj Brooks", "Woody Marks", "Tai Felton", 
  "Jarquez Hunter", "Brashard Smith", "Kyle Monangai", "Oronde Gadsden II", 
  "Will Howard", "Isaac TeSlaa", "Dillon Gabriel", "Gunnar Helm", 
  "Damien Martinez", "Tez Johnson", "Xavier Restrepo", "Dont'e Thornton Jr.", 
  "Kyle McCord", "Chimere Dike", "Quinn Ewers", "Ricky White III", 
  "Jacory Croskey-Merritt", "LeQuint Allen Jr.", "Riley Leonard", "Jaylin Lane", 
  "Arian Smith", "KeAndre Lambert-Smith", "Jimmy Horn Jr.", "Kalel Mullings", 
  "Nick Nash", "Kaden Prather", "Raheim Sanders", "Isaiah Bond", "Luke Lachey", 
  "Kurtis Rourke", "Mitchell Evans", "Phil Mafah", "Jordan Watkins", 
  "Jake Briningstool", "Ja'Corey Brooks", "Donovan Edwards", "Thomas Fidone II", 
  "Marcus Yarns", "Beaux Collins", "Dominic Lovett", "Isaiah Neyor", 
  "LaJohntay Wester", "Theo Wease Jr.", "Antwane Wells Jr.", "Elijhah Badger", 
  "Da'Quan Felton", "Bru McCoy", "Konata Mumpfield", "Jalin Conyers", 
  "Kobe Hudson", "Zakhari Franklin", "Efton Chism III", "Andrew Armstrong", 
  "Ajou Ajou", "Gavin Bartholomew", "Mario Williams", "Bryson Nesbit", 
  "Moose Muhammad III", "Corey Kiner", "Jackson Hawes", "Brady Cook", 
  "Montrell Johnson Jr.", "Graham Mertz", "Benjamin Yurosek", "Joshua Simon", 
  "Junior Bergen", "Caleb Lohner", "Jamaal Pritchett", "Moliki Matavao", 
  "Roc Taylor", "CJ Dippre", "Traeshin Holden", "Jacolby George", 
  "Robbie Ouzts", "Caden Prieskorn", "Seth Henigan", "Kelly Akharaiyi", 
  "Rivaldo Fairweather", "Will Sheppard", "Tommy Mellott", "Josh Kelly", 
  "Mario Anderson", "Payton Thorne", "Ulysses Bentley IV", "Taylor Elgersma", 
  "Gee Scott Jr.", "Jacksin Meeks"
)

# Load fonts
font_add_google("Inter", "Inter")
font_add_google("Fjalla One", "Fjalla One")
showtext_auto()

# Source configurations and utilities
source("R/theme_config.R")
source("R/utils.R")
source("R/data_processing.R")

# Source modules
source("modules/components/comp_week_selector.R")
source("modules/components/comp_team_selector.R")
source("modules/mod_top_performers.R")
source("modules/mod_over_under_performers.R")
source("modules/mod_team_summaries.R")
source("modules/mod_team_detail.R")

# Load data from public Google Sheet - NO FALLBACK
cat("Loading data from public Google Sheet...\n")

# Read the combined_xfp_data from the public Google Sheet
combined_xfp_data <- read_sheet(
  PUBLIC_DATA_SHEET_URL, 
  sheet = "combined_xfp_data",
  col_types = "c"  # Read all as character first to avoid type issues
) %>%
  # Convert to proper types
  mutate(
    week = as.numeric(week),
    season = as.numeric(season),
    xFP = as.numeric(xFP),
    actual_FP = as.numeric(actual_FP),
    target = as.numeric(target),
    air_yards = as.numeric(air_yards),
    rz_target = as.numeric(rz_target),
    receptions = as.numeric(receptions),
    receiving_yards = as.numeric(receiving_yards),
    receiving_touchdowns = as.numeric(receiving_touchdowns),
    carries = as.numeric(carries),
    rush_yards = as.numeric(rush_yards),
    rush_touchdowns = as.numeric(rush_touchdowns),
    goal_line_carry = as.numeric(goal_line_carry),
    adot = as.numeric(adot),
    catch_rate = as.numeric(catch_rate),
    ypt = as.numeric(ypt),
    fp_diff = as.numeric(fp_diff),
    games = as.numeric(games)
  )

cat("Successfully loaded", nrow(combined_xfp_data), "rows from Google Sheets.\n")