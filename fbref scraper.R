library(rvest)
library(dplyr)
library(stringr)
library(janitor)

scrape_fbref_table <- function(league_url, table_id = "stats_shooting", pause_sec = 3) {
  message("Fetching: ", league_url)
  
  page <- read_html(league_url)
  
  comments <- page %>%
    html_nodes(xpath = "//comment()") %>%
    html_text()
  
  tbl_html <- comments[str_detect(comments, table_id)]
  
  if (length(tbl_html) == 0) {
    stop(paste("Could not find", table_id, "table in commented HTML"))
  }
  
  tbl_page <- read_html(tbl_html[1])
  
  raw_tbl <- tbl_page %>%
    html_element(paste0("table#", table_id)) %>%
    html_table(fill = TRUE)
  
  # --- Fix column names ---
  # First row is spanners, second row is actual headers
  header <- raw_tbl[1, ] |> unlist() |> as.character()
  names(raw_tbl) <- header
  
  # Drop the first header row
  tbl <- raw_tbl[-1, ]
  
  # Clean and tidy
  tbl_clean <- tbl %>%
    clean_names() %>%
    filter(!is.na(player), player != "Player") %>%
    mutate(across(where(is.character), str_trim))
  
  Sys.sleep(pause_sec)
  return(tbl_clean)
}

# Example: Premier League 2025-26
url_prem <- "https://fbref.com/en/comps/9/2025-2026/shooting/2025-2026-Premier-League-Stats"
prem_shooting <- scrape_fbref_table(url_prem, "stats_shooting")

names(prem_shooting)
head(prem_shooting, 5)
