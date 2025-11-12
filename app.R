# app.R - Main application file with all styling fixes

# Source global.R explicitly
if(file.exists("global.R")) {
  source("global.R")
  # Source the Over/Under Performers module
  source("modules/mod_over_under_performers.R")
  
  # Load QB data from separate Google Sheet
  QB_DATA_SHEET_URL <- "https://docs.google.com/spreadsheets/d/10MbQBNY1fNJ1pp5VnNXmEtX-TdAsaN_hOE6DpVy-krI/edit?gid=0#gid=0"
  
  qb_data_raw <- tryCatch({
    read_sheet(
      QB_DATA_SHEET_URL,
      col_types = "c"
    ) %>%
      mutate(
        week = as.numeric(week),
        season = as.numeric(season)
      )
  }, error = function(e) {
    stop("ERROR: Could not load QB data from Google Sheets. Error: ", e$message)
  })
} else {
  # If global.R doesn't exist or isn't loading, source everything directly here
  
  # Load required libraries
  library(shiny)
  library(bslib)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(reactable)
  library(htmltools)
  library(shinyjs)
  library(showtext)
  library(base64enc)
  
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
  
  # Load/create data - STOP if files don't exist
  if(file.exists("data/combined_xfp.csv")) {
    combined_xfp_data <- read.csv("data/combined_xfp.csv", stringsAsFactors = FALSE)
  } else {
    stop("ERROR: data/combined_xfp.csv file not found. Cannot start application without data.")
  }
  
  if(file.exists("data/all_players_xfp.csv")) {
    all_players_xfp <- read.csv("data/all_players_xfp.csv", stringsAsFactors = FALSE)
  } else {
    stop("ERROR: data/all_players_xfp.csv file not found. Cannot start application without data.")
  }
  
  # Process data
  combined_xfp_data <- process_xfp_data(combined_xfp_data)
  all_players_xfp <- process_xfp_data(all_players_xfp)
  
  # Load QB data
  QB_DATA_SHEET_URL <- "https://docs.google.com/spreadsheets/d/10MbQBNY1fNJ1pp5VnNXmEtX-TdAsaN_hOE6DpVy-krI/edit?gid=0#gid=0"
  
  qb_data_raw <- tryCatch({
    read_sheet(
      QB_DATA_SHEET_URL,
      col_types = "c"
    ) %>%
      mutate(
        week = as.numeric(week),
        season = as.numeric(season)
      )
  }, error = function(e) {
    stop("ERROR: Could not load QB data from Google Sheets. Error: ", e$message)
  })
}

# UI Definition - Changed from fluidPage to navbarPage
ui <- navbarPage(
  title = div(
    style = "display: flex; align-items: center;",
    tags$img(src = "logos/ETR_ball_logo.png", height = "60px", style = "margin-right: 20px;"),
    span("xFantasy Points", style = "font-size: 2.5rem !important; font-weight: 700 !important;")
  ),
  theme = bs_theme(
    version = 5,
    bg = "#f0f4f1",
    fg = "#111",
    primary = "#37af4a",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    font_scale = 1,
    `enable-rounded` = TRUE,
    `enable-shadows` = TRUE,
    "headings-font-weight" = 900
  ),
  windowTitle = "xFantasy Points",
  
  # Head tags
  header = tagList(
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;900&display=swap"
      ),
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
      ),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"),
      tags$script(HTML(screenshot_js())),
      
      # Complete CSS with all fixes
      tags$style(HTML("
      /* Navbar styling with animated highlighter effect */
      .navbar {
  background: var(--etr-bg-secondary, #f0f5f1) !important;
  border-bottom: 2px solid #e8ede9;
  box-shadow: 0 1px 3px rgba(0,0,0,0.08);
  padding: 1.5rem 1rem !important;
  position: relative;
  display: flex !important;
  align-items: center !important;
  min-height: 90px !important;
}

.container-fluid {
  max-width: 1800px !important;
  margin: 0 auto !important;
  display: flex !important;
  align-items: center !important;
  width: 100% !important;
}

.navbar-brand {
  color: var(--etr-gray-900, #2d2d2d) !important;
  font-weight: 700 !important;
  font-size: 2.5rem !important;
  display: flex !important;
  align-items: center !important;
  margin-right: 2rem !important;
  padding: 0 !important;
}

.navbar-brand img {
  height: 60px !important;
  width: auto !important;
}

.navbar-brand span {
  font-size: 2.5rem !important;
  font-weight: 700 !important;
}

.navbar-nav {
  display: flex !important;
  align-items: center !important;
  margin: 0 !important;
}

.navbar-nav .nav-link {
  color: var(--etr-gray-600, #666) !important;
  font-weight: 500;
  font-size: 2rem !important;
  padding: 0.5rem 0.75rem !important;
  border-radius: 0;
  margin: 0 10px;
  transition: var(--etr-transition-default, var(--transition-default));
  position: relative;
  background: transparent !important;
  border: none !important;
  font-family: var(--etr-font-primary, var(--font-stack-primary));
  display: flex !important;
  align-items: center !important;
  height: 45px !important;
}

.navbar-nav .nav-link:hover {
  color: var(--etr-primary, #37af4a) !important;
  background: transparent !important;
}

.navbar-nav .nav-link.active {
  color: var(--etr-gray-900, #2d2d2d) !important;
  background: transparent !important;
  font-weight: 700 !important;
  position: relative;
  z-index: 2;
}

.navbar-nav .nav-link.active::before {
  content: '';
  position: absolute;
  top: 50%;
  left: 0;
  height: 20px;
  width: 0;
  transform: translateY(-50%) skewX(-3deg);
  background: linear-gradient(90deg, rgba(86, 214, 105, 0.8) 0%, rgba(86, 214, 105, 0.6) 100%);
  z-index: -1;
  animation: highlighterDraw 0.8s ease-out forwards;
}

@keyframes highlighterDraw {
  from { width: 0; }
  to { width: 100%; }
}
      
      body {
        font-family: 'Inter', -apple-system, sans-serif;
        background: #f0f4f1;
      }
      
      .tab-content {
        width: 100% !important;
        max-width: 100% !important;
      }
      
      .tab-pane {
        width: 100% !important;
      }
      
      .main-container, .content-wrapper {
        max-width: 1800px !important;
        width: 100% !important;
        margin: 0 auto !important;
        padding: 30px 20px !important;
        display: block !important;
      }
      
      .container-fluid > .content-wrapper {
        max-width: 1800px !important;
      }
      
      .capture-container {
        background: white;
        border-radius: 12px;
        box-shadow: 0 2px 12px rgba(0,0,0,0.08);
        overflow: hidden;
        margin-bottom: 15px;
        margin-left: auto;
        margin-right: auto;
      }
      
      /* Button styling with larger fonts */
      .btn-etr, .btn {
        background: #37af4a !important;
        color: white !important;
        border: none !important;
        padding: 12px 24px !important;
        border-radius: 6px !important;
        font-weight: 600 !important;
        font-size: 23px !important;
        transition: all 0.2s !important;
        cursor: pointer;
        font-family: 'Inter', sans-serif !important;
        min-height: 48px !important;
      }
      
      .btn-etr:hover, .btn:hover {
        background: #2d9940 !important;
        transform: translateY(-1px) !important;
        box-shadow: 0 4px 12px rgba(55, 175, 74, 0.3) !important;
      }
      
      .btn-sm {
        padding: 8px 16px !important;
        font-size: 22px !important;
        min-height: 42px !important;
      }
      
      /* Toggle button styles for selection buttons - UPDATED */
      .toggle-btn {
        background: #f8f9fa !important;
        color: #495057 !important;
        border: 1px solid #ced4da !important;
        padding: 8px 16px !important;
        border-radius: 6px !important;
        font-weight: 600 !important;
        font-size: 22px !important;
        transition: all 0.15s !important;
        cursor: pointer;
        font-family: 'Inter', sans-serif !important;
        min-height: 42px !important;
        min-width: 100px !important;
        width: 100px !important;
      }
      
      .toggle-btn:hover {
        background: #e9ecef !important;
        transform: translateY(-1px) !important;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1) !important;
      }
      
      .toggle-btn:focus {
        background: white !important;
        color: #333 !important;
        border: 2px solid #37af4a !important;
        box-shadow: 0 0 0 3px rgba(55, 175, 74, 0.15) !important;
        padding: 7px 15px !important;
        outline: none !important;
      }
      
      .toggle-btn.active {
        background: white !important;
        color: #333 !important;
        border: 2px solid #37af4a !important;
        box-shadow: 0 0 0 3px rgba(55, 175, 74, 0.15) !important;
        padding: 7px 15px !important;
        transform: translateY(0px) !important;
      }
      
      .toggle-btn.active:hover {
        box-shadow: 0 0 0 4px rgba(55, 175, 74, 0.25) !important;
        transform: translateY(-1px) !important;
      }
      
      /* Section headers */
      h2 {
        font-size: 2.5rem !important;
        font-weight: 700 !important;
        color: #111 !important;
        margin-bottom: 30px !important;
        font-family: 'Inter', sans-serif !important;
        border-bottom: 3px solid #37af4a !important;
        padding-bottom: 15px !important;
      }
      
      /* Main title only - no border */
      h1 {
        font-size: 2.5rem !important;
        font-weight: 700 !important;
        color: #111 !important;
        margin-bottom: 30px !important;
        font-family: 'Inter', sans-serif !important;
        border: none !important;
        padding-bottom: 15px !important;
      }
      
      /* Fix reactable title spacing - TIGHT */
      #top_performers-table_container h1,
      #over_under_performers-table_container h1,
      #team_summaries-team_table_container h1,
      #team_detail-team_facet_container h1 {
        margin-bottom: 0 !important;
        line-height: 66px !important;
        height: 66px !important;
      }
      
      #top_performers-position_subtitle,
      #over_under_performers-position_subtitle,
      #team_summaries-team_subtitle,
      #team_detail-facet_subtitle {
        margin-top: 0 !important;
        margin-bottom: 10px !important;
      }
      
      .capture-container h1 + * {
        margin-top: 0 !important;
      }
      
      /* Section separator */
      hr.section-separator {
        border: none !important;
        height: 2px !important;
        background: linear-gradient(to right, transparent, #e0e0e0, transparent) !important;
        margin: 60px 0 !important;
      }
      
      /* Input controls with proper width filling */
      .filters-outside {
        max-width: 1750px !important;
        margin-left: auto !important;
        margin-right: auto !important;
        width: 100% !important;
        padding: 0 10px !important;
      }
      
      .filters-outside .form-group {
        margin-bottom: 20px;
      }
      
      .filters-outside .col-sm-4,
      .filters-outside [class*='col-'] {
        padding-left: 8px !important;
        padding-right: 8px !important;
      }
      
      .filters-outside .row {
        margin-left: -8px !important;
        margin-right: -8px !important;
      }
      
      /* Input controls with larger fonts and darker borders */
      .form-control, .selectize-input {
        font-family: 'Inter', sans-serif !important;
        font-size: 23px !important;
        background: white !important;
        border: 1px solid #ced4da !important;
        border-radius: 6px !important;
        padding: 12px 16px !important;
        height: auto !important;
        min-height: 48px !important;
        line-height: 1.4 !important;
        width: 100% !important;
      }
      
      .form-control:focus, .selectize-input.focus {
        border-color: #37af4a !important;
        box-shadow: 0 0 0 0.2rem rgba(55, 175, 74, 0.25) !important;
        outline: none !important;
      }
      
      /* Selectize specific adjustments */
      .selectize-control {
        font-size: 23px !important;
        width: 100% !important;
      }
      
      .selectize-input {
        min-height: 48px !important;
        display: flex !important;
        align-items: center !important;
        width: 100% !important;
      }
      
      .selectize-input > div,
      .selectize-input > input {
        font-size: 23px !important;
        line-height: 1.4 !important;
      }
      
      /* Selectize dropdown items */
      .selectize-dropdown {
        font-size: 23px !important;
        border: 1px solid #ced4da !important;
      }
      
      .selectize-dropdown-content .option {
        font-size: 23px !important;
        padding: 10px 16px !important;
        line-height: 1.4 !important;
      }
      
      /* Label styling with larger fonts */
      label.control-label, .control-label {
        font-size: 27px !important;
        font-weight: 600 !important;
        color: #666 !important;
        text-transform: uppercase !important;
        letter-spacing: 0.5px !important;
        margin-bottom: 12px !important;
        display: block !important;
        font-family: 'Inter', sans-serif !important;
      }
      
      /* Dropdown height enhancement */
      .selectize-dropdown,
      .selectize-dropdown-content {
        max-height: 440px !important;
      }
      
      /* Numeric input styling with larger fonts and darker borders */
      input[type='number'] {
        background: white !important;
        border: 1px solid #ced4da !important;
        border-radius: 6px !important;
        font-family: 'Inter', sans-serif !important;
        font-size: 23px !important;
        padding: 12px 16px !important;
        height: auto !important;
        min-height: 48px !important;
        line-height: 1.4 !important;
        width: 100% !important;
      }
      
      input[type='number']:focus {
        border-color: #37af4a !important;
        box-shadow: 0 0 0 0.2rem rgba(55, 175, 74, 0.25) !important;
        outline: none !important;
      }
      
      /* Container specific styling */
      #top_performers-table_container,
      #over_under_performers-table_container,
      #team_summaries-team_table_container,
      #team_detail-team_facet_container,
      #team_detail-player_chips_container {
        background: white !important;
        border-radius: 12px !important;
        box-shadow: 0 2px 12px rgba(0,0,0,0.08) !important;
        overflow: hidden !important;
        margin-bottom: 15px !important;
      }
      
      /* Placeholder text size */
      ::placeholder {
        font-size: 23px !important;
        opacity: 0.6;
      }
      
      .selectize-input input::placeholder {
        font-size: 23px !important;
        opacity: 0.6;
      }
      
      /* Ensure download buttons maintain consistent size */
      .download-section .btn {
        min-width: 200px !important;
      }
      
      /* Week selector box styles - UPDATED */
      .week-selector-box {
        transition: all 0.15s ease !important;
      }
      
      .week-selector-box:not(.selected) {
        background: #f8f9fa !important;
        color: #6c757d !important;
        border: 1px solid #dee2e6 !important;
        opacity: 0.8 !important;
      }
      
      .week-selector-box.selected {
        background: white !important;
        color: #333 !important;
        border: 2px solid #37af4a !important;
        box-shadow: 0 0 0 3px rgba(55, 175, 74, 0.15) !important;
        opacity: 1 !important;
        margin: 0 1px !important;
      }
      
      .week-selector-box.selected:hover {
        box-shadow: 0 0 0 4px rgba(55, 175, 74, 0.25) !important;
        transform: translateY(-1px) !important;
      }
      
      .week-selector-box:hover {
        transform: translateY(-2px) !important;
        box-shadow: 0 4px 8px rgba(0,0,0,0.15) !important;
        opacity: 1 !important;
      }
    "))
    ),
    
    # Enable shinyjs
    shinyjs::useShinyjs(),
    
    # Custom CSS
    tags$style(HTML(xfp_custom_css()))
  ),
  
  # Tab 1: Top Performers
  tabPanel(
    "Top Performers",
    div(
      class = "content-wrapper",
      mod_top_performers_ui("top_performers")
    )
  ),
  
  # Tab 2: Over/Under Performers
  tabPanel(
    "Over/Under Performers",
    div(
      class = "content-wrapper",
      mod_over_under_performers_ui("over_under_performers")
    )
  ),
  
  # Tab 3: Team Analysis
  tabPanel(
    "Team Analysis",
    div(
      class = "content-wrapper",
      
      # Team Summaries Section
      div(style = "margin-bottom: 60px;",
          mod_team_summaries_ui("team_summaries")
      ),
      
      # Separator
      hr(class = "section-separator"),
      
      # Team Detail Section
      div(style = "margin-bottom: 60px;",
          mod_team_detail_ui("team_detail")
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Create reactive data sources
  xfp_data <- reactive({ combined_xfp_data })
  qb_data <- reactive({ qb_data_raw })
  
  # Initialize modules
  mod_top_performers_server("top_performers", xfp_data, qb_data, rookie_list)
  mod_over_under_performers_server("over_under_performers", xfp_data, qb_data, rookie_list)
  mod_team_summaries_server("team_summaries", xfp_data)
  mod_team_detail_server("team_detail", xfp_data)
}

# Run Application
shinyApp(ui, server)