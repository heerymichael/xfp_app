# modules/mod_team_detail.R - Team Detail module

# UI
mod_team_detail_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Team Detail"),
    p("Select a team to view individual player xFP trends by week.",
      style = "color: #666; font-size: 1.1rem; margin-bottom: 20px;"),
    
    # Input controls row - OUTSIDE container
    div(
      class = "filters-outside",
      style = "margin-bottom: 20px;",
      fluidRow(
        column(4,
               div(class = "form-group",
                   tags$label("TEAM", class = "control-label"),
                   selectInput(ns("team_selector"), NULL,
                               choices = c("Select a team..." = "",
                                           setNames(NFL_TEAMS$all, 
                                                    sapply(NFL_TEAMS$all, get_team_name))),
                               selected = "",
                               width = "100%"))),
        column(4,
               div(class = "form-group",
                   tags$label("POSITION", class = "control-label"),
                   selectInput(ns("position_filter"), NULL,
                               choices = c("All Positions" = "ALL",
                                           "RB" = "RB", 
                                           "WR" = "WR", 
                                           "TE" = "TE"),
                               selected = "ALL",
                               width = "100%"))),
        column(4,
               div(class = "form-group",
                   tags$label("MINIMUM GAMES PLAYED", class = "control-label"),
                   numericInput(ns("min_games_facet"), NULL,
                                value = 3, min = 1, max = 17, step = 1,
                                width = "100%")))
      )
    ),
    
    # Player chips selector
    div(
      id = ns("player_chips_container"),
      class = "capture-container",
      style = "width: 1040px; margin: 20px auto 30px auto; padding: 50px; background: white; border-radius: 12px; box-shadow: 0 2px 12px rgba(0,0,0,0.08);",
      
      # Control buttons - using flexbox for left/right alignment
      div(
        style = "display: flex; justify-content: space-between; margin-bottom: 40px;",
        
        # Left side - Player selection
        div(
          style = "flex: 1;",
          tags$label("SELECT PLAYERS", 
                     class = "control-label",
                     style = paste0(
                       "display: block; ",
                       "margin-bottom: 10px;"
                     )),
          div(
            style = "display: flex; gap: 8px; flex-wrap: wrap;",
            actionButton(ns("select_all_players"), "All", 
                         class = "toggle-btn"),
            actionButton(ns("select_none_players"), "None", 
                         class = "toggle-btn"),
            actionButton(ns("select_top6_players"), "Top 6", 
                         class = "toggle-btn")
          )
        ),
        
        # Right side - Week selection
        div(
          style = "text-align: right;",
          tags$label("SELECT WEEKS", 
                     class = "control-label",
                     style = paste0(
                       "display: block; ",
                       "margin-bottom: 10px; ",
                       "text-align: right;"
                     )),
          div(
            style = "display: flex; gap: 8px; flex-wrap: wrap; justify-content: flex-end;",
            actionButton(ns("select_all_weeks"), "All Weeks", 
                         class = "toggle-btn"),
            actionButton(ns("select_last_4_weeks"), "Last 4", 
                         class = "toggle-btn"),
            actionButton(ns("select_last_8_weeks"), "Last 8", 
                         class = "toggle-btn")
          )
        )
      ),
      
      # Player chips will be rendered here
      uiOutput(ns("player_chips"))
    ),
    
    # Facet chart container
    div(
      id = ns("team_facet_container"),
      class = "capture-container",
      style = paste0(
        "width: 1040px; ",
        "margin: 30px auto 0 auto; ",
        "background: white; ",
        "padding: 50px; ",
        "border-radius: 12px; ",
        "box-shadow: 0 2px 12px rgba(0,0,0,0.08); ",
        "position: relative; ",
        "overflow: hidden;"
      ),
      
      # Conditional content based on selection
      conditionalPanel(
        condition = paste0("input['", ns("team_selector"), "'] == ''"),
        div(
          style = paste0(
            "text-align: center; ",
            "padding: 80px 20px; ",
            "color: #666;"
          ),
          tags$i(class = "fas fa-football-ball", 
                 style = paste0("font-size: 48px; color: #999; margin-bottom: 20px; display: block;")),
          h3("Select a Team", 
             style = paste0("color: #333; margin-bottom: 10px; border: none !important; padding: 0 !important;")),
          p("Choose a team from the selector above to view player performance trends", 
            style = "font-size: 1.1rem;")
        )
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("team_selector"), "'] != ''"),
        # Content
        div(
          style = "position: relative; z-index: 2;",
          # Title and logo
          div(
            style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 4px;",
            h1(textOutput(ns("facet_title")),
               style = "margin: 0; font-size: 2.8rem; line-height: 66px; height: 66px; display: flex; align-items: center; border: none !important; padding: 0 !important;"),
            tags$img(
              src = "logos/ETR_ball_logo.png",
              style = "height: 66px; width: auto;",
              alt = "ETR Logo"
            )
          ),
          
          # Subtitle
          uiOutput(ns("facet_subtitle")),
          
          # Plot
          div(style = "margin-top: 15px;",
              uiOutput(ns("team_facet_plot"))
          )
        )
      )
    ),
    
    # Download button
    conditionalPanel(
      condition = paste0("input['", ns("team_selector"), "'] != ''"),
      div(
        class = "download-section",
        style = "text-align: center; margin-top: 10px; margin-bottom: 30px;",
        actionButton(ns("download_team_facet"), "Download Team Chart",
                     class = "btn-etr",
                     onclick = paste0("downloadTableImage('", ns(""), "', 'team_facet_container')"))
      )
    )
  )
}

# Server
mod_team_detail_server <- function(id, combined_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive for selected team
    selected_team <- reactive({
      if (is.null(input$team_selector) || input$team_selector == "") {
        character(0)
      } else {
        input$team_selector
      }
    })
    
    # Reactive values
    included_players <- reactiveVal(character(0))
    selected_weeks_facet <- reactiveVal(1:17)
    
    # Output for conditional panel
    output$has_team_selected <- reactive({
      length(selected_team()) > 0
    })
    outputOptions(output, "has_team_selected", suspendWhenHidden = FALSE)
    
    # Update included players when team or position filter changes
    observeEvent(list(selected_team(), input$position_filter, input$min_games_facet), {
      team <- selected_team()
      
      if (length(team) > 0 && !is.null(combined_data)) {
        # Get all team players
        all_players <- get_team_players(
          combined_data(),
          team,
          input$min_games_facet %||% 3
        )
        
        # Filter by position if not "ALL"
        if (input$position_filter != "ALL") {
          all_players <- all_players %>%
            filter(position == input$position_filter)
        }
        
        if (nrow(all_players) > 0) {
          # Keep only currently included players that match the filter
          current_included <- included_players()
          filtered_included <- intersect(current_included, all_players$player)
          
          # If no players remain selected after filtering, select top 6
          if (length(filtered_included) == 0) {
            filtered_included <- all_players %>%
              slice_head(n = 6) %>%
              pull(player)
          }
          
          included_players(filtered_included)
        } else {
          included_players(character(0))
        }
      } else {
        included_players(character(0))
      }
    }, ignoreNULL = FALSE)
    
    # Handle player selection buttons
    observeEvent(input$select_all_players, {
      team <- selected_team()
      if (length(team) > 0 && !is.null(combined_data)) {
        players <- get_team_players(
          combined_data(),
          team,
          input$min_games_facet %||% 3
        )
        
        # Filter by position if not "ALL"
        if (input$position_filter != "ALL") {
          players <- players %>%
            filter(position == input$position_filter)
        }
        
        included_players(players$player)
      }
    })
    
    observeEvent(input$select_none_players, {
      included_players(character(0))
    })
    
    observeEvent(input$select_top6_players, {
      team <- selected_team()
      if (length(team) > 0 && !is.null(combined_data)) {
        players <- get_team_players(
          combined_data(),
          team,
          input$min_games_facet %||% 3
        )
        
        # Filter by position if not "ALL"
        if (input$position_filter != "ALL") {
          players <- players %>%
            filter(position == input$position_filter)
        }
        
        players <- players %>%
          slice_head(n = 6)
        included_players(players$player)
      }
    })
    
    # Handle week selection buttons
    observeEvent(input$select_all_weeks, {
      if (!is.null(combined_data)) {
        all_weeks <- sort(unique(combined_data()$week))
        selected_weeks_facet(all_weeks)
      }
    })
    
    observeEvent(input$select_last_4_weeks, {
      if (!is.null(combined_data)) {
        all_weeks <- sort(unique(combined_data()$week))
        last_four <- tail(all_weeks, 4)
        selected_weeks_facet(last_four)
      }
    })
    
    observeEvent(input$select_last_8_weeks, {
      if (!is.null(combined_data)) {
        all_weeks <- sort(unique(combined_data()$week))
        last_eight <- tail(all_weeks, 8)
        selected_weeks_facet(last_eight)
      }
    })
    
    # Handle chip clicks
    observeEvent(input$player_chip_click, {
      clicked_player <- input$player_chip_click
      current <- included_players()
      
      if (clicked_player %in% current) {
        included_players(setdiff(current, clicked_player))
      } else {
        included_players(c(current, clicked_player))
      }
    })
    
    # Render player chips
    output$player_chips <- renderUI({
      team <- selected_team()
      included <- included_players()
      
      if (length(team) == 0) {
        return(tags$div(
          "Please select a team above to see player chips",
          style = "color: #666; padding: 20px; text-align: center; font-style: italic;"
        ))
      }
      
      if (is.null(combined_data)) {
        return(tags$div(
          "Player data not available",
          style = "color: #666; padding: 20px; text-align: center;"
        ))
      }
      
      players <- get_team_players(
        combined_data(),
        team,
        input$min_games_facet %||% 3
      )
      
      # Filter by position if not "ALL"
      if (input$position_filter != "ALL") {
        players <- players %>%
          filter(position == input$position_filter)
      }
      
      players <- players %>%
        slice_head(n = 12)
      
      if (nrow(players) == 0) {
        return(tags$div(
          paste0("No ", ifelse(input$position_filter == "ALL", "players", input$position_filter), 
                 " meet minimum games requirement"),
          style = "color: #666; padding: 20px; text-align: center;"
        ))
      }
      
      chip_list <- lapply(1:nrow(players), function(i) {
        player <- players[i, ]
        is_selected <- player$player %in% included
        create_player_chip_fixed(player, is_selected, ns)
      })
      
      tags$div(
        style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 16px; max-width: 940px; margin: 0 auto;",
        chip_list
      )
    })
    
    # Dynamic facet title
    output$facet_title <- renderText({
      team <- selected_team()
      if (length(team) > 0) {
        team_name <- get_team_name(team)
        paste0(team_name, " xFP")
      } else {
        "Team xFP"
      }
    })
    
    # Dynamic facet subtitle
    output$facet_subtitle <- renderUI({
      team <- selected_team()
      included <- included_players()
      
      if (length(team) == 0) {
        return(p("Select a team to view individual player performance by week.",
                 style = paste0(
                   "margin: 0; ",
                   "color: ", etr_colors$gray_600, "; ",
                   "font-size: 1.8rem; ",
                   "font-family: 'Inter', sans-serif; ",
                   "font-weight: 500;"
                 )))
      }
      
      # Get all available players to check if all are selected
      all_available <- get_team_players(
        combined_data(),
        team,
        input$min_games_facet %||% 3
      )
      
      # Filter by position if not "ALL"
      if (input$position_filter != "ALL") {
        all_available <- all_available %>%
          filter(position == input$position_filter)
      }
      
      # Check if all available players are selected
      all_selected <- length(included) == nrow(all_available) && length(included) > 0
      
      pos_text <- if(input$position_filter == "ALL") "" else paste0("<b>", input$position_filter, "</b> ")
      
      # Handle singular vs plural for games
      min_games <- input$min_games_facet %||% 3
      games_text <- if(min_games == 1) {
        "<b>at least one game</b>"
      } else {
        paste0("<b>at least ", min_games, " games</b>")
      }
      
      # Add week range text - SIMPLIFIED
      weeks <- selected_weeks_facet()
      all_data_weeks <- if(!is.null(combined_data())) sort(unique(combined_data()$week)) else 1:17
      
      # Determine if we should add week text
      add_week_text <- TRUE
      week_suffix <- ""
      
      if(length(weeks) == length(all_data_weeks)) {
        # All weeks selected - don't add week text
        add_week_text <- FALSE
      } else if(length(weeks) == 4 && all(weeks == tail(all_data_weeks, 4))) {
        week_suffix <- " over the <b>last 4 weeks</b>"
      } else if(length(weeks) == 8 && all(weeks == tail(all_data_weeks, 8))) {
        week_suffix <- " over the <b>last 8 weeks</b>"
      } else if(length(weeks) == 1) {
        week_suffix <- paste0(" in <b>week ", weeks, "</b>")
      } else {
        week_suffix <- paste0(" for <b>weeks ", paste(range(weeks), collapse = "-"), "</b>")
      }
      
      if (all_selected) {
        subtitle_text <- HTML(paste0("Showing expected and actual fantasy points for ", 
                                     pos_text, "players who have played ", games_text,
                                     week_suffix, "."))
      } else {
        subtitle_text <- HTML(paste0("Showing expected and actual fantasy points for a selection of ", 
                                     pos_text, "players who have played ", games_text,
                                     week_suffix, "."))
      }
      
      p(subtitle_text,
        style = paste0(
          "margin: 0; ",
          "color: ", etr_colors$gray_600, "; ",
          "font-size: 1.8rem; ",
          "font-family: 'Inter', sans-serif; ",
          "font-weight: 500;"
        ))
    })
    
    # Render facet plot
    output$team_facet_plot <- renderUI({
      team <- selected_team()
      included <- included_players()
      
      if (length(team) == 0) {
        return(NULL)
      }
      
      if (is.null(combined_data)) {
        return(create_placeholder("Player data not available"))
      }
      
      if (length(included) == 0) {
        return(create_placeholder("No players selected - use the buttons above to select players"))
      }
      
      team_name <- get_team_name(team)
      
      # Filter by selected weeks
      plot_data <- prepare_team_facet_data(
        combined_data(),
        team,
        included,
        input$min_games_facet %||% 3
      ) %>%
        filter(week %in% selected_weeks_facet())
      
      if (is.null(plot_data) || nrow(plot_data) == 0) {
        return(create_placeholder(paste("No data available for", team_name)))
      }
      
      # Create the facet plot with real data
      p <- create_team_facet_plot_bars(plot_data, team_name, input$min_games_facet %||% 3)
      
      if (is.null(p)) {
        return(create_placeholder("Unable to generate plot"))
      }
      
      # Get the dynamic height from the plot attribute
      plot_height <- attr(p, "plot_height") %||% 8
      
      base64_data <- plot_to_base64(p, width = 11.4, height = plot_height)
      
      tags$img(
        src = paste0("data:image/png;base64,", base64_data),
        style = "width: 100%; height: auto; display: block;",
        alt = "Team xFP Facet Chart"
      )
    })
    
  })
}

# Helper functions
create_button_style <- function() {
  paste0(
    "background: #f0f0f0; ",
    "color: #333; ",
    "border: 1px solid #999; ",
    "padding: 4px 12px; ",
    "font-weight: 600;"
  )
}

# Fixed player chip function with proper sizing
create_player_chip_fixed <- function(player, is_selected, ns) {
  tags$div(
    class = paste("player-chip", if(is_selected) "selected" else "deselected"),
    onclick = paste0("Shiny.setInputValue('", ns("player_chip_click"), "', '", 
                     gsub("'", "\\\\'", player$player), "', {priority: 'event'})"),
    style = "width: 100%;",  # Take full width of grid cell
    tags$span(class = "player-name", player$player)
  )
}

# Keep old function for compatibility
create_player_chip <- function(player, is_selected, ns) {
  create_player_chip_fixed(player, is_selected, ns)
}

create_placeholder <- function(message) {
  div(
    message,
    style = paste0(
      "text-align: center; ",
      "padding: 40px; ",
      "color: ", etr_colors$gray_600, "; ",
      "font-size: 1.2rem;"
    )
  )
}

# Bar chart facet plot function
create_team_facet_plot_bars <- function(plot_data, team_name, min_games) {
  
  num_players <- length(unique(plot_data$player))
  y_max <- 38
  y_min <- 0
  
  # Calculate plot height based on number of rows (3 facets per row)
  num_rows <- ceiling(num_players / 3)
  plot_height <- 6 + (num_rows * 2.5)  # Base height + additional per row
  
  # Prepare data - xFP (expected) and actual_FP (actual)
  plot_data_long <- plot_data %>%
    mutate(
      expected = xFP,
      actual = actual_FP
    ) %>%
    select(player, position, week, expected, actual) %>%
    pivot_longer(
      cols = c(expected, actual),
      names_to = "type",
      values_to = "points"
    ) %>%
    mutate(
      type = factor(type, levels = c("expected", "actual"))
    )
  
  p <- ggplot(plot_data_long, aes(x = week, y = points)) +
    # Plot actual bars first (behind)
    geom_col(data = filter(plot_data_long, type == "actual"),
             aes(fill = type),
             width = 0.5,
             alpha = 1) +
    # Plot expected bars on top
    geom_col(data = filter(plot_data_long, type == "expected"),
             aes(color = type, fill = type),
             width = 0.5,
             size = 1.5) +
    # Horizontal line at 0
    geom_hline(yintercept = 0, color = "#333333", size = 1.5, linetype = 1) +
    facet_wrap(~ player, ncol = 3, scales = "fixed") +  # Changed from 4 to 3
    scale_x_continuous(breaks = 1:17) +
    scale_y_continuous(
      breaks = seq(0, y_max, by = 10),
      limits = c(y_min, y_max),
      labels = function(x) round(x, 0)
    ) +
    scale_fill_manual(
      values = c("expected" = "transparent", "actual" = etr_colors$primary),
      labels = c("expected" = "Expected Points", "actual" = "Actual Points"),
      name = ""
    ) +
    scale_color_manual(
      values = c("expected" = etr_colors$gray_800, "actual" = "transparent"),
      labels = c("expected" = "Expected Points", "actual" = "Actual Points"),
      name = "",
      guide = "none"
    ) +
    labs(
      title = NULL,
      subtitle = NULL,
      x = "Week",
      y = "Fantasy Points"
    ) +
    theme_etr() +
    theme(
      strip.text = element_text(
        size = 18,  # Increased from 12 (50% larger)
        face = "bold",
        margin = margin(b = 10)
      ),
      strip.background = element_blank(),
      panel.spacing.y = unit(2, "lines"),  # Increased vertical spacing
      panel.spacing.x = unit(5, "lines"),  # Increased horizontal spacing
      axis.text = element_text(size = 10),
      axis.text.x = element_text(size = 8),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.text = element_text(size = 12),
      legend.margin = margin(b = 10),
      legend.box.margin = margin(0, 0, 10, 0),
      panel.grid.major.y = element_line(color = etr_colors$gray_200, size = 0.3),
      panel.grid.major.x = element_blank()
    ) +
    guides(
      fill = guide_legend(
        override.aes = list(
          color = c(etr_colors$gray_800, NA),
          size = c(1.5, 0)
        )
      )
    )
  
  # Store the calculated height as an attribute
  attr(p, "plot_height") <- plot_height
  
  return(p)
}