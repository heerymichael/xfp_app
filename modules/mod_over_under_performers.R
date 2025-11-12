# modules/mod_over_under_performers.R - Over/Underperformers module

# UI
mod_over_under_performers_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Over & Underperformers"),
    p("Analyze players with the biggest differences between expected and actual fantasy points performance.",
      style = "color: #666; font-size: 1.1rem; margin-bottom: 20px;"),
    
    # Custom CSS for toggle switch
    tags$style(HTML(paste0("
      #", ns("rookies_switch_container"), " .toggle-switch {
        position: relative;
        display: inline-block;
        width: 80px;
        height: 40px;
        cursor: pointer;
        vertical-align: middle;
      }
      
      #", ns("rookies_switch_container"), " .toggle-switch input[type='checkbox'] {
        display: none;
      }
      
      #", ns("rookies_switch_container"), " .toggle-switch-background {
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: #ddd;
        border-radius: 20px;
        box-shadow: inset 0 0 0 2px #ccc;
        transition: background-color 0.3s ease-in-out;
      }
      
      #", ns("rookies_switch_container"), " .toggle-switch-handle {
        position: absolute;
        top: 5px;
        left: 5px;
        width: 30px;
        height: 30px;
        background-color: #fff;
        border-radius: 50%;
        box-shadow: 0 2px 5px rgba(0, 0, 0, 0.2);
        transition: transform 0.3s ease-in-out;
      }
      
      #", ns("rookies_switch_container"), " .toggle-switch input[type='checkbox']:checked ~ .toggle-switch-background {
        background-color: #37af4a;
        box-shadow: inset 0 0 0 2px #2d9940;
      }
      
      #", ns("rookies_switch_container"), " .toggle-switch input[type='checkbox']:checked ~ .toggle-switch-background .toggle-switch-handle {
        transform: translateX(40px);
      }
      
      #", ns("rookies_switch_container"), " .toggle-label-text {
        display: inline-block;
        margin-left: 15px;
        font-size: 23px;
        font-weight: 600;
        color: #333;
        vertical-align: middle;
        line-height: 40px;
      }
      
      #", ns("rookies_switch_container"), " {
        display: inline-flex;
        align-items: center;
        padding: 12px 20px;
        background: white;
        border: 1px solid #ced4da;
        border-radius: 6px;
        margin-top: 20px;
      }
      
      .qb-filter-container {
        background: #f0f4f1;
        border: 1px solid #ced4da;
        border-radius: 6px;
        padding: 20px;
        margin-top: 20px;
        position: relative;
        padding-top: 35px;
      }
      
      .qb-filter-label {
        position: absolute;
        top: -12px;
        left: 15px;
        background: #f0f4f1;
        padding: 0 8px;
        font-size: 19px !important;
        font-weight: 600 !important;
        color: #999 !important;
        text-transform: uppercase !important;
        letter-spacing: 0.5px !important;
        font-family: 'Inter', sans-serif !important;
        line-height: 1;
      }
      
      .qb-filter-container .form-control,
      .qb-filter-container .selectize-input {
        background: white !important;
      }
    "))),
    
    # Input controls - Filters OUTSIDE container
    div(
      class = "filters-outside",
      style = paste0("margin-bottom: 30px;"),
      
      # First row - Position and View Mode
      fluidRow(
        column(6, 
               div(class = "form-group",
                   tags$label("POSITION", class = "control-label"),
                   selectInput(ns("position"), NULL,
                               choices = c("All Positions" = "ALL",
                                           "Running Back" = "RB", 
                                           "Wide Receiver" = "WR",
                                           "Wide Receiver & Tight End" = "WR_TE",
                                           "Tight End" = "TE"),
                               selected = "ALL",
                               width = "100%"))),
        column(6,
               div(class = "form-group",
                   tags$label("VIEW MODE", class = "control-label"),
                   selectInput(ns("view_mode"), NULL,
                               choices = c("Biggest Overperformers" = "overperformers",
                                           "Biggest Underperformers" = "underperformers"),
                               selected = "overperformers",
                               width = "100%")))
      ),
      
      # Second row - Number of Players, Minimum Games, and Minimum xFP
      fluidRow(
        column(4,
               div(class = "form-group",
                   tags$label("NUMBER OF PLAYERS", class = "control-label"),
                   numericInput(ns("num_players"), NULL,
                                value = 12, min = 1, max = 50, step = 1,
                                width = "100%"))),
        column(4,
               div(class = "form-group",
                   tags$label("MINIMUM GAMES", class = "control-label"),
                   numericInput(ns("min_games"), NULL,
                                value = 1, min = 1, max = 18, step = 1,
                                width = "100%"))),
        column(4,
               div(class = "form-group",
                   tags$label("MINIMUM xFP PER GAME", class = "control-label"),
                   numericInput(ns("min_xfp"), NULL,
                                value = 0, min = 0, max = 30, step = 0.5,
                                width = "100%")))
      ),
      
      # Third row - QB filters
      div(
        class = "qb-filter-container",
        tags$label("SPECIFY QB WEEKS", class = "qb-filter-label"),
        fluidRow(
          column(5,
                 div(class = "form-group",
                     tags$label("TEAM", class = "control-label"),
                     selectInput(ns("qb_team"), NULL,
                                 choices = c("Select a team..." = "",
                                             setNames(NFL_TEAMS$all, 
                                                      sapply(NFL_TEAMS$all, get_team_name))),
                                 selected = "",
                                 width = "100%"))),
          column(5,
                 div(class = "form-group",
                     tags$label("QUARTERBACK", class = "control-label"),
                     selectInput(ns("qb_selector"), NULL,
                                 choices = c("All Quarterbacks" = "ALL"),
                                 selected = "ALL",
                                 width = "100%"))),
          column(2,
                 div(class = "form-group",
                     tags$label("", class = "control-label", style = "display: block; height: 39px;"),
                     actionButton(ns("clear_qb"), "Clear",
                                  class = "btn-sm",
                                  style = "width: 100%; margin-top: 0;")))
        )
      ),
      
      # Week selector
      comp_week_selector_ui(ns("week_selector")),
      
      # Rookies toggle switch
      div(
        style = "text-align: center; margin-top: 20px;",
        div(
          id = ns("rookies_switch_container"),
          tags$label(
            class = "toggle-switch",
            tags$input(type = "checkbox", id = ns("rookies_only")),
            tags$div(
              class = "toggle-switch-background",
              tags$div(class = "toggle-switch-handle")
            )
          ),
          tags$span(class = "toggle-label-text", "ROOKIES ONLY")
        )
      )
    ),
    
    # Table Container
    div(
      id = ns("table_container"),
      class = "capture-container",
      style = paste0(
        "width: 1040px; ",
        "margin: 0 auto; ",
        "background: white; ",
        "padding: 50px; ",
        "border-radius: 12px; ",
        "box-shadow: 0 2px 12px rgba(0,0,0,0.08);"
      ),
      
      # Title and logo
      div(
        style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 4px;",
        h1(textOutput(ns("table_title")),
           style = "margin: 0; font-size: 2.8rem; line-height: 66px; height: 66px; display: flex; align-items: center; border: none !important; padding: 0 !important;"),
        tags$img(
          src = "logos/ETR_ball_logo.png",
          style = "height: 66px; width: auto;",
          alt = "ETR Logo"
        )
      ),
      
      # Subtitle
      uiOutput(ns("position_subtitle")),
      
      # Table
      div(style = "margin-top: 15px;",
          reactableOutput(ns("xfp_table"))
      )
    ),
    
    # Download button
    div(
      class = "download-section",
      style = "text-align: center; margin-top: 10px; margin-bottom: 30px;",
      actionButton(ns("download_table"), "Download Table Image",
                   style = "background: #37af4a; color: white; border: none; padding: 10px 20px; border-radius: 6px; font-weight: 600; font-size: 14px; min-width: 150px;",
                   onclick = paste0("downloadTableImage('", ns(""), "', 'table_container')"))
    )
  )
}

# Server
mod_over_under_performers_server <- function(id, data, qb_data, rookie_list = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Clear QB filter button
    observeEvent(input$clear_qb, {
      updateSelectInput(session, "qb_team", selected = "")
      updateSelectInput(session, "qb_selector", selected = "ALL")
      
      # Reset week selector to all weeks
      all_weeks <- sort(unique(data()$week))
      lapply(all_weeks, function(week) {
        shinyjs::runjs(sprintf("$('#over_under_performers-week_selector-week_box_%s').addClass('selected');", week))
      })
    })
    
    # Initialize QB selector
    observe({
      req(qb_data())
      
      all_qbs <- qb_data() %>%
        filter(season == 2025) %>%
        pull(team_qb) %>%
        unique() %>%
        sort()
      
      qb_choices <- c("All Quarterbacks" = "ALL",
                      setNames(all_qbs, all_qbs))
      
      updateSelectInput(session, "qb_selector",
                        choices = qb_choices,
                        selected = "ALL")
    })
    
    # Update QB selector when team is selected
    observeEvent(input$qb_team, {
      req(qb_data())
      
      if (is.null(input$qb_team) || input$qb_team == "") {
        all_qbs <- qb_data() %>%
          filter(season == 2025) %>%
          pull(team_qb) %>%
          unique() %>%
          sort()
        
        qb_choices <- c("All Quarterbacks" = "ALL",
                        setNames(all_qbs, all_qbs))
        
        updateSelectInput(session, "qb_selector",
                          choices = qb_choices,
                          selected = "ALL")
      } else {
        team_qbs <- qb_data() %>%
          filter(
            season == 2025,
            posteam == input$qb_team
          ) %>%
          pull(team_qb) %>%
          unique() %>%
          sort()
        
        qb_choices <- c("All Quarterbacks" = "ALL",
                        setNames(team_qbs, team_qbs))
        
        updateSelectInput(session, "qb_selector",
                          choices = qb_choices,
                          selected = "ALL")
      }
    })
    
    # Reactive for QB-filtered weeks
    qb_filtered_weeks <- reactive({
      req(qb_data())
      
      if (is.null(input$qb_selector) || input$qb_selector == "ALL") {
        return(NULL)
      }
      
      if (!is.null(input$qb_team) && input$qb_team != "") {
        qb_weeks <- qb_data() %>%
          filter(
            season == 2025,
            posteam == input$qb_team,
            team_qb == input$qb_selector
          ) %>%
          pull(week) %>%
          unique() %>%
          sort()
      } else {
        qb_weeks <- qb_data() %>%
          filter(
            season == 2025,
            team_qb == input$qb_selector
          ) %>%
          pull(week) %>%
          unique() %>%
          sort()
      }
      
      return(qb_weeks)
    })
    
    # Initialize week selector
    selected_weeks <- comp_week_selector_server("week_selector", data)
    
    # Auto-update week selector when QB filter changes
    observeEvent(qb_filtered_weeks(), {
      qb_weeks <- qb_filtered_weeks()
      all_weeks <- sort(unique(data()$week))
      
      if (!is.null(qb_weeks) && length(qb_weeks) > 0) {
        shinyjs::runjs(sprintf("
          var qbWeeks = [%s];
          var allWeeks = [%s];
          
          allWeeks.forEach(function(week) {
            var element = $('#over_under_performers-week_selector-week_box_' + week);
            if (qbWeeks.includes(week)) {
              element.addClass('selected');
            } else {
              element.removeClass('selected');
            }
          });
          
          Shiny.setInputValue('over_under_performers-week_selector-weeks', qbWeeks, {priority: 'event'});
        ", 
                               paste(qb_weeks, collapse = ","),
                               paste(all_weeks, collapse = ",")))
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Reactive data with QB and week filtering
    filtered_data_final <- reactive({
      base_data <- data()
      qb_weeks <- qb_filtered_weeks()
      user_weeks <- selected_weeks()
      
      if (!is.null(qb_weeks) && length(qb_weeks) > 0) {
        final_weeks <- intersect(qb_weeks, user_weeks)
        if (length(final_weeks) > 0) {
          base_data <- base_data %>%
            filter(week %in% final_weeks)
        } else {
          base_data <- base_data %>%
            filter(week %in% qb_weeks)
        }
      } else {
        base_data <- base_data %>%
          filter(week %in% user_weeks)
      }
      
      return(base_data)
    })
    
    # Dynamic title
    output$table_title <- renderText({
      view_mode <- input$view_mode
      rookies <- isTRUE(input$rookies_only)
      
      prefix <- if (rookies) "Rookie " else ""
      
      suffix <- switch(view_mode,
                       "overperformers" = "Overperformers",
                       "underperformers" = "Underperformers",
                       "Performers")
      
      if (input$position == "ALL") {
        paste0(prefix, "Overall ", suffix)
      } else if (input$position == "WR_TE") {
        paste0(prefix, "Wide Receiver & Tight End ", suffix)
      } else {
        pos_name <- switch(input$position,
                           "RB" = "Running Back",
                           "WR" = "Wide Receiver",
                           "TE" = "Tight End")
        paste0(prefix, pos_name, " ", suffix)
      }
    })
    
    # Dynamic subtitle
    output$position_subtitle <- renderUI({
      all_weeks <- sort(unique(data()$week))
      view_mode <- input$view_mode
      
      qb_filter_active <- !is.null(input$qb_team) && input$qb_team != "" && 
        !is.null(input$qb_selector) && input$qb_selector != "ALL"
      
      pos_name <- if (isTRUE(input$rookies_only) && input$position == "ALL") {
        "all rookies"
      } else {
        prefix <- if (isTRUE(input$rookies_only)) "rookie " else ""
        switch(input$position,
               "ALL" = paste0(prefix, "all players"),
               "RB" = paste0(prefix, "running backs"),
               "WR" = paste0(prefix, "wide receivers"), 
               "TE" = paste0(prefix, "tight ends"),
               "WR_TE" = paste0(prefix, "wide receivers and tight ends"))
      }
      
      metric_description <- switch(view_mode,
                                   "overperformers" = "players with the <b>largest positive difference</b> between actual and expected fantasy points",
                                   "underperformers" = "players with the <b>largest negative difference</b> between actual and expected fantasy points")
      
      # Add minimum xFP filter to subtitle if > 0
      min_xfp_text <- ""
      if (!is.null(input$min_xfp) && input$min_xfp > 0) {
        min_xfp_text <- paste0(" with at least <b>", input$min_xfp, " xFP per game</b>")
      }
      
      if (qb_filter_active) {
        team_name <- get_team_name(input$qb_team)
        base_text <- HTML(paste0("Table shows ", metric_description, " for <b>", 
                                 pos_name, "</b>", min_xfp_text, " during weeks where <b>", 
                                 input$qb_selector, "</b> was starting quarterback for the <b>", team_name, "</b>."))
      } else {
        week_text <- get_week_range_text(selected_weeks(), all_weeks)
        base_text <- HTML(paste0("Table shows ", metric_description, " for <b>", 
                                 pos_name, "</b>", min_xfp_text, " during <b>", week_text, "</b>."))
      }
      
      p(base_text,
        style = paste0(
          "margin: 0; ",
          "color: ", etr_colors$gray_600, "; ",
          "font-size: 1.8rem; ",
          "font-family: 'Inter', sans-serif; ",
          "font-weight: 500;"
        ))
    })
    
    # Prepare table data
    table_data <- reactive({
      req(filtered_data_final(), input$position, input$min_games)
      req(selected_weeks())
      
      num_players_val <- if (!is.null(input$num_players)) {
        val <- as.numeric(input$num_players)
        if (is.na(val) || val < 1) 12 else val
      } else {
        12
      }
      
      rookies_selected <- isTRUE(input$rookies_only)
      view_mode <- input$view_mode
      min_xfp_val <- input$min_xfp %||% 0
      
      if (rookies_selected) {
        if (is.null(rookie_list) || length(rookie_list) == 0) {
          showNotification("Rookie list not available", type = "warning")
          return(data.frame())
        }
      }
      
      prepare_top_performers_data(
        filtered_data_final(),
        input$position,
        selected_weeks(),
        input$min_games,
        num_players_val,
        rookies_only = rookies_selected,
        rookie_list = rookie_list,
        view_mode = view_mode,
        min_xfp = min_xfp_val
      )
    })
    
    # Create reactable
    output$xfp_table <- renderReactable({
      df <- table_data()
      
      if (nrow(df) == 0) {
        message_text <- if (isTRUE(input$rookies_only)) {
          if (input$position != "ALL") {
            pos_display <- switch(input$position,
                                  "RB" = "running back",
                                  "WR" = "wide receiver",
                                  "TE" = "tight end",
                                  "WR_TE" = "wide receiver or tight end")
            paste0("No rookie ", pos_display, 
                   "s meet the criteria (", input$min_games, "+ games, ", 
                   input$min_xfp, "+ xFP per game)")
          } else {
            paste0("No rookies meet the criteria (", input$min_games,
                   "+ games, ", input$min_xfp, "+ xFP per game)")
          }
        } else {
          paste0("No players meet the criteria (", input$min_games,
                 "+ games, ", input$min_xfp, "+ xFP per game)")
        }
        return(create_empty_table(message_text))
      }
      
      max_expected <- max(df$expected_points, na.rm = TRUE)
      max_actual <- max(df$actual_points, na.rm = TRUE)
      max_val <- max(c(max_expected, max_actual), na.rm = TRUE) * 1.1
      
      if (max_val <= 0 || is.na(max_val) || is.infinite(max_val)) {
        max_val <- 25
      }
      
      num_players_val <- if (!is.null(input$num_players)) {
        val <- as.numeric(input$num_players)
        if (is.na(val) || val < 1) 12 else val
      } else {
        12
      }
      
      reactable(
        df,
        theme = etr_reactable(
          font_size = 17.6,
          header_font_size = 15,
          cell_padding = 0,
          centered = FALSE
        ),
        columnGroups = list(
          colGroup(
            name = "FP PER GAME",
            columns = c("expected_points", "actual_points")
          )
        ),
        columns = list(
          player = colDef(
            name = "PLAYER",
            width = 250,
            style = list(borderRight = paste0(etr_borders$width_medium, " solid ", etr_colors$gray_500)),
            cell = function(value, index) {
              create_player_cell(value, df$team[index], df$team_name[index])
            }
          ),
          games_played = colDef(
            name = "GAMES",
            width = 100,
            align = "center",
            cell = function(value) {
              div(value, class = "stats-value")
            }
          ),
          expected_points = colDef(
            name = "EXPECTED",
            width = 100,
            align = "center",
            cell = function(value) {
              div(format(round(value, 1), nsmall = 1), class = "stats-value")
            }
          ),
          actual_points = colDef(
            name = "ACTUAL",
            width = 100,
            align = "center",
            cell = function(value) {
              div(format(round(value, 1), nsmall = 1), class = "stats-value")
            }
          ),
          receiver_id = colDef(
            name = "",
            minWidth = 390,
            cell = function(value, index) {
              create_bar_chart_cell(
                df$expected_points[index],
                df$actual_points[index],
                max_val,
                index == 1
              )
            }
          ),
          position = colDef(show = FALSE),
          team = colDef(show = FALSE),
          team_name = colDef(show = FALSE),
          weeks_played_list = colDef(show = FALSE),
          total_expected = colDef(show = FALSE),
          total_actual = colDef(show = FALSE),
          fp_diff_per_game = colDef(show = FALSE)
        ),
        borderless = TRUE,
        showSortIcon = FALSE,
        striped = FALSE,
        compact = TRUE,
        wrap = FALSE,
        defaultPageSize = num_players_val,
        showPageInfo = FALSE,
        showPagination = FALSE,
        style = list(
          fontFamily = "'Inter', sans-serif",
          width = "940px",
          maxWidth = "940px"
        )
      )
    })
  })
}

# Helper functions (reuse from mod_top_performers.R)
create_empty_table <- function(message) {
  reactable(
    data.frame(Message = message),
    columns = list(
      Message = colDef(
        name = "",
        cell = function(value) {
          div(
            style = paste0(
              "text-align: center; ",
              "padding: 40px; ",
              "color: ", etr_colors$gray_600, "; ",
              "font-size: 1.2rem;"
            ),
            value
          )
        }
      )
    ),
    theme = etr_reactable(),
    borderless = TRUE,
    showSortIcon = FALSE
  )
}

create_player_cell <- function(player, team, team_name) {
  logo_url <- paste0("logos/", team, ".webp")
  
  htmltools::div(
    style = "position: relative; height: 100%; display: table; width: 100%; padding: 0 10px;",
    htmltools::div(
      style = paste0(
        "position: absolute; ",
        "top: 50%; left: 80%; ",
        "transform: translate(-50%, -50%) scale(1); ",
        "width: 280%; height: 280%; ",
        "background-image: url('", logo_url, "'); ",
        "background-repeat: no-repeat; ",
        "background-position: center; ",
        "background-size: contain; ",
        "opacity: 0.08; ",
        "pointer-events: none;"
      )
    ),
    htmltools::div(
      style = "position: relative; z-index: 1; display: table-cell; vertical-align: middle;",
      htmltools::div(
        player,
        style = paste0(
          "font-weight: ", etr_fonts$weight_semibold, "; ",
          "font-size: 1.32rem; ",
          "margin: 0; ",
          "line-height: 1.1;"
        )
      ),
      htmltools::div(
        team_name,
        style = paste0(
          "font-size: 0.88rem; ",
          "color: ", etr_colors$gray_600, "; ",
          "margin: 2px 0 0 0; ",
          "line-height: 1.1;"
        )
      )
    )
  )
}

create_bar_chart_cell <- function(expected, actual, max_val, show_labels = FALSE) {
  expected_width <- if (expected > 0) (expected / max_val) * 100 else 0
  actual_width <- if (actual > 0) (actual / max_val) * 100 else 0
  
  div(
    class = "bar-chart-cell",
    div(
      class = "bar-container",
      if (actual_width > 0) {
        div(
          class = "actual-bar",
          style = paste0("width: ", actual_width, "%;")
        )
      },
      if (expected_width > 0) {
        div(
          class = "expected-bar",
          style = paste0("width: ", expected_width, "%;"),
          if (show_labels) {
            div("EXPECTED POINTS",
                class = "bar-label expected-label")
          }
        )
      },
      if (show_labels && actual_width > 15) {
        div("ACTUAL POINTS",
            class = "bar-label actual-label")
      }
    )
  )
}