# modules/components/comp_team_selector.R - Reusable team selector component

# Team Selector UI
comp_team_selector_ui <- function(id, clear_button_label = "Clear Selection") {
  ns <- NS(id)
  
  tagList(
    div(
      style = paste0("max-width: ", etr_dimensions$content_max_width, "; margin: 0 auto;"),
      
      # Clear button
      div(
        style = paste0("text-align: center; margin-bottom: ", etr_spacing$sm, "px;"),
        actionButton(
          ns("clear_team"), 
          label = clear_button_label,
          style = paste0(
            "background: ", etr_colors$gray_50, "; ",
            "border: 1px solid ", etr_colors$gray_300, "; ",
            "color: ", etr_colors$gray_600, "; ",
            "font-weight: ", etr_fonts$weight_medium, "; ",
            "padding: 0.5rem 1rem; ",
            "border-radius: ", etr_borders$radius_lg, ";"
          )
        )
      ),
      
      # AFC Teams Row
      div(
        class = "logo-row",
        lapply(NFL_TEAMS$afc, function(team) {
          div(
            class = "team-logo",
            id = ns(paste0("logo_", team)),
            tags$img(
              src = sprintf("logos/%s.webp", team),
              style = sprintf("width: %s; height: %s; object-fit: contain;", 
                              etr_dimensions$logo_selector, 
                              etr_dimensions$logo_selector),
              loading = "lazy",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                                ns("team_click"), team),
              title = get_team_name(team),
              alt = get_team_name(team)
            )
          )
        })
      ),
      
      # NFC Teams Row
      div(
        class = "logo-row",
        lapply(NFL_TEAMS$nfc, function(team) {
          div(
            class = "team-logo",
            id = ns(paste0("logo_", team)),
            tags$img(
              src = sprintf("logos/%s.webp", team),
              style = sprintf("width: %s; height: %s; object-fit: contain;", 
                              etr_dimensions$logo_selector, 
                              etr_dimensions$logo_selector),
              loading = "lazy",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                                ns("team_click"), team),
              title = get_team_name(team),
              alt = get_team_name(team)
            )
          )
        })
      )
    )
  )
}

# Team Selector Server
comp_team_selector_server <- function(id, selection_mode = "single", max_selections = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    selected_teams <- reactiveVal(character(0))
    
    # Handle team clicks
    observeEvent(input$team_click, {
      clicked <- input$team_click
      current <- selected_teams()
      
      if (selection_mode == "single") {
        selected_teams(clicked)
      } else {
        if (clicked %in% current) {
          selected_teams(setdiff(current, clicked))
        } else {
          if (is.null(max_selections) || length(current) < max_selections) {
            selected_teams(c(current, clicked))
          } else {
            showNotification(
              paste("Maximum", max_selections, "teams can be selected"),
              type = "warning",
              duration = 3
            )
          }
        }
      }
    })
    
    # Handle clear button
    observeEvent(input$clear_team, {
      selected_teams(character(0))
    })
    
    # Update logo styling based on selection
    observe({
      selected <- selected_teams()
      
      for (team in NFL_TEAMS$all) {
        logo_id <- paste0("#", ns(paste0("logo_", team)))
        
        if (team %in% selected) {
          shinyjs::runjs(sprintf(
            "var el = document.querySelector('%s'); if(el) el.classList.add('selected');",
            logo_id
          ))
        } else {
          shinyjs::runjs(sprintf(
            "var el = document.querySelector('%s'); if(el) el.classList.remove('selected');",
            logo_id
          ))
        }
      }
    })
    
    return(selected_teams)
  })
}