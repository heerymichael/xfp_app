# modules/components/comp_week_selector.R - Reusable week selector component with toggle buttons

# Week Selector UI
comp_week_selector_ui <- function(id) {
  ns <- NS(id)
  
  div(
    style = "margin-top: 20px;",
    div(
      tags$label("WHICH WEEKS", 
                 class = "control-label"),
      
      # Hidden checkbox group for backend
      div(style = "display: none;",
          checkboxGroupInput(ns("weeks"), "WHICH WEEKS?",
                             choices = 1:17,
                             selected = 1:17,
                             inline = FALSE)
      ),
      
      # Visual grid
      div(
        id = ns("week_grid"),
        style = paste0(
          "display: flex; ",
          "justify-content: space-between; ",
          "align-items: center; ",
          "padding: 12px; ",
          "background: white; ",
          "border: 1px solid #ced4da; ",
          "border-radius: 6px; ",
          "overflow-x: auto; ",
          "width: 100%; ",
          "box-sizing: border-box;"
        ),
        
        # Week buttons container
        div(
          style = "display: flex; gap: 4px; flex: 1;",
          lapply(1:17, function(i) {
            div(
              id = ns(paste0("week_box_", i)),
              class = "week-selector-box selected",
              `data-week` = i,
              style = paste0(
                "flex: 1; ",
                "min-width: 35px; ",
                "max-width: 50px; ",
                "height: 40px; ",
                "display: flex; ",
                "align-items: center; ",
                "justify-content: center; ",
                "font-size: 23px; ",
                "font-weight: 600; ",
                "border-radius: 6px; ",
                "cursor: pointer; ",
                "user-select: none; ",
                "transition: all 0.15s ease; ",
                "margin: 0 2px; ",
                "background: #37af4a; ",
                "color: white; ",
                "border: 1px solid #2d9940;"
              ),
              onclick = paste0("Shiny.setInputValue('", ns("week_toggle"), "', ", i, ", {priority: 'event'})"),
              as.character(i)
            )
          })
        ),
        
        # Action buttons with toggle style
        div(
          style = "display: flex; gap: 8px; margin-left: 20px;",
          actionButton(ns("select_all_weeks"), "All", 
                       class = "toggle-btn"),
          actionButton(ns("select_none_weeks"), "None", 
                       class = "toggle-btn"),
          actionButton(ns("select_last4_weeks"), "Last 4", 
                       class = "toggle-btn")
        )
      )
    )
  )
}

# Week Selector Server
comp_week_selector_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    selected_weeks <- reactiveVal(1:17)
    
    # Update available weeks based on data
    observe({
      req(data())
      weeks <- sort(unique(data()$week))
      selected_weeks(weeks)
      updateCheckboxGroupInput(session, "weeks",
                               choices = weeks,
                               selected = weeks)
      
      # Update visual grid
      lapply(1:17, function(i) {
        if (i %in% weeks) {
          shinyjs::runjs(paste0(
            "$('#", ns(paste0("week_box_", i)), "').addClass('selected').show();"
          ))
        } else {
          shinyjs::runjs(paste0(
            "$('#", ns(paste0("week_box_", i)), "').removeClass('selected').hide();"
          ))
        }
      })
    })
    
    # Handle individual week toggles
    observeEvent(input$week_toggle, {
      week <- input$week_toggle
      current <- selected_weeks()
      
      if (week %in% current) {
        selected_weeks(setdiff(current, week))
        shinyjs::runjs(paste0(
          "$('#", ns(paste0("week_box_", week)), "').removeClass('selected');"
        ))
      } else {
        selected_weeks(sort(c(current, week)))
        shinyjs::runjs(paste0(
          "$('#", ns(paste0("week_box_", week)), "').addClass('selected');"
        ))
      }
      
      updateCheckboxGroupInput(session, "weeks", selected = selected_weeks())
    })
    
    # Handle "All" button
    observeEvent(input$select_all_weeks, {
      all_weeks <- sort(unique(data()$week))
      selected_weeks(all_weeks)
      updateCheckboxGroupInput(session, "weeks", selected = all_weeks)
      
      lapply(all_weeks, function(i) {
        shinyjs::runjs(paste0(
          "$('#", ns(paste0("week_box_", i)), "').addClass('selected');"
        ))
      })
    })
    
    # Handle "None" button
    observeEvent(input$select_none_weeks, {
      selected_weeks(numeric(0))
      updateCheckboxGroupInput(session, "weeks", selected = numeric(0))
      
      all_weeks <- sort(unique(data()$week))
      lapply(all_weeks, function(i) {
        shinyjs::runjs(paste0(
          "$('#", ns(paste0("week_box_", i)), "').removeClass('selected');"
        ))
      })
    })
    
    # Handle "Last 4" button
    observeEvent(input$select_last4_weeks, {
      all_weeks <- sort(unique(data()$week))
      last_four <- tail(all_weeks, 4)
      selected_weeks(last_four)
      updateCheckboxGroupInput(session, "weeks", selected = last_four)
      
      lapply(all_weeks, function(i) {
        if (i %in% last_four) {
          shinyjs::runjs(paste0(
            "$('#", ns(paste0("week_box_", i)), "').addClass('selected');"
          ))
        } else {
          shinyjs::runjs(paste0(
            "$('#", ns(paste0("week_box_", i)), "').removeClass('selected');"
          ))
        }
      })
    })
    
    return(selected_weeks)
  })
}