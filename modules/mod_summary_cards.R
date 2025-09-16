# modules/mod_summary_cards.R
# Summary Cards Module

#' Summary Cards Module UI
#'
#' @param id Character string. The module's ID
#' @param title Character string. Title for the card section
#' @param show_refresh Logical. Whether to show refresh button
#'
#' @return Shiny UI elements
mod_summary_cards_ui <- function(id, title = "Key Metrics", show_refresh = TRUE) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      if (show_refresh) {
        column(12,
               div(
                 class = "pull-right",
                 actionButton(
                   ns("refresh_metrics"),
                   "Refresh Metrics",
                   icon = icon("sync-alt"),
                   class = "btn-outline-primary btn-sm"
                 )
               ),
               h4(title),
               hr()
        )
      } else {
        column(12, h4(title), hr())
      }
    ),
    
    fluidRow(
      column(3, valueBoxOutput(ns("total_consultations"), width = NULL)),
      column(3, valueBoxOutput(ns("active_facilities"), width = NULL)),
      column(3, valueBoxOutput(ns("disease_categories"), width = NULL)),
      column(3, valueBoxOutput(ns("alert_status"), width = NULL))
    ),
    
    fluidRow(
      column(4, infoBoxOutput(ns("recent_trend"), width = NULL)),
      column(4, infoBoxOutput(ns("geographic_coverage"), width = NULL)),
      column(4, infoBoxOutput(ns("climate_sensitive"), width = NULL))
    )
  )
}

#' Summary Cards Module Server
#'
#' @param id Character string. The module's ID
#' @param data Reactive. Filtered dataset
#' @param config List. Configuration parameters
#'
#' @return Reactive containing key metrics
mod_summary_cards_server <- function(id, data, config = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for metrics
    metrics <- reactiveValues(
      last_updated = Sys.time(),
      trend_data = NULL
    )
    
    # Calculate key metrics
    key_metrics <- reactive({
      req(data())
      
      df <- data()
      
      # Basic counts
      total_consultations <- nrow(df)
      unique_facilities <- if ("orgunit" %in% colnames(df)) {
        length(unique(df$orgunit))
      } else { 0 }
      
      disease_categories <- if ("category_canonical_disease_imc" %in% colnames(df)) {
        length(unique(df$category_canonical_disease_imc[df$category_canonical_disease_imc != "Unknown"]))
      } else { 0 }
      
      # Alert status calculation - check for epidemic diseases using new taxonomy
      epidemic_cases <- 0
      if ("epidemic_prone" %in% colnames(df)) {
        epidemic_cases <- sum(df$epidemic_prone == TRUE, na.rm = TRUE)
      } else if ("is_epidemic" %in% colnames(df)) {
        epidemic_cases <- sum(df$is_epidemic == TRUE, na.rm = TRUE)
      } else if ("morbidity" %in% colnames(df) && exists("get_epidemic_groups")) {
        epidemic_conditions <- unlist(get_epidemic_groups(), use.names = FALSE)
        epidemic_cases <- sum(df$morbidity %in% epidemic_conditions, na.rm = TRUE)
      }
      
      # Geographic coverage
      geographic_coverage <- if ("admin1" %in% colnames(df)) {
        length(unique(df$admin1))
      } else { 0 }
      
      # Climate-sensitive cases
      climate_sensitive_cases <- 0
      if ("climate_sensitive" %in% colnames(df)) {
        climate_sensitive_cases <- sum(df$climate_sensitive == TRUE, na.rm = TRUE)
      } else if ("climate_sensitive_case" %in% colnames(df)) {
        climate_sensitive_cases <- sum(df$climate_sensitive_case == TRUE, na.rm = TRUE)
      }
      
      # Recent trend calculation - prioritize datevisit per preprocessing
      date_col <- if ("datevisit" %in% colnames(df)) "datevisit" 
                  else if ("datevisitnew" %in% colnames(df)) "datevisitnew" 
                  else NULL
      
      recent_trend <- if (!is.null(date_col)) {
        current_date <- max(df[[date_col]], na.rm = TRUE)
        one_month_ago <- current_date - 30
        
        current_month <- sum(df[[date_col]] >= one_month_ago, na.rm = TRUE)
        previous_month <- sum(df[[date_col]] >= (one_month_ago - 30) & 
                             df[[date_col]] < one_month_ago, na.rm = TRUE)
        
        if (previous_month > 0) {
          round(((current_month - previous_month) / previous_month) * 100, 1)
        } else { 0 }
      } else { 0 }
      
      list(
        total_consultations = total_consultations,
        unique_facilities = unique_facilities,
        disease_categories = disease_categories,
        epidemic_cases = epidemic_cases,
        geographic_coverage = geographic_coverage,
        climate_sensitive_cases = climate_sensitive_cases,
        recent_trend = recent_trend,
        last_updated = Sys.time()
      )
    })
    
    # Update metrics when refresh button is clicked
    observeEvent(input$refresh_metrics, {
      metrics$last_updated <- Sys.time()
      showNotification("Metrics refreshed", type = "success", duration = 2)
    })
    
    # Value boxes
    output$total_consultations <- renderValueBox({
      m <- key_metrics()
      
      valueBox(
        value = format(m$total_consultations, big.mark = ","),
        subtitle = "Total Consultations",
        icon = icon("user-md"),
        color = "blue"
      )
    })
    
    output$active_facilities <- renderValueBox({
      m <- key_metrics()
      
      valueBox(
        value = m$unique_facilities,
        subtitle = "Active Facilities",
        icon = icon("hospital"),
        color = "green"
      )
    })
    
    output$disease_categories <- renderValueBox({
      m <- key_metrics()
      
      valueBox(
        value = m$disease_categories,
        subtitle = "Disease Categories",
        icon = icon("virus"),
        color = "yellow"
      )
    })
    
    output$alert_status <- renderValueBox({
      m <- key_metrics()
      
      alert_color <- if (m$epidemic_cases > 50) "red" 
                    else if (m$epidemic_cases > 10) "orange" 
                    else "green"
      
      valueBox(
        value = m$epidemic_cases,
        subtitle = "Epidemic Alert Cases",
        icon = icon("exclamation-triangle"),
        color = alert_color
      )
    })
    
    # Info boxes
    output$recent_trend <- renderInfoBox({
      m <- key_metrics()
      
      trend_icon <- if (m$recent_trend > 5) "arrow-up" 
                   else if (m$recent_trend < -5) "arrow-down" 
                   else "minus"
      trend_color <- if (m$recent_trend > 5) "green" 
                    else if (m$recent_trend < -5) "red" 
                    else "blue"
      
      infoBox(
        "Monthly Trend",
        paste0(ifelse(m$recent_trend > 0, "+", ""), m$recent_trend, "%"),
        icon = icon(trend_icon),
        color = trend_color,
        subtitle = "vs Previous Month"
      )
    })
    
    output$geographic_coverage <- renderInfoBox({
      m <- key_metrics()
      
      infoBox(
        "Geographic Coverage",
        paste(m$geographic_coverage, "Governorates"),
        icon = icon("map"),
        color = "purple"
      )
    })
    
    output$climate_sensitive <- renderInfoBox({
      m <- key_metrics()
      
      climate_percentage <- if (m$total_consultations > 0) {
        round((m$climate_sensitive_cases / m$total_consultations) * 100, 1)
      } else { 0 }
      
      infoBox(
        "Climate-Sensitive",
        paste0(m$climate_sensitive_cases, " (", climate_percentage, "%)"),
        icon = icon("thermometer-half"),
        color = "orange"
      )
    })
    
    # Return reactive metrics for use by parent module
    return(key_metrics)
  })
}