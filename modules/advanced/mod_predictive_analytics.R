# modules/advanced/mod_predictive_analytics.R - Enhanced Predictive Analytics Module
# Features: Disease drill-down, taxonomy-based analysis, decision-maker insights

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(shiny)
  library(shinydashboard)
  library(lubridate)
  library(stringr)
  library(scales)
})

# Define null coalescing operator if not available
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

# ─────────────────────────────────────────────────────────────────────────────
# Enhanced Supporting Functions
# ─────────────────────────────────────────────────────────────────────────────

#' Prepare time series data for forecasting with taxonomy context
prepare_forecast_data <- function(data, disease, facility = "all", taxonomy_filter = "all") {
  
  # Start with base filtering
  if (disease %in% unique(data$category_canonical_disease_imc)) {
    # Category-level filtering
    df <- data %>% filter(category_canonical_disease_imc == disease)
  } else {
    # Specific disease filtering
    df <- data %>% filter(morbidity == disease)
  }
  
  # Apply facility filter
  if (facility != "all") {
    df <- df %>% filter(orgunit == facility)
  }
  
  # Create weekly time series using available date column
  date_col <- if ("datevisit" %in% names(df)) "datevisit" 
              else if ("datevisitnew" %in% names(df)) "datevisitnew" 
              else NULL
  
  cat("🔍 prepare_forecast_data: date column =", date_col, ", rows =", nrow(df), "\n")
  
  if (is.null(date_col) || nrow(df) == 0) {
    # Return empty structure if no valid data
    cat("⚠️  No valid data for forecasting\n")
    return(list(
      ts_object = ts(c(0), frequency = 52),
      raw_data = data.frame(week = Sys.Date(), cases = 0),
      disease = disease,
      facility = facility,
      taxonomy_filter = taxonomy_filter,
      total_cases = 0,
      epidemic_percentage = 0,
      weeks_observed = 0
    ))
  }
  
  # Convert date column to Date if it isn't already
  if (!inherits(df[[date_col]], "Date")) {
    df[[date_col]] <- as.Date(df[[date_col]])
  }
  
  ts_data <- df %>%
    mutate(week = floor_date(.data[[date_col]], "week")) %>%
    count(week, name = "cases") %>%
    complete(week = seq(min(week, na.rm = TRUE), max(week, na.rm = TRUE), by = "week"), 
             fill = list(cases = 0)) %>%
    arrange(week)
  
  cat("📅 Generated", nrow(ts_data), "time series points\n")
  
  # Calculate metadata for decision-making
  total_cases <- sum(ts_data$cases, na.rm = TRUE)
  epidemic_percentage <- if ("epidemic_prone" %in% names(df)) {
    round(sum(df$epidemic_prone, na.rm = TRUE) / nrow(df) * 100, 1)
  } else { 0 }
  
  # Convert to ts object
  if (nrow(ts_data) > 0 && total_cases > 0) {
    ts_object <- ts(ts_data$cases, frequency = 52, 
                   start = c(year(min(ts_data$week)), week(min(ts_data$week))))
  } else {
    ts_object <- ts(c(0), frequency = 52)
  }
  
  return(list(
    ts_object = ts_object,
    raw_data = ts_data,
    disease = disease,
    facility = facility,
    taxonomy_filter = taxonomy_filter,
    total_cases = total_cases,
    epidemic_percentage = epidemic_percentage,
    weeks_observed = nrow(ts_data)
  ))
}

#' Enhanced risk assessment with taxonomy integration
calculate_enhanced_risk_assessment <- function(data, threshold = 0.3) {
  
  date_col <- if ("datevisit" %in% names(data)) "datevisit" 
              else if ("datevisitnew" %in% names(data)) "datevisitnew" 
              else NULL
              
  if (is.null(date_col) || nrow(data) == 0) {
    # Return empty risk analysis if no valid data
    return(data.frame(
      category_canonical_disease_imc = character(0),
      cases_recent = numeric(0),
      risk_score = numeric(0),
      risk_category = character(0),
      recommended_action = character(0),
      alert_triggered = logical(0),
      resource_priority = integer(0)
    ))
  }
  
  # Calculate recent trends (last 4 weeks vs previous 4 weeks)
  current_date <- max(data[[date_col]], na.rm = TRUE)
  recent_data <- data %>%
    filter(.data[[date_col]] >= current_date - weeks(8)) %>%
    mutate(
      period = if_else(.data[[date_col]] >= current_date - weeks(4), "recent", "previous"),
      week = floor_date(.data[[date_col]], "week")
    )
  
  # Risk analysis by morbidity category with taxonomy context
  risk_analysis <- recent_data %>%
    group_by(category_canonical_disease_imc, period) %>%
    summarise(
      cases = n(),
      epidemic_cases = sum(if("epidemic_prone" %in% names(.)) epidemic_prone else FALSE, na.rm = TRUE),
      unique_facilities = n_distinct(orgunit, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = period, values_from = c(cases, epidemic_cases, unique_facilities),
                values_fill = 0) %>%
    mutate(
      # Trend calculations
      case_trend = (cases_recent - cases_previous) / pmax(cases_previous, 1),
      epidemic_trend = (epidemic_cases_recent - epidemic_cases_previous) / pmax(epidemic_cases_previous, 1),
      facility_spread = unique_facilities_recent - unique_facilities_previous,
      
      # Risk components (0-1 scale)
      volume_risk = pmin(cases_recent / 100, 1),  # Normalize to reasonable scale
      epidemic_risk = epidemic_cases_recent / pmax(cases_recent, 1),
      spread_risk = pmin(facility_spread / 5, 1),  # More than 5 new facilities = high risk
      trend_risk = pmin(pmax(case_trend, 0) / 2, 1),  # 200%+ increase = max risk
      
      # Composite risk score (weighted average)
      risk_score = 0.30 * volume_risk + 0.30 * epidemic_risk + 
                   0.20 * spread_risk + 0.20 * trend_risk,
      
      # Risk categories with actionable labels
      risk_category = case_when(
        risk_score >= 0.7 ~ "Critical - Immediate Action Required",
        risk_score >= 0.5 ~ "High - Enhanced Monitoring",
        risk_score >= 0.3 ~ "Medium - Standard Surveillance",
        risk_score >= 0.1 ~ "Low - Routine Monitoring",
        TRUE ~ "Very Low - Background Surveillance"
      ),
      
      # Decision recommendations
      recommended_action = case_when(
        risk_score >= 0.7 ~ "Deploy rapid response team, increase resources",
        risk_score >= 0.5 ~ "Activate alert protocols, prepare resources",
        risk_score >= 0.3 ~ "Monitor closely, ready contingency plans",
        risk_score >= 0.1 ~ "Continue routine surveillance",
        TRUE ~ "Maintain baseline monitoring"
      ),
      
      # Alert status
      alert_triggered = risk_score >= threshold,
      
      # Resource priority (1-5 scale)
      resource_priority = case_when(
        risk_score >= 0.7 ~ 5L,
        risk_score >= 0.5 ~ 4L,
        risk_score >= 0.3 ~ 3L,
        risk_score >= 0.1 ~ 2L,
        TRUE ~ 1L
      )
    ) %>%
    arrange(desc(risk_score))
  
  return(risk_analysis)
}

# ─────────────────────────────────────────────────────────────────────────────
# Enhanced UI with Drill-down Capabilities
# ─────────────────────────────────────────────────────────────────────────────

#' Enhanced Predictive Analytics Module UI
predictive_analytics_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Key Performance Indicators
    fluidRow(
      column(3, valueBoxOutput(ns("total_risk_alerts"), width = NULL)),
      column(3, valueBoxOutput(ns("high_risk_diseases"), width = NULL)),
      column(3, valueBoxOutput(ns("epidemic_trend"), width = NULL)),
      column(3, valueBoxOutput(ns("data_status"), width = NULL))
    ),
    
    fluidRow(
      column(12,
        box(
          title = "🔮 Advanced Predictive Analytics for Decision-Makers",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          
          tabsetPanel(
            id = ns("prediction_tabs"),
            
            # Enhanced Demand Forecasting Tab
            tabPanel(
              "📈 Smart Demand Forecasting",
              br(),
              fluidRow(
                column(4,
                  wellPanel(
                    style = "background-color: #f8f9fa;",
                    h5("🎯 Forecasting Configuration", style = "color: #495057;"),
                    
                    selectInput(ns("forecast_category"), "Disease Category:",
                                choices = NULL,
                                selected = NULL),
                    
                    conditionalPanel(
                      condition = paste0("input['", ns("forecast_category"), "'] != null"),
                      selectInput(ns("forecast_specific"), "Specific Disease (Optional):",
                                  choices = NULL)
                    ),
                    
                    selectInput(ns("forecast_facility"), "Facility Focus:",
                                choices = c("All Facilities" = "all")),
                    
                    numericInput(ns("forecast_horizon"), "Forecast Weeks:",
                                value = 12, min = 4, max = 26, step = 1),
                    
                    actionButton(ns("generate_forecast"), "🚀 Generate Forecast",
                                class = "btn-primary", style = "width: 100%;")
                  )
                ),
                column(8,
                  plotlyOutput(ns("forecast_plot"), height = "500px"),
                  br(),
                  div(id = ns("forecast_insights"),
                    h5("📊 Forecast Insights"),
                    verbatimTextOutput(ns("forecast_summary"))
                  )
                )
              )
            ),
            
            # Enhanced Risk Assessment Tab
            tabPanel(
              "⚠️ Risk Assessment Dashboard",
              br(),
              fluidRow(
                column(4,
                  wellPanel(
                    style = "background-color: #fff3cd;",
                    h5("🎛️ Risk Analysis Controls", style = "color: #856404;"),
                    
                    sliderInput(ns("risk_threshold"), "Alert Threshold:",
                               min = 0.1, max = 0.9, value = 0.3, step = 0.05),
                    
                    actionButton(ns("calculate_risk"), "🔍 Analyze Risk",
                                class = "btn-warning", style = "width: 100%;")
                  )
                ),
                column(8,
                  DT::dataTableOutput(ns("risk_table"))
                )
              ),
              br(),
              fluidRow(
                column(12,
                  plotlyOutput(ns("risk_distribution"), height = "400px")
                )
              )
            )
          )
        )
      )
    )
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Enhanced Server Logic
# ─────────────────────────────────────────────────────────────────────────────

#' Enhanced Predictive Analytics Module Server
predictive_analytics_Server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for enhanced predictions and analysis
    prediction_values <- reactiveValues(
      forecast_data = NULL,
      risk_analysis = NULL,
      disease_choices = NULL,
      specific_disease_choices = NULL
    )
    
    # Update disease category choices when data becomes available
    # Use observeEvent instead of observe for better control
    observeEvent(filtered_data(), {
      df <- filtered_data()
      
      # DEBUG: Print data information
      cat("🔍 PREDICTIVE ANALYTICS DEBUG:\n")
      cat("  - Data is null:", is.null(df), "\n")
      if (!is.null(df)) {
        cat("  - Data rows:", nrow(df), "\n")
        cat("  - Data columns:", ncol(df), "\n")
        cat("  - Available columns:", paste(names(df), collapse = ", "), "\n")
        cat("  - Has category_canonical_disease_imc:", "category_canonical_disease_imc" %in% names(df), "\n")
      }
      
      # Basic validation
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        cat("  - Validation failed: data is null, not data.frame, or empty\n")
        return()
      }
      
      # Update disease categories
      if ("category_canonical_disease_imc" %in% names(df)) {
        cat("  - Found category_canonical_disease_imc column\n")
        categories <- df %>%
          dplyr::filter(!is.na(category_canonical_disease_imc), category_canonical_disease_imc != "") %>%
          dplyr::count(category_canonical_disease_imc, sort = TRUE) %>%
          dplyr::pull(category_canonical_disease_imc)
        
        cat("  - Categories found:", length(categories), "\n")
        if (length(categories) > 0) {
          cat("  - Sample categories:", paste(head(categories, 3), collapse = ", "), "\n")
          updateSelectInput(session, "forecast_category",
                           choices = setNames(categories, categories))
          cat("  - Updated forecast_category dropdown\n")
        } else {
          cat("  - No valid categories found after filtering\n")
        }
      } else {
        cat("  - category_canonical_disease_imc column NOT found\n")
        cat("  - Available columns:", paste(names(df), collapse = ", "), "\n")
      }
      
      # Update facility choices
      if ("orgunit" %in% names(df)) {
        facilities <- sort(unique(df$orgunit))
        if (length(facilities) > 0) {
          facility_choices <- c("All Facilities" = "all", setNames(facilities, facilities))
          updateSelectInput(session, "forecast_facility", choices = facility_choices)
        }
      }
    }, ignoreInit = FALSE, ignoreNULL = TRUE)
    
    # Update specific disease choices based on selected category
    observe({
      req(input$forecast_category, filtered_data())
      
      df <- filtered_data()
      
      if ("morbidity" %in% names(df)) {
        specific_diseases <- df %>%
          filter(category_canonical_disease_imc == input$forecast_category) %>%
          count(morbidity, sort = TRUE) %>%
          pull(morbidity)
        
        prediction_values$specific_disease_choices <- specific_diseases
        
        choices <- c("All in Category" = "", setNames(specific_diseases, specific_diseases))
        
        updateSelectInput(session, "forecast_specific", choices = choices)
      }
    })
    
    # Key Performance Indicators
    output$total_risk_alerts <- renderValueBox({
      req(prediction_values$risk_analysis)
      
      alerts <- sum(prediction_values$risk_analysis$alert_triggered, na.rm = TRUE)
      
      valueBox(
        value = alerts,
        subtitle = "Active Risk Alerts",
        icon = icon("exclamation-triangle"),
        color = if (alerts > 5) "red" else if (alerts > 2) "yellow" else "green"
      )
    })
    
    output$high_risk_diseases <- renderValueBox({
      req(prediction_values$risk_analysis)
      
      high_risk <- sum(prediction_values$risk_analysis$risk_score >= 0.5, na.rm = TRUE)
      
      valueBox(
        value = high_risk,
        subtitle = "High-Risk Categories",
        icon = icon("virus"),
        color = if (high_risk > 3) "red" else if (high_risk > 1) "yellow" else "green"
      )
    })
    
    output$epidemic_trend <- renderValueBox({
      req(filtered_data())
      
      df <- filtered_data()
      if ("epidemic_prone" %in% names(df)) {
        epidemic_pct <- round(sum(df$epidemic_prone, na.rm = TRUE) / nrow(df) * 100, 1)
      } else {
        epidemic_pct <- 0
      }
      
      valueBox(
        value = paste0(epidemic_pct, "%"),
        subtitle = "Epidemic-Prone Cases",
        icon = icon("chart-line"),
        color = if (epidemic_pct > 15) "red" else if (epidemic_pct > 8) "yellow" else "blue"
      )
    })
    
    output$data_status <- renderValueBox({
      req(filtered_data())
      
      df <- filtered_data()
      status <- if (nrow(df) > 0) "Available" else "No Data"
      
      valueBox(
        value = nrow(df),
        subtitle = "Total Records",
        icon = icon("database"),
        color = if (nrow(df) > 1000) "green" else if (nrow(df) > 100) "yellow" else "red"
      )
    })
    
    # Enhanced forecast generation
    observeEvent(input$generate_forecast, {
      req(input$forecast_category, filtered_data())
      
      cat("🚀 Starting forecast generation...\n")
      
      tryCatch({
        df <- filtered_data()
        disease <- if (!is.null(input$forecast_specific) && input$forecast_specific != "") {
          input$forecast_specific
        } else {
          input$forecast_category
        }
        
        cat("📊 Selected disease/category:", disease, "\n")
        cat("📈 Data rows available:", nrow(df), "\n")
        
        # Generate forecast data
        forecast_data <- prepare_forecast_data(
          df, disease, input$forecast_facility %||% "all", "all"
        )
        
        cat("🔍 Forecast data prepared. Cases found:", forecast_data$total_cases, "\n")
        prediction_values$forecast_data <- forecast_data
        
        # Simple forecast calculation
        ts_data <- forecast_data$raw_data
        cat("📅 Time series data points:", nrow(ts_data), "\n")
        
        if (nrow(ts_data) > 3) {
          
          # Calculate trend
          recent_avg <- mean(tail(ts_data$cases, min(4, nrow(ts_data))), na.rm = TRUE)
          overall_avg <- mean(ts_data$cases, na.rm = TRUE)
          
          cat("📊 Recent average:", recent_avg, "Overall average:", overall_avg, "\n")
          
          # Generate simple forecast - fix the weeks() function issue
          max_week <- max(ts_data$week, na.rm = TRUE)
          forecast_weeks <- seq(from = max_week + 7, # Add 7 days instead of weeks(1)
                               by = 7, # 7 days per week
                               length.out = input$forecast_horizon)
          forecast_weeks <- as.Date(forecast_weeks)
          
          # Simple trend-based forecast
          forecast_values <- rep(max(recent_avg, 1), input$forecast_horizon)
          
          # Add some uncertainty bounds
          forecast_lower <- pmax(0, forecast_values * 0.7)
          forecast_upper <- forecast_values * 1.3
          
          prediction_values$forecast_result <- data.frame(
            week = forecast_weeks,
            forecast = forecast_values,
            lower = forecast_lower,
            upper = forecast_upper
          )
          
          cat("✅ Forecast generated successfully for", input$forecast_horizon, "periods\n")
          
          # Show notification
          showNotification("Forecast generated successfully!", type = "message")
          
        } else {
          cat("⚠️  Insufficient data for forecasting (need >3 points, have", nrow(ts_data), ")\n")
          showNotification("Insufficient data for forecasting. Need at least 4 data points.", 
                          type = "warning", duration = 5000)
        }
        
      }, error = function(e) {
        cat("❌ Error in forecast generation:", e$message, "\n")
        showNotification(paste("Error generating forecast:", e$message), 
                        type = "error", duration = 8000)
      })
    })
    
    # Enhanced risk calculation
    observeEvent(input$calculate_risk, {
      req(filtered_data())
      
      risk_analysis <- calculate_enhanced_risk_assessment(
        filtered_data(), input$risk_threshold
      )
      
      prediction_values$risk_analysis <- risk_analysis
    })
    
    # Forecast plot
    output$forecast_plot <- renderPlotly({
      req(prediction_values$forecast_data, prediction_values$forecast_result)
      
      historical <- prediction_values$forecast_data$raw_data
      forecast <- prediction_values$forecast_result
      
      p <- ggplot() +
        geom_line(data = historical, aes(x = week, y = cases), 
                 color = "steelblue", size = 1) +
        geom_point(data = historical, aes(x = week, y = cases), 
                  color = "steelblue", size = 2) +
        geom_ribbon(data = forecast, aes(x = week, ymin = lower, ymax = upper), 
                   fill = "orange", alpha = 0.3) +
        geom_line(data = forecast, aes(x = week, y = forecast), 
                 color = "orange", size = 1, linetype = "dashed") +
        geom_point(data = forecast, aes(x = week, y = forecast), 
                  color = "orange", size = 2) +
        labs(
          title = paste("Demand Forecast:", prediction_values$forecast_data$disease),
          subtitle = paste("Facility:", prediction_values$forecast_data$facility),
          x = "Week",
          y = "Predicted Cases"
        ) +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma_format())
      
      ggplotly(p, tooltip = c("x", "y"))
    })
    
    # Risk analysis table
    output$risk_table <- DT::renderDataTable({
      req(prediction_values$risk_analysis)
      
      risk_data <- prediction_values$risk_analysis %>%
        select(
          Category = category_canonical_disease_imc,
          `Risk Score` = risk_score,
          `Risk Level` = risk_category,
          `Recent Cases` = cases_recent,
          `Action Required` = recommended_action
        ) %>%
        mutate(
          `Risk Score` = round(`Risk Score`, 3)
        )
      
      DT::datatable(
        risk_data,
        options = list(
          pageLength = 15,
          dom = "tip",
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Risk Level",
          backgroundColor = DT::styleEqual(
            c("Critical - Immediate Action Required", "High - Enhanced Monitoring", 
              "Medium - Standard Surveillance", "Low - Routine Monitoring"),
            c("#f8d7da", "#fff3cd", "#d1ecf1", "#d4edda")
          )
        )
    })
    
    # Forecast summary
    output$forecast_summary <- renderText({
      req(prediction_values$forecast_data, prediction_values$forecast_result)
      
      forecast_data <- prediction_values$forecast_data
      forecast_result <- prediction_values$forecast_result
      
      total_predicted <- sum(forecast_result$forecast, na.rm = TRUE)
      avg_weekly <- mean(forecast_result$forecast, na.rm = TRUE)
      peak_week <- forecast_result$week[which.max(forecast_result$forecast)]
      
      paste0(
        "📊 FORECAST SUMMARY\n",
        "══════════════════════════════════════════════\n",
        "Disease: ", forecast_data$disease, "\n",
        "Historical Period: ", forecast_data$weeks_observed, " weeks\n",
        "Total Historical Cases: ", forecast_data$total_cases, "\n\n",
        
        "🔮 PREDICTIONS (Next ", input$forecast_horizon, " weeks)\n",
        "──────────────────────────────────────────────\n",
        "Total Predicted Cases: ", round(total_predicted), "\n",
        "Average Weekly Cases: ", round(avg_weekly, 1), "\n",
        "Peak Expected: ", format(peak_week, "%Y-%m-%d"), "\n",
        "Peak Cases: ", round(max(forecast_result$forecast, na.rm = TRUE)), "\n\n",
        
        "💡 INSIGHTS\n",
        "──────────────────────────────────────────────\n",
        if (avg_weekly > mean(forecast_data$raw_data$cases, na.rm = TRUE) * 1.2) {
          "⚠️  ALERT: Significant increase predicted\n"
        } else if (avg_weekly < mean(forecast_data$raw_data$cases, na.rm = TRUE) * 0.8) {
          "📉 Decline expected - consider resource reallocation\n"
        } else {
          "📈 Stable trend expected\n"
        },
        
        "🎯 Confidence: Medium (Simple forecasting model)\n",
        "📅 Next Review: ", format(Sys.Date() + weeks(2), "%Y-%m-%d")
      )
    })
    
    # Risk distribution plot
    output$risk_distribution <- renderPlotly({
      req(prediction_values$risk_analysis)
      
      risk_data <- prediction_values$risk_analysis
      
      p <- ggplot(risk_data, aes(x = reorder(category_canonical_disease_imc, risk_score), y = risk_score)) +
        geom_col(aes(fill = risk_category), alpha = 0.8) +
        coord_flip() +
        scale_fill_manual(values = c(
          "Critical - Immediate Action Required" = "#dc3545",
          "High - Enhanced Monitoring" = "#fd7e14",
          "Medium - Standard Surveillance" = "#ffc107",
          "Low - Routine Monitoring" = "#28a745",
          "Very Low - Background Surveillance" = "#6c757d"
        )) +
        labs(
          title = "Risk Score Distribution",
          x = "Disease Category",
          y = "Risk Score",
          fill = "Risk Level"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      ggplotly(p, tooltip = c("y", "fill"))
    })
    
  })
}