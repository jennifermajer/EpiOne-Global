# modules/mod_settings.R
# Settings and Configuration Module - Refactored to mod_ pattern

#' Settings Module UI
#'
#' @param id Character string. The module's ID
#'
#' @return Shiny UI elements
mod_settings_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(6,
             box(
               title = "AI Configuration",
               status = "warning",
               solidHeader = TRUE,
               width = NULL,
               
               h5("EpiBot AI Assistant Status"),
               hr(),
               div(class = "alert alert-info",
                   div(
                     p(strong("Status: "), textOutput(ns("ai_status"), inline = TRUE)),
                     p(strong("Mode: "), textOutput(ns("ai_mode"), inline = TRUE)),
                     p(strong("Features: "), "Basic chat, Data analysis"),
                     p(strong("Model: "), textOutput(ns("ai_model"), inline = TRUE))
                   )
               ),
               
               h5("AI Configuration Options"),
               fluidRow(
                 column(6,
                        selectInput(ns("ai_response_style"), "Response Style:",
                                    choices = list(
                                      "Professional" = "professional",
                                      "Conversational" = "conversational",
                                      "Technical" = "technical"
                                    ),
                                    selected = "professional")
                 ),
                 column(6,
                        numericInput(ns("ai_max_response_length"), "Max Response Length:",
                                     value = 500, min = 100, max = 2000, step = 50)
                 )
               ),
               
               fluidRow(
                 column(6,
                        checkboxInput(ns("ai_enable_insights"), "Enable Auto-Insights", value = TRUE)
                 ),
                 column(6,
                        checkboxInput(ns("ai_enable_alerts"), "Enable AI Alerts", value = FALSE)
                 )
               ),
               
               div(style = "text-align: center; margin-top: 20px;",
                   actionButton(ns("test_ai_connection"), "Test AI Connection", 
                                class = "btn-info", icon = icon("robot")),
                   actionButton(ns("reset_ai_session"), "Reset AI Session", 
                                class = "btn-warning", icon = icon("refresh"))
               )
             )
      ),
      
      column(6,
             box(
               title = "Data Configuration",
               status = "primary",
               solidHeader = TRUE,
               width = NULL,
               
               h5("Current Data Source:"),
               div(class = "well",
                   verbatimTextOutput(ns("data_source_info"))
               ),
               
               h5("Data Refresh Settings"),
               checkboxInput(ns("use_synthetic"), "Use Synthetic Test Data", value = FALSE),
               helpText("Generates realistic test data with all required fields for dashboards and reports."),
               div(style = "margin-bottom: 10px;"),
               fluidRow(
                 column(6,
                        selectInput(ns("data_refresh_interval"), "Auto-Refresh Interval:",
                                    choices = list(
                                      "Manual only" = "manual",
                                      "Every hour" = "hourly",
                                      "Every 6 hours" = "6hourly",
                                      "Daily" = "daily"
                                    ),
                                    selected = "manual")
                 ),
                 column(6,
                        dateInput(ns("data_last_update"), "Last Update:",
                                  value = Sys.Date())
                 )
               ),
               
               h5("Data Quality Settings"),
               fluidRow(
                 column(6,
                        checkboxInput(ns("enable_data_validation"), "Enable Data Validation", value = TRUE)
                 ),
                 column(6,
                        checkboxInput(ns("exclude_incomplete_records"), "Exclude Incomplete Records", value = FALSE)
                 )
               ),
               
               fluidRow(
                 column(6,
                        numericInput(ns("min_record_completeness"), "Min Record Completeness (%):",
                                     value = 80, min = 50, max = 100, step = 5)
                 ),
                 column(6,
                        numericInput(ns("data_retention_days"), "Data Retention (days):",
                                     value = 730, min = 30, max = 3650, step = 30)
                 )
               ),
               
               div(style = "text-align: center; margin-top: 20px;",
                   actionButton(ns("validate_data"), "Validate Data", 
                                class = "btn-success", icon = icon("check-circle")),
                   actionButton(ns("clear_cache"), "Clear Cache", 
                                class = "btn-danger", icon = icon("trash"))
               )
            )
      )
    ),
    
    # Dashboard Preferences
    fluidRow(
      column(6,
             box(
               title = "Dashboard Preferences",
               status = "info",
               solidHeader = TRUE,
               width = NULL,
               
               h5("Display Settings"),
               fluidRow(
                 column(6,
                        selectInput(ns("dashboard_theme"), "Dashboard Theme:",
                                    choices = list(
                                      "Blue (Default)" = "blue",
                                      "Green" = "green",
                                      "Purple" = "purple",
                                      "Red" = "red",
                                      "Yellow" = "yellow",
                                      "Black" = "black"
                                    ),
                                    selected = "blue")
                 ),
                 column(6,
                        selectInput(ns("date_format"), "Date Format:",
                                    choices = list(
                                      "YYYY-MM-DD" = "%Y-%m-%d",
                                      "DD/MM/YYYY" = "%d/%m/%Y",
                                      "MM/DD/YYYY" = "%m/%d/%Y",
                                      "DD-MMM-YYYY" = "%d-%b-%Y"
                                    ),
                                    selected = "%Y-%m-%d")
                 )
               ),
               
               fluidRow(
                 column(6,
                        numericInput(ns("default_plot_height"), "Default Plot Height (px):",
                                     value = 400, min = 300, max = 800, step = 50)
                 ),
                 column(6,
                        selectInput(ns("number_format"), "Number Format:",
                                    choices = list(
                                      "1,234" = "comma",
                                      "1 234" = "space",
                                      "1234" = "none"
                                    ),
                                    selected = "comma")
                 )
               ),
               
               h5("Default Filters"),
               fluidRow(
                 column(6,
                        numericInput(ns("default_date_range_days"), "Default Date Range (days):",
                                     value = 90, min = 7, max = 365, step = 7)
                 ),
                 column(6,
                        selectInput(ns("default_aggregation"), "Default Time Aggregation:",
                                    choices = list(
                                      "Daily" = "daily",
                                      "Weekly" = "weekly",
                                      "Monthly" = "monthly"
                                    ),
                                    selected = "weekly")
                 )
               ),
               
               div(style = "text-align: center; margin-top: 20px;",
                   actionButton(ns("save_preferences"), "Save Preferences", 
                                class = "btn-primary", icon = icon("save")),
                   actionButton(ns("reset_preferences"), "Reset to Defaults", 
                                class = "btn-secondary", icon = icon("undo"))
               )
             )
      ),
      
      column(6,
             box(
               title = "Export & Notifications",
               status = "success",
               solidHeader = TRUE,
               width = NULL,
               
               h5("Export Settings"),
               fluidRow(
                 column(6,
                        selectInput(ns("default_export_format"), "Default Export Format:",
                                    choices = list(
                                      "CSV" = "csv",
                                      "Excel" = "xlsx",
                                      "PDF Report" = "pdf"
                                    ),
                                    selected = "csv")
                 ),
                 column(6,
                        checkboxInput(ns("include_metadata"), "Include Metadata in Exports", value = TRUE)
                 )
               ),
               
               h5("Notification Settings"),
               fluidRow(
                 column(6,
                        checkboxInput(ns("enable_alert_notifications"), "Enable Alert Notifications", value = TRUE)
                 ),
                 column(6,
                        checkboxInput(ns("enable_email_reports"), "Enable Email Reports", value = FALSE)
                 )
               ),
               
               conditionalPanel(
                 condition = "input.enable_email_reports",
                 ns = ns,
                 textInput(ns("notification_email"), "Notification Email:",
                           placeholder = "user@organization.org")
               ),
               
               fluidRow(
                 column(6,
                        selectInput(ns("alert_threshold"), "Alert Threshold:",
                                    choices = list(
                                      "Low (More alerts)" = "low",
                                      "Medium" = "medium",
                                      "High (Fewer alerts)" = "high"
                                    ),
                                    selected = "medium")
                 ),
                 column(6,
                        selectInput(ns("report_frequency"), "Report Frequency:",
                                    choices = list(
                                      "Daily" = "daily",
                                      "Weekly" = "weekly",
                                      "Monthly" = "monthly"
                                    ),
                                    selected = "weekly")
                 )
               ),
               
               div(style = "text-align: center; margin-top: 20px;",
                   actionButton(ns("test_notifications"), "Test Notification", 
                                class = "btn-info", icon = icon("bell")),
                   actionButton(ns("save_notification_settings"), "Save Settings", 
                                class = "btn-success", icon = icon("save"))
               )
             )
      )
    ),
    
    # System Information
    fluidRow(
      column(12,
             box(
               title = "System Information",
               status = "info",
               solidHeader = TRUE,
               width = NULL,
               collapsible = TRUE,
               collapsed = TRUE,
               
               fluidRow(
                 column(6,
                        h5("Application Info"),
                        verbatimTextOutput(ns("app_info"))
                 ),
                 column(6,
                        h5("Session Info"),
                        verbatimTextOutput(ns("session_info"))
                 )
               ),
               
               h5("Data Statistics"),
               tableOutput(ns("data_statistics")),
               
               h5("Module Status"),
               tableOutput(ns("module_status")),
               
               div(style = "text-align: center; margin-top: 20px;",
                   actionButton(ns("refresh_system_info"), "Refresh System Info", 
                                class = "btn-secondary", icon = icon("sync")),
                   downloadButton(ns("download_system_report"), "Download System Report", 
                                  class = "btn-primary", icon = icon("download"))
               )
             )
      )
    )
  )
}

#' Settings Module Server
#'
#' @param id Character string. The module's ID
#' @param data Reactive. Current dataset for statistics
#' @param config List. Application configuration
#'
#' @return List of reactive settings
mod_settings_server <- function(id, data = NULL, config = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for settings
    settings <- reactiveValues(
      ai_config = list(
        status = "Ready for Demo",
        mode = "Demonstration Mode",
        model = "Demo Assistant (Local)",
        response_style = "professional",
        max_response_length = 500,
        enable_insights = TRUE,
        enable_alerts = FALSE
      ),
      data_config = list(
        source = "Demo Data",
        refresh_interval = "manual",
        last_update = Sys.Date(),
        validation_enabled = TRUE,
        exclude_incomplete = FALSE,
        min_completeness = 80,
        retention_days = 730
      ),
      dashboard_prefs = list(
        theme = "blue",
        date_format = "%Y-%m-%d",
        plot_height = 400,
        number_format = "comma",
        date_range_days = 90,
        aggregation = "weekly"
      ),
      notifications = list(
        alert_notifications = TRUE,
        email_reports = FALSE,
        email = "",
        alert_threshold = "medium",
        report_frequency = "weekly"
      ),
      reload_signal = NULL
    )

    # Initialize synthetic toggle from config/env if available
    observe({
      cfg <- try(get("app_cfg", envir = .GlobalEnv), silent = TRUE)
      by_cfg <- try(isTRUE(cfg$testing$use_synthetic_data), silent = TRUE)
      by_env <- tolower(Sys.getenv("USE_SYNTHETIC_DATA", "false")) %in% c("1","true","yes")
      current <- isTRUE(by_cfg) || isTRUE(by_env)
      updateCheckboxInput(session, "use_synthetic", value = current)
      settings$data_config$source <- if (current) "Synthetic Data" else (settings$data_config$source %||% "Configured Source")
    })
    
    # AI Status outputs
    output$ai_status <- renderText({
      settings$ai_config$status
    })
    
    output$ai_mode <- renderText({
      settings$ai_config$mode
    })
    
    output$ai_model <- renderText({
      settings$ai_config$model
    })
    
    # Data source information
    output$data_source_info <- renderText({
      paste(
        "Source:", settings$data_config$source,
        "\nLast Updated:", format(settings$data_config$last_update, settings$dashboard_prefs$date_format),
        "\nRefresh Mode:", str_to_title(settings$data_config$refresh_interval),
        "\nValidation:", ifelse(settings$data_config$validation_enabled, "Enabled", "Disabled")
      )
    })
    
    # Application info
    output$app_info <- renderText({
      app_meta <- if (exists("app_meta")) app_meta else list(title = "IMC Epi Dashboard", version = "dev")
      
      paste(
        "Application:", app_meta$title %||% "IMC Epi Dashboard",
        "\nVersion:", app_meta$version %||% "dev", 
        "\nR Version:", R.version.string,
        "\nShiny Version:", packageVersion("shiny"),
        "\nPlatform:", Sys.info()["sysname"]
      )
    })
    
    # Session info
    output$session_info <- renderText({
      paste(
        "Session ID:", session$token,
        "\nUser Agent:", session$clientData$user_agent %||% "Unknown",
        "\nURL:", session$clientData$url_hostname %||% "localhost",
        "\nScreen Resolution:", paste0(
          session$clientData$pixelratio %||% 1, "x",
          session$clientData$output_pixelratio %||% 1
        ),
        "\nTimezone:", Sys.timezone()
      )
    })
    
    # Data statistics
    output$data_statistics <- renderTable({
      if (is.null(data) || is.null(data())) {
        return(data.frame(
          Metric = "No data available",
          Value = ""
        ))
      }
      
      df <- data()
      
      # Calculate statistics
      stats <- tibble(
        Metric = c(
          "Total Records",
          "Date Range", 
          "Unique Facilities",
          "Disease Categories",
          "Geographic Regions",
          "Data Completeness",
          "Last Record Date"
        ),
        Value = c(
          format(nrow(df), big.mark = ","),
          if ("datevisit" %in% names(df)) {
            date_range <- range(df$datevisit, na.rm = TRUE)
            paste(format(date_range[1], settings$dashboard_prefs$date_format), "to", 
                  format(date_range[2], settings$dashboard_prefs$date_format))
          } else { "Not available" },
          if ("orgunit" %in% names(df)) n_distinct(df$orgunit, na.rm = TRUE) else 0,
          if ("category_canonical_disease_imc" %in% names(df)) n_distinct(df$category_canonical_disease_imc, na.rm = TRUE) else 0,
          if ("admin1" %in% names(df)) n_distinct(df$admin1, na.rm = TRUE) else 0,
          paste0(round(sum(complete.cases(df)) / nrow(df) * 100, 1), "%"),
          if ("datevisit" %in% names(df)) {
            format(max(df$datevisit, na.rm = TRUE), settings$dashboard_prefs$date_format)
          } else { "Not available" }
        )
      )
      
      stats
    }, striped = TRUE, hover = TRUE)
    
    # Module status
    output$module_status <- renderTable({
      modules <- tibble(
        Module = c(
          "Summary Cards",
          "EpiBot Assistant", 
          "Geographic Analysis",
          "Disease Surveillance",
          "Time Trends",
          "Settings"
        ),
        Status = c(
          "✅ Active",
          ifelse(exists("ai_flags") && ai_flags$enabled, "✅ Active", "⚠️ Demo Mode"),
          "✅ Active",
          "✅ Active", 
          "✅ Active",
          "✅ Active"
        ),
        Features = c(
          "Value boxes, metrics",
          "Chat, analysis, insights",
          "Maps, regional analysis",
          "Alerts, epidemic monitoring",
          "Time series, seasonality",
          "Configuration, preferences"
        )
      )
      
      modules
    }, striped = TRUE, hover = TRUE)
    
    # Event handlers for AI configuration
    observeEvent(input$test_ai_connection, {
      showNotification("AI connection test: Demo mode active", type = "info", duration = 3)
    })
    
    observeEvent(input$reset_ai_session, {
      settings$ai_config$status <- "Reset - Ready for Demo"
      showNotification("AI session reset successfully", type = "success", duration = 3)
    })
    
    # Event handlers for data configuration
    observeEvent(input$validate_data, {
      if (is.null(data) || is.null(data())) {
        showNotification("No data available to validate", type = "warning", duration = 3)
      } else {
        df <- data()
        completeness <- round(sum(complete.cases(df)) / nrow(df) * 100, 1)
        showNotification(
          paste("Data validation complete. Completeness:", completeness, "%"), 
          type = if (completeness >= 80) "success" else "warning", 
          duration = 4
        )
      }
    })
    
    observeEvent(input$clear_cache, {
      # In a real app, this would clear actual cache
      showNotification("Cache cleared successfully", type = "success", duration = 3)
    })
    
    # Event handlers for preferences
    observeEvent(input$save_preferences, {
      # Update settings
      settings$dashboard_prefs$theme <- input$dashboard_theme
      settings$dashboard_prefs$date_format <- input$date_format
      settings$dashboard_prefs$plot_height <- input$default_plot_height
      settings$dashboard_prefs$number_format <- input$number_format
      settings$dashboard_prefs$date_range_days <- input$default_date_range_days
      settings$dashboard_prefs$aggregation <- input$default_aggregation
      
      showNotification("Preferences saved successfully", type = "success", duration = 3)
    })
    
    observeEvent(input$reset_preferences, {
      # Reset to defaults
      updateSelectInput(session, "dashboard_theme", selected = "blue")
      updateSelectInput(session, "date_format", selected = "%Y-%m-%d")
      updateNumericInput(session, "default_plot_height", value = 400)
      updateSelectInput(session, "number_format", selected = "comma")
      updateNumericInput(session, "default_date_range_days", value = 90)
      updateSelectInput(session, "default_aggregation", selected = "weekly")
      
      showNotification("Preferences reset to defaults", type = "info", duration = 3)
    })
    
    # Event handlers for notifications
    observeEvent(input$test_notifications, {
      showNotification("Test notification sent successfully", type = "info", duration = 3)
    })

    # Apply data source change and request reload
    observeEvent(input$use_synthetic, {
      if (isTRUE(input$use_synthetic)) {
        Sys.setenv(USE_SYNTHETIC_DATA = "true")
        settings$data_config$source <- "Synthetic Data"
        showNotification("Synthetic data enabled — reload to apply.", type = "warning", duration = 4)
      } else {
        Sys.unsetenv("USE_SYNTHETIC_DATA")
        settings$data_config$source <- "Configured Source"
        showNotification("Synthetic data disabled — reload to apply.", type = "message", duration = 4)
      }
      # Emit reload signal
      settings$reload_signal <- Sys.time()
    }, ignoreInit = TRUE)
    
    observeEvent(input$save_notification_settings, {
      # Update notification settings
      settings$notifications$alert_notifications <- input$enable_alert_notifications
      settings$notifications$email_reports <- input$enable_email_reports
      settings$notifications$email <- input$notification_email
      settings$notifications$alert_threshold <- input$alert_threshold
      settings$notifications$report_frequency <- input$report_frequency
      
      showNotification("Notification settings saved", type = "success", duration = 3)
    })
    
    # Refresh system info
    observeEvent(input$refresh_system_info, {
      settings$data_config$last_update <- Sys.Date()
      showNotification("System information refreshed", type = "info", duration = 2)
    })
    
    # Download system report
    output$download_system_report <- downloadHandler(
      filename = function() {
        paste0("system_report_", Sys.Date(), ".txt")
      },
      content = function(file) {
        # Generate comprehensive system report
        app_meta <- if (exists("app_meta")) app_meta else list(title = "IMC Epi Dashboard", version = "dev")
        
        report_content <- paste0(
          "=== SYSTEM REPORT ===\n",
          "Generated: ", Sys.time(), "\n\n",
          
          "=== APPLICATION INFO ===\n",
          "Name: ", app_meta$title %||% "IMC Epi Dashboard", "\n",
          "Version: ", app_meta$version %||% "dev", "\n",
          "R Version: ", R.version.string, "\n",
          "Platform: ", Sys.info()["sysname"], "\n",
          "Working Directory: ", getwd(), "\n\n",
          
          "=== SESSION INFO ===\n",
          "Session ID: ", session$token, "\n",
          "Timezone: ", Sys.timezone(), "\n",
          "Locale: ", Sys.getlocale(), "\n\n",
          
          "=== CURRENT SETTINGS ===\n",
          "AI Status: ", settings$ai_config$status, "\n",
          "Data Source: ", settings$data_config$source, "\n",
          "Dashboard Theme: ", settings$dashboard_prefs$theme, "\n",
          "Date Format: ", settings$dashboard_prefs$date_format, "\n",
          "Default Plot Height: ", settings$dashboard_prefs$plot_height, "px\n",
          "Alert Notifications: ", ifelse(settings$notifications$alert_notifications, "Enabled", "Disabled"), "\n\n"
        )
        
        # Add data statistics if available
        if (!is.null(data) && !is.null(data())) {
          df <- data()
          report_content <- paste0(report_content,
            "=== DATA STATISTICS ===\n",
            "Total Records: ", format(nrow(df), big.mark = ","), "\n",
            "Columns: ", ncol(df), "\n",
            "Completeness: ", round(sum(complete.cases(df)) / nrow(df) * 100, 1), "%\n"
          )
          
          if ("datevisit" %in% names(df)) {
            date_range <- range(df$datevisit, na.rm = TRUE)
            report_content <- paste0(report_content,
              "Date Range: ", format(date_range[1]), " to ", format(date_range[2]), "\n"
            )
          }
          
          if ("orgunit" %in% names(df)) {
            report_content <- paste0(report_content,
              "Unique Facilities: ", n_distinct(df$orgunit, na.rm = TRUE), "\n"
            )
          }
          
          if ("admin1" %in% names(df)) {
            report_content <- paste0(report_content,
              "Geographic Regions: ", n_distinct(df$admin1, na.rm = TRUE), "\n"
            )
          }
        }
        
        report_content <- paste0(report_content, "\n=== END REPORT ===")
        
        writeLines(report_content, file)
      }
    )
    
    # Return settings for use by other modules
    return(list(
      ai_config = reactive(settings$ai_config),
      data_config = reactive(settings$data_config),
      dashboard_prefs = reactive(settings$dashboard_prefs),
      notifications = reactive(settings$notifications),
      request_reload = reactive(settings$reload_signal)
    ))
  })
}
