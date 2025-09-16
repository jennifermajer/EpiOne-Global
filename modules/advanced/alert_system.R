# modules/advanced/alert_system.R - Dashboard module

# ─────────────────────────────────────────────────────────────────────────────
# Supporting functions
# ─────────────────────────────────────────────────────────────────────────────
#' Main alert generation function
#' @param health_data Data frame with health consultation data
#' @param climate_data Data frame with climate data (optional)
#' @param alert_config List with alert configuration parameters
#' @return List with generated alerts
generate_alerts <- function(health_data, climate_data = NULL, alert_config) {
  
  current_time <- Sys.time()
  all_alerts <- list()
  
  # Generate epidemiological alerts
  if ("outbreak" %in% alert_config$alert_types) {
    outbreak_alerts <- generate_outbreak_alerts(
      health_data, 
      alert_config$outbreak_config
    )
    all_alerts <- append(all_alerts, outbreak_alerts)
  }
  
  # Generate climate-based alerts
  if ("climate" %in% alert_config$alert_types && !is.null(climate_data)) {
    climate_alerts <- generate_climate_alerts(
      health_data,
      climate_data,
      alert_config$climate_config
    )
    all_alerts <- append(all_alerts, climate_alerts)
  }
  
  # Generate capacity alerts
  if ("capacity" %in% alert_config$alert_types) {
    capacity_alerts <- generate_capacity_alerts(
      health_data,
      alert_config$capacity_config
    )
    all_alerts <- append(all_alerts, capacity_alerts)
  }
  
  # Consolidate and prioritize alerts
  consolidated_alerts <- consolidate_alerts(all_alerts)
  
  # Add timestamps and metadata
  final_alerts <- consolidated_alerts %>%
    mutate(
      generated_at = current_time,
      alert_id = paste0("ALT_", format(current_time, "%Y%m%d_%H%M%S"), "_", row_number()),
      status = "active"
    )
  
  return(final_alerts)
}

#' Generate disease outbreak alerts
#' @param health_data Data frame with health consultation data
#' @param config List with outbreak alert configuration
#' @return Data frame with outbreak alerts
generate_outbreak_alerts <- function(health_data, config) {
  
  # Calculate baseline rates for each disease and geographic area
  baseline_data <- calculate_baseline_rates(
    health_data, 
    diseases = config$monitored_diseases,
    baseline_weeks = config$baseline_weeks,
    geographic_level = config$geographic_level
  )
  
  # Calculate recent case counts
  alert_window_start <- Sys.Date() - days(config$alert_window)
  recent_data <- health_data %>%
    filter(
      datevisit >= alert_window_start,
      category_canonical_disease_imc %in% config$monitored_diseases
    ) %>%
    count(!!sym(config$geographic_level), category_canonical_disease_imc, name = "recent_cases")
  
  # Identify alerts
  outbreak_alerts <- baseline_data %>%
    left_join(recent_data, by = c(config$geographic_level, "category_canonical_disease_imc")) %>%
    mutate(
      recent_cases = ifelse(is.na(recent_cases), 0, recent_cases),
      alert_threshold = baseline_mean * config$outbreak_multiplier,
      alert_triggered = recent_cases >= alert_threshold,
      excess_cases = recent_cases - baseline_mean,
      relative_increase = (recent_cases / baseline_mean) - 1
    ) %>%
    filter(alert_triggered) %>%
    mutate(
      alert_type = "outbreak",
      priority = case_when(
        relative_increase >= 3 ~ "critical",
        relative_increase >= 1.5 ~ "high", 
        relative_increase >= 0.5 ~ "medium",
        TRUE ~ "low"
      ),
      message = paste0(
        "Potential outbreak of ", morbidity,
        " in ", !!sym(config$geographic_level),
        ". Current cases: ", recent_cases,
        " (", round(relative_increase * 100, 1), "% above baseline)"
      ),
      recommendation = generate_outbreak_recommendation(relative_increase, morbidity)
    )
  
  return(outbreak_alerts)
}

#' Calculate baseline disease rates
calculate_baseline_rates <- function(health_data, diseases, baseline_weeks, geographic_level) {
  
  baseline_start <- Sys.Date() - weeks(baseline_weeks + 4)  # Extra buffer
  baseline_end <- Sys.Date() - weeks(4)  # Exclude recent weeks
  
  baseline_data <- health_data %>%
    filter(
      datevisit >= baseline_start,
      datevisit <= baseline_end,
      category_canonical_disease_imc %in% diseases
    ) %>%
    mutate(week = floor_date(datevisit, "week")) %>%
    count(!!sym(geographic_level), category_canonical_disease_imc, week) %>%
    group_by(!!sym(geographic_level), category_canonical_disease_imc) %>%
    summarise(
      baseline_mean = mean(n, na.rm = TRUE),
      baseline_sd = sd(n, na.rm = TRUE),
      baseline_weeks_data = n(),
      .groups = "drop"
    ) %>%
    mutate(baseline_sd = ifelse(is.na(baseline_sd) | baseline_sd == 0, 1, baseline_sd))
  
  return(baseline_data)
}

#' Generate climate-based health alerts
generate_climate_alerts <- function(health_data, climate_data, config) {
  
  current_climate <- climate_data %>%
    slice_tail(n = 1)  # Most recent climate data
  
  climate_alerts <- tibble()
  
  # Temperature alerts
  if (current_climate$temperature_mean >= config$temperature_threshold) {
    temp_alert <- tibble(
      alert_type = "climate_temperature",
      priority = if(current_climate$temperature_mean >= config$temperature_threshold + 5) "high" else "medium",
      location = "Region-wide",
      disease = "Heat-related conditions",
      current_value = current_climate$temperature_mean,
      threshold = config$temperature_threshold,
      message = paste0(
        "High temperature alert: ", round(current_climate$temperature_mean, 1), 
        "°C (threshold: ", config$temperature_threshold, "°C)"
      ),
      recommendation = "Monitor for heat-related illnesses, ensure adequate hydration resources"
    )
    climate_alerts <- bind_rows(climate_alerts, temp_alert)
  }
  
  # Precipitation alerts (flood risk)
  if (!is.na(current_climate$precipitation) && current_climate$precipitation >= config$precipitation_threshold) {
    precip_alert <- tibble(
      alert_type = "climate_precipitation",
      priority = if(current_climate$precipitation >= config$precipitation_threshold * 2) "high" else "medium",
      location = "Region-wide", 
      disease = "Water-borne diseases",
      current_value = current_climate$precipitation,
      threshold = config$precipitation_threshold,
      message = paste0(
        "Heavy rainfall alert: ", round(current_climate$precipitation, 1),
        "mm (threshold: ", config$precipitation_threshold, "mm)"
      ),
      recommendation = "Monitor for waterborne diseases, ensure water quality testing"
    )
    climate_alerts <- bind_rows(climate_alerts, precip_alert)
  }
  
  # Climate-disease correlation alerts
  if (length(config$climate_diseases) > 0) {
    correlation_alerts <- generate_climate_correlation_alerts(
      health_data, climate_data, config
    )
    climate_alerts <- bind_rows(climate_alerts, correlation_alerts)
  }
  
  return(climate_alerts)
}

#' Generate capacity-based alerts
generate_capacity_alerts <- function(health_data, config) {
  
  # Calculate recent utilization
  recent_week <- health_data %>%
    filter(datevisit >= Sys.Date() - days(7)) %>%
    count(orgunit) %>%
    summarise(
      total_consultations = sum(n),
      facilities_reporting = n(),
      avg_per_facility = mean(n)
    )
  
  capacity_alerts <- tibble()
  
  # High utilization alert
  if (recent_week$avg_per_facility >= config$high_utilization_threshold) {
    capacity_alert <- tibble(
      alert_type = "capacity_high",
      priority = if(recent_week$avg_per_facility >= config$high_utilization_threshold * 1.5) "high" else "medium",
      location = "System-wide",
      disease = "All conditions",
      current_value = recent_week$avg_per_facility,
      threshold = config$high_utilization_threshold,
      message = paste0(
        "High facility utilization: ", round(recent_week$avg_per_facility, 1),
        " consultations per facility (threshold: ", config$high_utilization_threshold, ")"
      ),
      recommendation = "Consider capacity expansion or patient load redistribution"
    )
    capacity_alerts <- bind_rows(capacity_alerts, capacity_alert)
  }
  
  return(capacity_alerts)
}

#' Consolidate and deduplicate alerts
consolidate_alerts <- function(alert_list) {
  
  if (length(alert_list) == 0) {
    return(tibble(
      alert_id = character(),
      alert_type = character(),
      priority = character(),
      location = character(),
      disease = character(),
      message = character(),
      recommendation = character(),
      generated_at = as.POSIXct(character())
    ))
  }
  
  # Combine all alerts
  all_alerts <- bind_rows(alert_list)
  
  # Remove duplicates based on type, location, and disease
  consolidated <- all_alerts %>%
    group_by(alert_type, location, disease) %>%
    slice_max(order_by = case_when(
      priority == "critical" ~ 4,
      priority == "high" ~ 3,
      priority == "medium" ~ 2,
      TRUE ~ 1
    ), n = 1) %>%
    ungroup() %>%
    arrange(
      case_when(
        priority == "critical" ~ 4,
        priority == "high" ~ 3,
        priority == "medium" ~ 2,
        TRUE ~ 1
      ) %>% desc(),
      alert_type
    )
  
  return(consolidated)
}

#' Generate outbreak recommendations
generate_outbreak_recommendation <- function(relative_increase, disease) {
  
  base_recommendations <- list(
    "Acute Watery Diarrhea" = "Implement cholera response protocols, water quality testing",
    "Measles" = "Activate vaccination campaign, case isolation measures",
    "Acute Respiratory Infections" = "Enhance respiratory precautions, contact tracing",
    "Meningitis" = "Immediate case investigation, prophylaxis for contacts"
  )
  
  urgency_modifier <- case_when(
    relative_increase >= 3 ~ "URGENT: ",
    relative_increase >= 1.5 ~ "PRIORITY: ",
    TRUE ~ ""
  )
  
  base_rec <- base_recommendations[[disease]] %||% "Investigate cases, implement control measures"
  
  return(paste0(urgency_modifier, base_rec))
}

#' Send automated notifications
#' @param alerts Data frame with alerts to send
#' @param notification_config List with notification settings
send_alert_notifications <- function(alerts, notification_config) {
  
  if (nrow(alerts) == 0) return(invisible())
  
  # Filter alerts by priority
  alerts_to_send <- alerts %>%
    filter(priority %in% notification_config$include_priorities)
  
  if (nrow(alerts_to_send) == 0) return(invisible())
  
  # Group by frequency setting
  if (notification_config$frequency == "immediate") {
    # Send each alert immediately
    pwalk(alerts_to_send, send_individual_alert, notification_config)
  } else {
    # Store for batch sending
    store_alerts_for_batch(alerts_to_send, notification_config)
  }
}

#' Send individual alert notification
send_individual_alert <- function(alert_id, alert_type, priority, location, disease, message, recommendation, ..., notification_config) {
  
  # Prepare email content
  email_body <- glue::glue(
    notification_config$email_template,
    alert_type = alert_type,
    location = location,
    disease = disease,
    current_cases = "N/A",  # Would need to pass this through
    threshold = "N/A",      # Would need to pass this through
    recommendation = recommendation
  )
  
  subject <- paste0("[", toupper(priority), " ALERT] ", disease, " - ", location)
  
  # Send email (implementation depends on your email system)
  tryCatch({
    # Example using sendmailR (you may need to configure your email system)
    # sendmailR::sendmail(
    #   from = "alerts@imc.org",
    #   to = strsplit(notification_config$email_recipients, ";\\s*")[[1]],
    #   subject = subject,
    #   msg = email_body
    # )
    
    # Log the alert instead (for demonstration)
    log_info("ALERT SENT: {subject}")
    log_info("Recipients: {notification_config$email_recipients}")
    log_info("Message: {email_body}")
    
  }, error = function(e) {
    log_error("Failed to send alert notification: {e$message}")
  })
}

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────
#' Automated Alert System Module UI
alert_system_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             box(
               title = "Automated Alert Management System",
               status = "danger",
               solidHeader = TRUE,
               width = NULL,
               
               tabsetPanel(
                 # Alert Configuration Tab
                 tabPanel(
                   "Alert Configuration",
                   br(),
                   fluidRow(
                     column(6,
                            box(
                              title = "Epidemiological Alerts",
                              status = "warning",
                              width = NULL,
                              
                              h5("Disease Outbreak Thresholds"),
                              fluidRow(
                                column(6,
                                       selectInput(ns("outbreak_diseases"), "Monitor Diseases:",
                                                   choices = NULL,
                                                   multiple = TRUE)
                                ),
                                column(6,
                                       numericInput(ns("outbreak_multiplier"), "Alert Threshold (x baseline):",
                                                    value = 2.0, min = 1.1, max = 5.0, step = 0.1)
                                )
                              ),
                              
                              h5("Surveillance Parameters"),
                              fluidRow(
                                column(6,
                                       numericInput(ns("baseline_weeks"), "Baseline Period (weeks):",
                                                    value = 8, min = 4, max = 26)
                                ),
                                column(6,
                                       numericInput(ns("alert_window"), "Alert Window (days):",
                                                    value = 7, min = 1, max = 30)
                                )
                              ),
                              
                              h5("Geographic Scope"),
                              selectInput(ns("alert_geographic_level"), "Geographic Level:",
                                          choices = list(
                                            "Facility Level" = "facility",
                                            "District Level" = "district", 
                                            "Governorate Level" = "governorate",
                                            "National Level" = "national"
                                          )),
                              
                              actionButton(ns("save_epi_config"), "Save Configuration",
                                           class = "btn-warning")
                            )
                     ),
                     
                     column(6,
                            box(
                              title = "Climate-Based Alerts",
                              status = "info",
                              width = NULL,
                              
                              h5("Weather Thresholds"),
                              fluidRow(
                                column(6,
                                       numericInput(ns("temp_alert_threshold"), "Temperature Alert (°C):",
                                                    value = 35, min = 25, max = 50)
                                ),
                                column(6,
                                       numericInput(ns("precip_alert_threshold"), "Rainfall Alert (mm/day):",
                                                    value = 50, min = 10, max = 200)
                                )
                              ),
                              
                              fluidRow(
                                column(6,
                                       numericInput(ns("humidity_alert_threshold"), "Humidity Alert (%):",
                                                    value = 85, min = 60, max = 100)
                                ),
                                column(6,
                                       numericInput(ns("wind_alert_threshold"), "Wind Speed Alert (km/h):",
                                                    value = 50, min = 20, max = 150)
                                )
                              ),
                              
                              h5("Climate-Health Triggers"),
                              selectInput(ns("climate_diseases"), "Climate-Sensitive Diseases:",
                                          choices = NULL,
                                          multiple = TRUE),
                              
                              numericInput(ns("climate_correlation_threshold"), "Correlation Threshold:",
                                           value = 0.3, min = 0.1, max = 0.9, step = 0.1),
                              
                              actionButton(ns("save_climate_config"), "Save Configuration",
                                           class = "btn-info")
                            )
                     )
                   )
                 ),
                 
                 # Active Alerts Tab
                 tabPanel(
                   "Active Alerts",
                   br(),
                   fluidRow(
                     column(3,
                            valueBoxOutput(ns("total_active_alerts"), width = NULL)
                     ),
                     column(3,
                            valueBoxOutput(ns("high_priority_alerts"), width = NULL)
                     ),
                     column(3,
                            valueBoxOutput(ns("climate_alerts"), width = NULL)
                     ),
                     column(3,
                            valueBoxOutput(ns("outbreak_alerts"), width = NULL)
                     )
                   ),
                   
                   fluidRow(
                     column(12,
                            box(
                              title = "Current Alert Status",
                              status = "danger",
                              width = NULL,
                              
                              DT::dataTableOutput(ns("active_alerts_table"))
                            )
                     )
                   ),
                   
                   fluidRow(
                     column(6,
                            box(
                              title = "Alert Timeline",
                              width = NULL,
                              plotlyOutput(ns("alert_timeline"), height = "300px")
                            )
                     ),
                     column(6,
                            box(
                              title = "Alert Geographic Distribution",
                              width = NULL,
                              plotlyOutput(ns("alert_map"), height = "300px")
                            )
                     )
                   )
                 ),
                 
                 # Notification Settings Tab
                 tabPanel(
                   "Notification Settings",
                   br(),
                   fluidRow(
                     column(6,
                            box(
                              title = "Stakeholder Notifications",
                              status = "success",
                              width = NULL,
                              
                              h5("Email Notifications"),
                              textAreaInput(ns("email_recipients"), "Email Recipients:",
                                            value = "epi@imc.org; manager@imc.org",
                                            placeholder = "Enter email addresses separated by semicolons"),
                              
                              h5("Notification Frequency"),
                              selectInput(ns("notification_frequency"), "Send Alerts:",
                                          choices = list(
                                            "Immediately" = "immediate",
                                            "Hourly Digest" = "hourly",
                                            "Daily Digest" = "daily",
                                            "Weekly Summary" = "weekly"
                                          )),
                              
                              h5("Alert Priorities to Include"),
                              checkboxGroupInput(ns("notification_priorities"), "Include:",
                                                 choices = list(
                                                   "Critical Alerts" = "critical",
                                                   "High Priority" = "high",
                                                   "Medium Priority" = "medium",
                                                   "Low Priority" = "low"
                                                 ),
                                                 selected = c("critical", "high")),
                              
                              actionButton(ns("save_notification_config"), "Save Settings",
                                           class = "btn-success")
                            )
                     ),
                     
                     column(6,
                            box(
                              title = "Alert Templates",
                              status = "primary",
                              width = NULL,
                              
                              h5("Email Template"),
                              textAreaInput(ns("email_template"), "Email Body Template:",
                                            value = "ALERT: {alert_type} detected in {location}. 
Disease: {disease}
Current cases: {current_cases}
Threshold: {threshold}
Recommendation: {recommendation}",
                                            rows = 8),
                              
                              h5("SMS Template (if enabled)"),
                              textAreaInput(ns("sms_template"), "SMS Template:",
                                            value = "HEALTH ALERT: {disease} outbreak in {location}. Cases: {current_cases}. Contact epidemiologist.",
                                            rows = 3),
                              
                              actionButton(ns("test_notification"), "Send Test Alert",
                                           class = "btn-primary")
                            )
                     )
                   )
                 ),
                 
                 # Alert History Tab
                 tabPanel(
                   "Alert History",
                   br(),
                   fluidRow(
                     column(4,
                            wellPanel(
                              h5("Filter Options"),
                              dateRangeInput(ns("history_date_range"), "Date Range:",
                                             start = Sys.Date() - 30,
                                             end = Sys.Date()),
                              selectInput(ns("history_alert_type"), "Alert Type:",
                                          choices = list(
                                            "All" = "all",
                                            "Disease Outbreak" = "outbreak",
                                            "Climate Alert" = "climate",
                                            "Capacity Alert" = "capacity"
                                          )),
                              selectInput(ns("history_priority"), "Priority Level:",
                                          choices = list(
                                            "All" = "all",
                                            "Critical" = "critical",
                                            "High" = "high",
                                            "Medium" = "medium",
                                            "Low" = "low"
                                          )),
                              actionButton(ns("filter_history"), "Apply Filters",
                                           class = "btn-secondary")
                            )
                     ),
                     column(8,
                            plotlyOutput(ns("alert_history_trend"), height = "400px")
                     )
                   ),
                   
                   fluidRow(
                     column(12,
                            box(
                              title = "Alert History Details",
                              width = NULL,
                              DT::dataTableOutput(ns("alert_history_table"))
                            )
                     )
                   )
                 )
               )
             )
      )
    )
  )
}