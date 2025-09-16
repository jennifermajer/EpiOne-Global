# modules/mod_disease_surveillance.R
# Disease Surveillance Module - Updated to use disease_categories_taxaware.R for taxonomy

# Required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(plotly)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(purrr)
  library(DT)
  library(shinyWidgets)
})

# Source disease_categories_taxaware functions - UPDATED to use new taxonomy system
tryCatch({
  # Try different possible paths for the new taxonomy file
  possible_paths <- c(
    "R/disease_categories_taxaware.R",           # New taxonomy-aware file
    "../R/disease_categories_taxaware.R",       # If modules is a subfolder
    "disease_categories_taxaware.R",            # Same directory
    file.path("R", "disease_categories_taxaware.R"), # Explicit file.path
    here::here("R", "disease_categories_taxaware.R") # Using here package
  )
  
  sourced <- FALSE
  for (path in possible_paths) {
    if (file.exists(path)) {
      source(path)
      cat("Successfully sourced disease_categories_taxaware.R from:", path, "\n")
      sourced <- TRUE
      break
    }
  }
  
  if (!sourced) {
    warning("Could not find disease_categories_taxaware.R in any of the expected locations: ", 
         paste(possible_paths, collapse = ", "))
  }
}, error = function(e) {
  warning("Failed to source disease_categories_taxaware.R: ", e$message)
  cat("Current working directory:", getwd(), "\n")
  cat("Files in current directory:", paste(list.files(), collapse = ", "), "\n")
  cat("Files in R directory (if exists):", 
      if (dir.exists("R")) paste(list.files("R"), collapse = ", ") else "R directory not found", 
      "\n")
  
  # Set sourced to false to use fallback
  sourced <- FALSE
})

# Fallback function definitions if taxonomy system is not available
if (!exists("sourced") || !sourced || !exists("get_epidemic_groups")) {
  cat("Loading fallback taxonomy functions...\n")
  
  # Essential function definitions (minimal versions)
  get_epidemic_groups <- function() {
    list(
      "Vaccine_Preventable" = c("Measles", "Suspected Measles", "Pneumonia", "Influenza Like Illness"),
      "Water_Food_Borne" = c("Acute Watery Diarrhoea (Suspected Cholera)", "Typhoid"),
      "Vector_Borne" = c("Malaria"),
      "Respiratory_Epidemic" = c("Influenza Like Illness", "COVID-19 suspected case", "SARI", "Tuberculosis")
    )
  }
  
  get_syndromic_groups <- function() {
    list(
      "Diarrheal_Syndrome" = c("Acute Watery Diarrhea", "Acute bloody diarrhea"),
      "Respiratory_Syndrome" = c("Pneumonia", "SARI", "Influenza Like Illness"),
      "Fever_Syndrome" = c("Fever of unknown origin", "Malaria (Suspected)"),
      "Rash_Illness" = c("Suspected Measles", "Measles (Suspected)"),
      "Neurological_Syndrome" = c("Suspected Meningitis", "Acute Flaccid Paralysis (Suspected Polio)")
    )
  }
  
  epidemic_alert_level <- function(category_name, disease_display, recent_cases) {
    recent_cases <- as.numeric(recent_cases)
    if (is.na(recent_cases)) return("None")
    
    if (recent_cases >= 15) return("High")
    if (recent_cases >= 8) return("Medium") 
    if (recent_cases >= 3) return("Low")
    return("None")
  }
  
  # Additional helper functions if not available from taxonomy system
  if (!exists("tibble")) {
    tibble <- function(...) {
      data.frame(..., stringsAsFactors = FALSE)
    }
  }
  
  cat("Fallback taxonomy functions loaded successfully.\n")
}

# Define null coalescer operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

#' Disease Surveillance Module UI
#'
#' @param id Character string. The module's ID
#' @param height Character string. Plot height (default: "500px")
#'
#' @return Shiny UI elements
mod_disease_surveillance_ui <- function(id, height = "500px") {
  ns <- NS(id)
  
  tagList(
    # Alert status cards
    fluidRow(
      column(3, infoBoxOutput(ns("cholera_alerts"), width = NULL)),
      column(3, infoBoxOutput(ns("measles_alerts"), width = NULL)),
      column(3, infoBoxOutput(ns("ari_alerts"), width = NULL)),
      column(3, infoBoxOutput(ns("afp_alerts"), width = NULL))
    ),
    
    # Main surveillance dashboard
    fluidRow(
      column(8,
             box(
               title = "Disease Surveillance Dashboard",
               status = "danger",
               solidHeader = TRUE,
               width = NULL,
               
               tabsetPanel(
                 id = ns("surveillance_tabs"),
                 
                 tabPanel(
                   "Epidemic Trends",
                   br(),
                   plotlyOutput(ns("epidemic_trends"), height = height),
                   br(),
                   h5("Trend Analysis Controls"),
                   fluidRow(
                     column(4,
                            selectInput(
                              ns("trend_period"),
                              "Time Period:",
                              choices = list(
                                "Last 30 days" = 30,
                                "Last 90 days" = 90,
                                "Last 6 months" = 180,
                                "Last year" = 365
                              ),
                              selected = 90
                            )
                     ),
                     column(4,
                            pickerInput(
                              ns("selected_epidemic_categories"),
                              "Select Categories:",
                              choices = NULL,
                              multiple = TRUE,
                              options = pickerOptions(
                                actionsBox = TRUE,
                                maxOptions = 4,
                                title = "Choose epidemic categories"
                              )
                            )
                     ),
                     column(4,
                            pickerInput(
                              ns("selected_epidemic_diseases"),
                              "Select Specific Diseases:",
                              choices = NULL,
                              multiple = TRUE,
                              options = pickerOptions(
                                actionsBox = TRUE,
                                maxOptions = 8,
                                title = "Choose specific diseases"
                              )
                            )
                     )
                   )
                 ),
                 
                 tabPanel(
                   "Outbreak Detection",
                   br(),
                   plotlyOutput(ns("outbreak_detection"), height = height),
                   br(),
                   h5("Outbreak Detection Controls"),
                   fluidRow(
                     column(6,
                            selectInput(
                              ns("outbreak_disease_selection"),
                              "Select Disease for Outbreak Detection:",
                              choices = NULL,
                              selected = NULL
                            )
                     ),
                     column(6,
                            selectInput(
                              ns("outbreak_threshold_method"),
                              "Detection Method:",
                              choices = list(
                                "Moving Average + 2SD" = "ma_2sd",
                                "Moving Average + 1.5SD" = "ma_1.5sd",
                                "75th Percentile Threshold" = "percentile_75",
                                "90th Percentile Threshold" = "percentile_90"
                              ),
                              selected = "ma_2sd"
                            )
                     )
                   ),
                   br(),
                   h5("Statistical Alerts"),
                   DT::dataTableOutput(ns("outbreak_alerts"))
                 ),
                 
                 tabPanel(
                   "Syndromic Surveillance",
                   br(),
                   plotlyOutput(ns("syndromic_trends"), height = height),
                   br(),
                   h5("Syndrome Definitions"),
                   verbatimTextOutput(ns("syndrome_definitions"))
                 )
               )
             )
      ),
      
      column(4,
             # Alert summary
             box(
               title = "Current Alerts",
               status = "warning",
               solidHeader = TRUE,
               width = NULL,
               height = "300px",
               DT::dataTableOutput(ns("alert_table"))
             ),
             
             # Weekly summary
             box(
               title = "Weekly Summary",
               status = "info",
               solidHeader = TRUE,
               width = NULL,
               height = "250px",
               tableOutput(ns("weekly_summary"))
             )
      )
    ),
    
    # Geographic hotspots
    fluidRow(
      column(12,
             box(
               title = "Geographic Disease Hotspots",
               status = "primary",
               solidHeader = TRUE,
               width = NULL,
               plotlyOutput(ns("geographic_hotspots"), height = "400px")
             )
      )
    )
  )
}

#' Disease Surveillance Module Server
#'
#' @param id Character string. The module's ID
#' @param data Reactive. Filtered dataset
#' @param config List. Configuration parameters
#'
#' @return Surveillance data reactive
mod_disease_surveillance_server <- function(id, data, config = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Use epidemic groups from taxonomy system
    epidemic_diseases <- reactive({
      get_epidemic_groups()
    })
    
    # Use syndromic groups from taxonomy system
    syndromic_groups <- reactive({
      get_syndromic_groups()
    })
    
    # Process data - ensure we have required columns
    processed_data <- reactive({
      req(data())
      
      df <- data()
      
      # Ensure we have canonical_disease_imc column (created by preprocessing)
      if (!"canonical_disease_imc" %in% names(df) && "morbidity" %in% names(df)) {
        # Fallback: use morbidity as canonical_disease_imc
        df$canonical_disease_imc <- df$morbidity
      }
      
      # Ensure we have epidemic detection columns - updated for flexible taxonomy
      # The taxonomy system creates 'epidemic_prone', so use that as standard
      if (!"epidemic_prone" %in% names(df)) {
        if ("is_epidemic_prone" %in% names(df)) {
          df$epidemic_prone <- df$is_epidemic_prone
        } else if ("is_epidemic" %in% names(df)) {
          df$epidemic_prone <- df$is_epidemic
        } else if ("canonical_disease_imc" %in% names(df)) {
          epidemic_list <- unlist(epidemic_diseases(), use.names = FALSE)
          df$epidemic_prone <- df$canonical_disease_imc %in% epidemic_list
        } else if ("morbidity" %in% names(df)) {
          epidemic_list <- unlist(epidemic_diseases(), use.names = FALSE)
          df$epidemic_prone <- df$morbidity %in% epidemic_list
        } else {
          df$epidemic_prone <- FALSE
        }
      }
      
      # Create alias for compatibility
      if (!"is_epidemic_prone" %in% names(df)) {
        df$is_epidemic_prone <- df$epidemic_prone
      }
      
      return(df)
    })
    
    # Get available epidemic categories and diseases in data
    available_epidemic_data <- reactive({
      req(processed_data())
      
      df <- processed_data()
      if (!"canonical_disease_imc" %in% colnames(df)) return(list(categories = NULL, diseases = NULL))
      
      epidemic_list <- epidemic_diseases()
      
      # Check which categories have cases
      available_categories <- names(epidemic_list)[
        sapply(epidemic_list, function(diseases) {
          any(diseases %in% unique(df$canonical_disease_imc))
        })
      ]
      
      # Get available epidemic diseases in data - prioritize morbidity column
      all_epidemic_diseases <- unlist(epidemic_list, use.names = FALSE)
      disease_col <- if ("morbidity" %in% colnames(df)) "morbidity" 
                     else if ("canonical_disease_imc" %in% colnames(df)) "canonical_disease_imc" 
                     else NULL
      
      available_diseases <- if (!is.null(disease_col)) {
        intersect(all_epidemic_diseases, unique(df[[disease_col]]))
      } else {
        character(0)
      }
      
      list(
        categories = available_categories,
        diseases = available_diseases,
        category_disease_map = epidemic_list
      )
    })
    
    # Update category choices
    observe({
      req(available_epidemic_data())
      
      available_data <- available_epidemic_data()
      
      # Pretty category names for display
      category_pretty <- c(
        Vaccine_Preventable = "Vaccine Preventable",
        Water_Food_Borne = "Water & Food-borne",
        Vector_Borne = "Vector-borne",
        Respiratory_Epidemic = "Respiratory Epidemic"
      )
      
      category_choices <- setNames(
        available_data$categories,
        category_pretty[available_data$categories]
      )
      
      updatePickerInput(
        session, "selected_epidemic_categories",
        choices = category_choices,
        selected = head(available_data$categories, 2)
      )
    })
    
    # Update outbreak disease choices based on available epidemic diseases
    observe({
      req(available_epidemic_data())
      
      available_data <- available_epidemic_data()
      
      # Get all available epidemic diseases for outbreak detection
      outbreak_choices <- available_data$diseases
      
      # Add a default "All AWD/Cholera-like" option
      outbreak_choices_with_default <- c(
        "Acute Watery Diarrhea (Suspected Cholera)" = "Acute Watery Diarrhea (Suspected Cholera)",
        outbreak_choices
      )
      
      updateSelectInput(
        session, "outbreak_disease_selection",
        choices = outbreak_choices_with_default,
        selected = "Acute Watery Diarrhea (Suspected Cholera)"
      )
    })
    
    # Update disease choices based on selected categories
    observe({
      req(available_epidemic_data(), input$selected_epidemic_categories)
      
      available_data <- available_epidemic_data()
      
      if (length(input$selected_epidemic_categories) > 0) {
        # Get diseases from selected categories
        category_diseases <- unlist(
          available_data$category_disease_map[input$selected_epidemic_categories],
          use.names = FALSE
        )
        
        # Filter to diseases actually in the data
        available_diseases <- intersect(category_diseases, available_data$diseases)
      } else {
        # Show all available epidemic diseases if no categories selected
        available_diseases <- available_data$diseases
      }
      
      updatePickerInput(
        session, "selected_epidemic_diseases",
        choices = available_diseases,
        selected = head(available_diseases, 6)
      )
    })
    
    # Calculate surveillance metrics using functions from disease_categories.R
    surveillance_data <- reactive({
      req(processed_data())
      
      df <- processed_data()
      date_col <- if ("datevisit" %in% colnames(df)) "datevisit" 
      else if ("datevisitnew" %in% colnames(df)) "datevisitnew" 
      else NULL
      
      if (is.null(date_col)) {
        return(tibble(Disease = character(), `Total Cases` = integer(), 
                      `Past Week` = integer(), `Alert Level` = character()))
      }
      
      current_date <- max(df[[date_col]], na.rm = TRUE)
      
      # Filter by selected time period
      if (!is.null(input$trend_period)) {
        days_back <- as.numeric(input$trend_period)
        df <- df %>%
          filter(.data[[date_col]] >= current_date - lubridate::days(days_back))
      }
      
      # Get epidemic diseases from standardized function
      epidemic_list <- epidemic_diseases()
      
      # Calculate metrics for each epidemic disease category
      surveillance_summary <- purrr::map_dfr(names(epidemic_list), function(disease_name) {
        disease_conditions <- epidemic_list[[disease_name]]
        
        # Filter using standardized disease names
        disease_data <- df %>%
          filter(canonical_disease_imc %in% disease_conditions)
        
        if (nrow(disease_data) == 0) {
          return(tibble(
            Disease = disease_name,
            `Total Cases` = 0,
            `Past Week` = 0,
            `Past Month` = 0,
            `Alert Level` = "None"
          ))
        }
        
        # Calculate time-based metrics
        past_week <- sum(disease_data[[date_col]] >= current_date - 7, na.rm = TRUE)
        past_month <- sum(disease_data[[date_col]] >= current_date - 30, na.rm = TRUE)
        total_cases <- nrow(disease_data)
        
        # Use alert level function from disease_categories.R
        alert_level <- epidemic_alert_level(
          category_name = disease_name,
          disease_display = disease_name,
          recent_cases = past_week
        )
        
        tibble(
          Disease = disease_name,
          `Total Cases` = total_cases,
          `Past Week` = past_week,
          `Past Month` = past_month,
          `Alert Level` = alert_level
        )
      }) %>%
        # Only show diseases with cases
        filter(`Total Cases` > 0) %>%
        arrange(desc(`Past Week`))
      
      return(surveillance_summary)
    })
    
    # Alert info boxes using standardized disease mapping
    output$cholera_alerts <- renderInfoBox({
      req(surveillance_data())
      
      # Map to standardized category name from disease_categories.R
      cholera_cases <- surveillance_data() %>%
        filter(Disease == "Water_Food_Borne") %>%
        pull(`Past Week`)
      
      if (length(cholera_cases) == 0) cholera_cases <- 0
      
      alert_color <- if (cholera_cases >= 10) "red" else if (cholera_cases >= 5) "yellow" else "green"
      
      infoBox(
        "Cholera/AWD",
        paste(cholera_cases, "cases"),
        icon = icon("tint"),
        color = alert_color,
        subtitle = "Past 7 days"
      )
    })
    
    output$measles_alerts <- renderInfoBox({
      req(surveillance_data())
      
      # Use standardized vaccine preventable category
      measles_cases <- surveillance_data() %>%
        filter(Disease == "Vaccine_Preventable") %>%
        pull(`Past Week`)
      
      if (length(measles_cases) == 0) measles_cases <- 0
      
      alert_color <- if (measles_cases >= 3) "red" else if (measles_cases >= 1) "yellow" else "green"
      
      infoBox(
        "Measles/VPD",
        paste(measles_cases, "cases"),
        icon = icon("user-injured"),
        color = alert_color,
        subtitle = "Past 7 days"
      )
    })
    
    output$ari_alerts <- renderInfoBox({
      req(surveillance_data())
      
      # Use respiratory epidemic category
      ari_cases <- surveillance_data() %>%
        filter(Disease == "Respiratory_Epidemic") %>%
        pull(`Past Week`)
      
      if (length(ari_cases) == 0) ari_cases <- 0
      
      alert_color <- if (ari_cases >= 50) "red" else if (ari_cases >= 25) "yellow" else "green"
      
      infoBox(
        "ARI/Pneumonia",
        paste(ari_cases, "cases"),
        icon = icon("lungs"),
        color = alert_color,
        subtitle = "Past 7 days"
      )
    })
    
    output$afp_alerts <- renderInfoBox({
      req(processed_data())
      
      # Check for AFP specifically using standardized disease labels
      df_categorized <- processed_data()
      date_col <- if ("datevisit" %in% colnames(df_categorized)) "datevisit" 
      else if ("datevisitnew" %in% colnames(df_categorized)) "datevisitnew" 
      else NULL
      
      if (!is.null(date_col)) {
        current_date <- max(df_categorized[[date_col]], na.rm = TRUE)
        
        afp_cases <- df_categorized %>%
          filter(
            canonical_disease_imc == "Acute Flaccid Paralysis (Suspected Polio)",
            .data[[date_col]] >= current_date - 7
          ) %>%
          nrow()
      } else {
        afp_cases <- 0
      }
      
      alert_color <- if (afp_cases >= 1) "red" else "green"
      
      infoBox(
        "AFP/Polio",
        paste(afp_cases, "cases"),
        icon = icon("wheelchair"),
        color = alert_color,
        subtitle = "Past 7 days"
      )
    })
    
    # Epidemic trends plot using standardized functions
    output$epidemic_trends <- renderPlotly({
      req(processed_data())
      
      df <- processed_data()
      date_col <- if ("datevisit" %in% colnames(df)) "datevisit" 
      else if ("datevisitnew" %in% colnames(df)) "datevisitnew" 
      else NULL
      
      # Check for required columns with flexible disease column handling
      disease_col <- if ("morbidity" %in% colnames(df)) "morbidity" 
                     else if ("canonical_disease_imc" %in% colnames(df)) "canonical_disease_imc" 
                     else NULL
      
      if (is.null(date_col) || is.null(disease_col)) {
        return(plot_ly() %>%
                 add_annotations(
                   text = "Required surveillance data not available",
                   x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                   showarrow = FALSE
                 ))
      }
      
      # Determine what to show based on user selection - updated for flexible disease column
      if (!is.null(input$selected_epidemic_diseases) && length(input$selected_epidemic_diseases) > 0) {
        # Show specific diseases
        selected_items <- input$selected_epidemic_diseases
        group_by_var <- disease_col
        filter_condition <- df[[disease_col]] %in% selected_items
        legend_title <- "Disease"
      } else if (!is.null(input$selected_epidemic_categories) && length(input$selected_epidemic_categories) > 0) {
        # Show categories
        epidemic_list <- epidemic_diseases()
        category_disease_map <- purrr::map_dfr(names(epidemic_list), function(cat_name) {
          tibble(
            disease = epidemic_list[[cat_name]],
            category = cat_name
          )
        })
        names(category_disease_map)[1] <- disease_col
        
        df <- df %>%
          left_join(category_disease_map, by = disease_col, relationship = "many-to-many")
        
        selected_items <- input$selected_epidemic_categories
        group_by_var <- "category"
        filter_condition <- df$category %in% selected_items
        legend_title <- "Category"
      } else {
        return(plot_ly() %>%
                 add_annotations(
                   text = "Please select categories or diseases to display",
                   x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                   showarrow = FALSE
                 ))
      }
      
      # Filter and aggregate data
      epidemic_data <- df %>%
        filter(filter_condition) %>%
        mutate(date_week = floor_date(.data[[date_col]], "week")) %>%
        group_by(date_week, .data[[group_by_var]]) %>%
        summarise(n = n(), .groups = "drop") %>%
        arrange(date_week)
      
      if (nrow(epidemic_data) == 0) {
        return(plot_ly() %>%
                 add_annotations(
                   text = "No data found for selected items",
                   x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                   showarrow = FALSE
                 ))
      }
      
      # Create plot
      p <- ggplot(epidemic_data, aes(x = date_week, y = n, fill = .data[[group_by_var]])) +
        geom_area(alpha = 0.7, position = "stack") +
        scale_fill_brewer(type = "qual", palette = "Set1") +
        labs(
          title = "Epidemic Disease Surveillance Trends",
          subtitle = "Based on standardized disease definitions from disease_categories.R",
          x = "Week",
          y = "Number of Cases",
          fill = legend_title
        ) +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma_format()) +
        scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
        theme(legend.position = "bottom")
      
      ggplotly(p, tooltip = c("fill", "x", "y")) %>%
        layout(
          hovermode = "x unified",
          legend = list(orientation = "h", x = 0, y = -0.2)
        )
    })
    
    # Outbreak detection plot using standardized disease definitions with user selection
    output$outbreak_detection <- renderPlotly({
      req(processed_data(), input$outbreak_disease_selection, input$outbreak_threshold_method)
      
      df <- processed_data()
      date_col <- if ("datevisit" %in% colnames(df)) "datevisit" 
      else if ("datevisitnew" %in% colnames(df)) "datevisitnew" 
      else NULL
      
      if (is.null(date_col) || !"canonical_disease_imc" %in% colnames(df)) {
        return(plot_ly() %>%
                 add_annotations(
                   text = "Required data not available for outbreak detection",
                   x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                   showarrow = FALSE
                 ))
      }
      
      # Check for required columns with flexible disease column handling
      disease_col <- if ("morbidity" %in% colnames(df)) "morbidity" 
                     else if ("canonical_disease_imc" %in% colnames(df)) "canonical_disease_imc" 
                     else NULL
      
      if (is.null(disease_col)) {
        return(plot_ly() %>%
                 add_annotations(
                   text = "Disease column not available for outbreak detection",
                   x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                   showarrow = FALSE
                 ))
      }
      
      # Use selected disease for outbreak detection
      selected_disease <- input$outbreak_disease_selection
      detection_method <- input$outbreak_threshold_method
      
      # Filter data for selected disease using flexible disease column
      disease_data <- df %>%
        filter(.data[[disease_col]] == selected_disease) %>%
        mutate(date_week = floor_date(.data[[date_col]], "week")) %>%
        count(date_week) %>%
        arrange(date_week)
      
      if (nrow(disease_data) == 0) {
        return(plot_ly() %>%
                 add_annotations(
                   text = paste("No cases found for", selected_disease),
                   x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                   showarrow = FALSE
                 ))
      }
      
      # Apply selected detection method
      if (detection_method == "ma_2sd") {
        disease_data <- disease_data %>%
          mutate(
            ma_4week = zoo::rollmean(n, k = 4, fill = NA, align = "right"),
            sd_4week = zoo::rollapply(n, width = 4, FUN = sd, fill = NA, align = "right"),
            upper_limit = ifelse(!is.na(ma_4week) & !is.na(sd_4week), 
                                 ma_4week + 2 * sd_4week, NA),
            alert = n > upper_limit & !is.na(upper_limit)
          )
        method_description <- "4-week moving average + 2 standard deviations"
        
      } else if (detection_method == "ma_1.5sd") {
        disease_data <- disease_data %>%
          mutate(
            ma_4week = zoo::rollmean(n, k = 4, fill = NA, align = "right"),
            sd_4week = zoo::rollapply(n, width = 4, FUN = sd, fill = NA, align = "right"),
            upper_limit = ifelse(!is.na(ma_4week) & !is.na(sd_4week), 
                                 ma_4week + 1.5 * sd_4week, NA),
            alert = n > upper_limit & !is.na(upper_limit)
          )
        method_description <- "4-week moving average + 1.5 standard deviations"
        
      } else if (detection_method == "percentile_75") {
        threshold_75 <- quantile(disease_data$n, 0.75, na.rm = TRUE)
        disease_data <- disease_data %>%
          mutate(
            ma_4week = zoo::rollmean(n, k = 4, fill = NA, align = "right"),
            upper_limit = threshold_75,
            alert = n > upper_limit
          )
        method_description <- "75th percentile threshold"
        
      } else if (detection_method == "percentile_90") {
        threshold_90 <- quantile(disease_data$n, 0.90, na.rm = TRUE)
        disease_data <- disease_data %>%
          mutate(
            ma_4week = zoo::rollmean(n, k = 4, fill = NA, align = "right"),
            upper_limit = threshold_90,
            alert = n > upper_limit
          )
        method_description <- "90th percentile threshold"
      }
      
      # Create plot
      p <- ggplot(disease_data, aes(x = date_week)) +
        geom_ribbon(aes(ymin = 0, ymax = pmax(0, upper_limit, na.rm = TRUE)), 
                    fill = "red", alpha = 0.2, na.rm = TRUE) +
        geom_line(aes(y = ma_4week), color = "blue", size = 1, na.rm = TRUE) +
        geom_point(aes(y = n), color = "black", alpha = 0.7) +
        geom_point(data = filter(disease_data, alert), aes(y = n), 
                   color = "red", size = 3, alpha = 0.8) +
        labs(
          title = paste("Outbreak Detection:", selected_disease),
          subtitle = paste("Method:", method_description, "- Red area = outbreak threshold, Blue line = 4-week average"),
          x = "Week",
          y = "Weekly Cases"
        ) +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma_format())
      
      ggplotly(p, tooltip = c("x", "y"))
    })
    
    # Syndromic surveillance using get_syndromic_groups()
    output$syndromic_trends <- renderPlotly({
      req(processed_data())
      
      df <- processed_data()
      date_col <- if ("datevisit" %in% colnames(df)) "datevisit" 
      else if ("datevisitnew" %in% colnames(df)) "datevisitnew" 
      else NULL
      
      if (is.null(date_col) || !"morbidity" %in% colnames(df)) {
        return(plot_ly() %>%
                 add_annotations(
                   text = "Syndromic surveillance data not available",
                   x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                   showarrow = FALSE
                 ))
      }
      
      # Use syndromic groups from disease_categories.R
      syndromic_groups_list <- syndromic_groups()
      
      syndromic_data <- purrr::map_dfr(names(syndromic_groups_list), function(syndrome) {
        conditions <- syndromic_groups_list[[syndrome]]
        
        df %>%
          filter(morbidity %in% conditions) %>%
          mutate(date_week = floor_date(.data[[date_col]], "week")) %>%
          count(date_week) %>%
          mutate(syndrome = syndrome)
      })
      
      if (nrow(syndromic_data) == 0) {
        return(plot_ly() %>%
                 add_annotations(
                   text = "No syndromic surveillance data available",
                   x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                   showarrow = FALSE
                 ))
      }
      
      p <- ggplot(syndromic_data, aes(x = date_week, y = n, color = syndrome)) +
        geom_line(size = 1.2, alpha = 0.8) +
        geom_point(size = 2) +
        scale_color_brewer(type = "qual", palette = "Dark2") +
        labs(
          title = "Syndromic Surveillance Trends",
          subtitle = "Using standardized syndrome definitions from disease_categories.R",
          x = "Week",
          y = "Number of Cases",
          color = "Syndrome Group"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      ggplotly(p, tooltip = c("colour", "x", "y"))
    })
    
    # Alert table
    output$alert_table <- DT::renderDataTable({
      req(surveillance_data())
      
      alert_data <- surveillance_data() %>%
        filter(`Alert Level` %in% c("High", "Medium")) %>%
        select(Disease, `Past Week`, `Alert Level`) %>%
        arrange(desc(`Past Week`))
      
      if (nrow(alert_data) == 0) {
        return(data.frame(Message = "No current alerts"))
      }
      
      DT::datatable(
        alert_data,
        options = list(
          pageLength = 5,
          dom = 't',
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Alert Level",
          backgroundColor = DT::styleEqual(
            c("High", "Medium"),
            c("lightcoral", "orange")
          )
        )
    })
    
    # Weekly summary
    output$weekly_summary <- renderTable({
      req(surveillance_data())
      
      summary_data <- surveillance_data() %>%
        summarise(
          `Epidemic Diseases Monitored` = n(),
          `Active Alerts` = sum(`Alert Level` %in% c("High", "Medium")),
          `Cases This Week` = sum(`Past Week`),
          `Cases This Month` = sum(`Past Month`)
        ) %>%
        pivot_longer(everything(), names_to = "Metric", values_to = "Value")
      
      summary_data
    }, striped = TRUE, hover = TRUE)
    
    # Geographic hotspots using standardized epidemic diseases
    output$geographic_hotspots <- renderPlotly({
      req(processed_data())
      
      df <- processed_data()
      
      if (!all(c("admin1", "admin2", "canonical_disease_imc") %in% colnames(df))) {
        return(plot_ly() %>%
                 add_annotations(
                   text = "Geographic data not available",
                   x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                   showarrow = FALSE
                 ))
      }
      
      # Use epidemic categorization from disease_categories.R
      hotspot_data <- df %>%
        filter(is_epidemic_prone == TRUE) %>%
        group_by(admin1, admin2) %>%
        summarise(
          epidemic_cases = n(),
          .groups = "drop"
        ) %>%
        arrange(desc(epidemic_cases)) %>%
        slice_head(n = 15)
      
      if (nrow(hotspot_data) == 0) {
        return(plot_ly() %>%
                 add_annotations(
                   text = "No epidemic hotspots detected",
                   x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                   showarrow = FALSE
                 ))
      }
      
      p <- ggplot(hotspot_data, aes(x = reorder(admin2, epidemic_cases), y = epidemic_cases, fill = admin1)) +
        geom_col() +
        coord_flip() +
        scale_fill_brewer(type = "qual", palette = "Set2") +
        labs(
          title = "Top 15 Epidemic Disease Hotspots",
          subtitle = "Using standardized epidemic disease definitions from disease_categories.R",
          x = "District",
          y = "Epidemic Cases",
          fill = "Governorate"
        ) +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma_format()) +
        theme(legend.position = "bottom")
      
      ggplotly(p, tooltip = c("fill", "y")) %>%
        layout(
          legend = list(orientation = "h", x = 0, y = -0.2),
          margin = list(l = 120)
        )
    })
    
    # Outbreak alerts table using standardized functions with disease selection
    output$outbreak_alerts <- DT::renderDataTable({
      req(processed_data(), input$outbreak_disease_selection)
      
      df <- processed_data()
      date_col <- if ("datevisit" %in% colnames(df)) "datevisit" 
      else if ("datevisitnew" %in% colnames(df)) "datevisitnew" 
      else NULL
      
      if (is.null(date_col)) {
        return(data.frame(Message = "Date information required for outbreak alerts"))
      }
      
      # Statistical alerts based on recent increases in selected disease
      current_date <- max(df[[date_col]], na.rm = TRUE)
      selected_disease <- input$outbreak_disease_selection
      
      # Get epidemic disease categories from disease_categories.R
      epidemic_list <- epidemic_diseases()
      
      # Define disease column for filtering (used in both branches)
      disease_col_for_filter <- if ("morbidity" %in% colnames(df)) "morbidity" 
                                else if ("canonical_disease_imc" %in% colnames(df)) "canonical_disease_imc" 
                                else NULL
      
      if (is.null(disease_col_for_filter)) {
        return(data.frame(Message = "Disease information required for outbreak alerts"))
      }
      
      # If specific disease selected, focus on that; otherwise show all categories
      if (!is.null(selected_disease) && selected_disease != "") {
        # Single disease analysis using flexible disease column
        
        recent_cases <- df %>%
          filter(.data[[disease_col_for_filter]] == selected_disease) %>%
          mutate(
            period = case_when(
              .data[[date_col]] >= current_date - 7 ~ "This Week",
              .data[[date_col]] >= current_date - 14 ~ "Last Week",
              .data[[date_col]] >= current_date - 21 ~ "2 Weeks Ago",
              .data[[date_col]] >= current_date - 28 ~ "3 Weeks Ago",
              TRUE ~ "Earlier"
            )
          ) %>%
          count(period) %>%
          pivot_wider(names_from = period, values_from = n, values_fill = 0)
        
        this_week <- recent_cases$`This Week` %||% 0
        last_week <- recent_cases$`Last Week` %||% 0
        two_weeks_ago <- recent_cases$`2 Weeks Ago` %||% 0
        three_weeks_ago <- recent_cases$`3 Weeks Ago` %||% 0
        
        # Calculate various trend indicators
        week_change <- if (last_week > 0) round((this_week - last_week) / last_week * 100, 1) else 0
        two_week_trend <- if (two_weeks_ago > 0) round((this_week - two_weeks_ago) / two_weeks_ago * 100, 1) else 0
        four_week_avg <- round((this_week + last_week + two_weeks_ago + three_weeks_ago) / 4, 1)
        
        # Determine alert status based on various criteria
        alert_reasons <- c()
        alert_level <- "Normal"
        
        if (this_week > four_week_avg + (2 * sd(c(this_week, last_week, two_weeks_ago, three_weeks_ago), na.rm = TRUE))) {
          alert_reasons <- c(alert_reasons, "Above 2SD threshold")
          alert_level <- "Statistical Alert"
        } else if (this_week > four_week_avg * 1.5) {
          alert_reasons <- c(alert_reasons, "50% above 4-week average")
          alert_level <- "Watch"
        }
        
        if (week_change >= 100) {
          alert_reasons <- c(alert_reasons, "100%+ weekly increase")
          alert_level <- "Statistical Alert"
        } else if (week_change >= 50) {
          alert_reasons <- c(alert_reasons, "50%+ weekly increase")
          if (alert_level == "Normal") alert_level <- "Watch"
        }
        
        if (two_week_trend >= 200) {
          alert_reasons <- c(alert_reasons, "200%+ two-week trend")
          alert_level <- "Statistical Alert"
        }
        
        # Use disease-specific thresholds from disease_categories.R
        category_alert <- epidemic_alert_level(
          category_name = "Disease_Specific",
          disease_display = selected_disease,
          recent_cases = this_week
        )
        
        if (category_alert == "High") {
          alert_reasons <- c(alert_reasons, "High case threshold exceeded")
          alert_level <- "Statistical Alert"
        } else if (category_alert == "Medium" && alert_level == "Normal") {
          alert_reasons <- c(alert_reasons, "Medium case threshold exceeded")
          alert_level <- "Watch"
        }
        
        alert_summary <- tibble(
          Disease = selected_disease,
          `This Week` = this_week,
          `Last Week` = last_week,
          `4-Week Avg` = four_week_avg,
          `Week Change (%)` = week_change,
          `2-Week Trend (%)` = two_week_trend,
          `Alert Level` = alert_level,
          `Alert Reasons` = if (length(alert_reasons) > 0) paste(alert_reasons, collapse = "; ") else "None"
        ) %>%
          filter(`Alert Level` != "Normal")
        
      } else {
        # Multiple category analysis (fallback to original logic)
        alert_summary <- purrr::map_dfr(names(epidemic_list), function(disease_name) {
          conditions <- epidemic_list[[disease_name]]
          
          recent_cases <- df %>%
            filter(.data[[disease_col_for_filter]] %in% conditions) %>%
            mutate(
              period = case_when(
                .data[[date_col]] >= current_date - 7 ~ "This Week",
                .data[[date_col]] >= current_date - 14 ~ "Last Week",
                TRUE ~ "Earlier"
              )
            ) %>%
            count(period) %>%
            pivot_wider(names_from = period, values_from = n, values_fill = 0)
          
          this_week <- recent_cases$`This Week` %||% 0
          last_week <- recent_cases$`Last Week` %||% 0
          
          change_pct <- if (last_week > 0) round((this_week - last_week) / last_week * 100, 1) else 0
          
          # Use alert level function from disease_categories.R
          alert_status <- epidemic_alert_level(
            category_name = disease_name,
            disease_display = disease_name,
            recent_cases = this_week
          )
          
          # Convert to watch/alert format
          alert_status <- case_when(
            alert_status == "High" ~ "Statistical Alert",
            alert_status == "Medium" & change_pct >= 50 ~ "Watch",
            TRUE ~ "Normal"
          )
          
          tibble(
            Disease = disease_name,
            `This Week` = this_week,
            `Last Week` = last_week,
            `4-Week Avg` = NA_real_,
            `Week Change (%)` = change_pct,
            `2-Week Trend (%)` = NA_real_,
            `Alert Level` = alert_status,
            `Alert Reasons` = if (alert_status != "Normal") "Category threshold exceeded" else "None"
          )
        }) %>%
          filter(`Alert Level` != "Normal")
      }
      
      if (nrow(alert_summary) == 0) {
        return(data.frame(Message = paste("No outbreak alerts detected for", 
                                          if (!is.null(selected_disease)) selected_disease else "selected diseases")))
      }
      
      # Create data table with appropriate columns
      display_cols <- if (!is.null(selected_disease) && selected_disease != "") {
        c("Disease", "This Week", "Last Week", "4-Week Avg", "Week Change (%)", "2-Week Trend (%)", "Alert Level", "Alert Reasons")
      } else {
        c("Disease", "This Week", "Last Week", "Week Change (%)", "Alert Level")
      }
      
      DT::datatable(
        alert_summary %>% select(all_of(display_cols)),
        options = list(
          pageLength = 10,
          dom = 't',
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Alert Level",
          backgroundColor = DT::styleEqual(
            c("Statistical Alert", "Watch"),
            c("lightcoral", "orange")
          )
        )
    })
    
    # Syndrome definitions using get_syndromic_groups()
    output$syndrome_definitions <- renderText({
      syndromic_groups_list <- syndromic_groups()
      
      paste0(
        "SYNDROMIC SURVEILLANCE DEFINITIONS (from disease_categories.R):\n\n",
        paste(
          names(syndromic_groups_list),
          ": ",
          sapply(syndromic_groups_list, function(x) paste(head(x, 3), collapse = ", ")),
          collapse = "\n\n"
        ),
        "\n\nNote: Syndromic surveillance monitors broader symptom patterns",
        "\nwhile epidemic surveillance focuses on WHO-defined outbreak-prone diseases.",
        "\nAll definitions are sourced from the centralized disease_categories.R file."
      )
    })
    
    # Return surveillance data for other modules
    return(surveillance_data)
  })
}