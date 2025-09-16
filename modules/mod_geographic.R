# modules/mod_geographic.R
# Geographic Analysis Module - Complete and Enhanced with Leaflet maps

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(scales)
  library(stringr)
  library(lubridate)
  library(leaflet)
  library(tidyr)
})

# Helper function to safely load shinyWidgets if available
load_shinywidgets <- function() {
  if (requireNamespace("shinyWidgets", quietly = TRUE)) {
    return(TRUE)
  } else {
    warning("shinyWidgets not available - using standard selectInput instead of pickerInput")
    return(FALSE)
  }
}

# %||% operator for safe null handling
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# --- INTEGRATED HELPER FUNCTIONS ---

#' Create enhanced geographic summary statistics
create_geographic_summary <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(data.frame())
  }
  
  # Check required columns
  if (!"admin1" %in% colnames(data)) {
    return(data.frame())
  }
  
  # Get epidemic diseases if function exists
  epidemic_conditions <- if (exists("get_epidemic_diseases_standardized")) {
    unlist(get_epidemic_diseases_standardized(), use.names = FALSE)
  } else {
    c("Acute Watery Diarrhea", "Suspected Cholera", "Suspected Measles", 
      "Pneumonia", "SARI", "Influenza Like Illness")
  }
  
  # Get malnutrition conditions
  malnut_conditions <- c("Complicated severe acute Malnutrition", "Moderate acute Malnutrition", 
                        "Severe Malnutrition", "Uncomplicated severe acute Malnutrition", "Underweight")
  
  # Create comprehensive summary
  summary_data <- data %>%
    group_by(admin1) %>%
    summarise(
      # Basic metrics
      total_consultations = n(),
      unique_facilities = if ("orgunit" %in% colnames(data)) n_distinct(orgunit, na.rm = TRUE) else 0,
      consultations_per_facility = if ("orgunit" %in% colnames(data)) {
        round(n() / pmax(n_distinct(orgunit, na.rm = TRUE), 1), 1)
      } else 0,
      
      # Disease diversity
      disease_diversity = if ("category_canonical_disease_imc" %in% colnames(data)) {
        n_distinct(category_canonical_disease_imc, na.rm = TRUE)
      } else if ("morbidity" %in% colnames(data)) {
        n_distinct(morbidity, na.rm = TRUE)
      } else 0,
      
      # Trauma analysis
      trauma_cases = if ("type_case" %in% colnames(data)) {
        sum(str_detect(tolower(type_case %||% ""), "trauma") & 
            !str_detect(tolower(type_case %||% ""), "non"), na.rm = TRUE)
      } else 0,
      trauma_percentage = if ("type_case" %in% colnames(data)) {
        round(sum(str_detect(tolower(type_case %||% ""), "trauma") & 
                 !str_detect(tolower(type_case %||% ""), "non"), na.rm = TRUE) / n() * 100, 1)
      } else 0,
      
      # Epidemic cases
      epidemic_cases = if ("morbidity" %in% colnames(data)) {
        sum(morbidity %in% epidemic_conditions, na.rm = TRUE)
      } else 0,
      
      # Malnutrition cases
      malnutrition_cases = if ("morbidity" %in% colnames(data)) {
        sum(morbidity %in% malnut_conditions, na.rm = TRUE)
      } else 0,
      
      # Demographics
      female_percentage = if ("sex" %in% colnames(data)) {
        round(sum(sex %in% c("Female", "F", "female"), na.rm = TRUE) / n() * 100, 1)
      } else 0,
      
      pediatric_percentage = if ("age_group_new" %in% colnames(data)) {
        round(sum(age_group_new %in% c("0-5 y", "<5", "0-4"), na.rm = TRUE) / n() * 100, 1)
      } else if ("age_group" %in% colnames(data)) {
        round(sum(age_group %in% c("0-5 y", "<5", "0-4"), na.rm = TRUE) / n() * 100, 1)
      } else 0,
      
      .groups = "drop"
    ) %>%
    arrange(desc(total_consultations))
  
  return(summary_data)
}

#' Create risk profile analysis by governorate
create_risk_profile_analysis <- function(data) {
  if (!"admin1" %in% colnames(data) || nrow(data) == 0) {
    return(tibble(admin1 = character(), risk_category = character(), 
                  cases = integer(), percentage = numeric()))
  }

  n_rows <- nrow(data)

  is_high_risk_age <- rep(FALSE, n_rows)
  if ("age_group_new" %in% colnames(data)) {
    is_high_risk_age <- is_high_risk_age | data$age_group_new %in% c("0-5 y", "60+ y")
  }
  if ("age_group" %in% colnames(data)) {
    is_high_risk_age <- is_high_risk_age | data$age_group %in% c("0-5 y", "60+ y")
  }

  is_displaced <- if ("resident" %in% colnames(data)) {
    data$resident %in% c("IDP", "Displaced", "IDP/Returnee")
  } else rep(FALSE, n_rows)

  is_trauma <- if ("type_case" %in% colnames(data)) {
    vals <- tolower(as.character(data$type_case))
    str_detect(vals, "trauma") & !str_detect(vals, "non")
  } else rep(FALSE, n_rows)

  chronic_conditions <- c(
    "Diabetes mellitus", "Hypertension", "Chronic obstructive pulmonary disease (COPD)",
    "Stroke/cerebrovascular accident", "Asthma", "Heart failure", "Cancer", 
    "Chronic renal Failure"
  )
  is_chronic <- if ("morbidity" %in% colnames(data)) {
    data$morbidity %in% chronic_conditions
  } else rep(FALSE, n_rows)

  epidemic_conditions <- c(
    "Acute Watery Diarrhea", "Suspected Cholera", "Suspected Measles", 
    "Pneumonia", "SARI"
  )
  is_epidemic <- if ("morbidity" %in% colnames(data)) {
    data$morbidity %in% epidemic_conditions
  } else rep(FALSE, n_rows)

  risk_factors <- data %>%
    mutate(
      risk_category = case_when(
        is_high_risk_age ~ "High Risk Age",
        is_displaced ~ "Displaced Population",
        is_trauma ~ "Trauma Risk",
        is_chronic ~ "Chronic Disease Risk",
        is_epidemic ~ "Epidemic Risk",
        TRUE ~ "Standard Risk"
      )
    ) %>%
    filter(!is.na(admin1)) %>%
    count(admin1, risk_category, name = "cases") %>%
    group_by(admin1) %>%
    mutate(percentage = round(cases / sum(cases) * 100, 1)) %>%
    ungroup()

  return(risk_factors)
}

# --- MODULE UI ---

#' Geographic Analysis Module UI
mod_geographic_ui <- function(id, height = "600px") {
  ns <- NS(id)
  
  tagList(
    # Value boxes for key geographic metrics
    fluidRow(
      column(3, valueBoxOutput(ns("geo_total_regions"), width = NULL)),
      column(3, valueBoxOutput(ns("geo_total_facilities"), width = NULL)),
      column(3, valueBoxOutput(ns("geo_avg_utilization"), width = NULL)),
      column(3, valueBoxOutput(ns("geo_trauma_rate"), width = NULL))
    ),
    
    # Enhanced controls section
    fluidRow(
      column(12,
             box(
               title = "Geographic Analysis Controls",
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = NULL,
               
               fluidRow(
                 column(3,
                        selectInput(
                          ns("map_metric"),
                          "Select Metric:",
                          choices = list(
                            "Total Consultations" = "consultations",
                            "Consultations per Facility" = "consultations_per_facility",
                            "Disease Diversity" = "disease_diversity",
                            "Trauma Rate (%)" = "trauma_rate",
                            "Epidemic Cases" = "epidemic_cases",
                            "Female Percentage" = "female_percentage"
                          ),
                          selected = "consultations"
                        )
                 ),
                 column(3,
                        selectInput(
                          ns("map_disease"),
                          "Filter by Disease:",
                          choices = NULL
                        )
                 ),
                 column(3,
                        selectInput(
                          ns("map_time_period"),
                          "Time Period:",
                          choices = list(
                            "Last 30 days" = 30,
                            "Last 90 days" = 90,  
                            "Last 6 months" = 180,
                            "Last year" = 365,
                            "All data" = 9999
                          ),
                          selected = 90
                        )
                 ),
                 column(3,
                        br(),
                        downloadButton(
                          ns("download_map_data"),
                          "Download Data",
                          icon = icon("download"),
                          class = "btn-success"
                        )
                 )
               )
             )
      )
    ),
    
    # Main mapping interface with enhanced tabs
    fluidRow(
      column(8,
             box(
               title = "Interactive Geographic Maps",
               status = "success",
               solidHeader = TRUE,
               width = NULL,
               
               tabsetPanel(
                 id = ns("map_tabs"),
                 
                 tabPanel(
                   "Overview Map",
                   br(),
                   leafletOutput(ns("overview_map"), height = height)
                 ),
                 
                 tabPanel(
                   "Detailed Map", 
                   br(),
                   leafletOutput(ns("detailed_map"), height = height)
                 ),
                 
                 tabPanel(
                   "Governorate Analysis",
                   br(),
                   plotlyOutput(ns("choropleth_map"), height = height)
                 ),
                 
                 tabPanel(
                   "Spatial Clustering",
                   br(),
                   plotlyOutput(ns("spatial_clustering_plot"), height = height)
                 )
               )
             )
      ),
      
      column(4,
             # Geographic insights panel
             box(
               title = "Geographic Insights",
               status = "info",
               solidHeader = TRUE,
               width = NULL,
               height = "350px",
               
               conditionalPanel(
                 condition = "output.geographic_insights_available",
                 ns = ns,
                 uiOutput(ns("geographic_insights_summary"))
               ),
               conditionalPanel(
                 condition = "!output.geographic_insights_available",
                 ns = ns,
                 div(class = "alert alert-info",
                     "Geographic insights will appear when data is loaded.")
               )
             ),
             
             # Governorate priority matrix
             box(
               title = "Governorate Priority Matrix",
               status = "warning",
               solidHeader = TRUE,
               width = NULL,
               height = "300px",
               
               div(style = "max-height: 250px; overflow-y: auto;",
                   tableOutput(ns("regional_priority_matrix"))
               )
             )
      )
    ),
    
    # Geographic Summary Table
    fluidRow(
      column(12,
             box(
               title = "Geographic Summary Statistics",
               status = "info",
               solidHeader = TRUE,
               width = NULL,
               collapsible = TRUE,
               DT::dataTableOutput(ns("geo_summary_table"))
             )
      )
    ),
    
    # Enhanced analysis panels
    fluidRow(
      column(6,
             box(
               title = "Disease Geography Analysis",
               status = "primary",
               solidHeader = TRUE,
               width = NULL,
               
               plotlyOutput(ns("disease_geography_plot"), height = "400px")
             )
      ),
      
      column(6,
             box(
               title = "Health System Performance",
               status = "warning",
               solidHeader = TRUE,
               width = NULL,
               
               plotlyOutput(ns("health_system_performance"), height = "400px")
             )
      )
    ),
    
    # Risk profile and facility analysis
    fluidRow(
      column(6,
             box(
               title = "Population Health Risk Profile",
               status = "danger",
               solidHeader = TRUE,
               width = NULL,
               
               plotlyOutput(ns("risk_profile_plot"), height = "400px")
             )
      ),
      
      column(6,
             box(
               title = "Facility Distribution Analysis",
               status = "success",
               solidHeader = TRUE,
               width = NULL,
               
               plotlyOutput(ns("facility_distribution_plot"), height = "400px")
             )
      )
    )
  )
}

# --- MODULE SERVER ---

#' Geographic Analysis Module Server
mod_geographic_server <- function(id, data, config = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Helper functions
    create_fallback_plot <- function(title, message) {
      plot_ly() %>%
        add_annotations(
          text = message,
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(title = title)
    }
    
    # Helper function to get date column
    get_date_col <- function(df) {
      if ("datevisitnew" %in% colnames(df)) "datevisitnew"
      else if ("datevisit" %in% colnames(df)) "datevisit"
      else NULL
    }
    
    # Update disease filter choices
    observe({
      req(data())
      
      if ("category_canonical_disease_imc" %in% colnames(data())) {
        diseases <- data() %>%
          filter(!is.na(category_canonical_disease_imc)) %>%
          count(category_canonical_disease_imc, sort = TRUE) %>%
          pull(category_canonical_disease_imc)
        
        disease_choices <- c("All Diseases" = "all", setNames(diseases, diseases))
        
        updateSelectInput(
          session, "map_disease",
          choices = disease_choices,
          selected = "all"
        )
      }
    })
    
    # Filtered data for mapping
    map_data <- reactive({
      req(data(), input$map_time_period)
      
      df <- data()
      date_col <- get_date_col(df)
      
      # Apply time filter
      if (!is.null(date_col) && input$map_time_period != 9999) {
        current_date <- max(df[[date_col]], na.rm = TRUE)
        if (!is.na(current_date)) {
          df <- df %>%
            filter(.data[[date_col]] >= current_date - days(as.numeric(input$map_time_period)))
        }
      }
      
      # Apply disease filter
      if (!is.null(input$map_disease) && input$map_disease != "all" && "category_canonical_disease_imc" %in% colnames(df)) {
        df <- df %>%
          filter(category_canonical_disease_imc == input$map_disease)
      }
      
      return(df)
    })
    
    # Geographic summary data using helper function
    geographic_summary <- reactive({
      req(map_data())
      
      df <- map_data()
      
      if (!"admin1" %in% colnames(df) || nrow(df) == 0) {
        return(data.frame())
      }
      
      # Use the helper function
      summary_data <- create_geographic_summary(df)
      
      # Add calculated metrics for UI
      summary_data <- summary_data %>%
        mutate(
          consultations = total_consultations,
          facilities = unique_facilities,
          consultations_per_facility = consultations_per_facility,
          trauma_rate = trauma_percentage,
          epidemic_cases = epidemic_cases
        )
      
      return(summary_data)
    })
    
    # Value boxes for key geographic metrics
    output$geo_total_regions <- renderValueBox({
      df <- map_data()
      total_regions <- if (!is.null(df) && "admin1" %in% colnames(df)) {
        n_distinct(df$admin1, na.rm = TRUE)
      } else { 0 }
      
      valueBox(
        value = total_regions,
        subtitle = "Active Governorates",
        icon = icon("map-marker-alt"),
        color = "blue"
      )
    })
    
    output$geo_total_facilities <- renderValueBox({
      df <- map_data()
      total_facilities <- if (!is.null(df) && "orgunit" %in% colnames(df)) {
        n_distinct(df$orgunit, na.rm = TRUE)
      } else { 0 }
      
      valueBox(
        value = total_facilities,
        subtitle = "Health Facilities",
        icon = icon("hospital"),
        color = "green"
      )
    })
    
    output$geo_avg_utilization <- renderValueBox({
      df <- map_data()
      avg_util <- if (!is.null(df) && all(c("admin1", "orgunit") %in% colnames(df))) {
        avg_calc <- df %>%
          group_by(orgunit) %>%
          summarise(consultations = n(), .groups = "drop") %>%
          summarise(avg = round(mean(consultations), 1)) %>%
          pull(avg)
        paste0(avg_calc, "/facility")
      } else { "N/A" }
      
      valueBox(
        value = avg_util,
        subtitle = "Average Utilization",
        icon = icon("chart-bar"),
        color = "yellow"
      )
    })
    
    output$geo_trauma_rate <- renderValueBox({
      df <- map_data()
      trauma_rate <- if (!is.null(df) && "type_case" %in% colnames(df)) {
        rate <- round(sum(str_detect(tolower(df$type_case %||% ""), "trauma") & 
                         !str_detect(tolower(df$type_case %||% ""), "non"), na.rm = TRUE) / nrow(df) * 100, 1)
        paste0(rate, "%")
      } else { "N/A" }
      
      color <- if (grepl("N/A", trauma_rate)) "blue" 
              else if (as.numeric(gsub("%", "", trauma_rate)) > 15) "red" 
              else "green"
      
      valueBox(
        value = trauma_rate,
        subtitle = "Trauma Rate",
        icon = icon("ambulance"),
        color = color
      )
    })
    
    # Enhanced Syria overview map
    output$overview_map <- renderLeaflet({
      df <- map_data()
      
      tryCatch({
        # Create enhanced Syria map with consultation volume
        map <- leaflet() %>%
          addTiles() %>%
          setView(lng = 38.0, lat = 35.0, zoom = 6) %>%
          addProviderTiles(providers$CartoDB.Positron)
        
        if (!is.null(df) && nrow(df) > 0 && "admin1" %in% names(df)) {
          
          # Calculate consultation volume by governorate
          gov_data <- df %>%
            group_by(admin1) %>%
            summarise(
              total_consultations = n(),
              facilities = if("orgunit" %in% names(df)) n_distinct(orgunit, na.rm = TRUE) else 0,
              consultations_per_facility = if("orgunit" %in% names(df)) {
                round(n() / pmax(n_distinct(orgunit, na.rm = TRUE), 1), 1)
              } else 0,
              trauma_cases = if("type_case" %in% names(df)) {
                sum(str_detect(tolower(type_case %||% ""), "trauma") & 
                    !str_detect(tolower(type_case %||% ""), "non"), na.rm = TRUE)
              } else 0,
              trauma_rate = if("type_case" %in% names(df)) {
                round(sum(str_detect(tolower(type_case %||% ""), "trauma") & 
                         !str_detect(tolower(type_case %||% ""), "non"), na.rm = TRUE) / n() * 100, 1)
              } else 0,
              top_disease = if("category_canonical_disease_imc" %in% names(df)) {
                disease_table <- table(category_canonical_disease_imc)
                if(length(disease_table) > 0) names(disease_table)[which.max(disease_table)] else "Unknown"
              } else "Unknown",
              .groups = "drop"
            ) %>%
            arrange(desc(total_consultations))
          
          # Enhanced coordinates for Syrian governorates
          gov_coords <- data.frame(
            admin1 = c("Aleppo", "Damascus", "Homs", "Al-Hasakeh", "Ar-Raqqa", 
                       "Daraa", "Deir-ez-Zor", "Idlib", "Lattakia", "Tartous", 
                       "As-Sweida", "Quneitra", "Damascus Countryside", "Hama"),
            lng = c(37.16, 36.29, 36.73, 40.74, 39.01, 36.10, 40.14, 36.63, 
                    35.78, 35.88, 36.57, 35.82, 36.29, 35.13),
            lat = c(36.20, 33.51, 34.73, 36.50, 35.95, 32.62, 35.34, 35.93, 
                    35.53, 34.88, 32.71, 33.12, 33.51, 35.13),
            stringsAsFactors = FALSE
          )
          
          # Merge consultation data with coordinates
          map_data_merged <- dplyr::left_join(gov_coords, gov_data, by = "admin1") %>%
            mutate(
              total_consultations = replace_na(total_consultations, 0),
              facilities = replace_na(facilities, 0),
              consultations_per_facility = replace_na(consultations_per_facility, 0),
              trauma_rate = replace_na(trauma_rate, 0),
              top_disease = replace_na(top_disease, "No data")
            )
          
          # Create color palette based on consultation volume
          if(max(map_data_merged$total_consultations) > 0) {
            pal <- colorNumeric(
              palette = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#99000D"),
              domain = c(0, max(map_data_merged$total_consultations))
            )
          } else {
            pal <- colorNumeric(palette = "Blues", domain = c(0, 1))
          }
          
          # Add circle markers for each governorate
          for (i in 1:nrow(map_data_merged)) {
            row <- map_data_merged[i, ]
            
            # Calculate marker size based on consultation volume
            base_radius <- 8
            max_radius <- 25
            if(max(map_data_merged$total_consultations) > 0) {
              radius <- base_radius + (row$total_consultations / max(map_data_merged$total_consultations)) * (max_radius - base_radius)
            } else {
              radius <- base_radius
            }
            
            # Create detailed popup
            popup_content <- paste0(
              "<div style='font-family: Arial, sans-serif; line-height: 1.4;'>",
              "<b style='font-size: 16px; color: #2c3e50;'>", row$admin1, "</b><br/>",
              "<hr style='margin: 8px 0; border: none; border-top: 1px solid #bdc3c7;'/>",
              "<div style='display: flex; justify-content: space-between; margin: 5px 0;'>",
              "<span><i class='fas fa-user-md' style='color: #3498db;'></i> Total Consultations:</span>",
              "<strong style='color: #2980b9;'>", format(row$total_consultations, big.mark = ","), "</strong>",
              "</div>",
              "<div style='display: flex; justify-content: space-between; margin: 5px 0;'>",
              "<span><i class='fas fa-hospital' style='color: #27ae60;'></i> Health Facilities:</span>",
              "<strong style='color: #27ae60;'>", row$facilities, "</strong>",
              "</div>",
              "<div style='display: flex; justify-content: space-between; margin: 5px 0;'>",
              "<span><i class='fas fa-chart-line' style='color: #f39c12;'></i> Cons. per Facility:</span>",
              "<strong style='color: #e67e22;'>", row$consultations_per_facility, "</strong>",
              "</div>",
              if(row$trauma_rate > 0) {
                paste0(
                  "<div style='display: flex; justify-content: space-between; margin: 5px 0;'>",
                  "<span><i class='fas fa-exclamation-triangle' style='color: #e74c3c;'></i> Trauma Rate:</span>",
                  "<strong style='color: #c0392b;'>", row$trauma_rate, "%</strong>",
                  "</div>"
                )
              } else "",
              "<div style='display: flex; justify-content: space-between; margin: 5px 0;'>",
              "<span><i class='fas fa-virus' style='color: #9b59b6;'></i> Top Disease:</span>",
              "<strong style='color: #8e44ad;'>", substr(row$top_disease, 1, 20), if(nchar(row$top_disease) > 20) "..." else "", "</strong>",
              "</div>",
              "</div>"
            )
            
            # Determine marker color based on consultation volume
            marker_color <- if(row$total_consultations > 0) {
              pal(row$total_consultations)
            } else {
              "#95a5a6"
            }
            
            # Add circle marker
            map <- map %>%
              addCircleMarkers(
                lng = row$lng,
                lat = row$lat,
                radius = radius,
                popup = popup_content,
                popupOptions = popupOptions(maxWidth = 300, closeButton = TRUE),
                label = paste0(row$admin1, ": ", format(row$total_consultations, big.mark = ","), " consultations"),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "12px",
                  direction = "auto"
                ),
                color = "white",
                weight = 2,
                opacity = 0.8,
                fillColor = marker_color,
                fillOpacity = 0.7,
                options = markerOptions(riseOnHover = TRUE)
              )
          }
          
          # Add legend for consultation volume
          if(max(map_data_merged$total_consultations) > 0) {
            map <- map %>%
              addLegend(
                pal = pal,
                values = map_data_merged$total_consultations,
                opacity = 0.7,
                title = "Consultation<br>Volume",
                position = "bottomright",
                labFormat = labelFormat(big.mark = ",")
              )
          }
          
          # Add summary information
          total_consultations <- sum(map_data_merged$total_consultations, na.rm = TRUE)
          total_facilities <- sum(map_data_merged$facilities, na.rm = TRUE)
          active_govs <- sum(map_data_merged$total_consultations > 0)
          
          # Add control box with summary
          map <- map %>%
            addControl(
              html = paste0(
                "<div style='background: rgba(255,255,255,0.9); padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.2); font-family: Arial, sans-serif;'>",
                "<h4 style='margin: 0 0 10px 0; color: #2c3e50; font-size: 16px;'><i class='fas fa-chart-pie'></i> Syria Health Surveillance</h4>",
                "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 10px; font-size: 12px;'>",
                "<div><strong>Total Consultations:</strong><br/><span style='color: #2980b9; font-size: 14px; font-weight: bold;'>", format(total_consultations, big.mark = ","), "</span></div>",
                "<div><strong>Active Governorates:</strong><br/><span style='color: #27ae60; font-size: 14px; font-weight: bold;'>", active_govs, "/", nrow(map_data_merged), "</span></div>",
                "<div><strong>Health Facilities:</strong><br/><span style='color: #f39c12; font-size: 14px; font-weight: bold;'>", total_facilities, "</span></div>",
                "<div><strong>Data Status:</strong><br/><span style='color: #27ae60; font-size: 14px; font-weight: bold;'>Active</span></div>",
                "</div>",
                "<div style='margin-top: 10px; font-size: 11px; color: #7f8c8d;'>",
                "Click markers for detailed information<br/>",
                "Marker size indicates consultation volume",
                "</div>",
                "</div>"
              ),
              position = "topleft"
            )
          
        } else {
          # Fallback when no data is available
          map <- map %>%
            addMarkers(
              lng = 38.0, lat = 35.0,
              popup = paste0(
                "<div style='text-align: center; font-family: Arial, sans-serif;'>",
                "<h4 style='color: #e74c3c; margin: 10px 0;'><i class='fas fa-exclamation-circle'></i> No Data Available</h4>",
                "<p style='margin: 5px 0;'>No surveillance data is currently loaded.</p>",
                "<p style='margin: 5px 0; font-size: 12px; color: #7f8c8d;'>Please check data filters or refresh the application.</p>",
                "</div>"
              ),
              options = markerOptions(opacity = 0.7)
            ) %>%
            addControl(
              html = paste0(
                "<div style='background: rgba(255,255,255,0.9); padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.2); font-family: Arial, sans-serif;'>",
                "<h4 style='margin: 0 0 10px 0; color: #e74c3c; font-size: 16px;'><i class='fas fa-exclamation-triangle'></i> Syria Health Surveillance</h4>",
                "<p style='margin: 5px 0; color: #7f8c8d; font-size: 12px;'>No epidemiological data available</p>",
                "<p style='margin: 5px 0; color: #7f8c8d; font-size: 12px;'>Please load data or adjust filters</p>",
                "</div>"
              ),
              position = "topleft"
            )
        }
        
        return(map)
        
      }, error = function(e) {
        # Error fallback
        leaflet() %>%
          addTiles() %>%
          setView(lng = 38.0, lat = 35.0, zoom = 6) %>%
          addMarkers(
            lng = 38.0, lat = 35.0,
            popup = paste0(
              "<div style='text-align: center; font-family: Arial, sans-serif;'>",
              "<h4 style='color: #e74c3c;'><i class='fas fa-exclamation-circle'></i> Map Error</h4>",
              "<p>Error loading map: ", e$message, "</p>",
              "</div>"
            )
          )
      })
    })
    
    # Detailed map for geographic analysis
    output$detailed_map <- renderLeaflet({
      df <- map_data()
      
      # Create basic Syria map with data information
      map <- leaflet() %>%
        addTiles() %>%
        setView(lng = 38.0, lat = 35.0, zoom = 6) %>%
        addProviderTiles(providers$OpenStreetMap)
      
      if (!is.null(df) && nrow(df) > 0) {
        # Add markers for major Syrian cities with case counts
        if ("admin1" %in% names(df)) {
          city_data <- df %>%
            count(admin1, sort = TRUE) %>%
            head(10)
          
          # Approximate coordinates for major Syrian governorates
          city_coords <- data.frame(
            admin1 = c("Aleppo", "Damascus", "Homs", "Al-Hasakeh", "Ar-Raqqa", "Daraa", "Deir-ez-Zor", "Idlib", "Lattakia", "Tartous"),
            lng = c(37.16, 36.29, 36.73, 40.74, 39.01, 36.10, 40.14, 36.63, 35.78, 35.88),
            lat = c(36.20, 33.51, 34.73, 36.50, 35.95, 32.62, 35.34, 35.93, 35.53, 34.88),
            stringsAsFactors = FALSE
          )
          
          # Merge with case data
          map_data_detailed <- dplyr::left_join(city_coords, as.data.frame(city_data), by = "admin1")
          map_data_detailed$n <- tidyr::replace_na(map_data_detailed$n, 0)
          
          # Add markers with enhanced information
          for (i in 1:nrow(map_data_detailed)) {
            # Calculate additional metrics for this governorate
            region_data <- df %>% filter(admin1 == map_data_detailed$admin1[i])
            
            facilities_count <- if ("orgunit" %in% names(df)) {
              n_distinct(region_data$orgunit, na.rm = TRUE)
            } else { 0 }
            
            trauma_cases <- if ("type_case" %in% names(df)) {
              sum(str_detect(tolower(region_data$type_case %||% ""), "trauma") & 
                  !str_detect(tolower(region_data$type_case %||% ""), "non"), na.rm = TRUE)
            } else { 0 }
            
            # Create enhanced popup
            popup_content <- paste0(
              "<div style='font-family: Arial, sans-serif;'>",
              "<h4 style='margin: 5px 0; color: #2c3e50;'>", map_data_detailed$admin1[i], "</h4>",
              "<hr style='margin: 5px 0;'/>",
              "<p><strong>Total Cases:</strong> ", format(map_data_detailed$n[i], big.mark = ","), "</p>",
              "<p><strong>Health Facilities:</strong> ", facilities_count, "</p>",
              if (facilities_count > 0) {
                paste0("<p><strong>Cases per Facility:</strong> ", round(map_data_detailed$n[i] / facilities_count, 1), "</p>")
              } else "",
              if (trauma_cases > 0) {
                paste0("<p><strong>Trauma Cases:</strong> ", trauma_cases, " (", round(trauma_cases / map_data_detailed$n[i] * 100, 1), "%)</p>")
              } else "",
              "</div>"
            )
            
            map <- map %>%
              addCircleMarkers(
                lng = map_data_detailed$lng[i],
                lat = map_data_detailed$lat[i],
                radius = sqrt(map_data_detailed$n[i]) / 3 + 5,
                popup = popup_content,
                popupOptions = popupOptions(maxWidth = 250),
                label = paste0(map_data_detailed$admin1[i], ": ", format(map_data_detailed$n[i], big.mark = ",")),
                color = if (trauma_cases / pmax(map_data_detailed$n[i], 1) > 0.15) "#e74c3c" else "#3498db",
                fillOpacity = 0.7,
                weight = 2
              )
          }
        }
        
        # Add overall data summary marker
        map <- map %>%
          addMarkers(
            lng = 38.0, lat = 35.0,
            popup = paste0(
              "<div style='text-align: center; font-family: Arial, sans-serif;'>",
              "<h4 style='color: #2c3e50;'><i class='fas fa-chart-pie'></i> Syria Health Surveillance</h4>",
              "<hr/>",
              "<p><strong>Total Cases:</strong> ", format(nrow(df), big.mark = ","), "</p>",
              "<p><strong>Active Governorates:</strong> ", n_distinct(df$admin1, na.rm = TRUE), "</p>",
              "<p><strong>Data Status:</strong> <span style='color: #27ae60;'>Active</span></p>",
              "</div>"
            ),
            options = markerOptions(opacity = 0.8)
          )
      } else {
        map <- map %>%
          addMarkers(
            lng = 38.0, lat = 35.0,
            popup = paste0(
              "<div style='text-align: center; font-family: Arial, sans-serif;'>",
              "<h4 style='color: #e74c3c;'>No Data Available</h4>",
              "<p>No surveillance data is currently loaded.</p>",
              "</div>"
            )
          )
      }
      
      return(map)
    })
    
    # Geographic insights availability flag
    output$geographic_insights_available <- reactive({
      df <- map_data()
      !is.null(df) && nrow(df) > 0 && "admin1" %in% names(df)
    })
    outputOptions(output, "geographic_insights_available", suspendWhenHidden = FALSE)
    
    # Geographic insights summary
    output$geographic_insights_summary <- renderUI({
      df <- map_data()
      
      if (is.null(df) || nrow(df) == 0 || !"admin1" %in% names(df)) {
        return(div("No geographic data available for analysis."))
      }
      
      tryCatch({
        # Create geographic summary
        geo_summary <- geographic_summary()
        
        if (nrow(geo_summary) == 0) {
          return(div("No geographic patterns identified."))
        }
        
        # Generate insights
        top_region <- geo_summary$admin1[1]
        total_regions <- nrow(geo_summary)
        
        # Trauma analysis if available
        trauma_insight <- if ("trauma_percentage" %in% names(geo_summary)) {
          high_trauma_regions <- sum(geo_summary$trauma_percentage > 15, na.rm = TRUE)
          if (high_trauma_regions > 0) {
            paste0("⚠️ ", high_trauma_regions, " governorate(s) show high trauma burden (>15%)")
          } else {
            "✅ Trauma rates within normal ranges across all governorates"
          }
        } else {
          "Trauma data not available for governorate analysis"
        }
        
        # Create insights list
        insights <- tags$div(
          tags$div(class = "alert alert-info",
                   tags$strong("📍 Geographic Coverage: "), 
                   paste("Active surveillance in", total_regions, "administrative regions")
          ),
          tags$div(class = "alert alert-warning",
                   tags$strong("🎯 Highest Activity: "), 
                   paste(top_region, "shows highest consultation volume")
          ),
          tags$div(class = if (grepl("⚠️", trauma_insight)) "alert alert-danger" else "alert alert-success",
                   tags$strong("🚑 Trauma Assessment: "), trauma_insight
          ),
          tags$br(),
          tags$h6("📊 Top 5 Governorates by Volume:"),
          tags$ul(
            lapply(1:min(5, nrow(geo_summary)), function(i) {
              row <- geo_summary[i, ]
              tags$li(paste0(row$admin1, ": ", scales::comma(row$total_consultations), " consultations"))
            })
          )
        )
        
        return(insights)
        
      }, error = function(e) {
        return(div(class = "alert alert-warning", 
                   "Error generating geographic insights: ", e$message))
      })
    })
    
    # Governorate priority matrix
    output$regional_priority_matrix <- renderTable({
      df <- map_data()
      
      if (is.null(df) || nrow(df) == 0 || !"admin1" %in% names(df)) {
        return(data.frame(Message = "No governorate data available"))
      }
      
      tryCatch({
        priority_matrix <- df %>%
          group_by(admin1) %>%
          summarise(
            `Total Cases` = n(),
            `Trauma Rate` = if ("type_case" %in% names(df)) {
              round(sum(str_detect(tolower(type_case %||% ""), "trauma") & 
                       !str_detect(tolower(type_case %||% ""), "non"), na.rm = TRUE) / n() * 100, 1)
            } else 0,
            `Facilities` = if ("orgunit" %in% names(df)) {
              n_distinct(orgunit, na.rm = TRUE)
            } else 0,
            `Priority Score` = {
              # Calculate priority based on case volume, trauma rate, and facility density
              case_score <- rank(n()) / n() * 40
              trauma_score <- if ("type_case" %in% names(df)) {
                (sum(str_detect(tolower(type_case %||% ""), "trauma") & 
                    !str_detect(tolower(type_case %||% ""), "non"), na.rm = TRUE) / n()) * 40
              } else 0
              facility_score <- if ("orgunit" %in% names(df)) {
                (n() / n_distinct(orgunit, na.rm = TRUE)) / 100 * 20
              } else 0
              round(case_score + trauma_score + facility_score, 1)
            },
            .groups = "drop"
          ) %>%
          arrange(desc(`Priority Score`)) %>%
          slice_head(n = 8) %>%
          select(
            Region = admin1,
            `Total Cases`,
            `Trauma Rate`,
            Facilities,
            `Priority Score`
          )
        
        return(priority_matrix)
        
      }, error = function(e) {
        return(data.frame(Error = paste("Analysis error:", e$message)))
      })
    }, striped = TRUE, hover = TRUE)
    
    # Spatial clustering plot
    output$spatial_clustering_plot <- renderPlotly({
      df <- map_data()
      
      if (is.null(df) || !"admin1" %in% names(df)) {
        return(plot_ly() %>%
                 add_annotations(text = "No geographic data available for clustering analysis",
                               x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                               showarrow = FALSE, font = list(size = 16)))
      }
      
      tryCatch({
        # Create spatial clustering visualization
        cluster_data <- df %>%
          group_by(admin1) %>%
          summarise(
            consultations = n(),
            disease_diversity = if ("category_canonical_disease_imc" %in% names(df)) {
              n_distinct(category_canonical_disease_imc, na.rm = TRUE)
            } else 1,
            trauma_rate = if ("type_case" %in% names(df)) {
              sum(str_detect(tolower(type_case %||% ""), "trauma") & 
                  !str_detect(tolower(type_case %||% ""), "non"), na.rm = TRUE) / n() * 100
            } else 0,
            .groups = "drop"
          ) %>%
          mutate(
            cluster_group = case_when(
              consultations >= quantile(consultations, 0.75, na.rm = TRUE) & trauma_rate >= 15 ~ "High Volume + High Trauma",
              consultations >= quantile(consultations, 0.75, na.rm = TRUE) ~ "High Volume",
              trauma_rate >= 15 ~ "High Trauma",
              TRUE ~ "Standard"
            )
          )
        
        p <- ggplot(cluster_data, aes(x = consultations, y = trauma_rate, 
                                     color = cluster_group, size = disease_diversity)) +
          geom_point(alpha = 0.7) +
          geom_text(aes(label = admin1), vjust = -0.8, size = 3, fontface = "bold") +
          scale_color_manual(values = c(
            "High Volume + High Trauma" = "#e74c3c",
            "High Volume" = "#f39c12", 
            "High Trauma" = "#e67e22",
            "Standard" = "#3498db"
          )) +
          scale_size_continuous(range = c(3, 12), name = "Disease\nDiversity") +
          labs(
            title = "Governorate Health Service Clustering Analysis",
            x = "Total Consultations",
            y = "Trauma Rate (%)",
            color = "Cluster Group"
          ) +
          theme_minimal() +
          scale_x_continuous(labels = scales::comma_format())
        
        ggplotly(p, tooltip = c("x", "y", "size", "colour", "text"))
        
      }, error = function(e) {
        plot_ly() %>%
          add_annotations(text = paste("Clustering analysis error:", e$message),
                         x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                         showarrow = FALSE)
      })
    })
    
    # Geographic summary table
    output$geo_summary_table <- DT::renderDataTable({
      req(geographic_summary())
      
      summary_data <- geographic_summary() %>%
        select(
          `Governorate` = admin1,
          `Total Consultations` = total_consultations,
          `Health Facilities` = unique_facilities,
          `Consultations per Facility` = consultations_per_facility,
          `Disease Diversity` = disease_diversity,
          `Trauma Rate (%)` = trauma_percentage,
          `Epidemic Cases` = epidemic_cases,
          `Female (%)` = female_percentage
        ) %>%
        mutate(
          `Total Consultations` = comma(`Total Consultations`)
        )
      
      DT::datatable(
        summary_data,
        options = list(
          pageLength = 15,
          dom = 'frtip',
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Trauma Rate (%)",
          backgroundColor = DT::styleInterval(c(10, 20), c("lightgreen", "orange", "lightcoral"))
        )
    })
    
    # Choropleth map (using bar chart as fallback)
    output$choropleth_map <- renderPlotly({
      req(geographic_summary())
      
      summary_data <- geographic_summary()
      
      if (nrow(summary_data) == 0) {
        return(create_fallback_plot("Governorate Analysis", "No geographic data available"))
      }
      
      # Select metric for visualization
      metric_col <- switch(input$map_metric,
                          "consultations" = "total_consultations",
                          "consultations_per_facility" = "consultations_per_facility", 
                          "disease_diversity" = "disease_diversity",
                          "trauma_rate" = "trauma_percentage",
                          "epidemic_cases" = "epidemic_cases",
                          "female_percentage" = "female_percentage",
                          "total_consultations")
      
      y_values <- summary_data[[metric_col]]
      
      # Create enhanced bar plot as choropleth alternative
      p <- ggplot(summary_data, aes(x = reorder(admin1, y_values), y = y_values)) +
        geom_col(aes(fill = y_values), alpha = 0.8, color = "white", size = 0.5) +
        coord_flip() +
        scale_fill_gradient2(low = "#E3F2FD", mid = "#2196F3", high = "#0D47A1",
                            midpoint = median(y_values, na.rm = TRUE)) +
        labs(
          title = paste("Geographic Distribution:", str_to_title(gsub("_", " ", input$map_metric))),
          x = "Governorate",
          y = str_to_title(gsub("_", " ", input$map_metric)),
          fill = "Value"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 10)
        ) +
        scale_y_continuous(labels = comma_format())
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(margin = list(l = 100))
    })
    
    # Disease geography plot
    output$disease_geography_plot <- renderPlotly({
      req(map_data())
      
      df <- map_data()
      
      if (!all(c("admin1", "category_canonical_disease_imc") %in% colnames(df))) {
        return(create_fallback_plot("Disease Geography", "Disease or geographic data not available"))
      }
      
      # Top diseases by governorate
      disease_geo <- df %>%
        filter(!is.na(category_canonical_disease_imc), !is.na(admin1)) %>%
        count(admin1, category_canonical_disease_imc) %>%
        group_by(admin1) %>%
        slice_max(n, n = 3) %>%
        ungroup() %>%
        arrange(desc(n))
      
      if (nrow(disease_geo) == 0) {
        return(create_fallback_plot("Disease Geography", "No disease data available for selected filters"))
      }
      
      p <- ggplot(disease_geo, aes(x = reorder(category_canonical_disease_imc, n), y = n, fill = admin1)) +
        geom_col(position = "dodge", alpha = 0.8) +
        coord_flip() +
        scale_fill_brewer(type = "qual", palette = "Set1") +
        labs(
          title = "Top Disease Conditions by Governorate",
          x = "Disease Category",
          y = "Number of Cases", 
          fill = "Governorate"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom"
        ) +
        scale_y_continuous(labels = comma_format())
      
      ggplotly(p, tooltip = c("fill", "y")) %>%
        layout(legend = list(orientation = "h", x = 0, y = -0.2))
    })
    
    # Health system performance
    output$health_system_performance <- renderPlotly({
      req(map_data())
      
      df <- map_data()
      
      if (!"admin1" %in% colnames(df)) {
        return(create_fallback_plot("Health System Performance", "Administrative data not available"))
      }
      
      # Calculate utilization categories
      facility_utilization <- df %>%
        group_by(admin1) %>%
        summarise(
          total_consultations = n(),
          unique_facilities = if ("orgunit" %in% colnames(df)) n_distinct(orgunit, na.rm = TRUE) else 1,
          avg_consultations = round(n() / pmax(if ("orgunit" %in% colnames(df)) n_distinct(orgunit, na.rm = TRUE) else 1, 1), 1),
          utilization_score = case_when(
            n() / pmax(if ("orgunit" %in% colnames(df)) n_distinct(orgunit, na.rm = TRUE) else 1, 1) >= 50 ~ "High Utilization",
            n() / pmax(if ("orgunit" %in% colnames(df)) n_distinct(orgunit, na.rm = TRUE) else 1, 1) >= 20 ~ "Medium Utilization", 
            TRUE ~ "Low Utilization"
          ),
          .groups = "drop"
        )
      
      p <- ggplot(facility_utilization, aes(x = unique_facilities, y = avg_consultations, 
                                           color = utilization_score, size = total_consultations)) +
        geom_point(alpha = 0.7) +
        geom_text(aes(label = admin1), vjust = -0.8, size = 3, fontface = "bold") +
        scale_color_manual(values = c("High Utilization" = "#2E7D32", 
                                     "Medium Utilization" = "#F57C00", 
                                     "Low Utilization" = "#C62828")) +
        scale_size_continuous(range = c(3, 12), guide = "none") +
        labs(
          title = "Health System Performance Analysis",
          x = "Total Facilities",
          y = "Average Consultations per Facility",
          color = "Utilization Level"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom"
        )
      
      ggplotly(p, tooltip = c("x", "y", "colour", "text"))
    })
    
    # Population health risk profile
    output$risk_profile_plot <- renderPlotly({
      req(map_data())
      
      df <- map_data()
      
      # Use the helper function
      risk_factors <- create_risk_profile_analysis(df)
      
      if (nrow(risk_factors) == 0) {
        return(create_fallback_plot("Risk Profile", "No risk profile data available"))
      }
      
      p <- ggplot(risk_factors, aes(x = admin1, y = percentage, fill = risk_category)) +
        geom_col(position = "stack", alpha = 0.8) +
        coord_flip() +
        scale_fill_brewer(type = "qual", palette = "Set2") +
        labs(
          title = "Population Risk Profile by Governorate",
          x = "Governorate",
          y = "Percentage of Consultations",
          fill = "Risk Category"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom"
        )
      
      ggplotly(p, tooltip = c("x", "y", "fill"))
    })
    
    # Facility distribution plot
    output$facility_distribution_plot <- renderPlotly({
      req(map_data())
      
      df <- map_data()
      
      if (!all(c("admin1", "orgunit") %in% colnames(df))) {
        return(create_fallback_plot("Facility Distribution", "Facility data not available"))
      }
      
      # Enhanced facility analysis
      facility_analysis <- df %>%
        group_by(admin1) %>%
        summarise(
          facilities = n_distinct(orgunit, na.rm = TRUE),
          avg_consultations = round(n() / pmax(n_distinct(orgunit, na.rm = TRUE), 1), 1),
          total_consultations = n(),
          .groups = "drop"
        )
      
      p <- ggplot(facility_analysis, aes(x = facilities, y = avg_consultations)) +
        geom_point(aes(size = total_consultations, color = admin1), alpha = 0.7) +
        geom_text(aes(label = admin1), vjust = -0.8, size = 3, fontface = "bold") +
        scale_size_continuous(range = c(3, 15), name = "Total\nConsultations", labels = comma_format()) +
        scale_color_brewer(type = "qual", palette = "Set2", guide = "none") +
        labs(
          title = "Facility Distribution and Performance",
          subtitle = "Bubble size represents total consultations",
          x = "Number of Facilities",
          y = "Average Consultations per Facility"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold"))
      
      ggplotly(p, tooltip = c("x", "y", "size", "text"))
    })
    
    # Download handler
    output$download_map_data <- downloadHandler(
      filename = function() {
        paste0("geographic_analysis_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(geographic_summary())
        
        download_data <- geographic_summary() %>%
          mutate(
            analysis_date = Sys.Date(),
            time_period_days = input$map_time_period,
            disease_filter = input$map_disease %||% "all",
            metric_analyzed = input$map_metric
          )
        
        write.csv(download_data, file, row.names = FALSE)
      }
    )
    
    # Return geographic summary for other modules
    return(list(
      geographic_summary = geographic_summary,
      map_data = map_data
    ))
  })
}
