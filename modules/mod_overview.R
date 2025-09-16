# modules/mod_overview.R
# Dashboard Overview Module - Enhanced with Leaflet map

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

#' Overview Module UI
#'
#' @param id Character string. The module's ID
#'
#' @return Shiny UI elements
mod_overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Value boxes row
    fluidRow(
      column(3, valueBoxOutput(ns("total_consultations"), width = NULL)),
      column(3, valueBoxOutput(ns("active_facilities"), width = NULL)),
      column(3, valueBoxOutput(ns("top_disease"), width = NULL)),
      column(3, valueBoxOutput(ns("recent_trend"), width = NULL))
    ),
    
    # Main content row with map and trends
    fluidRow(
      column(8,
             box(
               title = "Health Surveillance Map",
               status = "primary",
               solidHeader = TRUE,
               width = NULL,
               
               # Map controls
               fluidRow(
                 column(4,
                        selectInput(ns("map_metric"), "Map Metric:",
                                    choices = list(
                                      "Total Consultations" = "total_consultations",
                                      "Consultations per Facility" = "consultations_per_facility",
                                      "Trauma Rate (%)" = "trauma_rate",
                                      "Disease Diversity" = "disease_diversity"
                                    ),
                                    selected = "total_consultations")
                 ),
                 column(4,
                        selectInput(ns("map_time_filter"), "Time Period:",
                                    choices = list(
                                      "Last 30 days" = 30,
                                      "Last 90 days" = 90,
                                      "Last 6 months" = 180,
                                      "All data" = 9999
                                    ),
                                    selected = 90)
                 ),
                 column(4,
                        div(style = "padding-top: 25px;",
                            downloadButton(ns("download_map_data"), "Download Map Data",
                                           class = "btn-success btn-sm", icon = icon("download"))
                        )
                 )
               ),
               
               leafletOutput(ns("overview_map"), height = "500px")
             )
      ),
      column(4,
             box(
               title = "Quick Statistics",
               status = "info",
               solidHeader = TRUE,
               width = NULL,
               height = "580px",
               
               tableOutput(ns("quick_stats_table")),
               
               hr(),
               
               h5("Recent Activity"),
               div(id = ns("recent_activity"),
                   uiOutput(ns("recent_activity_content"))
               ),
               
               hr(),
               
               h5("Map Legend"),
               div(id = ns("map_legend"),
                   uiOutput(ns("map_legend_content"))
               )
             )
      )
    ),
    
    # Disease trends row
    fluidRow(
      column(12,
             box(
               title = "Disease Trends Overview",
               status = "success",
               solidHeader = TRUE,
               width = NULL,
               collapsible = TRUE,
               
               # Controls for the overview plot
               fluidRow(
                 column(3,
                        selectInput(ns("overview_time_period"), "Time Period:",
                                    choices = list(
                                      "Last 30 days" = 30,
                                      "Last 90 days" = 90,
                                      "Last 6 months" = 180,
                                      "Last year" = 365
                                    ),
                                    selected = 90)
                 ),
                 column(3,
                        selectInput(ns("overview_chart_type"), "Chart Type:",
                                    choices = list(
                                      "Line Chart" = "line",
                                      "Area Chart" = "area",
                                      "Bar Chart" = "bar"
                                    ),
                                    selected = "line")
                 ),
                 column(3,
                        numericInput(ns("overview_top_diseases"), "Top Diseases:",
                                     value = 5, min = 3, max = 10, step = 1)
                 ),
                 column(3,
                        div(style = "padding-top: 25px;",
                            downloadButton(ns("download_trends_data"), "Download Trends",
                                           class = "btn-info btn-sm", icon = icon("chart-line"))
                        )
                 )
               ),
               
               plotlyOutput(ns("overview_disease_trends"), height = "400px")
             )
      )
    ),
    
    # Secondary metrics row
    fluidRow(
      column(4,
             box(
               title = "Geographic Distribution",
               status = "success",
               solidHeader = TRUE,
               width = NULL,
               
               plotlyOutput(ns("geographic_overview"), height = "300px")
             )
      ),
      column(4,
             box(
               title = "Disease Categories",
               status = "warning",
               solidHeader = TRUE,
               width = NULL,
               
               plotlyOutput(ns("disease_categories_overview"), height = "300px")
             )
      ),
      column(4,
             box(
               title = "Health System Metrics",
               status = "danger",
               solidHeader = TRUE,
               width = NULL,
               
               tableOutput(ns("health_system_metrics"))
             )
      )
    ),
    
    # Alert summary row
    fluidRow(
      column(6,
             box(
               title = "Health Alerts Summary",
               status = "warning",
               solidHeader = TRUE,
               width = NULL,
               
               h5("Current Alerts"),
               tableOutput(ns("alerts_summary")),
               
               br(),
               actionButton(ns("view_detailed_alerts"), "View Detailed Alerts",
                            class = "btn-warning", icon = icon("exclamation-triangle"))
             )
      ),
      column(6,
             box(
               title = "Data Quality Indicators",
               status = "info", 
               solidHeader = TRUE,
               width = NULL,
               
               h5("Data Completeness"),
               uiOutput(ns("data_quality_indicators")),
               
               br(),
               div(style = "text-align: center;",
                   actionButton(ns("refresh_overview"), "Refresh Overview",
                                class = "btn-primary", icon = icon("sync"))
               )
             )
      )
    )
  )
}

#' Overview Module Server
#'
#' @param id Character string. The module's ID
#' @param data Reactive. Filtered dataset
#' @param config List. Configuration parameters
#'
#' @return Overview metrics reactive
mod_overview_server <- function(id, data, config = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Helper function to get date column
    get_date_col <- function(df) {
      if ("datevisit" %in% colnames(df)) "datevisit"
      else if ("datevisitnew" %in% colnames(df)) "datevisitnew"
      else NULL
    }
    
    # Filtered data for map based on time selection
    map_filtered_data <- reactive({
      req(data(), input$map_time_filter)
      
      df <- data()
      date_col <- get_date_col(df)
      
      # Apply time filter
      if (!is.null(date_col) && input$map_time_filter != 9999) {
        current_date <- max(df[[date_col]], na.rm = TRUE)
        if (!is.na(current_date)) {
          df <- df %>%
            filter(.data[[date_col]] >= current_date - days(as.numeric(input$map_time_filter)))
        }
      }
      
      return(df)
    })
    
    # Calculate overview metrics
    overview_metrics <- reactive({
      req(data())
      
      df <- data()
      date_col <- get_date_col(df)
      
      # Basic counts
      total_consultations <- nrow(df)
      
      active_facilities <- if ("orgunit" %in% colnames(df)) {
        n_distinct(df$orgunit, na.rm = TRUE)
      } else { 0 }
      
      # Top disease - use canonical_disease_imc from new taxonomy system
      top_disease <- if ("canonical_disease_imc" %in% colnames(df)) {
        disease_counts <- table(df$canonical_disease_imc)
        if (length(disease_counts) > 0) {
          list(
            name = names(disease_counts)[which.max(disease_counts)],
            count = max(disease_counts)
          )
        } else {
          list(name = "Unknown", count = 0)
        }
      } else if ("category_canonical_disease_imc" %in% colnames(df)) {
        # Fallback to old system
        disease_counts <- table(df$category_canonical_disease_imc)
        if (length(disease_counts) > 0) {
          list(
            name = names(disease_counts)[which.max(disease_counts)],
            count = max(disease_counts)
          )
        } else {
          list(name = "Unknown", count = 0)
        }
      } else {
        list(name = "No data", count = 0)
      }
      
      # Recent trend
      recent_trend <- if (!is.null(date_col)) {
        current_date <- max(df[[date_col]], na.rm = TRUE)
        current_week <- sum(df[[date_col]] >= (current_date - 7), na.rm = TRUE)
        previous_week <- sum(df[[date_col]] >= (current_date - 14) & 
                            df[[date_col]] < (current_date - 7), na.rm = TRUE)
        
        if (previous_week > 0) {
          change_pct <- round(((current_week - previous_week) / previous_week) * 100, 1)
          list(
            change = change_pct,
            direction = if (change_pct > 5) "up" else if (change_pct < -5) "down" else "stable",
            current_week = current_week,
            previous_week = previous_week
          )
        } else {
          list(change = 0, direction = "stable", current_week = current_week, previous_week = 0)
        }
      } else {
        list(change = 0, direction = "stable", current_week = 0, previous_week = 0)
      }
      
      # Geographic coverage
      geographic_coverage <- if ("admin1" %in% colnames(df)) {
        n_distinct(df$admin1, na.rm = TRUE)
      } else { 0 }
      
      # Disease diversity - use canonical_disease_imc for better diversity measure
      disease_diversity <- if ("canonical_disease_imc" %in% colnames(df)) {
        n_distinct(df$canonical_disease_imc, na.rm = TRUE)
      } else if ("category_canonical_disease_imc" %in% colnames(df)) {
        n_distinct(df$category_canonical_disease_imc, na.rm = TRUE)
      } else { 0 }
      
      list(
        total_consultations = total_consultations,
        active_facilities = active_facilities,
        top_disease = top_disease,
        recent_trend = recent_trend,
        geographic_coverage = geographic_coverage,
        disease_diversity = disease_diversity,
        last_updated = Sys.time()
      )
    })
    
    # Value boxes
    output$total_consultations <- renderValueBox({
      metrics <- overview_metrics()
      
      valueBox(
        value = format(metrics$total_consultations, big.mark = ","),
        subtitle = "Total Consultations",
        icon = icon("user-md"),
        color = "blue"
      )
    })
    
    output$active_facilities <- renderValueBox({
      metrics <- overview_metrics()
      
      valueBox(
        value = metrics$active_facilities,
        subtitle = "Active Facilities",
        icon = icon("hospital"),
        color = "green"
      )
    })
    
    output$top_disease <- renderValueBox({
      metrics <- overview_metrics()
      
      valueBox(
        value = metrics$top_disease$count,
        subtitle = paste("Top Disease:", 
                        str_trunc(metrics$top_disease$name, 20)),
        icon = icon("virus"),
        color = "yellow"
      )
    })
    
    output$recent_trend <- renderValueBox({
      metrics <- overview_metrics()
      trend <- metrics$recent_trend
      
      icon_name <- switch(trend$direction,
                         "up" = "arrow-up",
                         "down" = "arrow-down",
                         "stable" = "minus")
      
      color <- switch(trend$direction,
                     "up" = "green",
                     "down" = "red", 
                     "stable" = "blue")
      
      valueBox(
        value = paste0(ifelse(trend$change > 0, "+", ""), trend$change, "%"),
        subtitle = "Weekly Trend",
        icon = icon(icon_name),
        color = color
      )
    })
    
    # Enhanced overview map with consultation volume
    output$overview_map <- renderLeaflet({
      df <- map_filtered_data()
      
      tryCatch({
        # Create enhanced map with consultation volume
        # Dynamic map coordinates based on country
        country_coords <- list(
          "Syria" = list(lng = 38.0, lat = 35.0, zoom = 6),
          "Yemen" = list(lng = 48.0, lat = 15.5, zoom = 6),
          "South Sudan" = list(lng = 30.0, lat = 7.0, zoom = 6),
          "default" = list(lng = 35.0, lat = 25.0, zoom = 5)  # Middle East/East Africa region
        )
        
        # Detect country from data
        country <- if (!is.null(df) && "admin0" %in% names(df)) unique(df$admin0)[1] else "default"
        coords <- country_coords[[country]] %||% country_coords[["default"]]
        
        map <- leaflet() %>%
          addTiles() %>%
          setView(lng = coords$lng, lat = coords$lat, zoom = coords$zoom) %>%
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
              trauma_cases = if("trauma_related" %in% names(df)) {
                # Use new taxonomy flag for trauma detection
                sum(trauma_related %||% FALSE, na.rm = TRUE)
              } else if("type_case" %in% names(df)) {
                # Fallback to old detection method
                sum(str_detect(tolower(type_case %||% ""), "trauma") & 
                    !str_detect(tolower(type_case %||% ""), "non"), na.rm = TRUE)
              } else 0,
              trauma_rate = if("trauma_related" %in% names(df)) {
                # Use new taxonomy flag for trauma rate
                round(sum(trauma_related %||% FALSE, na.rm = TRUE) / n() * 100, 1)
              } else if("type_case" %in% names(df)) {
                # Fallback to old detection method
                round(sum(str_detect(tolower(type_case %||% ""), "trauma") & 
                         !str_detect(tolower(type_case %||% ""), "non"), na.rm = TRUE) / n() * 100, 1)
              } else 0,
              disease_diversity = if("canonical_disease_imc" %in% names(df)) {
                n_distinct(canonical_disease_imc, na.rm = TRUE)
              } else if("category_canonical_disease_imc" %in% names(df)) {
                n_distinct(category_canonical_disease_imc, na.rm = TRUE)
              } else 0,
              top_disease = if("canonical_disease_imc" %in% names(df)) {
                disease_table <- table(canonical_disease_imc)
                if(length(disease_table) > 0) names(disease_table)[which.max(disease_table)] else "Unknown"
              } else if("category_canonical_disease_imc" %in% names(df)) {
                disease_table <- table(category_canonical_disease_imc)
                if(length(disease_table) > 0) names(disease_table)[which.max(disease_table)] else "Unknown"
              } else "Unknown",
              .groups = "drop"
            ) %>%
            arrange(desc(total_consultations))
          
          # Enhanced coordinates for Syrian governorates
          gov_coords <- data.frame(
            admin1 = c("Aleppo", "Damascus", "Homs", "Al-Hasakeh", "Ar-Raqqa", 
                       "Dar'a", "Deir-ez-Zor", "Idleb", "Lattakia", "Tartous", 
                       "As-Sweida", "Quneitra", "Rural Damascus", "Hama"),
            lng = c(37.16, 36.29, 36.73, 40.74, 39.01, 36.10, 40.14, 36.63, 
                    35.78, 35.88, 36.57, 35.82, 36.29, 35.13),
            lat = c(36.20, 33.51, 34.73, 36.50, 35.95, 32.62, 35.34, 35.93, 
                    35.53, 34.88, 32.71, 33.12, 33.51, 35.13),
            stringsAsFactors = FALSE
          )
          
          # Merge consultation data with coordinates
          map_data <- dplyr::left_join(gov_coords, gov_data, by = "admin1") %>%
            mutate(
              total_consultations = replace_na(total_consultations, 0),
              facilities = replace_na(facilities, 0),
              consultations_per_facility = replace_na(consultations_per_facility, 0),
              trauma_rate = replace_na(trauma_rate, 0),
              disease_diversity = replace_na(disease_diversity, 0),
              top_disease = replace_na(top_disease, "No data")
            )
          
          # Create color palette based on selected metric
          metric_col <- switch(input$map_metric,
                              "total_consultations" = "total_consultations",
                              "consultations_per_facility" = "consultations_per_facility",
                              "trauma_rate" = "trauma_rate",
                              "disease_diversity" = "disease_diversity",
                              "total_consultations")
          
          metric_values <- map_data[[metric_col]]
          
          if(max(metric_values, na.rm = TRUE) > 0) {
            pal <- colorNumeric(
              palette = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#99000D"),
              domain = c(0, max(metric_values, na.rm = TRUE))
            )
          } else {
            pal <- colorNumeric(palette = "Blues", domain = c(0, 1))
          }
          
          # Add circle markers for each governorate
          for (i in 1:nrow(map_data)) {
            row <- map_data[i, ]
            
            # Calculate marker size based on selected metric
            base_radius <- 8
            max_radius <- 25
            if(max(metric_values, na.rm = TRUE) > 0) {
              radius <- base_radius + (row[[metric_col]] / max(metric_values, na.rm = TRUE)) * (max_radius - base_radius)
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
              "<span><i class='fas fa-dna' style='color: #9b59b6;'></i> Disease Diversity:</span>",
              "<strong style='color: #8e44ad;'>", row$disease_diversity, "</strong>",
              "</div>",
              "<div style='display: flex; justify-content: space-between; margin: 5px 0;'>",
              "<span><i class='fas fa-virus' style='color: #9b59b6;'></i> Top Disease:</span>",
              "<strong style='color: #8e44ad;'>", substr(row$top_disease, 1, 20), if(nchar(row$top_disease) > 20) "..." else "", "</strong>",
              "</div>",
              "</div>"
            )
            
            # Determine marker color based on selected metric
            marker_color <- if(row[[metric_col]] > 0) {
              pal(row[[metric_col]])
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
                label = paste0(row$admin1, ": ", format(row[[metric_col]], big.mark = ","), " ", gsub("_", " ", input$map_metric)),
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
          
          # Add legend for selected metric
          if(max(metric_values, na.rm = TRUE) > 0) {
            map <- map %>%
              addLegend(
                pal = pal,
                values = metric_values,
                opacity = 0.7,
                title = str_to_title(gsub("_", " ", input$map_metric)),
                position = "bottomright",
                labFormat = labelFormat(big.mark = ",")
              )
          }
          
          # Add summary information
          total_consultations <- sum(map_data$total_consultations, na.rm = TRUE)
          total_facilities <- sum(map_data$facilities, na.rm = TRUE)
          active_govs <- sum(map_data$total_consultations > 0)
          
          # Add control box with summary
          map <- map %>%
            addControl(
              html = paste0(
                "<div style='background: rgba(255,255,255,0.9); padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.2); font-family: Arial, sans-serif;'>",
                "<h4 style='margin: 0 0 10px 0; color: #2c3e50; font-size: 16px;'><i class='fas fa-chart-pie'></i> Syria Health Surveillance</h4>",
                "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 10px; font-size: 12px;'>",
                "<div><strong>Total Consultations:</strong><br/><span style='color: #2980b9; font-size: 14px; font-weight: bold;'>", format(total_consultations, big.mark = ","), "</span></div>",
                "<div><strong>Active Governorates:</strong><br/><span style='color: #27ae60; font-size: 14px; font-weight: bold;'>", active_govs, "/", nrow(map_data), "</span></div>",
                "<div><strong>Health Facilities:</strong><br/><span style='color: #f39c12; font-size: 14px; font-weight: bold;'>", total_facilities, "</span></div>",
                "<div><strong>Data Status:</strong><br/><span style='color: #27ae60; font-size: 14px; font-weight: bold;'>Active</span></div>",
                "</div>",
                "<div style='margin-top: 10px; font-size: 11px; color: #7f8c8d;'>",
                "Click markers for detailed information<br/>",
                "Marker size indicates ", tolower(str_to_title(gsub("_", " ", input$map_metric))),
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
    
    # Main disease trends plot
    output$overview_disease_trends <- renderPlotly({
      req(data(), input$overview_time_period)
      
      df <- data()
      date_col <- get_date_col(df)
      
      # Check for disease columns - prioritize canonical_disease_imc
      disease_col_to_use <- if ("canonical_disease_imc" %in% colnames(df)) {
        "canonical_disease_imc"
      } else if ("category_canonical_disease_imc" %in% colnames(df)) {
        "category_canonical_disease_imc" 
      } else {
        NULL
      }
      
      if (is.null(date_col) || is.null(disease_col_to_use)) {
        return(plot_ly() %>%
                 add_annotations(
                   text = "Disease trend data not available",
                   x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                   showarrow = FALSE
                 ))
      }
      
      # Filter by time period
      current_date <- max(df[[date_col]], na.rm = TRUE)
      days_back <- as.numeric(input$overview_time_period)
      
      filtered_df <- df %>%
        filter(.data[[date_col]] >= current_date - days(days_back))
      
      # Get top diseases using the determined column
      top_diseases <- filtered_df %>%
        filter(!is.na(.data[[disease_col_to_use]])) %>%
        count(.data[[disease_col_to_use]], sort = TRUE) %>%
        slice_head(n = input$overview_top_diseases) %>%
        pull(.data[[disease_col_to_use]])
      
      # Create trend data
      trends_data <- filtered_df %>%
        filter(.data[[disease_col_to_use]] %in% top_diseases) %>%
        mutate(week = floor_date(.data[[date_col]], "week")) %>%
        count(week, .data[[disease_col_to_use]]) %>%
        arrange(week) %>%
        rename(disease = !!disease_col_to_use)
      
      if (nrow(trends_data) == 0) {
        return(plot_ly() %>%
                 add_annotations(
                   text = "No data available for selected time period",
                   x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                   showarrow = FALSE
                 ))
      }
      
      # Create plot based on chart type
      if (input$overview_chart_type == "area") {
        p <- ggplot(trends_data, aes(x = week, y = n, fill = disease)) +
          geom_area(alpha = 0.7, position = "stack") +
          scale_fill_brewer(type = "qual", palette = "Set1")
      } else if (input$overview_chart_type == "bar") {
        p <- ggplot(trends_data, aes(x = week, y = n, fill = disease)) +
          geom_col(position = "dodge", alpha = 0.8) +
          scale_fill_brewer(type = "qual", palette = "Set1")
      } else {
        p <- ggplot(trends_data, aes(x = week, y = n, color = disease)) +
          geom_line(size = 1.2, alpha = 0.8) +
          geom_point(size = 2, alpha = 0.8) +
          scale_color_brewer(type = "qual", palette = "Set1")
      }
      
      p <- p +
        labs(
          title = "Top Disease Trends Over Time",
          x = "Week",
          y = "Number of Cases",
          color = "Disease Category",
          fill = "Disease Category"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_y_continuous(labels = comma_format())
      
      ggplotly(p, tooltip = c("x", "y", "colour", "fill")) %>%
        layout(
          hovermode = "x unified",
          legend = list(orientation = "h", x = 0, y = -0.2)
        )
    })
    
    # Quick statistics table
    output$quick_stats_table <- renderTable({
      metrics <- overview_metrics()
      
      stats <- tibble(
        Metric = c(
          "Total Consultations",
          "Active Facilities",
          "Disease Categories", 
          "Geographic Coverage",
          "Weekly Change"
        ),
        Value = c(
          format(metrics$total_consultations, big.mark = ","),
          metrics$active_facilities,
          metrics$disease_diversity,
          paste(metrics$geographic_coverage, "regions"),
          paste0(ifelse(metrics$recent_trend$change > 0, "+", ""), 
                 metrics$recent_trend$change, "%")
        )
      )
      
      stats
    }, striped = TRUE, hover = TRUE)
    
    # Recent activity content
    output$recent_activity_content <- renderUI({
      req(data())
      
      df <- data()
      date_col <- get_date_col(df)
      
      if (is.null(date_col)) {
        return(p("No recent activity data available"))
      }
      
      # Get recent activity (last 7 days)
      current_date <- max(df[[date_col]], na.rm = TRUE)
      recent_activity <- df %>%
        filter(.data[[date_col]] >= current_date - 7) %>%
        nrow()
      
      daily_avg <- round(recent_activity / 7, 1)
      
      tagList(
        p(strong("Last 7 days:"), format(recent_activity, big.mark = ","), "cases"),
        p(strong("Daily average:"), daily_avg, "cases"),
        p(strong("Last update:"), format(current_date, "%Y-%m-%d"))
      )
    })
    
    # Map legend content
    output$map_legend_content <- renderUI({
      tagList(
        p(style = "font-size: 12px; margin: 5px 0;", 
          strong("Map Features:")),
        tags$ul(style = "font-size: 11px; margin: 0; padding-left: 15px;",
          tags$li("Circle size = metric value"),
          tags$li("Color intensity = volume"),
          tags$li("Click markers for details"),
          tags$li("Use controls to change metric")
        )
      )
    })
    
    # Geographic overview
    output$geographic_overview <- renderPlotly({
      req(data())
      
      df <- data()
      
      if (!"admin1" %in% colnames(df)) {
        return(plot_ly() %>% add_annotations(text = "Geographic data not available"))
      }
      
      geo_data <- df %>%
        filter(!is.na(admin1)) %>%
        count(admin1, sort = TRUE) %>%
        slice_head(n = 8)  # Top 8 regions
      
      p <- ggplot(geo_data, aes(x = reorder(admin1, n), y = n)) +
        geom_col(fill = "#2E86AB", alpha = 0.8) +
        coord_flip() +
        labs(
          title = "Cases by Region",
          x = "",
          y = "Cases"
        ) +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 9)) +
        scale_y_continuous(labels = comma_format())
      
      ggplotly(p, tooltip = c("x", "y"))
    })
    
    # Disease categories overview - use category_canonical_disease_imc for disease categories
    output$disease_categories_overview <- renderPlotly({
      req(data())
      
      df <- data()
      
      if (!"category_canonical_disease_imc" %in% colnames(df)) {
        return(plot_ly() %>% add_annotations(text = "Disease categories not available"))
      }
      
      disease_data <- df %>%
        filter(!is.na(category_canonical_disease_imc)) %>%
        count(category_canonical_disease_imc, sort = TRUE) %>%
        slice_head(n = 8)
      
      plot_ly(disease_data, labels = ~category_canonical_disease_imc, values = ~n, type = 'pie',
              textposition = 'inside', textinfo = 'label+percent',
              hoverinfo = 'label+value+percent') %>%
        layout(
          title = "Disease Distribution",
          showlegend = FALSE,
          margin = list(t = 40, b = 0, l = 0, r = 0)
        )
    })
    
    # Health system metrics
    output$health_system_metrics <- renderTable({
      req(data())
      
      df <- data()
      
      # Calculate health system metrics
      consultations_per_facility <- if ("orgunit" %in% colnames(df)) {
        round(nrow(df) / pmax(n_distinct(df$orgunit, na.rm = TRUE), 1), 1)
      } else { 0 }
      
      utilization_score <- if (consultations_per_facility > 100) "High" 
                          else if (consultations_per_facility > 50) "Medium" 
                          else "Low"
      
      case_complexity <- if ("chronic_condition" %in% colnames(df)) {
        # Use new taxonomy flag for chronic conditions
        complex_cases <- sum(df$chronic_condition %||% FALSE, na.rm = TRUE)
        round(complex_cases / nrow(df) * 100, 1)
      } else if ("category_canonical_disease_imc" %in% colnames(df)) {
        # Fallback to old system
        complex_diseases <- c("Cancer", "Diabetes mellitus", "Hypertension", 
                             "Stroke/cerebrovascular accident")
        complex_cases <- sum(df$category_canonical_disease_imc %in% complex_diseases, na.rm = TRUE)
        round(complex_cases / nrow(df) * 100, 1)
      } else { 0 }
      
      metrics <- tibble(
        Metric = c(
          "Consultations/Facility",
          "System Utilization", 
          "Complex Cases (%)",
          "Data Coverage"
        ),
        Value = c(
          consultations_per_facility,
          utilization_score,
          paste0(case_complexity, "%"),
          paste0(round(sum(complete.cases(df)) / nrow(df) * 100, 1), "%")
        )
      )
      
      metrics
    }, striped = TRUE, hover = TRUE)
    
    # Alerts summary
    output$alerts_summary <- renderTable({
      req(data())
      
      df <- data()
      
      # Use new taxonomy system for epidemic disease detection
      epidemic_diseases <- if (exists("get_epidemic_groups")) {
        # Get diseases from new taxonomy system
        epidemic_groups <- get_epidemic_groups()
        unlist(epidemic_groups, use.names = FALSE)
      } else if (exists("get_epidemic_diseases_standardized")) {
        unlist(get_epidemic_diseases_standardized(), use.names = FALSE)
      } else {
        c("Acute Watery Diarrhea", "Suspected Measles", "Pneumonia")
      }
      
      date_col <- get_date_col(df)
      
      if (!is.null(date_col)) {
        current_date <- max(df[[date_col]], na.rm = TRUE)
        
        # Use new taxonomy flags for epidemic detection if available
        recent_epidemic_cases <- if ("epidemic_prone" %in% colnames(df) || "epidemic_group" %in% colnames(df)) {
          df %>%
            filter(.data[[date_col]] >= current_date - 7,
                   (if("epidemic_prone" %in% names(.)) epidemic_prone else FALSE) %||% FALSE |
                   !is.na(if("epidemic_group" %in% names(.)) epidemic_group else NA)) %>%
            nrow()
        } else if ("morbidity" %in% colnames(df)) {
          # Fallback to old method
          df %>%
            filter(.data[[date_col]] >= current_date - 7,
                   morbidity %in% epidemic_diseases) %>%
            nrow()
        } else { 0 }
        
        alert_level <- if (recent_epidemic_cases >= 50) "High"
                      else if (recent_epidemic_cases >= 20) "Medium"
                      else "Low"
        
        alerts <- tibble(
          Alert_Type = c("Epidemic Diseases", "Data Quality", "System Status"),
          Level = c(alert_level, "Normal", "Normal"),
          Count = c(recent_epidemic_cases, 0, 0)
        )
      } else {
        alerts <- tibble(
          Alert_Type = "No alerts",
          Level = "Normal", 
          Count = 0
        )
      }
      
      alerts
    }, striped = TRUE, hover = TRUE)
    
    # Data quality indicators
    output$data_quality_indicators <- renderUI({
      req(data())
      
      df <- data()
      
      # Calculate completeness metrics
      overall_completeness <- round(sum(complete.cases(df)) / nrow(df) * 100, 1)
      
      # Key field completeness
      key_fields <- c("category_canonical_disease_imc", "admin1", "orgunit")
      field_completeness <- map_dbl(key_fields, function(field) {
        if (field %in% colnames(df)) {
          round(sum(!is.na(df[[field]])) / nrow(df) * 100, 1)
        } else { 0 }
      })
      names(field_completeness) <- key_fields
      
      tagList(
        div(
          p(strong("Overall Completeness: "), 
            span(paste0(overall_completeness, "%"), 
                 style = paste0("color: ", if (overall_completeness >= 80) "green" else "orange")))
        ),
        div(
          p(strong("Key Fields:")),
          tags$ul(
            lapply(seq_along(field_completeness), function(i) {
              field <- names(field_completeness)[i]
              pct <- field_completeness[i]
              color <- if (pct >= 90) "green" else if (pct >= 75) "orange" else "red"
              tags$li(paste0(str_to_title(gsub("_", " ", field)), ": "), 
                     span(paste0(pct, "%"), style = paste0("color: ", color)))
            })
          )
        )
      )
    })
    
    # Event handlers
    observeEvent(input$view_detailed_alerts, {
      showNotification("Detailed alerts feature coming soon", type = "info", duration = 3)
    })
    
    observeEvent(input$refresh_overview, {
      # Trigger reactivity by updating a reactive value
      showNotification("Overview refreshed", type = "success", duration = 2)
    })
    
    # Download handlers
    output$download_map_data <- downloadHandler(
      filename = function() {
        paste0("syria_map_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(map_filtered_data())
        
        df <- map_filtered_data()
        
        # Create map summary data
        if ("admin1" %in% colnames(df)) {
          map_summary <- df %>%
            group_by(admin1) %>%
            summarise(
              total_consultations = n(),
              facilities = if("orgunit" %in% names(df)) n_distinct(orgunit, na.rm = TRUE) else 0,
              consultations_per_facility = if("orgunit" %in% names(df)) {
                round(n() / pmax(n_distinct(orgunit, na.rm = TRUE), 1), 1)
              } else 0,
              trauma_rate = if("trauma_related" %in% names(df)) {
                # Use new taxonomy flag for trauma rate
                round(sum(trauma_related %||% FALSE, na.rm = TRUE) / n() * 100, 1)
              } else if("type_case" %in% names(df)) {
                # Fallback to old detection method
                round(sum(str_detect(tolower(type_case %||% ""), "trauma") & 
                         !str_detect(tolower(type_case %||% ""), "non"), na.rm = TRUE) / n() * 100, 1)
              } else 0,
              disease_diversity = if("canonical_disease_imc" %in% names(df)) {
                n_distinct(canonical_disease_imc, na.rm = TRUE) 
              } else if("category_canonical_disease_imc" %in% names(df)) {
                n_distinct(category_canonical_disease_imc, na.rm = TRUE)
              } else 0,
              .groups = "drop"
            ) %>%
            mutate(
              export_date = Sys.Date(),
              time_filter = input$map_time_filter,
              metric_displayed = input$map_metric
            )
          
          write.csv(map_summary, file, row.names = FALSE)
        } else {
          # Fallback if no geographic data
          tibble(message = "No geographic data available") %>%
            write.csv(file, row.names = FALSE)
        }
      }
    )
    
    output$download_trends_data <- downloadHandler(
      filename = function() {
        paste0("disease_trends_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(data())
        
        df <- data()
        date_col <- get_date_col(df)
        
        # Determine which disease column to use for export
        disease_export_col <- if ("canonical_disease_imc" %in% colnames(df)) {
          "canonical_disease_imc"
        } else if ("category_canonical_disease_imc" %in% colnames(df)) {
          "category_canonical_disease_imc"
        } else { NULL }
        
        if (!is.null(date_col) && !is.null(disease_export_col)) {
          # Create trends export
          current_date <- max(df[[date_col]], na.rm = TRUE)
          days_back <- as.numeric(input$overview_time_period)
          
          trends_export <- df %>%
            filter(.data[[date_col]] >= current_date - days(days_back)) %>%
            filter(!is.na(.data[[disease_export_col]])) %>%
            mutate(week = floor_date(.data[[date_col]], "week")) %>%
            count(week, .data[[disease_export_col]]) %>%
            arrange(week, .data[[disease_export_col]]) %>%
            rename(disease = !!disease_export_col) %>%
            mutate(
              export_date = Sys.Date(),
              time_period_days = days_back,
              chart_type = input$overview_chart_type
            )
          
          write.csv(trends_export, file, row.names = FALSE)
        } else {
          # Fallback if no trends data
          tibble(message = "No trends data available") %>%
            write.csv(file, row.names = FALSE)
        }
      }
    )
    
    # Return overview metrics for use by other modules
    return(overview_metrics)
  })
}