# modules/advanced/multi_disease_correlation.R - Dashboard module

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────

#' Multi-Disease Correlation Module UI
multi_disease_correlation_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             box(
               title = "Multi-Disease Correlation Analysis",
               status = "warning",
               solidHeader = TRUE,
               width = NULL,
               
               tabsetPanel(
                 # Correlation Matrix Tab
                 tabPanel(
                   "Correlation Matrix",
                   br(),
                   fluidRow(
                     column(3,
                            wellPanel(
                              h5("Analysis Parameters"),
                              selectInput(ns("correlation_type"), "Correlation Type:",
                                          choices = list(
                                            "Temporal (Time-based)" = "temporal",
                                            "Geographic (Location-based)" = "geographic",
                                            "Demographic (Age/Sex)" = "demographic"
                                          )),
                              selectInput(ns("time_window"), "Time Window:",
                                          choices = list(
                                            "Weekly" = "week",
                                            "Monthly" = "month",
                                            "Quarterly" = "quarter"
                                          )),
                              pickerInput(ns("selected_diseases"), "Select Diseases:",
                                          choices = NULL,
                                          multiple = TRUE,
                                          options = pickerOptions(
                                            actionsBox = TRUE,
                                            maxOptions = 15,
                                            selectedTextFormat = "count > 3"
                                          )),
                              numericInput(ns("min_correlation"), "Minimum Correlation:",
                                           value = 0.3, min = 0, max = 1, step = 0.1),
                              actionButton(ns("calculate_correlation"), "Calculate",
                                           class = "btn-warning")
                            )
                     ),
                     column(9,
                            plotlyOutput(ns("correlation_heatmap"), height = "500px")
                     )
                   )
                 ),
                 
                 # Co-occurrence Patterns Tab
                 tabPanel(
                   "Co-occurrence Patterns",
                   br(),
                   fluidRow(
                     column(6,
                            box(
                              title = "Disease Co-occurrence Network",
                              width = NULL,
                              plotlyOutput(ns("cooccurrence_network"), height = "400px")
                            )
                     ),
                     column(6,
                            box(
                              title = "Temporal Co-occurrence",
                              width = NULL,
                              plotlyOutput(ns("temporal_cooccurrence"), height = "400px")
                            )
                     )
                   ),
                   fluidRow(
                     column(12,
                            box(
                              title = "Co-occurrence Statistics",
                              width = NULL,
                              DT::dataTableOutput(ns("cooccurrence_table"))
                            )
                     )
                   )
                 ),
                 
                 # Syndromic Clustering Tab
                 tabPanel(
                   "Syndromic Clustering",
                   br(),
                   fluidRow(
                     column(4,
                            wellPanel(
                              h5("Clustering Parameters"),
                              selectInput(ns("clustering_method"), "Method:",
                                          choices = list(
                                            "K-means" = "kmeans",
                                            "Hierarchical" = "hierarchical",
                                            "DBSCAN" = "dbscan"
                                          )),
                              numericInput(ns("num_clusters"), "Number of Clusters:",
                                           value = 5, min = 2, max = 15),
                              selectInput(ns("clustering_features"), "Feature Set:",
                                          choices = list(
                                            "Disease frequencies" = "frequency",
                                            "Temporal patterns" = "temporal",
                                            "Geographic patterns" = "geographic",
                                            "Combined features" = "combined"
                                          )),
                              actionButton(ns("perform_clustering"), "Cluster Diseases",
                                           class = "btn-info")
                            )
                     ),
                     column(8,
                            plotlyOutput(ns("clustering_plot"), height = "500px")
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
# Server
# ─────────────────────────────────────────────────────────────────────────────

#' Calculate multi-disease correlations
#' @param data Data frame with health consultation data
#' @param correlation_type Character. Type of correlation analysis
#' @param time_window Character. Time aggregation window
#' @param selected_diseases Character vector. Diseases to include
#' @return List with correlation matrix and metadata

resolve_disease_column <- function(data) {
  candidates <- c("category_canonical_disease_imc", "canonical_disease_imc",
                  "standardized_disease_global", "morbidity")
  matched <- candidates[candidates %in% names(data)]
  if (length(matched)) matched[1] else NULL
}

resolve_date_column <- function(data) {
  candidates <- c("datevisitnew", "datevisit", "visit_date", "date")
  matched <- candidates[candidates %in% names(data)]
  if (length(matched)) matched[1] else NULL
}

resolve_entity_column <- function(data) {
  candidates <- c("patientid", "patient_id", "household_id", "orgunit", "facility", "admin1")
  matched <- candidates[candidates %in% names(data)]
  if (length(matched)) matched[1] else NULL
}

calculate_disease_correlations <- function(data, correlation_type, time_window, selected_diseases) {
  
  disease_col <- resolve_disease_column(data)
  if (is.null(disease_col)) {
    return(list(matrix = NULL, long_format = NULL, method = correlation_type,
                message = "Disease classifications not found in dataset"))
  }
  
  analysis_data <- data %>%
    filter(!is.na(.data[[disease_col]])) %>%
    mutate(category_canonical_disease_imc = .data[[disease_col]]) %>%
    filter(category_canonical_disease_imc %in% selected_diseases)
  
  if (length(unique(analysis_data$category_canonical_disease_imc)) < 2) {
    return(list(matrix = NULL, long_format = NULL, method = correlation_type,
                message = "Need at least two diseases for correlation analysis"))
  }
  
  correlation_result <- switch(correlation_type,
                               "temporal" = calculate_temporal_correlations(analysis_data, time_window),
                               "geographic" = calculate_geographic_correlations(analysis_data),
                               "demographic" = calculate_demographic_correlations(analysis_data)
  )
  
  return(correlation_result)
}

#' Calculate temporal correlations between diseases
calculate_temporal_correlations <- function(data, time_window) {
  date_col <- resolve_date_column(data)
  if (is.null(date_col)) {
    return(list(matrix = NULL, long_format = NULL, method = "temporal",
                message = "Date column not found for temporal analysis"))
  }
  
  data <- data %>% filter(!is.na(.data[[date_col]]))
  if (nrow(data) == 0) {
    return(list(matrix = NULL, long_format = NULL, method = "temporal",
                message = "No records available after filtering dates"))
  }
  
  time_aggregated <- data %>%
    mutate(
      time_period = case_when(
        time_window == "week" ~ floor_date(.data[[date_col]], "week"),
        time_window == "month" ~ floor_date(.data[[date_col]], "month"),
        time_window == "quarter" ~ floor_date(.data[[date_col]], "quarter"),
        TRUE ~ floor_date(.data[[date_col]], "month")
      )
    ) %>%
    count(time_period, category_canonical_disease_imc) %>%
    pivot_wider(names_from = category_canonical_disease_imc, values_from = n, values_fill = 0)
  
  disease_cols <- dplyr::select(time_aggregated, -time_period)
  if (ncol(disease_cols) < 2 || nrow(disease_cols) < 2) {
    return(list(matrix = NULL, long_format = NULL, time_data = time_aggregated, method = "temporal",
                message = "Not enough data points to compute temporal correlations"))
  }
  
  correlation_matrix <- suppressWarnings(cor(disease_cols, use = "complete.obs"))
  correlation_long <- correlation_matrix %>%
    as.data.frame() %>%
    rownames_to_column("disease1") %>%
    pivot_longer(-disease1, names_to = "disease2", values_to = "correlation") %>%
    filter(disease1 != disease2)
  
  list(matrix = correlation_matrix,
       long_format = correlation_long,
       time_data = time_aggregated,
       method = "temporal")
}

#' Calculate geographic correlations between diseases
calculate_geographic_correlations <- function(data) {
  geo_cols <- intersect(c("admin1", "admin2", "admin3"), names(data))
  if (length(geo_cols) == 0) {
    return(list(matrix = NULL, long_format = NULL, method = "geographic",
                message = "Geographic columns not found for correlation analysis"))
  }
  
  geo_aggregated <- data %>%
    filter(!if_any(all_of(geo_cols), is.na)) %>%
    count(across(all_of(geo_cols)), category_canonical_disease_imc) %>%
    pivot_wider(names_from = category_canonical_disease_imc, values_from = n, values_fill = 0)
  
  disease_cols <- dplyr::select(geo_aggregated, -all_of(geo_cols))
  if (ncol(disease_cols) < 2 || nrow(disease_cols) < 2) {
    return(list(matrix = NULL, long_format = NULL, geo_data = geo_aggregated, method = "geographic",
                message = "Not enough geographic variation to compute correlations"))
  }
  
  correlation_matrix <- suppressWarnings(cor(disease_cols, use = "complete.obs"))
  correlation_long <- correlation_matrix %>%
    as.data.frame() %>%
    rownames_to_column("disease1") %>%
    pivot_longer(-disease1, names_to = "disease2", values_to = "correlation") %>%
    filter(disease1 != disease2)
  
  list(matrix = correlation_matrix,
       long_format = correlation_long,
       geo_data = geo_aggregated,
       method = "geographic")
}

calculate_demographic_correlations <- function(data) {
  demo_cols <- intersect(c("sex", "gender", "age_group_new", "age_group"), names(data))
  if (length(demo_cols) == 0) {
    return(list(matrix = NULL, long_format = NULL, method = "demographic",
                message = "No demographic columns available"))
  }

  demographic_data <- data
  demographic_data$demographic_group <- NA_character_

  if ("sex" %in% demo_cols) {
    demographic_data$demographic_group <- ifelse(is.na(demographic_data$demographic_group) | demographic_data$demographic_group == "",
                                                 as.character(demographic_data$sex),
                                                 demographic_data$demographic_group)
  }
  if ("gender" %in% demo_cols) {
    demographic_data$demographic_group <- ifelse(is.na(demographic_data$demographic_group) | demographic_data$demographic_group == "",
                                                 as.character(demographic_data$gender),
                                                 demographic_data$demographic_group)
  }
  other_demo <- setdiff(demo_cols, c("sex", "gender"))
  if (length(other_demo) > 0) {
    fallback_col <- other_demo[1]
    demographic_data$demographic_group <- ifelse(is.na(demographic_data$demographic_group) | demographic_data$demographic_group == "",
                                                 as.character(demographic_data[[fallback_col]]),
                                                 demographic_data$demographic_group)
  }

  demographic_data <- demographic_data %>%
    filter(!is.na(demographic_group), demographic_group != "")

  if (nrow(demographic_data) == 0) {
    return(list(matrix = NULL, long_format = NULL, method = "demographic",
                message = "No demographic data available"))
  }

  demo_aggregated <- demographic_data %>%
    count(demographic_group, category_canonical_disease_imc) %>%
    pivot_wider(names_from = category_canonical_disease_imc, values_from = n, values_fill = 0)

  disease_cols <- dplyr::select(demo_aggregated, -demographic_group)
  if (ncol(disease_cols) < 2 || nrow(disease_cols) < 2) {
    return(list(matrix = NULL, long_format = NULL, method = "demographic",
                message = "Insufficient demographic groups for correlation analysis"))
  }

  correlation_matrix <- suppressWarnings(cor(disease_cols, use = "complete.obs"))
  correlation_long <- correlation_matrix %>%
    as.data.frame() %>%
    rownames_to_column("disease1") %>%
    pivot_longer(-disease1, names_to = "disease2", values_to = "correlation") %>%
    filter(disease1 != disease2)

  list(matrix = correlation_matrix,
       long_format = correlation_long,
       demo_data = demo_aggregated,
       method = "demographic")
}

#' Identify disease co-occurrence patterns
#' @param data Data frame with health consultation data
#' @param time_window Integer. Days to consider for co-occurrence
#' @return Data frame with co-occurrence statistics
analyze_disease_cooccurrence <- function(data, time_window = 30) {
  disease_col <- resolve_disease_column(data)
  if (is.null(disease_col)) {
    return(tibble())
  }
  date_col <- resolve_date_column(data)
  entity_col <- resolve_entity_column(data)
  if (is.null(date_col) || is.null(entity_col)) {
    return(tibble())
  }

  analysis_data <- data %>%
    filter(!is.na(.data[[date_col]]), !is.na(.data[[entity_col]]), !is.na(.data[[disease_col]])) %>%
    mutate(
      category_canonical_disease_imc = .data[[disease_col]],
      entity_id = .data[[entity_col]]
    )
  if (nrow(analysis_data) == 0) {
    return(tibble())
  }

  analysis_data <- analysis_data %>%
    arrange(entity_id, .data[[date_col]]) %>%
    group_by(entity_id) %>%
    mutate(
      visit_group = cumsum(c(1, diff(as.numeric(.data[[date_col]])) > time_window))
    ) %>%
    group_by(entity_id, visit_group) %>%
    summarise(
      diseases = list(unique(category_canonical_disease_imc)),
      visit_dates = list(.data[[date_col]]),
      .groups = "drop"
    ) %>%
    filter(lengths(diseases) > 1)

  # Calculate pairwise co-occurrences
  cooccurrence_pairs <- analysis_data %>%
    rowwise() %>%
    do({
      disease_pairs <- combn(.$diseases[[1]], 2, simplify = FALSE)
      tibble(
        disease1 = purrr::map_chr(disease_pairs, 1),
        disease2 = purrr::map_chr(disease_pairs, 2),
        entity_id = .$entity_id,
        visit_group = .$visit_group
      )
    }) %>%
    ungroup()

  if (nrow(cooccurrence_pairs) == 0) {
    return(tibble())
  }

  # Calculate co-occurrence statistics
  cooccurrence_stats <- cooccurrence_pairs %>%
    count(disease1, disease2, name = "cooccurrence_count") %>%
    # Add individual disease counts
    left_join(
      data %>% count(category_canonical_disease_imc, name = "disease1_total") %>% rename(disease1 = category_canonical_disease_imc),
      by = "disease1"
    ) %>%
    left_join(
      data %>% count(category_canonical_disease_imc, name = "disease2_total") %>% rename(disease2 = category_canonical_disease_imc),
      by = "disease2"
    ) %>%
    mutate(
      # Calculate lift (observed/expected ratio)
      expected_cooccurrence = (disease1_total * disease2_total) / max(nrow(analysis_data), 1),
      lift = ifelse(expected_cooccurrence > 0, cooccurrence_count / expected_cooccurrence, NA_real_),
      
      # Calculate support (proportion of total cases)
      support = cooccurrence_count / max(nrow(analysis_data), 1),
      
      # Calculate confidence (conditional probability)
      confidence_1to2 = cooccurrence_count / disease1_total,
      confidence_2to1 = cooccurrence_count / disease2_total,
      
      # Statistical significance
      p_value = purrr::map2_dbl(cooccurrence_count, expected_cooccurrence, 
                                ~ifelse(is.na(.y) || .y <= 0, NA_real_, poisson.test(.x, r = .y)$p.value)),
      significant = ifelse(is.na(p_value), FALSE, p_value < 0.05)
    ) %>%
    arrange(desc(lift))
  
  attr(cooccurrence_stats, "entity_col") <- entity_col
  attr(cooccurrence_stats, "date_col") <- date_col
  return(cooccurrence_stats)
}

#' Perform syndromic disease clustering
#' @param data Data frame with health consultation data
#' @param method Character. Clustering method
#' @param num_clusters Integer. Number of clusters (for k-means)
#' @param feature_set Character. Features to use for clustering
#' @return List with clustering results
perform_disease_clustering <- function(data, method = "kmeans", num_clusters = 5, feature_set = "frequency") {
  
  # Prepare feature matrix
  feature_matrix <- prepare_clustering_features(data, feature_set)
  if (is.null(dim(feature_matrix)) || nrow(feature_matrix) == 0 || ncol(feature_matrix) == 0) {
    stop("Insufficient data to compute clustering features")
  }
  
  if (nrow(feature_matrix) < 2) {
    stop("Need at least two diseases for clustering analysis")
  }
  
  # Perform clustering
  clustering_result <- switch(method,
                              "kmeans" = {
                                num_clusters <- min(num_clusters, nrow(feature_matrix))
                                if (num_clusters < 2) {
                                  stop("Not enough diseases to create multiple clusters")
                                }
                                set.seed(123)
                                kmeans(feature_matrix, centers = num_clusters, nstart = 25)
                              },
                              "hierarchical" = {
                                dist_matrix <- dist(feature_matrix)
                                hclust(dist_matrix, method = "ward.D2")
                              },
                              "dbscan" = {
                                if (!requireNamespace("dbscan", quietly = TRUE)) {
                                  stop("Package 'dbscan' is required for DBSCAN clustering", call. = FALSE)
                                }
                                dbscan(feature_matrix, eps = 0.5, minPts = 2)
                              }
  )
  
  # Extract cluster assignments
  if (method == "hierarchical") {
    k <- min(num_clusters, nrow(feature_matrix))
    if (k < 2) {
      stop("Not enough observations to cut hierarchical clustering into requested clusters")
    }
    cluster_assignments <- cutree(clustering_result, k = k)
  } else {
    cluster_assignments <- clustering_result$cluster
  }
  
  # Create results summary
  cluster_summary <- tibble(
    disease = rownames(feature_matrix),
    cluster = cluster_assignments
  ) %>%
    group_by(cluster) %>%
    summarise(
      diseases = list(disease),
      cluster_size = n(),
      .groups = "drop"
    )
  
  return(list(
    model = clustering_result,
    assignments = cluster_assignments,
    summary = cluster_summary,
    features = feature_matrix,
    method = method
  ))
}

#' Prepare features for disease clustering
prepare_clustering_features <- function(data, feature_set) {
  disease_col <- resolve_disease_column(data)
  if (is.null(disease_col)) {
    return(matrix(numeric(0), nrow = 0, ncol = 0))
  }
  
  data <- data %>% mutate(category_canonical_disease_imc = .data[[disease_col]])
  if (nrow(data) == 0) {
    return(matrix(numeric(0), nrow = 0, ncol = 0))
  }

  date_col <- resolve_date_column(data)

  features <- switch(feature_set,
    "frequency" = {
      data %>%
        count(category_canonical_disease_imc) %>%
        column_to_rownames("category_canonical_disease_imc") %>%
        as.matrix()
    },
    "temporal" = {
      if (is.null(date_col)) {
        return(matrix(numeric(0), nrow = 0, ncol = 0))
      }
      data %>%
        mutate(
          month = lubridate::month(.data[[date_col]]),
          day_of_week = lubridate::wday(.data[[date_col]]),
          week_of_year = lubridate::week(.data[[date_col]])
        ) %>%
        group_by(category_canonical_disease_imc) %>%
        summarise(
          seasonal_peak = {
            tbl <- table(month)
            if (length(tbl)) as.integer(names(tbl)[which.max(tbl)]) else NA_integer_
          },
          weekday_pattern = mean(day_of_week %in% 2:6, na.rm = TRUE),
          temporal_variance = var(as.numeric(.data[[date_col]]), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        column_to_rownames("category_canonical_disease_imc") %>%
        scale(center = TRUE, scale = TRUE) %>%
        as.matrix()
    },
    "geographic" = {
      geo_cols <- intersect(c("admin1", "admin2"), names(data))
      if (length(geo_cols) == 0) {
        return(matrix(numeric(0), nrow = 0, ncol = 0))
      }
      data %>%
        group_by(category_canonical_disease_imc) %>%
        summarise(
          urban_rural_ratio = if ("facility_type" %in% names(data)) {
            ft <- ifelse(is.na(facility_type), "", tolower(facility_type))
            sum(ft == "urban", na.rm = TRUE) / n()
          } else NA_real_,
          geographic_spread = n_distinct(do.call(paste, c(across(all_of(geo_cols)), sep = "::")), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        column_to_rownames("category_canonical_disease_imc") %>%
        scale(center = TRUE, scale = TRUE) %>%
        as.matrix()
    },
    "combined" = {
      freq_features <- data %>%
        count(category_canonical_disease_imc) %>%
        rename(frequency = n)

      temporal_features <- if (!is.null(date_col)) {
        data %>%
          mutate(month = lubridate::month(.data[[date_col]])) %>%
          group_by(category_canonical_disease_imc) %>%
          summarise(
            seasonal_peak = {
              tbl <- table(month)
              if (length(tbl)) as.integer(names(tbl)[which.max(tbl)]) else NA_integer_
            },
            temporal_variance = var(as.numeric(.data[[date_col]]), na.rm = TRUE),
            .groups = "drop"
          )
      } else NULL

      geographic_features <- if ("admin1" %in% names(data)) {
        data %>%
          group_by(category_canonical_disease_imc) %>%
          summarise(geographic_spread = n_distinct(admin1), .groups = "drop")
      } else NULL

      combined <- freq_features
      if (!is.null(temporal_features)) {
        combined <- combined %>% left_join(temporal_features, by = "category_canonical_disease_imc")
      }
      if (!is.null(geographic_features)) {
        combined <- combined %>% left_join(geographic_features, by = "category_canonical_disease_imc")
      }
      combined %>%
        column_to_rownames("category_canonical_disease_imc") %>%
        scale(center = TRUE, scale = TRUE) %>%
        as.matrix()
    },
    {
      data %>%
        count(category_canonical_disease_imc) %>%
        column_to_rownames("category_canonical_disease_imc") %>%
        as.matrix()
    }
  )
  
  features[is.na(features)] <- 0
  features
}



#' Multi-Disease Correlation Module Server
multi_disease_correlation_Server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for correlation analysis
    correlation_values <- reactiveValues(
      correlation_results = NULL,
      cooccurrence_results = NULL,
      clustering_results = NULL,
      disease_choices = NULL
    )
    
    # Update disease choices when data changes
    observe({
      req(filtered_data())
      df <- filtered_data()
      
      disease_col <- resolve_disease_column(df)
      if (!is.null(disease_col)) {
        diseases <- sort(unique(df[[disease_col]][!is.na(df[[disease_col]])]))
        updatePickerInput(session, "selected_diseases", choices = diseases, selected = head(diseases, 8))
        correlation_values$disease_choices <- diseases
      }
    })
    
    # Calculate disease correlations
    observeEvent(input$calculate_correlation, {
      req(filtered_data(), input$selected_diseases)
      
      if (length(input$selected_diseases) < 2) {
        showNotification("Please select at least 2 diseases for correlation analysis.", 
                        type = "warning", duration = 3000)
        return()
      }
      
      showNotification("Calculating disease correlations...", type = "message", duration = NULL, id = "correlation_calc")
      
      tryCatch({
        disease_col <- resolve_disease_column(filtered_data())
        if (is.null(disease_col)) {
          stop("Disease classifications not available in dataset")
        }
        correlation_result <- calculate_disease_correlations(
          data = filtered_data(),
          correlation_type = input$correlation_type,
          time_window = input$time_window,
          selected_diseases = input$selected_diseases
        )
        
        correlation_values$correlation_results <- correlation_result
        
        removeNotification("correlation_calc")
        showNotification("Correlation analysis complete!", type = "success", duration = 3000)
        
      }, error = function(e) {
        removeNotification("correlation_calc")
        showNotification(paste("Error in correlation analysis:", e$message), type = "error", duration = 5000)
      })
    })
    
    # Correlation heatmap
    output$correlation_heatmap <- renderPlotly({
      req(correlation_values$correlation_results)
      
      result <- correlation_values$correlation_results
      correlation_data <- result$long_format
      
      if (is.null(correlation_data) || nrow(correlation_data) == 0) {
        msg <- if (!is.null(result$message)) result$message else "No correlation data available"
        return(plotly_empty(msg))
      }
      
      # Filter by minimum correlation if specified
      if (!is.null(input$min_correlation)) {
        correlation_data <- correlation_data %>%
          filter(abs(correlation) >= input$min_correlation)
      }
      
      p <- ggplot(correlation_data, aes(x = disease1, y = disease2, fill = correlation)) +
        geom_tile(color = "white", size = 0.5) +
        scale_fill_gradient2(
          low = "#3498db", 
          mid = "white", 
          high = "#e74c3c",
          midpoint = 0,
          name = "Correlation",
          limits = c(-1, 1)
        ) +
        labs(
          title = paste("Disease Correlation Heatmap:", str_to_title(input$correlation_type)),
          subtitle = paste("Time Window:", str_to_title(input$time_window)),
          x = "Disease 1",
          y = "Disease 2"
        ) +
        theme_report() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 0, hjust = 1)
        )
      
      ggplotly(p, tooltip = c("x", "y", "fill"))
    })
    
    # Calculate co-occurrence patterns
    observe({
      req(filtered_data())
      
      if (!is.null(correlation_values$correlation_results) && !is.null(correlation_values$correlation_results$matrix)) {
        # Automatically calculate co-occurrence when correlation is calculated
        tryCatch({
          cooccurrence_result <- analyze_disease_cooccurrence(
            data = filtered_data(),
            time_window = 30  # 30-day window for co-occurrence
          )
          
          correlation_values$cooccurrence_results <- cooccurrence_result
          
        }, error = function(e) {
          log_warn(paste("Error calculating co-occurrence:", e$message))
        })
      }
    })
    
    # Co-occurrence network plot
    output$cooccurrence_network <- renderPlotly({
      req(correlation_values$cooccurrence_results)
      
      cooccurrence_data <- correlation_values$cooccurrence_results
      
      if (is.null(cooccurrence_data) || nrow(cooccurrence_data) == 0) {
        return(plotly_empty("No co-occurrence data available"))
      }
      
      # Filter for significant co-occurrences
      significant_cooccurrence <- cooccurrence_data %>%
        filter(significant == TRUE, !is.na(lift), lift > 1.2) %>%
        head(20)  # Top 20 co-occurrences
      
      if (nrow(significant_cooccurrence) == 0) {
        return(plotly_empty("No significant co-occurrences found"))
      }
      
      # Create network-like visualization using a scatter plot
      p <- ggplot(significant_cooccurrence, aes(x = support, y = lift)) +
        geom_point(aes(size = cooccurrence_count, color = confidence_1to2), alpha = 0.7) +
        geom_text(aes(label = paste(str_trunc(disease1, 15), "→", str_trunc(disease2, 15))), 
                 size = 3, vjust = -1) +
        scale_size_continuous(name = "Co-occurrence\nCount", range = c(3, 15)) +
        scale_color_viridis_c(name = "Confidence") +
        labs(
          title = "Disease Co-occurrence Network",
          subtitle = "Size = Count, Color = Confidence",
          x = "Support (Proportion of Total Cases)",
          y = "Lift (Observed/Expected Ratio)"
        ) +
        theme_report()
      
      ggplotly(p, tooltip = c("x", "y", "size", "colour", "text"))
    })
    
    # Temporal co-occurrence plot
    output$temporal_cooccurrence <- renderPlotly({
      req(correlation_values$cooccurrence_results, filtered_data())
      
      # Create temporal analysis of top co-occurring diseases
      cooccurrence_data <- correlation_values$cooccurrence_results
      top_pairs <- cooccurrence_data %>%
        filter(significant == TRUE) %>%
        arrange(desc(lift)) %>%
        head(5)
      
      if (nrow(top_pairs) == 0) {
        return(plotly_empty("No significant co-occurrences for temporal analysis"))
      }
      
      date_col <- attr(cooccurrence_data, "date_col")
      entity_col <- attr(cooccurrence_data, "entity_col")
      if (is.null(date_col)) {
        date_col <- resolve_date_column(filtered_data())
      }
      if (is.null(entity_col)) {
        entity_col <- resolve_entity_column(filtered_data())
      }
      if (is.null(date_col) || is.null(entity_col)) {
        return(plotly_empty("Date or grouping columns unavailable for temporal co-occurrence"))
      }

      # Calculate monthly trends for top pairs
      temporal_data <- purrr::map_dfr(1:nrow(top_pairs), function(i) {
        pair <- top_pairs[i, ]
        
        monthly_cooccurrence <- filtered_data() %>%
          filter(category_canonical_disease_imc %in% c(pair$disease1, pair$disease2)) %>%
          mutate(
            .corr_date = .data[[date_col]],
            .entity = .data[[entity_col]]
          ) %>%
          mutate(
            month = floor_date(.corr_date, "month"),
            disease_pair = paste(pair$disease1, "×", pair$disease2)
          ) %>%
          group_by(.entity, month) %>%
          summarise(
            diseases_present = list(unique(category_canonical_disease_imc)),
            .groups = "drop"
          ) %>%
          filter(lengths(diseases_present) >= 2) %>%
          count(month, name = "cooccurrence_count") %>%
          complete(month = seq(min(month), max(month), by = "month"), 
                  fill = list(cooccurrence_count = 0)) %>%
          mutate(disease_pair = paste(pair$disease1, "×", pair$disease2))
        
        monthly_cooccurrence
      })
      
      p <- ggplot(temporal_data, aes(x = month, y = cooccurrence_count, color = disease_pair)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(
          title = "Temporal Co-occurrence Patterns",
          subtitle = "Monthly trends for top disease pairs",
          x = "Month",
          y = "Co-occurrence Count",
          color = "Disease Pair"
        ) +
        theme_report() +
        scale_color_viridis_d()
      
      ggplotly(p, tooltip = c("x", "y", "colour"))
    })
    
    # Co-occurrence statistics table
    output$cooccurrence_table <- DT::renderDataTable({
      req(correlation_values$cooccurrence_results)
      
      if (is.null(correlation_values$cooccurrence_results) || nrow(correlation_values$cooccurrence_results) == 0) {
        return(DT::datatable(data.frame(Message = "No co-occurrence data available"), options = list(dom = 't')))
      }

      cooccurrence_data <- correlation_values$cooccurrence_results %>%
        select(disease1, disease2, cooccurrence_count, lift, support, 
               confidence_1to2, confidence_2to1, p_value, significant) %>%
        mutate(
          lift = round(lift, 2),
          support = round(support, 4),
          confidence_1to2 = round(confidence_1to2, 3),
          confidence_2to1 = round(confidence_2to1, 3),
          p_value = round(p_value, 4)
        ) %>%
        arrange(desc(lift))
      
      DT::datatable(
        cooccurrence_data,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          order = list(list(3, 'desc')),  # Order by lift
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        filter = 'top',
        colnames = c('Disease 1', 'Disease 2', 'Count', 'Lift', 'Support', 
                    'Confidence 1→2', 'Confidence 2→1', 'P-value', 'Significant')
      ) %>%
        DT::formatStyle(
          'significant',
          backgroundColor = DT::styleEqual(
            c(TRUE, FALSE),
            c('#e8f5e8', '#ffebee')
          )
        ) %>%
        DT::formatStyle(
          'lift',
          backgroundColor = DT::styleInterval(
            cuts = c(1, 1.5, 2),
            values = c('#f8f9fa', '#fff3cd', '#d4edda', '#c3e6cb')
          )
        )
    })
    
    # Perform disease clustering
    observeEvent(input$perform_clustering, {
      req(filtered_data(), input$selected_diseases)
      
      if (length(input$selected_diseases) < 3) {
        showNotification("Please select at least 3 diseases for clustering analysis.", 
                        type = "warning", duration = 3000)
        return()
      }
      
      showNotification("Performing disease clustering...", type = "message", duration = NULL, id = "clustering_calc")
      
      tryCatch({
        # Filter data to selected diseases
        disease_col <- resolve_disease_column(filtered_data())
        if (is.null(disease_col)) {
          stop("Disease classifications not available for clustering")
        }
        clustering_data <- filtered_data() %>%
          mutate(category_canonical_disease_imc = .data[[disease_col]]) %>%
          filter(category_canonical_disease_imc %in% input$selected_diseases)
        
        clustering_result <- perform_disease_clustering(
          data = clustering_data,
          method = input$clustering_method,
          num_clusters = input$num_clusters,
          feature_set = input$clustering_features
        )
        
        correlation_values$clustering_results <- clustering_result
        
        removeNotification("clustering_calc")
        showNotification("Disease clustering complete!", type = "success", duration = 3000)
        
      }, error = function(e) {
        removeNotification("clustering_calc")
        showNotification(paste("Error in clustering analysis:", e$message), type = "error", duration = 5000)
      })
    })
    
    # Clustering plot
    output$clustering_plot <- renderPlotly({
      req(correlation_values$clustering_results)
      
      clustering_result <- correlation_values$clustering_results
      
      if (is.null(clustering_result$features) || nrow(clustering_result$features) == 0) {
        return(plotly_empty("No clustering data available"))
      }
      
      # Perform PCA for visualization
      if (ncol(clustering_result$features) >= 2) {
        pca_result <- prcomp(clustering_result$features, scale. = TRUE)
        
        # Create plot data
        plot_data <- data.frame(
          Disease = rownames(clustering_result$features),
          PC1 = pca_result$x[, 1],
          PC2 = pca_result$x[, 2],
          Cluster = factor(clustering_result$assignments)
        )
        
        # Calculate variance explained
        var_explained <- round(summary(pca_result)$importance[2, 1:2] * 100, 1)
        
        p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
          geom_point(size = 4, alpha = 0.8) +
          geom_text(aes(label = str_trunc(Disease, 20)), size = 3, vjust = -1) +
          labs(
            title = paste("Disease Clustering:", str_to_title(input$clustering_method)),
            subtitle = paste("Features:", str_to_title(input$clustering_features)),
            x = paste0("PC1 (", var_explained[1], "% variance)"),
            y = paste0("PC2 (", var_explained[2], "% variance)"),
            color = "Cluster"
          ) +
          theme_report() +
          scale_color_viridis_d()
        
        ggplotly(p, tooltip = c("x", "y", "colour", "text"))
        
      } else {
        # Fallback: simple bar plot of cluster assignments
        cluster_summary <- correlation_values$clustering_results$summary
        
        plot_data <- cluster_summary %>%
          unnest(diseases) %>%
          mutate(cluster = factor(cluster))
        
        p <- ggplot(plot_data, aes(x = reorder(diseases, cluster), y = cluster, fill = cluster)) +
          geom_col() +
          coord_flip() +
          labs(
            title = "Disease Cluster Assignments",
            x = "Disease",
            y = "Cluster",
            fill = "Cluster"
          ) +
          theme_report() +
          scale_fill_viridis_d()
        
        ggplotly(p, tooltip = c("x", "y", "fill"))
      }
    })
    
  })
}
