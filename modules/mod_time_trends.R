# modules/mod_time_trends.R
# Time Trends Analysis Module - Enhanced with disease drill-down functionality

# Source the disease categories functions with robust path handling
tryCatch({
  # Try different possible paths for taxonomy-aware system
  possible_paths <- c(
    "R/disease_categories_taxaware.R",                    # Standard R package structure
    "../R/disease_categories_taxaware.R",                # If modules is a subfolder
    "disease_categories_taxaware.R",                     # Same directory
    file.path("R", "disease_categories_taxaware.R"),     # Explicit file.path
    here::here("R", "disease_categories_taxaware.R")     # Using here package (if available)
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
    stop("Could not find disease_categories_taxaware.R in any of the expected locations: ", 
         paste(possible_paths, collapse = ", "))
  }
}, error = function(e) {
  warning("Failed to source disease_categories_taxaware.R: ", e$message)
  
  # Fallback: Define essential functions locally
  cat("Loading fallback functions for time trends...\n")
  
  get_epidemic_groups <- function() {
    list(
      "Vaccine_Preventable" = c("Measles", "Suspected Measles", "Pneumonia", "Influenza Like Illness"),
      "Water_Food_Borne" = c("Acute Watery Diarrhea (Suspected Cholera)", "Typhoid"),
      "Vector_Borne" = c("Malaria"),
      "Respiratory_Epidemic" = c("Influenza Like Illness", "COVID-19 suspected case", "SARI", "Tuberculosis")
    )
  }
  
  standardize_disease_labels <- function(disease_names) {
    disease_names[is.na(disease_names)] <- "Unknown"
    return(disease_names)
  }
  
  # Use the main comprehensive morbidity categorization function
  apply_epidemic_categorization <- function(df) {
    if (exists("apply_morbidity_categorization")) {
      return(apply_morbidity_categorization(df))
    } else {
      # Fallback for cases where main function is not loaded
      if (!"morbidity" %in% names(df)) return(df)
      epidemic_list <- unlist(get_epidemic_groups())
      df$canonical_disease_imc <- standardize_disease_labels(df$morbidity)
      df$epidemic_prone <- df$canonical_disease_imc %in% epidemic_list
      return(df)
    }
  }
  
  cat("Fallback functions loaded successfully.\n")
})

# Define null coalescer operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

#' Time Trends Module UI
#'
#' @param id Character string. The module's ID
#' @param height Character string. Plot height (default: "500px")
#'
#' @return Shiny UI elements
mod_time_trends_ui <- function(id, height = "500px") {
  ns <- NS(id)
  
  tagList(
    # Header with key metrics
    fluidRow(
      column(3, valueBoxOutput(ns("trends_total_diseases"), width = NULL)),
      column(3, valueBoxOutput(ns("trends_epidemic_diseases"), width = NULL)), 
      column(3, valueBoxOutput(ns("trends_peak_month"), width = NULL)),
      column(3, valueBoxOutput(ns("trends_seasonal_change"), width = NULL))
    ),
    
    # Main trends analysis with improved controls
    fluidRow(
      column(12,
             box(
               title = "📈 Disease Trends Over Time",
               status = "primary",
               solidHeader = TRUE,
               width = NULL,
               height = "700px",
               
               fluidRow(
                 column(3,
                        wellPanel(
                          style = "background-color: #f8f9fa; border: 1px solid #dee2e6;",
                          h5("📊 Analysis Settings", style = "color: #495057; margin-bottom: 15px;"),
                          
                          selectInput(
                            ns("trend_type"),
                            "What to analyze:",
                            choices = list(
                              "🦠 Disease Categories" = "disease_category",
                              "🔬 Specific Diseases" = "specific_disease", 
                              "⚠️ Epidemic Diseases" = "epidemic_disease",
                              "🌍 Geographic Regions" = "geographic",
                              "👥 Age Groups" = "age",
                              "🏥 Facility Types" = "facility"
                            ),
                            selected = "disease_category"
                          ),
                          
                          selectInput(
                            ns("trends_time_period"),
                            "Time grouping:",
                            choices = list(
                              "📅 Daily" = "day", 
                              "📊 Weekly" = "week", 
                              "📈 Monthly" = "month"
                            ),
                            selected = "week"
                          ),
                 
                          
                          conditionalPanel(
                            condition = "input.trend_type == 'disease_category'",
                            ns = ns,
                            pickerInput(
                              ns("selected_disease_categories"),
                              "Disease categories to show:",
                              choices = NULL,
                              multiple = TRUE,
                              options = pickerOptions(
                                maxOptions = 6,
                                actionsBox = TRUE,
                                selectedTextFormat = "count > 2",
                                title = "Choose categories..."
                              )
                            )
                          ),
                          
                          conditionalPanel(
                            condition = "input.trend_type == 'specific_disease'",
                            ns = ns,
                            pickerInput(
                              ns("selected_specific_diseases"),
                              "Specific diseases to show:",
                              choices = NULL,
                              multiple = TRUE,
                              options = pickerOptions(
                                maxOptions = 8,
                                actionsBox = TRUE,
                                selectedTextFormat = "count > 3",
                                title = "Choose diseases..."
                              )
                            )
                          ),
                          
                          conditionalPanel(
                            condition = "input.trend_type == 'epidemic_disease'",
                            ns = ns,
                            pickerInput(
                              ns("selected_epidemic_categories"),
                              "Epidemic categories:",
                              choices = NULL,
                              multiple = TRUE,
                              options = pickerOptions(
                                actionsBox = TRUE,
                                maxOptions = 4,
                                title = "Choose epidemic types..."
                              )
                            )
                          ),
                          
                          conditionalPanel(
                            condition = "input.trend_type == 'geographic'",
                            ns = ns,
                            pickerInput(
                              ns("selected_regions"),
                              "Governorates to show:",
                              choices = NULL,
                              multiple = TRUE,
                              options = pickerOptions(
                                maxOptions = 6,
                                actionsBox = TRUE,
                                title = "Choose governorates..."
                              )
                            )
                          ),
                          
                          hr(style = "margin: 15px 0;"),
                          
                          h5("📊 Chart Options", style = "color: #495057; margin-bottom: 15px;"),
                          
                          radioButtons(
                            ns("trends_chart_type"),
                            "Chart style:",
                            choices = list(
                              "📈 Line chart" = "line",
                              "📊 Area chart" = "area", 
                              "🔲 Separate panels" = "facet"
                            ),
                            selected = "line"
                          ),
                          
                          checkboxInput(
                            ns("trends_show_trend"),
                            "📈 Show trend lines",
                            value = FALSE
                          ),
                          
                          checkboxInput(
                            ns("highlight_epidemic"),
                            "⚠️ Highlight epidemic diseases",
                            value = FALSE
                          )
                        )
                 ),
                 
                 column(9,
                        plotlyOutput(ns("detailed_trends_plot"), height = "580px")
                 )
               )
             )
      )
    ),
    
    # Disease-specific analysis panels
    fluidRow(
      column(6,
             box(
               title = "Disease Distribution Analysis",
               status = "info",
               solidHeader = TRUE,
               width = NULL,
               height = "600px",
               
               tabsetPanel(
                 id = ns("disease_distribution_tabs"),
                 
                 tabPanel("Overall Distribution",
                          br(),
                          plotlyOutput(ns("disease_distribution_plot"), height = "450px")
                 ),
                 
                 tabPanel("By Category",
                          br(),
                          plotlyOutput(ns("disease_by_category_plot"), height = "450px")
                 ),
                 
                 tabPanel("Epidemic Focus",
                          br(),
                          plotlyOutput(ns("epidemic_disease_plot"), height = "450px")
                 ),
                 
                 tabPanel("Treemap View", 
                          br(),
                          plotOutput(ns("disease_treemap_plot"), height = "450px")
                 )
               )
             )
      ),
      
      column(6,
             box(
               title = "Disease Burden & Seasonality",
               status = "warning",
               solidHeader = TRUE,
               width = NULL,
               height = "600px",
               
               tabsetPanel(
                 id = ns("burden_analysis_tabs"),
                 
                 tabPanel("Burden Matrix",
                          br(),
                          plotlyOutput(ns("disease_burden_matrix"), height = "450px")
                 ),
                 
                 tabPanel("Seasonal Patterns",
                          br(),
                          selectInput(ns("seasonal_analysis_type"), "Analysis Type:",
                                      choices = list("Monthly Patterns" = "monthly", 
                                                     "Quarterly Trends" = "quarterly",
                                                     "Weekly Cycles" = "weekly"),
                                      selected = "monthly"),
                          plotlyOutput(ns("seasonal_patterns_plot"), height = "350px")
                 ),
                 
                 tabPanel("Disease Velocity",
                          br(),
                          plotlyOutput(ns("disease_velocity_plot"), height = "450px")
                 )
               )
             )
      )
    ),
    
    # Statistics and detailed analysis
    fluidRow(
      column(6,
             box(
               title = "Trend Statistics",
               status = "primary",
               solidHeader = TRUE,
               width = NULL,
               height = "500px",
               
               DT::dataTableOutput(ns("trend_stats"))
             )
      ),
      
      column(6,
             box(
               title = "Disease-Specific Insights",
               status = "success",
               solidHeader = TRUE,
               width = NULL,
               height = "500px",
               
               tabsetPanel(
                 id = ns("insights_tabs"),
                 
                 tabPanel("Top Movers",
                          br(),
                          DT::dataTableOutput(ns("disease_movers_table"))
                 ),
                 
                 tabPanel("Epidemic Alerts",
                          br(),
                          DT::dataTableOutput(ns("epidemic_trends_table"))
                 )
               )
             )
      )
    ),
    
    # Detailed analysis tables
    fluidRow(
      column(12,
             box(
               title = "Detailed Disease Analysis",
               status = "primary",
               solidHeader = TRUE,
               width = NULL,
               collapsible = TRUE,
               collapsed = TRUE,
               
               tabsetPanel(
                 id = ns("detailed_analysis_tabs"),
                 
                 tabPanel("Geographic Summary",
                          br(),
                          DT::dataTableOutput(ns("disease_geographic_summary"))
                 ),
                 
                 tabPanel("Seasonal Analysis",
                          br(),
                          DT::dataTableOutput(ns("seasonal_disease_analysis"))
                 ),
                 
                 tabPanel("Priority Assessment",
                          br(),
                          DT::dataTableOutput(ns("disease_priority_matrix"))
                 ),
                 
                 tabPanel("Disease Correlation",
                          br(),
                          DT::dataTableOutput(ns("disease_correlation_matrix"))
                 )
               )
             )
      )
    )
  )
}

#' Time Trends Module Server
#'
#' @param id Character string. The module's ID
#' @param data Reactive. Filtered dataset
#' @param config List. Configuration parameters
#'
#' @return Trend data reactive
mod_time_trends_server <- function(id, data, config = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Helper function to get date column - prioritize datevisit per preprocessing
    get_date_col <- function(df) {
      if ("datevisit" %in% colnames(df)) "datevisit"
      else if ("datevisitnew" %in% colnames(df)) "datevisitnew"
      else NULL
    }
    
    # Process data with epidemic categorization
    processed_data <- reactive({
      req(data())
      
      df <- data()
      
      # Apply taxonomy categorization - try multiple approaches
      if (exists("apply_taxonomy_categorization")) {
        df <- apply_taxonomy_categorization(df)
      } else if (exists("apply_epidemic_categorization")) {
        df <- apply_epidemic_categorization(df)
      } else {
        # Fallback: manual epidemic detection and categorization
        if ("morbidity" %in% colnames(df)) {
          # Create standardized disease column
          df$canonical_disease_imc <- df$morbidity
          
          # Create category_canonical_disease_imc if missing
          if (!"category_canonical_disease_imc" %in% colnames(df)) {
            df$category_canonical_disease_imc <- case_when(
              grepl("pneumonia|respiratory|lung|breathing", df$morbidity, ignore.case = TRUE) ~ "Respiratory",
              grepl("diarrhea|diarrhoea|gastroenteritis|stomach|intestinal", df$morbidity, ignore.case = TRUE) ~ "GI",
              grepl("fever|malaria|dengue|typhoid", df$morbidity, ignore.case = TRUE) ~ "Infectious",
              grepl("measles|vaccination|immuniz", df$morbidity, ignore.case = TRUE) ~ "Vaccine Preventable",
              grepl("trauma|injury|accident|wound", df$morbidity, ignore.case = TRUE) ~ "Trauma",
              grepl("diabetes|hypertension|heart|cardiovascular", df$morbidity, ignore.case = TRUE) ~ "Non-communicable",
              grepl("skin|dermatitis|rash", df$morbidity, ignore.case = TRUE) ~ "Dermatological",
              grepl("pregnancy|maternal|delivery|birth", df$morbidity, ignore.case = TRUE) ~ "Maternal/Child Health",
              TRUE ~ "Other"
            )
          }
          
          # Add epidemic categorization
          if (exists("get_epidemic_groups")) {
            epidemic_list <- get_epidemic_groups()
            all_epidemic_diseases <- unlist(epidemic_list, use.names = FALSE)
            df$epidemic_prone <- df$morbidity %in% all_epidemic_diseases
          } else {
            df$epidemic_prone <- FALSE
          }
        }
      }
      
      return(df)
    })
    
    # Get available epidemic data
    available_epidemic_data <- reactive({
      req(processed_data())
      
      df <- processed_data()
      
      if (exists("get_epidemic_groups") && "canonical_disease_imc" %in% colnames(df)) {
        epidemic_list <- get_epidemic_groups()
        
        # Check which categories have cases
        available_categories <- names(epidemic_list)[
          sapply(epidemic_list, function(diseases) {
            any(diseases %in% unique(df$canonical_disease_imc))
          })
        ]
        
        # Get all available epidemic diseases in data
        all_epidemic_diseases <- unlist(epidemic_list, use.names = FALSE)
        available_diseases <- intersect(all_epidemic_diseases, unique(df$canonical_disease_imc))
        
        return(list(
          categories = available_categories,
          diseases = available_diseases,
          category_disease_map = epidemic_list
        ))
      } else {
        return(list(categories = NULL, diseases = NULL, category_disease_map = NULL))
      }
    })
    
    # Update choices based on data
    observe({
      req(processed_data())
      
      df <- processed_data()
      
      # Update disease category choices
      if ("category_canonical_disease_imc" %in% colnames(df)) {
        categories <- df %>%
          filter(!is.na(category_canonical_disease_imc), category_canonical_disease_imc != "Unknown") %>%
          count(category_canonical_disease_imc, sort = TRUE) %>%
          slice_head(n = 20) %>%
          pull(category_canonical_disease_imc)
        
        updatePickerInput(
          session, "selected_disease_categories",
          choices = categories,
          selected = head(categories, 6)
        )
      }
      
      # Update specific disease choices (from morbidity)
      if ("morbidity" %in% colnames(df)) {
        specific_diseases <- df %>%
          filter(!is.na(morbidity), morbidity != "Unknown") %>%
          count(morbidity, sort = TRUE) %>%
          slice_head(n = 50) %>%
          pull(morbidity)
        
        updatePickerInput(
          session, "selected_specific_diseases",
          choices = specific_diseases,
          selected = head(specific_diseases, 8)
        )
      }
      
      # Update region choices
      if ("admin1" %in% colnames(df)) {
        regions <- sort(unique(df$admin1[!is.na(df$admin1)]))
        
        updatePickerInput(
          session, "selected_regions",
          choices = regions,
          selected = head(regions, 4)
        )
      }
    })
    
    # Update epidemic disease choices
    observe({
      req(available_epidemic_data())
      
      available_data <- available_epidemic_data()
      
      if (!is.null(available_data$categories)) {
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
      }
    })
    
    # Update epidemic disease choices based on selected categories
    observe({
      req(available_epidemic_data(), input$selected_epidemic_categories)
      
      available_data <- available_epidemic_data()
      
      if (!is.null(available_data$category_disease_map) && length(input$selected_epidemic_categories) > 0) {
        # Get diseases from selected categories
        category_diseases <- unlist(
          available_data$category_disease_map[input$selected_epidemic_categories],
          use.names = FALSE
        )
        
        # Filter to diseases actually in the data
        available_diseases <- intersect(category_diseases, available_data$diseases)
        
        updatePickerInput(
          session, "selected_epidemic_diseases",
          choices = available_diseases,
          selected = head(available_diseases, 6)
        )
      }
    })
    
    # Prepare trend data based on trend type
    trend_data <- reactive({
      req(processed_data(), input$trend_type, input$trends_time_period)
      
      df <- processed_data()
      date_col <- get_date_col(df)
      
      if (is.null(date_col)) {
        return(NULL)
      }
      
      # Create time grouping variable
      df <- df %>%
        mutate(
          time_period = case_when(
            input$trends_time_period == "day" ~ as.Date(.data[[date_col]]),
            input$trends_time_period == "week" ~ floor_date(.data[[date_col]], "week"),
            input$trends_time_period == "month" ~ floor_date(.data[[date_col]], "month")
          )
        ) %>%
        filter(!is.na(time_period))
      
      # Group by trend type
      if (input$trend_type == "disease_category") {
        if (!"category_canonical_disease_imc" %in% colnames(df)) return(NULL)
        
        # If no categories selected, use top categories
        selected_cats <- input$selected_disease_categories
        if (is.null(selected_cats) || length(selected_cats) == 0) {
          selected_cats <- df %>%
            filter(!is.na(category_canonical_disease_imc), category_canonical_disease_imc != "Unknown") %>%
            count(category_canonical_disease_imc, sort = TRUE) %>%
            slice_head(n = 6) %>%
            pull(category_canonical_disease_imc)
        }
        
        if (length(selected_cats) == 0) return(NULL)
        
        trend_data <- df %>%
          filter(category_canonical_disease_imc %in% selected_cats) %>%
          count(time_period, category_canonical_disease_imc) %>%
          rename(category = category_canonical_disease_imc, cases = n)
        
      } else if (input$trend_type == "specific_disease") {
        if (!"morbidity" %in% colnames(df)) return(NULL)
        
        # If no diseases selected, use top diseases
        selected_diseases <- input$selected_specific_diseases
        if (is.null(selected_diseases) || length(selected_diseases) == 0) {
          selected_diseases <- df %>%
            filter(!is.na(morbidity), morbidity != "Unknown") %>%
            count(morbidity, sort = TRUE) %>%
            slice_head(n = 8) %>%
            pull(morbidity)
        }
        
        if (length(selected_diseases) == 0) return(NULL)
        
        trend_data <- df %>%
          filter(morbidity %in% selected_diseases) %>%
          count(time_period, morbidity) %>%
          rename(category = morbidity, cases = n)
        
      } else if (input$trend_type == "epidemic_disease") {
        if (!is.null(input$selected_epidemic_diseases) && length(input$selected_epidemic_diseases) > 0) {
          # Show specific epidemic diseases
          if (!"canonical_disease_imc" %in% colnames(df)) return(NULL)
          
          trend_data <- df %>%
            filter(canonical_disease_imc %in% input$selected_epidemic_diseases) %>%
            count(time_period, canonical_disease_imc) %>%
            rename(category = canonical_disease_imc, cases = n)
          
        } else if (!is.null(input$selected_epidemic_categories) && length(input$selected_epidemic_categories) > 0) {
          # Show epidemic categories
          if (!"canonical_disease_imc" %in% colnames(df)) return(NULL)
          
          epidemic_list <- available_epidemic_data()$category_disease_map
          category_disease_map <- purrr::map_dfr(names(epidemic_list), function(cat_name) {
            dplyr::tibble(
              canonical_disease_imc = epidemic_list[[cat_name]],
              epidemic_category = cat_name
            )
          })
          
          trend_data <- df %>%
            left_join(category_disease_map, by = "canonical_disease_imc") %>%
            filter(epidemic_category %in% input$selected_epidemic_categories) %>%
            count(time_period, epidemic_category) %>%
            rename(category = epidemic_category, cases = n)
        } else {
          return(NULL)
        }
        
      } else if (input$trend_type == "geographic") {
        if (!"admin1" %in% colnames(df)) return(NULL)
        
        # If no regions selected, use top regions
        selected_regions <- input$selected_regions
        if (is.null(selected_regions) || length(selected_regions) == 0) {
          selected_regions <- df %>%
            filter(!is.na(admin1)) %>%
            count(admin1, sort = TRUE) %>%
            slice_head(n = 4) %>%
            pull(admin1)
        }
        
        if (length(selected_regions) == 0) return(NULL)
        
        trend_data <- df %>%
          filter(admin1 %in% selected_regions) %>%
          count(time_period, admin1) %>%
          rename(category = admin1, cases = n)
        
      } else if (input$trend_type == "age") {
        if (!"age_group_new" %in% colnames(df)) return(NULL)
        
        trend_data <- df %>%
          filter(!is.na(age_group_new)) %>%
          count(time_period, age_group_new) %>%
          rename(category = age_group_new, cases = n)
        
      } else if (input$trend_type == "facility") {
        if (!"facilitytype" %in% colnames(df)) return(NULL)
        
        trend_data <- df %>%
          filter(!is.na(facilitytype)) %>%
          count(time_period, facilitytype) %>%
          rename(category = facilitytype, cases = n)
      }
      
      # Add epidemic highlighting if requested
      if (input$highlight_epidemic && exists("get_epidemic_groups")) {
        epidemic_diseases_list <- unlist(get_epidemic_groups(), use.names = FALSE)
        
        trend_data <- trend_data %>%
          mutate(
            is_epidemic = case_when(
              input$trend_type == "specific_disease" ~ category %in% epidemic_diseases_list,
              input$trend_type == "disease_category" ~ {
                # Check if any diseases in this category are epidemic
                if ("morbidity" %in% colnames(df)) {
                  category_diseases <- df %>%
                    filter(category_canonical_disease_imc == category) %>%
                    pull(morbidity) %>%
                    unique()
                  any(category_diseases %in% epidemic_diseases_list)
                } else {
                  FALSE
                }
              },
              TRUE ~ FALSE
            )
          )
      }
      
      trend_data %>%
        arrange(time_period)
    })
    
    # Value boxes for key metrics (enhanced)
    output$trends_total_diseases <- renderValueBox({
      req(processed_data())
      
      df <- processed_data()
      total_diseases <- if ("morbidity" %in% colnames(df)) {
        n_distinct(df$morbidity, na.rm = TRUE)
      } else if ("category_canonical_disease_imc" %in% colnames(df)) {
        n_distinct(df$category_canonical_disease_imc, na.rm = TRUE)
      } else { 0 }
      
      valueBox(
        value = total_diseases,
        subtitle = "Unique Diseases/Categories",
        icon = icon("virus"),
        color = "blue"
      )
    })
    
    output$trends_epidemic_diseases <- renderValueBox({
      req(processed_data())
      
      df <- processed_data()
      epidemic_diseases <- if ("epidemic_prone" %in% colnames(df)) {
        sum(df$epidemic_prone == TRUE, na.rm = TRUE)
      } else if ("canonical_disease_imc" %in% colnames(df) && exists("get_epidemic_groups")) {
        epidemic_conditions <- unlist(get_epidemic_groups(), use.names = FALSE)
        sum(df$canonical_disease_imc %in% epidemic_conditions, na.rm = TRUE)
      } else if ("morbidity" %in% colnames(df) && exists("get_epidemic_groups")) {
        epidemic_conditions <- unlist(get_epidemic_groups(), use.names = FALSE)
        sum(df$morbidity %in% epidemic_conditions, na.rm = TRUE)
      } else { 0 }
      
      valueBox(
        value = epidemic_diseases,
        subtitle = "Epidemic Disease Cases",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    })
    
    output$trends_peak_month <- renderValueBox({
      req(processed_data())
      
      df <- processed_data()
      date_col <- get_date_col(df)
      
      peak_month <- if (!is.null(date_col)) {
        month_counts <- df %>%
          mutate(month = lubridate::month(.data[[date_col]], label = TRUE)) %>%
          count(month, sort = TRUE)
        
        if (nrow(month_counts) > 0) as.character(month_counts$month[1]) else "Unknown"
      } else { "Unknown" }
      
      valueBox(
        value = peak_month,
        subtitle = "Peak Activity Month",
        icon = icon("calendar"),
        color = "yellow"
      )
    })
    
    output$trends_seasonal_change <- renderValueBox({
      req(processed_data())
      
      df <- processed_data()
      date_col <- get_date_col(df)
      
      seasonal_change <- if (!is.null(date_col)) {
        current_date <- max(df[[date_col]], na.rm = TRUE)
        current_month <- sum(df[[date_col]] >= floor_date(current_date, "month"), na.rm = TRUE)
        previous_month <- sum(df[[date_col]] >= floor_date(current_date - months(1), "month") & 
                                df[[date_col]] < floor_date(current_date, "month"), na.rm = TRUE)
        
        if (previous_month > 0) {
          change_pct <- round(((current_month - previous_month) / previous_month) * 100, 1)
          paste0(ifelse(change_pct > 0, "+", ""), change_pct, "%")
        } else { "N/A" }
      } else { "N/A" }
      
      color <- if (grepl("\\+", seasonal_change)) "green" else if (grepl("\\-", seasonal_change)) "red" else "blue"
      
      valueBox(
        value = seasonal_change,
        subtitle = "Monthly Change",
        icon = icon("chart-line"),
        color = color
      )
    })
    
    # Main trends plot (enhanced)
    output$detailed_trends_plot <- renderPlotly({
      req(trend_data())
      
      trend_df <- trend_data()
      
      if (nrow(trend_df) == 0) {
        return(plot_ly() %>%
                 add_annotations(
                   text = "No data available for selected parameters",
                   x = 0.5, y = 0.5,
                   xref = "paper", yref = "paper",
                   showarrow = FALSE
                 ))
      }
      
      # Create base plot
      if (input$trends_chart_type == "facet") {
        # Faceted plot
        p <- ggplot(trend_df, aes(x = time_period, y = cases))
        
        if (input$highlight_epidemic && "is_epidemic" %in% colnames(trend_df)) {
          p <- p + geom_line(aes(color = is_epidemic), linewidth = 1.2) +
            geom_point(aes(color = is_epidemic), size = 2) +
            scale_color_manual(values = c("TRUE" = "#D32F2F", "FALSE" = "#667eea"), 
                               labels = c("TRUE" = "Epidemic", "FALSE" = "Non-epidemic"))
        } else {
          p <- p + geom_line(color = "#667eea", linewidth = 1.2) +
            geom_point(color = "#667eea", size = 2)
        }
        
        p <- p + facet_wrap(~category, scales = "free_y") +
          labs(
            title = paste("Trends by", str_to_title(gsub("_", " ", input$trend_type))),
            x = str_to_title(input$trends_time_period),
            y = "Number of Cases"
          ) +
          theme_minimal() +
          theme(
            strip.text = element_text(size = 10, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )
      } else {
        # Line or area chart
        if (input$highlight_epidemic && "is_epidemic" %in% colnames(trend_df)) {
          p <- ggplot(trend_df, aes(x = time_period, y = cases, color = interaction(category, is_epidemic)))
        } else {
          p <- ggplot(trend_df, aes(x = time_period, y = cases, color = category))
        }
        
        if (input$trends_chart_type == "area") {
          p <- p + geom_area(aes(fill = category), alpha = 0.7, position = "stack")
        } else {
          p <- p + geom_line(linewidth = 1.2, alpha = 0.8) + geom_point(size = 2, alpha = 0.8)
        }
        
        # Add smoothing if requested
        if (input$trends_show_trend && input$trends_chart_type == "line") {
          p <- p + geom_smooth(method = "loess", se = TRUE, alpha = 0.3)
        }
        
        p <- p +
          scale_color_brewer(type = "qual", palette = "Set2") +
          scale_fill_brewer(type = "qual", palette = "Set2") +
          labs(
            title = paste("Trends by", str_to_title(gsub("_", " ", input$trend_type))),
            x = str_to_title(input$trends_time_period),
            y = "Number of Cases",
            color = str_to_title(gsub("_", " ", input$trend_type)),
            fill = str_to_title(gsub("_", " ", input$trend_type))
          ) +
          theme_minimal() +
          theme(legend.position = "bottom")
      }
      
      p <- p + scale_y_continuous(labels = scales::comma_format())
      
      # Convert to plotly
      ggplotly(p, tooltip = c("x", "y", "colour")) %>%
        layout(
          hovermode = "x unified",
          legend = list(orientation = "h", x = 0, y = -0.2)
        )
    })
    
    # Disease distribution by category (new)
    output$disease_by_category_plot <- renderPlotly({
      req(processed_data())
      
      df <- processed_data()
      
      if (!all(c("morbidity", "category_canonical_disease_imc") %in% colnames(df))) {
        return(plot_ly() %>% add_annotations(text = "Disease category data not available"))
      }
      
      category_dist <- df %>%
        filter(!is.na(category_canonical_disease_imc), !is.na(morbidity)) %>%
        count(category_canonical_disease_imc, morbidity, sort = TRUE) %>%
        group_by(category_canonical_disease_imc) %>%
        slice_head(n = 5) %>%  # Top 5 diseases per category
        ungroup()
      
      p <- ggplot(category_dist, aes(x = reorder(morbidity, n), y = n, fill = category_canonical_disease_imc)) +
        geom_col(alpha = 0.8) +
        coord_flip() +
        scale_fill_brewer(type = "qual", palette = "Set1") +
        labs(
          title = "Top Diseases by Category",
          x = "Disease",
          y = "Number of Cases",
          fill = "Category"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_y_continuous(labels = scales::comma_format())
      
      ggplotly(p, tooltip = c("fill", "y"))
    })
    
    # Epidemic disease focus plot (new)
    output$epidemic_disease_plot <- renderPlotly({
      req(processed_data())
      
      df <- processed_data()
      
      if (!"canonical_disease_imc" %in% colnames(df) || !exists("get_epidemic_groups")) {
        return(plot_ly() %>% add_annotations(text = "Epidemic disease data not available"))
      }
      
      epidemic_list <- get_epidemic_groups()
      
      epidemic_data <- map_dfr(names(epidemic_list), function(cat_name) {
        diseases <- epidemic_list[[cat_name]]
        df %>%
          filter(canonical_disease_imc %in% diseases) %>%
          count(canonical_disease_imc) %>%
          mutate(epidemic_category = cat_name)
      }) %>%
        arrange(desc(n)) %>%
        slice_head(n = 15)
      
      if (nrow(epidemic_data) == 0) {
        return(plot_ly() %>% add_annotations(text = "No epidemic diseases found"))
      }
      
      p <- ggplot(epidemic_data, aes(x = reorder(canonical_disease_imc, n), y = n, fill = epidemic_category)) +
        geom_col(alpha = 0.8) +
        coord_flip() +
        scale_fill_manual(values = c(
          "Vaccine_Preventable" = "#FF5722",
          "Water_Food_Borne" = "#2196F3", 
          "Vector_Borne" = "#4CAF50",
          "Respiratory_Epidemic" = "#FF9800"
        )) +
        labs(
          title = "Epidemic-Prone Disease Cases",
          x = "Disease",
          y = "Number of Cases",
          fill = "Epidemic Category"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_y_continuous(labels = scales::comma_format())
      
      ggplotly(p, tooltip = c("fill", "y"))
    })
    
    # Disease velocity plot (new - shows rate of change)
    output$disease_velocity_plot <- renderPlotly({
      req(trend_data())
      
      trend_df <- trend_data()
      
      if (nrow(trend_df) == 0) {
        return(plot_ly() %>% add_annotations(text = "No trend data available"))
      }
      
      # Calculate velocity (rate of change)
      velocity_data <- trend_df %>%
        group_by(category) %>%
        arrange(time_period) %>%
        mutate(
          velocity = (cases - lag(cases, default = first(cases))) / 
            as.numeric(time_period - lag(time_period, default = first(time_period))) * 7, # Weekly rate
          velocity = ifelse(is.infinite(velocity) | is.na(velocity), 0, velocity)
        ) %>%
        filter(!is.na(velocity)) %>%
        ungroup()
      
      p <- ggplot(velocity_data, aes(x = time_period, y = velocity, color = category)) +
        geom_line(linewidth = 1.2, alpha = 0.8) +
        geom_point(size = 2, alpha = 0.8) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        scale_color_brewer(type = "qual", palette = "Set2") +
        labs(
          title = "Disease Trend Velocity (Rate of Change)",
          x = "Time Period",
          y = "Weekly Change Rate",
          color = "Category"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      ggplotly(p, tooltip = c("x", "y", "colour"))
    })
    
    # Top movers table (new)
    output$disease_movers_table <- DT::renderDataTable({
      req(trend_data())
      
      trend_df <- trend_data()
      
      if (nrow(trend_df) == 0) return(NULL)
      
      # Calculate recent vs. previous period changes
      movers <- trend_df %>%
        group_by(category) %>%
        arrange(time_period) %>%
        summarise(
          recent_cases = last(cases),
          previous_cases = nth(cases, -2, default = last(cases)),
          total_cases = sum(cases),
          periods_tracked = n(),
          change_absolute = recent_cases - previous_cases,
          change_percent = ifelse(previous_cases > 0, 
                                  round((recent_cases - previous_cases) / previous_cases * 100, 1), 
                                  0),
          .groups = "drop"
        ) %>%
        mutate(
          trend_direction = case_when(
            change_percent > 25 ~ "🔺 Strong Increase",
            change_percent > 10 ~ "📈 Increase", 
            change_percent > -10 ~ "→ Stable",
            change_percent > -25 ~ "📉 Decrease",
            TRUE ~ "🔻 Strong Decrease"
          )
        ) %>%
        arrange(desc(abs(change_percent))) %>%
        select(
          Category = category,
          `Recent Cases` = recent_cases,
          `Previous Cases` = previous_cases,
          `Total Cases` = total_cases,
          `Change (%)` = change_percent,
          `Trend` = trend_direction
        )
      
      DT::datatable(
        movers,
        options = list(
          pageLength = 10,
          dom = 'tip',
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatCurrency(c("Recent Cases", "Previous Cases", "Total Cases"), 
                           currency = "", interval = 3, mark = ",", digits = 0) %>%
        DT::formatStyle(
          "Change (%)",
          color = DT::styleInterval(c(-10, 10), c("red", "black", "green"))
        )
    })
    
    # Epidemic trends table (new)
    output$epidemic_trends_table <- DT::renderDataTable({
      req(processed_data())
      
      df <- processed_data()
      
      if (!"epidemic_prone" %in% colnames(df) && !"canonical_disease_imc" %in% colnames(df)) {
        return(data.frame(Message = "Epidemic disease data not available"))
      }
      
      date_col <- get_date_col(df)
      if (is.null(date_col)) {
        return(data.frame(Message = "Date information required"))
      }
      
      current_date <- max(df[[date_col]], na.rm = TRUE)
      
      epidemic_trends <- df %>%
        filter(epidemic_prone == TRUE) %>%
        mutate(
          period = case_when(
            .data[[date_col]] >= current_date - 7 ~ "This Week",
            .data[[date_col]] >= current_date - 14 ~ "Last Week",
            .data[[date_col]] >= current_date - 30 ~ "This Month",
            TRUE ~ "Earlier"
          )
        ) %>%
        count(canonical_disease_imc, period) %>%
        pivot_wider(names_from = period, values_from = n, values_fill = 0) %>%
        mutate(
          weekly_change = ifelse(`Last Week` > 0, 
                                 round((`This Week` - `Last Week`) / `Last Week` * 100, 1), 
                                 0),
          alert_level = case_when(
            `This Week` >= 10 & weekly_change > 50 ~ "🔴 High Alert",
            `This Week` >= 5 & weekly_change > 25 ~ "🟡 Medium Alert", 
            `This Week` >= 3 ~ "🟢 Watch",
            TRUE ~ "Normal"
          )
        ) %>%
        arrange(desc(`This Week`)) %>%
        select(
          `Epidemic Disease` = canonical_disease_imc,
          `This Week`,
          `Last Week`, 
          `This Month`,
          `Weekly Change (%)` = weekly_change,
          `Alert Level` = alert_level
        )
      
      DT::datatable(
        epidemic_trends,
        options = list(
          pageLength = 15,
          dom = 'tip',
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Weekly Change (%)",
          color = DT::styleInterval(c(-10, 10), c("red", "black", "green"))
        )
    })
    
    # Enhanced trend statistics
    output$trend_stats <- DT::renderDataTable({
      req(trend_data())
      
      trend_df <- trend_data()
      
      if (nrow(trend_df) == 0) return(NULL)
      
      # Calculate enhanced statistics
      stats <- trend_df %>%
        group_by(category) %>%
        summarise(
          `Total Cases` = sum(cases, na.rm = TRUE),
          `Average per Period` = round(mean(cases, na.rm = TRUE), 1),
          `Peak Cases` = max(cases, na.rm = TRUE),
          `Peak Date` = time_period[which.max(cases)],
          `Min Cases` = min(cases, na.rm = TRUE),
          `Volatility` = round(sd(cases, na.rm = TRUE), 1),
          `Trend Direction` = {
            if (n() > 2) {
              trend_test <- cor.test(as.numeric(time_period), cases, method = "spearman")
              if (trend_test$p.value < 0.05) {
                if (trend_test$estimate > 0.3) "📈 Strong Increase"
                else if (trend_test$estimate > 0.1) "↗️ Increase" 
                else if (trend_test$estimate < -0.3) "📉 Strong Decrease"
                else if (trend_test$estimate < -0.1) "↘️ Decrease"
                else "→ Stable"
              } else {
                "→ Stable"
              }
            } else {
              "Insufficient data"
            }
          },
          .groups = "drop"
        ) %>%
        arrange(desc(`Total Cases`))
      
      DT::datatable(
        stats,
        options = list(
          pageLength = 10,
          dom = 'tip',
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatCurrency(c("Total Cases", "Peak Cases", "Min Cases"), 
                           currency = "", interval = 3, mark = ",", digits = 0) %>%
        DT::formatRound(c("Average per Period", "Volatility"), 1)
    })
    
    # Keep existing outputs with enhancements
    output$disease_distribution_plot <- renderPlotly({
      req(processed_data())
      
      df <- processed_data()
      
      if (!"category_canonical_disease_imc" %in% colnames(df)) {
        return(plot_ly() %>% add_annotations(text = "Disease data not available"))
      }
      
      disease_dist <- df %>%
        filter(!is.na(category_canonical_disease_imc)) %>%
        count(category_canonical_disease_imc, sort = TRUE) %>%
        slice_head(n = 15)
      
      # Add epidemic highlighting if available
      if (exists("get_epidemic_groups") && "morbidity" %in% colnames(df)) {
        epidemic_diseases_list <- unlist(get_epidemic_groups(), use.names = FALSE)
        
        disease_dist <- disease_dist %>%
          mutate(
            contains_epidemic = sapply(category_canonical_disease_imc, function(cat) {
              category_diseases <- df %>%
                filter(category_canonical_disease_imc == cat) %>%
                pull(morbidity) %>%
                unique()
              any(category_diseases %in% epidemic_diseases_list)
            })
          )
        
        p <- ggplot(disease_dist, aes(x = reorder(category_canonical_disease_imc, n), y = n, fill = contains_epidemic)) +
          geom_col(alpha = 0.8) +
          scale_fill_manual(values = c("TRUE" = "#D32F2F", "FALSE" = "#667eea"),
                            labels = c("TRUE" = "Contains Epidemic", "FALSE" = "Standard"))
      } else {
        p <- ggplot(disease_dist, aes(x = reorder(category_canonical_disease_imc, n), y = n)) +
          geom_col(fill = "#667eea", alpha = 0.8)
      }
      
      p <- p +
        coord_flip() +
        labs(
          title = "Top 15 Disease Categories",
          x = "Disease Category",
          y = "Number of Cases"
        ) +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma_format())
      
      ggplotly(p, tooltip = c("x", "y"))
    })
    
    # Enhanced disease treemap
    output$disease_treemap_plot <- renderPlot({
      req(processed_data())
      
      df <- processed_data()
      
      if (!"category_canonical_disease_imc" %in% colnames(df)) {
        return(NULL)
      }
      
      # Check if treemapify is available
      if (!requireNamespace("treemapify", quietly = TRUE)) {
        # Fallback to simple bar chart
        disease_dist <- df %>%
          filter(!is.na(category_canonical_disease_imc)) %>%
          count(category_canonical_disease_imc, sort = TRUE) %>%
          slice_head(n = 15)
        
        ggplot(disease_dist, aes(x = reorder(category_canonical_disease_imc, n), y = n)) +
          geom_col(fill = "#667eea", alpha = 0.8) +
          coord_flip() +
          labs(title = "Disease Distribution (Treemap not available)")
      } else {
        library(treemapify)
        
        treemap_data <- df %>%
          filter(!is.na(category_canonical_disease_imc)) %>%
          count(category_canonical_disease_imc, sort = TRUE) %>%
          slice_head(n = 20)
        
        # Add epidemic highlighting if available
        if (exists("get_epidemic_groups") && "morbidity" %in% colnames(df)) {
          epidemic_diseases_list <- unlist(get_epidemic_groups(), use.names = FALSE)
          
          treemap_data <- treemap_data %>%
            mutate(
              contains_epidemic = sapply(category_canonical_disease_imc, function(cat) {
                category_diseases <- df %>%
                  filter(category_canonical_disease_imc == cat) %>%
                  pull(morbidity) %>%
                  unique()
                any(category_diseases %in% epidemic_diseases_list)
              })
            )
          
          ggplot(treemap_data, aes(area = n, fill = contains_epidemic, label = category_canonical_disease_imc)) +
            geom_treemap() +
            geom_treemap_text(color = "white", place = "centre", grow = TRUE) +
            scale_fill_manual(values = c("TRUE" = "#D32F2F", "FALSE" = "#1976D2")) +
            labs(title = "Disease Category Distribution (Epidemic Highlighted)") +
            theme(legend.position = "bottom")
        } else {
          ggplot(treemap_data, aes(area = n, fill = n, label = category_canonical_disease_imc)) +
            geom_treemap() +
            geom_treemap_text(color = "white", place = "centre", grow = TRUE) +
            scale_fill_gradient(low = "#E3F2FD", high = "#1976D2") +
            labs(title = "Disease Category Distribution (Treemap)") +
            theme(legend.position = "none")
        }
      }
    })
    
    # Keep existing outputs (disease_burden_matrix, seasonal_patterns_plot, etc.) with minor enhancements
    # ... (continuing with the rest of the existing code)
    
    # Disease burden matrix (enhanced)
    output$disease_burden_matrix <- renderPlotly({
      req(processed_data())
      
      df <- processed_data()
      
      # Choose base column for severity analysis
      burden_source_col <- dplyr::case_when(
        "canonical_disease_imc" %in% names(df) ~ "canonical_disease_imc",
        "standardized_disease_global" %in% names(df) ~ "standardized_disease_global",
        "morbidity" %in% names(df) ~ "morbidity",
        TRUE ~ NA_character_
      )

      if (is.na(burden_source_col)) {
        return(plot_ly() %>% add_annotations(text = "Disease data not available"))
      }

      # Compute burden data using shared helper when available
      burden_data <- NULL
      if (exists("add_burden_scores")) {
        burden_data <- tryCatch({
          add_burden_scores(df, morbidity_col = burden_source_col, top_n = 20) %>%
            dplyr::rename(label = morbidity)
        }, error = function(e) NULL)
      }

      # Fallback if helper not available or errored
      if (is.null(burden_data)) {
        if (!"category_canonical_disease_imc" %in% colnames(df)) {
          return(plot_ly() %>% add_annotations(text = "Disease categories not available"))
        }

        severity_lookup <- NULL
        if (exists("get_severity_mapping")) {
          raw_map <- get_severity_mapping()
          # Flatten pattern-based map to disease -> severity
          severity_lookup <- purrr::imap_dfr(raw_map, function(vals, sev) {
            tibble::tibble(label = as.character(vals), severity = sev)
          })
        }

        burden_data <- df %>%
          filter(!is.na(category_canonical_disease_imc)) %>%
          count(category_canonical_disease_imc, sort = TRUE) %>%
          slice_head(n = 20) %>%
          rename(label = category_canonical_disease_imc) %>%
          left_join(severity_lookup, by = "label") %>%
          mutate(severity = dplyr::case_when(
            !is.na(severity) ~ severity,
            grepl("malnutrition|complicated", label, ignore.case = TRUE) ~ "High",
            grepl("pneumonia|meningitis|cholera", label, ignore.case = TRUE) ~ "High",
            grepl("diarrhea|diarrhoea|malaria|typhoid", label, ignore.case = TRUE) ~ "Medium",
            TRUE ~ "Low"
          ))
      }

      burden_data <- burden_data %>%
        mutate(
          severity = factor(severity, levels = c("Low", "Medium", "High"), ordered = TRUE),
          burden_score = case_when(
            severity == "High" ~ n * 3,
            severity == "Medium" ~ n * 2,
            TRUE ~ n * 1
          ),
          frequency_rank = rank(-n),
          label = tools::toTitleCase(label)
        )
      
      if (nrow(burden_data) == 0) {
        return(plot_ly() %>% add_annotations(text = "Insufficient data for burden matrix"))
      }
      
      p <- ggplot(burden_data, aes(x = frequency_rank, y = burden_score)) +
        geom_point(aes(size = n, color = severity, text = sprintf("%s<br>Cases: %s", label, scales::comma(n))), alpha = 0.75) +
        scale_color_manual(values = c("High" = "#D32F2F", "Medium" = "#FF6F00", 
                                      "Low" = "#388E3C", "Unknown" = "#757575")) +
        scale_size_continuous(range = c(3, 15), labels = scales::comma_format()) +
        labs(
          title = "Disease Burden Matrix",
          x = "Frequency Rank (1 = most common)",
          y = "Burden Score (frequency × severity weight)",
          color = "Severity Level",
          size = "Cases"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      ggplotly(p, tooltip = "text")
    })
    
    # Enhanced seasonal patterns plot
    output$seasonal_patterns_plot <- renderPlotly({
      req(processed_data())
      
      df <- processed_data()
      date_col <- get_date_col(df)
      
      if (is.null(date_col)) {
        return(plot_ly() %>% add_annotations(text = "Date data not available"))
      }
      
      # Choose what to analyze based on current trend type
      disease_col <- case_when(
        input$trend_type == "specific_disease" ~ "morbidity",
        input$trend_type == "epidemic_disease" ~ "canonical_disease_imc",
        TRUE ~ "category_canonical_disease_imc"
      )
      
      if (!disease_col %in% colnames(df)) {
        return(plot_ly() %>% add_annotations(text = "Disease data not available"))
      }
      
      seasonal_data <- df %>%
        filter(!is.na(.data[[disease_col]])) %>%
        mutate(
          time_unit = case_when(
            input$seasonal_analysis_type == "monthly" ~ lubridate::month(.data[[date_col]], label = TRUE),
            input$seasonal_analysis_type == "quarterly" ~ paste0("Q", lubridate::quarter(.data[[date_col]])),
            input$seasonal_analysis_type == "weekly" ~ as.character(lubridate::wday(.data[[date_col]], label = TRUE))
          )
        ) %>%
        count(time_unit, .data[[disease_col]]) %>%
        group_by(.data[[disease_col]]) %>%
        filter(sum(n) >= 50) %>%  # Only diseases with sufficient cases
        ungroup() %>%
        group_by(time_unit) %>%
        slice_max(n, n = 5) %>%  # Top 5 diseases per time unit
        ungroup()
      
      # Rename column for plotting
      names(seasonal_data)[names(seasonal_data) == disease_col] <- "disease"
      
      p <- ggplot(seasonal_data, aes(x = time_unit, y = n, color = disease, group = disease)) +
        geom_line(linewidth = 1.2, alpha = 0.8) +
        geom_point(size = 2) +
        scale_color_brewer(type = "qual", palette = "Set1") +
        labs(
          title = paste("Seasonal Patterns -", str_to_title(input$seasonal_analysis_type)),
          x = str_to_title(input$seasonal_analysis_type),
          y = "Number of Cases",
          color = "Disease"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      ggplotly(p, tooltip = c("x", "y", "colour"))
    })
    
    # Enhanced download handlers
    output$download_trends_plot <- downloadHandler(
      filename = function() {
        paste0("trend_analysis_", gsub("_", "-", input$trend_type), "_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(trend_data())
        
        # Create plot for download
        trend_df <- trend_data()
        
        p <- ggplot(trend_df, aes(x = time_period, y = cases, color = category)) +
          {if (input$trends_chart_type == "area") geom_area(aes(fill = category), alpha = 0.7, position = "stack")
            else if (input$trends_chart_type == "facet") list(geom_line(linewidth = 1.2), geom_point(size = 2), facet_wrap(~category, scales = "free_y"))
            else list(geom_line(linewidth = 1.2), geom_point(size = 2))} +
          scale_color_brewer(type = "qual", palette = "Set2") +
          scale_fill_brewer(type = "qual", palette = "Set2") +
          labs(
            title = paste("Time Trends by", str_to_title(gsub("_", " ", input$trend_type))),
            x = str_to_title(input$trends_time_period),
            y = "Number of Cases",
            color = str_to_title(gsub("_", " ", input$trend_type)),
            fill = str_to_title(gsub("_", " ", input$trend_type))
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14, face = "bold"),
            legend.position = "bottom"
          ) +
          scale_y_continuous(labels = scales::comma_format())
        
        ggsave(file, plot = p, width = 12, height = 8, dpi = 300)
      }
    )
    
    output$download_trends_data <- downloadHandler(
      filename = function() {
        paste0("trend_data_", gsub("_", "-", input$trend_type), "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(trend_data())
        write.csv(trend_data(), file, row.names = FALSE)
      }
    )
    
    # Keep existing detailed analysis tables with minor enhancements
    # Disease correlation matrix (new)
    output$disease_correlation_matrix <- DT::renderDataTable({
      req(processed_data())
      
      df <- processed_data()
      date_col <- get_date_col(df)
      
      if (is.null(date_col) || !"category_canonical_disease_imc" %in% colnames(df)) {
        return(data.frame(Message = "Disease correlation data not available"))
      }
      
      # Calculate disease co-occurrence patterns
      correlation_data <- df %>%
        filter(!is.na(category_canonical_disease_imc)) %>%
        mutate(week = floor_date(.data[[date_col]], "week")) %>%
        count(week, category_canonical_disease_imc) %>%
        pivot_wider(names_from = category_canonical_disease_imc, values_from = n, values_fill = 0) %>%
        select(-week)
      
      if (ncol(correlation_data) < 2) {
        return(data.frame(Message = "Insufficient data for correlation analysis"))
      }
      
      # Calculate correlation matrix
      cor_matrix <- cor(correlation_data, use = "complete.obs")
      
      # Convert to long format for display
      cor_results <- cor_matrix %>%
        as.data.frame() %>%
        rownames_to_column("Disease_1") %>%
        pivot_longer(-Disease_1, names_to = "Disease_2", values_to = "Correlation") %>%
        filter(Disease_1 != Disease_2) %>%
        mutate(
          Correlation = round(Correlation, 3),
          Strength = case_when(
            abs(Correlation) > 0.7 ~ "Strong",
            abs(Correlation) > 0.5 ~ "Moderate", 
            abs(Correlation) > 0.3 ~ "Weak",
            TRUE ~ "Very Weak"
          )
        ) %>%
        arrange(desc(abs(Correlation))) %>%
        slice_head(n = 20)
      
      DT::datatable(
        cor_results,
        options = list(
          pageLength = 15,
          dom = 'tip',
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Correlation",
          color = DT::styleInterval(c(-0.5, 0.5), c("red", "black", "green"))
        )
    })
    
    # Keep existing detailed analysis tables
    output$disease_geographic_summary <- DT::renderDataTable({
      req(processed_data())
      
      df <- processed_data()
      
      if (!all(c("category_canonical_disease_imc", "admin1") %in% colnames(df))) {
        return(data.frame(Message = "Geographic disease data not available"))
      }
      
      geo_summary <- df %>%
        filter(!is.na(category_canonical_disease_imc), !is.na(admin1)) %>%
        count(category_canonical_disease_imc, admin1) %>%
        group_by(category_canonical_disease_imc) %>%
        summarise(
          `Total Cases` = sum(n),
          `Regions Affected` = n_distinct(admin1),
          `Primary Region` = admin1[which.max(n)],
          `Primary Region Cases` = max(n),
          `Geographic Spread` = round(n_distinct(admin1) / n_distinct(df$admin1, na.rm = TRUE) * 100, 1),
          .groups = "drop"
        ) %>%
        arrange(desc(`Total Cases`))
      
      DT::datatable(
        geo_summary,
        options = list(
          pageLength = 15,
          dom = 'frtip',
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })
    
    # Enhanced seasonal analysis table
    output$seasonal_disease_analysis <- DT::renderDataTable({
      req(processed_data())
      
      df <- processed_data()
      date_col <- get_date_col(df)
      
      if (is.null(date_col) || !"category_canonical_disease_imc" %in% colnames(df)) {
        return(data.frame(Message = "Date or disease data not available"))
      }
      
      seasonal_analysis <- df %>%
        filter(!is.na(category_canonical_disease_imc)) %>%
        mutate(
          month = lubridate::month(.data[[date_col]], label = TRUE),
          quarter = paste0("Q", lubridate::quarter(.data[[date_col]])),
          season = case_when(
            month %in% c("Dec", "Jan", "Feb") ~ "Winter",
            month %in% c("Mar", "Apr", "May") ~ "Spring",
            month %in% c("Jun", "Jul", "Aug") ~ "Summer",
            TRUE ~ "Autumn"
          )
        ) %>%
        group_by(category_canonical_disease_imc) %>%
        summarise(
          `Total Cases` = n(),
          `Peak Month` = {
            month_counts <- table(month)
            names(month_counts)[which.max(month_counts)]
          },
          `Peak Season` = {
            season_counts <- table(season)
            names(season_counts)[which.max(season_counts)]
          },
          `Peak Quarter` = {
            quarter_counts <- table(quarter)
            names(quarter_counts)[which.max(quarter_counts)]
          },
          `Seasonal Variation` = round(sd(table(month)) / mean(table(month)) * 100, 1),
          `Winter Cases (%)` = round(sum(month %in% c("Dec", "Jan", "Feb")) / n() * 100, 1),
          `Summer Cases (%)` = round(sum(month %in% c("Jun", "Jul", "Aug")) / n() * 100, 1),
          .groups = "drop"
        ) %>%
        arrange(desc(`Total Cases`))
      
      DT::datatable(
        seasonal_analysis,
        options = list(
          pageLength = 15,
          dom = 'frtip',
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })
    
    # Enhanced disease priority matrix table
    output$disease_priority_matrix <- DT::renderDataTable({
      req(processed_data())
      
      df <- processed_data()
      
      base_col <- case_when(
        "canonical_disease_imc" %in% names(df) ~ "canonical_disease_imc",
        "category_canonical_disease_imc" %in% names(df) ~ "category_canonical_disease_imc",
        "morbidity" %in% names(df) ~ "morbidity",
        TRUE ~ NA_character_
      )

      if (is.na(base_col)) {
        return(data.frame(Message = "Disease data not available"))
      }

      priority_matrix <- NULL

      if (exists("add_burden_scores")) {
        priority_matrix <- tryCatch({
          add_burden_scores(df, morbidity_col = base_col, top_n = 40) %>%
            mutate(
              severity = as.character(severity),
              priority_score = case_when(
                severity == "High" ~ 5,
                severity == "Medium" ~ 3,
                severity == "Low" ~ 2,
                TRUE ~ 1
              ),
              label = tools::toTitleCase(morbidity)
            )
        }, error = function(e) NULL)
      }

      if (is.null(priority_matrix)) {
        severity_lookup <- NULL
        if (exists("get_severity_mapping")) {
          raw_map <- get_severity_mapping()
          severity_lookup <- purrr::imap_dfr(raw_map, function(vals, sev) {
            tibble::tibble(morbidity = as.character(vals), severity = sev)
          })
        }

        priority_matrix <- df %>%
          filter(!is.na(.data[[base_col]])) %>%
          count(.data[[base_col]], sort = TRUE, name = "n") %>%
          rename(morbidity = !!base_col) %>%
          left_join(severity_lookup, by = "morbidity") %>%
          mutate(
            severity = case_when(
              !is.na(severity) ~ severity,
              grepl("meningitis|cholera|malnutrition|myocardial", morbidity, ignore.case = TRUE) ~ "High",
              grepl("pneumonia|diarrhea|malaria|typhoid|dengue", morbidity, ignore.case = TRUE) ~ "Medium",
              TRUE ~ "Low"
            ),
            priority_score = case_when(
              severity == "High" ~ 5,
              severity == "Medium" ~ 3,
              severity == "Low" ~ 2,
              TRUE ~ 1
            ),
            label = tools::toTitleCase(morbidity)
          )
      }

      if (nrow(priority_matrix) == 0) {
        return(data.frame(Message = "No disease records available"))
      }

      priority_matrix <- priority_matrix %>%
        mutate(
          intervention_urgency = case_when(
            priority_score >= 5 ~ "Immediate",
            priority_score >= 4 ~ "High",
            priority_score >= 3 ~ "Standard",
            TRUE ~ "Low"
          ),
          overall_priority = priority_score * log10(n + 1),
          case_burden = case_when(
            n >= 1000 ~ "Very High",
            n >= 500 ~ "High", 
            n >= 100 ~ "Medium",
            TRUE ~ "Low"
          )
        ) %>%
        select(
          `Disease` = label,
          `Total Cases` = n,
          `Case Burden` = case_burden,
          `Severity` = severity,
          `Priority Score` = priority_score,
          `Intervention Urgency` = intervention_urgency,
          `Overall Priority` = overall_priority
        ) %>%
        arrange(desc(`Overall Priority`))
      
      DT::datatable(
        priority_matrix,
        options = list(
          pageLength = 15,
          dom = 'frtip',
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound("Overall Priority", 2) %>%
        DT::formatStyle(
          "Intervention Urgency",
          backgroundColor = DT::styleEqual(
            c("Immediate", "High", "Standard", "Low"),
            c("#ffebee", "#fff3e0", "#f3e5f5", "#e8f5e8")
          )
        )
    })
    
    # Return trend data for other modules
    return(trend_data)
  })
}
