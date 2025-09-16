# app/server.R — PERFORMANCE-OPTIMIZED server for faster data loading
# - Async data loading with progress indicators
# - Efficient filter operations
# - Lazy module initialization
# - Optimized reactive patterns

# Load required libraries for server functionality
library(dplyr, warn.conflicts = FALSE)

if (!exists("case_when")) {
  # Add case_when if not available from dplyr
  case_when <- function(...) {
    dots <- list(...)
    n <- length(dots)
    if (n == 0) return(logical(0))
    
    # Get the length from the first condition
    first_cond <- tryCatch(eval(dots[[1]][[2]], parent.frame()), 
                          error = function(e) TRUE)
    result_length <- if (is.logical(first_cond)) length(first_cond) else 1
    result <- rep(NA, result_length)
    
    for (i in seq_len(n)) {
      formula <- dots[[i]]
      if (inherits(formula, "formula")) {
        condition <- eval(formula[[2]], parent.frame())
        value <- eval(formula[[3]], parent.frame())
        
        # Handle TRUE as catch-all
        if (length(condition) == 1 && isTRUE(condition)) {
          result[is.na(result)] <- value
        } else if (is.logical(condition)) {
          result[condition & is.na(result)] <- value
        }
      }
    }
    return(result)
  }
}

server <- function(input, output, session) {
  # Read URL query parameters for country override (e.g., ?country=synthetic)
  observe({
    query <- try(parseQueryString(session$clientData$url_search), silent = TRUE)
    if (!inherits(query, "try-error") && length(query)) {
      if (!is.null(query$country) && tolower(query$country) == "synthetic") {
        Sys.setenv(USE_SYNTHETIC_DATA = "true")
        notify("Synthetic mode enabled via URL parameter", "warning", 5000)
      }
    }
  })
  
  # ────────────────────────────────────────────────────────────────────────────
  # PERFORMANCE: Progress and session setup
  # ────────────────────────────────────────────────────────────────────────────
  
  # Define logging functions if they don't exist
  if (!exists("log_info", mode = "function")) {
    log_info <- function(msg, ...) cat("[INFO]", sprintf(msg, ...), "\n")
  }
  if (!exists("log_warn", mode = "function")) {
    log_warn <- function(msg, ...) cat("[WARN]", sprintf(msg, ...), "\n")
  }
  if (!exists("log_error", mode = "function")) {
    log_error <- function(msg, ...) cat("[ERROR]", sprintf(msg, ...), "\n")
  }
  
  # Create progress indicator
  progress <- NULL
  
  show_progress <- function(message, value = 0) {
    tryCatch({
      if (is.null(progress)) {
        progress <<- shiny::Progress$new()
        progress$set(message = message, value = value)
      } else {
        progress$set(message = message, value = value)
      }
    }, error = function(e) {
      # Fallback if progress fails
      cat("[PROGRESS]", message, "\n")
    })
  }
  
  hide_progress <- function() {
    tryCatch({
      if (!is.null(progress)) {
        progress$close()
        progress <<- NULL
      }
    }, error = function(e) {
      # Silent fail for progress cleanup
      progress <<- NULL
    })
  }
  
  # Session info (lightweight)
  output$session_info <- renderPrint({
    list(
      app = if(exists("app_meta")) app_meta$title else "Epi Dashboard",
      modular = if (exists("is_modular_structure")) is_modular_structure() else FALSE,
      timestamp = Sys.time()
    )
  })

  # Synthetic badge in header
  output$synthetic_badge <- shiny::renderUI({
    if (synthetic_active()) {
      tags$span(
        style = "margin-right:12px;",
        shiny::tags$span(
          class = "label label-warning",
          style = "background:#ff9800; color:white; font-weight:600;",
          HTML("<i class='fa fa-vial' style='margin-right:6px;'></i> Synthetic Data")
        )
      )
    } else {
      NULL
    }
  })

  # Expose synthetic mode flag for UI conditionalPanel
  output$synthetic_mode_on <- shiny::renderText({
    if (synthetic_active()) "true" else "false"
  })
  outputOptions(output, "synthetic_mode_on", suspendWhenHidden = FALSE)

  output$header_branding <- renderUI({
    epi_logo <- tags$img(src = "epione_logo.png", alt = "EpiOne")
    if (synthetic_active()) {
      tags$div(class = "header-branding", epi_logo)
    } else {
      imc_logo <- tags$img(src = "imc_logo.png", alt = "International Medical Corps")
      tags$div(class = "header-branding", epi_logo, imc_logo)
    }
  })

  output$branding_banner <- renderUI({
    epi_logo <- tags$img(src = "epione_logo.png", alt = "EpiOne")
    if (synthetic_active()) {
      tags$div(
        class = "branding-banner",
        epi_logo,
        tags$div(
          tags$strong("Synthetic Demonstration"),
          HTML("These exploratory templates were produced by International Medical Corps for testing and training only. International Medical Corps assumes no liability for operational decisions made from synthetic outputs.")
        )
      )
    } else {
      imc_logo <- tags$img(src = "imc_logo.png", alt = "International Medical Corps")
      tags$div(
        class = "branding-banner",
        epi_logo,
        imc_logo,
        tags$div(
          tags$strong("Powered by EpiOne"),
          "Joint branding for active country deployments supported by International Medical Corps."
        )
      )
    }
  })
  
  # Advanced module availability (cached)
  output$predictive_module_available <- reactive({
    exists("predictive_analytics_UI") && exists("predictive_analytics_Server")
  })
  outputOptions(output, "predictive_module_available", suspendWhenHidden = FALSE)
  
  output$correlation_module_available <- reactive({
    exists("multi_disease_correlation_UI") && exists("multi_disease_correlation_Server")
  })
  outputOptions(output, "correlation_module_available", suspendWhenHidden = FALSE)
  
  output$climate_module_available <- reactive({
    exists("climate_integration_UI") && exists("climate_integration_Server")
  })
  outputOptions(output, "climate_module_available", suspendWhenHidden = FALSE)
  
  # ────────────────────────────────────────────────────────────────────────────
  # PERFORMANCE: Optimized data loading with async patterns
  # ────────────────────────────────────────────────────────────────────────────
  
  values <- reactiveValues(
    raw = NULL,
    register = NULL,
    meta = NULL,
    demo_mode = FALSE,
    synthetic_mode = FALSE,
    loading = FALSE,
    load_error = NULL,
    last_refresh = NULL
  )
  
  # PERFORMANCE: Fast notification helper - FIXED
  notify <- function(msg, type = "message", duration = 3000) {
    # Ensure type is valid for shiny notifications
    valid_types <- c("default", "message", "warning", "error")
    if (!type %in% valid_types) {
      type <- "message"
    }
    # Map "success" to "message" since Shiny doesn't have "success" type
    if (type == "success") {
      type <- "message"
    }
    showNotification(msg, type = type, duration = duration)
  }
  
  # Try to enable synthetic generator if present
  if (!exists("generate_synthetic_register")) {
    syn_path <- here::here("R", "synthetic_data.R")
    if (file.exists(syn_path)) {
      try(suppressWarnings(sys.source(syn_path, envir = .GlobalEnv)), silent = TRUE)
    }
  }

  synthetic_enabled <- function() {
    # Use config flag or env var
    cfg <- try(get("app_cfg", envir = .GlobalEnv), silent = TRUE)
    by_cfg <- try(isTRUE(cfg$testing$use_synthetic_data), silent = TRUE)
    by_env <- tolower(Sys.getenv("USE_SYNTHETIC_DATA", "false")) %in% c("1", "true", "yes")
    isTRUE(by_cfg) || isTRUE(by_env)
  }

  synthetic_active <- function() {
    flag <- FALSE
    if (!is.null(values$synthetic_mode) && isTRUE(values$synthetic_mode)) flag <- TRUE
    if (!flag) {
      cfg <- try(get("app_cfg", envir = .GlobalEnv), silent = TRUE)
      flag <- try(isTRUE(cfg$testing$use_synthetic_data), silent = TRUE)
    }
    if (!flag) {
      flag <- tolower(Sys.getenv("USE_SYNTHETIC_DATA", "false")) %in% c("1", "true", "yes")
    }
    isTRUE(flag)
  }

  # PERFORMANCE: Optimized data loader
  load_data_optimized <- function(force_refresh = FALSE) {
    values$loading <- TRUE
    values$load_error <- NULL

    show_progress("Initializing data load...", 0.1)

    # Branch early to synthetic mode if enabled
    if (synthetic_enabled() && exists("generate_synthetic_register")) {
      show_progress("Generating synthetic data...", 0.5)
      cfg <- try(get("app_cfg", envir = .GlobalEnv), silent = TRUE)
      args <- try(resolve_synthetic_args(cfg), silent = TRUE)
      if (inherits(args, "try-error") || is.null(args)) {
        args <- list(n = 5000, months = 12, country = "syria", facilities = 50)
      }
      demo_data <- do.call(generate_synthetic_register, args)
      values$demo_mode <- TRUE
      values$synthetic_mode <- TRUE
      return(process_load_result(list(register = demo_data, data = demo_data)))
    }
    
    # PERFORMANCE: Try cache first (fastest path)
    if (!force_refresh) {
      show_progress("Checking cache...", 0.2)
      
      if (exists("load_unified_data")) {
        result <- tryCatch({
          load_unified_data(country_code = "default", force_refresh = FALSE)
        }, error = function(e) {
          log_warn(paste("Cache/API load failed:", e$message))
          NULL
        })
        
        if (!is.null(result)) {
          show_progress("Processing data...", 0.8)
          return(process_load_result(result))
        }
      }
    }
    
    # PERFORMANCE: API fetch with progress
    show_progress("Fetching from API...", 0.4)
    
    if (exists("load_unified_data")) {
      result <- tryCatch({
        load_unified_data(country_code = "default", force_refresh = TRUE)
      }, error = function(e) {
        log_error(paste("API load failed:", e$message))
        list(error = e$message)
      })
      
      if (!is.null(result) && is.null(result$error)) {
        show_progress("Processing data...", 0.8)
        return(process_load_result(result))
      }
    }
    
    # PERFORMANCE: Fast demo fallback
    show_progress("Generating demo data...", 0.6)
    demo_data <- if (exists("generate_synthetic_register")) {
      # Use the richer synthetic generator even as demo fallback
      generate_synthetic_register(n = 1500, months = 3, country = "syria", facilities = 30)
    } else {
      generate_demo_data_fast()
    }
    values$demo_mode <- TRUE
    return(process_load_result(list(register = demo_data, data = demo_data)))
  }
  
  # PERFORMANCE: Fast result processing
  process_load_result <- function(result) {
    show_progress("Finalizing...", 0.9)
    
    # Normalize data structure
    if (is.data.frame(result)) {
      values$raw <- result
      values$register <- result
    } else if (is.list(result)) {
      values$raw <- result$register %||% result$data %||% result
      values$register <- values$raw
    }
    
    # PERFORMANCE: Skip heavy metadata loading for now
    values$meta <- NULL
    
    # PERFORMANCE: Minimal data validation
    if (is.null(values$register) || !is.data.frame(values$register) || nrow(values$register) == 0) {
      values$load_error <- "No valid data available"
      return(FALSE)
    }
    
    # PERFORMANCE: Ensure required columns exist (fast)
    ensure_required_columns(values$register)
    
    # TAXONOMY: Apply comprehensive disease categorization
    show_progress("Applying taxonomy categorization...", 0.95)
    values$register <- apply_taxonomy_processing(values$register)
    
    values$loading <- FALSE
    values$last_refresh <- Sys.time()
    hide_progress()
    
    notify("Data loaded successfully!", "message")
    if (values$demo_mode) notify("Demo mode active", "warning", 5000)
    
    return(TRUE)
  }
  
  # PERFORMANCE: Fast demo data generator
  generate_demo_data_fast <- function(n = 1500) {  # Smaller for speed
    set.seed(42)  # Consistent seed
    
    days <- seq.Date(Sys.Date() - 90, Sys.Date(), by = "day")  # Smaller range
    
    # Create realistic diseases that map to taxonomy categories
    diseases <- c(
      "Pneumonia", "Acute Upper Respiratory Infection", "Influenza-Like Illness", "COVID-19",
      "Acute Watery Diarrhoea (Suspected Cholera)", "Typhoid Fever", "Acute Bloody Diarrhoea (Suspected Shigellosis)",
      "Malaria (Suspected)", "Dengue", "Vector-borne Disease",
      "Measles", "Diphtheria", "Pertussis",
      "Diabetes Mellitus", "Hypertension", "Asthma",
      "Fracture", "Burn", "Trauma",
      "Conjunctivitis", "Dermatitis", "Acute Sinusitis",
      "Urinary Tract Infection", "Pregnancy Complication",
      "Malnutrition", "Iron Deficiency Anaemia",
      "Depression", "Anxiety Disorder"
    )
    
    data.frame(
      orgunit = sample(paste0("FAC", sprintf("%03d", 1:30)), n, TRUE),  # Fewer facilities
      admin1 = sample(c("Aleppo", "Damascus", "Homs", "Hama", "Idlib"), n, TRUE),
      admin2 = sample(paste("District", 1:8), n, TRUE),  # Fewer districts
      sex = sample(c("Female", "Male"), n, TRUE),
      age = sample(0:80, n, TRUE),
      datevisit = sample(days, n, TRUE),
      datevisitnew = sample(days, n, TRUE),
      morbidity = sample(diseases, n, TRUE),
      morbidity_category = "To be processed",  # Will be overridden by taxonomy processing
      type_case = sample(c("Trauma", "Non-trauma"), n, TRUE, prob = c(0.15, 0.85)),
      age_group_new = cut(sample(0:80, n, TRUE), 
                          breaks = c(-Inf, 5, 15, 50, Inf), 
                          labels = c("0-5 y", "6-14 y", "15-49 y", "50+ y")),
      stringsAsFactors = FALSE
    )
  }
  
  # PERFORMANCE: Fast column creation - FIXED
  ensure_required_columns <- function(df) {
    # Work with a copy to avoid modifying the original
    result_df <- df
    
    if (!"datevisit" %in% names(result_df)) {
      date_cols <- c("datevisitnew", "visit_date", "date", "eventdate")
      existing_date_col <- intersect(date_cols, names(result_df))[1]
      if (!is.na(existing_date_col)) {
        names(result_df)[names(result_df) == existing_date_col] <- "datevisit"
      } else {
        result_df$datevisit <- Sys.Date() - sample(0:90, nrow(result_df), replace = TRUE)
      }
    }
    
    # Ensure datevisitnew exists
    if (!"datevisitnew" %in% names(result_df) && "datevisit" %in% names(result_df)) {
      result_df$datevisitnew <- result_df$datevisit
    }
    
    # Fast age group creation
    if ("age" %in% names(result_df) && !"age_group" %in% names(result_df)) {
      result_df$age_group <- cut(result_df$age,
                                 breaks = c(-Inf, 5, 15, 25, 50, 65, Inf),
                                 labels = c("0-4", "5-14", "15-24", "25-49", "50-64", "65+"),
                                 right = FALSE)
    }
    
    # Update the reactive value
    values$register <- result_df
    
    # Return the modified data frame
    return(result_df)
  }
  
  # TAXONOMY: Apply comprehensive disease categorization with ICD-11 mapping
  apply_taxonomy_processing <- function(df) {
    tryCatch({
      cat("🏷️ Applying comprehensive taxonomy processing...\n")
      
      # Step 1: Source the ICD-11 taxonomy functions
      taxonomy_files <- c(
        "R/taxonomy/icd11_integration.R",
        "R/taxonomy/icd11_disease_mapper.R", 
        "R/taxonomy/icd11_taxonomy_mapper.R"
      )
      
      for (file in taxonomy_files) {
        if (file.exists(file)) {
          source(file)
        }
      }
      
      # Step 2: Apply the new taxonomy mapping with feature flags
      if (exists("add_epidemiological_classifications")) {
        cat("🔧 Applying epidemiological classifications with feature flags...\n")
        
        # Create canonical disease column if missing
        if (!"canonical_disease_imc" %in% names(df)) {
          if ("morbidity" %in% names(df)) {
            df$canonical_disease_imc <- df$morbidity
          } else if ("disease" %in% names(df)) {
            df$canonical_disease_imc <- df$disease
          } else {
            df$canonical_disease_imc <- "Unspecified"
          }
        }
        
        # Apply comprehensive taxonomy classification
        df <- add_epidemiological_classifications(df)
        cat("✅ ICD-11 taxonomy mapping with feature flags completed\n")
        
      } else {
        cat("⚠️ ICD-11 taxonomy functions not available, using fallback\n")
        # Fallback to basic pattern matching
        df <- apply_basic_taxonomy_fallback(df)
      }
      
      # Step 3: Ensure backward compatibility - create missing columns
      required_columns <- c(
        "vaccine_preventable", "climate_sensitive", "outbreak_prone_case", 
        "trauma_related", "epidemic_prone", "amr_relevant"
      )
      
      for (col in required_columns) {
        if (!col %in% names(df)) {
          df[[col]] <- FALSE
          cat("⚠️ Created missing column:", col, "\n")
        }
      }
      
      return(df)
      
    }, error = function(e) {
      cat("⚠️ Error in taxonomy processing:", e$message, "\n")
      # Return original data with basic fallback processing
      df <- apply_basic_taxonomy_fallback(df)
      return(df)
    })
  }
  
  # Fallback taxonomy processing for when ICD-11 system isn't available
  apply_basic_taxonomy_fallback <- function(df) {
    cat("🔧 Applying basic taxonomy fallback...\n")
    
    # Ensure morbidity_category exists
    if (!"morbidity_category" %in% names(df) || all(is.na(df$morbidity_category))) {
      if ("morbidity" %in% names(df)) {
        df$morbidity_category <- case_when(
          grepl("pneumonia|respiratory|lung|breathing|uri|lri|sari|ili|covid|influenza", 
                df$morbidity, ignore.case = TRUE) ~ "Respiratory Infectious Diseases",
          grepl("diarrhea|diarrhoea|cholera|gastroenteritis|stomach|intestinal|typhoid", 
                df$morbidity, ignore.case = TRUE) ~ "Enteric & Food/Water-borne Infections",
          grepl("malaria|dengue|fever|vector", 
                df$morbidity, ignore.case = TRUE) ~ "Vector-borne Diseases",
          grepl("measles|vaccination|immuniz|vaccine", 
                df$morbidity, ignore.case = TRUE) ~ "Vaccine-Preventable Diseases",
          grepl("trauma|injury|accident|wound|fracture|burn", 
                df$morbidity, ignore.case = TRUE) ~ "Injury & External Causes",
          grepl("diabetes|hypertension|heart|cardiovascular|copd|asthma", 
                df$morbidity, ignore.case = TRUE) ~ "Non-Communicable Diseases (non-MNS)",
          grepl("skin|dermatitis|rash|conjunctivitis|eye|ear", 
                df$morbidity, ignore.case = TRUE) ~ "Skin, Eye & ENT Disorders",
          grepl("pregnancy|maternal|delivery|birth|reproductive", 
                df$morbidity, ignore.case = TRUE) ~ "Maternal, Newborn & Reproductive Health",
          grepl("malnutrition|underweight|anemia", 
                df$morbidity, ignore.case = TRUE) ~ "Nutrition",
          grepl("depression|anxiety|mental|neurological", 
                df$morbidity, ignore.case = TRUE) ~ "Mental, Neurological & Substance Use (MNS) Disorders",
          TRUE ~ "General Signs/Symptoms & Unspecified"
        )
      } else {
        df$morbidity_category <- "General Signs/Symptoms & Unspecified"
      }
    }
    
    # Basic feature flags using pattern matching
    if ("morbidity" %in% names(df)) {
      # Vaccine preventable
      df$vaccine_preventable <- grepl(
        "measles|polio|diphtheria|pertussis|mumps|rubella|hepatitis a|hepatitis b|yellow fever", 
        df$morbidity, ignore.case = TRUE
      )
      
      # Climate sensitive  
      df$climate_sensitive <- grepl(
        "malaria|dengue|cholera|diarrhea|diarrhoea|vector", 
        df$morbidity, ignore.case = TRUE
      )
      
      # Outbreak prone
      df$outbreak_prone_case <- grepl(
        "malaria|dengue|cholera|measles|influenza|covid|typhoid|meningitis|yellow fever|plague|ebola|lassa|polio", 
        df$morbidity, ignore.case = TRUE
      )
      df$epidemic_prone <- df$outbreak_prone_case
      
      # Trauma related
      df$trauma_related <- grepl(
        "trauma|injury|accident|wound|fracture|burn|gunshot|blast", 
        df$morbidity, ignore.case = TRUE
      )
      
      # AMR relevant
      df$amr_relevant <- grepl(
        "tuberculosis|pneumonia|typhoid|gonorrhoea|bacterial|sepsis", 
        df$morbidity, ignore.case = TRUE
      )
    } else {
      # Initialize all flags as FALSE if no morbidity column
      df$vaccine_preventable <- FALSE
      df$climate_sensitive <- FALSE
      df$outbreak_prone_case <- FALSE
      df$epidemic_prone <- FALSE
      df$trauma_related <- FALSE
      df$amr_relevant <- FALSE
    }
    
    return(df)
  }
  
  # ────────────────────────────────────────────────────────────────────────────
  # PERFORMANCE: Initial data load (async)
  # ────────────────────────────────────────────────────────────────────────────
  
  # Load data once at startup
  observeEvent(TRUE, {
    load_data_optimized(force_refresh = FALSE)
  }, once = TRUE)
  
  # Data availability reactive
  output$data_available <- reactive({
    !is.null(values$register) && is.data.frame(values$register) && nrow(values$register) > 0
  })
  outputOptions(output, "data_available", suspendWhenHidden = FALSE)
  
  # ────────────────────────────────────────────────────────────────────────────
  # PERFORMANCE: Optimized filters with debouncing
  # ────────────────────────────────────────────────────────────────────────────
  
  # PERFORMANCE: Update filter choices efficiently
  observe({
    req(values$register)
    df <- values$register
    
    # Batch all filter updates to avoid multiple re-renders
    isolate({
      # Date range
      if ("datevisit" %in% names(df)) {
        date_range <- range(df$datevisit, na.rm = TRUE)
        updateDateRangeInput(session, "date_range",
                             start = date_range[1], end = date_range[2],
                             min = date_range[1], max = date_range[2])
      }
      
      # Other filters (only update if they have reasonable number of options)
      if ("admin1" %in% names(df)) {
        regions <- sort(unique(df$admin1[!is.na(df$admin1)]))
        if (length(regions) <= 50) {  # Performance limit
          updateSelectInput(session, "selected_regions", choices = regions)
        }
      }
      
      if ("orgunit" %in% names(df)) {
        facilities <- sort(unique(df$orgunit[!is.na(df$orgunit)]))
        if (length(facilities) <= 100) {  # Performance limit
          updateSelectInput(session, "selected_facilities", choices = facilities)
        }
      }
      
      if ("sex" %in% names(df)) {
        sex_values <- sort(unique(df$sex[!is.na(df$sex)]))
        updateSelectInput(session, "selected_sex", choices = sex_values)
      }
      
      # Age groups
      age_col <- if ("age_group_new" %in% names(df)) "age_group_new" else "age_group"
      if (age_col %in% names(df)) {
        age_groups <- sort(unique(df[[age_col]][!is.na(df[[age_col]])]))
        updateSelectInput(session, "selected_age_groups", choices = age_groups)
      }
    })
  })
  
  # PERFORMANCE: Debounced filter application
  filter_trigger <- reactiveVal(0)
  
  # Debounce filter changes
  observeEvent(list(
    input$date_range, input$selected_regions, input$selected_facilities,
    input$selected_sex, input$selected_age_groups
  ), {
    # Use debounce to avoid excessive filtering
    invalidateLater(500, session)  # Wait 500ms before applying filters
    filter_trigger(filter_trigger() + 1)
  }, ignoreInit = TRUE)
  
  # PERFORMANCE: Optimized filter application
  apply_filters_fast <- function(df) {
    if (!is.data.frame(df) || nrow(df) == 0) return(df)
    
    # Use data.table-style filtering for speed if dataset is large
    if (nrow(df) > 10000 && requireNamespace("data.table", quietly = TRUE)) {
      # Convert to data.table for faster filtering
      dt <- data.table::as.data.table(df)
      
      # Apply filters with proper column checking
      if (!is.null(input$date_range) && length(input$date_range) == 2 && "datevisit" %in% names(dt)) {
        dt <- dt[datevisit >= as.Date(input$date_range[1]) & 
                   datevisit <= as.Date(input$date_range[2])]
      }
      
      if (length(input$selected_regions) > 0 && "admin1" %in% names(dt)) {
        dt <- dt[admin1 %in% input$selected_regions]
      }
      
      if (length(input$selected_facilities) > 0 && "orgunit" %in% names(dt)) {
        dt <- dt[orgunit %in% input$selected_facilities]
      }
      
      if (length(input$selected_sex) > 0 && "sex" %in% names(dt)) {
        dt <- dt[sex %in% input$selected_sex]
      }
      
      # Age group filtering
      age_col <- if ("age_group_new" %in% names(dt)) "age_group_new" else "age_group"
      if (length(input$selected_age_groups) > 0 && age_col %in% names(dt)) {
        dt <- dt[get(age_col) %in% input$selected_age_groups]
      }
      
      # Convert back to data.frame
      return(as.data.frame(dt))
    } else {
      # Use dplyr for smaller datasets
      result <- df
      
      # Date filter
      if (!is.null(input$date_range) && length(input$date_range) == 2 && "datevisit" %in% names(result)) {
        result <- result %>%
          dplyr::filter(datevisit >= as.Date(input$date_range[1]) & 
                          datevisit <= as.Date(input$date_range[2]))
      }
      
      # Other filters
      if (length(input$selected_regions) > 0 && "admin1" %in% names(result)) {
        result <- result %>% dplyr::filter(admin1 %in% input$selected_regions)
      }
      
      if (length(input$selected_facilities) > 0 && "orgunit" %in% names(result)) {
        result <- result %>% dplyr::filter(orgunit %in% input$selected_facilities)
      }
      
      if (length(input$selected_sex) > 0 && "sex" %in% names(result)) {
        result <- result %>% dplyr::filter(sex %in% input$selected_sex)
      }
      
      age_col <- if ("age_group_new" %in% names(result)) "age_group_new" else "age_group"
      if (length(input$selected_age_groups) > 0 && age_col %in% names(result)) {
        result <- result %>% dplyr::filter(.data[[age_col]] %in% input$selected_age_groups)
      }
      
      return(result)
    }
  }
  
  # PERFORMANCE: Main data reactives
  register <- reactive({
    req(values$register)
    values$register
  })
  
  filtered_data <- reactive({
    req(register())
    filter_trigger()  # React to filter changes
    
    result <- apply_filters_fast(register())
    
    # Performance logging for large datasets
    if (nrow(register()) > 10000) {
      log_info(paste("Filtered", nrow(register()), "->", nrow(result), "rows"))
    }
    
    result
  }) %>% bindCache(filter_trigger(), register())  # Cache filtered results
  
  # ────────────────────────────────────────────────────────────────────────────
  # PERFORMANCE: Lazy module initialization
  # ────────────────────────────────────────────────────────────────────────────
  
  # Determine module structure
  using_modular <- exists("is_modular_structure") && is_modular_structure()
  
  if (using_modular) {
    log_info("Initializing NEW MODULAR structure servers...")
    
    # Initialize overview module immediately on startup
    if (exists("mod_overview_server")) {
      tryCatch({
        mod_overview_server("overview", filtered_data)
        cat("✅ Overview module initialized successfully\n")
      }, error = function(e) {
        cat("❌ Overview module initialization failed:", e$message, "\n")
        log_warn(paste("Overview module failed:", e$message))
      })
    }
    
    # Initialize other modules lazily when tabs are visited
    observeEvent(input$sidebar_menu, {
      current_tab <- input$sidebar_menu
      
      if (current_tab == "epibot" && exists("mod_epibot_server")) {
        if (!exists("epibot_initialized")) {
          tryCatch({
            mod_epibot_server("epibot", filtered_data)
            mod_summary_cards_server("epibot_summary", filtered_data)
            assign("epibot_initialized", TRUE, envir = .GlobalEnv)
          }, error = function(e) log_warn(paste("EpiBot module failed:", e$message)))
        }
      }
      
      if (current_tab == "surveillance" && exists("mod_disease_surveillance_server")) {
        if (!exists("surveillance_initialized")) {
          tryCatch({
            mod_disease_surveillance_server("surveillance", filtered_data)
            assign("surveillance_initialized", TRUE, envir = .GlobalEnv)
          }, error = function(e) log_warn(paste("Surveillance module failed:", e$message)))
        }
      }
      
      if (current_tab == "trends" && exists("mod_time_trends_server")) {
        if (!exists("trends_initialized")) {
          tryCatch({
            mod_time_trends_server("trends", filtered_data)
            assign("trends_initialized", TRUE, envir = .GlobalEnv)
          }, error = function(e) log_warn(paste("Trends module failed:", e$message)))
        }
      }
      
      if (current_tab == "geographic" && exists("mod_geographic_server")) {
        if (!exists("geographic_initialized")) {
          tryCatch({
            mod_geographic_server("geographic", filtered_data)
            assign("geographic_initialized", TRUE, envir = .GlobalEnv)
          }, error = function(e) log_warn(paste("Geographic module failed:", e$message)))
        }
      }
      
      if (current_tab == "settings" && exists("mod_settings_server")) {
        if (!exists("settings_initialized")) {
          tryCatch({
            settings_state <<- mod_settings_server("settings", filtered_data)
            assign("settings_initialized", TRUE, envir = .GlobalEnv)
          }, error = function(e) log_warn(paste("Settings module failed:", e$message)))
        }
      }
      
      if (current_tab == "correlations" && exists("multi_disease_correlation_Server")) {
        if (!exists("correlation_initialized")) {
          tryCatch({
            multi_disease_correlation_Server("disease_correlations", filtered_data)
            assign("correlation_initialized", TRUE, envir = .GlobalEnv)
          }, error = function(e) log_warn(paste("Correlation module failed:", e$message)))
        }
      }
      
      if (current_tab == "climate" && exists("climate_integration_Server")) {
        if (!exists("climate_initialized")) {
          tryCatch({
            climate_integration_Server("climate_integration", filtered_data)
            assign("climate_initialized", TRUE, envir = .GlobalEnv)
          }, error = function(e) log_warn(paste("Climate module failed:", e$message)))
        }
      }
    })
    
    # Always initialize summary cards (needed for overview)
    if (exists("mod_summary_cards_server")) {
      tryCatch({
        mod_summary_cards_server("overview_cards", filtered_data)
      }, error = function(e) log_warn(paste("Summary cards failed:", e$message)))
    }
    
    # Initialize advanced modules immediately (not lazily)
    if (exists("predictive_analytics_Server")) {
      tryCatch({
        predictive_analytics_Server("predictive_analytics", filtered_data)
      }, error = function(e) {
        log_warn(paste("Predictive module failed:", e$message))
      })
    }
    
    if (exists("multi_disease_correlation_Server")) {
      tryCatch({
        multi_disease_correlation_Server("disease_correlations", filtered_data)
      }, error = function(e) {
        log_warn(paste("Correlation module failed:", e$message))
      })
    }
    
    if (exists("climate_integration_Server")) {
      tryCatch({
        climate_integration_Server("climate_integration", filtered_data)
      }, error = function(e) {
        log_warn(paste("Climate module failed:", e$message))
      })
    }
    
  } else {
    # Legacy module initialization (simplified)
    log_info("Initializing LEGACY structure servers...")
    
    # Initialize only essential legacy modules
    if (exists("summaryCardsServer")) {
      try(summaryCardsServer("summary_cards", filtered_data), silent = TRUE)
    }
    
    if (exists("chatbotAssistantServer") && isTRUE(ai_flags$chat_enabled)) {
      try(chatbotAssistantServer("epibot", filtered_data), silent = TRUE)
    }
  }
  
  # ────────────────────────────────────────────────────────────────────────────
  # PERFORMANCE: Action handlers
  # ────────────────────────────────────────────────────────────────────────────

  # Listen for settings-driven reload requests
  observe({
    if (exists("settings_state", envir = .GlobalEnv)) {
      ss <- get("settings_state", envir = .GlobalEnv)
      if (is.list(ss) && !is.null(ss$request_reload)) {
        observeEvent(ss$request_reload(), {
          notify("Applying settings and reloading data...", "message")
          success <- load_data_optimized(force_refresh = TRUE)
          if (success) notify("Reloaded successfully", "message") else notify("Reload failed", "error")
        }, ignoreInit = TRUE)
      }
    }
  })
  
  # Refresh data
  observeEvent(input$refresh_data, {
    notify("Refreshing data...", "message")
    success <- load_data_optimized(force_refresh = TRUE)
    if (success) {
      notify("Data refreshed successfully", "message")
    } else {
      notify("Data refresh failed", "error")
    }
  })
  
  # AI Summary
  observeEvent(input$generate_ai_summary, {
    if (isTRUE(ai_flags$enabled)) {
      notify("Generating AI summary...", "info")
    } else {
      notify("AI features not available", "warning")
    }
  })
  
  # PERFORMANCE: Fast download handlers
  output$download_report <- downloadHandler(
    filename = function() paste0("epi_report_", Sys.Date(), ".html"),
    content = function(file) {
      req(filtered_data())
      
      df <- filtered_data()
      total_cases <- nrow(df)
      facilities <- if ("orgunit" %in% names(df)) length(unique(df$orgunit)) else 0
      regions <- if ("admin1" %in% names(df)) length(unique(df$admin1)) else 0
      
      # Minimal HTML report for speed
      html_content <- sprintf(
        "<!DOCTYPE html><html><head><title>Dashboard Report</title></head><body>
        <h1>📊 Epidemiological Dashboard Report</h1>
        <p><strong>Generated:</strong> %s</p>
        <h2>Summary Statistics</h2>
        <ul>
        <li>Total Consultations: %s</li>
        <li>Health Facilities: %d</li>
        <li>Geographic Regions: %d</li>
        <li>Demo Mode: %s</li>
        </ul>
        </body></html>",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        format(total_cases, big.mark = ","),
        facilities, regions,
        ifelse(values$demo_mode, "Yes", "No")
      )
      
      writeLines(html_content, file)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() paste0("epi_data_", Sys.Date(), ".csv"),
    content = function(file) {
      req(filtered_data())
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )

  # Combined export when synthetic mode is on
  output$download_synthetic_export <- downloadHandler(
    filename = function() {
      choice <- isolate(input$synthetic_export_choice)
      if (identical(choice, "filtered")) paste0("filtered_data_", Sys.Date(), ".csv")
      else paste0("synthetic_register_", Sys.Date(), ".csv")
    },
    content = function(file) {
      choice <- isolate(input$synthetic_export_choice)
      if (identical(choice, "filtered")) {
        req(filtered_data())
        write.csv(filtered_data(), file, row.names = FALSE)
      } else {
        # Ensure generator is loaded
        if (!exists("generate_synthetic_register")) {
          syn_path <- here::here("R", "synthetic_data.R")
          if (file.exists(syn_path)) sys.source(syn_path, envir = .GlobalEnv)
        }
        if (exists("generate_synthetic_register")) {
          cfg <- try(get("app_cfg", envir = .GlobalEnv), silent = TRUE)
          args <- try(resolve_synthetic_args(cfg), silent = TRUE)
          if (inherits(args, "try-error") || is.null(args)) {
            args <- list(n = 5000, months = 12, country = "syria", facilities = 60)
          }
          df <- do.call(generate_synthetic_register, args)
          write.csv(df, file, row.names = FALSE)
        } else {
          df <- try(filtered_data(), silent = TRUE)
          if (inherits(df, "try-error") || is.null(df)) df <- data.frame()
          write.csv(df, file, row.names = FALSE)
        }
      }
    }
  )
  
  # ────────────────────────────────────────────────────────────────────────────
  # PERFORMANCE: Fallback value boxes (for legacy compatibility)
  # ────────────────────────────────────────────────────────────────────────────
  
  if (!using_modular || !exists("mod_overview_server")) {
    
    output$total_consultations <- renderValueBox({
      req(filtered_data())
      
      valueBox(
        value = format(nrow(filtered_data()), big.mark = ","),
        subtitle = "Total Consultations",
        icon = icon("user-md"),
        color = "blue"
      )
    })
    
    output$active_facilities <- renderValueBox({
      req(filtered_data())
      
      facilities <- if ("orgunit" %in% names(filtered_data())) {
        length(unique(filtered_data()$orgunit))
      } else { 0 }
      
      valueBox(
        value = facilities,
        subtitle = "Active Facilities", 
        icon = icon("hospital"),
        color = "green"
      )
    })
    
    output$top_disease <- renderValueBox({
      req(filtered_data())
      
      df <- filtered_data()
      if ("morbidity_category" %in% names(df) && nrow(df) > 0) {
        top_disease <- names(sort(table(df$morbidity_category), decreasing = TRUE))[1]
        top_count <- max(table(df$morbidity_category))
      } else {
        top_disease <- "N/A"
        top_count <- 0
      }
      
      valueBox(
        value = top_count,
        subtitle = paste("Top Disease:", substr(top_disease, 1, 15)),
        icon = icon("virus"),
        color = "yellow"
      )
    })
    
    output$recent_trend <- renderValueBox({
      req(filtered_data())
      
      df <- filtered_data()
      if ("datevisit" %in% names(df) && nrow(df) > 0) {
        current_date <- max(df$datevisit, na.rm = TRUE)
        current_week <- sum(df$datevisit >= (current_date - 7), na.rm = TRUE)
        previous_week <- sum(df$datevisit >= (current_date - 14) & 
                               df$datevisit < (current_date - 7), na.rm = TRUE)
        
        change_pct <- if (previous_week > 0) {
          round(((current_week - previous_week) / previous_week) * 100, 1)
        } else { 0 }
        
        color <- if (change_pct > 5) "green" else if (change_pct < -5) "red" else "blue"
        icon_name <- if (change_pct > 5) "arrow-up" else if (change_pct < -5) "arrow-down" else "minus"
      } else {
        change_pct <- 0
        color <- "blue"
        icon_name <- "minus"
      }
      
      valueBox(
        value = paste0(ifelse(change_pct > 0, "+", ""), change_pct, "%"),
        subtitle = "Weekly Trend",
        icon = icon(icon_name),
        color = color
      )
    })
  }
  
  # ────────────────────────────────────────────────────────────────────────────
  # Session cleanup
  # ────────────────────────────────────────────────────────────────────────────
  
  onStop(function() {
    hide_progress()
    log_info("Session ended.")
  })
}
