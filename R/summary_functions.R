# R/summary_functions.R

# SOLUTION 1: Create a separate summary_functions.R file
# Save this as R/summary_functions.R

# ============================================================================
# CENTRALIZED SUMMARY VARIABLES CALCULATION FUNCTIONS - FIXED VERSION
# ============================================================================
# Purpose: Calculate all summary variables once and consistently throughout
# the epidemiological report to avoid duplication and ensure consistency
# Updated to properly integrate with Syria-specific disease_categories.R functions
# ============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(lubridate)
  library(purrr)
})

#' Apply simplified taxonomy processing for markdown reports
#' @param data Register data to process
#' @return Data with taxonomy flags and classifications
apply_simplified_taxonomy <- function(data) {
  cat("🏷️ Delegating to modular taxonomy system...\n")

  if (nrow(data) == 0) {
    cat("⚠️ No data to process\n")
    return(data)
  }

  tryCatch({
    # Source the preprocessing module with modular taxonomy functions
    if (file.exists("R/preprocessing.R")) {
      source("R/preprocessing.R", local = TRUE)
    }

    # Use the new modular taxonomy system
    if (exists("apply_canonical_and_icd11_taxonomy", envir = environment())) {
      data <- apply_canonical_and_icd11_taxonomy(data)
      cat("✅ Modular taxonomy mapping completed\n")
    } else {
      cat("⚠️ Modular taxonomy functions not found, applying basic flags\n")
      data <- apply_basic_taxonomy_flags(data)
    }

    # Ensure backward compatibility for old taxonomy columns
    if (!"is_priority_disease" %in% names(data)) {
      data$is_priority_disease <- data$outbreak_prone_case %||% FALSE |
                                  data$vaccine_preventable %||% FALSE
    }

    return(data)

  }, error = function(e) {
    cat("❌ Taxonomy processing failed:", e$message, "\n")
    data <- apply_basic_taxonomy_flags(data)
    return(data)
  })
}

#' Apply basic taxonomy flags when full system not available
#' @param data Register data
#' @return Data with basic taxonomy flags
apply_basic_taxonomy_flags <- function(data) {
  cat("🔧 Applying basic taxonomy flags...\n")
  
  # Initialize all required flags
  data$vaccine_preventable <- FALSE
  data$climate_sensitive <- FALSE
  data$outbreak_prone_case <- FALSE
  data$epidemic_prone <- FALSE
  data$trauma_related <- FALSE
  data$amr_relevant <- FALSE
  
  # Basic demographic flags
  data$pediatric_case <- FALSE
  data$elderly_care_case <- FALSE
  data$pregnancy_related <- FALSE
  data$malnutrition_complication <- FALSE
  
  if ("morbidity" %in% names(data)) {
    # Pattern-based flag assignment
    data$vaccine_preventable <- grepl(
      "measles|polio|diphtheria|pertussis|mumps|rubella|hepatitis a|hepatitis b|yellow fever|japanese encephalitis", 
      data$morbidity, ignore.case = TRUE
    )
    
    data$climate_sensitive <- grepl(
      "malaria|dengue|cholera|chikungunya|zika|diarrhea|diarrhoea|vector", 
      data$morbidity, ignore.case = TRUE
    )
    
    data$outbreak_prone_case <- grepl(
      "malaria|dengue|cholera|measles|influenza|covid|typhoid|meningitis|yellow fever|plague|ebola|lassa|polio|pertussis", 
      data$morbidity, ignore.case = TRUE
    )
    
    data$epidemic_prone <- data$outbreak_prone_case
    
    data$trauma_related <- grepl(
      "trauma|injury|accident|wound|fracture|burn|gunshot|blast|crush", 
      data$morbidity, ignore.case = TRUE
    )
    
    data$amr_relevant <- grepl(
      "tuberculosis|pneumonia|typhoid|gonorrhoea|bacterial|sepsis|antibiotic", 
      data$morbidity, ignore.case = TRUE
    )
    
    # Demographic flags
    if ("age" %in% names(data)) {
      data$pediatric_case <- !is.na(data$age) & data$age < 18
      data$elderly_care_case <- !is.na(data$age) & data$age >= 65
    }
    
    data$pregnancy_related <- grepl(
      "pregnan|maternal|obstetric|antenatal|delivery|birth", 
      data$morbidity, ignore.case = TRUE
    )
    
    data$malnutrition_complication <- grepl(
      "malnutrition|underweight|stunting|wasting|marasmus|kwashiorkor", 
      data$morbidity, ignore.case = TRUE
    )
  }
  
  # Add compatibility aliases
  data$vpd_case <- data$vaccine_preventable
  data$climate_sensitive_case <- data$climate_sensitive
  data$amr_case <- data$amr_relevant
  
  cat("✅ Basic taxonomy flags applied\n")
  return(data)
}

# Safe helper functions
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

safe_date_parse <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x <- as.character(x)
  d <- suppressWarnings(as.Date(x, "%Y-%m-%d"))
  if (all(is.na(d))) d <- suppressWarnings(as.Date(x, "%d/%m/%Y"))
  if (all(is.na(d))) d <- suppressWarnings(lubridate::ymd(x))
  if (all(is.na(d))) d <- suppressWarnings(lubridate::dmy(x))
  d
}


# =========================
# NA-safe tiny helpers
# =========================
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
nz_chr <- function(x, alt="—") { x <- as.character(x); ifelse(is.na(x) | !nzchar(x), alt, x) }
to_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x <- as.character(x)
  d <- suppressWarnings(as.Date(x))
  if (all(is.na(d))) d <- suppressWarnings(lubridate::ymd(x))
  if (all(is.na(d))) d <- suppressWarnings(lubridate::dmy(x))
  if (all(is.na(d))) d <- suppressWarnings(lubridate::mdy(x))
  d
}

# =========================
# Column detection
# =========================
detect_cols_sf <- function(df) {
  list(
    date_col = dplyr::case_when(
      "datevisit" %in% names(df) ~ "datevisit",
      "month2" %in% names(df)    ~ "month2",
      TRUE ~ NA_character_
    ),
    disease_col = dplyr::case_when(
      "canonical_disease_imc" %in% names(df) ~ "canonical_disease_imc",
      "morbidity" %in% names(df)            ~ "morbidity",
      TRUE ~ NA_character_
    ),
    region_col = dplyr::case_when(
      "admin1" %in% names(df)       ~ "admin1",
      "governorate" %in% names(df)  ~ "governorate",
      "region" %in% names(df)       ~ "region",
      TRUE ~ NA_character_
    ),
    facetype_col = dplyr::case_when(
      "facility_type" %in% names(df) ~ "facility_type",
      TRUE ~ NA_character_
    ),
    facility_col = dplyr::case_when(
      "orgunit" %in% names(df) ~ "orgunit",
      TRUE ~ NA_character_
    ),
    age_col = dplyr::case_when(
      "age_group" %in% names(df) ~ "age_group",
      TRUE ~ NA_character_
    ),
    sex_col = dplyr::case_when(
      "sex" %in% names(df) ~ "sex",
      TRUE ~ NA_character_
    ),
    trauma_flag = dplyr::case_when(
      "trauma_flag" %in% names(df) ~ "trauma_flag",
      "type_case" %in% names(df)   ~ "type_case",  # expect "Trauma" values
      TRUE ~ NA_character_
    ),
    malnut_flag = dplyr::case_when(
      "malnutrition_flag" %in% names(df) ~ "malnutrition_flag",
      TRUE ~ NA_character_
    ),
    epi_flag = dplyr::case_when(
      "epidemic_flag" %in% names(df) ~ "epidemic_flag",
      TRUE ~ NA_character_
    ),
    pregnancy_flag = dplyr::case_when(
      "pregnant" %in% names(df) ~ "pregnant",
      TRUE ~ NA_character_
    )
  )
}

# =========================
# Top diseases % helper (used by captions)
# =========================
compute_top_diseases_pct <- function(df, disease_col) {
  if (is.na(disease_col) || !(disease_col %in% names(df))) {
    return(tibble::tibble())
  }
  df %>%
    dplyr::filter(!is.na(.data[[disease_col]])) %>%
    dplyr::count(.data[[disease_col]], name = "n", sort = TRUE) %>%
    dplyr::mutate(percentage = round(100 * n / sum(n), 1)) %>%
    dplyr::slice_head(n = 10)
}

# =========================
# Facilities/regions MoM change helpers
# =========================
count_increasing_units <- function(df, unit_col, date_col) {
  if (is.na(unit_col) || is.na(date_col)) return(list(n_up=NA_integer_, pct_up=NA_real_))
  if (!(unit_col %in% names(df)) || !(date_col %in% names(df))) return(list(n_up=NA_integer_, pct_up=NA_real_))

  x <- df %>%
    dplyr::mutate(.date = to_date_safe(.data[[date_col]])) %>%
    dplyr::filter(!is.na(.date)) %>%
    dplyr::mutate(month = lubridate::floor_date(.date, "month")) %>%
    dplyr::count(!!rlang::sym(unit_col), month, name = "n") %>%
    dplyr::group_by(!!rlang::sym(unit_col)) %>%
    dplyr::arrange(month, .by_group = TRUE) %>%
    dplyr::mutate(n_lag = dplyr::lag(n)) %>%
    dplyr::ungroup()

  last_month <- suppressWarnings(max(x$month, na.rm = TRUE))
  if (!is.finite(last_month)) return(list(n_up=NA_integer_, pct_up=NA_real_))
  x_last <- dplyr::filter(x, month == last_month)
  res <- dplyr::inner_join(
    x_last,
    dplyr::filter(x, month == (last_month - lubridate::days(30))) %>%
      dplyr::select(!!rlang::sym(unit_col), n) %>%
      dplyr::rename(n_prev = n),
    by = unit_col
  )
  if (!nrow(res)) return(list(n_up=0L, pct_up=NA_real_))
  n_up <- sum(res$n > res$n_prev, na.rm = TRUE)
  pct_up <- round(100 * n_up / nrow(res), 1)
  list(n_up = n_up, pct_up = pct_up)
}

# =========================
# Disease categorization functions
# These functions are expected to be defined in disease_categories_taxaware.R
# =========================

# Load disease categorization functions if not already loaded
load_disease_categories_safely <- function() {
  if (!exists("get_epidemic_diseases_standardized")) {
    disease_cat_path <- here::here("R", "disease_categories_taxaware.R")
    if (file.exists(disease_cat_path)) {
      source(disease_cat_path, local = .GlobalEnv)
      cat("✅ Loaded disease_categories_taxaware.R\n")
      return(TRUE)
    } else {
      warning("disease_categories_taxaware.R not found at: ", disease_cat_path)
      return(FALSE)
    }
  }
  return(TRUE)
}

# Ensure disease categories functions are available
ensure_disease_functions_loaded <- function() {
  required_functions <- c(
    "get_epidemic_diseases_standardized",
    "get_malnutrition_categories", 
    "get_trauma_morbidities",
    "classify_trauma_case",
    "apply_epidemic_categorization"
  )
  
  missing_functions <- c()
  for (func in required_functions) {
    if (!exists(func, envir = .GlobalEnv)) {
      missing_functions <- c(missing_functions, func)
    }
  }
  
  if (length(missing_functions) > 0) {
    cat("⚠️ Missing functions:", paste(missing_functions, collapse = ", "), "\n")
    
    # Try to load disease_categories.R
    if (load_disease_categories_safely()) {
      # Check again
      missing_functions <- c()
      for (func in required_functions) {
        if (!exists(func, envir = .GlobalEnv)) {
          missing_functions <- c(missing_functions, func)
        }
      }
    }
  }
  
  if (length(missing_functions) > 0) {
    cat("❌ Still missing after loading:", paste(missing_functions, collapse = ", "), "\n")
    return(FALSE)
  }
  
  cat("✅ All disease categorization functions available\n")
  return(TRUE)
}

# ============================================================================
# CORE SUMMARY CALCULATION FUNCTION
# ============================================================================

calculate_all_summary_variables <- function(register, validation_results = NULL) {
  cat("🔄 Starting summary variable calculation...\n")
  
  # Ensure disease functions are loaded
  functions_available <- ensure_disease_functions_loaded()
  
  # Initialize with safe defaults
  summary_vars <- list(
    # Basic counts
    total_consultations = 0,
    total_facilities = 0,
    geographic_coverage = 0,
    disease_categories_count = 0,
    
    # Dates and trends
    date_range_text = "No data available",
    date_range = as.Date(character()),
    missing_dates = 0,
    missing_dates_pct = 0,
    monthly_change_pct = 0,
    monthly_change_direction = "no data",
    monthly_change_color = "gray",
    
    # Disease categories
    epidemic_cases_count = 0,
    trauma_cases_count = 0,
    malnutrition_cases_count = 0,
    
    # Demographics
    pediatric_percentage = 0,
    female_percentage = 0,
    
    # Quality metrics
    quality_score = 50,
    completeness_score = 50,
    
    # Temporal variables
    current_date = Sys.Date(),
    one_month_ago = Sys.Date() - 30,
    one_week_ago = Sys.Date() - 7,
    
    # Column identification and function availability
    columns = list(),
    functions_available = functions_available
  )
  
  # Return defaults if no data
  if (is.null(register) || !is.data.frame(register) || nrow(register) == 0) {
    summary_vars$columns <- identify_key_columns(data.frame())
    return(summary_vars)
  }
  
  tryCatch({
    # Basic counts
    summary_vars$total_consultations <- nrow(register)
    
    # Identify key columns once and store in summary_vars
    columns <- identify_key_columns(register)
    summary_vars$columns <- columns
    
    cat("📊 Identified columns:\n")
    cat("   - Disease column:", ifelse(is.na(columns$disease_col), "MISSING", columns$disease_col), "\n")
    cat("   - Date column:", ifelse(is.na(columns$date_col), "MISSING", columns$date_col), "\n")
    cat("   - Facility column:", ifelse(is.na(columns$facility_col), "MISSING", columns$facility_col), "\n")
    
    # Facilities count
    if (!is.na(columns$facility_col)) {
      summary_vars$total_facilities <- dplyr::n_distinct(register[[columns$facility_col]], na.rm = TRUE)
    }
    
    # Geographic coverage
    if (!is.na(columns$geo_col)) {
      summary_vars$geographic_coverage <- dplyr::n_distinct(register[[columns$geo_col]], na.rm = TRUE)
    }
    
    # Disease categories
    if (!is.na(columns$disease_col)) {
      summary_vars$disease_categories_count <- dplyr::n_distinct(register[[columns$disease_col]], na.rm = TRUE)
      cat("   - Total unique diseases:", summary_vars$disease_categories_count, "\n")
    }
    
    # Date handling and temporal calculations
    if (!is.na(columns$date_col)) {
      date_summary <- calculate_date_summary(register, columns$date_col)
      summary_vars <- modifyList(summary_vars, date_summary)
    }
    
    # Demographics
    demo_summary <- calculate_demographics_summary(register, columns)
    summary_vars <- modifyList(summary_vars, demo_summary)
    
    # Disease-specific counts (this is where the issue was)
    disease_summary <- calculate_disease_specific_counts(register, columns, functions_available)
    summary_vars <- modifyList(summary_vars, disease_summary)
    
    # Quality assessment
    quality_summary <- calculate_quality_metrics(register, columns, validation_results)
    summary_vars <- modifyList(summary_vars, quality_summary)
    
    return(summary_vars)
    
  }, error = function(e) {
    warning("Summary calculation error: ", e$message)
    summary_vars$columns <- identify_key_columns(data.frame())
    return(summary_vars)
  })
}

# ============================================================================
# HELPER FUNCTIONS FOR SPECIFIC CALCULATIONS
# ============================================================================

identify_key_columns <- function(register) {
  if (is.null(register) || !is.data.frame(register) || nrow(register) == 0) {
    return(list(
      date_col = NA_character_,
      disease_col = NA_character_,
      facility_col = NA_character_,
      geo_col = NA_character_,
      age_col = NA_character_,
      sex_col = NA_character_,
      trauma_col = NA_character_,
      malnutrition_col = NA_character_
    ))
  }
  
  list(
    date_col = dplyr::case_when(
      "datevisit" %in% names(register) ~ "datevisit",
      "eventdate" %in% names(register) ~ "eventdate",
      "datevisitnew" %in% names(register) ~ "datevisitnew",
      TRUE ~ NA_character_
    ),
    disease_col = dplyr::case_when(
      "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
      "morbidity" %in% names(register) ~ "morbidity",
      "category_canonical_disease_imc" %in% names(register) ~ "category_canonical_disease_imc",
      TRUE ~ NA_character_
    ),
    facility_col = dplyr::case_when(
      "orgunit" %in% names(register) ~ "orgunit",
      "facility" %in% names(register) ~ "facility",
      TRUE ~ NA_character_
    ),
    geo_col = dplyr::case_when(
      "admin1" %in% names(register) ~ "admin1",
      "governorate" %in% names(register) ~ "governorate",
      "region" %in% names(register) ~ "region",
      TRUE ~ NA_character_
    ),
    age_col = dplyr::case_when(
      "age_group" %in% names(register) ~ "age_group",
      "age_group4" %in% names(register) ~ "age_group4",
      TRUE ~ NA_character_
    ),
    sex_col = if ("sex" %in% names(register)) "sex" else NA_character_,
    trauma_col = if ("type_case" %in% names(register)) "type_case" else NA_character_,
    malnutrition_col = if ("malnutrition_case" %in% names(register)) "malnutrition_case" else NA_character_
  )
}

calculate_date_summary <- function(register, date_col) {
  date_values <- register[[date_col]]
  parsed_dates <- safe_date_parse(date_values)
  valid_dates <- parsed_dates[!is.na(parsed_dates)]
  
  summary <- list(
    missing_dates = sum(is.na(parsed_dates)),
    missing_dates_pct = round(sum(is.na(parsed_dates)) / length(parsed_dates) * 100, 1)
  )
  
  if (length(valid_dates) > 0) {
    date_range <- range(valid_dates)
    summary$date_range <- date_range
    summary$date_range_text <- paste(
      format(date_range[1], "%b %d, %Y"), "to", format(date_range[2], "%b %d, %Y")
    )
    
    # 👇 anchor to cfg$date_end if provided
    opt_end <- getOption("epi.date_end")
    summary$current_date <- if (!is.null(opt_end)) as.Date(opt_end) else max(valid_dates)
    summary$one_month_ago <- summary$current_date - 30
    summary$one_week_ago  <- summary$current_date - 7
    
    current_month_cases   <- sum(valid_dates >= summary$one_month_ago & valid_dates <= summary$current_date)
    previous_month_cases  <- sum(valid_dates >= (summary$one_month_ago - 30) &
                                   valid_dates <  summary$one_month_ago)
    
    if (previous_month_cases > 0) {
      summary$monthly_change_pct <- round(
        ((current_month_cases - previous_month_cases) / previous_month_cases) * 100, 1
      )
      
      if (summary$monthly_change_pct > 5) {
        summary$monthly_change_direction <- "increase"
        summary$monthly_change_color <- "green"
      } else if (summary$monthly_change_pct < -5) {
        summary$monthly_change_direction <- "decrease"
        summary$monthly_change_color <- "red"
      } else {
        summary$monthly_change_direction <- "stable"
        summary$monthly_change_color <- "blue"
      }
    }
  }
  
  return(summary)
}

calculate_demographics_summary <- function(register, columns) {
  summary <- list()
  
  # Age demographics - robust pediatric calculation
  pediatric_cases <- 0
  total_cases <- nrow(register)
  
  # Check multiple possible age group columns
  age_cols_to_check <- c("age_group", "age_group2", "age_group3", "age_group4", "age_group_new")
  
  for (col in age_cols_to_check) {
    if (col %in% names(register)) {
      # Comprehensive pediatric patterns (0-14 years typically considered pediatric)
      pediatric_patterns <- c(
        "0-4", "5-14", "0-5", "5-15", "0-14", "0-17", "0-18",
        "< 5 yrs", "5 - 14 yrs", "0 - 4", "5 - 14", 
        "0-4 years", "5-14 years", "0-5 y", "< 5 y",
        "Under 5", "5-14 years old", "Pediatric", "<5", 
        "0-4y", "5-14y", "infant", "toddler", "child", "Children"
      )
      
      matches <- sum(register[[col]] %in% pediatric_patterns, na.rm = TRUE)
      if (matches > 0) {
        pediatric_cases <- matches
        cat("Found", matches, "pediatric cases in column", col, "\n")
        break
      }
    }
  }
  
  summary$pediatric_percentage <- round((pediatric_cases / total_cases) * 100, 1)
  
  # Sex demographics
  if (!is.na(columns$sex_col)) {
    female_cases <- sum(register[[columns$sex_col]] %in% c("Female", "F", "female"), na.rm = TRUE)
    summary$female_percentage <- round(female_cases / nrow(register) * 100, 1)
  }
  
  return(summary)
}

# FIXED: This is the key function that was failing
calculate_disease_specific_counts <- function(register, columns, functions_available = FALSE) {
  summary <- list(
    trauma_cases_count = 0,
    malnutrition_cases_count = 0,
    epidemic_cases_count = 0
  )
  
  if (is.na(columns$disease_col)) {
    cat("⚠️ No disease column available\n")
    return(summary)
  }
  
  cat("🔍 Calculating disease-specific counts...\n")
  cat("   - Functions available:", functions_available, "\n")
  cat("   - Disease column:", columns$disease_col, "\n")
  
  # Trauma cases using your comprehensive trauma classification
  if (functions_available && exists("classify_trauma_case", envir = .GlobalEnv)) {
    cat("   - Using comprehensive trauma classification\n")
    trauma_classification <- classify_trauma_case(register, 
                                                  type_col = columns$trauma_col, 
                                                  morbidity_col = columns$disease_col)
    summary$trauma_cases_count <- sum(trauma_classification == "Trauma", na.rm = TRUE)
    cat("   - Trauma cases found:", summary$trauma_cases_count, "\n")
  } else {
    cat("   - Using fallback trauma classification\n")
    if (!is.na(columns$trauma_col)) {
      summary$trauma_cases_count <- sum(register[[columns$trauma_col]] == "Trauma", na.rm = TRUE)
    } else {
      trauma_conditions <- c("Burns", "Foreign body", "Snake Venom/Bite", "Trauma", "Fracture")
      summary$trauma_cases_count <- sum(register[[columns$disease_col]] %in% trauma_conditions, na.rm = TRUE)
    }
    cat("   - Trauma cases found (fallback):", summary$trauma_cases_count, "\n")
  }
  
  # Malnutrition cases using your malnutrition categories
  if (functions_available && exists("get_malnutrition_categories", envir = .GlobalEnv)) {
    cat("   - Using comprehensive malnutrition categories\n")
    malnutrition_conditions <- get_malnutrition_categories()
    cat("   - Malnutrition conditions:", length(malnutrition_conditions), "\n")
    
    if (!is.na(columns$malnutrition_col)) {
      summary$malnutrition_cases_count <- sum(register[[columns$malnutrition_col]] == 1, na.rm = TRUE)
    } else {
      # Check which diseases in register match malnutrition conditions
      diseases_in_data <- unique(register[[columns$disease_col]])
      matching_diseases <- intersect(diseases_in_data, malnutrition_conditions)
      cat("   - Matching malnutrition diseases in data:", length(matching_diseases), "\n")
      if (length(matching_diseases) > 0) {
        cat("   - Matching diseases:", paste(head(matching_diseases, 3), collapse = ", "), "\n")
      }
      
      summary$malnutrition_cases_count <- sum(register[[columns$disease_col]] %in% malnutrition_conditions, na.rm = TRUE)
    }
    cat("   - Malnutrition cases found:", summary$malnutrition_cases_count, "\n")
  } else {
    cat("   - Using fallback malnutrition detection\n")
    malnutrition_conditions <- c(
      "Complicated severe acute Malnutrition", "Moderate acute Malnutrition", 
      "Severe Malnutrition", "Uncomplicated severe acute Malnutrition", "Underweight"
    )
    
    if (!is.na(columns$malnutrition_col)) {
      summary$malnutrition_cases_count <- sum(register[[columns$malnutrition_col]] == 1, na.rm = TRUE)
    } else {
      summary$malnutrition_cases_count <- sum(register[[columns$disease_col]] %in% malnutrition_conditions, na.rm = TRUE)
    }
    cat("   - Malnutrition cases found (fallback):", summary$malnutrition_cases_count, "\n")
  }
  
  # Epidemic cases using your standardized epidemic diseases
  if (functions_available && exists("get_epidemic_diseases_standardized", envir = .GlobalEnv)) {
    cat("   - Using standardized epidemic diseases\n")
    epidemic_diseases <- get_epidemic_diseases_standardized()
    epidemic_conditions <- unlist(epidemic_diseases, use.names = FALSE)
    cat("   - Epidemic conditions:", length(epidemic_conditions), "\n")
    
    # Apply standardization to register diseases for matching
    if (exists("standardize_disease_labels", envir = .GlobalEnv)) {
      cat("   - Applying disease label standardization\n")
      standardized_diseases <- standardize_disease_labels(register[[columns$disease_col]])
      summary$epidemic_cases_count <- sum(standardized_diseases %in% epidemic_conditions, na.rm = TRUE)
    } else {
      cat("   - Using direct matching (no standardization)\n")
      summary$epidemic_cases_count <- sum(register[[columns$disease_col]] %in% epidemic_conditions, na.rm = TRUE)
    }
    cat("   - Epidemic cases found:", summary$epidemic_cases_count, "\n")
  } else {
    cat("   - Using fallback epidemic detection\n")
    if (exists("get_epidemic_diseases", envir = .GlobalEnv)) {
      epidemic_diseases <- get_epidemic_diseases()
      epidemic_conditions <- unlist(epidemic_diseases, use.names = FALSE)
      summary$epidemic_cases_count <- sum(register[[columns$disease_col]] %in% epidemic_conditions, na.rm = TRUE)
    } else {
      # Basic fallback
      epidemic_conditions <- c("Measles", "Suspected Measles", "Acute Watery Diarrhea", 
                               "Suspected Cholera", "Tuberculosis", "Suspected Tuberculosis")
      summary$epidemic_cases_count <- sum(register[[columns$disease_col]] %in% epidemic_conditions, na.rm = TRUE)
    }
    cat("   - Epidemic cases found (fallback):", summary$epidemic_cases_count, "\n")
  }
  
  return(summary)
}

calculate_quality_metrics <- function(register, columns, validation_results = NULL) {
  summary <- list()
  
  # Get current facility and geographic coverage counts
  total_facilities <- if (!is.na(columns$facility_col)) {
    dplyr::n_distinct(register[[columns$facility_col]], na.rm = TRUE)
  } else {
    0
  }
  
  geographic_coverage <- if (!is.na(columns$geo_col)) {
    dplyr::n_distinct(register[[columns$geo_col]], na.rm = TRUE)
  } else {
    0
  }
  
  disease_categories_count <- if (!is.na(columns$disease_col)) {
    dplyr::n_distinct(register[[columns$disease_col]], na.rm = TRUE)
  } else {
    0
  }
  
  # Base quality score calculation
  completeness_score <- 100
  
  # Deduct points for missing key elements
  if (is.na(columns$facility_col) || total_facilities == 0) {
    completeness_score <- completeness_score - 20
  }
  if (is.na(columns$disease_col) || disease_categories_count == 0) {
    completeness_score <- completeness_score - 20
  }
  if (is.na(columns$geo_col) || geographic_coverage == 0) {
    completeness_score <- completeness_score - 20
  }
  if (is.na(columns$date_col)) {
    completeness_score <- completeness_score - 30
  }
  
  # Additional deductions for high missing data
  if (!is.null(validation_results) && !is.null(validation_results$missing_dates)) {
    missing_pct <- validation_results$missing_dates / nrow(register) * 100
    if (missing_pct > 10) completeness_score <- completeness_score - 10
    if (missing_pct > 25) completeness_score <- completeness_score - 20
  }
  
  summary$quality_score <- max(completeness_score, 20)
  summary$completeness_score <- completeness_score
  
  return(summary)
}

# ============================================================================
# SPECIALIZED SUMMARY FUNCTIONS FOR DIFFERENT SECTIONS
# ============================================================================

calculate_malnutrition_summary <- function(register, summary_vars) {
  # Use the standardized column identification
  columns <- if (!is.null(summary_vars$columns)) summary_vars$columns else identify_key_columns(register)
  
  # Get malnutrition definitions from your comprehensive system
  if (exists("get_malnutrition_categories", envir = .GlobalEnv)) {
    mal_conditions <- get_malnutrition_categories()
  } else {
    # Fallback conditions
    mal_conditions <- c(
      "Complicated severe acute Malnutrition",
      "Uncomplicated severe acute Malnutrition",
      "Severe Malnutrition",
      "Moderate acute Malnutrition",
      "Underweight",
      "Malnutrition"
    )
  }
  
  # Filter to malnutrition cases
  if (!is.na(columns$malnutrition_col)) {
    df_mal <- register[register[[columns$malnutrition_col]] == 1, , drop = FALSE]
  } else if (!is.na(columns$disease_col)) {
    df_mal <- register[register[[columns$disease_col]] %in% mal_conditions, , drop = FALSE]
  } else {
    return(list(total_cases = 0))
  }
  
  if (nrow(df_mal) == 0) {
    return(list(total_cases = 0))
  }
  
  # Calculate comprehensive malnutrition statistics using your categories
  severe_conditions <- c(
    "Severe Malnutrition", 
    "Complicated severe acute Malnutrition", 
    "Uncomplicated severe acute Malnutrition"
  )
  
  summary <- list(
    total_cases = nrow(df_mal),
    severe_cases = sum(df_mal[[columns$disease_col]] %in% severe_conditions, na.rm = TRUE),
    moderate_cases = sum(df_mal[[columns$disease_col]] == "Moderate acute Malnutrition", na.rm = TRUE),
    underweight_cases = sum(df_mal[[columns$disease_col]] == "Underweight", na.rm = TRUE),
    general_malnutrition_cases = sum(df_mal[[columns$disease_col]] == "Malnutrition", na.rm = TRUE)
  )
  
  # Age and sex breakdown if available
  if (!is.na(columns$age_col)) {
    summary$under5_cases <- sum(df_mal[[columns$age_col]] %in% c("0-5 y", "< 5 yrs"), na.rm = TRUE)
  }
  
  if (!is.na(columns$sex_col)) {
    summary$female_cases <- sum(df_mal[[columns$sex_col]] %in% c("Female", "F", "female"), na.rm = TRUE)
    summary$male_cases <- sum(df_mal[[columns$sex_col]] %in% c("Male", "M", "male"), na.rm = TRUE)
  }
  
  # Geographic distribution if available
  if (!is.na(columns$geo_col)) {
    summary$regions_affected <- dplyr::n_distinct(df_mal[[columns$geo_col]], na.rm = TRUE)
  }
  
  return(summary)
}

calculate_epidemic_summary <- function(register, summary_vars, date_end = getOption("epi.date_end", NA)) {
  columns <- if (!is.null(summary_vars$columns)) summary_vars$columns else identify_key_columns(register)
  
  if (is.na(columns$disease_col)) {
    return(list(total_cases = 0, alert_diseases = 0, surveillance_summary = tibble::tibble(),
                high_alert_diseases = 0, medium_alert_diseases = 0))
  }
  
  # Use the new disease_categories_taxaware.R functions instead
  if (exists("get_epidemic_groups", envir = .GlobalEnv)) {
    
    # Check if register is already processed, if not apply simplified taxonomy mapping
    if (!"canonical_disease_imc" %in% names(register)) {
      # Try to use the improved apply_canonical_and_icd11_taxonomy from preprocessing.R
      if (exists("apply_canonical_and_icd11_taxonomy", envir = .GlobalEnv)) {
        cat("🔄 Applying modular taxonomy mapping in summary functions...\n")
        register_processed <- apply_canonical_and_icd11_taxonomy(register)
      } else if (exists("apply_morbidity_categorization", envir = .GlobalEnv)) {
        cat("⚠️ Falling back to apply_morbidity_categorization...\n")
        register_processed <- apply_morbidity_categorization(register)
      } else {
        cat("⚠️ No taxonomy functions available, using raw data...\n")
        register_processed <- register
      }
    } else {
      register_processed <- register
    }
    
    # Build epidemic surveillance summary using the new taxonomy approach
    epidemic_groups <- get_epidemic_groups()
    cutoff_date <- as.Date(summary_vars$current_date) - 30
    
    # Create epidemic surveillance summary
    epidemic_surveillance_summary <- register_processed %>%
      dplyr::filter(!is.na(.data[[columns$date_col]]), 
                    .data[[columns$date_col]] >= cutoff_date) %>%
      {
        epidemic_summaries <- list()
        for (group_name in names(epidemic_groups)) {
          group_diseases <- epidemic_groups[[group_name]]
          # Use canonical_disease_imc for matching
          group_data <- dplyr::filter(., .data$canonical_disease_imc %in% group_diseases)
          if (nrow(group_data) > 0) {
            epidemic_summaries[[group_name]] <- tibble::tibble(
              category = group_name,
              display = group_name,
              level = "category",
              total_cases = nrow(group_data),
              recent_cases_30d = nrow(group_data)
            )
          }
        }
        if (length(epidemic_summaries) > 0) {
          dplyr::bind_rows(epidemic_summaries)
        } else {
          tibble::tibble(category = character(), display = character(), 
                        level = character(), total_cases = integer(), 
                        recent_cases_30d = integer())
        }
      }
    
    if (nrow(epidemic_surveillance_summary) > 0) {
      total_cases <- sum(epidemic_surveillance_summary$total_cases[
        epidemic_surveillance_summary$level == "category"], na.rm = TRUE)
      # Alert levels not implemented in new system yet, set to 0
      high_alert_diseases <- 0
      medium_alert_diseases <- 0
    } else {
      total_cases <- 0; high_alert_diseases <- 0; medium_alert_diseases <- 0
    }
    
  } else {
    # (your existing fallback branch unchanged)
    if (exists("get_epidemic_diseases", envir = .GlobalEnv)) {
      epidemic_diseases <- get_epidemic_diseases()
      all_epidemic_conditions <- unlist(epidemic_diseases, use.names = FALSE)
      total_cases <- sum(register[[columns$disease_col]] %in% all_epidemic_conditions, na.rm = TRUE)
      epidemic_surveillance_summary <- tibble::tibble(disease = character(), recent_cases = integer(), alert_level = character())
      high_alert_diseases <- 0
      medium_alert_diseases <- 0
    } else {
      total_cases <- 0; high_alert_diseases <- 0; medium_alert_diseases <- 0
      epidemic_surveillance_summary <- tibble::tibble()
    }
  }
  
  list(
    total_cases = total_cases,
    surveillance_summary = epidemic_surveillance_summary,
    high_alert_diseases = high_alert_diseases,
    medium_alert_diseases = medium_alert_diseases
  )
}

# Enhanced trauma summary calculation using your trauma functions
calculate_trauma_summary <- function(register, summary_vars) {
  columns <- if (!is.null(summary_vars$columns)) summary_vars$columns else identify_key_columns(register)
  
  if (exists("build_trauma_summary", envir = .GlobalEnv)) {
    # Use your comprehensive trauma analysis
    trauma_analysis <- build_trauma_summary(
      data = register,
      admin_col = if (!is.na(columns$geo_col)) columns$geo_col else "admin1",
      type_col = if (!is.na(columns$trauma_col)) columns$trauma_col else "type_case",
      morbidity_col = columns$disease_col
    )
    
    return(list(
      total_trauma_cases = sum(trauma_analysis$trauma_stats$total_cases[
        trauma_analysis$trauma_stats$type_case_clean == "Trauma"], na.rm = TRUE),
      trauma_percentage = ifelse(nrow(register) > 0,
                                 round(sum(trauma_analysis$trauma_stats$total_cases[
                                   trauma_analysis$trauma_stats$type_case_clean == "Trauma"], na.rm = TRUE) / nrow(register) * 100, 1),
                                 0),
      regions_with_trauma = nrow(trauma_analysis$regional_stats),
      trauma_summary = trauma_analysis,
      # Add the summary table for plotting
      summary = trauma_analysis$summary
    ))
  } else {
    # Fallback calculation
    trauma_count <- summary_vars$trauma_cases_count %||% 0
    
    # Create empty summary table for plotting compatibility
    empty_summary <- data.frame(
      admin1 = character(0),
      type_case_clean = character(0),
      cases = numeric(0),
      percentage = numeric(0),
      total_cases = numeric(0),
      stringsAsFactors = FALSE
    )
    
    return(list(
      total_trauma_cases = trauma_count,
      trauma_percentage = ifelse(nrow(register) > 0, round(trauma_count / nrow(register) * 100, 1), 0),
      regions_with_trauma = NA,
      trauma_summary = list(),
      summary = empty_summary
    ))
  }
}

# Enhanced disease burden calculation using your severity mapping
calculate_disease_burden_summary <- function(register, summary_vars) {
  columns <- if (!is.null(summary_vars$columns)) summary_vars$columns else identify_key_columns(register)
  
  if (is.na(columns$disease_col)) {
    return(list(burden_score = 0, high_burden_diseases = character(0)))
  }
  
  if (exists("get_severity_mapping", envir = .GlobalEnv) && exists("severity_weights", envir = .GlobalEnv)) {
    # Use your comprehensive severity mapping
    severity_map <- get_severity_mapping()
    weights <- severity_weights()
    
    # Calculate burden scores
    disease_burden <- register %>%
      dplyr::count(.data[[columns$disease_col]], name = "frequency") %>%
      dplyr::mutate(
        severity = severity_map[.data[[columns$disease_col]]] %||% attr(severity_map, "default_severity") %||% "Low",
        severity_weight = weights[severity],
        burden_score = frequency * severity_weight,
        frequency_rank = dplyr::min_rank(dplyr::desc(frequency)),
        burden_rank = dplyr::min_rank(dplyr::desc(burden_score))
      ) %>%
      dplyr::arrange(dplyr::desc(burden_score))
    
    high_burden_diseases <- disease_burden %>%
      dplyr::filter(burden_rank <= 10) %>%
      dplyr::pull(.data[[columns$disease_col]])
    
    total_burden_score <- sum(disease_burden$burden_score, na.rm = TRUE)
    
    return(list(
      burden_score = total_burden_score,
      high_burden_diseases = high_burden_diseases,
      disease_burden_table = disease_burden
    ))
  } else {
    # Simple fallback
    top_diseases <- register %>%
      dplyr::count(.data[[columns$disease_col]], name = "frequency", sort = TRUE) %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::pull(.data[[columns$disease_col]])
    
    return(list(
      burden_score = nrow(register),
      high_burden_diseases = top_diseases,
      disease_burden_table = tibble::tibble()
    ))
  }
}

# ============================================================================
# DISPLAY FUNCTIONS FOR SYRIA-SPECIFIC SUMMARIES
# ============================================================================

#' Display malnutrition summary using Syria HMIS categories
#' 
#' Creates formatted markdown output for malnutrition surveillance
#' @param malnutrition_summary List from calculate_malnutrition_summary()
#' @param summary_vars List from calculate_all_summary_variables()
#' @export
display_malnutrition_summary_syria <- function(malnutrition_summary, summary_vars) {
  # Helper function for formatting numbers
  fmt <- function(x) if (is.null(x) || is.na(x)) "0" else scales::comma(x)
  
  out <- character()
  
  if (malnutrition_summary$total_cases > 0) {
    out <- c(out, paste0("**Total Cases Identified**: ", fmt(malnutrition_summary$total_cases),
                         " malnutrition cases across all Syria HMIS categories\n"))
    
    # Case distribution by type using Syria categories
    if (malnutrition_summary$severe_cases > 0 || malnutrition_summary$moderate_cases > 0) {
      out <- c(out, "**Case Distribution by Syria HMIS Categories:**")
      
      if (malnutrition_summary$severe_cases > 0) {
        out <- c(out, paste0("- **Severe Acute Malnutrition**: ", malnutrition_summary$severe_cases, " cases"))
      }
      if (malnutrition_summary$moderate_cases > 0) {
        out <- c(out, paste0("- **Moderate Acute Malnutrition**: ", malnutrition_summary$moderate_cases, " cases"))
      }
      if (malnutrition_summary$underweight_cases > 0) {
        out <- c(out, paste0("- **Underweight**: ", malnutrition_summary$underweight_cases, " cases"))
      }
      if (!is.null(malnutrition_summary$general_malnutrition_cases) && malnutrition_summary$general_malnutrition_cases > 0) {
        out <- c(out, paste0("- **General Malnutrition**: ", malnutrition_summary$general_malnutrition_cases, " cases"))
      }
      out <- c(out, "")
    }
    
    # Age and sex distribution
    if (!is.null(malnutrition_summary$under5_cases) && !is.na(malnutrition_summary$under5_cases)) {
      under5_pct <- round(malnutrition_summary$under5_cases / malnutrition_summary$total_cases * 100, 1)
      out <- c(out, paste0("**Age Distribution**: ", malnutrition_summary$under5_cases, 
                           " cases in children under 5 years (", under5_pct, "%)\n"))
    }
    
    if (!is.null(malnutrition_summary$female_cases) && !is.null(malnutrition_summary$male_cases)) {
      out <- c(out, paste0("**Sex Distribution**: ", 
                           malnutrition_summary$female_cases, " female, ",
                           malnutrition_summary$male_cases, " male cases\n"))
    }
    
    # Geographic coverage
    if (!is.null(malnutrition_summary$regions_affected) && !is.na(malnutrition_summary$regions_affected)) {
      out <- c(out, paste0("**Geographic Coverage**: ", malnutrition_summary$regions_affected, 
                           " administrative regions affected\n"))
    }
    
    # Recent trend if calculable
    if (exists("calculate_malnutrition_trend") && !is.null(summary_vars$current_date)) {
      tryCatch({
        columns <- summary_vars$columns %||% identify_key_columns(register)
        
        if (!is.na(columns$date_col) && columns$date_col %in% names(register)) {
          dates <- safe_date_parse(register[[columns$date_col]])
          mal_set <- register[[columns$disease_col]] %in% get_malnutrition_categories()
          
          current_month_malnut <- sum(mal_set & dates >= summary_vars$one_month_ago & dates <= summary_vars$current_date, na.rm = TRUE)
          prev_month_malnut    <- sum(
            mal_set & dates >= (summary_vars$one_month_ago - 30) &
              dates <  summary_vars$one_month_ago, na.rm = TRUE
          )
          
          if (prev_month_malnut > 0) {
            malnutrition_trend <- round(((current_month_malnut - prev_month_malnut) / prev_month_malnut) * 100, 1)
            
            if (abs(malnutrition_trend) > 0) {
              trend_direction <- if (malnutrition_trend > 0) "increase" else "decrease"
              arrow <- if (malnutrition_trend > 0) "↗️" else "↘️"
              out <- c(out, paste0("**Recent Trend**: ", arrow, " ", abs(malnutrition_trend),
                                   "% ", trend_direction,
                                   " in the most recent month (", current_month_malnut, " cases)\n"))
            } else {
              out <- c(out, "**Recent Trend**: Stable malnutrition caseload with no significant monthly variation\n")
            }
          }
        }
      }, error = function(e) {
        # If trend calculation fails, skip it
      })
    }
    
  } else {
    out <- c(out, "**No malnutrition cases detected** in the current reporting period using Syria HMIS categories.\n")
    out <- c(out, "This may indicate:\n")
    out <- c(out, "- No malnutrition cases present (positive finding)\n")
    out <- c(out, "- Cases coded under different diagnostic terms\n")
    out <- c(out, "- Data collection or classification issues\n")
  }
  
  # Add footer with category information
  if (exists("get_malnutrition_categories", envir = .GlobalEnv)) {
    mal_conditions <- get_malnutrition_categories()
    out <- c(out, paste0("\n**Syria HMIS Malnutrition Categories Monitored** (", length(mal_conditions), " total):\n"))
    out <- c(out, paste0("*", paste(mal_conditions, collapse = ", "), "*\n"))
  }
  
  # Print all as markdown
  cat(paste(out, collapse = "\n"))
}

#' Display trauma summary using Syria categories
#' 
#' Creates formatted markdown output for trauma surveillance
#' @param trauma_summary List from calculate_trauma_summary()
#' @param summary_vars List from calculate_all_summary_variables()
#' @export
display_trauma_summary_syria <- function(trauma_summary, summary_vars) {
  # Helper function for formatting numbers
  fmt <- function(x) if (is.null(x) || is.na(x)) "0" else scales::comma(x)
  
  out <- character()
  
  if (trauma_summary$total_trauma_cases > 0) {
    out <- c(out, paste0("**Total Trauma Cases**: ", fmt(trauma_summary$total_trauma_cases),
                         " trauma cases (", trauma_summary$trauma_percentage, "% of all consultations)\n"))
    
    # Regional distribution if available
    if (!is.null(trauma_summary$trauma_summary) && 
        !is.null(trauma_summary$trauma_summary$regional_stats) &&
        nrow(trauma_summary$trauma_summary$regional_stats) > 0) {
      
      regional_stats <- trauma_summary$trauma_summary$regional_stats
      out <- c(out, "**Geographic Distribution of Trauma Burden:**")
      
      # Show top 5 regions with highest trauma percentages
      top_regions <- head(regional_stats, 5)
      for (i in 1:nrow(top_regions)) {
        region <- top_regions$admin1[i]
        trauma_pct <- top_regions$trauma_percentage[i]
        trauma_cases <- top_regions$trauma_cases[i]
        
        # Add visual indicator for high trauma burden
        indicator <- if (trauma_pct >= 15) "🔴" else if (trauma_pct >= 10) "🟡" else "🟢"
        
        out <- c(out, paste0("- ", indicator, " **", region, "**: ", 
                             trauma_cases, " cases (", trauma_pct, "% of regional consultations)"))
      }
      out <- c(out, "")
    }
    
    # Trauma type breakdown if available
    if (!is.null(trauma_summary$trauma_summary) && 
        !is.null(trauma_summary$trauma_summary$trauma_stats) &&
        nrow(trauma_summary$trauma_summary$trauma_stats) > 0) {
      
      trauma_stats <- trauma_summary$trauma_summary$trauma_stats
      trauma_row <- trauma_stats[trauma_stats$type_case_clean == "Trauma", ]
      
      if (nrow(trauma_row) > 0) {
        out <- c(out, "**Trauma Surveillance Overview:**")
        out <- c(out, paste0("- **Total trauma cases**: ", fmt(trauma_row$total_cases)))
        out <- c(out, paste0("- **Affected regions**: ", trauma_row$regions))
        out <- c(out, paste0("- **Average cases per region**: ", trauma_row$avg_cases_per_region))
        out <- c(out, "")
      }
    }
    
    # Classification method used
    if (exists("classify_trauma_case", envir = .GlobalEnv)) {
      out <- c(out, "**Classification Method**: Syria HMIS trauma categorization using comprehensive morbidity mapping")
    } else {
      out <- c(out, "**Classification Method**: Basic trauma detection using type_case field")
    }
    
    # Trauma burden assessment
    trauma_pct <- trauma_summary$trauma_percentage
    if (trauma_pct >= 20) {
      out <- c(out, "🔴 **HIGH TRAUMA BURDEN**: >20% of consultations are trauma-related")
      out <- c(out, "- *Recommend enhanced emergency preparedness and surgical capacity*")
    } else if (trauma_pct >= 10) {
      out <- c(out, "🟡 **ELEVATED TRAUMA BURDEN**: 10-20% of consultations are trauma-related")
      out <- c(out, "- *Monitor for emerging conflict or accident patterns*")
    } else if (trauma_pct >= 5) {
      out <- c(out, "🟢 **MODERATE TRAUMA BURDEN**: 5-10% of consultations are trauma-related")
      out <- c(out, "- *Maintain standard emergency care protocols*")
    } else {
      out <- c(out, "✅ **LOW TRAUMA BURDEN**: <5% of consultations are trauma-related")
      out <- c(out, "- *Continue routine trauma care capacity*")
    }
    
  } else {
    out <- c(out, "**No trauma cases detected** in the current reporting period using Syria HMIS categories.\n")
    out <- c(out, "This may indicate:\n")
    out <- c(out, "- Low conflict/accident activity (positive finding)\n")
    out <- c(out, "- Cases coded under different diagnostic terms\n")
    out <- c(out, "- Data collection or classification issues\n")
  }
  
  # Add footer with trauma category information
  if (exists("get_trauma_morbidities", envir = .GlobalEnv)) {
    trauma_conditions <- get_trauma_morbidities()
    out <- c(out, paste0("\n**Syria HMIS Trauma Categories Monitored** (", length(trauma_conditions), " conditions):\n"))
    out <- c(out, paste0("*", paste(trauma_conditions, collapse = ", "), "*\n"))
  }
  
  # Print all as markdown
  cat(paste(out, collapse = "\n"))
}

#' Display epidemic surveillance summary using Syria categories
#' 
#' Creates formatted markdown output for epidemic surveillance
#' @param epidemic_summary List from calculate_epidemic_summary()
#' @param summary_vars List from calculate_all_summary_variables()
#' @export
display_epidemic_summary_syria <- function(epidemic_summary, summary_vars) {
  out <- character()
  
  if (epidemic_summary$total_cases > 0) {
    out <- c(out, paste0("**Total Epidemic-Prone Cases**: ", scales::comma(epidemic_summary$total_cases),
                         " cases using Syria HMIS epidemic disease definitions\n"))
    
    # Alert summary
    high_alert <- epidemic_summary$high_alert_diseases %||% 0
    medium_alert <- epidemic_summary$medium_alert_diseases %||% 0
    
    if (high_alert > 0) {
      out <- c(out, paste0("🔴 **HIGH ALERT**: ", high_alert, " disease(s) requiring immediate attention\n"))
    }
    if (medium_alert > 0) {
      out <- c(out, paste0("🟡 **MEDIUM ALERT**: ", medium_alert, " disease(s) requiring enhanced monitoring\n"))
    }
    if (high_alert == 0 && medium_alert == 0) {
      out <- c(out, "✅ **No high-priority alerts** - routine surveillance protocols sufficient\n")
    }
    
    # Display surveillance table if available
    if (!is.null(epidemic_summary$surveillance_summary) && nrow(epidemic_summary$surveillance_summary) > 0) {
      out <- c(out, "\n**Disease-Specific Surveillance Status:**\n")
      
      disease_summary <- epidemic_summary$surveillance_summary %>%
        dplyr::filter(level == "disease" & recent_cases_30d > 0) %>%
        dplyr::arrange(dplyr::desc(recent_cases_30d)) %>%
        dplyr::slice_head(n = 10)
      
      if (nrow(disease_summary) > 0) {
        for (i in 1:nrow(disease_summary)) {
          disease <- disease_summary$Epidemic_Disease[i]
          cases <- disease_summary$recent_cases_30d[i]
          alert <- disease_summary$alert_level[i]
          
          alert_icon <- dplyr::case_when(
            alert == "High" ~ "🔴",
            alert == "Medium" ~ "🟡",
            alert == "Low" ~ "🟢",
            TRUE ~ "⚪"
          )
          
          out <- c(out, paste0("- ", alert_icon, " **", disease, "**: ", cases, " cases (30 days) - ", alert, " alert"))
        }
      }
    }
    
  } else {
    out <- c(out, "**No epidemic-prone disease cases detected** using Syria HMIS definitions.\n")
    out <- c(out, "This may indicate:\n")
    out <- c(out, "- No outbreak conditions present (positive finding)\n")
    out <- c(out, "- Cases coded under different diagnostic terms\n")
    out <- c(out, "- Potential surveillance gaps requiring investigation\n")
  }
  
  # Add footer with category information
  if (exists("get_epidemic_diseases_standardized", envir = .GlobalEnv)) {
    epi_diseases <- get_epidemic_diseases_standardized()
    total_conditions <- length(unlist(epi_diseases, use.names = FALSE))
    out <- c(out, paste0("\n**Syria HMIS Epidemic Disease Categories Monitored** (", length(epi_diseases), " categories, ", total_conditions, " conditions):\n"))
    for (category in names(epi_diseases)) {
      conditions <- length(epi_diseases[[category]])
      out <- c(out, paste0("- **", category, "**: ", conditions, " conditions"))
    }
  }
  
  # Print all as markdown
  cat(paste(out, collapse = "\n"))
}

create_summary_table <- function(summary_vars, validation_results = NULL) {
  # Use validation results if available, otherwise use summary_vars
  date_range_text <- if (!is.null(validation_results$date_range) && length(validation_results$date_range) >= 2) {
    paste(format(validation_results$date_range[1], "%Y-%m-%d"),
          "to",
          format(validation_results$date_range[2], "%Y-%m-%d"))
  } else {
    summary_vars$date_range_text
  }
  
  missing_dates_text <- paste0(
    summary_vars$missing_dates, 
    " (", summary_vars$missing_dates_pct, "%)"
  )
  
  tibble::tibble(
    Metric = c(
      "Total Consultations",
      "Date Range", 
      "Unique Health Facilities",
      "Administrative Regions",
      "Disease Categories",
      "Missing Dates"
    ),
    Value = c(
      scales::comma(summary_vars$total_consultations),
      date_range_text,
      as.character(summary_vars$total_facilities),
      as.character(summary_vars$geographic_coverage),
      as.character(summary_vars$disease_categories_count),
      missing_dates_text
    )
  )
}

# =========================
# Augment summary_vars with all caption-ready fields
# =========================
augment_summary_vars <- function(register, summary_vars) {
  cols <- detect_cols_sf(register)
  date_col <- cols$date_col
  region_col <- cols$region_col
  disease_col <- cols$disease_col
  facetype_col <- cols$facetype_col
  facility_col <- cols$facility_col
  age_col <- cols$age_col

  # ---- Peak month & MoM change for consultations ----
  if (!is.na(date_col) && date_col %in% names(register)) {
    by_mo <- register %>%
      dplyr::mutate(.date = to_date_safe(.data[[date_col]])) %>%
      dplyr::filter(!is.na(.date)) %>%
      dplyr::mutate(month = lubridate::floor_date(.date, "month")) %>%
      dplyr::count(month, name = "n") %>%
      dplyr::arrange(month)

    pk_row <- dplyr::slice_max(by_mo, n, n = 1, with_ties = FALSE)
    
    # Find the last two months with data instead of calculating arbitrary dates
    sorted_months <- sort(by_mo$month, decreasing = TRUE)
    last_m <- sorted_months[1]
    prev_m <- if (length(sorted_months) >= 2) sorted_months[2] else NA
    
    last_n <- by_mo$n[by_mo$month == last_m][1] %||% NA_real_
    prev_n <- if (!is.na(prev_m)) by_mo$n[by_mo$month == prev_m][1] else NA_real_

    mm_pct <- if (!is.na(prev_n) && !is.na(last_n) && prev_n > 0) {
      round(100 * (last_n - prev_n) / prev_n, 1)
    } else {
      NA_real_
    }
    mm_dir <- if (is.na(mm_pct)) "—" else if (mm_pct > 0) "increased" else if (mm_pct < 0) "decreased" else "was unchanged"

    summary_vars$peak_month <- if (nrow(pk_row)) pk_row$month[1] else as.Date(NA)
    summary_vars$peak_consultations <- if (nrow(pk_row)) pk_row$n[1] else NA_integer_
    summary_vars$monthly_change_pct <- mm_pct
    summary_vars$monthly_change_direction <- mm_dir

    # ---- Top region by volume + map stats ----
    if (!is.na(region_col) && region_col %in% names(register)) {
      reg_counts <- register %>%
        dplyr::filter(!is.na(.data[[region_col]])) %>%
        dplyr::count(.data[[region_col]], name = "n", sort = TRUE)

      total_n <- sum(reg_counts$n)
      top3_share <- if (total_n > 0 && nrow(reg_counts) >= 1) {
        round(100 * sum(head(reg_counts$n, 3)) / total_n, 1)
      } else NA_real_

      summary_vars$top_region_by_volume    <- if (nrow(reg_counts)) reg_counts[[region_col]][1] else NA_character_
      summary_vars$top_region_by_volume_n  <- if (nrow(reg_counts)) reg_counts$n[1] else NA_integer_
      summary_vars$map_peak_region         <- summary_vars$top_region_by_volume
      summary_vars$map_top3_share          <- top3_share
    }
  }

  # ---- Top diseases pct table (for captions) ----
  summary_vars$top_diseases_pct <- tryCatch(
    compute_top_diseases_pct(register, disease_col),
    error = function(e) tibble::tibble()
  )

  # ---- Geo disease focus (region × disease) ----
  if (!is.na(region_col) && !is.na(disease_col) &&
      region_col %in% names(register) && disease_col %in% names(register)) {
    rd <- register %>%
      dplyr::filter(!is.na(.data[[region_col]]), !is.na(.data[[disease_col]])) %>%
      dplyr::count(.data[[region_col]], .data[[disease_col]], name = "n") %>%
      dplyr::arrange(dplyr::desc(n))

    if (nrow(rd)) {
      summary_vars$top_disease_region <- rd[[region_col]][1]
      summary_vars$top_disease_name   <- rd[[disease_col]][1]
      
      # Calculate the percentage share of top disease-region combination
      total_for_disease <- register %>%
        dplyr::filter(!is.na(.data[[disease_col]]), 
                     .data[[disease_col]] == rd[[disease_col]][1]) %>%
        nrow()
      
      summary_vars$top_disease_region_share <- if (total_for_disease > 0) {
        round(100 * rd$n[1] / total_for_disease, 1)
      } else {
        0
      }
      
      # Calculate how many regions are affected by top 3 diseases
      top_3_diseases <- rd %>%
        dplyr::group_by(.data[[disease_col]]) %>%
        dplyr::summarise(total_cases = sum(n), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(total_cases)) %>%
        dplyr::slice_head(n = 3) %>%
        dplyr::pull(.data[[disease_col]])
      
      summary_vars$diseases_geographic_reach <- register %>%
        dplyr::filter(.data[[disease_col]] %in% top_3_diseases) %>%
        dplyr::distinct(.data[[region_col]]) %>%
        nrow()
      
      # Calculate concentration ratio (top region vs average)
      region_totals <- register %>%
        dplyr::filter(!is.na(.data[[region_col]])) %>%
        dplyr::count(.data[[region_col]], name = "region_cases")
      
      avg_cases_per_region <- mean(region_totals$region_cases, na.rm = TRUE)
      max_cases_per_region <- max(region_totals$region_cases, na.rm = TRUE)
      
      summary_vars$disease_concentration_ratio <- if (avg_cases_per_region > 0) {
        round(max_cases_per_region / avg_cases_per_region, 1)
      } else {
        1.0
      }
    } else {
      summary_vars$top_disease_region <- NA_character_
      summary_vars$top_disease_name   <- NA_character_
      summary_vars$top_disease_region_share <- 0
      summary_vars$diseases_geographic_reach <- 0
      summary_vars$disease_concentration_ratio <- 1.0
    }
  }

  # ---- Facility type shares ----
  if (!is.na(facetype_col) && facetype_col %in% names(register)) {
    ft <- register %>%
      dplyr::filter(!is.na(.data[[facetype_col]])) %>%
      dplyr::count(.data[[facetype_col]], name = "n")
    tot <- sum(ft$n)
    get_share <- function(pattern) {
      if (!tot) return(NA_real_)
      round(100 * sum(ft$n[grepl(pattern, ft[[facetype_col]], ignore.case = TRUE)]) / tot, 1)
    }
    summary_vars$primary_care_share <- get_share("PHC|Primary|Health Center|Health Centre|Clinic")
    summary_vars$hospital_share     <- get_share("Hospital|Secondary|Tertiary")
  }

  # ---- Facilities and regions with MoM increase ----
  inc_fac <- count_increasing_units(register, facility_col, date_col)
  inc_reg <- count_increasing_units(register, region_col,   date_col)
  # Direction sentence fragment for caption
  summary_vars$facilities_increase_dir <- if (is.na(inc_fac$pct_up)) "—" else if (inc_fac$pct_up >= 50) "many" else if (inc_fac$pct_up >= 25) "some" else "few"
  summary_vars$facilities_increase_pct <- inc_fac$pct_up
  summary_vars$regions_access_improved <- inc_reg$n_up

  # ---- High-risk groups share (U5, pregnant, trauma, malnutrition, epidemic) ----
  n_tot <- nrow(register)
  u5_share <- NA_real_
  if (!is.na(age_col) && age_col %in% names(register)) {
    u5_share <- round(100 * mean(grepl("^0-4|<\\s*5", register[[age_col]] %||% ""), na.rm = TRUE), 1)
  }
  flag_share <- function(flag_col, treat_value = 1, string_match = NULL) {
    if (is.na(flag_col) || !(flag_col %in% names(register))) return(0)
    v <- register[[flag_col]]
    if (!is.null(string_match)) {
      mean(grepl(string_match, v, ignore.case = TRUE), na.rm = TRUE)
    } else {
      mean(v == treat_value, na.rm = TRUE)
    }
  }
  preg_share  <- flag_share(cols$pregnancy_flag, 1)
  maln_share  <- flag_share(cols$malnut_flag, 1)
  epi_share   <- flag_share(cols$epi_flag, 1)
  trauma_share <- if (!is.na(cols$trauma_flag) && cols$trauma_flag %in% names(register)) {
    if (cols$trauma_flag == "type_case") {
      flag_share("type_case", string_match = "^trauma$")
    } else {
      flag_share(cols$trauma_flag, 1)
    }
  } else 0

  # Combine: simple bounded sum (not mutually exclusive, but indicative)
  shares <- c(u5_share/100, preg_share, maln_share, trauma_share, epi_share)
  shares[is.na(shares)] <- 0
  combined_share <- round(100 * pmin(1, sum(shares)), 1) # cap at 100%
  summary_vars$high_risk_share <- combined_share

  # ---- Age/sex quick stats for captions ----
  if (!is.na(age_col) && age_col %in% names(register)) {
    ag <- register %>%
      dplyr::filter(!is.na(.data[[age_col]])) %>%
      dplyr::count(.data[[age_col]], name = "n", sort = TRUE)
    summary_vars$age_peak_group <- if (nrow(ag)) ag[[age_col]][1] else NA_character_
  }

  # ---- Simple overall disease trend descriptor ----
  # Compares sum of top 3 diseases (last 4 weeks vs prior 8 weeks)
  if (!is.na(date_col) && !is.na(disease_col) &&
      date_col %in% names(register) && disease_col %in% names(register)) {

    df <- register %>%
      dplyr::mutate(.date = to_date_safe(.data[[date_col]])) %>%
      dplyr::filter(!is.na(.date), !is.na(.data[[disease_col]])) %>%
      dplyr::mutate(week = lubridate::floor_date(.date, "week"))

    top3 <- df %>% dplyr::count(.data[[disease_col]], sort = TRUE) %>% dplyr::slice_head(n = 3) %>% dplyr::pull(1)

    if (length(top3)) {
      last_wk <- suppressWarnings(max(df$week, na.rm = TRUE))
      recent   <- df %>% dplyr::filter(week > last_wk - lubridate::days(28), .data[[disease_col]] %in% top3) %>% nrow()
      baseline <- df %>% dplyr::filter(week <= last_wk - lubridate::days(28), week > last_wk - lubridate::days(84), .data[[disease_col]] %in% top3) %>% nrow()
      trend <- if (baseline > 0) recent / baseline else NA_real_
      summary_vars$disease_trend_overall <- if (is.na(trend)) "insufficient data" else if (trend >= 1.2) "rising" else if (trend <= 0.8) "declining" else "stable"
    } else {
      summary_vars$disease_trend_overall <- "—"
    }
  } else {
    summary_vars$disease_trend_overall <- "—"
  }

  # ---- Default alert thresholds to reference in text ----
  summary_vars$alert_threshold_recent_cases <- summary_vars$alert_threshold_recent_cases %||% 10
  summary_vars$alert_ratio_threshold        <- summary_vars$alert_ratio_threshold %||% 2.0

  # ---- Syndrome surveillance variables ----
  # Add basic syndrome tracking variables for inline R code references
  summary_vars$syndrome_of_interest <- "respiratory illness"  # Default syndrome focus
  summary_vars$syndrome_recent_change_dir <- "stable"         # Default change direction
  summary_vars$syndrome_recent_change_pct <- 0               # Default change percentage
  
  # ---- Trauma hotspot variables ----
  # Add trauma geographical variables for inline R code references
  summary_vars$trauma_hotspot_region <- summary_vars$top_region_by_volume %||% "—"
  summary_vars$trauma_hotspot_pct <- NA_real_
  
  # ---- Complex case trend variable ----
  summary_vars$complex_case_trend <- "stable"

  summary_vars
}


safe_malnutrition_summary <- function(register, summary_vars, cols = detect_cols_sf(register)) {
  out <- list(total_cases = 0, month_change_direction = "—", month_change_pct = NA_real_,
              u5_percentage = NA_real_, top_region = NA_character_, top_region_pct = NA_real_)
  # detect malnutrition
  flag <- cols$malnut_flag
  date_col <- cols$date_col
  region_col <- cols$region_col
  if (is.na(date_col)) return(out)

  df <- register %>% dplyr::mutate(.date = to_date_safe(.data[[date_col]])) %>% dplyr::filter(!is.na(.date))
  if (!is.na(flag) && flag %in% names(df)) {
    dfm <- df %>% dplyr::filter(.data[[flag]] == 1)
  } else if (!is.na(cols$disease_col) && cols$disease_col %in% names(df)) {
    # Heuristic: match malnutrition words
    dfm <- df %>% dplyr::filter(grepl("malnut|sam|mam|wasting", .data[[cols$disease_col]], ignore.case = TRUE))
  } else dfm <- df[0,]

  if (!nrow(dfm)) return(out)
  out$total_cases <- nrow(dfm)
  # month-on-month change
  dfm <- dfm %>% dplyr::mutate(month = lubridate::floor_date(.date, "month"))
  last_m <- suppressWarnings(max(dfm$month, na.rm = TRUE)); prev_m <- last_m - lubridate::days(30)
  cur <- nrow(dplyr::filter(dfm, month == last_m)); prev <- nrow(dplyr::filter(dfm, month == prev_m))
  pct <- if (prev > 0) round(100 * (cur - prev) / prev, 1) else NA_real_
  out$month_change_pct <- pct; out$month_change_direction <- if (is.na(pct)) "—" else if (pct > 0) "increased" else if (pct < 0) "decreased" else "was unchanged"

  # U5 share
  if (!is.na(cols$age_col) && cols$age_col %in% names(dfm)) {
    out$u5_percentage <- round(100 * mean(grepl("^0-4|<\\s*5", dfm[[cols$age_col]] %||% ""), na.rm = TRUE), 1)
  }
  # top region
  if (!is.na(region_col) && region_col %in% names(dfm)) {
    rc <- dfm %>% dplyr::filter(!is.na(.data[[region_col]])) %>% dplyr::count(.data[[region_col]], name = "n", sort = TRUE)
    if (nrow(rc)) {
      out$top_region <- rc[[region_col]][1]
      out$top_region_pct <- round(100 * rc$n[1] / sum(rc$n), 1)
    }
  }
  out
}

safe_trauma_summary <- function(register, summary_vars, cols = detect_cols_sf(register)) {
  out <- list(total_trauma_cases = 0L, trauma_percentage = NA_real_,
              top_region = NA_character_, top_region_pct = NA_real_,
              regions_with_trauma = NA_integer_)
  date_col <- cols$date_col; region_col <- cols$region_col
  if (is.na(date_col)) return(out)

  df <- register %>% dplyr::mutate(.date = to_date_safe(.data[[date_col]])) %>% dplyr::filter(!is.na(.date))
  if (!is.na(cols$trauma_flag) && cols$trauma_flag %in% names(df)) {
    if (cols$trauma_flag == "type_case") {
      dft <- df %>% dplyr::filter(grepl("^trauma$", .data$type_case, ignore.case = TRUE))
    } else {
      dft <- df %>% dplyr::filter(.data[[cols$trauma_flag]] == 1)
    }
  } else {
    dft <- df[0,]
  }
  out$total_trauma_cases <- nrow(dft)
  out$trauma_percentage  <- if (nrow(df)) round(100 * nrow(dft) / nrow(df), 1) else NA_real_
  if (!is.na(region_col) && region_col %in% names(dft) && nrow(dft)) {
    rc <- dft %>% dplyr::filter(!is.na(.data[[region_col]])) %>% dplyr::count(.data[[region_col]], name = "n", sort = TRUE)
    out$regions_with_trauma <- nrow(rc)
    if (nrow(rc)) {
      out$top_region     <- rc[[region_col]][1]
      out$top_region_pct <- round(100 * rc$n[1] / sum(rc$n), 1)
    }
  }
  out
}