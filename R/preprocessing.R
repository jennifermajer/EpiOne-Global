# R/preprocessing.R
# FIXES APPLIED:
# 1. Fixed missing safe_load_config function definition order
# 2. Fixed CONFIG_AVAILABLE initialization
# 3. Enhanced error handling in prepare_register
# 4. Fixed taxonomy loading with better fallbacks
# 5. Improved date processing error handling
# 6. Fixed metadata join error handling

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(readr)
  library(logger)
  library(here)
  # NOTE: config library loaded conditionally below
})

options(stringsAsFactors = FALSE) 

# ---- Define %||% and helper functions first ----
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# repeat string helper used in print
if (!exists("%r%")) {
  `%r%` <- function(s, n) paste(rep(s, n), collapse = "")
}

# simple absolute-path check (avoid fs dependency)
is_abs_path <- function(p) grepl("^([A-Za-z]:)?[\\/]", p)

# ---- CRITICAL FIX: Define safe_load_config BEFORE using it ----
safe_load_config <- function() {
  # PERMANENT FIX: Always return FALSE to prevent config conflicts
  tryCatch({
    # Check if config is already loaded and detach it
    if ("package:config" %in% search()) {
      detach("package:config", unload = TRUE, force = TRUE)
      log_info("🔧 Detached config package to prevent conflicts")
    }
    return(FALSE)  # Always return FALSE to disable config loading
  }, error = function(e) {
    return(FALSE)
  })
}

# ---- NOW initialize CONFIG_AVAILABLE after safe_load_config is defined ----
CONFIG_AVAILABLE <- safe_load_config()

# ---- IMMEDIATE FIX: Run this now to fix your current session ----
immediate_preprocessing_fix <- function() {
  cat("🔧 Applying immediate preprocessing fix...\n")
  
  # 1. Forcibly detach config package
  if ("package:config" %in% search()) {
    detach("package:config", unload = TRUE, force = TRUE)
    cat("✅ Detached config package\n")
  }
  
  # 2. Set CONFIG_AVAILABLE to FALSE to prevent future config loading
  CONFIG_AVAILABLE <<- FALSE
  
  # 3. Override library function temporarily to block config loading
  original_library <- base::library
  library <<- function(package, ...) {
    pkg_name <- as.character(substitute(package))
    if (pkg_name == "config") {
      cat("🚫 Blocked config package loading\n")
      return(invisible(NULL))
    }
    original_library(package, ...)
  }
  
  cat("✅ Immediate preprocessing fix applied\n")
  cat("📄 Now run your data processing - config conflicts should be resolved\n")
}

# Robust date parser used across Rmd + modules (only if not provided elsewhere)
if (!exists("to_date_safe", mode = "function")) {
  to_date_safe <- function(x) {
    if (inherits(x, "Date"))   return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))
    x_chr <- as.character(x)
    d1 <- suppressWarnings(lubridate::ymd(x_chr, quiet = TRUE))
    d2 <- suppressWarnings(lubridate::dmy(x_chr, quiet = TRUE))
    d3 <- suppressWarnings(lubridate::mdy(x_chr, quiet = TRUE))
    as.Date(dplyr::coalesce(d1, d2, d3), origin = "1970-01-01")
  }
}

# ---- SOURCE ORDER (critical!) ----

# add utils if you have them (e.g., utils_dates.R for to_date_safe)
if (file.exists(here::here("R", "utils_dates.R"))) {
  source(here::here("R", "utils_dates.R"))
}

# --- Enhanced safe sourcing of optional modules with BETTER error handling ---

source_disease_categories <- function() {
  fp <- here("R", "disease_categories_taxaware.R")
  if (file.exists(fp)) {
    tryCatch({
      # CRITICAL: Prevent config loading entirely during taxonomy processing
      
      # Step 1: Record current config state
      config_was_attached <- "package:config" %in% search()
      
      # Step 2: Forcibly detach config if it's attached
      if (config_was_attached) {
        detach("package:config", unload = TRUE, force = TRUE)
        log_info("🔧 Temporarily detached config package for taxonomy loading")
      }
      
      # Step 3: Override config functions to prevent accidental loading
      original_library <- base::library
      assign("library", function(package, ...) {
        pkg_name <- as.character(substitute(package))
        if (pkg_name == "config") {
          stop("config package loading blocked during taxonomy processing")
        }
        original_library(package, ...)
      }, envir = .GlobalEnv)
      
      # Step 4: Source the taxonomy file
      source(fp, local = .GlobalEnv)
      log_info("📦 disease_categories_taxaware.R sourced into .GlobalEnv")
      
      # Step 5: Restore original library function
      assign("library", original_library, envir = .GlobalEnv)
      
      # Step 6: Only re-attach config if it was originally attached AND we're not in taxonomy context
      if (config_was_attached) {
        # Use config::get directly without attaching the package
        # This avoids namespace conflicts
        log_info("✅ Config package remains detached to prevent conflicts")
      }
      
      # Verify key functions are available
      required_functions <- c("apply_morbidity_categorization", "load_taxonomy", "global_canonicalize")
      missing_functions <- required_functions[!sapply(required_functions, exists)]
      
      if (length(missing_functions) > 0) {
        log_warn("⚠️ Some taxonomy functions missing: {paste(missing_functions, collapse = ', ')}")
        return(FALSE)
      }
      
      # Test basic functionality WITHOUT any config
      test_result <- tryCatch({
        test_tax <- load_taxonomy("base")
        !is.null(test_tax)
      }, error = function(e) {
        log_warn("⚠️ Taxonomy loading test failed: {e$message}")
        FALSE
      })
      
      if (!test_result) {
        log_warn("⚠️ Taxonomy functionality test failed")
        return(FALSE)
      }
      
      log_info("✅ Disease categorization functions validated")
      return(TRUE)
      
    }, error = function(e) {
      log_error("❌ Failed to source disease_categories_taxaware.R: {e$message}")
      # Restore library function if something went wrong
      if (exists("original_library")) {
        assign("library", original_library, envir = .GlobalEnv)
      }
      return(FALSE)
    })
  } else {
    log_info("ℹ️ disease_categories_taxaware.R not found - morbidity categorization will be skipped")
    return(FALSE)
  }
}

source_validation <- function() {
  fp <- here("R", "validation_rules.R")
  if (file.exists(fp)) {
    tryCatch({
      source(fp, local = .GlobalEnv)
      log_info("📦 validation_rules.R sourced into .GlobalEnv")
      return(TRUE)
    }, error = function(e) {
      log_warn("⚠️ Failed to source validation_rules.R: {e$message}")
      return(FALSE)
    })
  } else {
    log_info("ℹ️ validation_rules.R not found - validation will be skipped")
    return(FALSE)
  }
}

# ═══════════════════════════════════════════════════════════════════════════════
# Main entry point - ENHANCED with better error handling
# ═══════════════════════════════════════════════════════════════════════════════

#' Apply simplified disease taxonomy mapping (country-specific to base canonical + ICD11)
#' @param data Data frame with morbidity column
#' @return Data frame with canonical_disease_imc, category_canonical_disease_imc and icd11_title columns
apply_simplified_taxonomy <- function(data) {
  
  if (!"morbidity" %in% names(data)) {
    stop("No morbidity column found in data")
  }
  
  # Initialize output columns with correct names
  data$canonical_disease_imc <- NA_character_
  data$category_canonical_disease_imc <- NA_character_
  data$icd11_title <- NA_character_
  
  # Load country configuration to get disease mappings
  country_config <- get_current_country_config()
  
  if (is.null(country_config)) {
    log_warn("⚠️ No country configuration found, using raw morbidity as canonical_disease_imc")
    data$canonical_disease_imc <- data$morbidity
    data$category_canonical_disease_imc <- "Uncategorized"
    return(data)
  }
  
  # Load base taxonomy to get canonical disease names
  base_taxonomy_path <- here("taxonomy", "base.yaml")
  base_taxonomy <- NULL
  
  if (file.exists(base_taxonomy_path)) {
    tryCatch({
      base_taxonomy <- yaml::read_yaml(base_taxonomy_path)
    }, error = function(e) {
      log_warn("⚠️ Could not load base taxonomy: {e$message}")
    })
  }
  
  # Load country-specific taxonomy
  country_taxonomy <- NULL
  if (!is.null(country_config$taxonomy$country_file)) {
    country_taxonomy_path <- here("taxonomy", country_config$taxonomy$country_file)
    if (file.exists(country_taxonomy_path)) {
      tryCatch({
        country_taxonomy <- yaml::read_yaml(country_taxonomy_path)
      }, error = function(e) {
        log_warn("⚠️ Could not load country taxonomy: {e$message}")
      })
    }
  }
  
  # Create disease mappings
  disease_mappings <- list()
  
  # Add country config mappings
  if (!is.null(country_config$disease_mappings$excel_names)) {
    disease_mappings <- c(disease_mappings, country_config$disease_mappings$excel_names)
  }
  
  # Add country taxonomy synonyms
  if (!is.null(country_taxonomy$synonyms)) {
    disease_mappings <- c(disease_mappings, country_taxonomy$synonyms)
  }
  
  # Apply disease mappings to get canonical_disease_imc
  canonical_mappings <- list()
  
  # Add base taxonomy synonyms (most important)
  if (!is.null(base_taxonomy$synonyms)) {
    canonical_mappings <- c(canonical_mappings, base_taxonomy$synonyms)
  }
  
  # Also add canonical disease names as self-mappings
  if (!is.null(base_taxonomy$canonicals)) {
    canonical_self_mappings <- setNames(names(base_taxonomy$canonicals), names(base_taxonomy$canonicals))
    canonical_mappings <- c(canonical_mappings, canonical_self_mappings)
  }
  
  # Add country config mappings
  if (length(disease_mappings) > 0) {
    canonical_mappings <- c(canonical_mappings, disease_mappings)
  }
  
  # Apply canonical mappings with comprehensive text normalization
  if (length(canonical_mappings) > 0) {
    # Ensure morbidity column is character (not factor) for proper lookup
    morbidity_char <- as.character(data$morbidity)
    
    # Text normalization function
    normalize_text <- function(text) {
      if (is.na(text) || is.null(text)) return(text)
      
      # Convert to lowercase
      text <- tolower(text)
      
      # Remove extra whitespace (multiple spaces, tabs, newlines)
      text <- gsub("\\s+", " ", text)
      text <- trimws(text)
      
      # Standardize punctuation patterns
      text <- gsub("\\s*[,;:]\\s*", ", ", text)  # Normalize comma/semicolon spacing
      text <- gsub("\\s*[()]\\s*", " ", text)    # Remove parentheses and normalize spacing
      text <- gsub("\\s*\\[.*?\\]\\s*", " ", text)  # Remove square brackets and contents
      text <- gsub("\\s*/\\s*", "/", text)       # Normalize forward slash spacing
      text <- gsub("\\s*-\\s*", "-", text)       # Normalize hyphen spacing
      
      # Remove common noise words and patterns
      text <- gsub("\\b(suspected|confirmed|acute|chronic)\\s+", "", text)
      text <- gsub("\\b(disease|syndrome|disorder|infection)\\s*$", "", text)
      text <- gsub("\\bunspecified\\b", "", text)
      text <- gsub("\\bother\\b", "", text)
      
      # Normalize medical abbreviations and variants
      text <- gsub("\\bdiarrh?oea\\b", "diarrhea", text)  # Standardize diarrhea spelling
      text <- gsub("\\banaemia\\b", "anemia", text)       # Standardize anemia spelling
      text <- gsub("\\boedema\\b", "edema", text)         # Standardize edema spelling
      
      # Clean up whitespace again after transformations
      text <- gsub("\\s+", " ", text)
      text <- trimws(text)
      
      return(text)
    }
    
    # Create normalized lookup tables
    normalized_canonical <- setNames(canonical_mappings, sapply(names(canonical_mappings), normalize_text))
    case_insensitive_canonical <- setNames(canonical_mappings, tolower(names(canonical_mappings)))
    
    # Try multiple matching strategies
    mapped_values <- vector("list", length(morbidity_char))
    for (i in seq_along(morbidity_char)) {
      disease <- morbidity_char[i]
      mapped_values[[i]] <- NA  # default to NA
      
      # Strategy 1: Exact match
      if (disease %in% names(canonical_mappings)) {
        mapped_values[[i]] <- canonical_mappings[[disease]]
        next
      }
      
      # Strategy 2: Case-insensitive match
      lower_disease <- tolower(disease)
      if (lower_disease %in% names(case_insensitive_canonical)) {
        mapped_values[[i]] <- case_insensitive_canonical[[lower_disease]]
        next
      }
      
      # Strategy 3: Normalized text match
      normalized_disease <- normalize_text(disease)
      if (!is.na(normalized_disease) && normalized_disease != "" && 
          normalized_disease %in% names(normalized_canonical)) {
        mapped_values[[i]] <- normalized_canonical[[normalized_disease]]
        next
      }
      
      # Strategy 4: Fuzzy matching on normalized text (for very close matches)
      if (!is.na(normalized_disease) && normalized_disease != "") {
        # Find potential matches with small edit distances
        normalized_keys <- names(normalized_canonical)
        distances <- adist(normalized_disease, normalized_keys)
        min_distance <- min(distances)
        
        # Only accept very close matches (edit distance <= 2) for common words
        if (min_distance <= 2 && nchar(normalized_disease) >= 5) {
          best_match_idx <- which.min(distances)[1]
          mapped_values[[i]] <- normalized_canonical[[normalized_keys[best_match_idx]]]
          next
        }
      }
    }
    
    # Convert NULL values to NA to handle unmapped diseases properly
    mapped_values[sapply(mapped_values, is.null)] <- NA
    data$canonical_disease_imc <- unlist(mapped_values)
    
    # Mark unmapped diseases explicitly instead of falling back to raw morbidity
    unmapped <- is.na(data$canonical_disease_imc)
    if (sum(unmapped) > 0) {
      log_warn("⚠️ Found {sum(unmapped)} unmapped diseases: {paste(unique(morbidity_char[unmapped]), collapse=', ')}")
      data$canonical_disease_imc[unmapped] <- "Unclassified"
    }
  } else {
    log_warn("⚠️ No canonical mappings available - all diseases marked as Unclassified")
    data$canonical_disease_imc <- "Unclassified"
  }
  
  # Add disease categories from base taxonomy
  if (!is.null(base_taxonomy$canonicals)) {
    # Create category lookup from canonical diseases
    category_lookup <- list()
    for (disease_name in names(base_taxonomy$canonicals)) {
      disease_info <- base_taxonomy$canonicals[[disease_name]]
      if (!is.null(disease_info$category)) {
        category_lookup[[disease_name]] <- disease_info$category
      }
    }
    
    # Map canonical diseases to categories
    data$category_canonical_disease_imc <- sapply(data$canonical_disease_imc, function(disease) {
      if (!is.na(disease) && disease != "Unclassified" && disease %in% names(category_lookup)) {
        category_lookup[[disease]]
      } else if (!is.na(disease) && disease == "Unclassified") {
        "Unclassified"
      } else {
        "Uncategorized"
      }
    })
  } else {
    data$category_canonical_disease_imc <- "Uncategorized"
  }
  
  # Load ICD11 mappings if enabled
  if (!is.null(country_config$taxonomy$icd11_enabled) && 
      country_config$taxonomy$icd11_enabled == "yes") {
    
    # Try to load existing ICD11 mappings
    icd11_mappings_path <- here("taxonomy", "icd11", "disease_mappings.yml")
    if (file.exists(icd11_mappings_path)) {
      tryCatch({
        icd11_mappings <- yaml::read_yaml(icd11_mappings_path)
        
        # Apply ICD11 title mappings based on canonical_disease_imc
        if (!is.null(icd11_mappings$mappings)) {
          # Look through all confidence levels for mappings
          all_icd11_mappings <- list()
          for (confidence_level in names(icd11_mappings$mappings)) {
            level_mappings <- icd11_mappings$mappings[[confidence_level]]
            for (disease_name in names(level_mappings)) {
              if (!is.null(level_mappings[[disease_name]]$icd11_title)) {
                all_icd11_mappings[[disease_name]] <- level_mappings[[disease_name]]$icd11_title
              }
            }
          }
          
          # Apply ICD11 title mappings
          if (length(all_icd11_mappings) > 0) {
            data$icd11_title <- all_icd11_mappings[data$canonical_disease_imc]
          }
        }
      }, error = function(e) {
        log_warn("⚠️ Could not load ICD11 mappings: {e$message}")
      })
    }
  }
  
  # Log summary
  mapped_count <- sum(!is.na(data$canonical_disease_imc), na.rm = TRUE)
  icd11_count <- sum(!is.na(data$icd11_title), na.rm = TRUE)
  
  log_info("✅ Taxonomy mapping complete:")
  log_info("   - Raw morbidity: {length(unique(data$morbidity, na.rm = TRUE))} unique diseases")
  log_info("   - canonical_disease_imc: {mapped_count}/{nrow(data)} records mapped")
  log_info("   - icd11_title: {icd11_count}/{nrow(data)} records with ICD11 titles")
  
  return(data)
}

#' Get current country configuration
get_current_country_config <- function() {
  
  # Try to detect country from environment or global variable
  current_country <- NULL
  
  # Check if ACTIVE_COUNTRY is set globally
  if (exists("ACTIVE_COUNTRY", envir = .GlobalEnv)) {
    current_country <- get("ACTIVE_COUNTRY", envir = .GlobalEnv)
  }
  
  # For now, default to South Sudan for this implementation
  if (is.null(current_country)) {
    current_country <- "south_sudan"
  }
  
  # Load the specific country config
  country_config_path <- here("config", "countries", paste0(current_country, ".yml"))
  
  if (!file.exists(country_config_path)) {
    log_warn("⚠️ Country config not found for {current_country}: {country_config_path}")
    
    # Fallback: try to load any available config
    config_files <- list.files(here("config", "countries"), pattern = "\\.yml$", full.names = TRUE)
    if (length(config_files) > 0) {
      country_config_path <- config_files[1]
      log_info("ℹ️ Using fallback config: {basename(country_config_path)}")
    } else {
      return(NULL)
    }
  }
  
  tryCatch({
    config <- yaml::read_yaml(country_config_path)
    log_info("🌍 Loaded country config: {config$country$name}")
    return(config)
  }, error = function(e) {
    log_warn("⚠️ Could not load country config: {e$message}")
    return(NULL)
  })
}

# ═══════════════════════════════════════════════════════════════════════════════

#' Prepare the register (events) dataset for analysis
#' @param raw_data list or data.frame from data_loader (expects $data or $register)
#' @param metadata_file optional path to Org Unit metadata Excel
#' @param apply_validation logical
#' @return data.frame (register)
#' @export
prepare_register <- function(raw_data, metadata_file = NULL, apply_validation = TRUE) {
  log_info("🔄 Starting data preprocessing...")
  
  # Apply immediate config fix
  tryCatch({
    if ("package:config" %in% search()) {
      detach("package:config", unload = TRUE, force = TRUE)
      log_info("🔧 Detached config package to prevent conflicts")
    }
  }, error = function(e) {
    log_warn("⚠️ Could not detach config package: {e$message}")
  })
  
  # Normalize input with better error handling
  if (is.null(raw_data)) {
    stop("raw_data is NULL")
  }
  
  if (is.data.frame(raw_data)) {
    df   <- raw_data
    meta <- list()
  } else if (is.list(raw_data) && 
             (("data" %in% names(raw_data)) || ("register" %in% names(raw_data)))) {
    df   <- raw_data$data %||% raw_data$register
    meta <- raw_data$metadata %||% list()
  } else {
    stop("Invalid raw_data: expected data.frame/tibble or a list with $data/$register")
  }
  
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    stop("No data to process: df is NULL, not a data.frame, or has 0 rows")
  }
  
  log_info("📊 Processing {nrow(df)} records with {ncol(df)} columns")
  log_info("🧪 Available columns BEFORE column selection: {paste(names(df), collapse = ', ')}")
  
  # Load optional modules
  disease_cats_ok <- source_disease_categories()
  validation_ok   <- source_validation()
  
  # Processing pipeline with enhanced error handling for each step
  tryCatch({
    log_info("📝 Step 1: Column selection and renaming")
    df <- select_and_rename_columns(df)
    log_info("✅ Step 1 completed: {ncol(df)} columns after selection")
    
    log_info("📝 Step 2: Type coercion")
    df <- coerce_basic_types(df)
    log_info("✅ Step 2 completed")

    # 🩺 Re-check for missing datevisit after type coercion
    log_info("🩺 Re-checking for missing datevisit...")
    if (!("datevisit" %in% names(df)) || all(is.na(df$datevisit))) {
      date_col_candidates <- c("Date of visit", "eventDate", "eventdate", "visitdate", "occurredat", "date", "dateofvisit")
      found_col <- NULL
      for (col in date_col_candidates) {
        if (col %in% names(df)) {
          found_col <- col
          break
        }
      }
      if (!is.null(found_col)) {
        log_info("⏱️ Attempting to parse fallback date column: {found_col}")
        df$datevisit <- to_date_safe(df[[found_col]])
      } else {
        log_warn("⚠️ No valid date column found for fallback parsing")
      }
    }
    
    log_info("📝 Step 3: Metadata join")
    df_before_meta <- nrow(df)
    df <- join_metadata_with_fallback(df, metadata_file, meta)
    log_info("✅ Step 3 completed: {nrow(df)} records (was {df_before_meta})")
    
    log_info("📝 Step 4: Age group harmonization")
    df <- harmonize_age_groups(df)
    log_info("✅ Step 4 completed")
    
    log_info("📝 Step 5: Date processing")
    df <- process_dates_and_time(df)
    log_info("✅ Step 5 completed")
    
    log_info("📝 Step 6: Geography standardization")
    df <- standardize_facilities_geography(df)
    log_info("✅ Step 6 completed")
    
  }, error = function(e) {
    log_error("❌ Preprocessing failed at step: {e$message}")
    log_error("Stack trace: {paste(deparse(sys.calls()), collapse = '\n')}")
    stop("Preprocessing failed: ", e$message, call. = FALSE)
  })
  
  # Apply comprehensive taxonomy system encompassing both custom canonicals (IMC) and ICD-11
  # This replaces the simplified approach with full taxonomy mapping including:
  # - IMC canonical disease names and categories from base.yaml
  # - ICD-11 codes, titles, and categories from disease_mappings.yml
  # - Epidemiological feature flags (VPD, climate-sensitive, outbreak-prone, etc.)
  # - Risk group classifications (pediatric, elderly, pregnancy-related, etc.)
  log_info("🎯 Applying comprehensive taxonomy system (IMC canonicals + ICD-11 + feature flags)")
  
  tryCatch({
    # Source the comprehensive taxonomy system
    source(here("R", "disease_categories_taxaware.R"))
    
    # Verify we have the morbidity column
    if (!"morbidity" %in% names(df)) {
      log_warn("⚠️ No 'morbidity' column found for disease taxonomy")
      # Create placeholder columns
      df$canonical_disease_imc <- NA_character_
      df$category_canonical_disease_imc <- "Uncategorized"
      df$icd11_title <- NA_character_
      df$icd11_code <- NA_character_
      df$icd11_category <- "Unclassified"
    } else {
      # Apply comprehensive morbidity categorization (canonicals + categories + ICD-11 + feature flags)
      df <- apply_morbidity_categorization(df)
      log_info("✅ Comprehensive taxonomy system applied successfully (IMC + ICD-11 + features)")
    }
    
  }, error = function(e) {
    log_error("❌ Comprehensive taxonomy mapping failed: {e$message}")
    log_warn("⚠️ Creating placeholder taxonomy columns")
    
    # Create placeholder columns if mapping fails
    df$canonical_disease_imc <- df$morbidity %||% NA_character_
    df$category_canonical_disease_imc <- "Uncategorized"
    df$icd11_title <- NA_character_
    df$icd11_code <- NA_character_
    df$icd11_category <- "Unclassified"
  })
  
  # Final cleanup
  df <- final_cleanup(df)
  
  log_info("✅ Preprocessing completed successfully: {nrow(df)} records, {ncol(df)} columns")
  
  return(df)
}

# ═══════════════════════════════════════════════════════════════════════════════
# Column selection / renaming (matches your listed variable names)
# ═══════════════════════════════════════════════════════════════════════════════

.norm <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  gsub("[^a-z0-9]", "", x)
}

pick_col <- function(df, patterns) {
  if (!length(colnames(df))) return(NA_character_)
  cn <- colnames(df)
  cn_norm <- .norm(cn)
  pat_norm <- unique(.norm(patterns))
  hit <- which(vapply(cn_norm, function(n) any(n %in% pat_norm), logical(1)))
  if (length(hit)) cn[hit[1]] else NA_character_
}

select_and_rename_columns <- function(df) {
  # Base mapping for common columns
  mapping <- list(
    orgunit       = c("Organisation unit name","Organisation unit","orgunit","ouname","organisationunitname","orgunitname"),
    orgunit_code  = c("Organisation unit code","oucode","Organisation unit id","ou","orgunitcode","organisationunitid"),
    # ENHANCED: Add South Sudan-specific date column patterns and Excel-transformed columns
    datevisit     = c("datevisit","Date of visit","Event date","eventdate","occurredat","date","visitdate","dateofvisit","Reporting_Month","reporting_month","ReportingMonth","time_period"),
    age_group     = c("Age group 1 (0-5,6-17,18-59,60+) - SY","agegroup","age group","AgeGroup","Age_Group"),
    age_group2    = c("SY -  Age Group (0-59, 5-17, 18-49, 50 and Above)","agegroup2"),
    age_group3    = c("Age group 2 (0-11m, 1-4, 5-14,15-49,50-60,60+) - SY","agegroup3"),
    age_group4    = c("Age group 3 (< 5 , 5-14,15-18,19-49,50+) - SY","agegroup4"),
    sex           = c("Gender","sex","Sex"),
    resident      = c("Resident Status","residentstatus","resident","Population_Type","PopulationType"),
    # ENHANCED: Add South Sudan-specific disease/morbidity patterns  
    morbidity     = c("SY - Morbidity classification","morbidity","diagnosis","condition","Disease_Category","DiseaseCategory","disease_category","Disease"),
    disease_category_sy = c("Disease Category - SY","diseasecategory"),
    type_case     = c("Type of Case (Trauma/Non-trauma)","typecase","casetype"),
    disability    = c("Patient Presents With Disability (Y/N) ","disability","Malnutrition_Status"),
    visit_number  = c("Visit Number - SYR","visitnumber"),
    visit_type    = c("Visit Type","visittype"),
    event         = c("event","Event"),
    program_stage = c("Program stage","programstage"),
    program_status= c("Program status","programstatus"),
    event_status  = c("Event status","eventstatus"),
    donor         = c("Donor"),
    project       = c("SY - Project ","project"),
    longitude     = c("Longitude","lng","lon"),
    latitude      = c("Latitude","lat"),
    # CRITICAL FIX: Add administrative geographic columns
    admin0        = c("Country","country","admin0","Admin0","ADMIN0"),
    admin1        = c("State","Governorate","admin1","Admin1","ADMIN1","state","governorate"),
    admin2        = c("District","County","admin2","Admin2","ADMIN2","district","county"),
    admin3        = c("Sub-district","Subdistrict","Payam","admin3","Admin3","ADMIN3","subdistrict","payam")
  )
  
  out <- list()
  found_columns <- 0
  
  for (new in names(mapping)) {
    old <- pick_col(df, mapping[[new]])
    if (!is.na(old) && nzchar(old)) {
      out[[new]] <- df[[old]]
      found_columns <- found_columns + 1
    } else if (new == "datevisit") {
      log_warn("⚠️ No date column matched for 'datevisit'")
    } else if (new == "morbidity" && "disease" %in% names(df)) {
      # CRITICAL FIX: If no morbidity column found but disease column exists (from Excel transform)
      log_info("🔧 Using 'disease' column for morbidity mapping")
      out[[new]] <- df[["disease"]]
      found_columns <- found_columns + 1
    } else if (new == "datevisit" && "time_period" %in% names(df)) {
      # CRITICAL FIX: If no datevisit column found but time_period exists (from Excel transform)
      log_info("🔧 Using 'time_period' column for date mapping")
      out[[new]] <- df[["time_period"]]
      found_columns <- found_columns + 1
    }
  }
  
  # CRITICAL FIX: Preserve essential Excel-transformed columns even if not explicitly mapped
  excel_essential_cols <- c("cases", "location")
  for (col in excel_essential_cols) {
    if (col %in% names(df) && !col %in% names(out)) {
      out[[col]] <- df[[col]]
      log_info("🔧 Preserved essential Excel column: {col}")
    }
  }
  
  # CRITICAL FIX: Create orgunit from location if missing but location exists
  if (!"orgunit" %in% names(out) && "location" %in% names(df)) {
    out[["orgunit"]] <- df[["location"]]
    found_columns <- found_columns + 1
    log_info("🔧 Created orgunit column from location")
  }
  
  if (found_columns == 0) {
    log_warn("⚠️ No expected columns found in data")
    log_info("Available columns: {paste(names(df), collapse = ', ')}")
    # Return original data with minimal modifications
    return(df)
  }
  
  out <- tibble::as_tibble(out)
  log_info("🧭 Selected/renamed columns: {paste(names(out), collapse=', ')}")
  out
}

# ═══════════════════════════════════════════════════════════════════════════════
# Types & metadata join with enhanced error handling
# ═══════════════════════════════════════════════════════════════════════════════

coerce_basic_types <- function(df) {
  tryCatch({
    fact_cols <- c("orgunit", "morbidity", "sex", "resident",
                   "disability", "type_case", "visit_number", "visit_type")
    for (nm in intersect(fact_cols, names(df))) df[[nm]] <- as.factor(df[[nm]])
    if ("orgunit_code" %in% names(df)) df$orgunit_code <- as.character(df$orgunit_code)
    return(df)
  }, error = function(e) {
    log_warn("⚠️ Error in type coercion: {e$message}")
    return(df)
  })
}

# Enhanced metadata join with better error handling
join_metadata_with_fallback <- function(df, metadata_file = NULL, meta_from_loader = NULL) {
  log_info("🔗 Starting metadata join process...")
  
  tryCatch({
    # Skip if readxl not available
    if (!requireNamespace("readxl", quietly = TRUE)) {
      log_warn("readxl not available; skipping org unit metadata join")
      return(df)
    }
    
    # 1. Priority: Use metadata from loader if available and valid
    if (is.data.frame(meta_from_loader) && nrow(meta_from_loader) > 0) {
      log_info("📊 Using metadata from data loader ({nrow(meta_from_loader)} rows)")
      meta <- meta_from_loader
    } else {
      # 2. Try to find and load Excel metadata file
      meta <- load_excel_metadata_safe(metadata_file)
      if (is.null(meta)) {
        log_info("ℹ️ No metadata file found or loaded; proceeding without metadata join")
        return(df)
      }
    }
    
    # 3. Perform the actual join
    return(perform_metadata_join(df, meta))
    
  }, error = function(e) {
    log_error("❌ Error in metadata join process: {e$message}")
    log_warn("⚠️ Continuing without metadata join")
    return(df)
  })
}

# Safe Excel metadata loader with enhanced error handling
load_excel_metadata_safe <- function(metadata_file = NULL) {
  tryCatch({
    # Build candidate file paths
    candidates <- build_metadata_file_candidates(metadata_file)
    
    log_info("🔎 Searching for metadata file...")
    log_debug("Candidates: {paste(candidates, collapse = ', ')}")
    
    # Find first existing file
    path_found <- find_first_existing_file(candidates)
    if (is.na(path_found)) {
      log_info("📄 No metadata file found in candidate locations")
      return(NULL)
    }
    
    log_info("🔎 Found metadata file: {path_found}")
    
    # Check file size and warn if very large
    file_size_mb <- round(file.size(path_found) / 1024 / 1024, 2)
    if (file_size_mb > 50) {
      log_warn("⚠️ Large metadata file detected ({file_size_mb} MB) - this may take time to load")
    }
    
    # Load with safety checks and timeout protection
    return(read_excel_with_protection(path_found))
    
  }, error = function(e) {
    log_error("❌ Error loading Excel metadata: {e$message}")
    return(NULL)
  })
}

# Enhanced metadata file candidates with better fallbacks
build_metadata_file_candidates <- function(metadata_file = NULL) {
  candidates <- character(0)
  
  # 1. Explicit metadata_file parameter
  if (!is.null(metadata_file) && nzchar(metadata_file)) {
    candidates <- c(candidates,
                    if (!is_abs_path(metadata_file)) here::here(metadata_file),
                    metadata_file)
  }
  
  # 2. Enhanced standard repository locations
  standard_locations <- c(
    here::here("data", "metadata", "Org Unit Metadata.xlsx"),
    here::here("data", "metadata", "org unit metadata.xlsx"),
    here::here("data", "metadata", "OrgUnitMetadata.xlsx"),
    here::here("data", "metadata", "metadata.xlsx"),
    here::here("data", "Org Unit Metadata.xlsx"),
    here::here("metadata", "Org Unit Metadata.xlsx"),
    here::here("metadata", "org unit metadata.xlsx"),
    here::here("metadata", "metadata.xlsx"),
    here::here("Org Unit Metadata.xlsx"),
    here::here("metadata.xlsx"),
    # Also try without here() in case of path issues
    "data/metadata/Org Unit Metadata.xlsx",
    "data/metadata/org unit metadata.xlsx",
    "data/metadata/OrgUnitMetadata.xlsx",
    "data/metadata/metadata.xlsx",
    "metadata/Org Unit Metadata.xlsx",
    "metadata/org unit metadata.xlsx",
    "Org Unit Metadata.xlsx",
    "metadata.xlsx"
  )
  
  candidates <- c(candidates, standard_locations)
  
  # Remove duplicates and NAs
  candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates)])
  
  log_debug("Metadata file candidates: {paste(candidates[1:min(5, length(candidates))], collapse = ', ')}{if(length(candidates) > 5) '...' else ''}")
  
  return(candidates)
}

# Find first existing file from candidates
find_first_existing_file <- function(candidates) {
  for (candidate in candidates) {
    if (!is.na(candidate) && file.exists(candidate)) {
      return(candidate)
    }
  }
  return(NA_character_)
}

# Protected Excel reading with enhanced error handling
read_excel_with_protection <- function(path) {
  log_info("📖 Loading Excel metadata: {basename(path)}")
  
  tryCatch({
    # First, try to validate the file quickly
    if (!validate_excel_file_quick(path)) {
      log_warn("⚠️ Excel file validation failed, skipping: {path}")
      return(NULL)
    }
    
    # Try reading with safe parameters
    result <- read_excel_with_safe_params(path)
    
    # Validate result
    if (is.null(result) || !is.data.frame(result) || nrow(result) == 0) {
      log_warn("⚠️ No valid metadata loaded from Excel file")
      return(NULL)
    }
    
    log_info("✅ Metadata loaded: {nrow(result)} rows x {ncol(result)} columns")
    return(result)
    
  }, error = function(e) {
    log_warn("⚠️ Failed to read Excel file: {e$message}")
    return(NULL)
  })
}

# Quick Excel file validation
validate_excel_file_quick <- function(path) {
  # Check file exists and is readable
  if (!file.exists(path)) return(FALSE)
  if (file.access(path, mode = 4) != 0) return(FALSE)
  
  # Check file size (skip if too large)
  file_size_mb <- file.size(path) / 1024 / 1024
  if (file_size_mb > 100) {  # Skip files larger than 100MB
    log_warn("⚠️ Excel file too large ({round(file_size_mb, 1)} MB), skipping")
    return(FALSE)
  }
  
  # Check file extension
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("xlsx", "xls")) {
    log_warn("⚠️ Not an Excel file: {path}")
    return(FALSE)
  }
  
  return(TRUE)
}

# Read Excel with safe parameters
read_excel_with_safe_params <- function(path) {
  readxl::read_excel(
    path,
    sheet = 1,  # Always read first sheet
    col_names = TRUE,
    col_types = "text",  # Read everything as text to avoid type issues
    na = c("", "NA", "N/A", "#N/A", "NULL"),
    trim_ws = TRUE,
    skip = 0,
    n_max = 10000,  # Limit rows to prevent memory issues
    guess_max = 100,  # Limit guessing to speed up
    progress = FALSE,  # Disable progress bar
    .name_repair = "unique"
  )
}

# Perform the actual metadata join
perform_metadata_join <- function(df, meta) {
  log_info("🔗 Performing metadata join...")
  
  tryCatch({
    # Clean up metadata columns to avoid conflicts
    meta <- clean_metadata_for_join(meta)
    
    # Find the org unit column in metadata
    ou_meta_col <- find_org_unit_column(meta)
    if (is.na(ou_meta_col)) {
      log_warn("⚠️ No org unit column found in metadata; skipping join")
      return(df)
    }
    
    log_info("🎯 Using metadata column '{ou_meta_col}' for join")
    
    # Prepare join columns
    if (!"orgunit" %in% names(df)) {
      log_warn("⚠️ No 'orgunit' column in data; skipping metadata join")
      return(df)
    }
    
    # Clean org unit names for better matching
    df_clean <- df %>%
      mutate(orgunit_clean = clean_org_unit_name(orgunit))
    
    meta_clean <- meta %>%
      mutate(orgunit_clean = clean_org_unit_name(.data[[ou_meta_col]]))
    
    # Perform join
    # Primary join on cleaned names
    by_vec <- setNames("orgunit_clean", "orgunit_clean")
    result <- dplyr::left_join(df_clean, meta_clean, by = by_vec, suffix = c("", "_meta"))
    
    # Count successful joins
    successful_joins <- sum(!is.na(result[[paste0(ou_meta_col, "_meta")]]) | 
                              !is.na(result[[ou_meta_col]]), na.rm = TRUE)
    total_records <- nrow(df)
    join_rate <- round(100 * successful_joins / total_records, 1)
    
    log_info("✅ Metadata join completed: {successful_joins}/{total_records} records matched ({join_rate}%)")
    
    # Clean up temporary columns
    result <- result %>%
      select(-orgunit_clean) %>%
      select(-any_of(paste0(names(meta_clean), "_meta")))
    
    return(result)
    
  }, error = function(e) {
    log_warn("⚠️ Metadata join failed: {e$message}")
    return(df)
  })
}

# Clean metadata to avoid column conflicts
clean_metadata_for_join <- function(meta) {
  # Remove common event columns that might conflict
  conflict_cols <- c("event", "program", "program_stage", "eventdate", 
                     "datevisit", "morbidity", "sex", "age_group")
  
  meta_cleaned <- meta %>%
    select(-any_of(conflict_cols))
  
  removed_cols <- intersect(names(meta), conflict_cols)
  if (length(removed_cols) > 0) {
    log_info("🧹 Removed conflicting metadata columns: {paste(removed_cols, collapse = ', ')}")
  }
  
  return(meta_cleaned)
}

# Find org unit column in metadata
find_org_unit_column <- function(meta) {
  ou_patterns <- c(
    "OrgUnit", "orgunit", "Organisation unit name", "Organisation unit", 
    "Org Unit", "Org unit", "org_unit", "facility", "Facility",
    "facility_name", "unit_name", "organization", "Organization"
  )
  
  meta_cols <- names(meta)
  
  # Exact match first
  exact_match <- intersect(ou_patterns, meta_cols)
  if (length(exact_match) > 0) {
    return(exact_match[1])
  }
  
  # Fuzzy match
  meta_cols_lower <- tolower(meta_cols)
  ou_patterns_lower <- tolower(ou_patterns)
  
  for (pattern in ou_patterns_lower) {
    matches <- which(grepl(pattern, meta_cols_lower, fixed = TRUE))
    if (length(matches) > 0) {
      return(meta_cols[matches[1]])
    }
  }
  
  return(NA_character_)
}

# Clean org unit names for better matching
clean_org_unit_name <- function(x) {
  x %>%
    as.character() %>%
    trimws() %>%
    toupper() %>%
    # Remove common prefixes/suffixes
    gsub("^(PHC|HEALTH CENTER|HC|CLINIC)\\s*", "", .) %>%
    gsub("\\s*(PHC|HEALTH CENTER|HC|CLINIC)$", "", .) %>%
    # Remove extra whitespace
    gsub("\\s+", " ", .) %>%
    trimws()
}

# ═══════════════════════════════════════════════════════════════════════════════
# Age groups, dates/time, facility & geo standardization - ENHANCED
# ═══════════════════════════════════════════════════════════════════════════════

harmonize_age_groups <- function(df) {
  if (!"age_group" %in% names(df)) return(df)
  
  tryCatch({
    df %>%
      mutate(
        age_group_new = dplyr::case_when(
          age_group %in% c("<5", "0-5 y") ~ "0-5 y",
          age_group %in% c("5 - 17", "6-17 y") ~ "6-17 y",
          age_group == "18 - 30" ~ "18-30 y",
          age_group == "31 - 59" ~ "31-59 y",
          age_group == "18-59 y" ~ "18-59 y",
          age_group %in% c("60+", "≥60 y") ~ "60+ y",
          TRUE ~ as.character(age_group)
        ),
        age_group_new = factor(
          age_group_new,
          levels = c("0-5 y", "6-17 y", "18-30 y", "31-59 y", "18-59 y", "60+ y")
        )
      )
  }, error = function(e) {
    log_warn("⚠️ Error in age group harmonization: {e$message}")
    return(df)
  })
}

process_dates_and_time <- function(df, tz = "UTC") {
  log_info("🗓️ Starting robust date processing...")
  
  tryCatch({
    # Expanded date column candidates with more variations
    date_col_candidates <- c(
      "datevisit", "Date of visit", "eventdate", "Event date", 
      "visitdate", "date_visit", "occurredat", "date", "eventDate",
      "occurred_at", "Occurred at", "visit_date", "consultation_date",
      "dateofvisit", "DateOfVisit", "EventDate", "OccurredAt"
    )
    
    hit_col <- NULL
    
    # Strategy 1: Exact match (case insensitive)
    log_info("🔍 Searching for date columns...")
    for (candidate in date_col_candidates) {
      exact_matches <- which(tolower(names(df)) == tolower(candidate))
      if (length(exact_matches) > 0) {
        hit_col <- names(df)[exact_matches[1]]
        log_info("✅ Found exact match for date column: {hit_col}")
        break
      }
    }
    
    # Strategy 2: Partial match
    if (is.null(hit_col)) {
      for (candidate in date_col_candidates) {
        partial_matches <- which(grepl(tolower(candidate), tolower(names(df)), fixed = TRUE))
        if (length(partial_matches) > 0) {
          hit_col <- names(df)[partial_matches[1]]
          log_info("✅ Found partial match for date column: {hit_col}")
          break
        }
      }
    }
    
    # Strategy 3: Any column with 'date' in the name
    if (is.null(hit_col)) {
      date_cols <- names(df)[grepl("date", tolower(names(df)))]
      if (length(date_cols) > 0) {
        hit_col <- date_cols[1]
        log_info("✅ Found date-containing column: {hit_col}")
      }
    }
    
    # Strategy 4: Look for columns with date-like content
    if (is.null(hit_col)) {
      log_info("🔍 Analyzing column content for date patterns...")
      for (col_name in names(df)) {
        sample_values <- head(df[[col_name]], 20)
        sample_values <- sample_values[!is.na(sample_values)]
        
        if (length(sample_values) > 0) {
          # Check if values look like dates
          date_like_count <- sum(grepl("\\d{4}-\\d{2}-\\d{2}|\\d{2}/\\d{2}/\\d{4}|\\d{1,2}/\\d{1,2}/\\d{4}", 
                                     as.character(sample_values)))
          
          if (date_like_count > length(sample_values) * 0.5) {
            hit_col <- col_name
            log_info("✅ Found date column by content analysis: {hit_col}")
            break
          }
        }
      }
    }
    
    # Handle case where no date column found
    if (is.null(hit_col)) {
      log_error("❌ No date column found!")
      log_info("Available columns:")
      for (col in names(df)) {
        sample_val <- df[[col]][1]
        log_info("   - {col}: {sample_val}")
      }
      
      # Create empty date variables and return
      df$datevisit <- as.Date(NA)
      df$year <- NA_integer_
      df$month <- NA_character_
      df$quarter <- NA_character_
      df$month2 <- as.Date(NA)
      
      log_warn("⚠️ Created empty date variables - manual intervention required")
      return(df)
    }
    
    log_info("📅 Processing dates from column: {hit_col}")
    
    # Parse the dates using the identified column
    raw_dates <- df[[hit_col]]
    log_info("📊 Found {length(raw_dates)} date values to process")
    
    # Enhanced date parsing with better error handling
    parsed_dates <- enhanced_date_parser_v2(raw_dates, tz = tz)
    
    # Assign to standardized datevisit column
    df$datevisit <- parsed_dates
    
    # Report results
    successful_dates <- sum(!is.na(df$datevisit))
    total_dates <- length(df$datevisit)
    success_rate <- round((successful_dates / total_dates) * 100, 1)
    
    log_info("📊 Successfully parsed {successful_dates} of {total_dates} dates ({success_rate}%)")
    
    # Show sample of parsed dates for verification
    if (successful_dates > 0) {
      valid_dates <- df$datevisit[!is.na(df$datevisit)]
      sample_dates <- head(valid_dates, 3)
      date_range <- range(valid_dates)
      
      log_info("📅 Sample parsed dates: {paste(sample_dates, collapse = ', ')}")
      log_info("📅 Date range: {date_range[1]} to {date_range[2]}")
      
      # Generate derived date variables
      df$year <- as.integer(lubridate::year(df$datevisit))
      df$month <- format(df$datevisit, "%Y-%m")
      
      # Quarter calculation
      qn <- ceiling(lubridate::month(df$datevisit) / 3)
      df$quarter <- ifelse(is.na(qn), NA_character_, paste0(df$year, " Q", qn))
      df$month2 <- as.Date(lubridate::floor_date(df$datevisit, "month"))
      
      log_info("✅ Created derived date variables: year, month, quarter, month2")
    } else {
      log_warn("⚠️ No dates could be parsed - creating empty date variables")
      df$year <- NA_integer_
      df$month <- NA_character_
      df$quarter <- NA_character_
      df$month2 <- as.Date(NA)
    }
    
    return(df)
    
  }, error = function(e) {
    log_error("❌ Error in date processing: {e$message}")
    
    # Create minimal date variables to prevent downstream errors
    df$datevisit <- as.Date(NA)
    df$year <- NA_integer_
    df$month <- NA_character_
    df$quarter <- NA_character_
    df$month2 <- as.Date(NA)
    
    return(df)
  })
}

enhanced_date_parser_v2 <- function(date_values, tz = "UTC") {
  if (is.null(date_values) || length(date_values) == 0) {
    return(as.Date(character()))
  }
  
  # Handle different input types
  if (inherits(date_values, "Date")) {
    return(date_values)
  }
  
  if (inherits(date_values, c("POSIXct", "POSIXt"))) {
    return(as.Date(date_values, tz = tz))
  }
  
  # Convert to character and clean
  char_dates <- as.character(date_values)
  char_dates <- trimws(char_dates)
  
  # Replace common NA representations
  na_values <- c("", "NA", "N/A", "null", "NULL", "NaN", "#N/A", "missing", "MISSING", "na")
  char_dates[char_dates %in% na_values] <- NA_character_
  
  # Initialize result vector
  result <- rep(as.Date(NA), length(char_dates))
  
  # Process non-NA values
  valid_indices <- which(!is.na(char_dates) & nzchar(char_dates))
  
  if (length(valid_indices) == 0) {
    log_warn("⚠️ No valid date values found")
    return(result)
  }
  
  log_info("📄 Processing {length(valid_indices)} non-empty date values...")
  
  # Show sample of raw values for debugging
  sample_raw <- head(char_dates[valid_indices], 5)
  log_info("📋 Sample raw date values: {paste(sample_raw, collapse = ' | ')}")
  
  success_count <- 0
  
  for (i in valid_indices) {
    date_str <- char_dates[i]
    parsed_date <- NULL
    
    # Try multiple parsing strategies
    tryCatch({
      # Strategy 1: Check if it's a numeric value (Excel serial date)
      if (grepl("^[0-9.]+$", date_str)) {
        num_val <- as.numeric(date_str)
        
        # Excel serial dates (range: 25569 = 1970-01-01 to 100000 = 2173-10-14)
        if (num_val >= 25569 && num_val <= 100000) {
          parsed_date <- as.Date(num_val, origin = "1899-12-30")
        }
        # Unix timestamp in seconds (2000-2050 range)
        else if (num_val >= 946684800 && num_val <= 2524608000) {
          parsed_date <- as.Date(as.POSIXct(num_val, origin = "1970-01-01", tz = tz))
        }
        # Unix timestamp in milliseconds
        else if (num_val >= 946684800000 && num_val <= 2524608000000) {
          parsed_date <- as.Date(as.POSIXct(num_val / 1000, origin = "1970-01-01", tz = tz))
        }
      }
      
      # Strategy 2: Try standard date formats
      if (is.null(parsed_date)) {
        formats <- c(
          "%Y-%m-%d",           # 2023-12-31
          "%d/%m/%Y",           # 31/12/2023
          "%m/%d/%Y",           # 12/31/2023
          "%d-%m-%Y",           # 31-12-2023
          "%Y/%m/%d",           # 2023/12/31
          "%d.%m.%Y",           # 31.12.2023
          "%Y-%m-%d %H:%M:%S",  # 2023-12-31 15:30:00
          "%d/%m/%Y %H:%M:%S",  # 31/12/2023 15:30:00
          "%m/%d/%Y %H:%M:%S",  # 12/31/2023 15:30:00
          "%Y%m%d",             # 20231231
          "%d%m%Y",             # 31122023
          "%Y-%m-%dT%H:%M:%S",  # ISO format
          "%d/%m/%y",           # 31/12/23
          "%m/%d/%y"            # 12/31/23
        )
        
        for (fmt in formats) {
          test_date <- suppressWarnings(as.Date(date_str, format = fmt))
          if (!is.na(test_date)) {
            parsed_date <- test_date
            break
          }
        }
      }
      
      # Strategy 3: Use lubridate functions
      if (is.null(parsed_date)) {
        lubridate_parsers <- list(
          lubridate::ymd,
          lubridate::dmy,
          lubridate::mdy,
          function(x) as.Date(lubridate::ymd_hms(x, tz = tz)),
          function(x) as.Date(lubridate::dmy_hms(x, tz = tz)),
          function(x) as.Date(lubridate::mdy_hms(x, tz = tz)),
          function(x) as.Date(lubridate::ymd_hm(x, tz = tz)),
          function(x) as.Date(lubridate::dmy_hm(x, tz = tz)),
          function(x) as.Date(lubridate::mdy_hm(x, tz = tz))
        )
        
        for (parser in lubridate_parsers) {
          test_date <- suppressWarnings(parser(date_str))
          if (!is.na(test_date)) {
            parsed_date <- as.Date(test_date)
            break
          }
        }
      }
      
      # Assign result if successful and reasonable
      if (!is.null(parsed_date) && !is.na(parsed_date)) {
        # Sanity check: date should be reasonable (between 1900 and 2050)
        if (parsed_date >= as.Date("1900-01-01") && parsed_date <= as.Date("2050-12-31")) {
          result[i] <- parsed_date
          success_count <- success_count + 1
        }
      }
      
    }, error = function(e) {
      # Silent failure for individual dates
    })
  }
  
  log_info("✅ Successfully parsed {success_count} dates")
  
  return(result)
}

standardize_facilities_geography <- function(df) {
  tryCatch({
    log_info("🗺️ Standardizing geographic variables...")
    
    # CRITICAL FIX: Create admin0 if missing but admin1 exists
    if (!"admin0" %in% names(df) && "admin1" %in% names(df)) {
      # Try to determine country from admin1 values
      unique_admin1 <- unique(df$admin1[!is.na(df$admin1)])
      log_info("🔍 Detected admin1 values for country identification: {paste(head(unique_admin1, 3), collapse = ', ')}")
      
      # Country detection logic based on admin1 patterns
      if (any(grepl("(Upper Nile|Unity|Jonglei|Central Equatoria|Western Equatoria|Eastern Equatoria|Northern Bahr|Western Bahr|Lakes|Warrap)", unique_admin1, ignore.case = TRUE))) {
        df$admin0 <- "South Sudan"
        log_info("✅ Identified country as South Sudan from admin1 patterns")
      } else if (any(grepl("(Sana'a|Aden|Taiz|Hodeidah|Ibb|Dhamar|Al Hudaydah)", unique_admin1, ignore.case = TRUE))) {
        df$admin0 <- "Yemen"
        log_info("✅ Identified country as Yemen from admin1 patterns")  
      } else if (any(grepl("(Aleppo|Damascus|Homs|Hama|Idlib|Daraa|Deir|Raqqa|Hasakah)", unique_admin1, ignore.case = TRUE))) {
        df$admin0 <- "Syria"
        log_info("✅ Identified country as Syria from admin1 patterns")
      } else {
        # Default fallback - check if we have a country column that wasn't mapped
        if ("Country" %in% names(df)) {
          df$admin0 <- df$Country
          log_info("✅ Used existing Country column for admin0")
        } else {
          df$admin0 <- "Unknown"
          log_warn("⚠️ Could not identify country from admin1 patterns, set to 'Unknown'")
        }
      }
    } else if ("admin0" %in% names(df)) {
      log_info("✅ admin0 column already exists")
    }
    
    if ("facilitytype" %in% names(df)) {
      df <- df %>%
        mutate(facilitytype = dplyr::case_when(
          facilitytype == "Clinic" ~ "PHC",
          facilitytype == "MMT"    ~ "MMU",
          TRUE ~ as.character(facilitytype)
        ))
      df$facilitytype <- factor(df$facilitytype)
    }
    
    if ("region" %in% names(df)) df$region <- factor(df$region)
    
    # Enhanced admin1 standardization for multiple countries
    if ("admin1" %in% names(df)) {
      df <- df %>% mutate(admin1 = dplyr::case_when(
        # Syria standardizations
        admin1 == "Al-Hasakah" ~ "Al-Hasakeh",
        # South Sudan standardizations - ensure consistent naming
        admin1 == "Upper Nile State" ~ "Upper Nile",
        admin1 == "Unity State" ~ "Unity",
        admin1 == "Central Equatoria State" ~ "Central Equatoria",
        admin1 == "Western Equatoria State" ~ "Western Equatoria",
        admin1 == "Eastern Equatoria State" ~ "Eastern Equatoria",
        TRUE ~ as.character(admin1)
      ))
      log_info("🏛️ Standardized admin1 values")
    }
    
    # Enhanced admin2 standardization
    if ("admin2" %in% names(df)) {
      df <- df %>% mutate(admin2 = dplyr::case_when(
        # Syria standardizations
        admin2 == "Al Quasir" ~ "Al-Qusayr",
        TRUE ~ as.character(admin2)
      ))
      log_info("🏛️ Standardized admin2 values")
    }
    
    if ("orgunit" %in% names(df)) {
      df$orgunit_clean <- trimws(toupper(as.character(df$orgunit)))
    }
    
    # Report on geographic variables created/found
    geo_vars <- intersect(c("admin0", "admin1", "admin2", "admin3"), names(df))
    if (length(geo_vars) > 0) {
      log_info("🗺️ Geographic variables available: {paste(geo_vars, collapse = ', ')}")
      for (var in geo_vars) {
        unique_count <- length(unique(df[[var]][!is.na(df[[var]])]))
        log_info("   📊 {var}: {unique_count} unique values")
      }
    }
    
    return(df)
  }, error = function(e) {
    log_warn("⚠️ Error in geography standardization: {e$message}")
    return(df)
  })
}

# ═══════════════════════════════════════════════════════════════════════════════
# Final cleanup / minimal fallback
# ═══════════════════════════════════════════════════════════════════════════════

final_cleanup <- function(df) {
  tryCatch({
    keep <- vapply(df, function(x) any(!is.na(x)), logical(1))
    df <- df[, keep, drop = FALSE]
    n0 <- nrow(df)
    df <- dplyr::distinct(df)
    if (nrow(df) < n0) log_info("🧹 Removed {n0 - nrow(df)} duplicate rows")
    if ("datevisit" %in% names(df)) df <- dplyr::arrange(df, dplyr::desc(datevisit))
    return(df)
  }, error = function(e) {
    log_warn("⚠️ Error in final cleanup: {e$message}")
    return(df)
  })
}

minimal_preprocessing <- function(df) {
  log_warn("⚠️ Using minimal preprocessing fallback")
  tryCatch({
    if (!"datevisit" %in% names(df)) {
      if ("Date of visit" %in% names(df)) {
        df$datevisit <- suppressWarnings(lubridate::dmy(df[["Date of visit"]]))
      } else if ("eventdate" %in% names(df)) {
        df$datevisit <- as.Date(df$eventdate)
      }
    }
    return(df)
  }, error = function(e) {
    log_warn("⚠️ Even minimal preprocessing failed: {e$message}")
    return(df)
  })
}

# ═══════════════════════════════════════════════════════════════════════════════
# Summary helpers with enhanced error handling
# ═══════════════════════════════════════════════════════════════════════════════

generate_preprocessing_summary <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(list(error = "No data to summarize"))
  
  tryCatch({
    summary_info <- list(
      dataset_info = list(
        total_rows = nrow(df),
        total_columns = ncol(df),
        date_range = if ("datevisit" %in% names(df)) {
          c(min(df$datevisit, na.rm = TRUE), max(df$datevisit, na.rm = TRUE))
        } else "Date information not available"
      ),
      completeness = sapply(df, function(x) round(100 * sum(!is.na(x)) / length(x), 1)),
      key_variables = list()
    )
    
    if ("age_group" %in% names(df)) summary_info$key_variables$age_groups <- table(df$age_group, useNA = "ifany")
    if ("sex" %in% names(df))       summary_info$key_variables$sex_distribution <- table(df$sex, useNA = "ifany")
    if ("resident" %in% names(df))  summary_info$key_variables$population_types <- table(df$resident, useNA = "ifany")
    
    if ("category_canonical_disease_imc" %in% names(df)) {
      cat_summary <- table(df$category_canonical_disease_imc, useNA = "ifany")
      summary_info$key_variables$disease_categories <- head(sort(cat_summary, decreasing = TRUE), 10)
      summary_info$key_variables$uncategorized_count <- sum(df$category_canonical_disease_imc == "Uncategorized", na.rm = TRUE)
    }
    
    if ("admin1" %in% names(df)) summary_info$key_variables$governorates <- table(df$admin1, useNA = "ifany")
    if ("orgunit" %in% names(df)) summary_info$key_variables$facility_count <- length(unique(df$orgunit[!is.na(df$orgunit)]))
    if ("year" %in% names(df))    summary_info$key_variables$years <- table(df$year, useNA = "ifany")
    
    # Check for new taxonomy variables
    epidemic_cols <- c("is_epidemic", "cholera_case", "measles_case", "polio_case", "covid_case", 
                       "trauma_related", "malnutrition_related")
    ep_present <- intersect(epidemic_cols, names(df))
    if (length(ep_present) > 0) {
      summary_info$key_variables$epidemic_cases <- sapply(ep_present, function(col) sum(df[[col]], na.rm = TRUE))
    }
    
    # Check for new taxonomy flags
    taxonomy_flags <- c("epidemic_prone", "climate_sensitive", "amr_relevant", "vaccine_preventable",
                        "outbreak_prone_case", "vpd_case", "climate_sensitive_case", "amr_case")
    tax_present <- intersect(taxonomy_flags, names(df))
    if (length(tax_present) > 0) {
      summary_info$key_variables$taxonomy_flags <- sapply(tax_present, function(col) sum(df[[col]], na.rm = TRUE))
    }
    
    summary_info$data_quality <- list(
      missing_dates    = if ("datevisit" %in% names(df)) sum(is.na(df$datevisit)) else NA,
      missing_morbidity= if ("morbidity" %in% names(df)) sum(is.na(df$morbidity)) else NA,
      missing_geography= if ("admin1" %in% names(df)) sum(is.na(df$admin1)) else NA,
      duplicate_rows   = nrow(df) - nrow(dplyr::distinct(df))
    )
    
    return(summary_info)
    
  }, error = function(e) {
    log_error("❌ Error generating preprocessing summary: {e$message}")
    return(list(error = paste("Summary generation failed:", e$message)))
  })
}

print_preprocessing_summary <- function(summary_list) {
  if ("error" %in% names(summary_list)) {
    cat("❌ Error:", summary_list$error, "\n"); return()
  }
  cat("📊 PREPROCESSING SUMMARY REPORT\n")
  cat("=" %r% 50, "\n\n")
  
  info <- summary_list$dataset_info
  cat("📈 Dataset Overview:\n")
  cat("  • Total records:", format(info$total_rows, big.mark = ","), "\n")
  cat("  • Total variables:", info$total_columns, "\n")
  if (!is.character(info$date_range)) {
    cat("  • Date range:", format(info$date_range[1]), "to", format(info$date_range[2]), "\n")
  }
  cat("\n")
  
  cat("📋 Data Completeness (% non-missing):\n")
  keyc <- summary_list$completeness[c("datevisit", "morbidity", "age_group", "sex", "admin1", "orgunit")]
  keyc <- keyc[!is.na(names(keyc))]
  for (var in names(keyc)) cat("  • ", var, ":", keyc[var], "%\n")
  cat("\n")
  
  kv <- summary_list$key_variables
  if ("disease_categories" %in% names(kv)) {
    cat("🥼 Top Disease Categories:\n")
    topd <- head(kv$disease_categories, 5)
    for (i in seq_along(topd)) cat("  ", i, ".", names(topd)[i], ":", format(topd[i], big.mark = ","), "cases\n")
    if ("uncategorized_count" %in% names(kv) && kv$uncategorized_count > 0)
      cat("  ⚠️  Uncategorized diseases:", format(kv$uncategorized_count, big.mark = ","), "cases\n")
    cat("\n")
  }
  
  if ("governorates" %in% names(kv)) {
    cat("🗺️ Geographic Coverage:\n")
    cat("  • Governorates:", length(kv$governorates), "\n")
    topg <- head(sort(kv$governorates, decreasing = TRUE), 3)
    for (g in names(topg)) cat("  • ", g, ":", format(topg[g], big.mark = ","), "cases\n")
    cat("\n")
  }
  
  if ("epidemic_cases" %in% names(kv)) {
    cat("🚨 Epidemic Disease Indicators:\n")
    epi <- kv$epidemic_cases
    names_map <- c(
      "is_epidemic"="Total epidemic cases", "cholera_case"="Cholera/AWD","measles_case"="Measles",
      "polio_case"="Polio","covid_case"="COVID-19", "trauma_related"="Trauma cases",
      "malnutrition_related"="Malnutrition cases"
    )
    for (nm in names(epi)) {
      label <- names_map[[nm]] %||% nm
      cat("  • ", label, ":", format(epi[[nm]], big.mark = ","), "cases\n")
    }
    cat("\n")
  }
  
  if ("taxonomy_flags" %in% names(kv)) {
    cat("🏷️ Taxonomy Classification Summary:\n")
    tax <- kv$taxonomy_flags
    names_map <- c(
      "epidemic_prone"="Outbreak-prone diseases", "climate_sensitive"="Climate-sensitive diseases",
      "amr_relevant"="AMR-relevant diseases", "vaccine_preventable"="Vaccine-preventable diseases",
      "outbreak_prone_case"="Outbreak-prone cases", "vpd_case"="VPD cases",
      "climate_sensitive_case"="Climate-sensitive cases", "amr_case"="AMR cases"
    )
    for (nm in names(tax)) {
      label <- names_map[[nm]] %||% nm
      cat("  • ", label, ":", format(tax[[nm]], big.mark = ","), "cases\n")
    }
    cat("\n")
  }
  
  dq <- summary_list$data_quality
  issues <- c()
  if (!is.na(dq$missing_dates) && dq$missing_dates > 0) issues <- c(issues, paste("Missing dates:", dq$missing_dates))
  if (!is.na(dq$missing_morbidity) && dq$missing_morbidity > 0) issues <- c(issues, paste("Missing diagnoses:", dq$missing_morbidity))
  if (!is.na(dq$missing_geography) && dq$missing_geography > 0) issues <- c(issues, paste("Missing geography:", dq$missing_geography))
  if (!is.na(dq$duplicate_rows) && dq$duplicate_rows > 0) issues <- c(issues, paste("Duplicate rows:", dq$duplicate_rows))
  
  if (length(issues) > 0) {
    cat("⚠️ Data Quality Issues:\n")
    for (issue in issues) cat("  • ", issue, "\n")
  } else cat("✅ No major data quality issues detected\n")
  
  cat("\n📅 Report generated:", Sys.time(), "\n")
}

validate_required_columns <- function(df, required_cols = c("datevisit", "morbidity", "orgunit")) {
  results <- list(valid = TRUE, missing_columns = character(0), empty_columns = character(0), warnings = character(0))
  missing <- setdiff(required_cols, colnames(df))
  if (length(missing) > 0) { results$valid <- FALSE; results$missing_columns <- missing }
  present <- intersect(required_cols, colnames(df))
  for (col in present) {
    nn <- sum(!is.na(df[[col]]))
    if (nn == 0) { results$valid <- FALSE; results$empty_columns <- c(results$empty_columns, col) }
    else if (nn < nrow(df) * 0.5) results$warnings <- c(results$warnings, paste(col, "has >50% missing values"))
  }
  results
}

create_data_backup_info <- function(df, backup_file = NULL) {
  backup_info <- list(
    timestamp = Sys.time(),
    original_columns = colnames(df),
    column_types = sapply(df, class),
    sample_data = if (nrow(df) > 0) head(df, 3) else data.frame(),
    nrows = nrow(df),
    ncols = ncol(df)
  )
  if (!is.null(backup_file)) { saveRDS(backup_info, backup_file); log_info("💾 Backup info saved to: {backup_file}") }
  backup_info
}

# Success message
log_info("📦 preprocessing.R loaded successfully (v4.5.0 - ENHANCED with better error handling)")