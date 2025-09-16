# R/data_loader.R - PERFORMANCE-OPTIMIZED Data Loader
# FIXES APPLIED:
# 1. Fixed join_url function that was missing
# 2. Fixed authenticate function import
# 3. Removed config package usage entirely
# 4. Fixed config loading logic

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(lubridate)
  library(logger)
  library(httr)
  library(jsonlite)
  library(fs)
})

# ─────────────────────────────────────────────────────────────────────────────
# Unified Data Loading Interface
# ─────────────────────────────────────────────────────────────────────────────

#' Unified data loading function for all country contexts
#' @param country_code Country code or "default" for legacy Syria system
#' @param config_env Configuration environment
#' @param force_refresh Whether to force refresh data
#' @return Data frame with standardized structure
load_unified_data <- function(country_code = "default",
                             config_env = "default",
                             force_refresh = FALSE) {

  if (country_code == "default" || country_code == "syria") {
    # Use existing Syria system
    return(load_dhis2_data(force_refresh = force_refresh, config_env = config_env))
  } else {
    # Use country-specific loader
    if (file.exists(here("R", "data_loaders", "country_data_loader.R"))) {
      source(here("R", "data_loaders", "country_data_loader.R"))
      return(load_country_data(country_code, config_env))
    } else {
      stop("Country data loader not found")
    }
  }
}

# ─────────────────────────────────────────────────────────────────────────────
# Helpers and missing functions
# ─────────────────────────────────────────────────────────────────────────────

if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# CRITICAL FIX: Add missing join_url function
join_url <- function(base_url, endpoint) {
  # Remove trailing slash from base_url
  base_url <- gsub("/$", "", base_url)
  # Remove leading slash from endpoint  
  endpoint <- gsub("^/", "", endpoint)
  # Join with single slash
  paste0(base_url, "/", endpoint)
}

# CRITICAL FIX: Updated safe_load_config to never use config package
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

# CRITICAL FIX: Enhanced standardize_datevisit function
standardize_datevisit <- function(df, tz = "UTC") {
  log_info("🗓️ Standardizing date columns before preprocessing...")
  
  # If datevisit already exists and has valid dates, return as-is
  if ("datevisit" %in% names(df) && sum(!is.na(df$datevisit)) > 0) {
    log_info("✅ datevisit column already exists with valid dates")
    return(df)
  }
  
  # Expanded list of common DHIS2 date column names
  date_candidates <- c(
    "eventdate", "Event date", "occurredat", "Occurred at",
    "visitdate", "date_visit", "consultation_date", "date",
    "DateOfVisit", "EventDate", "OccurredAt", "dateofvisit",
    "eventDate", "occurred_at", "visit_date", "consultationdate"
  )
  
  found_col <- NULL
  
  # Strategy 1: Find the first available date column (exact match)
  for (candidate in date_candidates) {
    if (candidate %in% names(df)) {
      found_col <- candidate
      break
    }
  }
  
  # Strategy 2: Case insensitive search
  if (is.null(found_col)) {
    for (candidate in date_candidates) {
      matches <- which(tolower(names(df)) == tolower(candidate))
      if (length(matches) > 0) {
        found_col <- names(df)[matches[1]]
        break
      }
    }
  }
  
  # Strategy 3: Look for any column with 'date' in the name
  if (is.null(found_col)) {
    date_cols <- names(df)[grepl("date", tolower(names(df)))]
    if (length(date_cols) > 0) {
      found_col <- date_cols[1]
      log_info("📅 Found date column by pattern matching: '{found_col}'")
    }
  }
  
  if (!is.null(found_col)) {
    log_info("📅 Found date column '{found_col}' - creating datevisit")
    
    # Copy the found column to datevisit for consistent processing
    df$datevisit <- df[[found_col]]
    
    # Try to parse dates if they're not already in Date format
    if (!inherits(df$datevisit, "Date")) {
      log_info("🔄 Converting dates to proper Date format")
      
      # Enhanced date conversion with multiple strategies
      converted_dates <- tryCatch({
        if (inherits(df$datevisit, c("POSIXct", "POSIXt"))) {
          as.Date(df$datevisit)
        } else {
          # Try common formats
          char_dates <- as.character(df$datevisit)
          char_dates <- trimws(char_dates)
          
          # Remove empty/NA values
          char_dates[char_dates %in% c("", "NA", "N/A", "null", "NULL")] <- NA_character_
          
          parsed <- suppressWarnings(as.Date(char_dates))
          
          # If that fails, try ymd from lubridate
          if (all(is.na(parsed))) {
            parsed <- suppressWarnings(lubridate::ymd(char_dates))
          }
          
          # If still fails, try dmy
          if (all(is.na(parsed))) {
            parsed <- suppressWarnings(lubridate::dmy(char_dates))
          }
          
          # Try mdy as last resort
          if (all(is.na(parsed))) {
            parsed <- suppressWarnings(lubridate::mdy(char_dates))
          }
          
          # Try parsing as Excel serial dates if numeric
          if (all(is.na(parsed))) {
            numeric_dates <- suppressWarnings(as.numeric(char_dates))
            if (!all(is.na(numeric_dates))) {
              # Excel date range check (common range 1900-2050)
              valid_excel <- numeric_dates >= 25569 & numeric_dates <= 100000
              if (any(valid_excel, na.rm = TRUE)) {
                excel_parsed <- as.Date(numeric_dates, origin = "1899-12-30")
                parsed[valid_excel] <- excel_parsed[valid_excel]
              }
            }
          }
          
          as.Date(parsed)
        }
      }, error = function(e) {
        log_warn("⚠️ Date conversion failed: {e$message}")
        df$datevisit
      })
      
      df$datevisit <- converted_dates
    }
    
    # Report success
    valid_dates <- sum(!is.na(df$datevisit))
    total_dates <- length(df$datevisit)
    success_rate <- round((valid_dates / total_dates) * 100, 1)
    
    log_info("✅ Created datevisit column with {valid_dates}/{total_dates} valid dates ({success_rate}%)")
    
    # Show sample dates for verification
    if (valid_dates > 0) {
      sample_dates <- head(df$datevisit[!is.na(df$datevisit)], 3)
      date_range <- range(df$datevisit, na.rm = TRUE)
      log_info("📅 Sample dates: {paste(sample_dates, collapse = ', ')}")
      log_info("📅 Date range: {date_range[1]} to {date_range[2]}")
    }
    
  } else {
    log_warn("⚠️ No recognizable date column found")
    log_info("Available columns: {paste(names(df), collapse = ', ')}")
    
    # Create empty datevisit column
    df$datevisit <- as.Date(NA)
  }
  
  return(df)
}

# Lazy, safe loader for preprocessing.R (never errors at source time)
safe_require_preprocessing <- function() {
  preproc_path <- here::here("R", "preprocessing.R")
  if (!file.exists(preproc_path)) {
    logger::log_warn("⚠️ preprocessing.R not found at: {preproc_path}")
    return(FALSE)
  }
  ok <- tryCatch({
    source(preproc_path, local = .GlobalEnv)
    TRUE
  }, error = function(e) {
    logger::log_warn("⚠️ Failed to load preprocessing.R: {e$message}")
    FALSE
  })
  ok
}

# --- SAFETY: ensure validator exists before load_dhis2_data() uses it ---
if (!exists("validate_dhis2_config_fast")) {
  validate_dhis2_config_fast <- function(cfg) {
    # Minimal validation so API calls don’t proceed with empty config
    if (is.null(cfg$dhis2$base_url) || !nzchar(cfg$dhis2$base_url)) {
      stop("DHIS2 base URL not configured")
    }
    if (is.null(cfg$parameters$program) || !nzchar(cfg$parameters$program)) {
      stop("Program ID not configured")
    }
    if (is.null(cfg$parameters$stage) || !nzchar(cfg$parameters$stage)) {
      stop("Program stage ID not configured")
    }
    invisible(TRUE)
  }
}

# ---- Defensive shape check for cfg ----
assert_config_shape <- function(cfg) {
  if (is.function(cfg)) stop("Config object is a FUNCTION. Did you forget parentheses when calling get_cached_config_fixed()?")
  
  if (!is.list(cfg)) stop("Config must be a list; got: ", paste(class(cfg), collapse = "/"))
  
  # Helper to check nested field is a list
  must_be_list <- function(x, nm) {
    if (is.null(x)) stop("Config is missing '", nm, "'")
    if (is.function(x)) stop("Config$", nm, " is a FUNCTION (closure), not a list")
    if (!is.list(x)) stop("Config$", nm, " must be a list; got: ", paste(class(x), collapse = "/"))
  }
  
  must_be_list(cfg$dhis2,      "dhis2")
  must_be_list(cfg$parameters, "parameters")
  must_be_list(cfg$cache,      "cache")
  
  # Basic scalars
  req_scalar <- function(val, path) {
    if (is.null(val) || isTRUE(val == "")) stop("Missing config value: ", path)
    if (is.function(val)) stop("Config value ", path, " is a FUNCTION (closure)")
  }
  
  req_scalar(cfg$dhis2$base_url,         "dhis2$base_url")
  req_scalar(cfg$parameters$program,     "parameters$program")
  req_scalar(cfg$parameters$stage,       "parameters$stage")
  req_scalar(cfg$parameters$date_start,  "parameters$date_start")
  req_scalar(cfg$parameters$date_end,    "parameters$date_end")
  
  invisible(TRUE)
}

# PERFORMANCE: Check for utilities first, avoid repeated sourcing
UTILS_LOADED <- FALSE
if (!UTILS_LOADED && file.exists(here("R", "utils_dhis2_api.R"))) {
  source(here("R", "utils_dhis2_api.R"))
  UTILS_LOADED <- TRUE
  if (!exists("fetch_dhis2_events")) {
    log_warn("utils_dhis2_api.R did not define fetch_dhis2_events(); using built-in fallback.")
  }
} else if (!UTILS_LOADED) {
  log_warn("utils_dhis2_api.R not found - using built-in API functions")
}

# MANDATORY PREPROCESSING: Always load preprocessing functions
load_preprocessing_functions <- function() {
  preproc_path <- here::here("R", "preprocessing.R")
  if (file.exists(preproc_path)) {
    source(preproc_path, local = .GlobalEnv)
    log_info("📦 preprocessing.R loaded successfully")
    return(TRUE)
  } else {
    log_error("❌ preprocessing.R not found at: {preproc_path}")
    stop("Preprocessing is mandatory but preprocessing.R file not found. Please ensure preprocessing.R exists in R/ directory.")
  }
}

# Initialize preprocessing at load time - ACTIVATE AS NEEDED
#PREPROCESSING_LOADED <- load_preprocessing_functions()

# Validate that required preprocessing function exists
validate_preprocessing_functions <- function() {
  if (!exists("prepare_register")) {
    stop("Required preprocessing function 'prepare_register' not found. Please ensure it is defined in preprocessing.R")
  }
  log_info("✅ Required preprocessing functions validated")
}

# PERFORMANCE: Fast JSON -> data.frame converter (optimized)
build_events_df_from_analytics <- function(data_json) {
  # CRITICAL: Check if data_json is a function (closure)
  if (is.function(data_json)) {
    stop("data_json parameter is a function (closure), expected parsed JSON data")
  }
  
  log_info("🔍 Building dataframe from analytics data...")
  
  # Accept both top-level and listGrid shapes
  if (!is.null(data_json$listGrid)) {
    log_info("📊 Using listGrid structure")
    headers <- tryCatch({
      data_json$listGrid$headers
    }, error = function(e) {
      stop("Error accessing listGrid$headers: ", e$message)
    })
    rows <- tryCatch({
      data_json$listGrid$rows %||% list()
    }, error = function(e) {
      stop("Error accessing listGrid$rows: ", e$message)
    })
  } else {
    log_info("📊 Using direct structure")
    headers <- tryCatch({
      data_json$headers
    }, error = function(e) {
      stop("Error accessing headers: ", e$message)
    })
    rows <- tryCatch({
      data_json$rows %||% list()
    }, error = function(e) {
      stop("Error accessing rows: ", e$message)
    })
  }
  
  # Extract header names
  if (is.data.frame(headers)) {
    headers_vec <- if ("column" %in% names(headers)) headers$column else headers$name
  } else if (is.list(headers)) {
    headers_vec <- vapply(headers, function(h) h$column %||% h$name %||% "col", character(1))
  } else {
    stop("Could not extract column names from analytics headers.")
  }
  headers_vec <- make.unique(headers_vec %||% "col", sep = "_")
  
  # If no rows at all, return zero-row df with correct columns
  if (!length(rows)) {
    return(as.data.frame(setNames(replicate(length(headers_vec), logical(0), simplify = FALSE),
                                  headers_vec)))
  }
  
  # Build matrix fast
  n_cols <- length(headers_vec)
  n_rows <- length(rows)
  mat <- matrix(NA_character_, nrow = n_rows, ncol = n_cols)
  for (i in seq_len(n_rows)) {
    row_data <- as.character(rows[[i]])
    row_len  <- length(row_data)
    if (row_len >= n_cols) mat[i, ] <- row_data[1:n_cols]
    else { mat[i, 1:row_len] <- row_data }
  }
  
  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df) <- headers_vec
  
  # Optional metadata label expansion
  log_info("🔍 Processing metadata for label expansion...")
  items <- tryCatch({
    # First check if metaData exists and is not a function
    metadata_obj <- data_json$metaData
    if (is.function(metadata_obj)) {
      log_warn("data_json$metaData is a function (closure), trying listGrid")
      metadata_obj <- data_json$listGrid$metaData
      if (is.function(metadata_obj)) {
        log_warn("listGrid$metaData is also a function, skipping metadata expansion")
        return(NULL)
      }
    }
    
    # Now safely access items
    if (!is.null(metadata_obj) && !is.function(metadata_obj)) {
      items_obj <- metadata_obj$items
      if (is.function(items_obj)) {
        log_warn("metadata$items is a function (closure), skipping metadata expansion")
        return(NULL)
      }
      items_obj
    } else {
      NULL
    }
  }, error = function(e) {
    log_warn("Error accessing metadata items: {e$message}")
    NULL
  })
  
  if (!is.null(items) && length(items) > 0) {
    log_info("📝 Applying metadata labels...")
    
    # Additional debugging - check if items itself might be the issue
    if (is.function(items)) {
      log_warn("Items object is a function (closure)! Skipping metadata expansion.")
    } else {
      log_info("✅ Items object is valid, proceeding with label lookup")
      log_info("🔍 Items structure: {class(items)[1]} with {length(items)} entries")
      
      # Check if names(df) is problematic
      df_names <- tryCatch({
        names(df)
      }, error = function(e) {
        log_error("Error accessing names(df): {e$message}")
        character(0)
      })
      
      if (length(df_names) == 0) {
        log_warn("No column names available, skipping metadata expansion")
      } else {
        log_info("📊 Processing {length(df_names)} column names for metadata expansion")
        
        name_lookup <- tryCatch({
          # Use a more explicit loop instead of vapply to better isolate the error
          result <- character(length(df_names))
          for (i in seq_along(df_names)) {
            uid <- df_names[i]
            log_info("🔍 Processing column {i}/{length(df_names)}: {uid}")
            
            it <- tryCatch({
              items[[uid]]
            }, error = function(e) {
              log_warn("Error accessing items[[{uid}]]: {e$message}")
              NULL
            })
            
            if (is.null(it)) {
              result[i] <- uid
            } else if (is.function(it)) {
              log_warn("Item {uid} is a function (closure), using UID as name")
              result[i] <- uid
            } else {
              # Safe access to item properties
              name_val <- tryCatch({
                # Even safer approach - check each property individually
                if (!is.null(it$name) && !is.function(it$name)) {
                  val <- it$name
                } else if (!is.null(it$displayName) && !is.function(it$displayName)) {
                  val <- it$displayName
                } else if (!is.null(it$code) && !is.function(it$code)) {
                  val <- it$code
                } else {
                  val <- uid
                }
                log_info("✅ Successfully got name for {uid}: {val}")
                val
              }, error = function(e) {
                log_warn("Error accessing properties of item {uid}: {e$message}")
                log_warn("Disabling metadata expansion due to errors")
                # Signal to stop processing by throwing a specific error
                stop("METADATA_EXPANSION_FAILED")
              })
              result[i] <- name_val
            }
          }
          result
        }, error = function(e) {
          if (grepl("METADATA_EXPANSION_FAILED", e$message)) {
            log_warn("Metadata expansion failed due to closure issues, using original column names")
          } else {
            log_error("Error during label lookup: {e$message}")
          }
          df_names  # Return original names
        })
        
        names(df) <- make.unique(name_lookup, sep = "_")
      }
    }
  }
  
  # Light date parsing (best-effort)
  log_info("🔍 Starting date parsing...")
  date_candidates <- tryCatch({
    names(df)[grepl("date|time|created|updated", names(df), ignore.case = TRUE)]
  }, error = function(e) {
    log_error("Error getting date candidates: {e$message}")
    character(0)
  })
  
  log_info("📅 Found {length(date_candidates)} date candidate columns")
  
  if (length(date_candidates) > 0) {
    for (i in seq_along(date_candidates)) {
      col <- date_candidates[i]
      log_info("🔍 Processing date column {i}/{length(date_candidates)}: {col}")
      
      tryCatch({
        parsed <- suppressWarnings(lubridate::ymd_hms(df[[col]], quiet = TRUE))
        if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::ymd(df[[col]], quiet = TRUE))
        if (!all(is.na(parsed))) df[[col]] <- parsed
        log_info("✅ Successfully processed date column: {col}")
      }, error = function(e) {
        log_error("Error processing date column {col}: {e$message}")
        # Don't stop processing, just continue with next column
      })
    }
  }
  
  log_info("✅ Date parsing completed, returning dataframe")
  df
}

# ─────────────────────────────────────────────────────────────────────────────
# PERFORMANCE-OPTIMIZED Public API with MANDATORY preprocessing
# ─────────────────────────────────────────────────────────────────────────────

#' PERFORMANCE-OPTIMIZED: Load DHIS2 data with smart caching and MANDATORY preprocessing
#' @param force_refresh Logical. Force refresh from API even if cache exists
#' @param config_env Character. Configuration environment to use
#' @param config_file Character. Path to config file (optional)
#' @param skip_preprocessing Logical. Skip preprocessing (only allowed for cache loads, not API pulls)
#' @return list. Processed DHIS2 event data and metadata
#' @export
load_dhis2_data <- function(force_refresh = FALSE,
                            config_env = "default",
                            config_file = NULL,
                            skip_preprocessing = FALSE) {
  
  start_time <- Sys.time()
  log_info("🚀 Starting OPTIMIZED DHIS2 data loading...")
  
  # CRITICAL FIX: Load config WITHOUT config package
  cfg <- get_cached_config_fixed(config_env, config_file)
  
  # Enhanced debugging for closure error
  if (is.function(cfg)) {
    stop("Configuration object is a function (closure). This indicates get_cached_config_fixed() returned a function instead of configuration data. Please check the config file and loading mechanism.")
  }
  
  assert_config_shape(cfg)  # <--- add this
  
  # PERFORMANCE: Quick cache check first
  cache_file <- get_cache_file_path(cfg)
  use_cache <- cfg$cache$use_cache %||% TRUE
  cache_expiry_hours <- cfg$cache$cache_expiry_hours %||% 24
  
  # PERFORMANCE: Fast cache validation
  if (!force_refresh && use_cache) {
    cache_result <- try_load_cache_fast(cache_file, cache_expiry_hours)
    if (!is.null(cache_result)) {
      load_time <- as.numeric(Sys.time() - start_time, units = "secs")
      log_info("✅ Cache loaded in {round(load_time, 2)} seconds")
      
      # Apply preprocessing to cached data if requested
      if (!skip_preprocessing) {
        log_info("🔄 Applying preprocessing to cached data...")
        cache_result <- apply_preprocessing_to_cached_data(cache_result, cfg)
      }
      
      return(cache_result)
    }
  }
  
  has_preproc <- safe_require_preprocessing()
  if (!skip_preprocessing && !has_preproc) {
    stop("Required preprocessing is not available. Ensure R/preprocessing.R defines prepare_register().", call. = FALSE)
  }
  
  # MANDATORY PREPROCESSING VALIDATION for API pulls
  validate_preprocessing_functions()
  
  # PERFORMANCE: Fast API fetch with optimized processing
  log_info("🌐 Fetching from API (cache miss or forced refresh)...")
  
  tryCatch({
    # Fast config validation
    validate_dhis2_config_fast(cfg)
    
    # PERFORMANCE: Streamlined API call
    api_data <- fetch_dhis2_events_fast(cfg)
    if (is.null(api_data)) stop("API returned NULL")
    
    # CRITICAL: Re-validate cfg before passing to process_api_response_fast
    if (is.function(cfg)) {
      stop("Configuration object is a function (closure) before calling process_api_response_fast. This suggests the cfg variable was corrupted during API fetch.")
    }
    
    # PERFORMANCE: Optimized processing with MANDATORY preprocessing
    processed_data <- process_api_response_fast(api_data, cfg, force_preprocessing = TRUE)
    
    # Save to cache AFTER preprocessing (so cache contains preprocessed data)
    if (use_cache) {
      # Mark as preprocessed in cache metadata
      processed_data$cache_metadata$preprocessing_applied <- TRUE
      processed_data$cache_metadata$preprocessing_timestamp <- Sys.time()
      
      # PERFORMANCE: Save cache (remove async for reliability)
      save_to_cache_fast(processed_data, cache_file)
    }
    
    processed_data$cache_info <- list(
      cache_enabled = use_cache,
      cache_file = cache_file,
      fresh_fetch = TRUE,
      api_fetch_time = Sys.time(),
      config_used = config_env,
      preprocessing_applied = TRUE
    )
    
    load_time <- as.numeric(Sys.time() - start_time, units = "secs")
    log_info("✅ Fresh data loaded and preprocessed in {round(load_time, 2)} seconds")
    return(processed_data)
    
  }, error = function(e) {
    log_error(paste("❌ API fetch failed:", e$message))
    
    # PERFORMANCE: Quick fallback to any cache
    if (use_cache && file.exists(cache_file)) {
      log_info("🆘 Trying stale cache fallback...")
      stale_result <- try_load_cache_fast(cache_file, Inf)  # Accept any age
      if (!is.null(stale_result)) {
        stale_result$cache_info$fallback_used <- TRUE
        stale_result$cache_info$api_error <- e$message
        
        # Apply preprocessing to stale cache if not already applied
        if (!skip_preprocessing) {
          stale_result <- apply_preprocessing_to_cached_data(stale_result, cfg)
        }
        
        log_warn("⚠️ Using stale cache due to API failure")
        return(stale_result)
      }
    }
    
    stop("Failed to load DHIS2 data: ", e$message, call. = FALSE)
  })
}

# Helper function to apply preprocessing to cached data
apply_preprocessing_to_cached_data <- function(cached_data, cfg) {
  has_preproc <- safe_require_preprocessing()
  if (!has_preproc) {
    stop("Required preprocessing is not available. Ensure R/preprocessing.R defines prepare_register().", call. = FALSE)
  }
  validate_preprocessing_functions()
  
  # Check if preprocessing was already applied
  preprocessing_applied <- cached_data$cache_metadata$preprocessing_applied %||% FALSE
  
  if (preprocessing_applied) {
    log_info("✅ Cache already contains preprocessed data")
    return(cached_data)
  }
  
  # Apply preprocessing to cached data
  tryCatch({
    validate_preprocessing_functions()
    
    log_info("🔄 Applying preprocessing to cached data...")
    data_df <- cached_data$data %||% cached_data$register
    
    # CRITICAL: Standardize dates before preprocessing
    data_df <- standardize_datevisit(data_df, tz = "UTC")
    
    metadata <- cached_data$metadata %||% list()
    
    # Apply config fix before preprocessing
    tryCatch({
      if ("package:config" %in% search()) {
        detach("package:config", unload = TRUE, force = TRUE)
        log_info("🔧 Detached config package before preprocessing")
      }
    }, error = function(e) {
      log_debug("Config detach attempt: {e$message}")
    })
    
    preprocessed_data <- prepare_register(
      raw_data = list(data = data_df, metadata = metadata),
      metadata_file = cfg$paths$metadata_file %||% NULL
    )
    
    # Update the cached data with preprocessed version
    cached_data$register <- preprocessed_data
    cached_data$data <- preprocessed_data
    cached_data$cache_metadata$preprocessing_applied <- TRUE
    cached_data$cache_metadata$preprocessing_timestamp <- Sys.time()
    
    log_info("✅ Preprocessing applied to cached data")
    return(cached_data)
    
  }, error = function(e) {
    log_error("❌ Failed to apply preprocessing to cached data: {e$message}")
    
    # Provide detailed error information for debugging
    data_df <- cached_data$data %||% cached_data$register
    if (!is.null(data_df)) {
      log_error("Cached data structure at failure:")
      log_error("- Rows: {nrow(data_df)}")
      log_error("- Columns: {ncol(data_df)}")
      log_error("- Column names: {paste(names(data_df), collapse = ', ')}")
    }
    
    stop("Preprocessing failed on cached data: ", e$message, call. = FALSE)
  })
}

# ─────────────────────────────────────────────────────────────────────────────
# CRITICAL FIX: Configuration loading without config package
# ─────────────────────────────────────────────────────────────────────────────

.config_cache <- new.env()

# CRITICAL FIX: New config loading function that doesn't use config package
get_cached_config_fixed <- function(config_env = "default", config_file = NULL) {
  cache_key <- paste0(config_env, "_", config_file %||% "default")
  
  if (exists(cache_key, envir = .config_cache)) {
    cached_config <- get(cache_key, envir = .config_cache)
    # Ensure we didn't accidentally cache a function
    if (is.function(cached_config)) {
      warning("Cached config is a function, removing from cache and reloading")
      rm(list = cache_key, envir = .config_cache)
    } else {
      return(cached_config)
    }
  }
  
  # Load config without config package
  cfg <- load_configuration_fixed(config_env, config_file)
  
  # Extra validation before caching
  if (is.function(cfg)) {
    stop("load_configuration_fixed returned a function instead of configuration data")
  }
  
  assign(cache_key, cfg, envir = .config_cache)
  cfg
}

# CRITICAL FIX: Load configuration using yaml package instead of config
load_configuration_fixed <- function(config_env = "default", config_file = NULL) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("yaml package required for configuration loading")
  }
  
  if (is.null(config_file)) {
    # PERFORMANCE: Check most likely location first
    config_locations <- c(
      here("config", "config.yml"),
      "config/config.yml",
      here("config.yml"),
      "config.yml"
    )
    config_file <- NULL
    for (location in config_locations) {
      if (file.exists(location)) {
        config_file <- location
        break
      }
    }
    if (is.null(config_file)) {
      # Create minimal default config
      log_warn("⚠️ No configuration file found, using minimal defaults")
      return(create_minimal_config())
    }
  }
  
  if (!file.exists(config_file)) {
    log_warn("⚠️ Configuration file not found: {config_file}, using minimal defaults")
    return(create_minimal_config())
  }
  
  tryCatch({
    # Load YAML directly
    all_configs <- yaml::read_yaml(config_file)
    
    # Extract the specific environment
    cfg <- all_configs[[config_env]]
    if (is.null(cfg)) {
      cfg <- all_configs[["default"]]
      if (is.null(cfg)) {
        log_warn("⚠️ Configuration environment '{config_env}' not found, using minimal defaults")
        return(create_minimal_config())
      }
    }
    
    log_info("📋 Configuration loaded from: {config_file}")
    return(cfg)
    
  }, error = function(e) {
    log_warn("⚠️ Failed to load configuration: {e$message}, using minimal defaults")
    return(create_minimal_config())
  })
}

# CRITICAL FIX: Create minimal default configuration
create_minimal_config <- function() {
  list(
    dhis2 = list(
      base_url = Sys.getenv("DHIS2_BASE_URL", ""),
      endpoint = "api/analytics/events/query",
      username = Sys.getenv("DHIS2_USERNAME", ""),
      password = Sys.getenv("DHIS2_PASSWORD", ""),
      timeout = 300
    ),
    parameters = list(
      program = Sys.getenv("DHIS2_PROGRAM", ""),
      stage = Sys.getenv("DHIS2_STAGE", ""),
      date_start = Sys.getenv("DHIS2_DATE_START", format(Sys.Date() - 365, "%Y-%m-%d")),
      date_end = Sys.getenv("DHIS2_DATE_END", format(Sys.Date(), "%Y-%m-%d")),
      output_type = "EVENT"
    ),
    dimensions = list(
      organizational_unit = strsplit(Sys.getenv("DHIS2_ORG_UNITS", ""), ",")[[1]]
    ),
    cache = list(
      use_cache = TRUE,
      cache_expiry_hours = 24,
      cache_dir = here::here("data"),
      cache_filename = "dhis2_cache.rds"
    ),
    paths = list(
      metadata_file = Sys.getenv("METADATA_FILE", "data/metadata/Org Unit Metadata.xlsx")
    )
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# PERFORMANCE: Fast cache operations (UNCHANGED)
# ─────────────────────────────────────────────────────────────────────────────

get_cache_file_path <- function(cfg, country_code = NULL) {
  cf <- cfg$cache$cache_file
  if (!is.null(cf) && nzchar(cf)) {
    # Replace {country} placeholder with actual country code
    if (!is.null(country_code) && grepl("\\{country\\}", cf)) {
      cf <- gsub("\\{country\\}", country_code, cf)
    }
    if (!grepl("^([A-Za-z]:)?[\\/]", cf)) cf <- here::here(cf)
    return(cf)
  }
  
  cache_dir <- cfg$cache$cache_dir %||% here::here("data")
  # Make cache filename country-specific if country_code is provided
  if (!is.null(country_code)) {
    cache_filename <- paste0("dhis2_cache_", country_code, ".rds")
  } else {
    cache_filename <- cfg$cache$cache_filename %||% "dhis2_cache.rds"
  }
  file.path(cache_dir, cache_filename)
}

try_load_cache_fast <- function(cache_file, expiry_hours) {
  if (!file.exists(cache_file)) return(NULL)
  
  # PERFORMANCE: Quick file age check without full file.info()
  file_age_hours <- as.numeric(difftime(Sys.time(), file.mtime(cache_file), units = "hours"))
  if (file_age_hours > expiry_hours) {
    log_info("💾 Cache expired ({round(file_age_hours, 1)}h old)")
    return(NULL)
  }
  
  # PERFORMANCE: Fast cache loading with error recovery
  tryCatch({
    cached_data <- readRDS(cache_file)
    
    # Quick validation
    if (!is.list(cached_data)) return(NULL)
    data_df <- cached_data$data %||% cached_data$register
    if (is.null(data_df) || !is.data.frame(data_df) || nrow(data_df) == 0) return(NULL)
    
    cached_data$cache_info <- list(
      cache_enabled = TRUE,
      cache_file = cache_file,
      fresh_fetch = FALSE,
      cache_used = TRUE,
      loaded_at = Sys.time(),
      cache_age_hours = round(file_age_hours, 1)
    )
    
    log_info("💾 Cache loaded: {nrow(data_df)} records ({round(file_age_hours, 1)}h old)")
    return(cached_data)
    
  }, error = function(e) {
    log_warn("⚠️ Cache loading failed: {e$message}")
    return(NULL)
  })
}

save_to_cache_fast <- function(data, cache_file) {
  tryCatch({
    # PERFORMANCE: Ensure directory exists
    cache_dir <- dirname(cache_file)
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    
    # Quick validation
    data_df <- data$data %||% data$register
    if (is.null(data_df) || !is.data.frame(data_df) || nrow(data_df) == 0) {
      log_warn("Cannot cache: no valid data")
      return(FALSE)
    }
    
    # Add cache metadata including preprocessing status
    if (is.null(data$cache_metadata)) data$cache_metadata <- list()
    data$cache_metadata$cached_at <- Sys.time()
    data$cache_metadata$records_count <- nrow(data_df)
    data$cache_metadata$cache_version <- "2.1"
    
    # PERFORMANCE: Use compress = FALSE for faster writes
    saveRDS(data, cache_file, compress = FALSE)
    
    if (file.exists(cache_file)) {
      file_size <- round(file.size(cache_file) / 1024 / 1024, 2)
      preprocessing_status <- if (data$cache_metadata$preprocessing_applied %||% FALSE) " (preprocessed)" else ""
      log_info("💾 Cache saved: {file_size} MB{preprocessing_status}")
      return(TRUE)
    }
    return(FALSE)
    
  }, error = function(e) {
    log_warn("Cache save failed: {e$message}")
    return(FALSE)
  })
}

# ─────────────────────────────────────────────────────────────────────────────
# PERFORMANCE: Fast API operations
# ─────────────────────────────────────────────────────────────────────────────

fetch_dhis2_events_fast <- function(cfg) {
  # Enhanced debugging for closure error
  if (is.function(cfg)) {
    stop("Configuration object passed to fetch_dhis2_events_fast is a function (closure). Expected a list with dhis2 configuration.")
  }
  
  assert_config_shape(cfg)  # <--- add this
  if (exists("fetch_dhis2_events")) {
    return(fetch_dhis2_events(cfg))
  }
  
  # Built-in fast fetcher
  base_url <- tryCatch({
    cfg$dhis2$base_url
  }, error = function(e) {
    str_cfg <- capture.output(str(cfg, max.level = 1))
    stop("While reading cfg$dhis2$base_url: ", conditionMessage(e), "\nstr(cfg):\n", paste(str_cfg, collapse="\n"))
  })
  endpoint <- cfg$dhis2$endpoint %||% "api/analytics/events/query"
  program  <- cfg$parameters$program
  if (!nzchar(program)) stop("Program ID not configured")
  
  # Ensure trailing /query and then append /{program}
  endpoint_full <- file.path(endpoint, program)
  url <- join_url(base_url, endpoint_full)
  
  q <- list(
    stage       = cfg$parameters$stage,
    startDate   = cfg$parameters$date_start,
    endDate     = cfg$parameters$date_end,
    outputType  = cfg$parameters$output_type %||% "EVENT",
    paging      = "false",
    dataIdScheme= cfg$parameters$data_id_scheme %||% "UID"
  )
  
  # Add org units
  ous <- cfg$dimensions$organizational_unit
  if (length(ous)) q$ou <- paste(ous, collapse = ";")
  
  # PERFORMANCE: Get credentials once
  auth_user <- Sys.getenv("DHIS2_USERNAME", unset = cfg$dhis2$username %||% "")
  auth_pass <- Sys.getenv("DHIS2_PASSWORD", unset = cfg$dhis2$password %||% "")
  
  if (!nzchar(auth_user) || !nzchar(auth_pass)) {
    stop("DHIS2 credentials not configured")
  }
  
  # PERFORMANCE: Single HTTP request with optimized settings
  resp <- httr::RETRY(
    verb = "GET",
    url = url,
    query = q,
    httr::authenticate(auth_user, auth_pass),  # FIXED: Use httr::authenticate
    httr::timeout(cfg$dhis2$timeout %||% 300),
    times = 2,  # Reduced retries for speed
    terminate_on = c(400, 401, 403, 404, 409)
  )
  
  if (httr::status_code(resp) >= 300) {
    stop(sprintf("DHIS2 API error %s", httr::status_code(resp)))
  }
  
  # PERFORMANCE: Fast JSON parsing
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(txt, simplifyVector = FALSE)  # Faster parsing
  
  list(raw_json = parsed)
}

process_api_response_fast <- function(api_data, cfg, force_preprocessing = FALSE) {
  log_info("🔄 Fast processing API response...")
  
  # CRITICAL: Validate cfg parameter is not a function
  if (is.function(cfg)) {
    stop("Configuration object passed to process_api_response_fast is a function (closure). Expected a list with configuration data.")
  }
  
  log_info("✅ Configuration validation passed - cfg is not a function")
  
  events_data <- NULL
  
  log_info("🔍 Accessing api_data$metadata...")
  metadata <- tryCatch({
    api_data$metadata %||% list()
  }, error = function(e) {
    log_error("Error accessing api_data$metadata: {e$message}")
    list()
  })
  
  # PERFORMANCE: Quick data extraction
  log_info("🔍 Checking data extraction paths...")
  
  if (is.data.frame(api_data$data)) {
    log_info("📊 Using api_data$data (data.frame path)")
    events_data <- api_data$data
  } else if (!is.null(api_data$raw_json)) {
    log_info("📊 Using api_data$raw_json path")
    events_data <- tryCatch({
      result_df <- build_events_df_from_analytics(api_data$raw_json)
      log_info("✅ Successfully built dataframe with {nrow(result_df)} rows and {ncol(result_df)} columns")
      result_df
    }, error = function(e) {
      log_error("Error in build_events_df_from_analytics: {e$message}")
      stop("Failed to build events dataframe: ", e$message)
    })
    
    log_info("🔍 Checking metadata after dataframe creation...")
    if (!length(metadata) && !is.null(api_data$raw_json$metaData)) {
      log_info("🔍 Accessing api_data$raw_json$metaData...")
      metadata <- tryCatch({
        api_data$raw_json$metaData
      }, error = function(e) {
        log_error("Error accessing raw_json$metaData: {e$message}")
        metadata
      })
    }
    log_info("✅ Metadata processing completed for raw_json path")
  } else if (!is.null(api_data$headers) && !is.null(api_data$rows)) {
    log_info("📊 Using headers/rows path")
    events_data <- tryCatch({
      build_events_df_from_analytics(api_data)
    }, error = function(e) {
      log_error("Error in build_events_df_from_analytics (headers/rows): {e$message}")
      stop("Failed to build events dataframe from headers/rows: ", e$message)
    })
    if (is.null(events_data) || !is.data.frame(events_data)) {
      stop("No valid data in API response")
    }
    logger::log_info("📊 Processing {nrow(events_data)} events")
    # Standardize date + preprocessing even for zero rows (no-op where needed)
    events_data <- standardize_datevisit(events_data, tz = "UTC")
    
    if (force_preprocessing || !exists("skip_preprocessing") || !skip_preprocessing) {
      validate_preprocessing_functions()
    }
    if (!length(metadata) && !is.null(api_data$metaData)) {
      metadata <- api_data$metaData
    }
  }
  
  log_info("🔍 Validating events_data after extraction...")
  if (is.null(events_data) || !is.data.frame(events_data) || nrow(events_data) == 0) {
    log_error("❌ Invalid events_data: null={is.null(events_data)}, is_df={is.data.frame(events_data)}, rows={if(is.data.frame(events_data)) nrow(events_data) else 'N/A'}")
    stop("No valid data in API response")
  }
  
  log_info("✅ Events data validation passed")
  log_info("📊 Processing {nrow(events_data)} events with {ncol(events_data)} columns")
  
  # ✅ CRITICAL FIX: Standardize date columns BEFORE preprocessing
  log_info("🔄 Pre-processing date standardization...")
  events_data <- standardize_datevisit(events_data, tz = "UTC")
  
  # MANDATORY PREPROCESSING for API responses
  if (force_preprocessing || !exists("skip_preprocessing") || !skip_preprocessing) {
    validate_preprocessing_functions()
    
    # Apply config fix before preprocessing
    tryCatch({
      if ("package:config" %in% search()) {
        detach("package:config", unload = TRUE, force = TRUE)
        log_info("🔧 Detached config package before preprocessing")
      }
    }, error = function(e) {
      log_debug("Config detach attempt: {e$message}")
    })
    
    log_info("🔄 Applying MANDATORY preprocessing...")
    events_data <- tryCatch({
      # Safe config access for metadata file
      metadata_file <- tryCatch({
        cfg$paths$metadata_file %||% NULL
      }, error = function(e) {
        log_warn("Failed to access cfg$paths$metadata_file: {e$message}")
        NULL
      })
      
      prepare_register(
        raw_data = list(data = events_data, metadata = metadata),
        metadata_file = metadata_file
      )
    }, error = function(e) {
      log_error("❌ MANDATORY preprocessing failed: {e$message}")
      
      # Provide detailed error information
      log_error("Data structure at failure:")
      log_error("- Rows: {nrow(events_data)}")
      log_error("- Columns: {ncol(events_data)}")
      log_error("- Column names: {paste(names(events_data), collapse = ', ')}")
      
      # Show sample data for debugging
      if (nrow(events_data) > 0) {
        sample_data <- head(events_data, 2)
        log_error("Sample data structure:")
        for (col in names(sample_data)) {
          sample_val <- sample_data[[col]][1]
          log_error("  {col}: {class(sample_data[[col]])[1]} - '{sample_val}'")
        }
      }
      
      stop("Preprocessing is required but failed: ", e$message, call. = FALSE)
    })
    
    log_info("✅ Preprocessing completed successfully")
  }
  
  list(
    register = events_data,
    data = events_data,
    metadata = metadata,
    raw_response = api_data,
    processing_info = list(
      processed_at = Sys.time(),
      final_records = nrow(events_data),
      config_env = tryCatch({
        cfg$app$version %||% "unknown"
      }, error = function(e) {
        "unknown"
      }),
      preprocessing_applied = TRUE
    )
  )
}

validate_dhis2_config_fast <- function(cfg) {
  # PERFORMANCE: Minimal validation
  if (is.null(cfg$dhis2$base_url) || !nzchar(cfg$dhis2$base_url)) {
    stop("DHIS2 base URL not configured")
  }
  if (is.null(cfg$parameters$program) || !nzchar(cfg$parameters$program)) {
    stop("Program ID not configured")
  }
  if (is.null(cfg$parameters$stage) || !nzchar(cfg$parameters$stage)) {
    stop("Program stage ID not configured")
  }
}

# ─────────────────────────────────────────────────────────────────────────────
# Main dashboard data loader (optimized wrapper)
# ─────────────────────────────────────────────────────────────────────────────

#' Main dashboard data loader (optimized wrapper with mandatory preprocessing)
#' @param force_refresh Logical. Force refresh from API
#' @param config_env Character. Configuration environment
#' @param skip_preprocessing Logical. Only for cached data, preprocessing always runs for API pulls
#' @export
load_dashboard_data <- function(force_refresh = FALSE, config_env = NULL, skip_preprocessing = FALSE) {
  if (is.null(config_env)) config_env <- detect_environment()
  
  # Override skip_preprocessing for API pulls
  if (force_refresh) {
    skip_preprocessing <- FALSE
    log_info("🔄 Force refresh requested - preprocessing will be applied")
  }
  
  tryCatch({
    data <- load_dhis2_data(
      force_refresh = force_refresh, 
      config_env = config_env,
      skip_preprocessing = skip_preprocessing
    )
    log_info("✅ Dashboard data loaded successfully with preprocessing")
    data
  }, error = function(e) {
    log_error("❌ Dashboard data loading failed: {e$message}")
    stop("Dashboard data loading failed: ", e$message, call. = FALSE)
  })
}

detect_environment <- function() {
  env_var <- Sys.getenv("R_CONFIG_ACTIVE", unset = NA)
  if (!is.na(env_var) && nzchar(env_var)) return(env_var)
  if (Sys.getenv("SHINY_SERVER_VERSION", unset = "") != "") return("production")
  if (interactive()) return("development")
  "default"
}

# PERFORMANCE: Simplified error guidance
provide_error_guidance <- function(error) {
  error_msg <- as.character(error$message)
  if (grepl("401", error_msg)) {
    log_error("💡 Authentication failed - check DHIS2 credentials")
  } else if (grepl("404", error_msg)) {
    log_error("💡 Resource not found - check URLs and IDs")
  } else if (grepl("timeout|connection", error_msg, ignore.case = TRUE)) {
    log_error("💡 Network/timeout error - check connection")
  } else {
    log_error("💡 API Error: {error_msg}")
  }
}

# ─────────────────────────────────────────────────────────────────────────────
# Developer Helper: Reload cached data with fresh preprocessing
# ─────────────────────────────────────────────────────────────────────────────

#' Load cached DHIS2 register and force re-preprocessing
#'
#' Useful for development/testing when you want to test changes in
#' preprocessing.R without re-fetching from the API.
#'
#' @param config_env Character. Configuration environment to use
#' @param config_file Character. Path to config file (optional)
#' @param allow_stale Logical. Allow loading stale cache regardless of expiry
#' @return list containing $register (preprocessed df), $metadata, and $cache_info
#' @export
load_cached_register <- function(config_env = "default",
                                 config_file = NULL,
                                 allow_stale = TRUE) {
  log_info("💾 Loading cached register for environment: {config_env}")
  
  # Apply config fix first
  tryCatch({
    if ("package:config" %in% search()) {
      detach("package:config", unload = TRUE, force = TRUE)
      log_info("🔧 Detached config package before cache processing")
    }
  }, error = function(e) {
    log_debug("Config detach attempt: {e$message}")
  })
  
  # Load config
  cfg <- get_cached_config_fixed(config_env, config_file)
  cache_file <- get_cache_file_path(cfg)
  
  if (!file.exists(cache_file)) {
    stop("Cache file not found at: ", cache_file,
         ". Run load_dhis2_data(force_refresh = TRUE) at least once to create cache.")
  }
  
  # Load cache (ignoring expiry if allow_stale = TRUE)
  expiry_hours <- if (allow_stale) Inf else (cfg$cache$cache_expiry_hours %||% 24)
  cached_data <- try_load_cache_fast(cache_file, expiry_hours)
  
  if (is.null(cached_data)) {
    stop("Could not load cache from: ", cache_file)
  }
  
  has_preproc <- safe_require_preprocessing()
  if (!has_preproc) {
    stop("Required preprocessing is not available. Ensure R/preprocessing.R defines prepare_register().", call. = FALSE)
  }
  validate_preprocessing_functions()
  
  # Always reapply preprocessing (ignore existing preprocessing flag)
  validate_preprocessing_functions()
  
  log_info("🔄 Re-applying preprocessing to cached data...")
  data_df <- cached_data$data %||% cached_data$register
  metadata <- cached_data$metadata %||% list()
  
  # ✅ CRITICAL: Apply date standardization before preprocessing
  log_info("🔄 Pre-processing date standardization for cached data...")
  data_df <- standardize_datevisit(data_df, tz = "UTC")
  
  preprocessed_data <- tryCatch({
    prepare_register(
      raw_data = list(data = data_df, metadata = metadata),
      metadata_file = cfg$paths$metadata_file %||% NULL
    )
  }, error = function(e) {
    log_error("❌ Preprocessing failed on cached data: {e$message}")
    
    # Provide debugging information
    log_error("Cached data structure at failure:")
    log_error("- Rows: {nrow(data_df)}")
    log_error("- Columns: {ncol(data_df)}")
    log_error("- Column names: {paste(names(data_df), collapse = ', ')}")
    
    stop("Preprocessing failed on cached data: ", e$message, call. = FALSE)
  })
  
  cached_data$register <- preprocessed_data
  cached_data$data <- preprocessed_data
  cached_data$cache_metadata$preprocessing_applied <- TRUE
  cached_data$cache_metadata$preprocessing_timestamp <- Sys.time()
  
  log_info("✅ Cached register reloaded and re-preprocessed: {nrow(preprocessed_data)} records")
  return(cached_data)
}

assign(".__DATA_LOADER_SOURCED__", TRUE, envir = .GlobalEnv)
if (isTRUE(getOption("epi.loader.strict", FALSE))) {
  if (!is.function(get0("load_dhis2_data"))) stop("load_dhis2_data not defined after sourcing")
  if (!is.function(get0("load_cached_register"))) stop("load_cached_register not defined after sourcing")
}