# R/utils_dhis2_api.R
# DHIS2 API utilities for data fetching and processing

# Load required libraries
suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(logger)
})

#' Build DHIS2 Analytics API URL
#' 
#' @param base_url Character. Base URL for DHIS2 instance
#' @param event_date Character. Date range in format "YYYY-MM-DD_YYYY-MM-DD"
#' @param dimensions Character vector. DHIS2 dimension IDs
#' @return Character. Complete API URL
#' 
build_api_url <- function(base_url, event_date, dimensions) {
  # Validate inputs
  if (missing(base_url) || is.null(base_url) || base_url == "") {
    stop("base_url cannot be empty")
  }
  
  if (missing(event_date) || !grepl("^\\d{4}-\\d{2}-\\d{2}_\\d{4}-\\d{2}-\\d{2}$", event_date)) {
    stop("event_date must be in format YYYY-MM-DD_YYYY-MM-DD")
  }
  
  if (missing(dimensions) || length(dimensions) == 0) {
    stop("dimensions cannot be empty")
  }
  
  # Parse date range
  date_parts <- strsplit(event_date, "_")[[1]]
  start_date <- date_parts[1]
  end_date <- date_parts[2]
  
  # Build dimension string
  all_dimensions <- c(dimensions$organizational_unit, 
                     dimensions$program_indicators, 
                     dimensions$other_dimensions)
  dim_str <- paste0("dimension=", all_dimensions, collapse = "&")
  
  # Construct URL
  url <- paste0(
    base_url, 
    "analytics.json?", 
    dim_str, 
    "&startDate=", start_date,
    "&endDate=", end_date
  )
  
  log_info("Built API URL: {substr(url, 1, 100)}...")
  return(url)
}

#' Fetch data from DHIS2 API with retry logic
#' 
#' @param base_url Character. Base URL for DHIS2 instance
#' @param username Character. DHIS2 username
#' @param password Character. DHIS2 password
#' @param event_date Character. Date range in format "YYYY-MM-DD_YYYY-MM-DD"
#' @param dimensions List. DHIS2 dimensions configuration
#' @param timeout Numeric. Request timeout in seconds (default: 30)
#' @param max_retries Numeric. Maximum number of retry attempts (default: 3)
#' @return List. Parsed JSON response from DHIS2 API
#' 
fetch_dhis2_data <- function(base_url, username, password, event_date, 
                            dimensions, timeout = 30, max_retries = 3) {
  
  # Validate credentials
  if (missing(username) || missing(password) || username == "" || password == "") {
    stop("Valid username and password are required")
  }
  
  # Build API URL
  url <- build_api_url(base_url, event_date, dimensions)
  
  log_info("Fetching data from DHIS2 API...")
  
  # Configure request
  response <- NULL
  attempt <- 1
  
  while (attempt <= max_retries && is.null(response)) {
    log_info("Attempt {attempt} of {max_retries}")
    
    tryCatch({
      response <- RETRY(
        "GET", 
        url, 
        authenticate(username, password),
        timeout(timeout),
        times = 1,
        pause_min = 1,
        pause_cap = 5
      )
      
      # Check response status
      if (http_error(response)) {
        error_msg <- sprintf(
          "API request failed with status %d: %s", 
          status_code(response), 
          content(response, "text", encoding = "UTF-8")
        )
        log_error(error_msg)
        
        if (attempt == max_retries) {
          stop(error_msg)
        } else {
          response <- NULL
          attempt <- attempt + 1
          Sys.sleep(2^attempt)  # Exponential backoff
        }
      }
      
    }, error = function(e) {
      log_error("Request failed: {e$message}")
      if (attempt == max_retries) {
        stop(paste("Failed to fetch data after", max_retries, "attempts:", e$message))
      }
      response <<- NULL
      attempt <<- attempt + 1
      Sys.sleep(2^attempt)
    })
  }
  
  # Parse JSON response
  log_info("Parsing JSON response...")
  tryCatch({
    json_data <- content(response, "parsed", simplifyVector = TRUE)
    
    # Validate response structure
    if (is.null(json_data$rows) || is.null(json_data$headers)) {
      warning("Unexpected response structure - missing 'rows' or 'headers'")
    }
    
    log_info("Successfully fetched {length(json_data$rows)} rows of data")
    return(json_data)
    
  }, error = function(e) {
    log_error("Failed to parse JSON response: {e$message}")
    stop("Failed to parse API response: ", e$message)
  })
}

#' Convert DHIS2 API response to data frame
#' 
#' @param json_data List. Parsed JSON response from DHIS2 API
#' @return data.frame. Formatted data frame with proper column names
#' 
convert_dhis2_to_dataframe <- function(json_data) {
  if (is.null(json_data$rows) || is.null(json_data$headers)) {
    stop("Invalid JSON structure - missing 'rows' or 'headers'")
  }
  
  # Extract data components
  rows_data <- json_data$rows
  headers <- json_data$headers$column
  
  # Handle empty data
  if (length(rows_data) == 0) {
    log_warn("No data rows found in API response")
    return(data.frame())
  }
  
  # Convert to data frame
  tryCatch({
    df <- as.data.frame(rows_data, stringsAsFactors = FALSE)
    colnames(df) <- headers
    
    log_info("Created data frame with {nrow(df)} rows and {ncol(df)} columns")
    return(df)
    
  }, error = function(e) {
    log_error("Failed to convert to data frame: {e$message}")
    stop("Failed to convert API response to data frame: ", e$message)
  })
}

#' Extract metadata mapping from DHIS2 response
#' 
#' @param json_data List. Parsed JSON response from DHIS2 API
#' @return Named character vector. Code to name mapping
#' 
extract_metadata_mapping <- function(json_data) {
  if (is.null(json_data$metaData$items)) {
    log_warn("No metadata items found in API response")
    return(character(0))
  }
  
  tryCatch({
    metadata_items <- json_data$metaData$items
    
    # Create code to name mapping
    code_map <- sapply(metadata_items, function(x) {
      if (is.null(x$name)) return(NA_character_)
      return(x$name)
    })
    
    names(code_map) <- sapply(metadata_items, function(x) {
      if (is.null(x$code)) return(NA_character_)
      return(x$code)
    })
    
    # Remove entries with missing codes or names
    code_map <- code_map[!is.na(names(code_map)) & !is.na(code_map)]
    
    log_info("Extracted {length(code_map)} metadata mappings")
    return(code_map)
    
  }, error = function(e) {
    log_error("Failed to extract metadata: {e$message}")
    return(character(0))
  })
}