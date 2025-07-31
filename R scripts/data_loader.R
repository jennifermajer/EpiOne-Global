# R/data_loader.R
# Main data loading and caching functionality

# Load required libraries
suppressPackageStartupMessages({
  library(config)
  library(logger)
  library(here)
})

# Source utilities
source(here("R", "utils_dhis2_api.R"))

#' Load DHIS2 data with caching support
#' 
#' @param force_refresh Logical. Force refresh from API even if cache exists
#' @param config_env Character. Configuration environment to use
#' @return data.frame. Processed DHIS2 data
#' 
load_dhis2_data <- function(force_refresh = FALSE, config_env = "default") {
  
  # Load configuration
  cfg <- config::get(config = config_env)
  
  # Validate configuration
  validate_config(cfg)
  
  # Set up cache parameters
  use_cache <- cfg$cache$use_cache %||% FALSE
  cache_file <- here(cfg$cache$cache_file)
  cache_expiry_hours <- cfg$cache$cache_expiry_hours %||% 24
  
  # Create data directory if it doesn't exist
  data_dir <- dirname(cache_file)
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
    log_info("Created data directory: {data_dir}")
  }
  
  # Check cache validity
  cache_is_valid <- check_cache_validity(cache_file, cache_expiry_hours)
  
  # Load from cache if valid and not forcing refresh
  if (use_cache && cache_is_valid && !force_refresh) {
    log_info("Loading data from cache: {cache_file}")
    return(load_from_cache(cache_file))
  }
  
  # Fetch fresh data from API
  log_info("Fetching fresh data from DHIS2 API...")
  raw_data <- fetch_dhis2_data(
    base_url = cfg$dhis2$base_url,
    username = cfg$dhis2$username,
    password = cfg$dhis2$password,
    event_date = cfg$parameters$event_date,
    dimensions = cfg$dimensions,
    timeout = cfg$dhis2$timeout %||% 30,
    max_retries = cfg$dhis2$retry_attempts %||% 3
  )
  
  # Convert to data frame
  df <- convert_dhis2_to_dataframe(raw_data)
  
  # Extract metadata for later use
  metadata <- extract_metadata_mapping(raw_data)
  
  # Combine data and metadata
  result <- list(
    data = df,
    metadata = metadata,
    fetch_time = Sys.time()
  )
  
  # Save to cache if caching is enabled
  if (use_cache) {
    save_to_cache(result, cache_file)
  }
  
  return(result)
}

#' Validate configuration object
#' 
#' @param cfg List. Configuration object
#' 
validate_config <- function(cfg) {
  required_fields <- list(
    "dhis2" = c("base_url", "username", "password"),
    "parameters" = c("event_date"),
    "dimensions" = c("organizational_unit")
  )
  
  for (section in names(required_fields)) {
    if (!section %in% names(cfg)) {
      stop(paste("Missing configuration section:", section))
    }
    
    for (field in required_fields[[section]]) {
      if (!field %in% names(cfg[[section]]) || is.null(cfg[[section]][[field]])) {
        stop(paste("Missing required configuration field:", section, "->", field))
      }
    }
  }
  
  # Validate date format
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}_\\d{4}-\\d{2}-\\d{2}$", cfg$parameters$event_date)) {
    stop("event_date must be in format YYYY-MM-DD_YYYY-MM-DD")
  }
  
  log_info("Configuration validation passed")
}

#' Check if cache file is valid and not expired
#' 
#' @param cache_file Character. Path to cache file
#' @param expiry_hours Numeric. Cache expiry time in hours
#' @return Logical. TRUE if cache is valid
#' 
check_cache_validity <- function(cache_file, expiry_hours) {
  if (!file.exists(cache_file)) {
    log_info("Cache file does not exist: {cache_file}")
    return(FALSE)
  }
  
  file_time <- file.mtime(cache_file)
  current_time <- Sys.time()
  age_hours <- as.numeric(difftime(current_time, file_time, units = "hours"))
  
  if (age_hours > expiry_hours) {
    log_info("Cache file expired (age: {round(age_hours, 1)} hours)")
    return(FALSE)
  }
  
  log_info("Cache file is valid (age: {round(age_hours, 1)} hours)")
  return(TRUE)
}

#' Load data from cache file
#' 
#' @param cache_file Character. Path to cache file
#' @return List. Cached data object
#' 
load_from_cache <- function(cache_file) {
  tryCatch({
    data <- readRDS(cache_file)
    log_info("Successfully loaded data from cache")
    return(data)
  }, error = function(e) {
    log_error("Failed to load cache file: {e$message}")
    stop("Failed to load cache file: ", e$message)
  })
}

#' Save data to cache file
#' 
#' @param data List. Data object to cache
#' @param cache_file Character. Path to cache file
#' 
save_to_cache <- function(data, cache_file) {
  tryCatch({
    saveRDS(data, cache_file)
    log_info("Data saved to cache: {cache_file}")
  }, error = function(e) {
    log_error("Failed to save to cache: {e$message}")
    warning("Failed to save data to cache: ", e$message)
  })
}

#' Get data loading summary
#' 
#' @param data List. Loaded data object
#' @return List. Summary information
#' 
get_data_summary <- function(data) {
  if (is.null(data$data)) {
    return(list(error = "No data available"))
  }
  
  df <- data$data
  
  summary <- list(
    rows = nrow(df),
    columns = ncol(df),
    fetch_time = data$fetch_time,
    metadata_items = length(data$metadata),
    column_names = colnames(df)
  )
  
  return(summary)
}