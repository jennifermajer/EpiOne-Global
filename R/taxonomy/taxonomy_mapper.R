# taxonomy_mapper.R - Disease taxonomy mapping system
# Handles country-specific disease synonyms and maps to canonical terms

library(dplyr)
library(yaml)
library(here)

#' Apply disease taxonomy mapping to standardized data
#' @param data Standardized data frame with disease column
#' @param country_config Country configuration containing taxonomy settings
#' @return Data frame with canonical disease names
apply_disease_taxonomy <- function(data, country_config) {
  
  if (nrow(data) == 0) {
    warning("No data to process for taxonomy mapping")
    return(data)
  }
  
  cat("🏷️  Applying disease taxonomy mapping...\n")
  
  # Load base taxonomy
  base_taxonomy <- load_base_taxonomy()
  
  # Load country-specific taxonomy
  country_taxonomy <- load_country_taxonomy(country_config$taxonomy$country_file)
  
  # Get disease mappings for this country
  disease_mappings <- get_disease_mappings(country_config, base_taxonomy, country_taxonomy)
  
  # Apply mappings to the data
  mapped_data <- map_diseases_to_canonical(data, disease_mappings, country_config)
  
  # Add priority disease flags
  mapped_data <- add_priority_flags(mapped_data, country_config$taxonomy$priority_diseases)
  
  cat("✅ Taxonomy mapping complete. Mapped", length(unique(mapped_data$canonical_disease)), "canonical diseases\n")
  
  return(mapped_data)
}

#' Load base disease taxonomy
load_base_taxonomy <- function() {
  
  base_path <- here("taxonomy", "base.yaml")
  if (!file.exists(base_path)) {
    stop("Base taxonomy file not found: ", base_path)
  }
  
  base_taxonomy <- yaml::read_yaml(base_path)
  cat("📚 Loaded base taxonomy\n")
  
  return(base_taxonomy)
}

#' Load country-specific taxonomy
load_country_taxonomy <- function(country_file) {
  
  if (is.null(country_file)) {
    cat("ℹ️  No country-specific taxonomy file specified\n")
    return(NULL)
  }
  
  country_path <- here("taxonomy", country_file)
  if (!file.exists(country_path)) {
    warning("Country taxonomy file not found: ", country_path)
    return(NULL)
  }
  
  country_taxonomy <- yaml::read_yaml(country_path)
  cat("🌍 Loaded country taxonomy with", length(country_taxonomy$synonyms), "synonym mappings\n")
  
  return(country_taxonomy)
}

#' Get disease mappings for a specific country
get_disease_mappings <- function(country_config, base_taxonomy, country_taxonomy) {
  
  mappings <- list()
  
  # Start with direct mappings from country config
  if (!is.null(country_config$disease_mappings)) {
    if (!is.null(country_config$disease_mappings$excel_names)) {
      mappings <- c(mappings, country_config$disease_mappings$excel_names)
    }
    if (!is.null(country_config$disease_mappings$dhis2_mappings$data_elements)) {
      mappings <- c(mappings, country_config$disease_mappings$dhis2_mappings$data_elements)
    }
  }
  
  # Add country-specific synonym mappings
  if (!is.null(country_taxonomy$synonyms)) {
    mappings <- c(mappings, country_taxonomy$synonyms)
  }
  
  # Add base canonical names (map to themselves)
  if (!is.null(base_taxonomy$diseases)) {
    canonical_names <- names(base_taxonomy$diseases)
    canonical_mappings <- setNames(canonical_names, canonical_names)
    mappings <- c(mappings, canonical_mappings)
  }
  
  cat("🔗 Created", length(mappings), "disease mappings\n")
  return(mappings)
}

#' Map diseases in data to canonical names
map_diseases_to_canonical <- function(data, disease_mappings, country_config) {
  
  # Identify disease column
  disease_col <- identify_disease_column(data, country_config)
  if (is.null(disease_col)) {
    warning("No disease column found in data")
    data$canonical_disease <- NA
    data$original_disease <- NA
    return(data)
  }
  
  # Store original disease names
  data$original_disease <- data[[disease_col]]
  
  # Apply mappings
  data$canonical_disease <- disease_mappings[data[[disease_col]]]
  
  # Handle unmapped diseases
  unmapped_count <- sum(is.na(data$canonical_disease))
  if (unmapped_count > 0) {
    unmapped_diseases <- unique(data[[disease_col]][is.na(data$canonical_disease)])
    warning("Found ", unmapped_count, " records with unmapped diseases: ", 
            paste(unmapped_diseases, collapse = ", "))
    
    # For unmapped diseases, use original name
    data$canonical_disease[is.na(data$canonical_disease)] <- 
      data$original_disease[is.na(data$canonical_disease)]
  }
  
  return(data)
}

#' Identify the disease column in the data
identify_disease_column <- function(data, country_config) {
  
  # Check mapped disease column from data_mapping
  if (!is.null(country_config$data_mapping$disease_column)) {
    disease_col <- country_config$data_mapping$disease_column
    if (disease_col %in% names(data)) {
      return(disease_col)
    }
  }
  
  # Check for common disease column names
  common_names <- c("disease", "morbidity", "condition", "diagnosis", 
                   "Disease", "Morbidity", "mapped_disease")
  
  for (col_name in common_names) {
    if (col_name %in% names(data)) {
      return(col_name)
    }
  }
  
  return(NULL)
}

#' Add priority disease flags
add_priority_flags <- function(data, priority_diseases) {
  
  if (is.null(priority_diseases) || length(priority_diseases) == 0) {
    data$is_priority_disease <- FALSE
    return(data)
  }
  
  data$is_priority_disease <- data$canonical_disease %in% priority_diseases
  
  priority_count <- sum(data$is_priority_disease, na.rm = TRUE)
  cat("🎯 Flagged", priority_count, "records as priority diseases\n")
  
  return(data)
}

#' Standardize data structure across different countries
standardize_data_structure <- function(data, country_config) {
  
  if (nrow(data) == 0) {
    warning("No data to standardize")
    return(data)
  }
  
  cat("🔄 Standardizing data structure...\n")
  
  # Create standardized column mappings
  mapping <- country_config$data_mapping
  standardized_data <- data
  
  # Standardize date column
  if (!is.null(mapping$date_column) && mapping$date_column %in% names(data)) {
    standardized_data$report_date <- standardize_dates(data[[mapping$date_column]], country_config)
  }
  
  # Standardize geographic columns
  if (!is.null(mapping$geographic)) {
    for (level in names(mapping$geographic)) {
      col_name <- mapping$geographic[[level]]
      if (!is.null(col_name) && col_name %in% names(data)) {
        standardized_data[[paste0("std_", level)]] <- data[[col_name]]
      }
    }
  }
  
  # Standardize demographic columns
  if (!is.null(mapping$demographics)) {
    if (!is.null(mapping$demographics$age_column) && mapping$demographics$age_column %in% names(data)) {
      standardized_data$std_age <- data[[mapping$demographics$age_column]]
    }
    if (!is.null(mapping$demographics$sex_column) && mapping$demographics$sex_column %in% names(data)) {
      standardized_data$std_sex <- standardize_sex_values(data[[mapping$demographics$sex_column]])
    }
  }
  
  # Add case counts (default to 1 if not specified)
  if (!"cases" %in% names(standardized_data)) {
    standardized_data$cases <- 1
  }
  
  cat("✅ Data structure standardized\n")
  return(standardized_data)
}

#' Standardize date formats
standardize_dates <- function(dates, country_config) {
  
  # Handle different date formats based on country configuration
  if (!is.null(country_config$data_source$excel$date_format)) {
    date_format <- country_config$data_source$excel$date_format
    if (date_format == "%B %Y") {
      # Handle "January 2024" format
      dates <- as.Date(paste("01", dates), format = "%d %B %Y")
    }
  } else if (!is.null(country_config$data_source$dhis2)) {
    # Handle DHIS2 period formats (202401)
    if (all(nchar(as.character(dates)) == 6, na.rm = TRUE)) {
      dates <- as.Date(paste0(substr(dates, 1, 4), "-", substr(dates, 5, 6), "-01"))
    }
  }
  
  # Default: try to parse as Date
  if (!inherits(dates, "Date")) {
    dates <- as.Date(dates)
  }
  
  return(dates)
}

#' Standardize sex/gender values
standardize_sex_values <- function(sex_values) {
  
  sex_mapping <- c(
    "M" = "Male", "m" = "Male", "male" = "Male", "MALE" = "Male",
    "F" = "Female", "f" = "Female", "female" = "Female", "FEMALE" = "Female",
    "Male" = "Male", "Female" = "Female"
  )
  
  standardized <- sex_mapping[as.character(sex_values)]
  standardized[is.na(standardized)] <- as.character(sex_values[is.na(standardized)])
  
  return(standardized)
}

cat("✅ Disease taxonomy mapper functions loaded\n")