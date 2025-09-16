# country_data_loader.R - Multi-country data loading system
# Handles different data sources and structures for each country

library(dplyr)
library(yaml)
library(here)
library(httr)
library(jsonlite)
library(tidyr)
library(readxl)
library(readr)

#' Load data for a specific country
#' @param country_code Country code (e.g., "syria", "yemen", "south_sudan")
#' @param config_env Configuration environment (default, production, etc.)
#' @return Standardized data frame with unified column structure
load_country_data <- function(country_code, config_env = "default") {
  
  # Set active country for taxonomy mapping
  assign("ACTIVE_COUNTRY", country_code, envir = .GlobalEnv)
  cat("🌍 Set active country context:", country_code, "\n")
  
  # Load country-specific configuration
  country_config_path <- here("config", "countries", paste0(country_code, ".yml"))
  if (!file.exists(country_config_path)) {
    stop("Country configuration not found for: ", country_code)
  }
  
  country_config <- yaml::read_yaml(country_config_path)
  cat("📊 Loading data for:", country_config$country$name, "\n")
  cat("   Source type:", country_config$data_source$type, "\n")
  
  # Route to appropriate loader based on data source type
  raw_data <- switch(country_config$data_source$type,
    "dhis2" = {
      # Check if this is Syria-style event data or Yemen-style aggregate data
      if (!is.null(country_config$data_source$dhis2$data_type) && 
          country_config$data_source$dhis2$data_type == "aggregate") {
        load_dhis2_data_multi(country_config, config_env)
      } else {
        # Syria-style event data - use existing Syria system
        load_dhis2_events(country_config$data_source$dhis2, list(env = config_env), config_env)
      }
    },
    "excel" = load_excel_data(country_config),
    "csv" = load_csv_data(country_config),
    stop("Unsupported data source type: ", country_config$data_source$type)
  )
  
  # Apply preprocessing (including admin0 creation, geography standardization, etc.)
  if (!exists("prepare_register")) {
    source(here("R", "preprocessing.R"))
  }
  
  # Run full preprocessing pipeline which includes admin0 generation
  cat("🔄 Running preprocessing pipeline...\n")
  preprocessed_data <- prepare_register(raw_data, apply_validation = FALSE)
  
  # Apply ICD-11 disease taxonomy mapping
  if (!exists("apply_icd11_taxonomy")) {
    source(here("R", "taxonomy", "icd11_integration.R"))
    source(here("R", "taxonomy", "icd11_disease_mapper.R"))
    source(here("R", "taxonomy", "icd11_taxonomy_mapper.R"))
  }
  
  mapped_data <- apply_icd11_taxonomy(preprocessed_data, country_config)
  
  cat("✅ Loaded", nrow(mapped_data), "records for", country_config$country$name, "\n")
  
  return(mapped_data)
}

#' Load DHIS2 data (handles both event and aggregate)
load_dhis2_data_multi <- function(country_config, config_env) {
  
  dhis2_config <- country_config$data_source$dhis2
  
  # Load main configuration for API credentials
  main_config <- yaml::read_yaml(here("config", "config.yml"))[[config_env]]
  
  if (dhis2_config$data_type == "aggregate") {
    cat("📈 Loading DHIS2 aggregate data...\n")
    return(load_dhis2_aggregate(dhis2_config, main_config, country_config))
  } else {
    cat("📋 Loading DHIS2 event data...\n") 
    return(load_dhis2_events(dhis2_config, main_config))
  }
}

#' Load DHIS2 aggregate data (for aggregate data countries)
load_dhis2_aggregate <- function(dhis2_config, main_config, country_config) {
  
  # Build API URL for aggregate data
  base_url <- dhis2_config$base_url
  
  # Get credentials - check multiple sources in order of preference
  country_code <- tolower(country_config$country$code)
  username <- main_config$data_loaders$countries[[country_code]]$dhis2$username
  password <- main_config$data_loaders$countries[[country_code]]$dhis2$password
  
  # Fallback to main config
  if (is.null(username) || username == "" || is.null(password) || password == "") {
    username <- main_config$dhis2$username
    password <- main_config$dhis2$password
  }
  
  # Fallback to environment variables (from .Renviron)
  if (is.null(username) || username == "" || is.null(password) || password == "") {
    username <- Sys.getenv("DHIS2_USERNAME")
    password <- Sys.getenv("DHIS2_PASSWORD")
    cat("🔑 Using DHIS2 credentials from environment variables\n")
  }
  
  if (is.null(username) || username == "" || is.null(password) || password == "") {
    stop("DHIS2 credentials not found. Please set in config.yml or .Renviron file (DHIS2_USERNAME, DHIS2_PASSWORD)")
  }
  
  # Option 1: Use predefined data elements from config (current approach)
  # Option 2: Discover ALL available data elements from DHIS2 (new approach for complete data)
  
  use_all_data_elements <- TRUE  # Set to TRUE to discover all available diseases (temporary fix)
  
  if (use_all_data_elements) {
    cat("🔍 Discovering all available data elements from DHIS2...\n")
    
    # Get all data elements from DHIS2 metadata API with aggregation info
    metadata_url <- paste0(base_url, "dataElements.json")
    metadata_params <- list(
      fields = "id,name,displayName,domainType,valueType,aggregationType",
      paging = "false",
      filter = "domainType:eq:AGGREGATE"
    )
    
    tryCatch({
      metadata_response <- httr::GET(
        url = metadata_url,
        query = metadata_params,
        httr::authenticate(username, password),
        httr::timeout(30)
      )
      
      if (httr::status_code(metadata_response) == 200) {
        metadata_content <- httr::content(metadata_response, as = "text", encoding = "UTF-8")
        metadata_data <- jsonlite::fromJSON(metadata_content)
        
        if (!is.null(metadata_data$dataElements)) {
          all_elements_df <- metadata_data$dataElements
          cat("📊 Found", nrow(all_elements_df), "total data elements\n")
          
          # Filter for aggregatable data elements first (avoid E7115 error)
          # Check if required columns exist
          required_cols <- c("domainType", "valueType", "aggregationType")
          available_cols <- intersect(required_cols, names(all_elements_df))
          
          if (length(available_cols) == length(required_cols)) {
            aggregatable_elements <- all_elements_df[
              all_elements_df$domainType == "AGGREGATE" & 
              all_elements_df$valueType %in% c("INTEGER", "NUMBER", "INTEGER_POSITIVE", "INTEGER_ZERO_OR_POSITIVE") &
              all_elements_df$aggregationType %in% c("SUM", "AVERAGE", "COUNT"),
            ]
          } else {
            cat("⚠️ Metadata lacks aggregation info, using basic filtering\n")
            # Basic filter: just use AGGREGATE domain type
            if ("domainType" %in% names(all_elements_df)) {
              aggregatable_elements <- all_elements_df[all_elements_df$domainType == "AGGREGATE", ]
            } else {
              # No filtering possible, use all elements (risky but fallback)
              aggregatable_elements <- all_elements_df
            }
          }
          
          cat("📈 Found", nrow(aggregatable_elements), "aggregatable data elements\n")
          
          # Filter for health/disease related elements using flexible patterns
          # Use country config to define filtering patterns if available
          if (!is.null(dhis2_config$data_element_filters)) {
            filter_patterns <- dhis2_config$data_element_filters
          } else {
            # Default health/disease patterns
            filter_patterns <- c("case", "cases", "disease", "morbidity", "illness", 
                               "condition", "infection", "fever", "diarr", "pneumonia", 
                               "malaria", "tuberculosis", "cholera", "measles", "polio",
                               "opd", "treatment", "surveillance")
          }
          
          disease_elements <- aggregatable_elements[
            grepl(paste(filter_patterns, collapse = "|"), 
                  aggregatable_elements$displayName, ignore.case = TRUE),
          ]
          
          cat("🦠 Filtered to", nrow(disease_elements), "health-related aggregatable elements\n")
          
          # Use all discovered disease elements
          if (nrow(disease_elements) == 0) {
            cat("⚠️ No health-related elements found, using all aggregatable elements\n")
            data_elements <- aggregatable_elements$id[1:min(20, nrow(aggregatable_elements))]
          } else {
            data_elements <- disease_elements$id
            cat("📊 Using all", length(data_elements), "health-related elements\n")
          }
          
        } else {
          cat("⚠️ No data elements found in metadata, falling back to config\n")
          data_elements <- NULL
        }
      } else {
        cat("⚠️ Metadata API call failed, falling back to config\n")
        data_elements <- NULL
      }
      
    }, error = function(e) {
      cat("⚠️ Error discovering data elements:", e$message, ", falling back to config\n")
      data_elements <- NULL
    })
  } else {
    data_elements <- NULL
  }
  
  # Fallback: Use predefined data elements from config
  if (is.null(data_elements) || length(data_elements) == 0) {
    cat("📋 Using predefined data elements from config...\n")
    
    # Try dhis2_config first (nested under dhis2 section)
    if (!is.null(dhis2_config$dhis2_mappings$data_elements)) {
      data_elements <- names(dhis2_config$dhis2_mappings$data_elements)
    }
    
    # If not found, get from country_config top level (flexible structure)  
    if (is.null(data_elements) || length(data_elements) == 0) {
      # Use the ID mappings to get the actual DHIS2 IDs for the API call
      if (!is.null(country_config$dhis2_mappings$data_element_ids)) {
        data_elements <- names(country_config$dhis2_mappings$data_element_ids)
      }
    }
    
    if (is.null(data_elements) || length(data_elements) == 0) {
      stop("No data elements configured in dhis2_mappings for ", country_config$country$name)
    }
  }
  
  cat("📊 Requesting data for elements:", paste(data_elements, collapse = ", "), "\n")
  
  # Determine organization unit filter to avoid cross-country contamination
  ou_filter <- get_country_org_unit_filter(dhis2_config, country_config, base_url, username, password)
  
  # Construct analytics API call for aggregate data with age/sex demographics and geography
  params <- list(
    dimension = paste0("dx:", paste(data_elements, collapse = ";")),
    dimension = "pe:LAST_12_MONTHS", 
    dimension = "co",  # Category options for age/sex breakdowns
    dimension = ou_filter,  # Country-specific organization units
    showHierarchy = "false",
    hierarchyMeta = "false", 
    includeMetadataDetails = "true",
    includeNumDen = "true",
    skipRounding = "false",
    completedOnly = "false",
    outputIdScheme = "NAME"
  )
  
  cat("🔗 API URL:", paste0(base_url, dhis2_config$endpoints$analytics), "\n")
  cat("📋 Parameters:", paste(names(params), params, sep = "=", collapse = "&"), "\n")
  
  # Make API call
  tryCatch({
    response <- httr::GET(
      url = paste0(base_url, dhis2_config$endpoints$analytics),
      query = params,
      httr::authenticate(username, password),
      httr::timeout(dhis2_config$timeout),
      httr::add_headers("Accept" = "application/json")
    )
    
    cat("📡 API Response Status:", httr::status_code(response), "\n")
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      
      # Parse JSON response
      data <- tryCatch({
        jsonlite::fromJSON(content)
      }, error = function(e) {
        cat("❌ JSON parsing error:", e$message, "\n")
        cat("📄 Raw response (first 500 chars):", substr(content, 1, 500), "\n")
        stop("Failed to parse DHIS2 response as JSON")
      })
      
      # Display metadata for new country integration
      display_dhis2_metadata(data, dhis2_config)
      
      # Convert DHIS2 analytics response to data frame
      raw_data <- parse_dhis2_aggregate(data, dhis2_config, country_config)
      
      # Apply post-processing filter to remove cross-country contamination
      filtered_data <- filter_country_data(raw_data, country_config)
      return(filtered_data)
      
    } else {
      error_content <- httr::content(response, as = "text", encoding = "UTF-8")
      cat("❌ DHIS2 API Error Response:", error_content, "\n")
      stop("DHIS2 API error: ", httr::status_code(response), " - ", error_content)
    }
    
  }, error = function(e) {
    cat("❌ DHIS2 API call failed:", e$message, "\n")
    stop("Failed to load DHIS2 aggregate data: ", e$message)
  })
}

#' Parse DHIS2 aggregate analytics response
parse_dhis2_aggregate <- function(dhis2_data, dhis2_config, country_config) {
  
  cat("🔍 Parsing DHIS2 analytics response...\n")
  
  # Check if we have valid data structure
  if (is.null(dhis2_data)) {
    warning("DHIS2 data is NULL")
    return(data.frame())
  }
  
  # Extract headers and rows from DHIS2 analytics
  headers <- dhis2_data$headers
  rows <- dhis2_data$rows
  
  if (is.null(headers)) {
    warning("No headers found in DHIS2 response")
    cat("📄 Available data structure:", paste(names(dhis2_data), collapse = ", "), "\n")
    return(data.frame())
  }
  
  if (is.null(rows) || length(rows) == 0) {
    warning("No rows/data returned from DHIS2 analytics")
    return(data.frame())
  }
  
  # Get header names - handle both list and data frame structures
  if (is.data.frame(headers)) {
    header_names <- headers$name
  } else if (is.list(headers) && !is.null(headers$name)) {
    header_names <- headers$name
  } else {
    header_names <- names(headers)
  }
  
  # Expected headers for Yemen analytics with age/sex: dx, pe, co, ou (filtered), value
  # But actual order may vary, so we'll use the headers from the response
  
  cat("📊 Headers found:", paste(header_names, collapse = ", "), "\n")
  cat("📈 Data rows:", length(rows), "\n")
  
  # Debug: Show structure of first few rows
  cat("🔍 Debug - First row structure:\n")
  if (length(rows) > 0) {
    print(str(rows[[1]]))
    cat("🔍 Debug - Row 1 class:", class(rows[[1]]), "\n")
    cat("🔍 Debug - Row 1 length:", length(rows[[1]]), "\n")
  }
  
  # Convert rows to data frame - handle DHIS2 analytics format
  cat("🔍 Rows structure before processing:\n")
  cat("   - is.list(rows):", is.list(rows), "\n")
  cat("   - length(rows):", length(rows), "\n")
  cat("   - class(rows):", class(rows), "\n")
  
  if ((is.list(rows) || is.matrix(rows)) && length(rows) > 0) {
    cat("🚀 Entering row parsing logic with", length(rows), "elements\n")
    tryCatch({
      expected_cols <- length(header_names)
      cat("🎯 Expected columns:", expected_cols, "\n")
      
      # DHIS2 analytics returns rows in different formats
      # Check if we have a matrix (common DHIS2 format) or list structure
      
      if (is.matrix(rows)) {
        cat("✅ Processing DHIS2 matrix data\n")
        # Matrix case: rows is already properly structured as matrix
        matrix_dims <- dim(rows)
        cat("📊 Matrix dimensions:", matrix_dims[1], "rows ×", matrix_dims[2], "columns\n")
        
        if (matrix_dims[2] == expected_cols) {
          cat("✅ Matrix structure matches expected columns\n")
          df <- data.frame(rows, stringsAsFactors = FALSE)
          names(df) <- header_names
        } else {
          cat("❌ Matrix column count doesn't match headers\n")
          df <- data.frame()
        }
        
      } else if (is.list(rows)) {
        cat("✅ Processing DHIS2 list data\n")
        # Check the structure of the first row to understand the format
        first_row <- rows[[1]]
        cat("🔍 First row structure - class:", class(first_row), "length:", length(first_row), "\n")
        cat("🔍 Total rows:", length(rows), "Expected columns:", expected_cols, "\n")
        cat("🔍 Division check:", length(rows), "÷", expected_cols, "=", length(rows) / expected_cols, "\n")
        
        # Handle different possible structures
        if (is.character(first_row) && length(first_row) == expected_cols) {
        # Each row is already an array with the right number of elements
        cat("✅ Rows are character vectors with", expected_cols, "elements each\n")
        df <- data.frame(
          do.call(rbind, lapply(rows, function(x) as.character(x))),
          stringsAsFactors = FALSE
        )
        names(df) <- header_names
        
      } else if (is.list(first_row) && length(first_row) == expected_cols) {
        # Each row is a list with the right number of elements
        cat("✅ Rows are lists with", expected_cols, "elements each\n")
        df <- data.frame(
          do.call(rbind, lapply(rows, function(x) unlist(x))),
          stringsAsFactors = FALSE
        )
        names(df) <- header_names
        
      } else if (is.character(first_row) && length(first_row) == 1) {
        # Data is flattened - each element is a single value
        total_elements <- length(rows)
        if (total_elements %% expected_cols == 0) {
          num_rows <- total_elements / expected_cols
          cat("🔄 Reshaping flattened data:", total_elements, "elements into", num_rows, "rows x", expected_cols, "cols\n")
          
          # Unlist all elements and create matrix
          all_values <- unlist(rows)
          df <- data.frame(
            matrix(all_values, ncol = expected_cols, byrow = TRUE),
            stringsAsFactors = FALSE
          )
          names(df) <- header_names
        } else {
          cat("❌ Cannot reshape flattened data:", total_elements, "elements don't divide evenly by", expected_cols, "columns\n")
          cat("📝 Attempting alternative flattening approach...\n")
          
          # Try alternative: maybe the data is structured differently
          # Sometimes DHIS2 returns arrays within arrays
          all_values <- c()
          for (row in rows) {
            if (is.character(row)) {
              all_values <- c(all_values, row)
            } else if (is.list(row)) {
              all_values <- c(all_values, unlist(row))
            }
          }
          
          if (length(all_values) %% expected_cols == 0) {
            num_rows <- length(all_values) / expected_cols
            cat("✅ Alternative flattening worked:", length(all_values), "elements into", num_rows, "rows\n")
            df <- data.frame(
              matrix(all_values, ncol = expected_cols, byrow = TRUE),
              stringsAsFactors = FALSE
            )
            names(df) <- header_names
          } else {
            cat("❌ Alternative flattening also failed\n")
            df <- data.frame()
          }
        }
        
      } else {
        # Try to convert directly and see what happens
        cat("⚠️ Unexpected row structure, attempting direct conversion\n")
        cat("📝 Sample row content:", paste(first_row, collapse = ", "), "\n")
        
        # Try to bind rows directly
        df_attempt <- tryCatch({
          data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
        }, error = function(e) {
          cat("❌ Direct rbind failed:", e$message, "\n")
          data.frame()
        })
        
        if (nrow(df_attempt) > 0 && ncol(df_attempt) == expected_cols) {
          df <- df_attempt
          names(df) <- header_names
        } else {
          df <- data.frame()
        }
        }
      } else {
        cat("❌ Unsupported data structure for rows\n")
        df <- data.frame()
      }
      
      if (nrow(df) > 0) {
        cat("✅ Successfully parsed", nrow(df), "rows with", ncol(df), "columns\n")
      } else {
        cat("❌ Failed to parse any rows\n")
      }
      
    }, error = function(e) {
      cat("❌ Error parsing rows:", e$message, "\n")
      cat("📝 First few raw rows for debugging:\n")
      if (length(rows) > 0) {
        for (i in 1:min(3, length(rows))) {
          cat("Row", i, ":", paste(rows[[i]], collapse = ", "), "\n")
        }
      }
      df <- data.frame()
    })
  } else {
    warning("Invalid rows structure in DHIS2 response")
    df <- data.frame()
  }
  
  # Map DHIS2 data elements to disease names
  disease_mappings <- NULL
  
  # Try dhis2_config first (nested under dhis2 section)
  if (!is.null(dhis2_config$dhis2_mappings$data_elements)) {
    disease_mappings <- dhis2_config$dhis2_mappings$data_elements
  }
  
  # If not found, get from current country config top level 
  if (is.null(disease_mappings)) {
    if (!is.null(country_config$dhis2_mappings$data_elements)) {
      disease_mappings <- country_config$dhis2_mappings$data_elements
    }
  }
  
  # Look for data element column (before column name standardization)
  data_col <- NULL
  for (col in c("Data", "dx", "dataElement", "Data Element")) {
    if (col %in% names(df)) {
      data_col <- col
      break
    }
  }
  
  # Look for category option column for age/sex demographics
  category_col <- NULL
  for (col in c("Category option", "co", "categoryOption")) {
    if (col %in% names(df)) {
      category_col <- col
      break
    }
  }

  # CRITICAL DIAGNOSTIC: Check if category options are missing entirely
  if (is.null(category_col)) {
    cat("⚠️ CRITICAL: No category option column found in DHIS2 response!\n")
    cat("📊 Available columns:", paste(names(df), collapse = ", "), "\n")
    cat("🔍 This means the DHIS2 aggregate data lacks demographic breakdowns.\n")
    cat("💡 Possible causes:\n")
    cat("   1. Yemen DHIS2 data is aggregated without sex/age breakdowns\n")
    cat("   2. DHIS2 query missing 'dimension=co' parameter\n")
    cat("   3. Yemen dataset doesn't have category combinations configured\n")
    cat("📋 Impact: Sex and age group data will be completely missing\n")
  } else {
    cat("✅ Found category option column:", category_col, "\n")
  }

  # Standardize column names for consistency (only if df has columns)
  if (ncol(df) > 0) {
    names(df) <- gsub("\\s+", "_", tolower(names(df)))
    
    # Update column references after standardization
    if (!is.null(data_col)) {
      data_col <- gsub("\\s+", "_", tolower(data_col))
    }
    if (!is.null(category_col)) {
      category_col <- gsub("\\s+", "_", tolower(category_col))
    }
  }
  
  if (!is.null(data_col) && nrow(df) > 0) {
    cat("🗂️ Mapping diseases using column:", data_col, "\n")
    # Convert list-based mappings to character vector properly
    df$mapped_disease <- sapply(df[[data_col]], function(x) {
      mapping <- disease_mappings[[x]]
      if (is.null(mapping)) {
        NA_character_
      } else {
        as.character(mapping)[1]  # Take first element if vector
      }
    })
    
    # Create morbidity column for compatibility with downstream processing
    # Use mapped disease if available, otherwise use original data element name
    df$morbidity <- ifelse(is.na(df$mapped_disease), df[[data_col]], df$mapped_disease)
    
    # Count successful mappings
    mapped_count <- sum(!is.na(df$mapped_disease))
    total_count <- nrow(df)
    cat("✅ Disease mappings: ", mapped_count, "/", total_count, " successful\n")
    cat("📋 Created morbidity column with", sum(!is.na(df$morbidity)), "entries\n")
    
    # Show unmapped elements
    unmapped <- unique(df[[data_col]][is.na(df$mapped_disease)])
    if (length(unmapped) > 0) {
      cat("⚠️ Unmapped data elements:", paste(unmapped, collapse = ", "), "\n")
    }
  } else {
    if (nrow(df) == 0) {
      cat("⚠️ No data to map diseases for - empty data frame\n")
    } else {
      warning("No data element column found for disease mapping")
    }
    df$mapped_disease <- NA
    df$morbidity <- NA_character_
  }
  
  # Extract age and sex from category options if available
  if (!is.null(category_col) && category_col %in% names(df) && nrow(df) > 0) {
    cat("👥 Processing demographics from column:", category_col, "\n")
    
    # ENHANCED DEBUG: Show sample category option values
    unique_category_values <- unique(df[[category_col]][!is.na(df[[category_col]])])
    cat("🔍 DEBUG: Found", length(unique_category_values), "unique category option values:\n")
    if (length(unique_category_values) > 0) {
      sample_values <- head(unique_category_values, 10)
      for (i in seq_along(sample_values)) {
        cat("    ", i, ":", sample_values[i], "\n")
      }
      if (length(unique_category_values) > 10) {
        cat("    ... and", length(unique_category_values) - 10, "more\n")
      }
      
      # CRITICAL DIAGNOSTIC: Generate Yemen config mappings
      cat("\n🔧 DIAGNOSTIC: Suggested Yemen config mappings:\n")
      cat("     category_options:\n")
      for (val in unique_category_values) {
        # Try to guess if it's sex-related based on content
        if (grepl("male|female|sex", val, ignore.case = TRUE)) {
          suggested_sex <- if (grepl("female", val, ignore.case = TRUE)) "Female" else "Male"
          cat(sprintf("       %s: \"%s\"  # SEX MAPPING\n", val, suggested_sex))
        } else {
          cat(sprintf("       %s: \"[Age Group or Other]\"  # REVIEW NEEDED\n", val))
        }
      }
    } else {
      cat("    ⚠️ No category option values found!\n")
    }
    
    # Initialize age_group and sex columns
    df$age_group <- NA_character_
    df$sex <- NA_character_
    
    # Get category option mappings from current country config
    category_mappings <- country_config$dhis2_mappings$category_options
    
    # ENHANCED DEBUG: Show available mappings
    if (!is.null(category_mappings)) {
      cat("🔍 DEBUG: Available category mappings:\n")
      sex_mappings <- category_mappings[category_mappings %in% c("Male", "Female")]
      if (length(sex_mappings) > 0) {
        for (id in names(sex_mappings)) {
          cat("    ", id, "->", sex_mappings[[id]], "\n")
        }
      } else {
        cat("    ⚠️ No sex mappings found in config!\n")
      }
    } else {
      cat("    ⚠️ No category mappings available in config!\n")
    }
    
    # Process category options for age and sex
    # Handle both simple mappings and composite demographic options
    for (i in seq_len(nrow(df))) {
      category_value <- df[[category_col]][i]

      if (!is.na(category_value) && !is.null(category_value)) {
        # Initialize variables for this iteration
        category_clean <- NULL
        skip_parsing <- FALSE

        # Method 1: Direct ID mapping from config (if available)
        if (!is.null(category_mappings) && category_value %in% names(category_mappings)) {
          mapped_value <- category_mappings[[category_value]]

          if (mapped_value %in% c("Male", "Female")) {
            # Simple sex mapping - assign and skip parsing
            df$sex[i] <- mapped_value
            skip_parsing <- TRUE
          } else if (mapped_value %in% c("0-11m", "1-4y", "5-14y", "15-49y", "50+y", "All ages")) {
            # Simple age group mapping - assign and skip parsing
            df$age_group[i] <- mapped_value
            skip_parsing <- TRUE
          } else {
            # Everything else (composite strings, service types, special categories) - parse it
            category_clean <- trimws(mapped_value)
          }

        # Method 1b: Try to get metadata name for unmapped IDs
        } else if (!is.null(dhis2_data$metaData$items[[category_value]])) {
          metadata_name <- dhis2_data$metaData$items[[category_value]]$name
          cat("🔍 Found unmapped category option:", category_value, "=", metadata_name, "\n")
          category_clean <- trimws(metadata_name)

        } else {
          # Method 2: Use original category value for parsing
          category_clean <- trimws(category_value)
        }

        # Apply parsing logic to composite strings (unless we have a simple mapping)
        if (!skip_parsing && !is.null(category_clean)) {
          
          # Extract sex from the category option name - enhanced patterns
          # Look for sex indicators in various positions and formats
          if (grepl("\\bmale\\b", category_clean, ignore.case = TRUE) && 
              !grepl("\\bfemale\\b", category_clean, ignore.case = TRUE)) {
            df$sex[i] <- "Male"
          } else if (grepl("\\bfemale\\b", category_clean, ignore.case = TRUE)) {
            df$sex[i] <- "Female"
          } else {
            # Check for composite patterns like "0-11m_Male" or "age_sex" combinations
            if (grepl("_Male$|, Male$| Male$", category_clean, ignore.case = TRUE)) {
              df$sex[i] <- "Male"
            } else if (grepl("_Female$|, Female$| Female$", category_clean, ignore.case = TRUE)) {
              df$sex[i] <- "Female"
            }
          }
          
          # Extract age group information
          age_extracted <- FALSE
          
          # Common age patterns in DHIS2 category options (updated for expanded dataset)
          age_patterns <- list(
            "0-11m" = c("0-11\\s*months?", "under\\s*1", "0-11m", "<\\s*1", "0-4.*0-11"),
            "1-4y" = c("1-4\\s*years?", "1-4y", "1\\s*-\\s*4"),
            "5-9y" = c("5-9\\s*years?", "5\\s*-\\s*9", "5-9"),
            "10-14y" = c("10-14\\s*years?", "10\\s*-\\s*14", "10-14"),
            "5-14y" = c("5-14\\s*years?", "5\\s*-\\s*14", "5-14"),
            "15-17y" = c("15-17\\s*years?", "15\\s*-\\s*17", "15-17"),
            "18-49y" = c("18-49\\s*years?", "18\\s*-\\s*49", "18-49"),
            "19-29y" = c("19-29\\s*years?", "19\\s*-\\s*29", "19-29"),
            "15-49y" = c("15-45\\s*years?", "15-49\\s*years?", "15\\s*-\\s*45", "15\\s*-\\s*49", "15-45", "15-49"),
            "50+y" = c(">\\s*45\\s*years?", "45\\s*\\+", ">\\s*45", "≥\\s*45", "≥\\s*50", ">\\s*50", "50\\s*\\+"),
            "18+y" = c("≥\\s*18", ">\\s*18", "18\\s*\\+")
          )
          
          for (age_label in names(age_patterns)) {
            patterns <- age_patterns[[age_label]]
            for (pattern in patterns) {
              if (grepl(pattern, category_clean, ignore.case = TRUE)) {
                df$age_group[i] <- age_label
                age_extracted <- TRUE
                break
              }
            }
            if (age_extracted) break
          }
          
          # If no age pattern matched, handle special categories
          if (!age_extracted) {
            # Check if it's a total/aggregate category
            if (grepl("total|all|default", category_clean, ignore.case = TRUE)) {
              df$age_group[i] <- "All ages"
            } else if (grepl("^(fixed|mobile|static|pregnant|lactating)", category_clean, ignore.case = TRUE)) {
              # Service delivery types or special populations without age info - set as "All ages"
              df$age_group[i] <- "All ages"
              cat("📋 Service/Special category without age:", category_clean, "-> All ages\n")
            } else {
              cat("⚠️ Unmapped category option:", category_value, "->", category_clean, "\n")
              df$age_group[i] <- category_clean  # Use cleaned value instead of original ID
            }
          }
        } # End of parsing logic block
      } # End of if (!skip_parsing && !is.null(category_clean))
    } # End of for loop
    
    # Report on demographic extraction
    age_extracted <- sum(!is.na(df$age_group))
    sex_extracted <- sum(!is.na(df$sex))
    cat("👥 Demographics extracted - Age groups:", age_extracted, "Sex:", sex_extracted, "\n")
    
    if (age_extracted > 0) {
      cat("📊 Age groups found:", paste(unique(df$age_group[!is.na(df$age_group)]), collapse = ", "), "\n")
    }
    if (sex_extracted > 0) {
      cat("👫 Sex categories found:", paste(unique(df$sex[!is.na(df$sex)]), collapse = ", "), "\n")
    }
  }
  
  # Convert value columns to numeric where appropriate
  for (col in names(df)) {
    if (col %in% c("value", "cases", "count", "total")) {
      df[[col]] <- as.numeric(as.character(df[[col]]))
    }
  }
  
  # Create date variables for compatibility with preprocessing
  if (nrow(df) > 0) {
    df <- create_date_variables(df)
  }
  
  # For Yemen: ou contains governorates (admin1), not facilities
  # Create both admin1 and orgunit columns appropriately
  if (!"admin1" %in% names(df)) {
    ou_col <- NULL
    for (col in c("ou", "Organization Unit", "organisationunit")) {
      if (col %in% names(df)) {
        ou_col <- col
        break
      }
    }
    
    if (!is.null(ou_col)) {
      df$admin1 <- df[[ou_col]]  # ou is actually the governorate
      df$orgunit <- df[[ou_col]] # Keep orgunit for compatibility
      cat("🗺️ Created admin1 (governorate) from:", ou_col, "\n")
      cat("📍 Created orgunit (same as admin1 for Yemen aggregate data)\n")
    } else {
      df$admin1 <- "Unknown Governorate"
      df$orgunit <- "Unknown Facility"
      cat("⚠️ No organization unit column found, using defaults\n")
    }
  } else if (!"orgunit" %in% names(df)) {
    # admin1 exists but orgunit doesn't - copy admin1 to orgunit
    df$orgunit <- df$admin1
    cat("📍 Created orgunit column from existing admin1\n")
  }
  
  cat("📋 Final data structure:", paste(names(df), collapse = ", "), "\n")
  
  df
}

#' Create date variables from DHIS2 period data
create_date_variables <- function(df) {
  cat("📅 Creating date variables from DHIS2 periods...\n")
  
  # Look for period column
  period_col <- NULL
  for (col in c("pe", "period", "Period", "time")) {
    if (col %in% names(df)) {
      period_col <- col
      break
    }
  }
  
  if (is.null(period_col)) {
    # Create current date as fallback
    cat("⚠️ No period column found, using current date\n")
    df$datevisit <- Sys.Date()
    df$year <- as.integer(format(Sys.Date(), "%Y"))
    df$month <- format(Sys.Date(), "%Y-%m")
    df$quarter <- paste0(format(Sys.Date(), "%Y"), " Q", ceiling(as.integer(format(Sys.Date(), "%m")) / 3))
    df$month2 <- as.Date(paste0(format(Sys.Date(), "%Y-%m"), "-01"))
    return(df)
  }
  
  # Parse DHIS2 periods (e.g., "2024Q1", "202401", "2024W01", etc.)
  periods <- df[[period_col]]
  df$datevisit <- as.Date(NA)
  df$year <- NA_integer_
  df$month <- NA_character_
  df$quarter <- NA_character_
  df$month2 <- as.Date(NA)
  
  for (i in seq_len(nrow(df))) {
    period <- periods[i]
    if (!is.na(period) && !is.null(period)) {
      parsed_date <- parse_dhis2_period(period)
      if (!is.na(parsed_date)) {
        df$datevisit[i] <- parsed_date
        df$year[i] <- as.integer(format(parsed_date, "%Y"))
        df$month[i] <- format(parsed_date, "%Y-%m")
        qnum <- ceiling(as.integer(format(parsed_date, "%m")) / 3)
        df$quarter[i] <- paste0(format(parsed_date, "%Y"), " Q", qnum)
        df$month2[i] <- as.Date(paste0(format(parsed_date, "%Y-%m"), "-01"))
      }
    }
  }
  
  valid_dates <- sum(!is.na(df$datevisit))
  cat("📊 Successfully created", valid_dates, "date entries\n")
  
  return(df)
}

#' Parse DHIS2 period strings into dates
parse_dhis2_period <- function(period_str) {
  if (is.null(period_str) || is.na(period_str)) return(as.Date(NA))
  
  period_str <- trimws(as.character(period_str))
  
  # Try different DHIS2 period formats
  tryCatch({
    # Yemen format: "Month Year" (e.g., "January 2024", "Jan 2024", "01 2024") 
    if (grepl("^[A-Za-z]+\\s+\\d{4}$|^\\d{1,2}\\s+\\d{4}$", period_str)) {
      parts <- strsplit(period_str, "\\s+")[[1]]
      if (length(parts) == 2) {
        month_part <- parts[1]
        year_part <- parts[2]
        
        # Convert month name/number to numeric
        if (grepl("^\\d+$", month_part)) {
          # Already numeric month
          month_num <- as.integer(month_part)
        } else {
          # Month name - try to parse
          month_names <- c("january", "february", "march", "april", "may", "june",
                          "july", "august", "september", "october", "november", "december")
          month_abbr <- c("jan", "feb", "mar", "apr", "may", "jun",
                         "jul", "aug", "sep", "oct", "nov", "dec")
          
          month_lower <- tolower(month_part)
          month_num <- which(month_names == month_lower)
          if (length(month_num) == 0) {
            month_num <- which(month_abbr == month_lower)
          }
          if (length(month_num) == 0) {
            # Try partial match
            month_num <- which(grepl(paste0("^", month_lower), month_names))
          }
          if (length(month_num) > 0) {
            month_num <- month_num[1]
          } else {
            return(as.Date(NA))  # Couldn't parse month
          }
        }
        
        if (month_num >= 1 && month_num <= 12) {
          return(as.Date(paste0(year_part, "-", sprintf("%02d", month_num), "-15")))
        }
      }
    }
    
    # Monthly: 202401 -> 2024-01-15
    if (grepl("^\\d{6}$", period_str)) {
      year <- substr(period_str, 1, 4)
      month <- substr(period_str, 5, 6)
      return(as.Date(paste0(year, "-", month, "-15")))
    }
    
    # Quarterly: 2024Q1 -> 2024-02-15 (middle of quarter)
    if (grepl("^\\d{4}Q[1-4]$", period_str)) {
      year <- substr(period_str, 1, 4)
      quarter <- as.integer(substr(period_str, 6, 6))
      month <- (quarter - 1) * 3 + 2  # Middle month of quarter
      return(as.Date(paste0(year, "-", sprintf("%02d", month), "-15")))
    }
    
    # Yearly: 2024 -> 2024-07-01 (middle of year)
    if (grepl("^\\d{4}$", period_str)) {
      return(as.Date(paste0(period_str, "-07-01")))
    }
    
    # Weekly: 2024W01 -> First week of 2024
    if (grepl("^\\d{4}W\\d{1,2}$", period_str)) {
      year <- substr(period_str, 1, 4)
      week <- as.integer(substr(period_str, 6, nchar(period_str)))
      # Approximate: week * 7 days from start of year
      start_date <- as.Date(paste0(year, "-01-01"))
      return(start_date + (week - 1) * 7)
    }
    
    # Daily: 20240115 -> 2024-01-15
    if (grepl("^\\d{8}$", period_str)) {
      year <- substr(period_str, 1, 4)
      month <- substr(period_str, 5, 6)
      day <- substr(period_str, 7, 8)
      return(as.Date(paste0(year, "-", month, "-", day)))
    }
    
    # If none match, try standard date parsing
    return(as.Date(period_str))
    
  }, error = function(e) {
    return(as.Date(NA))
  })
}

#' Load DHIS2 event data (for Syria-style data)
load_dhis2_events <- function(dhis2_config, main_config, config_env = "default") {
  cat("📡 Loading DHIS2 event data (Syria system integration)...\n")
  
  # Load DHIS2 utilities directly to avoid circular dependency
  if (!exists("fetch_dhis2_events") &&
      file.exists(here("R", "utils_dhis2_api.R"))) {
    cat("🔧 Loading DHIS2 API utilities...\n")
    source(here("R", "utils_dhis2_api.R"))
  }
  
  # Check for the original Syria function with force_refresh parameter
  if (exists("load_dhis2_data")) {
    cat("📊 Using load_dhis2_data function...\n")
    tryCatch({
      # Call the Syria system function with proper parameters
      # CRITICAL FIX: Force refresh to avoid contaminated cache from other countries
      data_result <- load_dhis2_data(
        force_refresh = TRUE,  # Force fresh data to avoid cross-country contamination
        config_env = config_env
      )
      
      if (!is.null(data_result$register)) {
        cat("✅ Retrieved", nrow(data_result$register), "records from register\n")
        return(data_result$register)
      } else if (!is.null(data_result$data)) {
        cat("✅ Retrieved", nrow(data_result$data), "records from data\n")
        return(data_result$data)
      } else if (is.data.frame(data_result)) {
        cat("✅ Retrieved", nrow(data_result), "records directly\n")
        return(data_result)
      } else {
        cat("⚠️ No data in load_dhis2_data result\n")
        # Continue to next method
        NULL
      }
    }, error = function(e) {
      cat("⚠️ load_dhis2_data failed:", e$message, "\n")
      # Continue to next method
      NULL
    })
  }
  
  if (exists("load_dhis2_register")) {
    cat("📊 Using load_dhis2_register function...\n")
    tryCatch({
      register_data <- load_dhis2_register(config_env = config_env)
      if (!is.null(register_data) && nrow(register_data) > 0) {
        cat("✅ Retrieved", nrow(register_data), "records from register\n")
        return(register_data)
      }
    }, error = function(e) {
      cat("⚠️ load_dhis2_register failed:", e$message, "\n")
      # Continue to next method
    })
  }
  
  if (exists("load_cached_register")) {
    cat("📊 Using cached register as fallback...\n")
    tryCatch({
      cached_result <- load_cached_register(config_env = config_env)
      if (!is.null(cached_result$register)) {
        cat("✅ Retrieved", nrow(cached_result$register), "cached records\n")
        return(cached_result$register)
      }
    }, error = function(e) {
      cat("⚠️ load_cached_register failed:", e$message, "\n")
    })
  }
  
  stop("Could not load Syria DHIS2 event data - all methods failed")
}

#' Check if path is absolute
is_abs_path <- function(path) {
  if (is.null(path) || length(path) == 0) return(FALSE)
  # Check for Windows absolute paths (C:\ or \\) or Unix absolute paths (/)
  grepl("^([A-Za-z]:[\\\\/]|[\\\\/])", path)
}

#' Load Excel data using multi-sheet processing with taxonomy integration
load_excel_data <- function(country_config) {
  
  excel_config <- country_config$data_source$excel
  
  # Resolve file path using here() for proper path resolution from any working directory
  file_path <- excel_config$file_path
  if (!is_abs_path(file_path)) {
    file_path <- here(file_path)
  }
  
  cat("📊 Loading Excel data from:", file_path, "\n")
  
  if (!file.exists(file_path)) {
    stop("Excel file not found: ", file_path)
  }
  
  # Load the excel database loader and transformer if not already available
  if (!exists("load_excel_database")) {
    excel_loader_path <- here("R", "data_loaders", "excel_database_loader.R")
    excel_transformer_path <- here("R", "data_loaders", "excel_data_transformer.R")
    
    if (file.exists(excel_loader_path)) {
      source(excel_loader_path, local = FALSE)
      cat("📦 Loaded Excel database loader functions\n")
    } else {
      stop("Excel database loader not found: ", excel_loader_path)
    }
    
    if (file.exists(excel_transformer_path)) {
      source(excel_transformer_path, local = FALSE)
      cat("📦 Loaded Excel data transformer functions\n")
    } else {
      stop("Excel data transformer not found: ", excel_transformer_path)
    }
  }
  
  # Use the new multi-sheet Excel loader with country configuration
  cat("🔄 Using multi-sheet Excel loader with taxonomy integration...\n")
  raw_data <- load_excel_database(
    file_path = file_path,  # Use the resolved file path 
    country_code = country_config$country$code,
    config_env = "default"
  )
  
  # Apply taxonomy mapping if functions are available
  if (exists("apply_disease_taxonomy")) {
    cat("🏷️ Applying disease taxonomy mapping...\n")
    mapped_data <- apply_disease_taxonomy(raw_data, country_config)
    return(mapped_data)
  }
  
  return(raw_data)
}

#' Convert wide Excel format to long format
convert_wide_to_long <- function(data, country_config) {
  
  structure_config <- country_config$excel_structure
  
  # Identify disease columns using pattern matching
  disease_cols <- names(data)[grepl(structure_config$disease_columns$pattern, names(data))]
  
  if (length(disease_cols) == 0) {
    warning("No disease columns found matching pattern")
    return(data)
  }
  
  # Get non-disease columns (geographic, date, etc.)
  id_cols <- setdiff(names(data), disease_cols)
  
  # Melt from wide to long
  long_data <- data %>%
    tidyr::pivot_longer(
      cols = all_of(disease_cols),
      names_to = "disease_age_sex",
      values_to = "cases"
    ) %>%
    tidyr::separate(
      disease_age_sex, 
      into = c("disease", "age_group", "sex"),
      sep = structure_config$column_parsing$separator
    )
  
  return(long_data)
}

#' Load CSV data
load_csv_data <- function(country_config) {
  
  csv_config <- country_config$data_source$csv
  cat("📄 Loading CSV data from:", csv_config$file_path, "\n")
  
  if (!file.exists(csv_config$file_path)) {
    stop("CSV file not found: ", csv_config$file_path)
  }
  
  return(readr::read_csv(
    csv_config$file_path,
    locale = readr::locale(encoding = csv_config$encoding %||% "UTF-8")
  ))
}

#' Discover DHIS2 metadata for a country
#' @param country_code Country code (e.g., "yemen", "syria")
#' @param config_env Configuration environment 
#' @return List with data elements and organization units
discover_dhis2_metadata <- function(country_code, config_env = "default") {
  
  cat("🔍 Discovering DHIS2 Metadata for", country_code, "\n")
  cat("==========================================\n")
  
  # Load country configuration
  country_config_path <- here("config", "countries", paste0(country_code, ".yml"))
  if (!file.exists(country_config_path)) {
    stop("Country configuration not found for: ", country_code)
  }
  
  country_config <- yaml::read_yaml(country_config_path)
  main_config <- yaml::read_yaml(here("config", "config.yml"))[[config_env]]
  
  # Get DHIS2 config and credentials
  dhis2_config <- country_config$data_source$dhis2
  base_url <- dhis2_config$base_url
  
  # Get credentials using same logic as main loader
  username <- main_config$data_loaders$countries[[country_code]]$dhis2$username
  password <- main_config$data_loaders$countries[[country_code]]$dhis2$password
  
  if (is.null(username) || username == "" || is.null(password) || password == "") {
    username <- main_config$dhis2$username
    password <- main_config$dhis2$password
  }
  
  if (is.null(username) || username == "" || is.null(password) || password == "") {
    username <- Sys.getenv("DHIS2_USERNAME")
    password <- Sys.getenv("DHIS2_PASSWORD")
  }
  
  if (username == "" || password == "") {
    stop("DHIS2 credentials not found for ", country_code)
  }
  
  # Discover data elements
  discover_data_elements <- function() {
    cat("📊 Fetching data elements...\n")
    
    response <- httr::GET(
      url = paste0(base_url, "dataElements.json"),
      query = list(
        fields = "id,name,displayName,shortName,code",
        paging = "false"
      ),
      httr::authenticate(username, password),
      httr::timeout(dhis2_config$timeout %||% 30)
    )
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content)
      
      if (!is.null(data$dataElements)) {
        elements <- data$dataElements
        cat("✅ Found", nrow(elements), "data elements\n")
        
        # Filter for disease-related elements
        disease_keywords <- c("cholera", "malaria", "measles", "diphtheria", "dengue", 
                             "diarrhea", "awd", "chikungunya", "fever", "cases", "morbidity",
                             "acute", "watery", "surveillance", "notifiable")
        
        # Handle missing values in filtering - safer approach
        tryCatch({
          # Replace NA values with empty strings for each column
          safe_name <- ifelse(is.na(elements$name) | is.null(elements$name), "", elements$name)
          safe_display <- ifelse(is.na(elements$displayName) | is.null(elements$displayName), "", elements$displayName) 
          safe_short <- ifelse(is.na(elements$shortName) | is.null(elements$shortName), "", elements$shortName)
          
          # Combine text for searching
          search_text <- tolower(paste(safe_name, safe_display, safe_short, sep = " "))
          
          # Create search pattern
          pattern <- paste(disease_keywords, collapse = "|")
          
          # Find matches
          matches <- grepl(pattern, search_text, ignore.case = TRUE)
          disease_elements <- elements[matches, ]
        }, error = function(e) {
          cat("⚠️ Error in filtering, showing all elements for manual review\n")
          disease_elements <- data.frame()
        })
        
        if (!is.null(disease_elements) && nrow(disease_elements) > 0) {
          cat("\n🦠 Disease-related data elements found:\n")
          cat("=====================================\n")
          
          # Limit to first 20 to avoid overwhelming output
          max_show <- min(20, nrow(disease_elements))
          
          for (i in seq_len(max_show)) {
            tryCatch({
              cat("ID:", as.character(disease_elements$id[i]), "\n")
              cat("Name:", as.character(disease_elements$name[i]), "\n") 
              cat("Display Name:", as.character(disease_elements$displayName[i]), "\n")
              if (!is.na(disease_elements$code[i]) && as.character(disease_elements$code[i]) != "" && !is.null(disease_elements$code[i])) {
                cat("Code:", as.character(disease_elements$code[i]), "\n")
              }
              cat("---\n")
            }, error = function(e) {
              cat("Error displaying element", i, ":", e$message, "\n")
            })
          }
          
          if (nrow(disease_elements) > 20) {
            cat("... and", nrow(disease_elements) - 20, "more disease elements\n")
          }
          
          return(disease_elements)
        } else {
          cat("⚠️ No disease-related data elements found with keywords\n")
          cat("\n📋 First 20 data elements (for manual inspection):\n")
          print(head(elements[, c("id", "name", "displayName")], 20))
          return(elements)
        }
      } else {
        cat("❌ No data elements found in response\n")
        return(NULL)
      }
    } else {
      cat("❌ Failed to fetch data elements. Status:", httr::status_code(response), "\n")
      error_content <- httr::content(response, as = "text")
      cat("Error:", error_content, "\n")
      return(NULL)
    }
  }
  
  # Discover organization units
  discover_org_units <- function() {
    cat("\n🏥 Fetching organization units...\n")
    
    response <- httr::GET(
      url = paste0(base_url, "organisationUnits.json"),
      query = list(
        fields = "id,name,level,shortName",
        paging = "false"
      ),
      httr::authenticate(username, password),
      httr::timeout(dhis2_config$timeout %||% 30)
    )
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content)
      
      if (!is.null(data$organisationUnits)) {
        org_units <- data$organisationUnits
        cat("✅ Found", nrow(org_units), "organization units\n")
        
        # Show units by level
        for (level in sort(unique(org_units$level))) {
          units_at_level <- org_units[org_units$level == level, ]
          cat("\nLevel", level, "(", nrow(units_at_level), "units):\n")
          if (nrow(units_at_level) <= 10) {
            print(units_at_level[, c("id", "name")])
          } else {
            print(head(units_at_level[, c("id", "name")], 5))
            cat("... and", nrow(units_at_level) - 5, "more\n")
          }
        }
        
        return(org_units)
      } else {
        cat("❌ No organization units found\n")
        return(NULL)
      }
    } else {
      cat("❌ Failed to fetch organization units. Status:", httr::status_code(response), "\n")
      return(NULL)
    }
  }
  
  # Run discovery
  tryCatch({
    data_elements <- discover_data_elements()
    org_units <- discover_org_units()
    
    cat("\n💡 Next steps:\n")
    cat("1. Update", paste0(country_code, ".yml"), "with actual data element IDs from above\n")
    cat("2. Verify organization unit level (currently set to", dhis2_config$organisation_unit_level, ")\n")
    cat("3. Test the integration again with load_country_data('", country_code, "')\n")
    
    return(list(
      data_elements = data_elements,
      org_units = org_units,
      country_code = country_code
    ))
    
  }, error = function(e) {
    cat("❌ Error during metadata discovery:", e$message, "\n")
    return(NULL)
  })
}

#' Display DHIS2 metadata and configuration instructions
display_dhis2_metadata <- function(dhis2_data, dhis2_config) {
  cat("\n")
  cat("🔍 DHIS2 METADATA DISCOVERY\n")
  cat(strrep("=", 80), "\n")
  cat("This information helps you configure new countries in the system.\n\n")
  
  # Extract metadata from DHIS2 response
  if (!is.null(dhis2_data$metaData)) {
    metadata <- dhis2_data$metaData
    
    # Data Elements
    if (!is.null(metadata$items)) {
      cat("📊 AVAILABLE DATA ELEMENTS:\n")
      cat(strrep("-", 40), "\n")
      
      # Find data elements from metadata
      data_elements <- list()
      if (!is.null(metadata$dimensions$dx)) {
        for (dx_id in metadata$dimensions$dx) {
          if (!is.null(metadata$items[[dx_id]])) {
            data_elements[[dx_id]] <- metadata$items[[dx_id]]
          }
        }
      }
      
      if (length(data_elements) > 0) {
        for (element_id in names(data_elements)) {
          element <- data_elements[[element_id]]
          if (!is.null(element$name)) {
            cat(sprintf("  • %s: \"%s\"\n", element_id, element$name))
          }
        }
      }
      
      # Category Options (Demographics)
      cat("\n👥 AVAILABLE CATEGORY OPTIONS (Demographics):\n")
      cat(strrep("-", 50), "\n")
      
      if (!is.null(metadata$dimensions$co)) {
        category_options <- metadata$dimensions$co
        for (co_id in category_options) {
          if (!is.null(metadata$items[[co_id]])) {
            co_name <- metadata$items[[co_id]]$name
            cat(sprintf("  • %s: \"%s\"\n", co_id, co_name))
          }
        }
      }
      
      # Organization Units (Geography)
      cat("\n🌍 AVAILABLE ORGANIZATION UNITS (Geography):\n")
      cat(strrep("-", 50), "\n")
      
      if (!is.null(metadata$dimensions$ou)) {
        org_units <- metadata$dimensions$ou
        sample_size <- min(10, length(org_units))  # Show first 10
        for (i in seq_len(sample_size)) {
          ou_id <- org_units[i]
          if (!is.null(metadata$items[[ou_id]])) {
            ou_name <- metadata$items[[ou_id]]$name
            cat(sprintf("  • %s: \"%s\"\n", ou_id, ou_name))
          }
        }
        if (length(org_units) > 10) {
          cat(sprintf("  ... and %d more organization units\n", length(org_units) - 10))
        }
      }
    }
  }
  
  cat("\n")
  cat("⚙️  CONFIGURATION INSTRUCTIONS\n")
  cat(strrep("=", 80), "\n")
  cat("To integrate a new country similar to this setup:\n\n")
  
  cat("1. CREATE COUNTRY CONFIG FILE:\n")
  cat("   File: config/countries/[country_name].yml\n\n")
  
  cat("2. ADD DHIS2 MAPPINGS SECTION:\n")
  cat("   dhis2_mappings:\n")
  cat("     data_elements:\n")
  cat("       # Map DHIS2 data element names to standardized disease names\n")
  if (!is.null(dhis2_data$metaData$dimensions$dx)) {
    sample_elements <- head(dhis2_data$metaData$dimensions$dx, 3)
    for (element_id in sample_elements) {
      element <- dhis2_data$metaData$items[[element_id]]
      if (!is.null(element$name)) {
        cat(sprintf("       \"%s\": \"[Standardized Disease Name]\"\n", element$name))
      }
    }
  }
  
  cat("\n     # Category option mappings for demographics\n")
  cat("     category_options:\n")
  if (!is.null(dhis2_data$metaData$dimensions$co)) {
    sample_cos <- head(dhis2_data$metaData$dimensions$co, 5)
    for (co_id in sample_cos) {
      co_item <- dhis2_data$metaData$items[[co_id]]
      if (!is.null(co_item$name)) {
        cat(sprintf("       %s: \"[Age Group or Sex Label]\"\n", co_id))
      }
    }
  }
  
  cat("\n3. SET DATA SOURCE CONFIGURATION:\n")
  cat("   data_source:\n")
  cat("     type: dhis2\n")
  cat("     dhis2:\n")
  cat(sprintf("       base_url: %s\n", dhis2_config$base_url))
  cat("       data_type: aggregate\n")
  cat(sprintf("       organisation_unit_level: %s\n", dhis2_config$organisation_unit_level))
  
  cat("\n4. COPY THIS TEMPLATE TO YOUR NEW COUNTRY CONFIG:\n")
  cat("   Then update the mappings above with the actual values shown.\n")
  
  cat("\n", strrep("=", 80), "\n")
}

#' Get country-specific organization unit filter for DHIS2 queries
#' This prevents pulling data from other countries in multi-country DHIS2 instances
get_country_org_unit_filter <- function(dhis2_config, country_config, base_url, username, password) {
  
  country_name <- country_config$country$name
  country_code <- tolower(country_config$country$code)
  
  cat("🏛️ Determining organization unit filter for", country_name, "\n")
  
  # Check if organization unit root is specified in config
  if (!is.null(dhis2_config$organisation_unit_root)) {
    cat("✅ Using configured organization unit root:", dhis2_config$organisation_unit_root, "\n")
    
    # DEBUG: Let's see if the root itself contains cross-country data
    cat("🔍 DEBUG: Investigating root org unit hierarchy...\n")
    investigate_org_unit_hierarchy(dhis2_config$organisation_unit_root, dhis2_config$organisation_unit_level, base_url, username, password)
    
    return(paste0("ou:", dhis2_config$organisation_unit_root, ";LEVEL-", dhis2_config$organisation_unit_level))
  }
  
  # Check country config for org unit specification
  if (!is.null(country_config$data_source$dhis2$organisation_unit_root)) {
    root_id <- country_config$data_source$dhis2$organisation_unit_root
    cat("✅ Using country config organization unit root:", root_id, "\n")
    
    # Get specific country organization units, not just level-based
    country_ou_filter <- get_country_specific_org_units(root_id, dhis2_config$organisation_unit_level, country_name, base_url, username, password)
    return(country_ou_filter)
  }
  
  # Try to discover the country's organization unit root by querying DHIS2 metadata
  cat("🔍 Discovering country organization unit root...\n")
  
  tryCatch({
    # Query organization units at level 1 (country level)
    ou_response <- httr::GET(
      url = paste0(base_url, "organisationUnits.json"),
      query = list(
        filter = "level:eq:1",
        fields = "id,name,code",
        paging = "false"
      ),
      httr::authenticate(username, password),
      httr::timeout(30)
    )
    
    if (httr::status_code(ou_response) == 200) {
      content <- httr::content(ou_response, as = "text", encoding = "UTF-8")
      ou_data <- jsonlite::fromJSON(content)
      
      if (!is.null(ou_data$organisationUnits) && nrow(ou_data$organisationUnits) > 0) {
        org_units <- ou_data$organisationUnits
        
        # Try to match by country name, code, or organization patterns
        name_matches <- grep(country_name, org_units$name, ignore.case = TRUE)
        code_matches <- grep(country_code, org_units$code, ignore.case = TRUE, value = FALSE)
        
        # For Yemen, also try matching common organizational patterns
        if (country_name == "Yemen") {
          imc_matches <- grep("International Medical Corps|IMC", org_units$name, ignore.case = TRUE)
          if (length(imc_matches) > 0) {
            name_matches <- c(name_matches, imc_matches)
          }
        }
        
        if (length(name_matches) > 0) {
          root_id <- org_units$id[name_matches[1]]
          root_name <- org_units$name[name_matches[1]]
          cat("✅ Found country org unit by name:", root_name, "(", root_id, ")\n")
          
          # Validate that this org unit has children at the target level
          if (validate_org_unit_has_children(root_id, dhis2_config$organisation_unit_level, base_url, username, password)) {
            return(paste0("ou:", root_id, ";LEVEL-", dhis2_config$organisation_unit_level))
          } else {
            cat("⚠️ Org unit has no children at level", dhis2_config$organisation_unit_level, "\n")
          }
        } 
        
        if (length(code_matches) > 0) {
          root_id <- org_units$id[code_matches[1]]
          root_name <- org_units$name[code_matches[1]]
          cat("✅ Found country org unit by code:", root_name, "(", root_id, ")\n")
          
          if (validate_org_unit_has_children(root_id, dhis2_config$organisation_unit_level, base_url, username, password)) {
            return(paste0("ou:", root_id, ";LEVEL-", dhis2_config$organisation_unit_level))
          } else {
            cat("⚠️ Org unit has no children at level", dhis2_config$organisation_unit_level, "\n")
          }
        }
        
        # Try to find any org unit that has children at the target level
        cat("🔍 Searching for org units with children at level", dhis2_config$organisation_unit_level, "\n")
        for (i in 1:nrow(org_units)) {
          root_id <- org_units$id[i]
          root_name <- org_units$name[i]
          
          if (validate_org_unit_has_children(root_id, dhis2_config$organisation_unit_level, base_url, username, password)) {
            cat("✅ Found valid org unit:", root_name, "(", root_id, ")\n")
            return(paste0("ou:", root_id, ";LEVEL-", dhis2_config$organisation_unit_level))
          }
        }
        
        # Last resort - show available units and ask user to configure manually
        cat("⚠️ Available organization units:\n")
        for (i in 1:min(10, nrow(org_units))) {
          cat(sprintf("   - %s (%s)\n", org_units$name[i], org_units$id[i]))
        }
        cat("❌ Could not find valid organization unit for", country_name, "\n")
        cat("💡 Please add 'organisation_unit_root: \"CORRECT_ID\"' to your Yemen config\n")
        stop("No valid organization unit found for ", country_name)
      }
    }
    
    cat("⚠️ Could not discover organization units, falling back to level filter\n")
    return(paste0("ou:LEVEL-", dhis2_config$organisation_unit_level))
    
  }, error = function(e) {
    cat("❌ Error discovering organization units:", e$message, "\n")
    cat("⚠️ Falling back to level-based filter (may include other countries)\n")
    return(paste0("ou:LEVEL-", dhis2_config$organisation_unit_level))
  })
}

#' Filter out cross-country data contamination in post-processing
#' This removes any South Sudan (SS) data that may have leaked through
filter_country_data <- function(data, country_config) {
  
  if (is.null(data) || nrow(data) == 0) {
    return(data)
  }
  
  country_name <- country_config$country$name
  country_code <- country_config$country$code
  
  cat("🔍 Applying post-processing filter for", country_name, "\n")
  cat("📊 Input data:", nrow(data), "rows\n")
  
  # CRITICAL FIX: DHIS2 org unit hierarchy contains cross-country contamination
  # We need to filter out orgunits that don't belong to this country
  cat("🔧 DHIS2 contains mixed data - applying country-specific filtering\n")
  
  if (!"orgunit" %in% names(data)) {
    cat("⚠️ No orgunit column found - cannot filter by geography\n")
    return(data)
  }
  
  # Define country-specific orgunit patterns
  country_patterns <- list(
    "Yemen" = c("Sana'a", "Aden", "Taiz", "Hodeidah", "Ibb", "Dhamar", "Al Hudaydah", 
                "Lahj", "Al Dhale'e", "Dhale", "Abyan", "Shabwah", "Hadramout", 
                "Al Mahwit", "Raymah", "Sa'ada", "Al Jawf", "Ma'rib", "Socotra"),
    "Syria" = c("Aleppo", "Damascus", "Homs", "Hama", "Idlib", "Daraa", "Deir", 
                "Raqqa", "Hasakah", "Dahuk", "NES"),
    "South Sudan" = c("Upper Nile", "Unity", "Jonglei", "Central Equatorial", 
                      "Western Equatorial", "Eastern Equatorial", "Northern Bahr", 
                      "Western Bahr", "Lakes", "Warrap", "Western Bar Gazel"),
    "Somalia" = c("Galmudug", "South West", "Puntland", "Hirshabelle")
  )
  
  if (country_name %in% names(country_patterns)) {
    country_pattern <- paste(country_patterns[[country_name]], collapse = "|")
    
    # Filter to keep only orgunits matching this country's patterns
    original_count <- nrow(data)
    data <- data[grepl(country_pattern, data$orgunit, ignore.case = TRUE), ]
    filtered_count <- nrow(data)
    
    cat("✅ Filtered from", original_count, "to", filtered_count, "rows\n")
    cat("📋 Removed", original_count - filtered_count, "cross-country contaminated records\n")
    
    # Show sample of remaining orgunits
    if (filtered_count > 0) {
      sample_orgunits <- head(unique(data$orgunit), 5)
      cat("📍 Sample remaining orgunits:", paste(sample_orgunits, collapse = ", "), "\n")
    }
  } else {
    cat("⚠️ Unknown country pattern for", country_name, "- returning unfiltered data\n")
  }
  
  return(data)
}

#' Debug function to investigate organization unit hierarchy
investigate_org_unit_hierarchy <- function(root_id, target_level, base_url, username, password) {
  
  tryCatch({
    cat("📊 Checking what org units exist under root:", root_id, "at level:", target_level, "\n")
    
    # First, get info about the root itself
    root_response <- httr::GET(
      url = paste0(base_url, "organisationUnits/", root_id, ".json"),
      query = list(fields = "id,name,level,path"),
      httr::authenticate(username, password),
      httr::timeout(10)
    )
    
    if (httr::status_code(root_response) == 200) {
      root_content <- httr::content(root_response, as = "text", encoding = "UTF-8")
      root_data <- jsonlite::fromJSON(root_content)
      cat("🏛️ Root org unit:", root_data$name, "- Level:", root_data$level, "\n")
      cat("📍 Path:", root_data$path, "\n")
    }
    
    # Now get children at target level 
    children_response <- httr::GET(
      url = paste0(base_url, "organisationUnits.json"),
      query = list(
        filter = c(paste0("path:like:", root_id), paste0("level:eq:", target_level)),
        fields = "id,name,code,path",
        paging = "false"
      ),
      httr::authenticate(username, password),
      httr::timeout(15)
    )
    
    if (httr::status_code(children_response) == 200) {
      children_content <- httr::content(children_response, as = "text", encoding = "UTF-8")
      children_data <- jsonlite::fromJSON(children_content)
      
      if (!is.null(children_data$organisationUnits) && nrow(children_data$organisationUnits) > 0) {
        org_units <- children_data$organisationUnits
        cat("📊 Found", nrow(org_units), "org units at level", target_level, ":\n")
        
        # Look for suspicious ones
        for (i in 1:min(10, nrow(org_units))) {
          unit <- org_units[i,]
          is_suspicious <- grepl("NES|Syria|Syrian|North East|SS|South Sudan", unit$name, ignore.case = TRUE)
          marker <- if(is_suspicious) "⚠️ SUSPICIOUS:" else "✅"
          cat(sprintf("   %s %s (%s)\n", marker, unit$name, unit$id))
        }
        
        if (nrow(org_units) > 10) {
          cat("   ... and", nrow(org_units) - 10, "more\n")
        }
      } else {
        cat("❌ No org units found at level", target_level, "\n")
      }
    } else {
      cat("❌ Failed to get children, status:", httr::status_code(children_response), "\n")
    }
    
  }, error = function(e) {
    cat("❌ Debug investigation failed:", e$message, "\n")
  })
}

#' Get country-specific organization units by filtering for country code/name patterns
get_country_specific_org_units <- function(root_id, target_level, country_name, base_url, username, password) {
  
  cat("🔍 Finding", country_name, "specific organization units under root", root_id, "\n")
  
  tryCatch({
    # Query for all organization units under this root at the target level
    # DHIS2 requires multiple filters to be passed correctly
    ou_response <- httr::GET(
      url = paste0(base_url, "organisationUnits.json"),
      query = list(
        filter = c(paste0("path:like:", root_id), paste0("level:eq:", target_level)),
        fields = "id,name,code",
        paging = "false"
      ),
      httr::authenticate(username, password),
      httr::timeout(30)
    )
    
    if (httr::status_code(ou_response) == 200) {
      content <- httr::content(ou_response, as = "text", encoding = "UTF-8")
      ou_data <- jsonlite::fromJSON(content)
      
      if (!is.null(ou_data$organisationUnits) && nrow(ou_data$organisationUnits) > 0) {
        org_units <- ou_data$organisationUnits
        cat("📊 Found", nrow(org_units), "organization units at level", target_level, "\n")
        
        # Trust the DHIS2 hierarchy - if we queried under the correct root, 
        # all returned units should belong to that country
        cat("✅ Found", nrow(org_units), "organization units under country root:\n")
        for (i in 1:min(5, nrow(org_units))) {
          cat(sprintf("   - %s (%s)\n", org_units$name[i], org_units$id[i]))
        }
        if (nrow(org_units) > 5) {
          cat("   ... and", nrow(org_units) - 5, "more\n")
        }
        
        # Return filter with all org unit IDs found under the country root
        # This eliminates the need for pattern matching and is maintainable
        ou_ids <- paste(org_units$id, collapse = ";")
        return(paste0("ou:", ou_ids))
      } else {
        cat("⚠️ No organization units found at level", target_level, "\n")
        return(paste0("ou:", root_id, ";LEVEL-", target_level))
      }
    } else {
      cat("❌ Failed to query organization units:", httr::status_code(ou_response), "\n")
      return(paste0("ou:", root_id, ";LEVEL-", target_level))
    }
    
  }, error = function(e) {
    cat("❌ Error querying country-specific org units:", e$message, "\n")
    return(paste0("ou:", root_id, ";LEVEL-", target_level))
  })
}

#' Validate that an organization unit has children at the specified level
validate_org_unit_has_children <- function(root_id, target_level, base_url, username, password) {
  
  tryCatch({
    # Query for organization units under this root at the target level
    ou_response <- httr::GET(
      url = paste0(base_url, "organisationUnits.json"),
      query = list(
        filter = paste0("path:like:", root_id),
        filter = paste0("level:eq:", target_level),
        fields = "id",
        paging = "false"
      ),
      httr::authenticate(username, password),
      httr::timeout(15)
    )
    
    if (httr::status_code(ou_response) == 200) {
      content <- httr::content(ou_response, as = "text", encoding = "UTF-8")
      ou_data <- jsonlite::fromJSON(content)
      
      if (!is.null(ou_data$organisationUnits) && nrow(ou_data$organisationUnits) > 0) {
        return(TRUE)
      }
    }
    
    return(FALSE)
    
  }, error = function(e) {
    return(FALSE)
  })
}

#' Investigate org unit hierarchy to debug cross-country contamination
investigate_org_unit_hierarchy <- function(root_id, level, base_url, username, password) {
  cat("🔍 Investigating org unit hierarchy for root:", root_id, "at level:", level, "\n")
  
  tryCatch({
    # Get children of the root organization unit
    ou_response <- httr::GET(
      url = paste0(base_url, "organisationUnits/", root_id, ".json"),
      query = list(
        fields = "id,name,level,children[id,name,level]",
        includeDescendants = "true"
      ),
      httr::authenticate(username, password),
      httr::timeout(30)
    )
    
    if (httr::status_code(ou_response) == 200) {
      content <- httr::content(ou_response, as = "text", encoding = "UTF-8")
      ou_data <- jsonlite::fromJSON(content, flatten = TRUE)
      
      cat("📊 Root org unit:", ou_data$name, "(Level:", ou_data$level, ")\n")
      
      if (!is.null(ou_data$children)) {
        children <- ou_data$children
        if (nrow(children) > 0) {
          cat("📋 Children org units found:", nrow(children), "\n")
          # Sample a few children to check for cross-contamination
          sample_children <- head(children, 5)
          for (i in seq_len(nrow(sample_children))) {
            child <- sample_children[i, ]
            cat("  -", child$name, "(ID:", child$id, ", Level:", child$level, ")\n")
          }
        }
      }
    } else {
      cat("❌ Failed to get org unit hierarchy, status:", httr::status_code(ou_response), "\n")
    }
  }, error = function(e) {
    cat("❌ Error investigating org unit hierarchy:", e$message, "\n")
  })
}

#' Get country-specific organization units to prevent cross-contamination
get_country_specific_org_units <- function(root_id, target_level, country_name, base_url, username, password) {
  cat("🏛️ Getting country-specific org units for:", country_name, "\n")
  
  tryCatch({
    # Query for organization units at the target level under the specified root
    ou_response <- httr::GET(
      url = paste0(base_url, "organisationUnits.json"),
      query = list(
        filter = paste0("level:eq:", target_level),
        filter = paste0("path:like:", root_id),
        fields = "id,name,path",
        paging = "false"
      ),
      httr::authenticate(username, password),
      httr::timeout(30)
    )
    
    if (httr::status_code(ou_response) == 200) {
      content <- httr::content(ou_response, as = "text", encoding = "UTF-8")
      ou_data <- jsonlite::fromJSON(content)
      
      if (!is.null(ou_data$organisationUnits) && nrow(ou_data$organisationUnits) > 0) {
        org_units <- ou_data$organisationUnits
        cat("✅ Found", nrow(org_units), "organization units for", country_name, "\n")
        
        # Show sample of org units for verification
        sample_ous <- head(org_units$name, 5)
        cat("📋 Sample org units:", paste(sample_ous, collapse = ", "), "\n")
        
        # Create filter using specific org unit IDs instead of level-based approach
        ou_ids <- org_units$id
        ou_filter <- paste0("ou:", paste(ou_ids, collapse = ";"))
        
        cat("🎯 Created specific org unit filter with", length(ou_ids), "units\n")
        return(ou_filter)
      } else {
        cat("⚠️ No organization units found, falling back to level-based filter\n")
        return(paste0("ou:", root_id, ";LEVEL-", target_level))
      }
    } else {
      cat("❌ Failed to get org units, status:", httr::status_code(ou_response), "\n")
      cat("⚠️ Falling back to level-based filter\n")
      return(paste0("ou:", root_id, ";LEVEL-", target_level))
    }
  }, error = function(e) {
    cat("❌ Error getting country-specific org units:", e$message, "\n")
    cat("⚠️ Falling back to level-based filter\n")
    return(paste0("ou:", root_id, ";LEVEL-", target_level))
  })
}

cat("✅ Country data loader functions loaded\n")