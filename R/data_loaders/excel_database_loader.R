# excel_database_loader.R - Flexible Excel Database Loader for Country Morbidity Data
# Automatically detects structure and processes various Excel database formats

library(readxl)
library(dplyr)
library(tidyr)
library(here)
library(yaml)

#' Load and process Excel database with automatic structure detection
#' @param file_path Path to Excel file
#' @param country_code Country code for taxonomy mapping
#' @param config_env Configuration environment
#' @return Standardized data frame
load_excel_database <- function(file_path, country_code = NULL, config_env = "default") {
  
  cat("📊 Loading Excel database:", basename(file_path), "\n")
  
  if (!file.exists(file_path)) {
    stop("Excel file not found: ", file_path)
  }
  
  # Step 1: Analyze Excel file structure
  excel_info <- analyze_excel_structure(file_path, country_code)
  
  # Step 2: Determine which sheets to process
  sheets_to_process <- select_sheets_to_process(excel_info, country_code)
  
  # Step 3: Process each selected sheet and combine results
  all_data <- process_multiple_sheets(file_path, excel_info, sheets_to_process)
  
  # Step 4: Apply country-specific processing if country_code provided
  if (!is.null(country_code) && nrow(all_data) > 0) {
    all_data <- apply_country_processing(all_data, country_code)
  }
  
  cat("✅ Processed", nrow(all_data), "records from", length(sheets_to_process), "sheets\n")
  return(all_data)
}

#' Analyze Excel file structure to understand format
#' @param file_path Path to Excel file
#' @param country_code Country code for configuration
#' @return List with structure information
analyze_excel_structure <- function(file_path, country_code = NULL) {
  
  cat("🔍 Analyzing Excel file structure...\n")
  
  # Get sheet names
  sheet_names <- excel_sheets(file_path)
  cat("📋 Found", length(sheet_names), "sheets:", paste(sheet_names, collapse = ", "), "\n")
  
  structure_info <- list(
    file_path = file_path,
    sheet_names = sheet_names,
    sheets = list()
  )
  
  # Analyze each sheet
  for (sheet_name in sheet_names) {
    
    cat("🔍 Analyzing sheet:", sheet_name, "\n")
    
    # Read first few rows to understand structure
    tryCatch({
      # Try reading with different header assumptions
      preview_data <- read_excel(file_path, sheet = sheet_name, n_max = 20)
      
      # Skip empty sheets
      if (nrow(preview_data) == 0 || ncol(preview_data) == 0) {
        structure_info$sheets[[sheet_name]] <- list(
          name = sheet_name,
          dimensions = c(0, 0),
          format_type = "empty"
        )
        next
      }
      
      sheet_info <- list(
        name = sheet_name,
        dimensions = dim(preview_data),
        column_names = names(preview_data),
        data_types = sapply(preview_data, class),
        has_merged_cells = detect_merged_cells(preview_data),
        likely_header_row = detect_header_row(file_path, sheet_name, country_code),
        format_type = detect_format_type(preview_data)
      )
      
      structure_info$sheets[[sheet_name]] <- sheet_info
      
      cat("   📏 Dimensions:", sheet_info$dimensions[1], "rows x", sheet_info$dimensions[2], "cols\n")
      cat("   📋 Format:", sheet_info$format_type, "\n")
      
    }, error = function(e) {
      cat("⚠️ Could not analyze sheet", sheet_name, ":", e$message, "\n")
      structure_info$sheets[[sheet_name]] <- list(
        name = sheet_name,
        error = e$message
      )
    })
  }
  
  # Identify primary data sheet
  structure_info$primary_sheet <- identify_primary_sheet(structure_info$sheets)
  cat("🎯 Primary data sheet:", structure_info$primary_sheet, "\n")
  
  return(structure_info)
}

#' Detect merged cells in Excel data
#' @param data Preview data from Excel
#' @return Boolean indicating presence of merged cells
detect_merged_cells <- function(data) {
  # Look for patterns indicating merged cells (many NAs in specific patterns)
  na_pattern <- sapply(data, function(x) sum(is.na(x)) / length(x))
  return(any(na_pattern > 0.8) && any(na_pattern < 0.2))
}

#' Detect the likely header row in Excel sheet
#' @param file_path Excel file path
#' @param sheet_name Sheet name
#' @return Likely header row number
detect_header_row <- function(file_path, sheet_name, country_code = NULL) {
  
  # ENHANCED: Check country configuration first
  if (!is.null(country_code)) {
    if (!requireNamespace("here", quietly = TRUE)) {
      country_config_path <- file.path("config", "countries", paste0(country_code, ".yml"))
    } else {
      country_config_path <- here::here("config", "countries", paste0(country_code, ".yml"))
    }
    
    if (file.exists(country_config_path)) {
      tryCatch({
        country_config <- yaml::read_yaml(country_config_path)
        
        # Use header_row from config if specified
        if (!is.null(country_config$data_source$excel$header_row)) {
          config_header_row <- country_config$data_source$excel$header_row
          cat("📋 Using configured header row:", config_header_row, "\n")
          return(config_header_row)
        }
        
        # Use skip_rows + 1 if specified (skip_rows means rows to skip before header)
        if (!is.null(country_config$data_source$excel$skip_rows)) {
          skip_rows <- country_config$data_source$excel$skip_rows
          header_row <- skip_rows + 1
          cat("📋 Calculated header row from skip_rows:", header_row, "(skip", skip_rows, "+ 1)\n")
          return(header_row)
        }
      }, error = function(e) {
        cat("⚠️ Error reading country config for header row:", e$message, "\n")
      })
    }
  }
  
  # Fallback to auto-detection
  cat("📋 Auto-detecting header row...\n")
  
  # Read first 10 rows without headers
  preview <- read_excel(file_path, sheet = sheet_name, col_names = FALSE, n_max = 10)
  
  # Look for row with most non-missing, non-numeric text values
  text_scores <- apply(preview, 1, function(row) {
    non_na_count <- sum(!is.na(row))
    if (non_na_count == 0) return(0)
    text_count <- sum(!is.na(row) & !grepl("^[0-9.,-]+$", as.character(row[!is.na(row)])))
    return(text_count / non_na_count * non_na_count) # weighted by completeness
  })
  
  header_row <- which.max(text_scores)
  detected_row <- max(1, header_row)
  cat("📋 Auto-detected header row:", detected_row, "\n")
  return(detected_row)
}

#' Detect the format type of Excel data
#' @param data Preview data from Excel
#' @return String indicating format type
detect_format_type <- function(data) {
  
  col_names <- names(data)
  
  # Wide format: many columns with disease_age_sex patterns
  # ENHANCED: Include South Sudan patterns like Malaria_Under1_Male
  if (length(col_names) > 10 && any(grepl("_(male|female|Male|Female)", col_names, ignore.case = TRUE))) {
    return("wide_demographic")
  }
  
  # ENHANCED: Specific South Sudan pattern detection
  if (any(grepl("(malaria|diarrhea|pneumonia|measles|ari)_(under1|1to4|5to14|15to49|50plus)_(male|female)", col_names, ignore.case = TRUE))) {
    return("wide_demographic")
  }
  
  # Wide format: many disease columns
  if (length(col_names) > 15 && sum(grepl("(malaria|diarr|pneumonia|measles|cholera)", col_names, ignore.case = TRUE)) > 5) {
    return("wide_disease") 
  }
  
  # Long format: few columns with value/count columns
  if (length(col_names) < 10 && any(grepl("(value|count|cases|total|number)", col_names, ignore.case = TRUE))) {
    return("long_format")
  }
  
  # Pivot/crosstab format: first column is categorical, rest are numeric
  if (ncol(data) < 2) return("unknown")
  
  first_col_text <- sum(!is.na(data[,1]) & !grepl("^[0-9.,-]+$", as.character(data[,1])))
  other_cols_numeric <- sum(sapply(data[,-1], function(x) {
    if (is.numeric(x)) return(TRUE)
    non_na_vals <- x[!is.na(x)]
    if (length(non_na_vals) == 0) return(FALSE)
    return(all(grepl("^[0-9.,-]+$", as.character(non_na_vals))))
  }))
  
  if (first_col_text > nrow(data) * 0.7 && other_cols_numeric > (ncol(data) - 1) * 0.7) {
    return("pivot_table")
  }
  
  return("unknown")
}

#' Identify the primary data sheet from analyzed sheets
#' @param sheets List of sheet information
#' @return Name of primary sheet
identify_primary_sheet <- function(sheets) {
  
  # Score sheets based on likely data content
  sheet_scores <- sapply(names(sheets), function(name) {
    sheet <- sheets[[name]]
    
    if (is.null(sheet$dimensions)) return(0)
    if (!is.null(sheet$format_type) && sheet$format_type == "empty") return(0)
    if (is.null(sheet$error) && (is.null(sheet$dimensions) || sheet$dimensions[1] == 0 || sheet$dimensions[2] == 0)) return(0)
    
    score <- 0
    
    # Size score (bigger is better, up to a point)
    rows <- sheet$dimensions[1]
    cols <- sheet$dimensions[2]
    score <- score + min(rows / 100, 10) + min(cols / 10, 5)
    
    # Name score (certain keywords indicate data)
    name_lower <- tolower(name)
    
    # HEAVILY prioritize detailed location sheets over summary sheets
    if (grepl("poc|hospital|clinic|county|health.*facility|malakal|juba", name_lower) && 
        !grepl("epi|vaccination|vaccine", name_lower)) score <- score + 100
    
    # HEAVILY penalize summary sheets
    if (grepl("summary|total|overview|morbidity.*summary", name_lower)) score <- score - 50
    
    if (grepl("morbidity|surveillance|monthly|aggregate", name_lower) && 
        !grepl("summary", name_lower)) score <- score + 25
    if (grepl("sheet1|main|primary", name_lower)) score <- score + 5  
    if (grepl("metadata|dict|info|note|epi database", name_lower)) score <- score - 15
    
    # Format score
    if (!is.null(sheet$format_type) && sheet$format_type != "unknown") score <- score + 10
    
    return(score)
  })
  
  # Debug: show sheet scores
  cat("📊 Sheet scores:\n")
  for (name in names(sheet_scores)) {
    cat(sprintf("   %s: %.0f\n", name, sheet_scores[[name]]))
  }
  
  primary_sheet <- names(sheet_scores)[which.max(sheet_scores)]
  return(primary_sheet)
}

#' Load Excel data using smart detection
#' @param file_path Excel file path  
#' @param excel_info Structure information
#' @return Raw data frame
load_excel_data_smart <- function(file_path, excel_info) {
  
  primary_sheet_name <- excel_info$primary_sheet
  primary_sheet_info <- excel_info$sheets[[primary_sheet_name]]
  
  cat("📖 Loading primary sheet:", primary_sheet_name, "\n")
  
  # Determine optimal reading parameters
  skip_rows <- max(0, primary_sheet_info$likely_header_row - 1)
  
  # Read data with detected parameters
  data <- read_excel(
    file_path,
    sheet = primary_sheet_name,
    skip = skip_rows,
    .name_repair = "universal" # Handle duplicate/invalid column names
  )
  
  # Clean up data
  data <- data %>%
    # Remove completely empty rows and columns
    filter(if_any(everything(), ~ !is.na(.))) %>%
    select(where(~ !all(is.na(.))))
  
  cat("📊 Loaded", nrow(data), "rows x", ncol(data), "columns\n")
  return(data)
}

#' Select sheets to process based on country configuration
#' @param excel_info Excel structure information
#' @param country_code Country code for configuration lookup
#' @return Vector of sheet names to process
select_sheets_to_process <- function(excel_info, country_code = NULL) {
  
  # Load country configuration if available
  config <- load_country_config(country_code)
  
  # Get processing configuration
  processing_config <- get_processing_config(config)
  
  cat("📋 Sheet selection mode:", processing_config$mode, "\n")
  
  # Apply sheet selection strategy
  selected_sheets <- switch(processing_config$mode,
    "specific_sheets" = select_specific_sheets(excel_info, processing_config),
    "primary_only" = excel_info$primary_sheet,
    "auto_multiple" = select_multiple_sheets_auto(excel_info, processing_config)
  )
  
  cat("🎯 Selected", length(selected_sheets), "sheets:", paste(selected_sheets, collapse = ", "), "\n")
  return(selected_sheets)
}

#' Load country configuration
#' @param country_code Country code
#' @return Configuration list
load_country_config <- function(country_code) {
  if (is.null(country_code)) {
    return(list())
  }
  
  config_path <- here("config", "countries", paste0(country_code, ".yml"))
  if (file.exists(config_path)) {
    return(yaml::read_yaml(config_path))
  }
  
  return(list())
}

#' Get processing configuration with defaults
#' @param config Country configuration
#' @return Processing configuration
get_processing_config <- function(config) {
  # Default configuration
  defaults <- list(
    mode = "auto_multiple",
    exclude_patterns = c(".*Summary.*", ".*Database.*", "Analysis"),
    include_patterns = c(".*POC.*", ".*County.*"),
    min_score_threshold = 50,
    specific_sheets = NULL
  )
  
  # Override with country config if available
  if (!is.null(config$excel_processing$sheet_selection)) {
    processing_config <- config$excel_processing$sheet_selection
    
    # Merge with defaults
    for (key in names(defaults)) {
      if (is.null(processing_config[[key]])) {
        processing_config[[key]] <- defaults[[key]]
      }
    }
    return(processing_config)
  }
  
  return(defaults)
}

#' Select specific sheets from configuration
#' @param excel_info Excel structure information
#' @param processing_config Processing configuration
#' @return Vector of sheet names
select_specific_sheets <- function(excel_info, processing_config) {
  specific_sheets <- processing_config$specific_sheets
  available_sheets <- names(excel_info$sheets)
  
  # Filter to only existing sheets
  valid_sheets <- intersect(specific_sheets, available_sheets)
  
  if (length(valid_sheets) == 0) {
    warning("None of the specified sheets found, falling back to primary sheet")
    return(excel_info$primary_sheet)
  }
  
  return(valid_sheets)
}

#' Auto-select multiple sheets based on patterns and scores
#' @param excel_info Excel structure information  
#' @param processing_config Processing configuration
#' @return Vector of sheet names
select_multiple_sheets_auto <- function(excel_info, processing_config) {
  
  available_sheets <- names(excel_info$sheets)
  cat("🔍 Evaluating", length(available_sheets), "available sheets\n")
  
  # Step 1: Apply exclusion patterns
  excluded_sheets <- c()
  for (pattern in processing_config$exclude_patterns) {
    excluded <- available_sheets[grepl(pattern, available_sheets, ignore.case = TRUE)]
    excluded_sheets <- c(excluded_sheets, excluded)
  }
  excluded_sheets <- unique(excluded_sheets)
  
  remaining_sheets <- setdiff(available_sheets, excluded_sheets)
  cat("📋 Excluded", length(excluded_sheets), "sheets by patterns:", paste(head(excluded_sheets), collapse = ", "), 
      if(length(excluded_sheets) > 5) "..." else "", "\n")
  
  # Step 2: Apply inclusion patterns
  included_sheets <- c()
  for (pattern in processing_config$include_patterns) {
    included <- remaining_sheets[grepl(pattern, remaining_sheets, ignore.case = TRUE)]
    included_sheets <- c(included_sheets, included)
  }
  included_sheets <- unique(included_sheets)
  
  cat("📋 Included", length(included_sheets), "sheets by patterns:", paste(included_sheets, collapse = ", "), "\n")
  
  # Step 3: Apply score threshold
  sheet_scores <- sapply(included_sheets, function(name) {
    if (name %in% names(excel_info$sheets)) {
      return(calculate_sheet_score(name, excel_info$sheets[[name]]))
    }
    return(0)
  })
  
  high_score_sheets <- included_sheets[sheet_scores >= processing_config$min_score_threshold]
  cat("📋 High-scoring sheets (>= ", processing_config$min_score_threshold, "):", 
      length(high_score_sheets), "sheets\n")
  
  # Fallback to primary sheet if no sheets selected
  if (length(high_score_sheets) == 0) {
    warning("No sheets met criteria, falling back to primary sheet")
    return(excel_info$primary_sheet)
  }
  
  return(high_score_sheets)
}

#' Calculate sheet score (reuse existing logic)
#' @param name Sheet name
#' @param sheet Sheet information
#' @return Numeric score
calculate_sheet_score <- function(name, sheet) {
  if (is.null(sheet$dimensions)) return(0)
  if (!is.null(sheet$format_type) && sheet$format_type == "empty") return(0)
  if (is.null(sheet$error) && (is.null(sheet$dimensions) || sheet$dimensions[1] == 0 || sheet$dimensions[2] == 0)) return(0)
  
  score <- 0
  
  # Size score (bigger is better, up to a point)
  rows <- sheet$dimensions[1]
  cols <- sheet$dimensions[2]
  score <- score + min(rows / 100, 10) + min(cols / 10, 5)
  
  # Name score (certain keywords indicate data)
  name_lower <- tolower(name)
  
  # HEAVILY prioritize detailed location sheets over summary sheets
  if (grepl("poc|hospital|clinic|county|health.*facility|malakal|juba", name_lower) && 
      !grepl("epi|vaccination|vaccine", name_lower)) score <- score + 100
  
  # HEAVILY penalize summary sheets
  if (grepl("summary|total|overview|morbidity.*summary", name_lower)) score <- score - 50
  
  if (grepl("morbidity|surveillance|monthly|aggregate", name_lower) && 
      !grepl("summary", name_lower)) score <- score + 25
  if (grepl("sheet1|main|primary", name_lower)) score <- score + 5  
  if (grepl("metadata|dict|info|note|epi database", name_lower)) score <- score - 15
  
  # Format score
  if (!is.null(sheet$format_type) && sheet$format_type != "unknown") score <- score + 10
  
  return(score)
}

#' Process multiple sheets and combine results
#' @param file_path Excel file path
#' @param excel_info Excel structure information
#' @param sheets_to_process Vector of sheet names to process
#' @return Combined data frame
process_multiple_sheets <- function(file_path, excel_info, sheets_to_process) {
  
  all_results <- list()
  
  for (sheet_name in sheets_to_process) {
    
    cat("\n📄 Processing sheet:", sheet_name, "\n")
    
    tryCatch({
      # Update excel_info to use this sheet as primary
      sheet_excel_info <- excel_info
      sheet_excel_info$primary_sheet <- sheet_name
      
      # Load data from this sheet
      raw_data <- load_excel_data_smart(file_path, sheet_excel_info)
      
      if (nrow(raw_data) > 0 && ncol(raw_data) > 2) {
        # Transform the data
        transformed_data <- auto_transform_excel_data(raw_data, sheet_excel_info)
        
        if (nrow(transformed_data) > 0) {
          # Add sheet source for tracking
          transformed_data$source_sheet <- sheet_name
          
          # Add metadata attributes for country detection
          attr(transformed_data, "excel_path") <- file_path
          attr(transformed_data, "sheet_name") <- sheet_name
          
          all_results[[sheet_name]] <- transformed_data
          cat("✅ Processed", nrow(transformed_data), "records from", sheet_name, "\n")
        } else {
          cat("⚠️  No valid data extracted from", sheet_name, "\n")
        }
      } else {
        cat("⚠️  Sheet", sheet_name, "has insufficient data\n")
      }
      
    }, error = function(e) {
      cat("❌ Error processing sheet", sheet_name, ":", e$message, "\n")
    })
  }
  
  # Combine all results using bind_rows to handle different column structures
  if (length(all_results) > 0) {
    # Use bind_rows for safer combination that handles mismatched columns
    combined_data <- dplyr::bind_rows(all_results)
    
    # Reset row names
    rownames(combined_data) <- NULL
    
    # Preserve attributes from the first sheet for country detection
    if (length(all_results) > 0) {
      first_sheet <- all_results[[1]]
      attr(combined_data, "excel_path") <- attr(first_sheet, "excel_path")
      attr(combined_data, "sheet_name") <- attr(first_sheet, "sheet_name")
    }
    
    cat("\n📊 MULTI-SHEET SUMMARY:\n")
    cat("  Total records:", nrow(combined_data), "\n")
    cat("  Locations:", length(unique(combined_data$location)), "unique\n")
    cat("  Source sheets:", length(all_results), "sheets\n")
    
    # Show breakdown by location
    if (nrow(combined_data) > 0) {
      location_summary <- combined_data %>%
        group_by(location) %>%
        summarise(records = n(), .groups = "drop") %>%
        arrange(desc(records))
      
      cat("  Location breakdown:\n")
      for (i in 1:min(5, nrow(location_summary))) {
        cat("    ", location_summary$location[i], ":", location_summary$records[i], "records\n")
      }
      if (nrow(location_summary) > 5) {
        cat("    ... and", nrow(location_summary) - 5, "more locations\n")
      }
    }
    
    return(combined_data)
  } else {
    cat("❌ No data could be processed from any sheets\n")
    return(data.frame())
  }
}

cat("✅ Excel database loader functions loaded\n")
cat("💡 Run load_excel_database(file_path, country_code) to process Excel databases\n")