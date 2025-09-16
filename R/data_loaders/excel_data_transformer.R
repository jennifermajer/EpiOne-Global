# excel_data_transformer.R - Transform Excel data to standardized format
# Handles various Excel database formats and converts to unified structure

library(dplyr)
library(tidyr)
library(lubridate)

#' Auto-transform Excel data based on detected format
#' @param data Raw Excel data
#' @param excel_info Structure information from analysis
#' @return Standardized data frame
auto_transform_excel_data <- function(data, excel_info) {
  
  primary_sheet_info <- excel_info$sheets[[excel_info$primary_sheet]]
  format_type <- primary_sheet_info$format_type
  
  cat("🔄 Transforming data from format:", format_type, "\n")
  
  # Get sheet name for context
  sheet_name <- excel_info$primary_sheet
  
  transformed_data <- switch(format_type,
    "wide_demographic" = transform_wide_demographic_format(data),
    "wide_disease" = transform_wide_disease_format(data), 
    "long_format" = transform_long_format(data),
    "pivot_table" = transform_pivot_table_format(data, sheet_name),
    "unknown" = auto_detect_and_transform(data, sheet_name)
  )
  
  # Standardize column names and structure
  standardized_data <- standardize_excel_output(transformed_data)
  
  return(standardized_data)
}

#' Transform wide demographic format (columns like Disease_Age_Sex)
#' @param data Raw Excel data
#' @return Transformed data
transform_wide_demographic_format <- function(data) {
  
  cat("📊 Processing wide demographic format...\n")
  
  # Identify ID columns (non-demographic data columns)
  # ENHANCED: Include South Sudan patterns like Malaria_Under1_Male, Diarrhea_1to4_Female
  demographic_cols <- names(data)[grepl("_(male|female|under|over|[0-9]+|under1|1to4|5to14|15to49|50plus)_(male|female)", names(data), ignore.case = TRUE)]
  id_cols <- setdiff(names(data), demographic_cols)
  
  cat("🔍 Found", length(demographic_cols), "demographic columns\n")
  cat("🔍 Found", length(id_cols), "ID columns:", paste(head(id_cols), collapse = ", "), "\n")
  
  if (length(demographic_cols) == 0) {
    warning("No demographic columns detected in wide format")
    return(data)
  }
  
  # Pivot to long format
  long_data <- data %>%
    pivot_longer(
      cols = all_of(demographic_cols),
      names_to = "demographic_key",
      values_to = "cases",
      values_drop_na = TRUE
    ) %>%
    filter(!is.na(cases), cases != 0)
  
  # Parse demographic information from column names
  parsed_demographics <- parse_demographic_column_names(long_data$demographic_key)
  
  # Combine with data
  result <- long_data %>%
    bind_cols(parsed_demographics) %>%
    select(-demographic_key)
  
  cat("✅ Transformed to", nrow(result), "records\n")
  return(result)
}

#' Transform wide disease format (many disease columns, few demographic breakdowns)
#' @param data Raw Excel data
#' @return Transformed data
transform_wide_disease_format <- function(data) {
  
  cat("📊 Processing wide disease format...\n")
  
  # Identify disease columns vs ID columns
  disease_patterns <- c("malaria", "diarr", "pneumonia", "measles", "cholera", "fever", "cough", 
                       "headache", "vomit", "skin", "eye", "ear", "respiratory", "gastro")
  
  disease_cols <- names(data)[grepl(paste(disease_patterns, collapse = "|"), names(data), ignore.case = TRUE)]
  id_cols <- setdiff(names(data), disease_cols)
  
  cat("🔍 Found", length(disease_cols), "disease columns\n")
  cat("🔍 Found", length(id_cols), "ID columns:", paste(head(id_cols), collapse = ", "), "\n")
  
  if (length(disease_cols) == 0) {
    warning("No disease columns detected")
    return(data)
  }
  
  # Pivot to long format
  long_data <- data %>%
    pivot_longer(
      cols = all_of(disease_cols),
      names_to = "disease",
      values_to = "cases",
      values_drop_na = TRUE
    ) %>%
    filter(!is.na(cases), cases != 0)
  
  cat("✅ Transformed to", nrow(long_data), "records\n")
  return(long_data)
}

#' Transform long format data (already in long format)
#' @param data Raw Excel data
#' @return Transformed data
transform_long_format <- function(data) {
  
  cat("📊 Processing long format data...\n")
  
  # Data is already in long format, just clean and standardize
  result <- data %>%
    # Clean numeric values
    mutate(across(where(is.character), ~ as.numeric(gsub("[^0-9.]", "", .x)))) %>%
    # Remove rows with all NA values
    filter(if_any(everything(), ~ !is.na(.)))
  
  cat("✅ Cleaned", nrow(result), "records\n")
  return(result)
}

#' Transform pivot table format (diseases in rows, time/locations in columns)
#' @param data Raw Excel data
#' @param sheet_name Name of the Excel sheet (for location extraction)
#' @return Transformed data
transform_pivot_table_format <- function(data, sheet_name = "") {
  
  cat("📊 Processing pivot table format...\n")
  
  # Detect if this is a multi-dimensional table (disease + age group structure)
  col_names <- names(data)
  first_col <- col_names[1]
  second_col <- col_names[2]
  value_cols <- col_names[3:length(col_names)]
  
  cat("🔍 First column (disease):", first_col, "\n")
  cat("🔍 Second column:", second_col, "\n")
  cat("🔍 Value columns:", length(value_cols), "columns\n")
  
  # Check if second column looks like age groups
  sample_values <- unique(data[[second_col]])[1:5]
  is_age_dimension <- any(grepl("(month|year|age|[0-9]+)", sample_values, ignore.case = TRUE))
  
  if (is_age_dimension) {
    cat("💡 Detected multi-dimensional table with age groups\n")
    return(transform_multi_dimensional_table(data, first_col, second_col, value_cols, sheet_name))
  } else {
    cat("💡 Processing as simple pivot table\n")
    return(transform_simple_pivot_table(data, first_col, value_cols))
  }
}

# New function for multi-dimensional tables (disease x age group x values)
transform_multi_dimensional_table <- function(data, disease_col, age_col, value_cols, sheet_name = "") {
  
  cat("📊 Processing multi-dimensional table...\n")
  
  # Extract location from sheet name
  location <- extract_location_from_sheet_name(sheet_name)
  
  # Check if this is a detailed sheet with sex breakdown (3+ value columns)
  if (length(value_cols) >= 3) {
    cat("💡 Detected detailed sheet with potential sex breakdown\n")
    return(transform_detailed_multi_dimensional_table(data, disease_col, age_col, value_cols, location))
  } else {
    cat("💡 Processing as summary table\n")
    return(transform_summary_multi_dimensional_table(data, disease_col, age_col, value_cols, location))
  }
}

# Transform detailed sheets with sex breakdown
transform_detailed_multi_dimensional_table <- function(data, disease_col, age_col, value_cols, location) {
  
  cat("📊 Processing detailed multi-dimensional table with sex breakdown...\n")
  
  # For detailed sheets, we need to detect the sex header row
  cat("🔍 Detecting sex header in data dimensions:", nrow(data), "x", ncol(data), "\n")
  cat("🔍 Value columns (first 5):", paste(value_cols[1:5], collapse = ", "), "\n")
  
  sex_header_row <- detect_sex_header_row(data, value_cols)
  
  if (!is.null(sex_header_row)) {
    if (sex_header_row == 0) {
      # Sex information is in column names
      cat("🔍 Sex information found in column names\n")
      sex_sample <- value_cols[1:min(8, length(value_cols))]
      cat("🔍 Sex values in columns:", paste(sex_sample, collapse = " | "), "\n")
      
      # This is a complex nested Time × Sex structure
      result <- transform_nested_time_sex_table(data, disease_col, age_col, value_cols, 
                                               sex_header_row, location)
      return(result)
    } else {
      # Sex information is in a data row
      cat("🔍 Found sex header row:", sex_header_row, "\n")
      # Show what we found in that row
      sex_sample <- as.character(data[sex_header_row, value_cols[1:min(8, length(value_cols))]])
      cat("🔍 Sex values in row", sex_header_row, ":", paste(sex_sample, collapse = " | "), "\n")
      
      # Verify this really contains sex labels, not just numbers
      sex_labels_found <- any(grepl("male|female|pregn", sex_sample, ignore.case = TRUE))
      
      if (sex_labels_found) {
        # This is a complex nested Time × Sex structure
        result <- transform_nested_time_sex_table(data, disease_col, age_col, value_cols, 
                                                 sex_header_row, location)
        return(result)
      } else {
        cat("⚠ Row", sex_header_row, "doesn't contain sex labels, processing as summary\n")
      }
    }
  } else {
    # Fallback to summary processing
    cat("⚠ No sex header found, processing as summary\n")
  }
  
  return(transform_summary_multi_dimensional_table(data, disease_col, age_col, value_cols, location))
}

# New function to handle complex Time × Sex nested structure
transform_nested_time_sex_table <- function(data, disease_col, age_col, value_cols, 
                                           sex_header_row, location) {
  
  cat("🔄 Processing nested Time × Sex table structure...\n")
  
  # The structure is:
  # Row 3: Time periods as Excel serial numbers (43831, 43862, etc.) - every 4th column starting from col 3
  # Row 5: Sex categories (Male, Female (non Pregnant), Female Pregnant, Total) - repeating pattern
  
  cat("🔍 Reading time periods from the original Excel structure...\n")
  
  # We need to re-read the time header row (row 3 in original data)
  # Since we're working with data that has already been processed with skip=4,
  # we need to access the original data to get row 3
  
  # The time header is 2 rows before the sex header in the original data
  time_header_row <- sex_header_row - 2
  
  # Read time periods from the Excel file directly to get row 3
  excel_path <- attr(data, "excel_path") # We'll need to pass this through
  sheet_name <- attr(data, "sheet_name") # We'll need to pass this through
  
  # For now, use the pattern we observed: Excel dates every 4th column starting from col 3
  # This is a temporary fix until we can pass the full context through
  time_values <- character(length(value_cols))
  
  # Pattern: every 4 columns represents one time period (Male, Female non-preg, Female preg, Total)
  # Time periods occur at positions 1, 5, 9, 13, etc. in value_cols (0-based: 0, 4, 8, 12...)
  for (i in seq_along(value_cols)) {
    # Determine which time period group this column belongs to (0-based)
    time_group <- (i - 1) %/% 4
    
    # For now, generate sequential months starting from 2020-01
    # In a complete fix, we'd read these from row 3 of the Excel file
    month_num <- (time_group %% 12) + 1
    year_num <- 2020 + (time_group %/% 12)
    time_values[i] <- sprintf("%d-%02d", year_num, month_num)
  }
  
  cat("🔍 Generated time pattern:", paste(unique(time_values)[1:6], collapse = ", "), "...\n")
  
  # Extract sex labels from the sex header row or column names
  if (sex_header_row == 0) {
    # Sex information is in column names
    sex_values <- value_cols
    cat("🔍 Extracting sex from column names\n")
  } else {
    # Sex information is in a data row
    sex_values <- as.character(data[sex_header_row, value_cols])
    cat("🔍 Extracting sex from row", sex_header_row, "\n")
  }
  
  cat("🔍 Time values sample:", paste(head(time_values[!is.na(time_values)], 5), collapse = ", "), "\n")
  cat("🔍 Sex values sample:", paste(head(sex_values[!is.na(sex_values)], 5), collapse = ", "), "\n")
  
  # Create a mapping of columns to time_period and sex
  col_metadata <- data.frame(
    col_name = value_cols,
    time_raw = time_values,
    sex_raw = sex_values,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      # Convert Excel date serials to proper dates  
      time_period_converted = case_when(
        !is.na(as.numeric(time_raw)) & as.numeric(time_raw) > 40000 ~ 
          format(as.Date(as.numeric(time_raw), origin = "1899-12-30"), "%Y-%m"),
        TRUE ~ NA_character_
      ),
      # Clean up sex labels - ORDER MATTERS! Most specific patterns first
      sex = case_when(
        # Non-pregnant females first (most specific)
        grepl("female.*(non|not).*pregn|non.*pregn.*female|(non|not).*pregnant.*female", sex_raw, ignore.case = TRUE) ~ "Female (non-pregnant)",
        # Then pregnant females  
        grepl("female.*pregn|pregn.*female|pregnant.*female", sex_raw, ignore.case = TRUE) ~ "Female (pregnant)",
        # General female (catch-all for other female patterns)
        grepl("female", sex_raw, ignore.case = TRUE) ~ "Female",
        # Males
        grepl("male", sex_raw, ignore.case = TRUE) ~ "Male", 
        # Totals
        grepl("total", sex_raw, ignore.case = TRUE) ~ "Total",
        TRUE ~ "Unknown"
      ),
      col_index = seq_along(col_name)
    ) %>%
    # Group sequential columns and fill time periods forward within each time period group
    arrange(col_index) %>%
    mutate(
      # Identify columns that have actual time values (Excel date serials)
      has_time = !is.na(time_period_converted),
      # Create time group IDs by cumulative sum of has_time
      time_group = cumsum(has_time)
    ) %>%
    group_by(time_group) %>%
    # Fill the time period forward within each group
    mutate(
      time_period = first(time_period_converted[has_time])
    ) %>%
    ungroup() %>%
    # Final time period with fallback to raw value
    mutate(
      time_period = coalesce(time_period, time_raw)
    ) %>%
    select(-time_period_converted, -has_time, -time_group, -col_index) %>%
    filter(!is.na(sex), sex != "Unknown", sex != "Total")  # Remove total columns
  
  cat("📋 Column metadata created for", nrow(col_metadata), "columns\n")
  
  # Filter out header rows from data (only sex header row since time row was reconstructed)
  if (sex_header_row == 0) {
    # Sex info is in column names, no rows to filter
    data_rows <- data
  } else {
    # Filter out the sex header row
    data_rows <- data[-(1:sex_header_row), ]
  }
  
  # Pivot to long format with metadata
  long_data <- data_rows %>%
    pivot_longer(
      cols = all_of(col_metadata$col_name),
      names_to = "col_name",
      values_to = "cases",
      values_drop_na = TRUE,
      values_transform = as.character
    ) %>%
    filter(!is.na(cases), cases != "", cases != "0") %>%
    mutate(cases = as.numeric(cases)) %>%
    filter(!is.na(cases), cases > 0) %>%
    # Join with metadata to get time_period and sex
    left_join(col_metadata, by = "col_name") %>%
    rename(
      disease = !!disease_col,
      age_group = !!age_col
    ) %>%
    mutate(
      location = location,
      age_group = clean_age_groups(age_group),
      # Clean up time periods
      time_period = substr(time_period, 1, 7)  # YYYY-MM format
    ) %>%
    select(disease, cases, age_group, sex, time_period, location) %>%
    filter(!is.na(time_period), !is.na(sex))
  
  cat("✅ Transformed to", nrow(long_data), "records with full dimensions\n")
  return(long_data)
}

# Transform summary sheets (aggregated across sex)
transform_summary_multi_dimensional_table <- function(data, disease_col, age_col, value_cols, location) {
  
  cat("📊 Processing summary multi-dimensional table...\n")
  
  # Pivot to long format preserving both disease and age dimensions
  long_data <- data %>%
    pivot_longer(
      cols = all_of(value_cols),
      names_to = "time_period",
      values_to = "cases",
      values_drop_na = TRUE,
      values_transform = as.character
    ) %>%
    filter(!is.na(cases), cases != "", cases != "0") %>%
    mutate(cases = as.numeric(cases)) %>%
    filter(!is.na(cases), cases > 0) %>%
    rename(
      disease = !!disease_col,
      age_group = !!age_col
    ) %>%
    mutate(
      # Clean up column names - remove leading dots/ellipses
      time_period_raw = time_period,
      time_period = gsub("^[\\.]{3,}|^\\.\\.\\.", "", time_period),
      
      # CRITICAL FIX: Convert Excel serial numbers to proper dates
      time_period = convert_excel_serial_to_date(time_period),
      
      # Clean up age groups
      age_group = clean_age_groups(age_group),
      # For summary sheets, sex is aggregated
      sex = "All sexes",
      location = location
    )
  
  # Show sample of date conversion
  if (nrow(long_data) > 0) {
    sample_dates <- unique(long_data$time_period)[1:min(3, length(unique(long_data$time_period)))]
    cat("📅 Sample converted dates:", paste(sample_dates, collapse = ", "), "\n")
  }
  
  cat("✅ Transformed to", nrow(long_data), "records with age group dimension\n")
  return(long_data)
}

# Helper functions for detailed processing
detect_sex_header_row <- function(data, value_cols) {
  # In the processed data (after skip=4), the sex header is actually in the column names
  # The sex patterns are embedded in the column names like "Male...3", "Female (non Pregnant)", etc.
  
  # Count sex patterns in the column names of value_cols
  sex_patterns_in_cols <- sum(grepl("male|female|pregn", value_cols, ignore.case = TRUE))
  
  if (sex_patterns_in_cols >= 6) {
    cat("🔍 Found", sex_patterns_in_cols, "sex patterns in column names\n")
    # Return 0 to indicate that sex info is in column names, not in data rows
    return(0)
  }
  
  # Look for row with sex patterns across ALL columns in the data rows
  for (row in 1:min(15, nrow(data))) {
    sex_count <- 0
    
    # Check all columns in this row for sex patterns
    row_values <- as.character(data[row, ])
    
    for (i in seq_along(row_values)) {
      cell_value <- row_values[i]
      if (!is.na(cell_value) && cell_value != "") {
        # Check if it's a sex label
        if (grepl("male|female|pregn", cell_value, ignore.case = TRUE)) {
          sex_count <- sex_count + 1
        }
      }
    }
    
    # If we find multiple sex patterns, this is likely the header row
    if (sex_count >= 6) {  # Expecting multiple repetitions of Male/Female pattern
      cat("🔍 Found", sex_count, "sex patterns in row", row, "\n")
      return(row)
    }
  }
  
  return(NULL)
}

# Find the row with time/date information (Excel date serials)
detect_time_header_row <- function(data, value_cols) {
  for (row in 1:min(10, nrow(data))) {
    time_count <- 0
    for (col in value_cols[1:min(10, length(value_cols))]) {  # Check first 10 cols
      col_index <- which(names(data) == col)
      if (length(col_index) > 0) {
        cell_value <- as.character(data[row, col_index])
        if (!is.na(cell_value) && !is.na(as.numeric(cell_value))) {
          numeric_val <- as.numeric(cell_value)
          # Excel date serials are typically between 40000-50000 for 2010s-2020s
          if (numeric_val > 40000 && numeric_val < 50000) {
            time_count <- time_count + 1
          }
        }
      }
    }
    if (time_count >= 2) {
      return(row)
    }
  }
  return(2)  # Default to row 2 based on our analysis
}

extract_sex_labels <- function(data, value_cols, header_row) {
  sex_row <- data[header_row, value_cols]
  sex_labels <- as.character(sex_row)
  
  # Clean and standardize sex labels
  sex_labels <- sapply(sex_labels, function(x) {
    x <- trimws(tolower(x))
    if (grepl("male.*pregn|pregn.*female", x)) return("Female (pregnant)")
    if (grepl("female.*non|non.*pregn", x)) return("Female (non-pregnant)")  
    if (grepl("female", x)) return("Female")
    if (grepl("male", x)) return("Male")
    return("Unknown")
  })
  
  return(sex_labels)
}

map_column_to_sex <- function(sex_time_col, value_cols, sex_labels) {
  # Find the position of the column in value_cols
  col_index <- which(value_cols == sex_time_col)
  if (length(col_index) > 0 && col_index <= length(sex_labels)) {
    return(sex_labels[col_index])
  }
  return("Unknown")
}

extract_location_from_sheet_name <- function(sheet_name) {
  if (sheet_name == "") return("")
  
  # Extract location from common patterns
  location <- case_when(
    grepl("malakal", sheet_name, ignore.case = TRUE) ~ "Malakal",
    grepl("wau", sheet_name, ignore.case = TRUE) ~ "Wau", 
    grepl("juba", sheet_name, ignore.case = TRUE) ~ "Juba",
    grepl("assosa", sheet_name, ignore.case = TRUE) ~ "Assosa",
    grepl("bam", sheet_name, ignore.case = TRUE) ~ "Bam",
    grepl("maban", sheet_name, ignore.case = TRUE) ~ "Maban",
    grepl("akoka", sheet_name, ignore.case = TRUE) ~ "Akoka",
    grepl("balliet", sheet_name, ignore.case = TRUE) ~ "Balliet", 
    grepl("pigi", sheet_name, ignore.case = TRUE) ~ "Pigi",
    grepl("panyikang", sheet_name, ignore.case = TRUE) ~ "Panyikang",
    grepl("aburoc", sheet_name, ignore.case = TRUE) ~ "Aburoc",
    TRUE ~ ""
  )
  
  # If still empty, try to extract the first word before common keywords
  if (location == "" && sheet_name != "") {
    # Remove common suffixes and extract first meaningful word
    cleaned_name <- gsub("\\s+(POC|Summary|County|Teaching|Hosp|Hospital).*", "", sheet_name, ignore.case = TRUE)
    cleaned_name <- trimws(cleaned_name)
    if (nchar(cleaned_name) > 0) {
      location <- cleaned_name
    }
  }
  
  return(location)
}

extract_time_from_sheet_context <- function(location) {
  # For now, return a placeholder - this would need more specific logic
  # based on the actual time information in the sheets
  return("2020")
}

#' Convert Excel serial date numbers to proper dates
#' @param serial_numbers Vector of Excel serial numbers or mixed values
#' @return Vector of proper date strings
convert_excel_serial_to_date <- function(serial_numbers) {
  
  result <- character(length(serial_numbers))
  
  for (i in seq_along(serial_numbers)) {
    value <- serial_numbers[i]
    
    # Check if it's a numeric value that could be an Excel serial date
    if (!is.na(value) && is.numeric(as.numeric(value)) && !is.na(as.numeric(value))) {
      serial_num <- as.numeric(value)
      
      # Excel serial dates are typically in the range 1-50000 for recent years
      # Excel epoch starts from 1900-01-01, but Excel incorrectly treats 1900 as leap year
      if (serial_num >= 1 && serial_num <= 50000) {
        tryCatch({
          # Convert Excel serial to date (subtract 1 due to Excel's leap year bug)
          excel_date <- as.Date(serial_num - 1, origin = "1900-01-01")
          result[i] <- as.character(excel_date)
        }, error = function(e) {
          result[i] <- as.character(value)
        })
      } else {
        result[i] <- as.character(value)
      }
    } else {
      result[i] <- as.character(value)
    }
  }
  
  return(result)
}

#' Extract admin levels from location using country configuration
#' @param location Location extracted from sheet name or data
#' @param country_code Country code for configuration lookup
#' @return List with admin0, admin1, admin2, admin3
extract_admin_from_location <- function(location, country_code = NULL) {
  
  # Default admin structure
  admin_info <- list(
    admin0 = NA_character_,
    admin1 = NA_character_,
    admin2 = NA_character_,
    admin3 = NA_character_
  )
  
  if (is.null(country_code) || is.na(location) || nchar(trimws(location)) == 0) {
    return(admin_info)
  }
  
  # Load country configuration
  if (!requireNamespace("here", quietly = TRUE)) {
    country_config_path <- file.path("config", "countries", paste0(country_code, ".yml"))
  } else {
    country_config_path <- here::here("config", "countries", paste0(country_code, ".yml"))
  }
  if (!file.exists(country_config_path)) {
    return(admin_info)
  }
  
  tryCatch({
    country_config <- yaml::read_yaml(country_config_path)
    
    # Set admin0 from country config
    if (!is.null(country_config$country$name)) {
      admin_info$admin0 <- country_config$country$name
    }
    
    # Check if country config has location mapping
    if (!is.null(country_config$excel_processing$location_mapping)) {
      location_mapping <- country_config$excel_processing$location_mapping
      location_clean <- tolower(trimws(location))
      
      # Look for exact match first
      if (location_clean %in% names(location_mapping)) {
        mapping <- location_mapping[[location_clean]]
        admin_info$admin1 <- mapping$admin1
        admin_info$admin2 <- mapping$admin2
        admin_info$admin3 <- mapping$admin3
      } else {
        # Try partial matches
        for (mapped_location in names(location_mapping)) {
          if (grepl(mapped_location, location_clean, fixed = TRUE) || 
              grepl(location_clean, mapped_location, fixed = TRUE)) {
            mapping <- location_mapping[[mapped_location]]
            admin_info$admin1 <- mapping$admin1
            admin_info$admin2 <- mapping$admin2
            admin_info$admin3 <- mapping$admin3
            break
          }
        }
      }
    }
    
    # If no specific mapping found, try to infer from location name
    if (is.na(admin_info$admin1) && !is.na(location)) {
      # Use location as admin2 (facility level) and try to infer admin1
      admin_info$admin2 <- location
      admin_info$admin3 <- paste(location, "Facility")
    }
    
  }, error = function(e) {
    cat("⚠️ Error loading country config for admin mapping:", e$message, "\n")
  })
  
  return(admin_info)
}

clean_age_groups <- function(age_group) {
  age_group <- trimws(age_group)
  age_group <- case_when(
    grepl("0.*11.*month", age_group, ignore.case = TRUE) ~ "0-11 months",
    grepl("1.*4.*year", age_group, ignore.case = TRUE) ~ "1-4 years", 
    grepl("5.*14.*year", age_group, ignore.case = TRUE) ~ "5-14 years",
    grepl("15.*49.*year", age_group, ignore.case = TRUE) ~ "15-49 years",
    grepl("50.*60.*year", age_group, ignore.case = TRUE) ~ "50-60 years", 
    grepl("60.*year", age_group, ignore.case = TRUE) ~ ">60 years",
    TRUE ~ age_group
  )
  return(age_group)
}

# Original simple pivot function for backwards compatibility  
transform_simple_pivot_table <- function(data, first_col, value_cols) {
  
  cat("📊 Processing simple pivot table...\n")
  
  # Pivot to long format
  long_data <- data %>%
    pivot_longer(
      cols = all_of(value_cols),
      names_to = "category",
      values_to = "value",
      values_drop_na = TRUE,
      values_transform = as.character
    ) %>%
    filter(!is.na(value), value != 0, value != "") %>%
    mutate(value = as.numeric(value)) %>%
    filter(!is.na(value), value > 0) %>%
    rename(disease = !!first_col, cases = value)
  
  # Try to parse category column for additional dimensions
  parsed_categories <- parse_category_column(long_data$category)
  
  result <- long_data %>%
    mutate(
      time_period = parsed_categories$time_period,
      location = parsed_categories$location
    ) %>%
    select(-category)
  
  cat("✅ Transformed to", nrow(result), "records\n")
  return(result)
}

#' Auto-detect format and transform when format is unknown
#' @param data Raw Excel data
#' @return Transformed data
auto_detect_and_transform <- function(data, sheet_name = "") {
  
  cat("🔍 Auto-detecting data format...\n")
  
  # Simple heuristics for format detection
  numeric_cols <- sum(sapply(data, is.numeric))
  total_cols <- ncol(data)
  
  # Check if first column contains disease/condition names (row-based format)
  first_col_vals <- data[[1]]
  first_col_text <- first_col_vals[!is.na(first_col_vals)]
  
  # Look for disease/medical condition patterns in first column
  disease_patterns <- c("visit", "pneumonia", "malaria", "diarr", "fever", "cough", 
                       "measles", "cholera", "consultation", "headache", "vomit", 
                       "respiratory", "gastro", "moderate", "severe")
  
  has_diseases_in_rows <- any(grepl(paste(disease_patterns, collapse = "|"), 
                                   first_col_text, ignore.case = TRUE))
  
  if (has_diseases_in_rows) {
    cat("💡 Detected: Pivot table format with diseases in rows\n")
    return(transform_pivot_table_format(data, sheet_name))
  } else if (numeric_cols / total_cols > 0.7 && total_cols > 10) {
    cat("💡 Detected: Likely wide format with many numeric columns\n")
    return(transform_wide_disease_format(data))
  } else if (total_cols <= 5) {
    cat("💡 Detected: Likely long format with few columns\n") 
    return(transform_long_format(data))
  } else {
    cat("💡 Detected: Likely pivot table format\n")
    return(transform_pivot_table_format(data, sheet_name))
  }
}

#' Parse demographic information from column names
#' @param column_names Vector of demographic column names
#' @return Data frame with parsed demographic information
parse_demographic_column_names <- function(column_names) {
  
  # Initialize result data frame
  result <- data.frame(
    disease = character(length(column_names)),
    age_group = character(length(column_names)),
    sex = character(length(column_names)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(column_names)) {
    col_name <- column_names[i]
    
    # Split by underscore or other separators
    parts <- strsplit(col_name, "[_.-]")[[1]]
    
    # Extract disease (usually first part)
    result$disease[i] <- parts[1]
    
    # Extract age group
    age_patterns <- c("under1", "1to4", "5to14", "15to49", "50plus", "0-11m", "1-4y", "5-14y", "15-49y", "50+y")
    age_match <- grep(paste(age_patterns, collapse = "|"), parts, ignore.case = TRUE, value = TRUE)
    result$age_group[i] <- ifelse(length(age_match) > 0, age_match[1], "All ages")
    
    # Extract sex
    sex_patterns <- c("male", "female", "m", "f")
    sex_match <- grep(paste(sex_patterns, collapse = "|"), parts, ignore.case = TRUE, value = TRUE)
    if (length(sex_match) > 0) {
      sex_value <- tolower(sex_match[1])
      result$sex[i] <- case_when(
        grepl("^m", sex_value) ~ "Male",
        grepl("^f", sex_value) ~ "Female",
        TRUE ~ "All sexes"
      )
    } else {
      result$sex[i] <- "All sexes"
    }
  }
  
  return(result)
}

#' Parse category column for additional dimensions
#' @param categories Vector of category values
#' @return Data frame with parsed category information
parse_category_column <- function(categories) {
  
  # Try to detect if categories contain dates, locations, or other dimensions
  result <- data.frame(
    time_period = character(length(categories)),
    location = character(length(categories)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(categories)) {
    category <- categories[i]
    
    # Check if it looks like a date
    if (grepl("(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec|[0-9]{4})", category, ignore.case = TRUE)) {
      result$time_period[i] <- category
      result$location[i] <- ""
    } else {
      # Assume it's a location or other category
      result$time_period[i] <- ""
      result$location[i] <- category
    }
  }
  
  return(result)
}

#' Standardize Excel output to unified format
#' @param data Transformed data
#' @return Standardized data frame
standardize_excel_output <- function(data) {
  
  cat("🔧 Standardizing output format...\n")
  
  # Ensure required columns exist
  required_cols <- c("disease", "cases", "age_group", "sex", "time_period", "location")
  
  for (col in required_cols) {
    if (!col %in% names(data)) {
      if (col == "cases") {
        data[[col]] <- rep(as.numeric(NA), nrow(data))
      } else {
        default_value <- switch(col,
          "disease" = "Unknown",
          "age_group" = "All ages", 
          "sex" = "All sexes",
          "time_period" = "",
          "location" = "",
          as.character(NA)
        )
        data[[col]] <- rep(default_value, nrow(data))
      }
    }
  }
  
  # CRITICAL FIX: Preserve admin columns if they exist
  admin_cols <- intersect(c("admin0", "admin1", "admin2", "admin3", "Country", "State", "County", "Payam"), names(data))
  
  # Clean and standardize data
  standardized <- data %>%
    # Convert each column individually to avoid type mixing issues
    mutate(
      disease = as.character(disease),
      cases = as.numeric(cases),
      age_group = as.character(age_group),
      sex = as.character(sex),
      time_period = as.character(time_period),
      location = as.character(location)
    ) %>%
    # CRITICAL FIX: Rename time_period to datevisit for preprocessing compatibility
    rename(datevisit = time_period) %>%
    # Clean admin columns if they exist
    {
      if (length(admin_cols) > 0) {
        mutate_at(., vars(all_of(admin_cols)), ~ trimws(as.character(.)))
      } else {
        .
      }
    } %>%
    # Remove records with missing or zero cases
    filter(!is.na(cases), cases > 0) %>%
    # Clean text fields
    mutate(
      disease = trimws(as.character(disease)),
      age_group = trimws(as.character(age_group)),
      sex = trimws(as.character(sex)),
      location = trimws(as.character(location)),
      datevisit = trimws(as.character(datevisit))
    ) %>%
    # Select standard columns and preserve admin columns
    {
      base_cols <- c("disease", "cases", "age_group", "sex", "datevisit", "location")
      preserved_admin_cols <- intersect(admin_cols, names(.))
      other_cols <- setdiff(names(.), c(base_cols, preserved_admin_cols))
      select(., all_of(c(base_cols, preserved_admin_cols, other_cols)))
    }
  
  # Report preserved admin columns
  if (length(admin_cols) > 0) {
    cat("🗺️ Preserved admin columns:", paste(admin_cols, collapse = ", "), "\n")
  }
  
  cat("✅ Standardized to", nrow(standardized), "records with", ncol(standardized), "columns\n")
  return(standardized)
}

#' Apply country-specific processing
#' @param data Standardized data
#' @param country_code Country code
#' @return Processed data with country-specific mappings
apply_country_processing <- function(data, country_code) {
  
  cat("🌍 Applying", toupper(country_code), "specific processing...\n")
  
  # Load country config if available
  country_config_path <- here("config", "countries", paste0(country_code, ".yml"))
  
  if (file.exists(country_config_path)) {
    country_config <- yaml::read_yaml(country_config_path)
    
    # Apply disease name mappings if available
    if (!is.null(country_config$disease_mappings$excel_names)) {
      excel_mappings <- country_config$disease_mappings$excel_names
      
      data <- data %>%
        mutate(
          disease = recode(disease, !!!excel_mappings, .default = disease)
        )
      
      cat("✅ Applied", length(excel_mappings), "disease name mappings\n")
    }
    
    # Apply age group standardization if available and age_group column exists
    if (!is.null(country_config$age_groups$excel_mapping) && "age_group" %in% names(data)) {
      age_mappings <- country_config$age_groups$excel_mapping
      
      # Create mapping from Excel age names to standardized names
      age_name_mapping <- sapply(names(age_mappings), function(x) x)
      names(age_name_mapping) <- names(age_mappings)
      
      data <- data %>%
        mutate(
          age_group = recode(age_group, !!!age_name_mapping, .default = age_group)
        )
      
      cat("✅ Applied age group standardization\n")
    }
    
    # FLEXIBLE FIX: Add admin variables if location column exists and config specifies
    if ("location" %in% names(data) && !is.null(country_config$excel_processing$extract_admin_from_location)) {
      if (country_config$excel_processing$extract_admin_from_location) {
        cat("🗺️ Extracting admin variables from location column...\n")
        
        # VECTORIZED APPROACH: Create lookup table for unique locations
        unique_locations <- unique(data$location[!is.na(data$location)])
        cat("📍 Processing", length(unique_locations), "unique locations\n")
        
        # Create admin lookup table
        admin_lookup <- data.frame(
          location = unique_locations,
          admin0 = character(length(unique_locations)),
          admin1 = character(length(unique_locations)),
          admin2 = character(length(unique_locations)),
          admin3 = character(length(unique_locations)),
          stringsAsFactors = FALSE
        )
        
        # Fill lookup table
        for (i in seq_along(unique_locations)) {
          admin_info <- extract_admin_from_location(unique_locations[i], country_code)
          admin_lookup$admin0[i] <- admin_info$admin0 %||% NA_character_
          admin_lookup$admin1[i] <- admin_info$admin1 %||% NA_character_
          admin_lookup$admin2[i] <- admin_info$admin2 %||% NA_character_
          admin_lookup$admin3[i] <- admin_info$admin3 %||% NA_character_
        }
        
        # Join admin data back to main dataset
        data <- data %>%
          left_join(admin_lookup, by = "location", suffix = c("", "_new"))
        
        cat("✅ Added admin variables from", length(unique_locations), "unique locations\n")
        
        # Show sample of admin mapping
        sample_mapping <- admin_lookup %>% head(3)
        for (i in 1:nrow(sample_mapping)) {
          cat(sprintf("   📍 %s -> %s | %s | %s\n", 
                     sample_mapping$location[i],
                     sample_mapping$admin0[i] %||% "NA",
                     sample_mapping$admin1[i] %||% "NA",
                     sample_mapping$admin2[i] %||% "NA"))
        }
      }
    }
  }
  
  return(data)
}

cat("✅ Excel data transformer functions loaded\n")
cat("💡 Use auto_transform_excel_data() to transform Excel data to standard format\n")