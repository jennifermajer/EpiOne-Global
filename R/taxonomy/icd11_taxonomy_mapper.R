# icd11_taxonomy_mapper.R - ICD-11 Based Taxonomy Mapping System
# Updated taxonomy mapper that uses ICD-11 as the base classification

library(dplyr)
library(yaml)
library(here)

source(here("R", "taxonomy", "icd11_integration.R"))
source(here("R", "taxonomy", "icd11_disease_mapper.R"))

#' Apply country-specific synonym mapping to raw disease names
#' @param disease_names Vector of raw disease names from DHIS2/data source
#' @param country_config Country configuration object
#' @return Vector of canonical disease names
#' Apply exclusion filters from country taxonomy file
#' @param data Data frame with morbidity/disease information
#' @param country_config Country configuration object
#' @return Filtered data frame with non-morbidity items removed
apply_exclusion_filters <- function(data, country_config) {

  if (nrow(data) == 0) {
    return(data)
  }

  # Load country taxonomy file if specified
  country_file <- country_config$taxonomy$country_file
  if (is.null(country_file)) {
    cat("ℹ️ No country-specific taxonomy file specified, skipping exclusions\n")
    return(data)
  }

  country_path <- here("taxonomy", country_file)
  if (!file.exists(country_path)) {
    cat("⚠️ Country taxonomy file not found:", country_path, "\n")
    return(data)
  }

  tryCatch({
    country_taxonomy <- yaml::read_yaml(country_path)

    if (is.null(country_taxonomy$exclusions)) {
      cat("ℹ️ No exclusions found in country taxonomy file\n")
      return(data)
    }

    original_rows <- nrow(data)
    exclusions <- country_taxonomy$exclusions

    # Find the disease/morbidity column
    disease_col <- NULL
    if ("morbidity" %in% names(data)) {
      disease_col <- "morbidity"
    } else if ("disease" %in% names(data)) {
      disease_col <- "disease"
    } else {
      cat("⚠️ No disease/morbidity column found, skipping exclusions\n")
      return(data)
    }

    cat("🔍 Applying exclusion filters to column:", disease_col, "\n")

    # Apply specific non-morbidity exclusions
    if (!is.null(exclusions$non_morbidities)) {
      non_morbidities <- exclusions$non_morbidities
      cat("📋 Checking", length(non_morbidities), "specific exclusions\n")

      # Create exclusion mask
      exclude_mask <- data[[disease_col]] %in% non_morbidities

      if (sum(exclude_mask) > 0) {
        excluded_items <- unique(data[[disease_col]][exclude_mask])
        cat("🚫 Excluding", sum(exclude_mask), "records for", length(excluded_items), "non-morbidity items:\n")
        for (item in head(excluded_items, 5)) {
          count <- sum(data[[disease_col]] == item, na.rm = TRUE)
          cat("   •", item, "(", count, "records )\n")
        }
        if (length(excluded_items) > 5) {
          cat("   • ... and", length(excluded_items) - 5, "more items\n")
        }

        # Apply exclusion
        data <- data[!exclude_mask, ]
      }
    }

    # Apply pattern-based exclusions if any are enabled
    if (!is.null(exclusions$categories)) {
      categories <- exclusions$categories
      total_pattern_exclusions <- 0

      for (category_name in names(categories)) {
        category_patterns <- categories[[category_name]]
        if (!is.null(category_patterns) && length(category_patterns) > 0) {
          # Only apply if patterns are not commented out (contain actual regex)
          active_patterns <- category_patterns[!grepl("^\\s*#", category_patterns)]
          if (length(active_patterns) > 0) {
            cat("🔍 Applying", category_name, "pattern exclusions:", length(active_patterns), "patterns\n")

            for (pattern in active_patterns) {
              pattern_mask <- grepl(pattern, data[[disease_col]], ignore.case = TRUE)
              if (sum(pattern_mask) > 0) {
                excluded_by_pattern <- sum(pattern_mask)
                total_pattern_exclusions <- total_pattern_exclusions + excluded_by_pattern
                cat("   🚫 Pattern '", pattern, "' excluded", excluded_by_pattern, "records\n")
                data <- data[!pattern_mask, ]
              }
            }
          }
        }
      }

      if (total_pattern_exclusions > 0) {
        cat("🚫 Total pattern-based exclusions:", total_pattern_exclusions, "records\n")
      }
    }

    final_rows <- nrow(data)
    excluded_total <- original_rows - final_rows

    if (excluded_total > 0) {
      cat("✅ Exclusion filters applied:", excluded_total, "records excluded,", final_rows, "records remain\n")
      exclusion_rate <- round((excluded_total / original_rows) * 100, 1)
      cat("📊 Exclusion rate:", exclusion_rate, "%\n")
    } else {
      cat("ℹ️ No records matched exclusion criteria\n")
    }

    return(data)

  }, error = function(e) {
    cat("⚠️ Error applying exclusion filters:", e$message, "\n")
    return(data)
  })
}

apply_country_synonym_mapping <- function(disease_names, country_config) {
  
  # Load country taxonomy file if specified
  country_file <- country_config$taxonomy$country_file
  if (is.null(country_file)) {
    cat("ℹ️ No country-specific taxonomy file specified, using raw disease names\n")
    return(as.character(disease_names))
  }
  
  country_path <- here("taxonomy", country_file)
  if (!file.exists(country_path)) {
    cat("⚠️ Country taxonomy file not found:", country_path, "\n")
    return(as.character(disease_names))
  }
  
  tryCatch({
    country_taxonomy <- yaml::read_yaml(country_path)
    
    if (is.null(country_taxonomy$synonyms)) {
      cat("ℹ️ No synonyms found in country taxonomy file\n")
      return(as.character(disease_names))
    }
    
    cat("🌍 Loaded", length(country_taxonomy$synonyms), "country synonym mappings from", country_file, "\n")
    
    # Create mapping from synonyms to canonical names
    synonym_map <- country_taxonomy$synonyms
    
    # Apply mappings
    mapped_names <- character(length(disease_names))
    for (i in seq_along(disease_names)) {
      original_name <- as.character(disease_names[i])
      
      if (!is.na(original_name) && original_name %in% names(synonym_map)) {
        mapped_names[i] <- as.character(synonym_map[[original_name]])
      } else {
        mapped_names[i] <- original_name
      }
    }
    
    mapped_count <- sum(disease_names %in% names(synonym_map), na.rm = TRUE)
    cat("✅ Mapped", mapped_count, "out of", length(disease_names), "diseases using country synonyms\n")
    
    if (mapped_count > 0) {
      # Show sample mappings for verification
      sample_mappings <- head(disease_names[disease_names %in% names(synonym_map)], 3)
      cat("📋 Sample mappings:\n")
      for (sample in sample_mappings) {
        cat("   '", sample, "' → '", synonym_map[[sample]], "'\n", sep = "")
      }
    }
    
    return(mapped_names)
    
  }, error = function(e) {
    cat("⚠️ Error loading country taxonomy:", e$message, "\n")
    return(as.character(disease_names))
  })
}

#' Apply ICD-11 based taxonomy mapping to data (main entry point)
#' @param data Preprocessed data frame with disease information
#' @param country_config Country configuration object
#' @return Data frame with comprehensive taxonomy mapping and feature flags
apply_icd11_taxonomy <- function(data, country_config) {
  
  if (nrow(data) == 0) {
    warning("No data to process for ICD-11 taxonomy mapping")
    return(data)
  }
  
  cat("🏷️ Applying comprehensive ICD-11 taxonomy mapping...\n")
  cat("📊 Input data:", nrow(data), "rows,", ncol(data), "columns\n")
  
  # Step 0: Apply exclusion filters first (before any processing)
  tryCatch({
    cat("🔄 Step 0: Applying exclusion filters...\n")
    data <- apply_exclusion_filters(data, country_config)
    cat("✅ Step 0 completed\n")
  }, error = function(e) {
    cat("⚠️ Exclusion filters failed:", e$message, "\n")
  })

  # Step 1: Apply canonical mapping if not already present
  if (!"canonical_disease_imc" %in% names(data)) {
    if ("morbidity" %in% names(data)) {
      # Use the new canonical mapper
      source(here("R", "taxonomy", "canonical_mapper.R"))
      data <- apply_canonical_mapping(data, country_config)
      cat("🔧 Applied global base canonical mapping\n")
    } else {
      data$canonical_disease_imc <- "Unclassified"
      data$category_canonical_disease_imc <- "Uncategorized"
      cat("⚠️ No morbidity column found, using 'Unclassified'\n")
    }
  }
  
  # Step 2: Apply base taxonomy mapping first
  tryCatch({
    cat("🔄 Step 1: Applying base taxonomy mapping...\n")
    data <- apply_base_taxonomy_mapping(data, country_config)
    cat("✅ Step 1 completed\n")
  }, error = function(e) {
    cat("⚠️ Base taxonomy mapping failed:", e$message, "\n")
  })
  
  # Step 3: Apply epidemiological classifications and feature flags
  tryCatch({
    cat("🔄 Step 2: Adding epidemiological classifications...\n")
    data <- add_epidemiological_classifications(data)
    cat("✅ Step 2 completed\n")
  }, error = function(e) {
    cat("⚠️ Epidemiological classifications failed:", e$message, "\n")
    # Apply basic fallback
    data <- apply_basic_taxonomy_flags_fallback(data)
  })
  
  # Step 4: Create ICD-11 codes and categories (placeholder for now)
  tryCatch({
    cat("🔄 Step 3: Creating ICD-11 variables...\n")
    data <- add_icd11_variables(data)
    cat("✅ Step 3 completed\n")
  }, error = function(e) {
    cat("⚠️ ICD-11 variable creation failed:", e$message, "\n")
  })
  
  # Final summary
  cat("📊 Output data:", nrow(data), "rows,", ncol(data), "columns\n")
  
  # Report taxonomy flags created
  taxonomy_flags <- c("vaccine_preventable", "climate_sensitive", "outbreak_prone_case", "trauma_related")
  flag_counts <- sapply(taxonomy_flags, function(x) {
    if (x %in% names(data)) sum(data[[x]], na.rm = TRUE) else 0
  })
  
  cat("🏷️ Taxonomy flags applied:\n")
  for (i in seq_along(flag_counts)) {
    cat("   •", names(flag_counts)[i], ":", flag_counts[i], "cases\n")
  }
  
  cat("✅ ICD-11 taxonomy mapping completed successfully\n")
  return(data)
}

#' Apply base taxonomy mapping using base.yaml
#' @param data Data frame with canonical disease names
#' @param country_config Country configuration
#' @return Data with base taxonomy features applied
apply_base_taxonomy_mapping <- function(data, country_config) {
  
  # Load base taxonomy
  base_taxonomy_path <- here("taxonomy", "base.yaml") 
  if (!file.exists(base_taxonomy_path)) {
    cat("⚠️ Base taxonomy file not found at:", base_taxonomy_path, "\n")
    return(data)
  }
  
  base_taxonomy <- yaml::read_yaml(base_taxonomy_path)
  
  if (!"canonicals" %in% names(base_taxonomy)) {
    cat("⚠️ No canonicals found in base taxonomy\n")
    return(data)
  }
  
  canonicals <- base_taxonomy$canonicals
  cat("📚 Base taxonomy loaded with", length(canonicals), "canonical diseases\n")
  
  # Map canonical diseases to their attributes from base taxonomy
  data$category <- NA_character_
  data$severity <- NA_character_
  data$icd11_code <- NA_character_
  
  for (i in seq_len(nrow(data))) {
    disease_name <- data$canonical_disease_imc[i]
    
    if (!is.na(disease_name) && disease_name %in% names(canonicals)) {
      disease_info <- canonicals[[disease_name]]
      
      # Set category
      if ("category" %in% names(disease_info)) {
        data$category[i] <- disease_info$category
      }
      
      # Set severity
      if ("severity" %in% names(disease_info)) {
        data$severity[i] <- disease_info$severity
      }
      
      # Set ICD-11 codes if available
      if ("icd11_codes" %in% names(disease_info) && length(disease_info$icd11_codes) > 0) {
        # Take the first ICD-11 code
        data$icd11_code[i] <- as.character(disease_info$icd11_codes[1])
      }
    }
  }
  
  cat("✅ Base taxonomy mapping applied\n")
  return(data)
}

#' Add ICD-11 variables to data
#' @param data Data frame
#' @return Data with ICD-11 variables
add_icd11_variables <- function(data) {
  
  # Ensure ICD-11 columns exist
  if (!"icd11_code" %in% names(data)) {
    data$icd11_code <- "Unclassified"
  }
  
  if (!"icd11_title" %in% names(data)) {
    data$icd11_title <- "Unclassified"
  }
  
  if (!"icd11_category" %in% names(data)) {
    data$icd11_category <- "Unclassified"
  }
  
  # Initialize ICD-11 columns with placeholder values
  # Note: Official ICD-11 data should come from WHO API integration
  if (!"icd11_code" %in% names(data)) {
    data$icd11_code <- "Unclassified"
  }

  if (!"icd11_title" %in% names(data)) {
    data$icd11_title <- "Unclassified"
  }

  if (!"icd11_category" %in% names(data)) {
    data$icd11_category <- "Unclassified"
  }
  
  cat("📊 ICD-11 variables created\n")
  return(data)
}

#' Basic taxonomy flags fallback
#' @param data Data frame
#' @return Data with basic flags
apply_basic_taxonomy_flags_fallback <- function(data) {
  cat("🔧 Applying basic taxonomy flags fallback...\n")
  
  # Initialize flags
  data$vaccine_preventable <- FALSE
  data$climate_sensitive <- FALSE
  data$outbreak_prone_case <- FALSE
  data$epidemic_prone <- FALSE
  data$trauma_related <- FALSE
  data$amr_relevant <- FALSE
  
  if ("canonical_disease_imc" %in% names(data)) {
    # Basic pattern matching
    data$vaccine_preventable <- grepl(
      "measles|polio|diphtheria|pertussis|mumps|rubella|hepatitis a|hepatitis b|yellow fever", 
      data$canonical_disease_imc, ignore.case = TRUE
    )
    
    data$climate_sensitive <- grepl(
      "malaria|dengue|cholera|chikungunya|zika|diarrhea|diarrhoea", 
      data$canonical_disease_imc, ignore.case = TRUE
    )
    
    data$outbreak_prone_case <- grepl(
      "malaria|dengue|cholera|measles|influenza|covid|typhoid|meningitis|yellow fever|plague|ebola", 
      data$canonical_disease_imc, ignore.case = TRUE
    )
    
    data$epidemic_prone <- data$outbreak_prone_case
    
    data$trauma_related <- grepl(
      "trauma|injury|wound|fracture|burn|gunshot|blast", 
      data$canonical_disease_imc, ignore.case = TRUE
    )
    
    data$amr_relevant <- grepl(
      "tuberculosis|pneumonia|typhoid|bacterial|sepsis", 
      data$canonical_disease_imc, ignore.case = TRUE
    )
  }
  
  # Add compatibility flags
  data$vpd_case <- data$vaccine_preventable
  data$climate_sensitive_case <- data$climate_sensitive
  data$amr_case <- data$amr_relevant
  
  cat("✅ Basic taxonomy flags applied\n")
  return(data)
}

# Note: apply_icd11_taxonomy main function is defined above at line 15



#' Map standard disease names to ICD-11 codes
#' @param data Data frame with canonical_disease_imc column
#' @param icd11_mappings ICD-11 mapping data frame
#' @return Data frame with ICD-11 columns
map_canonical_to_icd11 <- function(data, icd11_mappings) {
  
  cat("🌐 Mapping standard disease names to ICD-11 codes...\n")
  cat("🔍 Available ICD-11 mappings:", nrow(icd11_mappings), "diseases\n")
  
  if (nrow(icd11_mappings) > 0) {
    cat("📋 Sample ICD-11 mappings:", paste(head(icd11_mappings$local_name, 5), collapse = ", "), "\n")
  }
  
  # Check if ICD-11 mappings are available
  if (nrow(icd11_mappings) == 0) {
    cat("⚠️ No ICD-11 mappings available, creating placeholder mappings...
")
    data$icd11_code <- "Unclassified"
    data$icd11_title <- "Unclassified"
    data$icd11_category <- "Unclassified"
    data$confidence <- "none"
    cat("✅ Created placeholder mappings for", nrow(data), "records
")
    return(data)
  }
  
  # Verify required columns exist in mappings
  required_cols <- c("local_name", "icd11_code", "icd11_title", "icd11_category", "confidence")
  missing_cols <- setdiff(required_cols, names(icd11_mappings))
  if (length(missing_cols) > 0) {
    cat("❌ Missing required columns in ICD-11 mappings:", paste(missing_cols, collapse = ", "), "
")
    cat("⚠️ Using placeholder mappings instead...
")
    data$icd11_code <- "Unclassified"
    data$icd11_title <- "Unclassified"
    data$icd11_category <- "Unclassified"
    data$confidence <- "none"
    return(data)
  }
  
  # Try multiple mapping strategies for better coverage
  
  # Strategy 1: Map using raw morbidity names (primary strategy)
  cat("🔄 Strategy 1: Mapping raw morbidity to ICD-11...\n")
  morbidity_lookup <- icd11_mappings %>%
    dplyr::select(local_name, icd11_code, icd11_title, icd11_category, confidence) %>%
    dplyr::rename(morbidity = local_name) %>%
    dplyr::mutate(morbidity = as.character(morbidity))
  
  data <- data %>%
    dplyr::mutate(morbidity = as.character(morbidity))
  
  # Join on raw morbidity names first
  data <- data %>%
    dplyr::left_join(morbidity_lookup, by = "morbidity", suffix = c("", "_morb"))
  
  mapped_via_morbidity <- sum(!is.na(data$icd11_code), na.rm = TRUE)
  cat("✅ Mapped", mapped_via_morbidity, "records via raw morbidity names\n")
  
  # Strategy 2: For unmapped records, try canonical disease mapping
  unmapped_after_morbidity <- is.na(data$icd11_code)
  if (sum(unmapped_after_morbidity) > 0) {
    cat("🔄 Strategy 2: Mapping canonical diseases to ICD-11 for", sum(unmapped_after_morbidity), "unmapped records...\n")
    
    canonical_lookup <- icd11_mappings %>%
      dplyr::select(local_name, icd11_code, icd11_title, icd11_category, confidence) %>%
      dplyr::rename(canonical_disease_imc = local_name) %>%
      dplyr::mutate(canonical_disease_imc = as.character(canonical_disease_imc))
    
    data <- data %>%
      dplyr::mutate(canonical_disease_imc = as.character(canonical_disease_imc))
    
    # Create temporary data for unmapped records
    data_unmapped <- data[unmapped_after_morbidity, ] %>%
      dplyr::select(-icd11_code, -icd11_title, -icd11_category, -confidence) %>%
      dplyr::left_join(canonical_lookup, 
                       by = "canonical_disease_imc", 
                       suffix = c("", "_canon"))
    
    data_mapped <- data[!unmapped_after_morbidity, ]
    
    # Combine results
    data <- rbind(data_mapped, data_unmapped)
    
    mapped_via_canonical <- sum(!is.na(data_unmapped$icd11_code), na.rm = TRUE)
    cat("✅ Mapped", mapped_via_canonical, "additional records via canonical names\n")
  }
  
  # Handle unmapped
  unmapped_mask <- is.na(data$icd11_code)
  unmapped_count <- sum(unmapped_mask)
  if (unmapped_count > 0) {
    unmapped_diseases <- unique(data$canonical_disease_imc[unmapped_mask])
    cat("⚠️ Found", unmapped_count, "records without ICD-11 mappings:",
        paste(head(unmapped_diseases, 5), collapse = ", "),
        if (length(unmapped_diseases) > 5) "..." else "", "
")
    data$icd11_code[unmapped_mask] <- "Unclassified"
    data$icd11_title[unmapped_mask] <- "Unclassified"
    data$icd11_category[unmapped_mask] <- "Unclassified"
    data$confidence[unmapped_mask] <- "none"
  }
  
  cat("✅ Mapped", sum(!unmapped_mask), "records to ICD-11 codes
")
  return(data)
}

#' Add priority disease flags based on ICD-11 categories and country priorities
#' @param data Data frame with ICD-11 mappings
#' @param priority_diseases Vector of priority disease names from country config
#' @return Data frame with priority flags
add_icd11_priority_flags <- function(data, priority_diseases = NULL) {
  
  cat("🎯 Adding priority disease flags...\n")
  
  # Check required columns exist
  if (!"canonical_disease_imc" %in% names(data)) {
    cat("⚠️ canonical_disease_imc column missing, skipping priority flags\n")
    return(data)
  }
  
  # Priority based on country-specific list
  if (!is.null(priority_diseases)) {
    data$is_country_priority <- data$canonical_disease_imc %in% priority_diseases
  } else {
    data$is_country_priority <- FALSE
  }
  
  # Priority based on ICD-11 categories (epidemic-prone diseases)
  if ("icd11_category" %in% names(data)) {
    epidemic_prone_categories <- c(
      "Infectious diseases",
      "Parasitic diseases", 
      "Vector-borne diseases"
    )
    
    data$is_epidemic_prone <- data$icd11_category %in% epidemic_prone_categories
  } else {
    data$is_epidemic_prone <- FALSE
  }
  
  # Priority based on specific ICD-11 codes (WHO priority diseases)
  if ("icd11_code" %in% names(data)) {
    who_priority_codes <- c(
      "1A00",    # Cholera
      "1F40",    # Malaria
      "1D26",    # Dengue
      "1F03",    # Measles
      "1C15",    # Diphtheria
      "8B00",    # Meningitis
      "1A40",    # Acute diarrhoea
      "CA40"     # Pneumonia
    )
    
    data$is_who_priority <- data$icd11_code %in% who_priority_codes
  } else {
    data$is_who_priority <- FALSE
  }
  
  # Overall priority flag
  data$is_priority_disease <- data$is_country_priority | data$is_epidemic_prone | data$is_who_priority
  
  priority_count <- sum(data$is_priority_disease, na.rm = TRUE)
  cat("🎯 Flagged", priority_count, "records as priority diseases\n")
  
  return(data)
}

#' Add epidemiological classifications and taxonomy flags based on base taxonomy and ICD-11
#' @param data Data frame with canonical disease names
#' @return Data frame with taxonomy flags for visualization
add_epidemiological_classifications <- function(data) {
  
  cat("🏷️ Adding epidemiological classifications and taxonomy flags...\n")
  
  # Load base taxonomy for attribute mapping
  base_taxonomy <- tryCatch({
    yaml::read_yaml(here::here("taxonomy", "base.yaml"))
  }, error = function(e) {
    cat("⚠️ Could not load base taxonomy, using fallback classifications\n")
    NULL
  })
  
  # Initialize flags
  data$vaccine_preventable <- FALSE
  data$climate_sensitive <- FALSE
  data$outbreak_prone_case <- FALSE
  data$epidemic_prone <- FALSE
  data$trauma_related <- FALSE
  data$amr_relevant <- FALSE
  
  # Apply flags based on base taxonomy if available
  if (!is.null(base_taxonomy) && "canonicals" %in% names(base_taxonomy)) {
    canonicals <- base_taxonomy$canonicals
    
    for (i in seq_len(nrow(data))) {
      disease_name <- data$canonical_disease_imc[i]
      
      if (!is.na(disease_name) && disease_name %in% names(canonicals)) {
        disease_info <- canonicals[[disease_name]]
        
        # Check attributes
        if ("attributes" %in% names(disease_info)) {
          attributes <- disease_info$attributes
          
          # VPD flag
          if ("VPD" %in% attributes) {
            data$vaccine_preventable[i] <- TRUE
          }
          
          # Climate sensitive flag
          if ("Climate-sensitive" %in% attributes) {
            data$climate_sensitive[i] <- TRUE
          }
          
          # Outbreak prone flag
          if ("Outbreak-prone" %in% attributes) {
            data$outbreak_prone_case[i] <- TRUE
            data$epidemic_prone[i] <- TRUE
          }
          
          # AMR relevant flag
          if ("AMR-relevant" %in% attributes) {
            data$amr_relevant[i] <- TRUE
          }
        }
        
        # Check category for trauma
        if ("category" %in% names(disease_info)) {
          category <- disease_info$category
          if (category == "Injury & External Causes") {
            data$trauma_related[i] <- TRUE
          }
        }
      }
    }
  }
  
  # Fallback pattern matching for key diseases if taxonomy unavailable
  if (is.null(base_taxonomy)) {
    cat("⚠️ Using pattern matching for taxonomy flags...\n")
    
    # Vaccine preventable diseases (pattern matching)
    vpd_patterns <- c(
      "measles", "polio", "diphtheria", "pertussis", "tetanus", "mumps", "rubella",
      "hepatitis a", "hepatitis b", "yellow fever", "japanese encephalitis", "rotavirus"
    )
    
    for (pattern in vpd_patterns) {
      mask <- grepl(pattern, data$canonical_disease_imc, ignore.case = TRUE)
      data$vaccine_preventable[mask] <- TRUE
    }
    
    # Climate sensitive diseases
    climate_patterns <- c(
      "malaria", "dengue", "chikungunya", "zika", "cholera", "diarrhea", "diarrhoea"
    )
    
    for (pattern in climate_patterns) {
      mask <- grepl(pattern, data$canonical_disease_imc, ignore.case = TRUE)
      data$climate_sensitive[mask] <- TRUE
    }
    
    # Outbreak prone diseases
    outbreak_patterns <- c(
      "measles", "cholera", "meningitis", "yellow fever", "viral haemorrhagic fever",
      "ebola", "marburg", "lassa", "plague", "anthrax"
    )
    
    for (pattern in outbreak_patterns) {
      mask <- grepl(pattern, data$canonical_disease_imc, ignore.case = TRUE)
      data$outbreak_prone_case[mask] <- TRUE
      data$epidemic_prone[mask] <- TRUE
    }
    
    # Trauma related
    trauma_patterns <- c(
      "injury", "trauma", "wound", "burn", "fracture", "gunshot", "blast", "crush"
    )
    
    for (pattern in trauma_patterns) {
      mask <- grepl(pattern, data$canonical_disease_imc, ignore.case = TRUE)
      data$trauma_related[mask] <- TRUE
    }
  }
  
  # Add case-specific flags (aliases for compatibility)
  data$vpd_case <- data$vaccine_preventable
  data$climate_sensitive_case <- data$climate_sensitive
  data$amr_case <- data$amr_relevant
  
  # Add demographic and risk group flags
  data <- add_demographic_risk_flags(data)
  
  # Classification based on ICD-11 code patterns if available
  if ("icd11_code" %in% names(data)) {
    data$epi_classification <- dplyr::case_when(
      # Infectious and parasitic diseases (Chapter 01)
      grepl("^1[A-Z]", data$icd11_code) ~ "Infectious",
      # Neoplasms (Chapter 02)
      grepl("^2[A-Z]", data$icd11_code) ~ "Neoplasms",
      # Diseases of the blood (Chapter 03)
      grepl("^3[A-Z]", data$icd11_code) ~ "Blood disorders",
      # Endocrine, nutritional and metabolic diseases (Chapter 05)
      grepl("^5[A-Z]", data$icd11_code) ~ "Metabolic",
      # Mental, behavioural and neurodevelopmental disorders (Chapter 06)
      grepl("^6[A-Z]", data$icd11_code) ~ "Mental health",
      # Diseases of the nervous system (Chapter 08)
      grepl("^8[A-Z]", data$icd11_code) ~ "Neurological",
      # Diseases of the circulatory system (Chapter 11)
      grepl("^B[A-Z]", data$icd11_code) ~ "Cardiovascular",
      # Diseases of the respiratory system (Chapter 12)
      grepl("^C[A-Z]", data$icd11_code) ~ "Respiratory",
      # Diseases of the digestive system (Chapter 13)
      grepl("^D[A-Z]", data$icd11_code) ~ "Digestive",
      # Injury, poisoning (Chapter 22)
      grepl("^N[A-Z]", data$icd11_code) ~ "Injuries",
      # Default for unmapped or unknown
      TRUE ~ "Other"
    )
    
    # Surveillance priority based on classification
    surveillance_priority_classes <- c("Infectious", "Respiratory", "Injuries")
    data$surveillance_priority <- data$epi_classification %in% surveillance_priority_classes
  }
  
  # Summary of flags applied
  flag_summary <- list(
    vaccine_preventable = sum(data$vaccine_preventable, na.rm = TRUE),
    climate_sensitive = sum(data$climate_sensitive, na.rm = TRUE),
    outbreak_prone_case = sum(data$outbreak_prone_case, na.rm = TRUE),
    trauma_related = sum(data$trauma_related, na.rm = TRUE),
    amr_relevant = sum(data$amr_relevant, na.rm = TRUE)
  )
  
  cat("📊 Taxonomy flags applied:\n")
  for (flag_name in names(flag_summary)) {
    cat("  •", flag_name, ":", flag_summary[[flag_name]], "cases\n")
  }
  
  return(data)
}

#' Add demographic and high-risk group flags
#' @param data Data frame with demographic information
#' @return Data frame with demographic risk flags
add_demographic_risk_flags <- function(data) {
  
  cat("👥 Adding demographic and risk group flags...\n")
  
  # Initialize demographic flags
  data$pediatric_case <- FALSE
  data$elderly_care_case <- FALSE
  data$pregnancy_related <- FALSE
  data$maternal_health_case <- FALSE
  data$malnutrition_complication <- FALSE
  data$chronic_condition <- FALSE
  data$ncd_related <- FALSE
  data$emergency_syndrome_case <- FALSE
  
  # Identify available demographic columns
  age_col <- NULL
  if ("age" %in% names(data)) {
    age_col <- "age"
  } else if ("ageyears" %in% names(data)) {
    age_col <- "ageyears"
  } else if ("age_years" %in% names(data)) {
    age_col <- "age_years"
  }
  
  sex_col <- NULL
  if ("sex" %in% names(data)) {
    sex_col <- "sex"
  } else if ("gender" %in% names(data)) {
    sex_col <- "gender"
  }
  
  # Age-based classifications
  if (!is.null(age_col) && age_col %in% names(data)) {
    data <- data %>%
      dplyr::mutate(
        age_numeric = as.numeric(.data[[age_col]]),
        pediatric_case = !is.na(age_numeric) & age_numeric < 18,
        elderly_care_case = !is.na(age_numeric) & age_numeric >= 65
      )
  }
  
  # Sex-based classifications for maternal health  
  if (!is.null(sex_col) && sex_col %in% names(data)) {
    # Check for pregnancy-related conditions in disease names
    pregnancy_patterns <- c(
      "pregnan", "maternal", "obstetric", "gestational", "antenatal", 
      "postnatal", "delivery", "birth", "labor", "labour"
    )
    
    for (pattern in pregnancy_patterns) {
      if ("canonical_disease_imc" %in% names(data)) {
        pregnancy_mask <- grepl(pattern, data$canonical_disease_imc, ignore.case = TRUE) &
                         !is.na(data[[sex_col]]) &
                         tolower(data[[sex_col]]) %in% c("f", "female", "woman")
        
        data$pregnancy_related[pregnancy_mask] <- TRUE
        data$maternal_health_case[pregnancy_mask] <- TRUE
      }
    }
  }
  
  # Disease-based risk classifications
  if ("canonical_disease_imc" %in% names(data)) {
    
    # Malnutrition cases
    malnutrition_patterns <- c(
      "malnutrition", "underweight", "stunting", "wasting", "marasmus", "kwashiorkor"
    )
    
    for (pattern in malnutrition_patterns) {
      mask <- grepl(pattern, data$canonical_disease_imc, ignore.case = TRUE)
      data$malnutrition_complication[mask] <- TRUE
    }
    
    # Chronic/NCD conditions
    chronic_patterns <- c(
      "diabetes", "hypertension", "cardiovascular", "asthma", "copd", "chronic", 
      "cancer", "malignancy", "epilepsy", "mental health"
    )
    
    for (pattern in chronic_patterns) {
      mask <- grepl(pattern, data$canonical_disease_imc, ignore.case = TRUE)
      data$chronic_condition[mask] <- TRUE
      data$ncd_related[mask] <- TRUE
    }
    
    # Emergency/critical conditions
    emergency_patterns <- c(
      "severe", "critical", "emergency", "shock", "sepsis", "coma", "hemorrhag",
      "acute abdomen", "respiratory failure", "cardiac arrest"
    )
    
    for (pattern in emergency_patterns) {
      mask <- grepl(pattern, data$canonical_disease_imc, ignore.case = TRUE)
      data$emergency_syndrome_case[mask] <- TRUE
    }
  }
  
  # Summary of demographic flags
  demo_summary <- list(
    pediatric_case = sum(data$pediatric_case, na.rm = TRUE),
    elderly_care_case = sum(data$elderly_care_case, na.rm = TRUE),
    pregnancy_related = sum(data$pregnancy_related, na.rm = TRUE),
    malnutrition_complication = sum(data$malnutrition_complication, na.rm = TRUE),
    chronic_condition = sum(data$chronic_condition, na.rm = TRUE),
    emergency_syndrome_case = sum(data$emergency_syndrome_case, na.rm = TRUE)
  )
  
  cat("👥 Demographic flags applied:\n")
  for (flag_name in names(demo_summary)) {
    cat("  •", flag_name, ":", demo_summary[[flag_name]], "cases\n")
  }
  
  return(data)
}

#' Identify the disease column in the data
#' @param data Data frame to search
#' @return Column name or NULL if not found
identify_disease_column <- function(data) {
  
  # Common disease column names in order of preference
  common_names <- c(
    "morbidity", "disease", "condition", "diagnosis", "Disease", "Morbidity"
  )
  
  for (col_name in common_names) {
    if (col_name %in% names(data)) {
      return(col_name)
    }
  }
  
  return(NULL)
}

#' Generate mapping summary report
#' @param data Mapped data frame
#' @param country_name Country name for the report
generate_mapping_summary <- function(data, country_name) {
  
  cat("\n📋 ICD-11 MAPPING SUMMARY FOR", toupper(country_name), "\n")
  cat(rep("=", 50), "\n")
  
  total_records <- nrow(data)
  
  # Mapping success rates
  icd11_mapped <- sum(!is.na(data$icd11_code) & data$icd11_code != "Unclassified", na.rm = TRUE)
  mapping_rate <- round((icd11_mapped / total_records) * 100, 1)
  
  cat("Total records:", scales::comma(total_records), "\n")
  cat("ICD-11 mapped:", scales::comma(icd11_mapped), "(", mapping_rate, "%)\n")
  
  # Confidence distribution
  if ("confidence" %in% names(data)) {
    conf_summary <- table(data$confidence, useNA = "ifany")
    cat("\nConfidence levels:\n")
    for (level in names(conf_summary)) {
      cat(" ", level, ":", scales::comma(conf_summary[level]), "\n")
    }
  }
  
  # Priority disease summary
  if ("is_priority_disease" %in% names(data)) {
    priority_count <- sum(data$is_priority_disease, na.rm = TRUE)
    priority_rate <- round((priority_count / total_records) * 100, 1)
    cat("\nPriority diseases:", scales::comma(priority_count), "(", priority_rate, "%)\n")
  }
  
  # Top diseases by frequency (only show diseases with official ICD-11 titles)
  if ("icd11_title" %in% names(data)) {
    # Split between mapped and unmapped diseases for clearer reporting
    mapped_diseases <- data %>%
      filter(!is.na(icd11_title) & icd11_code != "UNMAPPED") %>%
      count(icd11_code, icd11_title, sort = TRUE) %>%
      head(5)
    
    unmapped_diseases <- data %>%
      filter(is.na(icd11_title) | icd11_code == "Unclassified") %>%
      count(canonical_disease_imc, sort = TRUE) %>%
      head(5)
    
    if (nrow(mapped_diseases) > 0) {
      cat("\nTop 5 ICD-11 mapped diseases:\n")
      for (i in 1:nrow(mapped_diseases)) {
        cat(" ", i, ". ", mapped_diseases$icd11_title[i], " (", mapped_diseases$icd11_code[i], "): ", 
            scales::comma(mapped_diseases$n[i]), " cases\n", sep = "")
      }
    }
    
    if (nrow(unmapped_diseases) > 0) {
      cat("\nTop 5 unmapped diseases (no official ICD-11 title):\n")
      for (i in 1:nrow(unmapped_diseases)) {
        cat(" ", i, ". ", unmapped_diseases$canonical_disease_imc[i], " (Unclassified): ", 
            scales::comma(unmapped_diseases$n[i]), " cases\n", sep = "")
      }
    }
  }
  
  # Classification summary
  if ("epi_classification" %in% names(data)) {
    class_summary <- data %>%
      count(epi_classification, sort = TRUE)
    
    cat("\nEpidemiological classifications:\n")
    for (i in 1:nrow(class_summary)) {
      cat(" ", class_summary$epi_classification[i], ":", scales::comma(class_summary$n[i]), "\n")
    }
  }
  
  cat(rep("=", 50), "\n")
}

#' Validate ICD-11 mappings quality
#' @param data Mapped data frame
#' @return List with validation results
validate_icd11_mappings <- function(data) {
  
  results <- list(
    total_records = nrow(data),
    mapped_records = sum(!is.na(data$icd11_code) & data$icd11_code != "Unclassified"),
    unmapped_records = sum(is.na(data$icd11_code) | data$icd11_code == "UNMAPPED"),
    high_confidence = sum(data$confidence == "high", na.rm = TRUE),
    medium_confidence = sum(data$confidence == "medium", na.rm = TRUE),
    low_confidence = sum(data$confidence == "low", na.rm = TRUE),
    needs_review = sum(data$confidence == "none", na.rm = TRUE)
  )
  
  results$mapping_rate <- round((results$mapped_records / results$total_records) * 100, 1)
  results$quality_score <- round(((results$high_confidence + results$medium_confidence * 0.7) / 
                                    results$total_records) * 100, 1)
  
  return(results)
}

#' Create clean disease titles for unmapped diseases
#' Removes country codes, system artifacts, and formats for better readability
#' @param disease_names Vector of disease names to clean
#' @return Vector of cleaned disease titles
create_clean_disease_titles <- function(disease_names) {
  
  cleaned <- disease_names
  
  # Remove common DHIS2 artifacts and country codes
  cleaned <- gsub(" - OPD - YE$", "", cleaned)          # Remove "- OPD - YE"
  cleaned <- gsub(" - ICCM- YE$", "", cleaned)          # Remove "- ICCM- YE" 
  cleaned <- gsub(" Cases$", "", cleaned)               # Remove trailing "Cases"
  cleaned <- gsub("^YE - EPI Report - >1 Year - ", "", cleaned)  # Remove EPI report prefix
  cleaned <- gsub(" Cases - OPD - YE$", "", cleaned)    # Remove full pattern
  cleaned <- gsub("\\s*\\d+$", "", cleaned)             # Remove trailing numbers
  
  # Clean up specific patterns
  cleaned <- gsub("Balance from last month - (.+) Treatment.*", "\\1", cleaned)
  
  # Standardize medical terminology
  cleaned <- gsub("\\b(ARI|Ari)\\b", "Acute Respiratory Infection", cleaned)
  cleaned <- gsub("\\bUTI\\b", "Urinary Tract Infection", cleaned)
  cleaned <- gsub("\\bmellitus\\b", "", cleaned)        # Remove redundant "mellitus"
  
  # Capitalize first letter and clean whitespace
  cleaned <- trimws(cleaned)
  cleaned <- paste0(toupper(substr(cleaned, 1, 1)), substr(cleaned, 2, nchar(cleaned)))
  
  # Handle empty results
  cleaned[cleaned == "" | is.na(cleaned)] <- "Unspecified condition"
  
  return(cleaned)
}

#' Create basic ICD-11 mappings for common diseases
#' @return Data frame with basic disease mappings
create_basic_icd11_mappings <- function() {
  
  # Basic mappings for common diseases
  basic_mappings <- data.frame(
    local_name = c(
      "Acute respiratory infection", "ARI", "Upper respiratory tract infection", 
      "Lower respiratory tract infection", "Pneumonia",
      "Diarrhea", "Diarrhoea", "Acute watery diarrhea", "AWD", "Cholera",
      "Malaria", "Fever", "Dengue", "Dengue fever",
      "Measles", "Tuberculosis", "TB", "Meningitis",
      "Hepatitis", "Hepatitis A", "Hepatitis B", "Hepatitis E",
      "Hypertension", "High blood pressure", "Diabetes",
      "Malnutrition", "Severe acute malnutrition", "SAM",
      "Trauma", "Injury", "Fracture", "Burn"
    ),
    icd11_code = c(
      "CA07", "CA07", "CA07", "CA40", "CA40.0",
      "1A40", "1A40", "1A40", "1A40", "1A00",
      "1F40", "MG30", "1D26", "1D26",
      "1F03", "1B10", "1B10", "8B00",
      "1E50", "1E50.0", "1E51.0", "1E51.4",
      "BA00", "BA00", "5A10",
      "5B50", "5B51", "5B51",
      "QA00", "QA00", "QA00", "QA00"
    ),
    icd11_title = c(
      "Upper respiratory tract infection", "Upper respiratory tract infection", "Upper respiratory tract infection",
      "Lower respiratory tract infection", "Pneumonia",
      "Acute diarrhoea", "Acute diarrhoea", "Acute diarrhoea", "Acute diarrhoea", "Cholera",
      "Malaria", "Fever", "Dengue", "Dengue",
      "Measles", "Tuberculosis", "Tuberculosis", "Meningitis",
      "Hepatitis", "Hepatitis A", "Hepatitis B", "Hepatitis E",
      "Hypertension", "Hypertension", "Diabetes mellitus",
      "Malnutrition", "Severe malnutrition", "Severe malnutrition",
      "Injury", "Injury", "Injury", "Injury"
    ),
    icd11_category = c(
      rep("Respiratory diseases", 5),
      rep("Infectious diseases", 5),
      rep("Infectious diseases", 4),
      rep("Infectious diseases", 4),
      rep("Infectious diseases", 4),
      rep("Cardiovascular diseases", 2), "Metabolic diseases",
      rep("Nutritional diseases", 3),
      rep("Injuries", 4)
    ),
    confidence = rep("medium", 32),
    stringsAsFactors = FALSE
  )
  
  cat("🔧 Created basic ICD-11 mappings for", nrow(basic_mappings), "common diseases\n")
  return(basic_mappings)
}



#' Normalize function (if not available from disease_categories_taxaware)
#' @param x Vector to normalize
#' @return Normalized vector
#' Clean disease names (fallback function)
#' @param x Vector of disease names to clean
#' @return Cleaned disease names
clean_disease_names <- function(x) {
  # Basic cleaning: trim whitespace, fix common issues
  x <- trimws(as.character(x))
  x <- gsub("\\s+", " ", x)  # Normalize multiple spaces
  x
}

.norm <- function(x) {
  if (exists(".norm", envir = .GlobalEnv)) {
    return(.GlobalEnv$.norm(x))
  } else {
    # Fallback normalization
    x <- tolower(trimws(as.character(x)))
    gsub("[^a-z0-9]", "", x)
  }
}

cat("✅ ICD-11 taxonomy mapper functions loaded\n")
cat("💡 Run apply_icd11_taxonomy(data, country_config) to map diseases to ICD-11\n")