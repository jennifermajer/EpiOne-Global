# icd11_mapper.R - Pure ICD-11 Disease Mapping System
# Maps diseases to ICD-11 classification using ONLY WHO API data
# Input: morbidity OR canonical_disease_imc
# Output: icd11_code, icd11_title, icd11_category (all from WHO API)

suppressPackageStartupMessages({
  library(dplyr)
  library(yaml)
  library(here)
  library(stringr)
})

# Source the WHO API integration
source(here("R", "taxonomy", "icd11_integration.R"))

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Apply ICD-11 disease mapping using WHO API
#' @param data Data frame with disease column to map
#' @param input_column Column name to use for mapping ("morbidity" or "canonical_disease_imc")
#' @param country_config Country configuration object
#' @return Data frame with ICD-11 columns added
apply_icd11_mapping <- function(data, input_column = "morbidity", country_config = NULL) {

  if (!input_column %in% names(data)) {
    warning("Input column '", input_column, "' not found in data")
    return(add_empty_icd11_columns(data))
  }

  if (nrow(data) == 0) {
    warning("No data to process for ICD-11 mapping")
    return(add_empty_icd11_columns(data))
  }

  # Check if ICD-11 mapping is enabled for this country
  if (!is.null(country_config) &&
      !is.null(country_config$taxonomy$icd11_enabled) &&
      country_config$taxonomy$icd11_enabled != "yes") {
    cat("ℹ️ ICD-11 mapping disabled for this country\n")
    return(add_empty_icd11_columns(data))
  }

  cat("🏥 Applying pure ICD-11 mapping using WHO API...\n")
  cat("📊 Input data:", nrow(data), "rows\n")
  cat("📋 Input column:", input_column, "\n")

  # Get unique diseases to minimize API calls
  unique_diseases <- unique(data[[input_column]])
  unique_diseases <- unique_diseases[!is.na(unique_diseases) & unique_diseases != ""]

  cat("🔍 Processing", length(unique_diseases), "unique diseases\n")

  # Initialize ICD-11 results
  icd11_lookup <- list()

  # Process each unique disease through WHO API
  for (disease in unique_diseases) {
    if (is.na(disease) || disease == "" || disease == "Unclassified") {
      next
    }

    # Search ICD-11 using WHO API
    icd11_result <- search_icd11_disease(disease)

    if (!is.null(icd11_result) && length(icd11_result) > 0) {
      # Take the best match (first result)
      best_match <- icd11_result[[1]]

      icd11_lookup[[disease]] <- list(
        icd11_code = best_match$code %||% NA,
        icd11_title = best_match$title %||% NA,
        icd11_category = best_match$category %||% NA,
        icd11_confidence = best_match$confidence %||% "WHO_API"
      )
    } else {
      # No match found
      icd11_lookup[[disease]] <- list(
        icd11_code = NA,
        icd11_title = NA,
        icd11_category = NA,
        icd11_confidence = "not_found"
      )
    }
  }

  # Apply ICD-11 mappings to data
  data$icd11_code <- NA_character_
  data$icd11_title <- NA_character_
  data$icd11_category <- NA_character_
  data$icd11_confidence <- NA_character_

  for (i in seq_len(nrow(data))) {
    disease <- data[[input_column]][i]

    if (!is.na(disease) && disease != "" && disease %in% names(icd11_lookup)) {
      mapping <- icd11_lookup[[disease]]
      data$icd11_code[i] <- mapping$icd11_code
      data$icd11_title[i] <- mapping$icd11_title
      data$icd11_category[i] <- mapping$icd11_category
      data$icd11_confidence[i] <- mapping$icd11_confidence
    }
  }

  # Load any existing cached mappings
  data <- apply_cached_icd11_mappings(data, input_column)

  # Summary reporting
  mapped_count <- sum(!is.na(data$icd11_code))
  total_icd11 <- length(unique(data$icd11_code[!is.na(data$icd11_code)]))

  cat("✅ ICD-11 mapping complete:\n")
  cat("   • ", format(mapped_count, big.mark = ","), " records mapped to ICD-11\n")
  cat("   • ", total_icd11, " unique ICD-11 codes identified\n")
  cat("   • ", format(nrow(data) - mapped_count, big.mark = ","), " records without ICD-11 mapping\n")

  return(data)
}

#' Apply cached ICD-11 mappings from existing YAML files
#' @param data Data frame with disease data
#' @param input_column Column name used for mapping
#' @return Data frame with cached mappings applied
apply_cached_icd11_mappings <- function(data, input_column) {

  # Try to load existing ICD-11 mappings
  icd11_mappings_path <- here("taxonomy", "icd11", "disease_mappings.yml")

  if (!file.exists(icd11_mappings_path)) {
    cat("ℹ️ No cached ICD-11 mappings file found\n")
    return(data)
  }

  tryCatch({
    icd11_mappings <- yaml::read_yaml(icd11_mappings_path)

    if (is.null(icd11_mappings$mappings)) {
      cat("ℹ️ No mappings found in ICD-11 cache file\n")
      return(data)
    }

    # Collect all cached mappings across confidence levels
    all_cached_mappings <- list()

    for (confidence_level in names(icd11_mappings$mappings)) {
      level_mappings <- icd11_mappings$mappings[[confidence_level]]

      for (disease_name in names(level_mappings)) {
        mapping_info <- level_mappings[[disease_name]]

        if (!is.null(mapping_info$icd11_code) || !is.null(mapping_info$icd11_title)) {
          all_cached_mappings[[disease_name]] <- list(
            icd11_code = mapping_info$icd11_code %||% NA,
            icd11_title = mapping_info$icd11_title %||% NA,
            icd11_category = mapping_info$icd11_category %||% NA,
            icd11_confidence = confidence_level
          )
        }
      }
    }

    if (length(all_cached_mappings) == 0) {
      cat("ℹ️ No usable cached ICD-11 mappings found\n")
      return(data)
    }

    cat("💾 Applying", length(all_cached_mappings), "cached ICD-11 mappings\n")

    # Apply cached mappings where WHO API didn't find results
    cached_applied <- 0

    for (i in seq_len(nrow(data))) {
      disease <- data[[input_column]][i]

      # Only apply cached mapping if WHO API didn't find anything
      if (!is.na(disease) &&
          disease %in% names(all_cached_mappings) &&
          is.na(data$icd11_code[i])) {

        mapping <- all_cached_mappings[[disease]]
        data$icd11_code[i] <- mapping$icd11_code
        data$icd11_title[i] <- mapping$icd11_title
        data$icd11_category[i] <- mapping$icd11_category
        data$icd11_confidence[i] <- mapping$icd11_confidence
        cached_applied <- cached_applied + 1
      }
    }

    if (cached_applied > 0) {
      cat("💾 Applied", cached_applied, "cached ICD-11 mappings\n")
    }

    return(data)

  }, error = function(e) {
    cat("⚠️ Error loading cached ICD-11 mappings:", e$message, "\n")
    return(data)
  })
}

#' Add empty ICD-11 columns to data frame
#' @param data Input data frame
#' @return Data frame with empty ICD-11 columns
add_empty_icd11_columns <- function(data) {
  data$icd11_code <- NA_character_
  data$icd11_title <- NA_character_
  data$icd11_category <- NA_character_
  data$icd11_confidence <- NA_character_
  return(data)
}

#' Export unmapped diseases for manual ICD-11 review
#' @param data Data frame with ICD-11 mapping results
#' @param input_column Column name used for mapping
#' @param output_file Path to export unmapped diseases
export_unmapped_icd11_diseases <- function(data, input_column = "morbidity", output_file = NULL) {

  if (is.null(output_file)) {
    output_file <- here("taxonomy", "icd11", "unmapped_diseases.csv")
  }

  # Find unmapped diseases
  unmapped <- data[is.na(data$icd11_code), ]

  if (nrow(unmapped) == 0) {
    cat("✅ All diseases have ICD-11 mappings\n")
    return(invisible(NULL))
  }

  # Create summary of unmapped diseases
  unmapped_summary <- unmapped %>%
    group_by(.data[[input_column]]) %>%
    summarise(
      frequency = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(frequency))

  # Add empty ICD-11 columns for manual review
  unmapped_summary$suggested_icd11_code <- ""
  unmapped_summary$suggested_icd11_title <- ""
  unmapped_summary$notes <- ""

  # Export for manual review
  write.csv(unmapped_summary, output_file, row.names = FALSE)

  cat("📤 Exported", nrow(unmapped_summary), "unmapped diseases to:", output_file, "\n")
  cat("💡 Review and add ICD-11 mappings manually, then re-import\n")

  return(unmapped_summary)
}

#' Generate ICD-11 mapping quality report
#' @param data Data frame with ICD-11 mapping results
#' @param input_column Column name used for mapping
#' @return List with quality metrics
generate_icd11_quality_report <- function(data, input_column = "morbidity") {

  total_records <- nrow(data)
  mapped_records <- sum(!is.na(data$icd11_code))

  # Confidence level breakdown
  confidence_breakdown <- table(data$icd11_confidence, useNA = "ifany")

  # Unique diseases vs ICD-11 codes
  unique_diseases <- length(unique(data[[input_column]][!is.na(data[[input_column]])]))
  unique_icd11 <- length(unique(data$icd11_code[!is.na(data$icd11_code)]))

  quality_report <- list(
    total_records = total_records,
    mapped_records = mapped_records,
    mapping_rate = round(mapped_records / total_records * 100, 2),
    unique_diseases = unique_diseases,
    unique_icd11_codes = unique_icd11,
    confidence_breakdown = confidence_breakdown,
    consolidation_ratio = round(unique_diseases / unique_icd11, 2)
  )

  cat("📋 ICD-11 Mapping Quality Report:\n")
  cat("   • Total records:", format(total_records, big.mark = ","), "\n")
  cat("   • Mapped to ICD-11:", format(mapped_records, big.mark = ","), "(", quality_report$mapping_rate, "%)\n")
  cat("   • Unique diseases:", unique_diseases, "\n")
  cat("   • Unique ICD-11 codes:", unique_icd11, "\n")
  cat("   • Consolidation ratio:", quality_report$consolidation_ratio, ":1\n")

  if (length(confidence_breakdown) > 0) {
    cat("   • Confidence levels:\n")
    for (conf_level in names(confidence_breakdown)) {
      cat("     -", conf_level, ":", confidence_breakdown[[conf_level]], "\n")
    }
  }

  return(quality_report)
}

cat("✅ Pure ICD-11 mapper functions loaded\n")
cat("💡 Use apply_icd11_mapping(data, input_column, country_config) for WHO API-based ICD-11 mapping\n")