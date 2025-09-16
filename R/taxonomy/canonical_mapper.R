# canonical_mapper.R - Global Base Canonical Disease Mapping System
# Maps raw morbidity data to standardized global base canonical diseases

suppressPackageStartupMessages({
  library(dplyr)
  library(yaml)
  library(here)
  library(stringr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Apply canonical disease mapping
#' @param data Data frame with morbidity column
#' @param country_config Country configuration object
#' @return Data frame with canonical_disease_imc and category_canonical_disease_imc columns
apply_canonical_mapping <- function(data, country_config) {

  if (!"morbidity" %in% names(data)) {
    stop("No morbidity column found in data")
  }

  if (nrow(data) == 0) {
    warning("No data to process for canonical mapping")
    return(data)
  }

  cat("🏷️ Applying global base canonical mapping...\n")
  cat("📊 Input data:", nrow(data), "rows\n")

  # Step 1: Apply country-specific synonym mapping to normalize raw morbidity
  normalized_morbidity <- apply_country_synonyms(data$morbidity, country_config)

  # Step 2: Map normalized morbidity to global base canonical diseases
  canonical_results <- map_to_global_canonical(normalized_morbidity)

  # Step 3: Extract categories and features from canonical diseases
  canonical_features <- extract_canonical_features(canonical_results$canonical_diseases)

  # Add results to data
  data$canonical_disease_imc <- canonical_results$canonical_diseases
  data$category_canonical_disease_imc <- canonical_features$categories

  # Add taxonomy feature flags
  data <- add_canonical_feature_flags(data, canonical_results$canonical_diseases)

  # Summary reporting
  mapped_count <- sum(!is.na(data$canonical_disease_imc) & data$canonical_disease_imc != "Unclassified")
  total_canonical <- length(unique(data$canonical_disease_imc[data$canonical_disease_imc != "Unclassified"]))

  cat("✅ Canonical mapping complete:\n")
  cat("   • ", format(mapped_count, big.mark = ","), " records mapped to canonical diseases\n")
  cat("   • ", total_canonical, " unique canonical diseases identified\n")
  cat("   • ", format(nrow(data) - mapped_count, big.mark = ","), " records remain unclassified\n")

  return(data)
}

#' Apply country-specific synonym mapping
#' @param morbidity_vector Vector of raw morbidity names
#' @param country_config Country configuration object
#' @return Vector of normalized morbidity names
apply_country_synonyms <- function(morbidity_vector, country_config) {

  # Load country-specific taxonomy file
  country_file <- country_config$taxonomy$country_file
  if (is.null(country_file)) {
    cat("ℹ️ No country-specific taxonomy file specified\n")
    return(as.character(morbidity_vector))
  }

  country_path <- here("taxonomy", country_file)
  if (!file.exists(country_path)) {
    cat("⚠️ Country taxonomy file not found:", country_path, "\n")
    return(as.character(morbidity_vector))
  }

  tryCatch({
    country_taxonomy <- yaml::read_yaml(country_path)

    if (is.null(country_taxonomy$synonyms)) {
      cat("ℹ️ No synonyms found in country taxonomy file\n")
      return(as.character(morbidity_vector))
    }

    cat("🌍 Applying", length(country_taxonomy$synonyms), "country-specific synonyms\n")

    # Apply synonym mapping
    normalized <- character(length(morbidity_vector))
    for (i in seq_along(morbidity_vector)) {
      original <- as.character(morbidity_vector[i])

      if (!is.na(original) && original %in% names(country_taxonomy$synonyms)) {
        normalized[i] <- as.character(country_taxonomy$synonyms[[original]])
      } else {
        normalized[i] <- original
      }
    }

    mapped_count <- sum(morbidity_vector %in% names(country_taxonomy$synonyms), na.rm = TRUE)
    cat("✅ Applied synonyms to", mapped_count, "out of", length(morbidity_vector), "entries\n")

    return(normalized)

  }, error = function(e) {
    cat("⚠️ Error loading country taxonomy:", e$message, "\n")
    return(as.character(morbidity_vector))
  })
}

#' Map normalized morbidity to global base canonical diseases
#' @param normalized_morbidity Vector of normalized disease names
#' @return List with canonical_diseases vector
map_to_global_canonical <- function(normalized_morbidity) {

  # Load global base taxonomy
  base_taxonomy_path <- here("taxonomy", "base.yaml")
  if (!file.exists(base_taxonomy_path)) {
    cat("⚠️ Base taxonomy file not found:", base_taxonomy_path, "\n")
    return(list(canonical_diseases = rep("Unclassified", length(normalized_morbidity))))
  }

  tryCatch({
    base_taxonomy <- yaml::read_yaml(base_taxonomy_path)

    if (is.null(base_taxonomy$canonicals)) {
      cat("⚠️ No canonicals found in base taxonomy\n")
      return(list(canonical_diseases = rep("Unclassified", length(normalized_morbidity))))
    }

    canonical_names <- names(base_taxonomy$canonicals)
    cat("📚 Base taxonomy loaded with", length(canonical_names), "canonical diseases\n")

    # Create comprehensive mapping lookup
    canonical_lookup <- create_canonical_lookup(base_taxonomy)

    # Apply canonical mapping with text normalization
    canonical_diseases <- character(length(normalized_morbidity))

    for (i in seq_along(normalized_morbidity)) {
      disease_name <- normalized_morbidity[i]

      if (is.na(disease_name) || disease_name == "") {
        canonical_diseases[i] <- "Unclassified"
        next
      }

      # Try exact match first
      if (disease_name %in% names(canonical_lookup)) {
        canonical_diseases[i] <- canonical_lookup[[disease_name]]
        next
      }

      # Try normalized text matching
      normalized_input <- normalize_disease_text(disease_name)

      # Search through normalized canonical names
      matched_canonical <- find_best_canonical_match(normalized_input, canonical_lookup)

      if (!is.null(matched_canonical)) {
        canonical_diseases[i] <- matched_canonical
      } else {
        canonical_diseases[i] <- "Unclassified"
      }
    }

    return(list(canonical_diseases = canonical_diseases))

  }, error = function(e) {
    cat("⚠️ Error loading base taxonomy:", e$message, "\n")
    return(list(canonical_diseases = rep("Unclassified", length(normalized_morbidity))))
  })
}

#' Create comprehensive canonical lookup from base taxonomy
#' @param base_taxonomy Base taxonomy object
#' @return Named list for disease lookup
create_canonical_lookup <- function(base_taxonomy) {

  lookup <- list()

  # Add canonical names mapping to themselves
  canonical_names <- names(base_taxonomy$canonicals)
  for (name in canonical_names) {
    lookup[[name]] <- name
    # Also add normalized version
    lookup[[normalize_disease_text(name)]] <- name
  }

  # Add explicit synonyms from base taxonomy
  if (!is.null(base_taxonomy$synonyms)) {
    for (synonym in names(base_taxonomy$synonyms)) {
      canonical_target <- base_taxonomy$synonyms[[synonym]]
      if (canonical_target %in% canonical_names) {
        lookup[[synonym]] <- canonical_target
        lookup[[normalize_disease_text(synonym)]] <- canonical_target
      }
    }
  }

  cat("🔗 Created canonical lookup with", length(lookup), "mappings\n")
  return(lookup)
}

#' Normalize disease text for matching
#' @param text Disease name text
#' @return Normalized text
normalize_disease_text <- function(text) {
  if (is.na(text) || is.null(text)) return(text)

  # Convert to lowercase
  text <- tolower(text)

  # Remove extra whitespace
  text <- str_squish(text)

  # Standardize common variations
  text <- str_replace_all(text, c(
    "diarrhoea" = "diarrhea",
    "ari" = "acute respiratory infection",
    "urti" = "upper respiratory tract infection",
    "lrti" = "lower respiratory tract infection"
  ))

  return(text)
}

#' Find best canonical match using fuzzy matching
#' @param normalized_input Normalized input disease name
#' @param canonical_lookup Canonical lookup list
#' @return Best matching canonical disease or NULL
find_best_canonical_match <- function(normalized_input, canonical_lookup) {

  # Get all normalized canonical names
  normalized_canonicals <- names(canonical_lookup)

  # Look for partial matches (simple approach)
  for (canonical_norm in normalized_canonicals) {
    if (str_detect(normalized_input, fixed(canonical_norm)) ||
        str_detect(canonical_norm, fixed(normalized_input))) {
      return(canonical_lookup[[canonical_norm]])
    }
  }

  return(NULL)
}

#' Extract categories and features from canonical diseases
#' @param canonical_diseases Vector of canonical disease names
#' @return List with categories and features
extract_canonical_features <- function(canonical_diseases) {

  # Load base taxonomy for category lookup
  base_taxonomy_path <- here("taxonomy", "base.yaml")

  if (!file.exists(base_taxonomy_path)) {
    return(list(categories = rep("Uncategorized", length(canonical_diseases))))
  }

  tryCatch({
    base_taxonomy <- yaml::read_yaml(base_taxonomy_path)
    canonicals <- base_taxonomy$canonicals %||% list()

    # Extract categories
    categories <- character(length(canonical_diseases))

    for (i in seq_along(canonical_diseases)) {
      disease <- canonical_diseases[i]

      if (!is.na(disease) && disease != "Unclassified" && disease %in% names(canonicals)) {
        disease_info <- canonicals[[disease]]
        categories[i] <- disease_info$category %||% "Uncategorized"
      } else {
        categories[i] <- "Uncategorized"
      }
    }

    return(list(categories = categories))

  }, error = function(e) {
    cat("⚠️ Error extracting canonical features:", e$message, "\n")
    return(list(categories = rep("Uncategorized", length(canonical_diseases))))
  })
}

#' Add canonical-based feature flags
#' @param data Data frame with canonical_disease_imc column
#' @param canonical_diseases Vector of canonical disease names
#' @return Data frame with feature flags
add_canonical_feature_flags <- function(data, canonical_diseases) {

  # Load base taxonomy for attributes
  base_taxonomy_path <- here("taxonomy", "base.yaml")

  if (!file.exists(base_taxonomy_path)) {
    # Add placeholder flags
    data$vaccine_preventable <- FALSE
    data$climate_sensitive <- FALSE
    data$outbreak_prone_case <- FALSE
    data$epidemic_prone <- FALSE
    return(data)
  }

  tryCatch({
    base_taxonomy <- yaml::read_yaml(base_taxonomy_path)
    canonicals <- base_taxonomy$canonicals %||% list()

    # Initialize flags
    data$vaccine_preventable <- FALSE
    data$climate_sensitive <- FALSE
    data$outbreak_prone_case <- FALSE
    data$epidemic_prone <- FALSE
    data$trauma_related <- FALSE
    data$amr_relevant <- FALSE

    # Apply flags based on canonical attributes
    for (i in seq_along(canonical_diseases)) {
      disease <- canonical_diseases[i]

      if (!is.na(disease) && disease != "Unclassified" && disease %in% names(canonicals)) {
        disease_info <- canonicals[[disease]]

        # Check attributes
        if ("attributes" %in% names(disease_info)) {
          attributes <- disease_info$attributes %||% character(0)

          if ("VPD" %in% attributes) {
            data$vaccine_preventable[i] <- TRUE
          }

          if ("Climate-sensitive" %in% attributes) {
            data$climate_sensitive[i] <- TRUE
          }

          if ("Outbreak-prone" %in% attributes) {
            data$outbreak_prone_case[i] <- TRUE
            data$epidemic_prone[i] <- TRUE
          }

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

    # Summary of flags applied
    flag_counts <- list(
      vaccine_preventable = sum(data$vaccine_preventable),
      climate_sensitive = sum(data$climate_sensitive),
      outbreak_prone_case = sum(data$outbreak_prone_case),
      trauma_related = sum(data$trauma_related),
      amr_relevant = sum(data$amr_relevant)
    )

    cat("🏷️ Canonical feature flags applied:\n")
    for (flag_name in names(flag_counts)) {
      if (flag_counts[[flag_name]] > 0) {
        cat("   •", flag_name, ":", flag_counts[[flag_name]], "cases\n")
      }
    }

    return(data)

  }, error = function(e) {
    cat("⚠️ Error adding canonical feature flags:", e$message, "\n")
    return(data)
  })
}

cat("✅ Canonical mapper functions loaded\n")
cat("💡 Use apply_canonical_mapping(data, country_config) for global base canonical mapping\n")