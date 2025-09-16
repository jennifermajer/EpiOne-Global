# icd11_disease_mapper.R - Disease Mapping to ICD-11 Codes
# Maps local disease names from different countries to standardized ICD-11 codes

library(dplyr)
library(yaml)
library(here)

source(here("R", "taxonomy", "icd11_integration.R"))

#' Create ICD-11 disease mappings from existing taxonomy
#' @param force_refresh Force refresh of all mappings from API
#' @return Data frame with disease mappings to ICD-11
create_icd11_disease_mappings <- function(force_refresh = FALSE) {
  
  cat("🗺️ Creating ICD-11 disease mappings...\n")
  
  # Initialize ICD-11 system
  icd11_available <- initialize_icd11_system()
  
  # Load existing disease names from all sources
  all_diseases <- collect_all_disease_names()
  
  if (length(all_diseases) == 0) {
    warning("No diseases found to map")
    return(data.frame())
  }
  
  cat("📋 Found", length(all_diseases), "unique disease names to map\n")
  
  # Check for existing mappings
  mappings_file <- file.path(ICD11_CONFIG$cache_dir, "disease_mappings.rds")
  
  existing_mappings <- data.frame()
  if (!force_refresh && file.exists(mappings_file)) {
    existing_mappings <- readRDS(mappings_file)
    cat("📚 Loaded", nrow(existing_mappings), "existing mappings\n")
  }
  
  # Find diseases that need mapping
  if (nrow(existing_mappings) > 0) {
    unmapped_diseases <- setdiff(all_diseases, existing_mappings$local_name)
  } else {
    unmapped_diseases <- all_diseases
  }
  
  if (length(unmapped_diseases) == 0) {
    cat("✅ All diseases already mapped\n")
    return(existing_mappings)
  }
  
  cat("🔍 Need to map", length(unmapped_diseases), "new diseases\n")
  
  # Create new mappings
  new_mappings <- data.frame()
  
  if (icd11_available) {
    # Use ICD-11 API for new mappings
    new_mappings <- create_api_mappings(unmapped_diseases)
  } else {
    # Create manual mappings based on known patterns
    cat("⚠️ API not available, creating manual mappings\n")
    new_mappings <- create_manual_icd11_mappings(unmapped_diseases)
  }
  
  # Combine with existing mappings
  all_mappings <- rbind(existing_mappings, new_mappings)
  all_mappings <- all_mappings[!duplicated(all_mappings$local_name), ]
  
  # Save updated mappings
  saveRDS(all_mappings, mappings_file)
  cat("💾 Saved", nrow(all_mappings), "disease mappings\n")
  
  # Create human-readable mapping file
  export_mappings_to_yaml(all_mappings)
  
  return(all_mappings)
}

#' Collect all disease names from existing taxonomy files
#' @return Vector of unique disease names
collect_all_disease_names <- function() {
  
  all_diseases <- c()
  
  # From current base taxonomy
  base_file <- here("taxonomy", "base.yaml")
  if (file.exists(base_file)) {
    base_taxonomy <- yaml::read_yaml(base_file)
    if (!is.null(base_taxonomy$canonicals)) {
      all_diseases <- c(all_diseases, names(base_taxonomy$canonicals))
    }
  }
  
  # From country-specific taxonomies
  country_files <- list.files(here("taxonomy"), pattern = "\\.yml$", full.names = TRUE)
  
  for (file in country_files) {
    country_taxonomy <- yaml::read_yaml(file)
    if (!is.null(country_taxonomy$synonyms)) {
      all_diseases <- c(all_diseases, names(country_taxonomy$synonyms))
      all_diseases <- c(all_diseases, unlist(country_taxonomy$synonyms))
    }
  }
  
  # From country configuration files
  config_files <- list.files(here("config", "countries"), pattern = "\\.yml$", full.names = TRUE)
  
  for (file in config_files) {
    country_config <- yaml::read_yaml(file)
    
    # From disease mappings
    if (!is.null(country_config$disease_mappings)) {
      if (!is.null(country_config$disease_mappings$excel_names)) {
        all_diseases <- c(all_diseases, names(country_config$disease_mappings$excel_names))
        all_diseases <- c(all_diseases, unlist(country_config$disease_mappings$excel_names))
      }
      if (!is.null(country_config$dhis2_mappings$data_elements)) {
        all_diseases <- c(all_diseases, names(country_config$dhis2_mappings$data_elements))
        all_diseases <- c(all_diseases, unlist(country_config$dhis2_mappings$data_elements))
      }
    }
    
    # From priority diseases
    if (!is.null(country_config$taxonomy$priority_diseases)) {
      all_diseases <- c(all_diseases, country_config$taxonomy$priority_diseases)
    }
  }
  
  # Clean and deduplicate
  all_diseases <- unique(all_diseases[!is.na(all_diseases) & all_diseases != ""])
  
  return(all_diseases)
}

#' Create mappings using ICD-11 API
#' @param disease_names Vector of disease names to map
#' @return Data frame with API-based mappings
create_api_mappings <- function(disease_names) {
  
  cat("🌐 Creating API-based ICD-11 mappings...\n")
  
  # Batch search all diseases
  search_results <- batch_search_icd11(disease_names, max_results_per_disease = 3)
  
  if (nrow(search_results) == 0) {
    cat("⚠️ No API search results found\n")
    return(data.frame())
  }
  
  # Process search results to create best mappings
  mappings <- search_results %>%
    group_by(query) %>%
    # Take the highest scoring result for each disease
    slice_max(search_score, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      local_name = query,
      icd11_code = icd11_code,
      icd11_id = icd11_id,
      icd11_title = title,
      icd11_definition = definition,
      icd11_category = category,
      confidence = case_when(
        search_score >= 0.9 ~ "high",
        search_score >= 0.7 ~ "medium", 
        TRUE ~ "low"
      ),
      mapping_source = "icd11_api",
      mapping_date = Sys.Date(),
      needs_review = confidence == "low"
    )
  
  cat("✅ Created", nrow(mappings), "API-based mappings\n")
  return(mappings)
}

#' Create manual ICD-11 mappings based on known patterns
#' @param disease_names Vector of disease names to map
#' @return Data frame with manual mappings
create_manual_icd11_mappings <- function(disease_names) {
  
  cat("📝 Creating manual ICD-11 mappings...\n")
  
  # Predefined mapping patterns for common diseases
  manual_mappings <- list(
    # Infectious diseases - Diarrheal
    "Acute watery diarrhea" = list(code = "1A40", title = "Acute diarrhoea", category = "Infectious diseases"),
    "Cholera" = list(code = "1A00", title = "Cholera", category = "Infectious diseases"),
    "Bloody diarrhea" = list(code = "1A41", title = "Bloody diarrhoea", category = "Infectious diseases"),
    
    # Infectious diseases - Respiratory
    "Upper respiratory tract infections" = list(code = "CA07", title = "Upper respiratory tract infection", category = "Respiratory diseases"),
    "Lower respiratory tract infections" = list(code = "CA40", title = "Lower respiratory tract infection", category = "Respiratory diseases"),
    "Pneumonia" = list(code = "CA40.0", title = "Pneumonia", category = "Respiratory diseases"),
    
    # Vector-borne diseases
    "Malaria" = list(code = "1F40", title = "Malaria", category = "Infectious diseases"),
    "Dengue fever" = list(code = "1D26", title = "Dengue", category = "Infectious diseases"),
    "Chikungunya" = list(code = "1D24", title = "Chikungunya", category = "Infectious diseases"),
    
    # Vaccine-preventable
    "Measles" = list(code = "1F03", title = "Measles", category = "Infectious diseases"),
    "Diphtheria" = list(code = "1C15", title = "Diphtheria", category = "Infectious diseases"),
    "Meningitis" = list(code = "8B00", title = "Meningitis", category = "Nervous system diseases"),
    
    # Hepatitis
    "Hepatitis A" = list(code = "1E50.0", title = "Hepatitis A", category = "Infectious diseases"),
    "Hepatitis B" = list(code = "1E51.0", title = "Hepatitis B", category = "Infectious diseases"),
    "Hepatitis E" = list(code = "1E51.4", title = "Hepatitis E", category = "Infectious diseases"),
    
    # Other
    "Leishmaniasis" = list(code = "1F52", title = "Leishmaniasis", category = "Infectious diseases"),
    "Kala-azar" = list(code = "1F52.0", title = "Visceral leishmaniasis", category = "Infectious diseases")
  )
  
  # Create mappings for diseases that match our manual list
  mapped_diseases <- intersect(disease_names, names(manual_mappings))
  
  if (length(mapped_diseases) == 0) {
    cat("⚠️ No diseases match manual mapping patterns\n")
    return(data.frame())
  }
  
  mappings <- data.frame(
    local_name = mapped_diseases,
    icd11_code = sapply(mapped_diseases, function(x) manual_mappings[[x]]$code),
    icd11_id = NA,  # Not available for manual mappings
    icd11_title = sapply(mapped_diseases, function(x) manual_mappings[[x]]$title),
    icd11_definition = NA,
    icd11_category = sapply(mapped_diseases, function(x) manual_mappings[[x]]$category),
    confidence = "high",  # Manual mappings are curated
    mapping_source = "manual",
    mapping_date = Sys.Date(),
    needs_review = FALSE,
    stringsAsFactors = FALSE
  )
  
  # For unmapped diseases, create placeholder mappings that need review
  unmapped_diseases <- setdiff(disease_names, mapped_diseases)
  
  if (length(unmapped_diseases) > 0) {
    placeholder_mappings <- data.frame(
      local_name = unmapped_diseases,
      icd11_code = NA,
      icd11_id = NA,
      icd11_title = NA,
      icd11_definition = NA,
      icd11_category = "Unknown",
      confidence = "none",
      mapping_source = "placeholder",
      mapping_date = Sys.Date(),
      needs_review = TRUE,
      stringsAsFactors = FALSE
    )
    
    mappings <- rbind(mappings, placeholder_mappings)
  }
  
  cat("✅ Created", nrow(mappings), "manual mappings (", length(mapped_diseases), "matched,", length(unmapped_diseases), "need review)\n")
  return(mappings)
}

#' Export mappings to human-readable YAML format
#' @param mappings Data frame of disease mappings
export_mappings_to_yaml <- function(mappings) {
  
  yaml_file <- here("taxonomy", "icd11_disease_mappings.yml")
  
  # Group by confidence and source for better organization
  high_confidence <- mappings[mappings$confidence == "high" & !mappings$needs_review, ]
  medium_confidence <- mappings[mappings$confidence == "medium", ]
  needs_review <- mappings[mappings$needs_review, ]
  
  yaml_content <- list(
    metadata = list(
      created = Sys.Date(),
      source = "ICD-11 API + Manual Curation",
      total_mappings = nrow(mappings),
      high_confidence = nrow(high_confidence),
      medium_confidence = nrow(medium_confidence),
      needs_review = nrow(needs_review)
    ),
    
    high_confidence_mappings = create_yaml_mapping_section(high_confidence),
    medium_confidence_mappings = create_yaml_mapping_section(medium_confidence),
    needs_review_mappings = create_yaml_mapping_section(needs_review)
  )
  
  yaml::write_yaml(yaml_content, yaml_file)
  cat("📄 Exported mappings to:", yaml_file, "\n")
}

#' Create YAML mapping section
#' @param mapping_subset Subset of mappings data frame
#' @return List formatted for YAML export
create_yaml_mapping_section <- function(mapping_subset) {
  
  if (nrow(mapping_subset) == 0) {
    return(list())
  }
  
  mappings_list <- list()
  
  for (i in 1:nrow(mapping_subset)) {
    row <- mapping_subset[i, ]
    
    mappings_list[[row$local_name]] <- list(
      icd11_code = if (is.na(row$icd11_code)) NULL else row$icd11_code,
      icd11_title = if (is.na(row$icd11_title)) NULL else row$icd11_title,
      icd11_category = if (is.na(row$icd11_category)) NULL else row$icd11_category,
      confidence = row$confidence,
      source = row$mapping_source
    )
  }
  
  return(mappings_list)
}

#' Load ICD-11 disease mappings
#' @return Data frame with disease mappings
load_icd11_mappings <- function() {
  
  mappings_file <- file.path(ICD11_CONFIG$cache_dir, "disease_mappings.rds")
  
  if (!file.exists(mappings_file)) {
    cat("⚠️ No ICD-11 mappings found. Run create_icd11_disease_mappings() first.\n")
    return(data.frame())
  }
  
  mappings <- readRDS(mappings_file)
  cat("📚 Loaded", nrow(mappings), "ICD-11 disease mappings\n")
  
  return(mappings)
}

cat("✅ ICD-11 disease mapper functions loaded\n")
cat("💡 Run create_icd11_disease_mappings() to build disease mappings\n")