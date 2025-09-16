# ai_fallbacks.R - Fallback functions when AI is unavailable
# Consolidated fallback implementations
# VERSION: 1.0.0

library(dplyr)

# =============================================================================
# FALLBACK SUMMARY FUNCTIONS
# =============================================================================

#' Generate basic epidemic alerts without AI
#' @param register Health register data
#' @return Basic alert text
generate_basic_epidemic_alerts <- function(register) {
  # Get epidemic diseases
  if (exists("get_epidemic_diseases")) {
    epidemic_diseases <- tryCatch(get_epidemic_diseases(), error = function(e) character())
  } else {
    epidemic_diseases <- c(
      "Acute Watery Diarrhea", "Cholera", "Dysentery",
      "Measles", "Rubella", "Meningitis", "Tuberculosis",
      "Hepatitis", "Malaria", "Typhoid"
    )
  }
  
  if (is.null(register) || nrow(register) == 0) {
    return("**No surveillance data available** for epidemic alert generation.")
  }
  
  disease_col <- dplyr::case_when(
    "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
    "morbidity" %in% names(register) ~ "morbidity",
    "icd11_title" %in% names(register) ~ "icd_11_title",
    "category_canonical_disease_imc" %in% names(register) ~ "category_canonical_disease_imc",
    "disease" %in% names(register) ~ "disease",
    TRUE ~ NA_character_
  )
  
  if (is.na(disease_col)) {
    return("**Disease data not available** for alert generation.")
  }
  
  # Check for epidemic disease cases
  epidemic_matches <- register %>%
    dplyr::filter(.data[[disease_col]] %in% epidemic_diseases) %>%
    dplyr::count(.data[[disease_col]], sort = TRUE)
  
  if (nrow(epidemic_matches) == 0) {
    return("**No epidemic-prone diseases detected** in current surveillance period.")
  }
  
  # Create basic alert summary
  alerts <- epidemic_matches %>%
    dplyr::slice_head(n = 5) %>%
    dplyr::transmute(alert = paste0("- **", .data[[disease_col]], "**: ", n, " cases")) %>%
    dplyr::pull(alert)
  
  alert_text <- paste0(
    "**Epidemic Disease Surveillance** (", nrow(epidemic_matches), " disease types detected):\n\n",
    paste(alerts, collapse = "\n"), "\n\n",
    "*Enhanced AI analysis not available. Basic rule-based detection applied.*"
  )
  
  return(alert_text)
}

#' Fallback AI summaries when AI system is unavailable
#' @param register Health register data
#' @param ai_system AI system (unused in fallback)
#' @return Basic summary list
generate_fallback_ai_summaries <- function(register, ai_system) {
  if (is.null(register) || nrow(register) == 0) {
    return(list(
      monthly_summary = "**No surveillance data available** for monthly analysis.",
      geographic_insights = "**No geographic data available** for spatial analysis.",
      demographic_insights = "**No demographic data available** for population analysis.",
      disease_narratives = list(),
      facility_performance = "**No facility data available** for performance analysis.",
      risk_assessment = "**No data available** for risk assessment.",
      analysis_quality = 0
    ))
  }
  
  # Basic data summary
  total_cases <- nrow(register)
  
  # Geographic summary (basic)
  geo_summary <- if ("admin1" %in% names(register)) {
    top_regions <- register %>%
      dplyr::count(admin1, sort = TRUE) %>%
      dplyr::slice_head(n = 3) %>%
      dplyr::transmute(summary = paste0(admin1, ": ", n, " cases")) %>%
      dplyr::pull(summary) %>%
      paste(collapse = ", ")
    
    paste0("**Geographic Distribution**: Top regions - ", top_regions, 
           ". Geographic coverage: ", dplyr::n_distinct(register$admin1, na.rm = TRUE), " regions.")
  } else {
    "**Geographic analysis not available** - region data missing."
  }
  
  # Disease summary (basic)
  disease_summary <- tryCatch({
    disease_col <- dplyr::case_when(
      "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
      "morbidity" %in% names(register) ~ "morbidity",
      "icd11_title" %in% names(register) ~ "icd_11_title",
      "category_canonical_disease_imc" %in% names(register) ~ "category_canonical_disease_imc",
      "disease" %in% names(register) ~ "disease",
      TRUE ~ NA_character_
    )
    
    if (!is.na(disease_col)) {
      top_diseases <- register %>%
        dplyr::count(.data[[disease_col]], sort = TRUE) %>%
        dplyr::slice_head(n = 5) %>%
        dplyr::transmute(summary = paste0(.data[[disease_col]], ": ", n, " cases (", 
                                         round(100 * n / total_cases, 1), "%)")) %>%
        dplyr::pull(summary) %>%
        paste(collapse = "; ")
      
      paste0("**Disease Distribution**: ", top_diseases)
    } else {
      "**Disease analysis not available** - disease data missing."
    }
  }, error = function(e) {
    "**Disease analysis error** - unable to process disease data."
  })
  
  # Demographic summary (basic)
  demo_summary <- tryCatch({
    if (any(c("age_value", "sex_value") %in% names(register))) {
      demo_counts <- list()
      
      if ("sex_value" %in% names(register)) {
        sex_dist <- register %>%
          dplyr::count(sex_value, sort = TRUE) %>%
          dplyr::filter(!is.na(sex_value)) %>%
          dplyr::transmute(summary = paste0(sex_value, ": ", n)) %>%
          dplyr::pull(summary) %>%
          paste(collapse = ", ")
        demo_counts <- c(demo_counts, paste("Gender distribution:", sex_dist))
      }
      
      if ("age_value" %in% names(register)) {
        under5 <- sum(register$age_value < 5, na.rm = TRUE)
        if (under5 > 0) {
          demo_counts <- c(demo_counts, paste("Under-5 cases:", under5, 
                                            paste0("(", round(100 * under5 / total_cases, 1), "%)")))
        }
      }
      
      if (length(demo_counts) > 0) {
        paste0("**Demographic Patterns**: ", paste(demo_counts, collapse = "; "))
      } else {
        "**Demographic analysis available** but no significant patterns detected."
      }
    } else {
      "**Demographic analysis not available** - age/sex data missing."
    }
  }, error = function(e) {
    "**Demographic analysis error** - unable to process demographic data."
  })
  
  # Facility summary (basic)
  facility_summary <- if ("health_facility" %in% names(register)) {
    facility_count <- dplyr::n_distinct(register$health_facility, na.rm = TRUE)
    top_facility <- register %>%
      dplyr::count(health_facility, sort = TRUE) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::pull(health_facility)
    
    paste0("**Facility Performance**: ", facility_count, " active facilities. ",
           "Highest volume: ", top_facility, ".")
  } else {
    "**Facility analysis not available** - facility data missing."
  }
  
  # Monthly summary combining all elements
  monthly_summary <- paste0(
    "**Monthly Surveillance Summary** (Standard Analysis)\n\n",
    "**Total Cases**: ", scales::comma(total_cases), " consultations recorded.\n\n",
    disease_summary, "\n\n",
    geo_summary, "\n\n", 
    demo_summary, "\n\n",
    "*This analysis uses standard statistical methods. Enhanced AI analysis is not currently available.*"
  )
  
  return(list(
    monthly_summary = monthly_summary,
    geographic_insights = geo_summary,
    demographic_insights = demo_summary,
    disease_narratives = list(),
    facility_performance = facility_summary,
    risk_assessment = "**Risk assessment not available** - requires AI analysis capabilities.",
    analysis_quality = 60
  ))
}

#' Fallback format for AI summary display
#' @param summary Summary text
#' @param type Summary type
#' @param include_header Whether to include header
#' @return Formatted fallback summary
format_fallback_summary <- function(summary, type = "general", include_header = TRUE) {
  if (is.null(summary) || !is.character(summary) || nchar(summary) == 0) {
    summary <- "Analysis not available using standard methods."
  }
  
  if (include_header) {
    header <- "## Standard Statistical Analysis"
    footer <- "\n\n*Enhanced AI analysis not available.*"
    return(paste0(header, "\n\n", summary, footer))
  } else {
    return(paste0(summary, "\n\n*Standard analysis only.*"))
  }
}

#' Simple outbreak detection without AI
#' @param register Health register data
#' @return Basic outbreak assessment
generate_basic_outbreak_assessment <- function(register) {
  if (is.null(register) || nrow(register) == 0 || !"datevisit" %in% names(register)) {
    return("**Outbreak assessment not available** - insufficient data.")
  }
  
  tryCatch({
    # Simple threshold-based detection
    recent_data <- register %>%
      dplyr::filter(!is.na(datevisit)) %>%
      dplyr::arrange(dplyr::desc(datevisit)) %>%
      dplyr::slice_head(n = min(500, nrow(register)))
    
    if (nrow(recent_data) < 20) {
      return("**Outbreak assessment**: Insufficient recent cases for threshold analysis.")
    }
    
    # Check for geographic clustering
    geo_clusters <- recent_data %>%
      dplyr::count(admin1, sort = TRUE) %>%
      dplyr::filter(n > 5) %>%
      nrow()
    
    # Check for disease clustering 
    disease_col <- dplyr::case_when(
      "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
      "morbidity" %in% names(register) ~ "morbidity",
      "icd11_title" %in% names(register) ~ "icd_11_title",
      "category_canonical_disease_imc" %in% names(register) ~ "category_canonical_disease_imc",
      "disease" %in% names(register) ~ "disease",
      TRUE ~ NA_character_
    )
    
    disease_clusters <- if (!is.na(disease_col)) {
      recent_data %>%
        dplyr::count(.data[[disease_col]], sort = TRUE) %>%
        dplyr::filter(n > 10) %>%
        nrow()
    } else {
      0
    }
    
    # Simple risk classification
    risk_level <- if (geo_clusters >= 3 && disease_clusters >= 2) {
      "MODERATE"
    } else if (geo_clusters >= 2 || disease_clusters >= 1) {
      "LOW-MODERATE" 
    } else {
      "LOW"
    }
    
    return(paste0(
      "**Basic Outbreak Risk Assessment**: ", risk_level, " risk level detected.\n",
      "- Geographic clusters: ", geo_clusters, " regions with elevated activity\n",
      "- Disease clusters: ", disease_clusters, " conditions with notable increases\n\n",
      "*Standard threshold-based detection. Enhanced AI risk modeling not available.*"
    ))
    
  }, error = function(e) {
    return(paste("**Outbreak assessment error**:", e$message))
  })
}

cat("✅ AI Fallbacks module loaded successfully\n")