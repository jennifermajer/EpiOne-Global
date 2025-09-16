# R/synthetic_data.R — Synthetic register generator for testing dashboard and reports

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

# Lightweight helpers for standalone sourcing
if (!exists("%||%")) `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

.path_here <- function(...) {
  if (requireNamespace("here", quietly = TRUE)) {
    return(here::here(...))
  }
  # Fallback: simple join from working directory
  return(do.call(file.path, as.list(c(".", list(...)))))
}

#' Generate a synthetic register with required columns
#'
#' This dataset includes minimally all variables referenced downstream:
#' sex, age_group, morbidity, orgunit, admin0..admin3, datevisit, year,
#' month, quarter, month2, canonical_disease_imc, category_canonical_disease_imc,
#' feature flags, and ICD-11 variables (icd11_title, icd11_category).
#'
#' @param n Integer, number of rows
#' @param months Integer, number of months back from today to simulate
#' @param country Character, country context to mimic (affects admin names)
#' @param facilities Integer, number of facilities to simulate
#' @return data.frame in the standard register shape
generate_synthetic_register <- function(n = 5000,
                                        months = 12,
                                        country = "syria",
                                        facilities = 50) {
  set.seed(123)

  # Time span
  end_date <- Sys.Date()
  start_date <- end_date %m-% months(months - 1)
  all_days <- seq.Date(from = start_date, to = end_date, by = "day")

  # Geography presets (keep simple and consistent with available shapefiles)
  if (tolower(country) %in% c("syria", "sy")) {
    admin0_name <- "Syria"
    admin1_names <- c("Aleppo", "Damascus", "Homs", "Hama", "Idlib", "Lattakia", "Tartous")
  } else if (tolower(country) %in% c("yemen", "ye")) {
    admin0_name <- "Yemen"
    admin1_names <- c("Aden", "Sana'a", "Taiz", "Ibb", "Hajjah", "Hodeidah")
  } else if (tolower(country) %in% c("south_sudan", "ss")) {
    admin0_name <- "South Sudan"
    admin1_names <- c("Jonglei", "Unity", "Upper Nile", "Central Equatoria", "Warrap")
  } else {
    admin0_name <- "Synthetica"
    admin1_names <- paste("Region", 1:6)
  }

  # Facilities and lower admin levels
  orgunits <- paste0("FAC", sprintf("%04d", 1:facilities))
  admin2_names <- paste("District", 1:20)
  admin3_names <- paste("Subdistrict", 1:60)

  # Morbidity set chosen to cover taxonomy categories and ICD-11 mapping
  diseases <- c(
    # Respiratory infectious
    "Pneumonia", "Acute Upper Respiratory Infection", "Influenza-Like Illness", "COVID-19",
    # Enteric
    "Acute Watery Diarrhoea (Suspected Cholera)", "Acute Bloody Diarrhoea (Suspected Shigellosis)",
    "Typhoid Fever", "Gastroenteritis",
    # Vector-borne
    "Malaria (Suspected)", "Dengue", "Leishmaniasis",
    # VPD
    "Measles", "Diphtheria", "Pertussis",
    # NCD
    "Diabetes Mellitus", "Hypertension", "Asthma",
    # Injury/Trauma
    "Fracture", "Burn", "Trauma",
    # Other common
    "Conjunctivitis", "Dermatitis", "Urinary Tract Infection",
    "Iron Deficiency Anaemia", "Malnutrition"
  )

  # Core frame
  df <- tibble::tibble(
    orgunit   = sample(orgunits, n, replace = TRUE),
    admin0    = admin0_name,
    admin1    = sample(admin1_names, n, replace = TRUE),
    admin2    = sample(admin2_names, n, replace = TRUE),
    admin3    = sample(admin3_names, n, replace = TRUE),
    sex       = sample(c("Female", "Male"), n, replace = TRUE),
    age       = sample(0:90, n, replace = TRUE),
    datevisit = sample(all_days, n, replace = TRUE),
    morbidity = sample(diseases, n, replace = TRUE),
    type_case = sample(c("Trauma", "Non-trauma"), n, replace = TRUE, prob = c(0.15, 0.85))
  )

  # Derived temporal fields
  df <- df %>%
    mutate(
      datevisitnew = datevisit,
      year = year(datevisit),
      month = format(datevisit, "%Y-%m"),
      quarter = paste0(year(datevisit), "-Q", quarter(datevisit)),
      month2 = as.Date(paste0(format(datevisit, "%Y-%m"), "-01"))
    )

  # Age groups (both legacy and alternative forms used around the codebase)
  df <- df %>%
    mutate(
      age_group = cut(age,
                      breaks = c(-Inf, 5, 15, 25, 50, 65, Inf),
                      labels = c("0-4", "5-14", "15-24", "25-49", "50-64", "65+"),
                      right = FALSE),
      age_group_new = cut(age,
                          breaks = c(-Inf, 5, 14, 49, Inf),
                          labels = c("0-5 y", "6-14 y", "15-49 y", "50+ y"),
                          right = TRUE)
    )

  # Provide initial canonical fields; taxonomy will refine these
  df$canonical_disease_imc <- df$morbidity
  df$category_canonical_disease_imc <- NA_character_

  # Apply modular taxonomy system for proper canonical mapping and ICD-11 integration
  tryCatch({
    # Load the modular taxonomy system
    preprocessing_path <- .path_here("R", "preprocessing.R")
    if (file.exists(preprocessing_path)) {
      source(preprocessing_path, local = TRUE)
    }

    # Try the new modular taxonomy system first
    if (exists("apply_simplified_taxonomy", envir = .GlobalEnv)) {
      message("[synthetic] Using modular taxonomy system")
      df <- apply_simplified_taxonomy(df)
    } else {
      # Fallback to canonical mapper directly
      canonical_mapper_path <- .path_here("R", "taxonomy", "canonical_mapper.R")
      if (file.exists(canonical_mapper_path)) {
        source(canonical_mapper_path, local = TRUE)
        if (exists("apply_canonical_mapping", envir = .GlobalEnv)) {
          message("[synthetic] Using canonical mapper")
          country_config <- list(
            taxonomy = list(icd11_enabled = "yes", country_file = NULL)
          )
          df <- apply_canonical_mapping(df, country_config)
        }
      }

      # Add ICD-11 columns if not present
      icd11_cols <- c("icd11_code", "icd11_title", "icd11_category", "icd11_confidence")
      for (col in icd11_cols) {
        if (!col %in% names(df)) {
          df[[col]] <- if (col == "icd11_confidence") "medium" else "Unclassified"
        }
      }
    }
  }, error = function(e) {
    message("[synthetic] Modular taxonomy enrichment failed: ", e$message)
    # Fallback to old system if new system fails
    tryCatch({
      taxonomy_files <- c(
        .path_here("R", "taxonomy", "icd11_integration.R"),
        .path_here("R", "taxonomy", "icd11_disease_mapper.R"),
        .path_here("R", "taxonomy", "icd11_taxonomy_mapper.R")
      )
      for (f in taxonomy_files) if (file.exists(f)) sys.source(f, envir = .GlobalEnv)

      if (exists("apply_icd11_taxonomy")) {
        country_yaml <- .path_here("config", "countries", paste0(tolower(country), ".yml"))
        country_config <- tryCatch({
          if (file.exists(country_yaml)) yaml::read_yaml(country_yaml) else list()
        }, error = function(e) list())
        df <- apply_icd11_taxonomy(df, country_config)
      }
    }, error = function(e2) {
      message("[synthetic] All taxonomy systems failed: ", e2$message)
    })
  })

  # Ensure all expected feature flag and ICD-11 columns exist even if taxonomy isn't available
  required_flag_cols <- c("vaccine_preventable", "climate_sensitive", "outbreak_prone_case",
                          "trauma_related", "amr_relevant", "epidemic_prone")
  for (col in required_flag_cols) if (!col %in% names(df)) df[[col]] <- FALSE

  # Ensure ICD-11 columns exist
  icd11_cols <- c("icd11_code", "icd11_title", "icd11_category", "icd11_confidence")
  for (col in icd11_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- if (col == "icd11_confidence") "medium" else "Unclassified"
    }
  }

  # Ensure required chart columns exist
  if (!"age_group4" %in% names(df)) {
    df$age_group4 <- cut(df$age,
                         breaks = c(-Inf, 15, 25, 50, Inf),
                         labels = c("0-14", "15-24", "25-49", "50+"),
                         right = FALSE)
  }

  if (!"malnutrition_related" %in% names(df)) {
    df$malnutrition_related <- grepl("malnutrition|marasmus|kwashiorkor", df$morbidity, ignore.case = TRUE)
  }

  # Ensure category_canonical_disease_imc is not NA (fallback)
  if (all(is.na(df$category_canonical_disease_imc))) {
    # Simple fallback categorization based on disease names
    df$category_canonical_disease_imc <- dplyr::case_when(
      grepl("pneumonia|respiratory|influenza|covid", df$morbidity, ignore.case = TRUE) ~ "Respiratory Infectious Diseases",
      grepl("diarrhoea|cholera|typhoid|gastroenteritis", df$morbidity, ignore.case = TRUE) ~ "Enteric & Food/Water-borne Infections",
      grepl("malaria|dengue|leishmaniasis", df$morbidity, ignore.case = TRUE) ~ "Vector-borne Diseases",
      grepl("measles|diphtheria|pertussis", df$morbidity, ignore.case = TRUE) ~ "Vaccine-Preventable Diseases",
      grepl("diabetes|hypertension|asthma", df$morbidity, ignore.case = TRUE) ~ "Non-Communicable Diseases (non-MNS)",
      grepl("fracture|burn|trauma", df$morbidity, ignore.case = TRUE) ~ "Injury & External Causes",
      grepl("malnutrition", df$morbidity, ignore.case = TRUE) ~ "Nutrition",
      grepl("conjunctivitis|dermatitis", df$morbidity, ignore.case = TRUE) ~ "Skin, Eye & ENT Disorders",
      TRUE ~ "Other Conditions"
    )
  }

  df
}

#' Helper to resolve synthetic generation parameters from a config-like list
#' @param cfg list with testing$synthetic entries
#' @return list of args suitable for generate_synthetic_register()
resolve_synthetic_args <- function(cfg) {
  syn <- cfg$testing$synthetic %||% list()
  list(
    n = syn$n %||% 5000,
    months = syn$months %||% 12,
    country = syn$country %||% (cfg$app$default_country %||% "syria"),
    facilities = syn$facilities %||% 50
  )
}
