# disease_categories_taxaware.R – Taxonomy-aware classification (v4.5.0)
# FIXES APPLIED:
# 1. Fixed missing case_when function
# 2. Fixed missing purrr functions
# 3. Added fallback functions for missing dependencies
# 4. Fixed taxonomy loading errors
# 5. Improved error handling

suppressPackageStartupMessages({
  if (!requireNamespace("yaml", quietly = TRUE)) stop("Please install 'yaml'.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Please install 'dplyr'.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Please install 'stringr'.")
  if (!requireNamespace("lubridate", quietly = TRUE)) stop("Please install 'lubridate'.")
  if (!requireNamespace("purrr", quietly = TRUE)) stop("Please install 'purrr'.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Please install 'tibble'.")
})

# ================================================================================
# Utils & cache (enhanced with missing functions)
# ================================================================================
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# CRITICAL FIX: Add missing case_when function if not available
if (!exists("case_when")) {
  case_when <- function(...) {
    dots <- list(...)
    n <- length(dots)
    
    if (n == 0) return(logical(0))
    
    # Get the length from the first condition
    first_condition <- eval(dots[[1]][[2]], parent.frame())
    result_length <- length(first_condition)
    result <- rep(NA, result_length)
    
    for (i in seq_len(n)) {
      formula <- dots[[i]]
      if (inherits(formula, "formula")) {
        condition <- eval(formula[[2]], parent.frame())
        value <- eval(formula[[3]], parent.frame())
        
        # Handle TRUE as catch-all
        if (length(condition) == 1 && condition == TRUE) {
          result[is.na(result)] <- value
        } else {
          result[condition & is.na(result)] <- value
        }
      }
    }
    
    return(result)
  }
}

## Remove config package to avoid taxonomy conflicts
if ("package:config" %in% search()) {
  detach("package:config", unload = TRUE, force = TRUE)
  cat("🔧 Detached config package\n")
}

tryCatch({
  if (exists("get", envir = .GlobalEnv)) {
    rm(get, envir = .GlobalEnv)
    cat("🔧 Removed problematic get function\n")
  }
}, error = function(e) {
  # Silently ignore if get function doesn't exist or can't be removed
  cat("ℹ️ get function not found or already removed\n")
})

# Clear existing taxonomy functions that might have config dependencies
taxonomy_functions <- c("global_canonicalize", "load_taxonomy", "tax_category_lookup", 
                        "apply_morbidity_categorization", "standardize_disease_labels")

for (func in taxonomy_functions) {
  if (exists(func, envir = .GlobalEnv)) {
    rm(list = func, envir = .GlobalEnv)
  }
}
cat("🔧 Cleared existing taxonomy functions\n")

# Create completely clean taxonomy functions
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
.norm <- function(x) {
  x <- tolower(trimws(as.character(x)))
  gsub("[^a-z0-9]", "", x)
}

.clean_cache <- new.env(parent = emptyenv())
.clean_get <- function(k) {
  if (exists(k, .clean_cache, inherits = FALSE)) get(k, .clean_cache) else NULL
}
.clean_set <- function(k, v) assign(k, v, .clean_cache)

# Safe date parser (local fallback so this file is self-contained)
.to_date_safe <- function(x) {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x_chr <- as.character(x)
  d1 <- suppressWarnings(lubridate::ymd(x_chr, quiet = TRUE))
  d2 <- suppressWarnings(lubridate::dmy(x_chr, quiet = TRUE))
  d3 <- suppressWarnings(lubridate::mdy(x_chr, quiet = TRUE))
  as.Date(dplyr::coalesce(d1, d2, d3), origin = "1970-01-01")
}

# ---- Fixed Cache functions (R version compatible) ----
.tax_cache <- new.env(parent = emptyenv())

.cache_get <- function(k) {
  if (exists(k, .tax_cache, inherits = FALSE)) {
    get(k, .tax_cache)
  } else {
    NULL
  }
}

.cache_set <- function(k, v) {
  assign(k, v, .tax_cache)
}

.cache_clear <- function() {
  rm(list = ls(.tax_cache), envir = .tax_cache)
  cat("🧹 Cleared taxonomy cache\n")
}

# ---- Load taxonomy function (MISSING FUNCTION ADDED) ----
load_taxonomy <- function(which = "base") {
  cache_key <- paste0("tax_", which)
  cached <- .cache_get(cache_key)
  if (!is.null(cached)) return(cached)
  
  # Try to find and load the taxonomy file
  tax_file_path <- .find_taxonomy_file(paste0(which, ".yml"))
  
  # Handle special case where we created a minimal taxonomy
  if (startsWith(tax_file_path, "minimal_")) {
    tax <- .cache_get(tax_file_path)
    if (!is.null(tax)) {
      .cache_set(cache_key, tax)
      return(tax)
    }
  }
  
  # Try to load from file
  if (file.exists(tax_file_path)) {
    tryCatch({
      tax <- yaml::read_yaml(tax_file_path)
    }, error = function(e) {
      cat("⚠️ Error loading taxonomy file", tax_file_path, ":", e$message, "\n")
      tax <- NULL
    })
  } else {
    tax <- NULL
  }
  
  # If loading failed, create minimal taxonomy
  if (is.null(tax)) {
    cat("🔧 Creating minimal", which, "taxonomy\n")
    tax <- list(
      canonicals = list(
        "Acute respiratory infection" = list(category = "Respiratory", attributes = c("Outbreak-prone")),
        "Diarrhea" = list(category = "Gastrointestinal", attributes = c("Water-borne")),
        "Fever" = list(category = "General", attributes = c()),
        "Hypertension" = list(category = "Cardiovascular", attributes = c()),
        "Diabetes" = list(category = "Metabolic", attributes = c()),
        "Malnutrition" = list(category = "Nutrition", attributes = c("Climate-sensitive"))
      ),
      synonyms = list(
        "ARI" = "Acute respiratory infection",
        "Diarrhoea" = "Diarrhea",
        "High blood pressure" = "Hypertension"
      ),
      categories = list(
        "Respiratory" = c("Acute respiratory infection", "Pneumonia", "Asthma"),
        "Gastrointestinal" = c("Diarrhea", "Gastroenteritis"),
        "General" = c("Fever", "Headache"),
        "Cardiovascular" = c("Hypertension"),
        "Metabolic" = c("Diabetes"),
        "Nutrition" = c("Malnutrition")
      ),
      epidemic_groups = list(
        "Vaccine_Preventable" = c("Measles", "Polio"),
        "Water_Food_Borne" = c("Cholera", "Diarrhea"),
        "Respiratory_Epidemic" = c("COVID-19", "Influenza")
      ),
      syndromic_groups = list(
        "Diarrheal_Syndrome" = c("Diarrhea", "Gastroenteritis"),
        "Respiratory_Syndrome" = c("Acute respiratory infection", "Pneumonia"),
        "Fever_Syndrome" = c("Fever", "Malaria")
      ),
      age_risks = list(
        "Under_5" = c("Diarrhea", "Malnutrition"),
        "Elderly" = c("Hypertension", "Diabetes")
      ),
      seasonal_patterns = list(
        "Winter" = c("Acute respiratory infection"),
        "Summer" = c("Diarrhea")
      ),
      climate_sensitive_groups = list(
        "Water_and_Food_Safety" = c("Diarrhea", "Acute Watery Diarrhea", "Gastroenteritis", "Cholera"),
        "Vector_Borne_Diseases" = c("Malaria", "Dengue", "Chikungunya", "Zika"),
        "Heat_and_Air_Quality" = c("Acute respiratory infection", "Pneumonia", "Asthma"),
        "Nutrition_and_Food_Security" = c("Malnutrition", "Underweight", "Stunting")
      ),
      severity_map = list(
        "High" = c("Meningitis", "Pneumonia"),
        "Medium" = c("Hypertension", "Diabetes"),
        "Low" = c("Fever", "Headache")
      ),
      alert_thresholds = list(
        "Measles" = list(low = 1, medium = 3, high = 5),
        "Cholera" = list(low = 1, medium = 5, high = 10)
      ),
      malnutrition_labels = c("Malnutrition", "Underweight", "Stunting"),
      trauma_morbidities = c("Injury", "Fracture", "Burn")
    )
  }
  
  .cache_set(cache_key, tax)
  return(tax)
}

# ---- Build synonym index function (MISSING FUNCTION ADDED) ----
.build_syn_index <- function(which = "base") {
  cache_key <- paste0("syn_", which)
  cached <- .cache_get(cache_key)
  if (!is.null(cached)) {
    cat("🔍 Using cached synonym index with", length(cached), "entries\n")
    return(cached)
  }
  
  tryCatch({
    cat("🔧 Building synonym index for", which, "taxonomy\n")
    tax <- load_taxonomy(which)
    syn_map <- tax$synonyms %||% list()
    
    cat("📋 Found", length(syn_map), "synonyms in taxonomy\n")
    if (length(syn_map) > 0) {
      cat("📋 Sample synonyms:", paste(head(names(syn_map), 3), collapse = ", "), "\n")
    }
    
    # Create normalized lookup
    lookup <- setNames(character(0), character(0))
    
    for (syn in names(syn_map)) {
      canonical <- syn_map[[syn]]
      lookup[.norm(syn)] <- canonical
    }
    
    # Add direct mappings (canonical names map to themselves)
    if (!is.null(tax$canonicals)) {
      canonicals <- names(tax$canonicals)
      cat("📋 Adding", length(canonicals), "canonical names to lookup\n")
      for (canonical in canonicals) {
        lookup[.norm(canonical)] <- canonical
      }
    }
    
    cat("✅ Built synonym index with", length(lookup), "total mappings\n")
    if (length(lookup) > 0) {
      sample_lookup <- head(names(lookup), 5)
      cat("📋 Sample normalized keys:", paste(sample_lookup, collapse = ", "), "\n")
    }
    
    .cache_set(cache_key, lookup)
    return(lookup)
    
  }, error = function(e) {
    cat("⚠️ Error building synonym index:", e$message, "\n")
    # Return minimal lookup
    minimal_lookup <- c()
    names(minimal_lookup) <- character(0)
    .cache_set(cache_key, minimal_lookup)
    return(minimal_lookup)
  })
}

# ---- Build country-aware synonym index (NEW FUNCTION) ----
.build_country_syn_index <- function(country_file = NULL) {
  cache_key <- paste0("syn_country_", country_file %||% "base")
  cached <- .cache_get(cache_key)
  if (!is.null(cached)) {
    cat("🔍 Using cached country synonym index with", length(cached), "entries\n")
    return(cached)
  }
  
  tryCatch({
    cat("🔧 Building country-aware synonym index\n")
    
    # Start with base taxonomy
    base_tax <- load_taxonomy("base")
    lookup <- setNames(character(0), character(0))
    
    # Add base synonyms
    base_syn_map <- base_tax$synonyms %||% list()
    cat("📋 Found", length(base_syn_map), "base synonyms\n")
    
    for (syn in names(base_syn_map)) {
      canonical <- base_syn_map[[syn]]
      lookup[.norm(syn)] <- canonical
    }
    
    # Add base canonical names (map to themselves)
    if (!is.null(base_tax$canonicals)) {
      canonicals <- names(base_tax$canonicals)
      cat("📋 Adding", length(canonicals), "base canonical names to lookup\n")
      for (canonical in canonicals) {
        lookup[.norm(canonical)] <- canonical
      }
    }
    
    # Add country-specific synonyms if specified
    if (!is.null(country_file)) {
      country_path <- here::here("taxonomy", country_file)
      if (file.exists(country_path)) {
        cat("🌍 Loading country synonyms from", country_file, "\n")
        country_tax <- yaml::read_yaml(country_path)
        
        if (!is.null(country_tax$synonyms)) {
          country_syn_map <- country_tax$synonyms
          cat("📋 Found", length(country_syn_map), "country synonyms\n")
          
          # Country synonyms override base synonyms
          for (syn in names(country_syn_map)) {
            canonical <- country_syn_map[[syn]]
            lookup[.norm(syn)] <- canonical
          }
          
          # Show sample country mappings
          if (length(country_syn_map) > 0) {
            sample_country <- head(names(country_syn_map), 3)
            cat("📋 Sample country synonyms:", paste(sample_country, collapse = ", "), "\n")
          }
        }
      } else {
        cat("⚠️ Country taxonomy file not found:", country_path, "\n")
      }
    }
    
    cat("✅ Built country-aware synonym index with", length(lookup), "total mappings\n")
    if (length(lookup) > 0) {
      sample_lookup <- head(names(lookup), 5)
      cat("📋 Sample normalized keys:", paste(sample_lookup, collapse = ", "), "\n")
    }
    
    .cache_set(cache_key, lookup)
    return(lookup)
    
  }, error = function(e) {
    cat("⚠️ Error building country synonym index:", e$message, "\n")
    # Fall back to base taxonomy only
    return(.build_syn_index("base"))
  })
}

# ================================================================================
# CRITICAL FIX: Add fallback functions for missing purrr functions
# ================================================================================

# Fallback for purrr::imap if not available
if (!exists("imap") && !exists("imap", envir = asNamespace("purrr"))) {
  imap <- function(.x, .f, ...) {
    result <- list()
    names_x <- names(.x)
    if (is.null(names_x)) names_x <- seq_along(.x)
    
    for (i in seq_along(.x)) {
      result[[i]] <- .f(.x[[i]], names_x[i], ...)
    }
    names(result) <- names_x
    return(result)
  }
}

# Fallback for purrr::imap_dfr if not available
if (!exists("imap_dfr") && !exists("imap_dfr", envir = asNamespace("purrr"))) {
  imap_dfr <- function(.x, .f, ...) {
    result_list <- imap(.x, .f, ...)
    do.call(rbind, result_list)
  }
}

# Fallback for purrr::pmap_dfr if not available
if (!exists("pmap_dfr") && !exists("pmap_dfr", envir = asNamespace("purrr"))) {
  pmap_dfr <- function(.l, .f, ...) {
    result_list <- list()
    for (i in seq_len(nrow(.l))) {
      row_data <- lapply(.l, function(col) col[i])
      result_list[[i]] <- do.call(.f, c(row_data, list(...)))
    }
    do.call(rbind, result_list)
  }
}

# Fallback for purrr::pmap_chr if not available
if (!exists("pmap_chr") && !exists("pmap_chr", envir = asNamespace("purrr"))) {
  pmap_chr <- function(.l, .f, ...) {
    result <- character(length(.l[[1]]))
    for (i in seq_along(result)) {
      row_data <- lapply(.l, function(col) col[i])
      result[i] <- do.call(.f, c(row_data, list(...)))
    }
    return(result)
  }
}

# ================================================================================
# Simple taxonomy loading (NO CONFIG) - enhanced error handling
# ================================================================================
.find_taxonomy_file <- function(filename) {
  # Handle both .yml and .yaml extensions
  base_name <- tools::file_path_sans_ext(filename)
  extensions <- c(".yml", ".yaml")
  
  filenames_to_try <- character(0)
  for (ext in extensions) {
    filenames_to_try <- c(filenames_to_try, paste0(base_name, ext))
  }
  
  candidates <- character(0)
  for (fn in filenames_to_try) {
    candidates <- c(candidates,
      fn,
      file.path("taxonomy", fn),
      file.path(getwd(), "taxonomy", fn),
      file.path("data", "taxonomy", fn),
      file.path("config", "taxonomy", fn)
    )
  }
  
  if (requireNamespace("here", quietly = TRUE)) {
    for (fn in filenames_to_try) {
      candidates <- c(candidates,
                      here::here("taxonomy", fn),
                      here::here("data", "taxonomy", fn)
      )
    }
  }
  
  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(candidate)
    }
  }
  
  # CRITICAL FIX: Create minimal taxonomy if file not found
  cat("⚠️ Taxonomy file not found:", filename, "\n")
  cat("📁 Searched in:", paste(candidates[1:3], collapse = ", "), "...\n")
  cat("🔧 Creating minimal taxonomy fallback\n")
  
  return(create_minimal_taxonomy_file(filename))
}

# CRITICAL FIX: Create minimal taxonomy file if missing
create_minimal_taxonomy_file <- function(filename) {
  # Create minimal taxonomy structure
  minimal_taxonomy <- list(
    canonicals = list(
      "Acute respiratory infection" = list(category = "Respiratory"),
      "Diarrhea" = list(category = "Gastrointestinal"),
      "Fever" = list(category = "General"),
      "Hypertension" = list(category = "Cardiovascular"),
      "Diabetes" = list(category = "Metabolic"),
      "Malnutrition" = list(category = "Nutrition")
    ),
    synonyms = list(
      "ARI" = "Acute respiratory infection",
      "Diarrhoea" = "Diarrhea",
      "High blood pressure" = "Hypertension"
    ),
    categories = list(
      "Respiratory" = c("Acute respiratory infection", "Pneumonia", "Asthma"),
      "Gastrointestinal" = c("Diarrhea", "Gastroenteritis"),
      "General" = c("Fever", "Headache"),
      "Cardiovascular" = c("Hypertension"),
      "Metabolic" = c("Diabetes"),
      "Nutrition" = c("Malnutrition")
    ),
    epidemic_groups = list(
      "Vaccine_Preventable" = c("Measles", "Polio"),
      "Water_Food_Borne" = c("Cholera", "Diarrhea"),
      "Respiratory_Epidemic" = c("COVID-19", "Influenza")
    ),
    syndromic_groups = list(
      "Diarrheal_Syndrome" = c("Diarrhea", "Gastroenteritis"),
      "Respiratory_Syndrome" = c("Acute respiratory infection", "Pneumonia"),
      "Fever_Syndrome" = c("Fever", "Malaria")
    ),
    age_risks = list(
      "Under_5" = c("Diarrhea", "Malnutrition"),
      "Elderly" = c("Hypertension", "Diabetes")
    ),
    seasonal_patterns = list(
      "Winter" = c("Acute respiratory infection"),
      "Summer" = c("Diarrhea")
    ),
    climate_sensitive_groups = list(
      "Water_and_Food_Safety" = c("Diarrhea", "Acute Watery Diarrhea", "Gastroenteritis", "Cholera", "Typhoid Fever", "Hepatitis A"),
      "Vector_Borne_Diseases" = c("Malaria", "Dengue", "Chikungunya", "Zika", "Yellow Fever", "Leishmaniasis"),
      "Heat_and_Air_Quality" = c("Acute respiratory infection", "Pneumonia", "Asthma", "COPD"),
      "Extreme_Weather_Related" = c("Injury", "Trauma", "Hypertension", "Cardiovascular"),
      "Nutrition_and_Food_Security" = c("Malnutrition", "Underweight", "Stunting", "Vitamin Deficiency")
    ),
    severity_map = list(
      "High" = c("Meningitis", "Pneumonia"),
      "Medium" = c("Hypertension", "Diabetes"),
      "Low" = c("Fever", "Headache")
    ),
    alert_thresholds = list(
      "Measles" = list(low = 1, medium = 3, high = 5),
      "Cholera" = list(low = 1, medium = 5, high = 10)
    ),
    malnutrition_labels = c("Malnutrition", "Underweight", "Stunting"),
    trauma_morbidities = c("Injury", "Fracture", "Burn")
  )
  
  # Store in cache to avoid recreating
  cache_key <- paste0("minimal_", gsub("[^a-zA-Z0-9]", "_", filename))
  .cache_set(cache_key, minimal_taxonomy)
  
  return(cache_key)  # Return cache key instead of file path
}

.merge_tax <- function(base, overlay) {
  out <- base
  for (nm in names(overlay)) out[[nm]] <- overlay[[nm]]
  out
}


# ---- Fixed global_canonicalize function with better error handling ----
global_canonicalize <- function(disease, canonical_map = NULL) {
  if (is.null(canonical_map)) {
    canonical_map <- .build_syn_index("base")
  }
  .map_to_canonical(as.character(disease), canonical_map)
}

# ---- Fixed tax_category_lookup function with error handling ----
tax_category_lookup <- function(canonical_vec, which = "base") {
  tryCatch({
    tax <- load_taxonomy(which)
    can <- tax$canonicals %||% list()
    
    cat("🔍 tax_category_lookup: Loaded", length(can), "canonicals from", which, "taxonomy\n")
    
    # Build category lookup
    category_lookup <- list()
    for (disease_name in names(can)) {
      disease_info <- can[[disease_name]]
      if (!is.null(disease_info$category)) {
        category_lookup[[disease_name]] <- disease_info$category
      }
    }
    
    cat("📋 Built category lookup with", length(category_lookup), "entries\n")
    if (length(category_lookup) > 0) {
      sample_categories <- head(unique(unlist(category_lookup)), 3)
      cat("📋 Sample categories:", paste(sample_categories, collapse = ", "), "\n")
    }
    
    result <- character(length(canonical_vec))
    for (i in seq_along(canonical_vec)) {
      cn <- canonical_vec[i]
      if (is.null(cn) || is.na(cn) || !nzchar(cn)) {
        result[i] <- NA_character_
      } else {
        cn_str <- as.character(cn)
        if (cn_str %in% names(category_lookup)) {
          result[i] <- category_lookup[[cn_str]]
        } else {
          result[i] <- "Uncategorized"  # Default category
        }
      }
    }
    
    # Debug output
    categorized_count <- sum(result != "Uncategorized" & !is.na(result))
    uncategorized_count <- sum(result == "Uncategorized")
    cat("✅ Categorized", categorized_count, "diseases,", uncategorized_count, "uncategorized\n")
    
    if (uncategorized_count > 0) {
      uncategorized_diseases <- unique(canonical_vec[result == "Uncategorized" & !is.na(canonical_vec)])
      cat("⚠️ Uncategorized diseases (sample):", paste(head(uncategorized_diseases, 5), collapse = ", "), "\n")
    }
    
    return(result)
    
  }, error = function(e) {
    cat("⚠️ Error in tax_category_lookup:", e$message, "\n")
    return(rep("Uncategorized", length(canonical_vec)))
  })
}

# ================================================================================
# Accessors with fallback handling
# ================================================================================
get_categories_map <- function() {
  tryCatch({
    load_taxonomy("base")$categories
  }, error = function(e) {
    list("General" = c("Fever", "Headache"))
  })
}

get_synonym_map <- function(which = c("base")) {
  tryCatch({
    load_taxonomy(match.arg(which))$synonyms
  }, error = function(e) {
    list("ARI" = "Acute respiratory infection")
  })
}

get_epidemic_groups <- function() {
  tryCatch({
    load_taxonomy("base")$epidemic_groups
  }, error = function(e) {
    list("General" = c("Fever"))
  })
}

get_syndromic_groups <- function() {
  tryCatch({
    load_taxonomy("base")$syndromic_groups
  }, error = function(e) {
    list("General_Syndrome" = c("Fever"))
  })
}

get_age_risks <- function() {
  tryCatch({
    load_taxonomy("base")$age_risks
  }, error = function(e) {
    list("All_Ages" = c("Fever"))
  })
}

get_seasonal_patterns <- function() {
  tryCatch({
    load_taxonomy("base")$seasonal_patterns
  }, error = function(e) {
    list("Year_Round" = c("Fever"))
  })
}

get_severity_map_raw <- function() {
  tryCatch({
    load_taxonomy("base")$severity_map
  }, error = function(e) {
    list("Low" = c("Fever"))
  })
}

get_malnutrition_labels <- function() {
  tryCatch({
    load_taxonomy("base")$malnutrition_labels
  }, error = function(e) {
    c("Malnutrition")
  })
}

get_trauma_morbidities <- function() {
  tryCatch({
    load_taxonomy("base")$trauma_morbidities
  }, error = function(e) {
    c("Injury")
  })
}

get_climate_sensitive_groups <- function() {
  tryCatch({
    load_taxonomy("base")$climate_sensitive_groups
  }, error = function(e) {
    list("General" = c("Fever"))
  })
}

get_alert_thresholds <- function() {
  tryCatch({
    load_taxonomy("base")$alert_thresholds
  }, error = function(e) {
    list()
  })
}

# ================================================================================
# Category lookup (vectorized) with error handling
# ================================================================================
get_category_from_taxonomy <- function(canonical, which = "base") {
  tryCatch({
    tax <- load_taxonomy(which)
    vapply(canonical, function(cn) {
      if (is.null(cn) || is.na(cn) || !nzchar(cn)) return(NA_character_)
      rec <- tax$canonicals[[as.character(cn)]]
      if (is.list(rec) && !is.null(rec$category)) as.character(rec$category) else "Uncategorized"
    }, character(1), USE.NAMES = FALSE)
  }, error = function(e) {
    cat("⚠️ Error in get_category_from_taxonomy:", e$message, "\n")
    rep("Uncategorized", length(canonical))
  })
}


# ================================================================================
# DATA STRUCTURE DETECTION FOR MULTI-COUNTRY SUPPORT
# ================================================================================

#' Detect data structure type based on available columns and metadata
#' 
#' @param df data.frame to analyze
#' @param config optional country configuration for additional context
#' @return list with structure type and available features
detect_data_structure <- function(df, config = NULL) {
  features <- c()
  
  # Check for demographic columns
  if ("age_group" %in% names(df)) features <- c(features, "age_groups")
  if ("sex" %in% names(df)) features <- c(features, "sex")
  if ("age_years" %in% names(df) || "age" %in% names(df)) features <- c(features, "continuous_age")
  
  # Check for geographic columns  
  geo_cols <- c("admin0", "admin1", "admin2", "admin3", "governorate", "district", "ou", "org_unit")
  if (any(geo_cols %in% names(df))) features <- c(features, "geography")
  
  # Check for temporal columns
  temp_cols <- c("pe", "period", "date", "month", "year", "date_period")
  if (any(temp_cols %in% names(df))) features <- c(features, "temporal")
  
  # Check for DHIS2-specific columns
  dhis2_cols <- c("dx", "co", "numerator", "denominator")
  if (any(dhis2_cols %in% names(df))) features <- c(features, "dhis2_analytics")
  
  # Check for disease/morbidity columns
  disease_cols <- c("mapped_disease", "canonical_disease", "morbidity", "disease", "icd11_code")
  if (any(disease_cols %in% names(df))) features <- c(features, "disease_mapping")
  
  # Determine structure type
  structure_type <- "generic"
  
  if ("dhis2_analytics" %in% features && "age_groups" %in% features && "sex" %in% features) {
    structure_type <- "dhis2_demographic"  # Yemen-like structure
  } else if ("dhis2_analytics" %in% features) {
    structure_type <- "dhis2_basic"
  } else if ("continuous_age" %in% features) {
    structure_type <- "individual_records"  # Syria-like structure
  } else if ("disease_mapping" %in% features) {
    structure_type <- "disease_focused"
  }
  
  return(list(
    type = structure_type,
    features = features,
    has_demographics = any(c("age_groups", "sex", "continuous_age") %in% features),
    has_geography = "geography" %in% features,
    has_temporal = "temporal" %in% features
  ))
}

#' Apply processing specific to detected data structure
#' 
#' @param df data.frame with taxonomy variables already added
#' @param data_structure list from detect_data_structure()
#' @return data.frame with structure-specific processing applied
apply_structure_specific_processing <- function(df, data_structure) {
  
  cat("🔧 Applying", data_structure$type, "specific processing...\n")
  
  if (data_structure$type == "dhis2_demographic") {
    # Handle DHIS2 data with demographic breakdowns (Yemen-like)
    cat("📊 Processing DHIS2 demographic data structure\n")
    
    # Preserve existing demographic columns
    if ("age_group" %in% names(df)) {
      cat("   - Preserved age_group column\n")
    }
    if ("sex" %in% names(df)) {
      cat("   - Preserved sex column\n")
    }
    
    # Add standardized period/date handling
    if ("pe" %in% names(df) && !"date_period" %in% names(df)) {
      df$date_period <- df$pe
      cat("   - Added date_period from pe column\n")
    }
    
    # Add geographic standardization if org units present
    if ("ou" %in% names(df) && !"location" %in% names(df)) {
      df$location <- df$ou
      cat("   - Added location from ou column\n")
    }
    
  } else if (data_structure$type == "dhis2_basic") {
    # Handle basic DHIS2 data without demographics
    cat("📈 Processing basic DHIS2 data structure\n")
    
    if ("pe" %in% names(df)) {
      df$date_period <- df$pe
      cat("   - Added date_period from pe column\n")
    }
    
  } else if (data_structure$type == "individual_records") {
    # Handle individual record data (Syria-like)
    cat("👤 Processing individual records data structure\n")
    
    # Age group creation from continuous age if not present
    if (("age_years" %in% names(df) || "age" %in% names(df)) && !"age_group" %in% names(df)) {
      age_col <- if ("age_years" %in% names(df)) "age_years" else "age"
      df$age_group <- cut(df[[age_col]], 
                         breaks = c(0, 1, 5, 15, 50, Inf),
                         labels = c("0-11m", "1-4y", "5-14y", "15-49y", "50+y"),
                         right = FALSE)
      cat("   - Created age_group from", age_col, "\n")
    }
  }
  
  # Common processing for all structures
  if (data_structure$has_temporal && !"year" %in% names(df)) {
    # Try to extract year from various date columns
    date_cols <- c("date_period", "pe", "date", "period")
    for (col in date_cols) {
      if (col %in% names(df)) {
        # Basic year extraction - can be enhanced based on format
        df$year <- gsub("(\\d{4}).*", "\\1", df[[col]])
        cat("   - Extracted year from", col, "\n")
        break
      }
    }
  }
  
  return(df)
}

# ================================================================================
# CONSOLIDATED APPLY MORBIDITY CATEGORIZATION - MAIN FUNCTION WITH ERROR HANDLING
# ================================================================================

#' Apply comprehensive morbidity categorization with all taxonomy variables
#' 
#' This is the main function that adds ALL taxonomy-based variables to a dataset
#' UPDATED: Now properly applies country mapping for standardized_disease
#' 
#' @param df data.frame with morbidity data
#' @param morbidity_col character, name of morbidity column
#' @param which taxonomy to use ("base" or "syria")
#' @return data.frame with comprehensive taxonomy variables
#' @export
apply_morbidity_categorization <- function(df, morbidity_col = "morbidity", which = "base") {
  # Auto-detect morbidity column from common variants
  if (!morbidity_col %in% names(df)) {
    # Try common morbidity column names
    possible_cols <- c("morbidity", "disease", "original_disease")
    found_col <- NULL
    
    for (col in possible_cols) {
      if (col %in% names(df)) {
        found_col <- col
        break
      }
    }
    
    if (!is.null(found_col)) {
      cat("🔍 Auto-detected morbidity column:", found_col, "\n")
      morbidity_col <- found_col
    } else {
      warning("⚠️ apply_morbidity_categorization: No suitable morbidity column found. Tried: ", 
              paste(possible_cols, collapse = ", "), "\n")
      return(df)
    }
  }
  
  cat("🏷️ Applying comprehensive morbidity categorization...\n")
  
  # Detect data structure type based on available columns
  data_structure <- detect_data_structure(df)
  cat("🔍 Detected data structure:", data_structure$type, "\n")
  if (length(data_structure$features) > 0) {
    cat("📊 Available features:", paste(data_structure$features, collapse = ", "), "\n")
  }
  
  # Add log_info if not defined
  if (!exists("log_info", mode = "function")) {
    log_info <- function(msg, ...) cat(paste0("[INFO] ", sprintf(msg, ...), "\n"))
  }
  
  tryCatch({
    raw <- as.character(df[[morbidity_col]])
    
    # 1. Country-aware standardization for canonical disease
    # Try to get country config to use country-specific synonyms
    country_config <- tryCatch({
      if (exists("get_current_country_config", mode = "function")) {
        get_current_country_config()
      } else {
        # Try to detect country from global environment
        if (exists("ACTIVE_COUNTRY", envir = .GlobalEnv)) {
          country_name <- get("ACTIVE_COUNTRY", envir = .GlobalEnv)
          country_config_path <- here::here("config", "countries", paste0(country_name, ".yml"))
          if (file.exists(country_config_path)) {
            yaml::read_yaml(country_config_path)
          } else {
            NULL
          }
        } else {
          NULL
        }
      }
    }, error = function(e) {
      cat("⚠️ Could not get country config, using base taxonomy only:", e$message, "\n")
      NULL
    })
    
    # Use country-aware synonym mapping if available
    country_file <- NULL
    if (!is.null(country_config$taxonomy$country_file)) {
      country_file <- country_config$taxonomy$country_file
      cat("🌍 Using country-specific synonyms from:", country_file, "\n")
    }
    
    df$canonical_disease_imc <- .map_to_canonical(raw, .build_country_syn_index(country_file))
    log_info("🌍 Completed country-aware standardization")
    
    # 2.Canonical disease category  
    df$category_canonical_disease_imc <- tax_category_lookup(df$canonical_disease_imc, which = "base")
    log_info("📦 Completed morbidity category lookup")
    
    # 3. Add all taxonomy variables based on base taxonomy using canonical_disease_imc
    log_info("📊 Starting comprehensive taxonomy variable creation")
    df <- add_comprehensive_taxonomy_variables(df, which = "base")
    
    # 4. Structure-specific post-processing
    df <- apply_structure_specific_processing(df, data_structure)
    
    # 5. Add standard column aliases for compatibility
    df$canonical_disease <- df$canonical_disease_imc
    df$register <- "register"  # Create register column as requested
    
    cat("✅ Comprehensive morbidity categorization completed\n")
    cat("📋 Variables created:\n")
    cat("   - canonical_disease (base taxonomy)\n")
    cat("   - category_canonical_disease_imc (base taxonomy)\n")
    cat("   - register (standard column)\n")
    cat("   - comprehensive taxonomy variables (base taxonomy)\n")
    if (data_structure$has_demographics) {
      cat("   - Demographic variables preserved/processed\n")
    }
    
    return(df)
    
  }, error = function(e) {
    cat("❌ Error in apply_morbidity_categorization:", e$message, "\n")
    cat("🔧 Adding minimal taxonomy variables\n")
    
    # Add minimal variables to prevent downstream errors
    df$canonical_disease_imc <- df[[morbidity_col]]
    df$category_canonical_disease_imc <- "Uncategorized"
    df$epidemic_prone <- FALSE
    df$climate_sensitive <- FALSE
    df$amr_relevant <- FALSE
    df$vaccine_preventable <- FALSE
    
    return(df)
  })
}

#' Add comprehensive taxonomy variables to dataset with error handling
#' 
#' @param df data.frame with standardized_disease column
#' @param which taxonomy to use (should always be "base")
#' @return data.frame with all taxonomy variables
add_comprehensive_taxonomy_variables <- function(df, which = "base") {
  tryCatch({
    # Ensure we have the required columns
    if ("morbidity" %in% names(df) && !"canonical_disease_imc" %in% names(df)) {
      df$canonical_disease_imc <- .map_to_canonical(df$morbidity, .build_syn_index("base"))
    }
    tax <- load_taxonomy(which)
    canonicals <- tax$canonicals %||% list()
    epidemic_groups <- tax$epidemic_groups %||% list()
    syndromic_groups <- tax$syndromic_groups %||% list()
    age_risks <- tax$age_risks %||% list()
    seasonal_patterns <- tax$seasonal_patterns %||% list()
    climate_groups <- tax$climate_sensitive_groups %||% list()
    severity_map <- tax$severity_map %||% list()
    alert_thresholds <- tax$alert_thresholds %||% list()

    df <- initialize_taxonomy_variables(df)
    if (!exists("log_info", mode = "function")) {
      log_info <- function(msg, ...) cat(paste0("[INFO] ", sprintf(msg, ...), "\n"))
    }

    n <- nrow(df)
    # For future diagnostics: log every 50k rows (vectorized, so just log if n >= 50000)
    if (n >= 50000) log_info("⏳ Processed {n} rows (vectorized)...")

    diseases <- as.character(df$canonical_disease_imc)
    disease_na <- is.na(diseases) | !nzchar(diseases)

    # 1. Extract all taxonomy variables directly from canonicals structure
    # Initialize all taxonomy flags
    df$severity <- rep("Low", n)
    df$attributes <- rep(NA_character_, n)
    df$epidemic_groups_flag <- rep(NA_character_, n)
    df$epidemic_group <- rep(NA_character_, n)  # Backwards compatibility
    df$seasonal_flag <- rep(NA_character_, n)
    df$age_risks_flag <- rep(NA_character_, n)
    df$climate_groups_flag <- rep(NA_character_, n)
    
    # VECTORIZED taxonomy extraction - much faster for large datasets
    cat("🚀 Using vectorized taxonomy extraction for", n, "records\n")
    
    # Create lookup tables for fast vectorized operations
    unique_diseases <- unique(diseases[!disease_na])
    valid_diseases <- intersect(unique_diseases, names(canonicals))
    
    if (length(valid_diseases) > 0) {
      # Pre-compute all taxonomy values for unique diseases
      severity_lookup <- character(length(valid_diseases))
      attributes_lookup <- character(length(valid_diseases))
      epidemic_lookup <- character(length(valid_diseases))
      seasonal_lookup <- character(length(valid_diseases))
      age_risks_lookup <- character(length(valid_diseases))
      climate_lookup <- character(length(valid_diseases))
      icd11_code_lookup <- character(length(valid_diseases))
      icd11_title_lookup <- character(length(valid_diseases))
      icd11_category_lookup <- character(length(valid_diseases))
      confidence_lookup <- character(length(valid_diseases))
      
      names(severity_lookup) <- names(attributes_lookup) <- names(epidemic_lookup) <- 
        names(seasonal_lookup) <- names(age_risks_lookup) <- names(climate_lookup) <- 
        names(icd11_code_lookup) <- names(icd11_title_lookup) <- names(icd11_category_lookup) <- 
        names(confidence_lookup) <- valid_diseases
      
      # Build lookup tables
      for (disease in valid_diseases) {
        canonical_info <- canonicals[[disease]]
        
        severity_lookup[disease] <- if (!is.null(canonical_info$severity)) canonical_info$severity else "Low"
        attributes_lookup[disease] <- if (!is.null(canonical_info$attributes)) paste(canonical_info$attributes, collapse = "|") else NA_character_
        epidemic_lookup[disease] <- if (!is.null(canonical_info$epidemic_groups)) paste(canonical_info$epidemic_groups, collapse = "|") else NA_character_
        seasonal_lookup[disease] <- if (!is.null(canonical_info$seasonal)) paste(canonical_info$seasonal, collapse = "|") else NA_character_
        age_risks_lookup[disease] <- if (!is.null(canonical_info$age_risks)) paste(canonical_info$age_risks, collapse = "|") else NA_character_
        climate_lookup[disease] <- if (!is.null(canonical_info$climate_groups)) paste(canonical_info$climate_groups, collapse = "|") else NA_character_
        # Extract ICD-11 codes from base taxonomy structure (icd11_codes is a list)
        icd11_code_lookup[disease] <- if (!is.null(canonical_info$icd11_codes) && length(canonical_info$icd11_codes) > 0) {
          as.character(canonical_info$icd11_codes[[1]])  # Take first code if multiple
        } else NA_character_
        
        # Will be filled from ICD-11 mapping file below
        icd11_title_lookup[disease] <- NA_character_
        icd11_category_lookup[disease] <- NA_character_
        confidence_lookup[disease] <- "none"  # Default confidence for base taxonomy
      }
      
      # Vectorized assignment - MUCH faster than loops
      df$severity <- ifelse(diseases %in% names(severity_lookup), severity_lookup[diseases], "Low")
      df$attributes <- ifelse(diseases %in% names(attributes_lookup), attributes_lookup[diseases], NA_character_)
      
      epidemic_values <- ifelse(diseases %in% names(epidemic_lookup), epidemic_lookup[diseases], NA_character_)
      df$epidemic_groups_flag <- epidemic_values
      df$epidemic_group <- epidemic_values  # Backwards compatibility
      
      df$seasonal_flag <- ifelse(diseases %in% names(seasonal_lookup), seasonal_lookup[diseases], NA_character_)
      df$age_risks_flag <- ifelse(diseases %in% names(age_risks_lookup), age_risks_lookup[diseases], NA_character_)
      df$climate_groups_flag <- ifelse(diseases %in% names(climate_lookup), climate_lookup[diseases], NA_character_)
      
      # Add ICD-11 variables from base taxonomy first
      df$icd11_code <- ifelse(diseases %in% names(icd11_code_lookup), icd11_code_lookup[diseases], NA_character_)
      df$icd11_title <- ifelse(diseases %in% names(icd11_title_lookup), icd11_title_lookup[diseases], NA_character_)
      df$icd11_category <- ifelse(diseases %in% names(icd11_category_lookup), icd11_category_lookup[diseases], NA_character_)
      df$confidence <- ifelse(diseases %in% names(confidence_lookup), confidence_lookup[diseases], "none")
      
      # Now overlay with proper ICD-11 mappings from API/mapping file
      tryCatch({
        # Load ICD-11 mappings from the mapping file
        icd11_mapping_file <- here::here("taxonomy", "icd11", "disease_mappings.yml")
        if (file.exists(icd11_mapping_file)) {
          icd11_mappings <- yaml::read_yaml(icd11_mapping_file)
          
          # Extract mappings from all confidence levels
          all_mappings <- list()
          for (confidence_level in c("high_confidence", "medium_confidence", "needs_review")) {
            if (!is.null(icd11_mappings$mappings[[confidence_level]])) {
              all_mappings <- c(all_mappings, icd11_mappings$mappings[[confidence_level]])
            }
          }
          
          # Create lookup tables for original morbidity values
          if (length(all_mappings) > 0) {
            original_morbidities <- as.character(df$morbidity)
            
            # For each record, check if original morbidity has an ICD-11 mapping
            for (i in seq_len(nrow(df))) {
              orig_morbidity <- original_morbidities[i]
              if (!is.na(orig_morbidity) && orig_morbidity %in% names(all_mappings)) {
                mapping_info <- all_mappings[[orig_morbidity]]
                
                # Override with API-sourced ICD-11 data
                if (!is.null(mapping_info$icd11_code) && !is.na(mapping_info$icd11_code)) {
                  df$icd11_code[i] <- as.character(mapping_info$icd11_code)
                }
                if (!is.null(mapping_info$icd11_title) && !is.na(mapping_info$icd11_title)) {
                  df$icd11_title[i] <- as.character(mapping_info$icd11_title)
                }
                if (!is.null(mapping_info$icd11_category) && !is.na(mapping_info$icd11_category)) {
                  df$icd11_category[i] <- as.character(mapping_info$icd11_category)
                }
                if (!is.null(mapping_info$confidence) && !is.na(mapping_info$confidence)) {
                  df$confidence[i] <- as.character(mapping_info$confidence)
                }
              }
            }
            
            cat("✅ Applied ICD-11 API mappings from disease_mappings.yml\n")
          }
        }
      }, error = function(e) {
        cat("⚠️ Could not load ICD-11 mappings:", e$message, "\n")
      })
      
      cat("✅ Vectorized taxonomy extraction completed in seconds vs minutes\n")
    } else {
      cat("⚠️ No diseases found in taxonomy\n")
    }
    
    # Create boolean flags from attributes
    df$epidemic_prone <- grepl("\\bOutbreak-prone\\b", df$attributes %||% "")
    df$climate_sensitive <- grepl("\\bClimate-sensitive\\b", df$attributes %||% "")
    df$amr_relevant <- grepl("\\bAMR-relevant\\b", df$attributes %||% "")
    df$vaccine_preventable <- grepl("\\bVPD\\b", df$attributes %||% "")

    # 2. Epidemic groups (flattened membership)
    epidemic_group_vec <- rep(NA_character_, n)
    is_vaccine_preventable <- rep(FALSE, n)
    is_water_food_borne <- rep(FALSE, n)
    is_respiratory_epidemic <- rep(FALSE, n)
    is_vector_borne <- rep(FALSE, n)
    is_viral_hemorrhagic <- rep(FALSE, n)
    for (group_name in names(epidemic_groups)) {
      group_diseases <- epidemic_groups[[group_name]]
      in_group <- diseases %in% group_diseases
      epidemic_group_vec[is.na(epidemic_group_vec) & in_group] <- group_name
      if (group_name == "Vaccine_Preventable") is_vaccine_preventable <- is_vaccine_preventable | in_group
      if (group_name == "Water_Food_Borne") is_water_food_borne <- is_water_food_borne | in_group
      if (group_name == "Respiratory_Epidemic") is_respiratory_epidemic <- is_respiratory_epidemic | in_group
      if (group_name == "Vector_Borne") is_vector_borne <- is_vector_borne | in_group
      if (group_name == "Viral_Hemorrhagic_Fever") is_viral_hemorrhagic <- is_viral_hemorrhagic | in_group
    }
    df$epidemic_group <- epidemic_group_vec
    df$is_vaccine_preventable <- is_vaccine_preventable
    df$is_water_food_borne <- is_water_food_borne
    df$is_respiratory_epidemic <- is_respiratory_epidemic
    df$is_vector_borne <- is_vector_borne
    df$is_viral_hemorrhagic <- is_viral_hemorrhagic

    # 3. Syndromic groups (flattened membership)
    syndromic_group_vec <- rep(NA_character_, n)
    is_diarrheal_syndrome <- rep(FALSE, n)
    is_respiratory_syndrome <- rep(FALSE, n)
    is_fever_syndrome <- rep(FALSE, n)
    is_rash_illness <- rep(FALSE, n)
    is_neurological_syndrome <- rep(FALSE, n)
    is_jaundice_syndrome <- rep(FALSE, n)
    for (group_name in names(syndromic_groups)) {
      group_diseases <- syndromic_groups[[group_name]]
      in_group <- diseases %in% group_diseases
      syndromic_group_vec[is.na(syndromic_group_vec) & in_group] <- group_name
      if (grepl("Diarrheal", group_name)) is_diarrheal_syndrome <- is_diarrheal_syndrome | in_group
      if (grepl("Respiratory", group_name)) is_respiratory_syndrome <- is_respiratory_syndrome | in_group
      if (grepl("Fever", group_name)) is_fever_syndrome <- is_fever_syndrome | in_group
      if (grepl("Rash", group_name)) is_rash_illness <- is_rash_illness | in_group
      if (grepl("Neurological", group_name)) is_neurological_syndrome <- is_neurological_syndrome | in_group
      if (grepl("Jaundice", group_name)) is_jaundice_syndrome <- is_jaundice_syndrome | in_group
    }
    df$syndromic_group <- syndromic_group_vec
    df$is_diarrheal_syndrome <- is_diarrheal_syndrome
    df$is_respiratory_syndrome <- is_respiratory_syndrome
    df$is_fever_syndrome <- is_fever_syndrome
    df$is_rash_illness <- is_rash_illness
    df$is_neurological_syndrome <- is_neurological_syndrome
    df$is_jaundice_syndrome <- is_jaundice_syndrome

    # 4. Age risks (flattened membership, also is_seasonal if any)
    high_risk_under5 <- rep(FALSE, n)
    high_risk_school_age <- rep(FALSE, n)
    high_risk_adults <- rep(FALSE, n)
    high_risk_elderly <- rep(FALSE, n)
    high_risk_pregnant <- rep(FALSE, n)
    for (risk_group in names(age_risks)) {
      group_diseases <- age_risks[[risk_group]]
      in_group <- diseases %in% group_diseases
      if (risk_group == "Under_5") high_risk_under5 <- high_risk_under5 | in_group
      if (risk_group == "School_Age") high_risk_school_age <- high_risk_school_age | in_group
      if (risk_group == "Adults") high_risk_adults <- high_risk_adults | in_group
      if (risk_group == "Elderly") high_risk_elderly <- high_risk_elderly | in_group
      if (risk_group == "Pregnant") high_risk_pregnant <- high_risk_pregnant | in_group
    }
    df$high_risk_under5 <- high_risk_under5
    df$high_risk_school_age <- high_risk_school_age
    df$high_risk_adults <- high_risk_adults
    df$high_risk_elderly <- high_risk_elderly
    df$high_risk_pregnant <- high_risk_pregnant
    # is_seasonal: TRUE if any age risk or any seasonal pattern
    is_seasonal <- high_risk_under5 | high_risk_school_age | high_risk_adults | high_risk_elderly | high_risk_pregnant

    # 5. Seasonal patterns
    seasonal_pattern_vec <- rep(NA_character_, n)
    for (season in names(seasonal_patterns)) {
      group_diseases <- seasonal_patterns[[season]]
      in_group <- diseases %in% group_diseases
      seasonal_pattern_vec[is.na(seasonal_pattern_vec) & in_group] <- season
      is_seasonal <- is_seasonal | in_group
    }
    df$seasonal_pattern <- seasonal_pattern_vec
    df$is_seasonal <- is_seasonal

    # 6. Climate groups
    climate_group_vec <- rep(NA_character_, n)
    climate_vulnerability <- rep("Low", n)
    is_vector_borne_climate <- rep(FALSE, n)
    is_water_food_borne_climate <- rep(FALSE, n)
    is_heat_sensitive <- rep(FALSE, n)
    is_extreme_weather_sensitive <- rep(FALSE, n)
    is_nutrition_sensitive <- rep(FALSE, n)
    # CRITICAL FIX: Track if disease is in any climate group to update climate_sensitive flag
    in_any_climate_group <- rep(FALSE, n)
    for (group_name in names(climate_groups)) {
      group_diseases <- climate_groups[[group_name]]
      in_group <- diseases %in% group_diseases
      climate_group_vec[is.na(climate_group_vec) & in_group] <- group_name
      # CRITICAL FIX: Update climate_sensitive flag for any climate group membership
      in_any_climate_group <- in_any_climate_group | in_group
      # vulnerability
      if (grepl("Vector-borne|Extreme_Weather", group_name)) climate_vulnerability[in_group] <- "High"
      else if (grepl("Water_and_Food|Heat_and_Air", group_name)) climate_vulnerability[in_group] <- "Medium"
      # flags
      if (grepl("Vector-borne", group_name)) is_vector_borne_climate <- is_vector_borne_climate | in_group
      if (grepl("Water_and_Food", group_name)) is_water_food_borne_climate <- is_water_food_borne_climate | in_group
      if (grepl("Heat_and_Air", group_name)) is_heat_sensitive <- is_heat_sensitive | in_group
      if (grepl("Extreme_Weather", group_name)) is_extreme_weather_sensitive <- is_extreme_weather_sensitive | in_group
      if (grepl("Nutrition", group_name)) is_nutrition_sensitive <- is_nutrition_sensitive | in_group
    }
    # CRITICAL FIX: Update climate_sensitive flag to include climate group membership
    df$climate_sensitive <- df$climate_sensitive | in_any_climate_group
    df$climate_group <- climate_group_vec
    df$climate_vulnerability <- climate_vulnerability
    df$is_vector_borne_climate <- is_vector_borne_climate
    df$is_water_food_borne_climate <- is_water_food_borne_climate
    df$is_heat_sensitive <- is_heat_sensitive
    df$is_extreme_weather_sensitive <- is_extreme_weather_sensitive
    df$is_nutrition_sensitive <- is_nutrition_sensitive

    # 7. Severity mapping
    high_diseases <- severity_map$High %||% character()
    medium_diseases <- severity_map$Medium %||% character()
    disease_severity <- rep("Low", n)
    severity_weight <- rep(1, n)
    disease_severity[diseases %in% high_diseases] <- "High"
    severity_weight[diseases %in% high_diseases] <- 3
    disease_severity[diseases %in% medium_diseases] <- "Medium"
    severity_weight[diseases %in% medium_diseases] <- 2
    df$disease_severity <- disease_severity
    df$severity_weight <- severity_weight

    # 8. Alert thresholds
    alert_threshold_low <- rep(NA_real_, n)
    alert_threshold_medium <- rep(NA_real_, n)
    alert_threshold_high <- rep(NA_real_, n)
    for (d in intersect(diseases, names(alert_thresholds))) {
      idx <- which(diseases == d)
      thresholds <- alert_thresholds[[d]]
      if (!is.null(thresholds)) {
        alert_threshold_low[idx] <- thresholds$low %||% NA_real_
        alert_threshold_medium[idx] <- thresholds$medium %||% NA_real_
        alert_threshold_high[idx] <- thresholds$high %||% NA_real_
      }
    }
    df$alert_threshold_low <- alert_threshold_low
    df$alert_threshold_medium <- alert_threshold_medium
    df$alert_threshold_high <- alert_threshold_high

    # Convert to appropriate types
    df <- finalize_taxonomy_variable_types(df, epidemic_groups, syndromic_groups, climate_groups)
    df <- add_rollup_variables(df)
    df <- add_computed_metrics(df)
    cat("📊 Added comprehensive taxonomy variables\n")
    return(df)
  }, error = function(e) {
    cat("❌ Error in add_comprehensive_taxonomy_variables:", e$message, "\n")
    cat("🔧 Adding minimal taxonomy variables\n")
    df <- initialize_taxonomy_variables(df)
    return(df)
  })
}

#' Initialize all taxonomy variables with default values
initialize_taxonomy_variables <- function(df) {
  # Disease attribute flags
  df$epidemic_prone <- FALSE
  df$climate_sensitive <- FALSE
  df$amr_relevant <- FALSE
  df$vaccine_preventable <- FALSE
  
  # Epidemic group variables
  df$epidemic_group <- NA_character_
  df$is_vaccine_preventable <- FALSE
  df$is_water_food_borne <- FALSE
  df$is_respiratory_epidemic <- FALSE
  df$is_vector_borne <- FALSE
  df$is_viral_hemorrhagic <- FALSE
  
  # Syndromic variables
  df$syndromic_group <- NA_character_
  df$is_diarrheal_syndrome <- FALSE
  df$is_respiratory_syndrome <- FALSE
  df$is_fever_syndrome <- FALSE
  df$is_rash_illness <- FALSE
  df$is_neurological_syndrome <- FALSE
  df$is_jaundice_syndrome <- FALSE
  
  # Age risk variables
  df$high_risk_under5 <- FALSE
  df$high_risk_school_age <- FALSE
  df$high_risk_adults <- FALSE
  df$high_risk_elderly <- FALSE
  df$high_risk_pregnant <- FALSE
  
  # Seasonal variables
  df$seasonal_pattern <- NA_character_
  df$is_seasonal <- FALSE
  
  # Climate variables
  df$climate_group <- NA_character_
  df$climate_vulnerability <- "Low"
  df$is_vector_borne_climate <- FALSE
  df$is_water_food_borne_climate <- FALSE
  df$is_heat_sensitive <- FALSE
  df$is_extreme_weather_sensitive <- FALSE
  df$is_nutrition_sensitive <- FALSE
  
  # Severity variables
  df$disease_severity <- "Low"
  df$severity_weight <- 1
  df$alert_threshold_low <- NA_real_
  df$alert_threshold_medium <- NA_real_
  df$alert_threshold_high <- NA_real_
  
  return(df)
}

#' Convert taxonomy variables to appropriate factor types
finalize_taxonomy_variable_types <- function(df, epidemic_groups, syndromic_groups, climate_groups) {
  tryCatch({
    # Convert to factors (allowing for values not in levels)
    df$epidemic_group <- factor(df$epidemic_group, levels = names(epidemic_groups), exclude = NULL)
    df$syndromic_group <- factor(df$syndromic_group, levels = names(syndromic_groups), exclude = NULL)
    df$climate_group <- factor(df$climate_group, levels = names(climate_groups), exclude = NULL)
    
    df$seasonal_pattern <- factor(df$seasonal_pattern, 
                                  levels = c("Winter", "Spring", "Summer", "Autumn"), 
                                  exclude = NULL)
    
    df$disease_severity <- factor(df$disease_severity, 
                                  levels = c("Low", "Medium", "High"), 
                                  ordered = TRUE, exclude = NULL)
    
    df$climate_vulnerability <- factor(df$climate_vulnerability, 
                                       levels = c("Low", "Medium", "High"), 
                                       ordered = TRUE, exclude = NULL)
    
    return(df)
  }, error = function(e) {
    cat("⚠️ Error finalizing taxonomy variable types:", e$message, "\n")
    return(df)
  })
}

#' Add simplified rollup summary variables for dashboard analytics
add_rollup_variables <- function(df) {
  tryCatch({
    # Basic program rollups
    df$vpd_case <- df$vaccine_preventable
    df$outbreak_prone_case <- df$epidemic_prone
    df$climate_sensitive_case <- df$climate_sensitive
    df$amr_case <- df$amr_relevant
    
    # Severity rollups
    df$high_severity_case <- df$disease_severity == "High"
    df$medium_severity_case <- df$disease_severity == "Medium"
    
    # Age-specific rollups
    df$pediatric_case <- df$high_risk_under5
    df$elderly_care_case <- df$high_risk_elderly
    df$maternal_health_case <- df$high_risk_pregnant
    
    # Syndrome rollups
    df$emergency_syndrome_case <- df$is_respiratory_syndrome | 
      df$is_diarrheal_syndrome | 
      df$is_neurological_syndrome
    
    # Transmission pathway rollups
    df$vector_borne_case <- df$is_vector_borne | df$is_vector_borne_climate
    df$water_food_borne_case <- df$is_water_food_borne | df$is_water_food_borne_climate
    
    # Environmental health rollups
    df$environmental_health_case <- df$climate_sensitive_case | 
      df$is_heat_sensitive | 
      df$is_extreme_weather_sensitive
    
    # Seasonal surveillance priority
    df$seasonal_surveillance_case <- df$is_seasonal
    
    return(df)
  }, error = function(e) {
    cat("⚠️ Error adding rollup variables:", e$message, "\n")
    return(df)
  })
}

#' Add simplified computed metrics for analytics
add_computed_metrics <- function(df) {
  tryCatch({
    # Basic disease burden score
    df$disease_burden_score <- as.numeric(df$severity_weight)
    
    # Simple public health priority score (0-10 scale)
    df$public_health_priority_score <- (
      as.numeric(df$severity_weight) +                    # 1-3 points
        as.numeric(df$outbreak_prone_case) * 2 +           # 0-2 points
        as.numeric(df$climate_sensitive_case) +            # 0-1 points  
        as.numeric(df$pediatric_case) * 2 +                # 0-2 points
        as.numeric(df$emergency_syndrome_case) +           # 0-1 points
        as.numeric(df$amr_case)                            # 0-1 points
    )
    
    # Dashboard priority level (for filtering/grouping)
    df$dashboard_priority <- case_when(
      df$public_health_priority_score >= 8 ~ "Critical",
      df$public_health_priority_score >= 5 ~ "High",
      df$public_health_priority_score >= 3 ~ "Medium", 
      TRUE ~ "Low"
    )
    
    # Alert status (based on thresholds if available)
    df$has_alert_thresholds <- !is.na(df$alert_threshold_low) | 
      !is.na(df$alert_threshold_medium) | 
      !is.na(df$alert_threshold_high)
    
    return(df)
  }, error = function(e) {
    cat("⚠️ Error adding computed metrics:", e$message, "\n")
    return(df)
  })
}

# ================================================================================
# CONSOLIDATED create_epidemic_indicators function with error handling
# ================================================================================

#' Create epidemic indicators based on taxonomy (consolidated function)
#' 
#' This function creates epidemic, trauma, and climate indicators in one place
#' @param df data.frame with standardized disease columns
#' @return data.frame with epidemic indicators
create_epidemic_indicators <- function(df) {
  if (!"standardized_disease" %in% names(df)) {
    warning("No standardized_disease column found - using basic disease mapping")
    return(df)
  }
  
  cat("🚨 Creating epidemic indicators...\n")
  
  tryCatch({
    # Get taxonomy mappings
    epidemic_diseases <- get_epidemic_groups()
    trauma_diseases <- get_trauma_morbidities() %||% list()
    malnutrition_labels <- get_malnutrition_labels() %||% list()
    
    # Initialize epidemic flags
    df$is_epidemic <- FALSE
    df$cholera_case <- FALSE
    df$measles_case <- FALSE
    df$polio_case <- FALSE
    df$covid_case <- FALSE
    df$trauma_related <- FALSE
    df$malnutrition_related <- FALSE
    
    # Create epidemic disease indicators
    for (i in seq_len(nrow(df))) {
      disease <- df$standardized_disease[i]
      disease_str <- as.character(disease)
      if (is.na(disease_str) || !nzchar(disease_str)) next
      
      # Check if disease is in any epidemic group
      is_epidemic_disease <- any(sapply(epidemic_diseases, function(group) disease_str %in% group))
      if (is_epidemic_disease) df$is_epidemic[i] <- TRUE
      
      # Specific disease flags
      if (grepl("Cholera|Acute Watery Diarrhoea", disease_str, ignore.case = TRUE)) {
        df$cholera_case[i] <- TRUE
      }
      if (grepl("Measles", disease_str, ignore.case = TRUE)) {
        df$measles_case[i] <- TRUE
      }
      if (grepl("Polio|Acute Flaccid Paralysis", disease_str, ignore.case = TRUE)) {
        df$polio_case[i] <- TRUE
      }
      if (grepl("COVID-19|SARS", disease_str, ignore.case = TRUE)) {
        df$covid_case[i] <- TRUE
      }
      
      # Trauma indicators
      if (length(trauma_diseases) > 0) {
        trauma_list <- unlist(trauma_diseases)
        if (disease_str %in% trauma_list || grepl("Injury|Trauma|Fracture|Burn|Wound", disease_str, ignore.case = TRUE)) {
          df$trauma_related[i] <- TRUE
        }
      }
      
      # Malnutrition indicators
      if (length(malnutrition_labels) > 0) {
        malnut_list <- unlist(malnutrition_labels)
        if (disease_str %in% malnut_list || grepl("Malnutrition|Underweight|Stunting", disease_str, ignore.case = TRUE)) {
          df$malnutrition_related[i] <- TRUE
        }
      }
    }
    
    cat("✅ Epidemic indicators created\n")
    return(df)
    
  }, error = function(e) {
    cat("❌ Error creating epidemic indicators:", e$message, "\n")
    cat("🔧 Adding minimal epidemic indicators\n")
    
    # Add minimal indicators to prevent errors
    df$is_epidemic <- FALSE
    df$cholera_case <- FALSE
    df$measles_case <- FALSE
    df$polio_case <- FALSE
    df$covid_case <- FALSE
    df$trauma_related <- FALSE
    df$malnutrition_related <- FALSE
    
    return(df)
  })
}


# ================================================================================
# Internal: Helper for canonical mapping (tax-aware, always returns character)
# ================================================================================
# .map_to_canonical: Internal helper for mapping to canonical label (defensive version)
.map_to_canonical <- function(x, synonyms_lookup) {
  x <- as.character(x)
  k <- .norm(x)
  
  # Debug: Show sample of input and normalized keys
  sample_size <- min(5, length(x))
  if (sample_size > 0) {
    cat("🔍 Mapping sample diseases:\n")
    for (i in 1:sample_size) {
      raw_disease <- x[i]
      norm_key <- k[i]
      lookup_result <- synonyms_lookup[norm_key]
      cat("   ", raw_disease, "→", norm_key, "→", 
          if (is.na(lookup_result)) "NOT_FOUND" else as.character(lookup_result), "\n")
    }
  }
  
  hit <- synonyms_lookup[k]
  mapped_count <- sum(!is.na(hit) & nzchar(as.character(hit)))
  cat("✅ Mapped", mapped_count, "out of", length(x), "diseases\n")
  
  out <- ifelse(!is.na(hit) & nzchar(as.character(hit)), as.character(hit), "Unclassified")
  as.character(unname(out))
}

# Success message with testing
cat("✅ Config-free disease categorization functions loaded successfully (v4.5.0 - FIXED)\n")



# ================================================================================
# Alert Level Functions for Surveillance
# ================================================================================

#' Calculate epidemic alert level based on recent cases
#' @param category_name Disease category name
#' @param disease_display Disease display name
#' @param recent_cases Number of recent cases
#' @return Alert level (None, Low, Medium, High)
epidemic_alert_level <- function(category_name, disease_display, recent_cases) {
  recent_cases <- as.numeric(recent_cases)
  if (is.na(recent_cases)) return("None")
  
  # Disease-specific thresholds
  if (grepl("Cholera|AWD|Acute Watery", disease_display, ignore.case = TRUE)) {
    if (recent_cases >= 15) return("High")
    if (recent_cases >= 8) return("Medium") 
    if (recent_cases >= 3) return("Low")
  } else if (grepl("Measles", disease_display, ignore.case = TRUE)) {
    if (recent_cases >= 3) return("High")
    if (recent_cases >= 1) return("Medium")
  } else if (grepl("Polio|AFP", disease_display, ignore.case = TRUE)) {
    if (recent_cases >= 1) return("High")
  } else if (grepl("Pneumonia|ARI|Respiratory", disease_display, ignore.case = TRUE)) {
    if (recent_cases >= 50) return("High")
    if (recent_cases >= 25) return("Medium") 
    if (recent_cases >= 10) return("Low")
  } else {
    # General thresholds
    if (recent_cases >= 15) return("High")
    if (recent_cases >= 8) return("Medium") 
    if (recent_cases >= 3) return("Low")
  }
  
  return("None")
}

# ================================================================================
# Missing Functions Required by Summary Functions
# ================================================================================

#' Get Epidemic Diseases Standardized
#' 
#' Returns standardized epidemic disease categories
#' 
#' @return List of epidemic disease groups
#' @export
#' 
get_epidemic_diseases_standardized <- function() {
  return(get_epidemic_groups())
}

#' Get Malnutrition Categories  
#' 
#' Returns malnutrition condition labels
#' 
#' @return Vector of malnutrition condition names
#' @export
#' 
get_malnutrition_categories <- function() {
  return(get_malnutrition_labels())
}

#' Classify Trauma Case
#' 
#' Classifies trauma cases based on type and morbidity columns
#' 
#' @param df Data frame with health data
#' @param type_col Column name for case type (optional)
#' @param morbidity_col Column name for morbidity/disease
#' @return Vector indicating trauma classification
#' @export
#' 
classify_trauma_case <- function(df, type_col = NA, morbidity_col = "morbidity") {
  
  if (!morbidity_col %in% names(df)) {
    warning("Morbidity column not found, returning all FALSE")
    return(rep(FALSE, nrow(df)))
  }
  
  # Get trauma morbidities from taxonomy
  trauma_conditions <- get_trauma_morbidities()
  
  # Check morbidity column
  morbidity_trauma <- df[[morbidity_col]] %in% trauma_conditions
  
  # Check type column if available
  type_trauma <- FALSE
  if (!is.na(type_col) && type_col %in% names(df)) {
    type_trauma <- grepl("trauma|injury|wound|burn|fracture|accident", 
                        df[[type_col]], ignore.case = TRUE)
  }
  
  # Return logical vector
  return(morbidity_trauma | type_trauma)
}

#' Apply Epidemic Categorization
#' 
#' Applies epidemic disease categorization to a data frame
#' 
#' @param df Data frame with health data  
#' @param morbidity_col Column name for morbidity/disease
#' @return Data frame with epidemic categorization added
#' @export
#' 
apply_epidemic_categorization <- function(df, morbidity_col = "morbidity") {
  
  if (!morbidity_col %in% names(df)) {
    warning("Morbidity column not found")
    return(df)
  }
  
  # Get epidemic groups
  epidemic_groups <- get_epidemic_groups()
  epidemic_conditions <- unlist(epidemic_groups, use.names = FALSE)
  
  # Add epidemic flag
  df$is_epidemic <- df[[morbidity_col]] %in% epidemic_conditions
  
  # Add epidemic category
  df$epidemic_category <- NA_character_
  
  for (category in names(epidemic_groups)) {
    conditions <- epidemic_groups[[category]]
    matches <- df[[morbidity_col]] %in% conditions
    df$epidemic_category[matches] <- category
  }
  
  return(df)
}

# ================================================================================
# Additional functions needed by visualization_helpers.R
# ================================================================================

#' Get severity mapping for disease burden analysis  
#' @return list mapping disease patterns to severity scores
#' @export
get_severity_mapping <- function() {
  # Get the raw severity map from taxonomy
  severity_raw <- get_severity_map_raw()
  
  # Convert to format expected by visualization functions
  if (length(severity_raw) > 0) {
    return(severity_raw)
  } else {
    # Fallback mapping
    list(
      trauma = list(patterns = c("trauma", "injury", "wound", "fracture", "burn"), weight = 3.0),
      epidemic = list(patterns = c("cholera", "measles", "meningitis", "hepatitis", "typhoid"), weight = 2.5),  
      malnutrition = list(patterns = c("malnutrition", "kwashiorkor", "marasmus", "severe acute"), weight = 2.5),
      respiratory = list(patterns = c("pneumonia", "tuberculosis", "bronchitis"), weight = 2.0),
      diarrheal = list(patterns = c("diarrhea", "gastroenteritis", "dysentery"), weight = 2.0),
      fever = list(patterns = c("malaria", "fever", "sepsis"), weight = 1.8),
      chronic = list(patterns = c("diabetes", "hypertension", "heart", "asthma"), weight = 1.5),
      standard = list(patterns = c("cough", "headache", "pain", "skin", "eye"), weight = 1.0)
    )
  }
}

#' Get severity weights for burden calculation
#' @return named numeric vector of severity weights
#' @export
severity_weights <- function() {
  c(
    "Critical" = 3.0,
    "High" = 2.5,
    "Moderate" = 2.0, 
    "Standard" = 1.0
  )
}

#' Build trauma summary for trauma analysis (referenced by summary functions)
#' @param data Register data
#' @param admin_col Administrative column name
#' @param type_col Case type column name  
#' @param morbidity_col Morbidity column name
#' @return list with comprehensive trauma analysis
#' @export  
build_trauma_summary <- function(data, admin_col = "admin1", type_col = "type_case", morbidity_col = "morbidity") {
  if (!type_col %in% names(data)) {
    return(list(
      trauma_stats = tibble::tibble(),
      regional_stats = tibble::tibble(),
      summary = tibble::tibble(),
      total_trauma_cases = 0,
      trauma_percentage = 0
    ))
  }
  
  # Calculate trauma statistics
  trauma_stats <- data %>%
    dplyr::filter(!is.na(.data[[type_col]])) %>%
    dplyr::mutate(
      type_case_clean = .data[[type_col]],
      is_trauma = .data[[type_col]] == "Trauma"
    ) %>%
    dplyr::count(type_case_clean, name = "total_cases")
  
  total_trauma <- sum(trauma_stats$total_cases[trauma_stats$type_case_clean == "Trauma"], na.rm = TRUE)
  total_cases <- sum(trauma_stats$total_cases, na.rm = TRUE)
  trauma_pct <- if (total_cases > 0) round(total_trauma / total_cases * 100, 1) else 0
  
  # Regional breakdown
  if (admin_col %in% names(data)) {
    regional_stats <- data %>%
      dplyr::filter(!is.na(.data[[type_col]]), !is.na(.data[[admin_col]])) %>%
      dplyr::mutate(is_trauma = .data[[type_col]] == "Trauma") %>%
      dplyr::group_by(.data[[admin_col]]) %>%
      dplyr::summarise(
        total_cases = dplyr::n(),
        trauma_cases = sum(is_trauma, na.rm = TRUE),
        trauma_percentage = round(trauma_cases / total_cases * 100, 1),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(trauma_cases))
  } else {
    regional_stats <- tibble::tibble()
  }
  
  # Summary table for plotting - needs specific format for plot_trauma_distribution
  if (nrow(regional_stats) > 0) {
    # Create the format expected by plot_trauma_distribution
    trauma_rows <- regional_stats %>%
      dplyr::mutate(
        admin1 = .data[[admin_col]],
        type_case_clean = "Trauma",
        cases = trauma_cases,
        percentage = trauma_percentage
      ) %>%
      dplyr::select(admin1, type_case_clean, cases, percentage, total_cases)
    
    non_trauma_rows <- regional_stats %>%
      dplyr::mutate(
        admin1 = .data[[admin_col]],
        type_case_clean = "Non-trauma", 
        cases = total_cases - trauma_cases,
        percentage = round((total_cases - trauma_cases) / total_cases * 100, 1)
      ) %>%
      dplyr::select(admin1, type_case_clean, cases, percentage, total_cases)
    
    summary_table <- dplyr::bind_rows(trauma_rows, non_trauma_rows)
  } else {
    summary_table <- tibble::tibble(
      admin1 = character(),
      type_case_clean = character(), 
      cases = numeric(),
      percentage = numeric(),
      total_cases = numeric()
    )
  }
  
  list(
    trauma_stats = trauma_stats,
    regional_stats = regional_stats, 
    summary = summary_table,
    total_trauma_cases = total_trauma,
    trauma_percentage = trauma_pct
  )
}

cat("✅ Additional severity and trauma functions loaded successfully\n")