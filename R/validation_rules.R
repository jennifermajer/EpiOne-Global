# R/validation_rules.R
# Comprehensive data quality validation rules and functions

library(dplyr)
library(lubridate)
library(logger)
library(stringdist)
library(cluster)

#' Comprehensive data validation for register dataset
#'
#' @param register data.frame. Processed register data
#' @return List. Validation results with warnings and metrics
#'
validate_register_data <- function(register) {
  
  log_info("Starting comprehensive data validation")
  
  if (is.null(register) || nrow(register) == 0) {
    return(list(
      total_rows = 0,
      unique_facilities = 0,
      date_range = c(NA, NA),
      morbidity_categories = 0,
      missing_dates = 0,
      warnings = "Dataset is empty or null",
      validation_passed = FALSE
    ))
  }
  
  validation_results <- list(
    total_rows = nrow(register),
    total_columns = ncol(register),
    warnings = c(),
    errors = c(),
    quality_scores = list(),
    validation_passed = TRUE
  )
  
  # Date validation
  date_validation <- validate_dates(register)
  validation_results$missing_dates <- date_validation$missing_count
  validation_results$date_range <- date_validation$date_range
  validation_results$warnings <- c(validation_results$warnings, date_validation$warnings)
  
  # Geographic validation
  geo_validation <- validate_geographic_data(register)
  validation_results$unique_facilities <- geo_validation$unique_facilities
  validation_results$geographic_coverage <- geo_validation$coverage
  validation_results$warnings <- c(validation_results$warnings, geo_validation$warnings)
  
  # Clinical data validation
  clinical_validation <- validate_clinical_data(register)
  validation_results$morbidity_categories <- clinical_validation$morbidity_categories
  validation_results$warnings <- c(validation_results$warnings, clinical_validation$warnings)
  
  # Demographic validation
  demo_validation <- validate_demographic_data(register)
  validation_results$warnings <- c(validation_results$warnings, demo_validation$warnings)
  
  # Data consistency validation
  consistency_validation <- validate_data_consistency(register)
  validation_results$warnings <- c(validation_results$warnings, consistency_validation$warnings)
  
  # Calculate overall quality scores
  validation_results$quality_scores <- calculate_quality_scores(register, validation_results)
  
  # Determine if validation passed
  validation_results$validation_passed <- length(validation_results$errors) == 0
  
  log_info("Data validation completed: {length(validation_results$warnings)} warnings, {length(validation_results$errors)} errors")
  
  return(validation_results)
}

#' Validate date fields and temporal consistency
#'
#' @param register data.frame. Register data
#' @return List. Date validation results
#'
validate_dates <- function(register) {
  
  results <- list(
    missing_count = 0,
    date_range = c(NA, NA),
    warnings = c()
  )
  
  # Find date column
  date_col <- case_when(
    "datevisit" %in% names(register) ~ "datevisit",
    "eventdate" %in% names(register) ~ "eventdate",
    "date_visit" %in% names(register) ~ "date_visit",
    TRUE ~ NA_character_
  )
  
  if (is.na(date_col)) {
    results$warnings <- c(results$warnings, "No date column found in dataset")
    results$missing_count <- nrow(register)
    return(results)
  }
  
  # Validate dates
  date_data <- register[[date_col]]
  
  # Count missing dates
  results$missing_count <- sum(is.na(date_data))
  
  if (results$missing_count > 0) {
    missing_pct <- round(results$missing_count / nrow(register) * 100, 1)
    if (missing_pct > 5) {
      results$warnings <- c(results$warnings, 
                            paste0("High percentage of missing dates: ", missing_pct, "%"))
    }
  }
  
  # Calculate date range for valid dates
  valid_dates <- date_data[!is.na(date_data)]
  if (length(valid_dates) > 0) {
    results$date_range <- range(valid_dates)
    
    # Check for future dates
    future_dates <- sum(valid_dates > Sys.Date())
    if (future_dates > 0) {
      results$warnings <- c(results$warnings, 
                            paste0(future_dates, " records have future dates"))
    }
    
    # Check for very old dates (more than 10 years ago)
    old_cutoff <- Sys.Date() - years(10)
    old_dates <- sum(valid_dates < old_cutoff)
    if (old_dates > 0) {
      results$warnings <- c(results$warnings, 
                            paste0(old_dates, " records are more than 10 years old"))
    }
  }
  
  return(results)
}

#' Validate geographic and facility data
#'
#' @param register data.frame. Register data
#' @return List. Geographic validation results
#'
validate_geographic_data <- function(register) {
  
  results <- list(
    unique_facilities = 0,
    coverage = 0,
    warnings = c()
  )
  
  # Validate facility data
  facility_col <- case_when(
    "orgunit" %in% names(register) ~ "orgunit",
    "facility" %in% names(register) ~ "facility",
    "facility_name" %in% names(register) ~ "facility_name",
    TRUE ~ NA_character_
  )
  
  if (!is.na(facility_col)) {
    facility_data <- register[[facility_col]]
    results$unique_facilities <- length(unique(facility_data[!is.na(facility_data)]))
    
    missing_facilities <- sum(is.na(facility_data))
    if (missing_facilities > 0) {
      missing_pct <- round(missing_facilities / nrow(register) * 100, 1)
      if (missing_pct > 2) {
        results$warnings <- c(results$warnings, 
                              paste0("Missing facility data: ", missing_pct, "%"))
      }
    }
  } else {
    results$warnings <- c(results$warnings, "No facility identifier column found")
  }
  
  # Validate geographic coverage
  geo_col <- case_when(
    "admin1" %in% names(register) ~ "admin1",
    "governorate" %in% names(register) ~ "governorate",
    "region" %in% names(register) ~ "region",
    TRUE ~ NA_character_
  )
  
  if (!is.na(geo_col)) {
    geo_data <- register[[geo_col]]
    results$coverage <- length(unique(geo_data[!is.na(geo_data)]))
    
    missing_geo <- sum(is.na(geo_data))
    if (missing_geo > 0) {
      missing_pct <- round(missing_geo / nrow(register) * 100, 1)
      if (missing_pct > 2) {
        results$warnings <- c(results$warnings, 
                              paste0("Missing geographic data: ", missing_pct, "%"))
      }
    }
  } else {
    results$warnings <- c(results$warnings, "No geographic identifier column found")
  }
  
  return(results)
}

# ---- Org Unit metadata validation (patched) ----------------------------------

# --- Helpers -----------------------------------------------------------------
.norm_names <- function(x) tolower(gsub("[^a-z0-9]+", "_", x))
.str_norm  <- function(x) { x <- as.character(x); trimws(tolower(gsub("\\s+", " ", x))) }
.is_uid_like <- function(x) grepl("^[A-Za-z][A-Za-z0-9]{10}$", as.character(x))

.detect_ou_cols <- function(df, uid_threshold = 0.6) {
  nm <- .norm_names(names(df)); names(df) <- nm
  # broader pool; we'll score which one is UID-like
  cand_cols <- intersect(
    c("orgunitid","orgunit_uid","org_unit_id","orgunit_id","organisationunitid",
      "organisation_unit_id","orgunituid","org_unituid","ouid","uid","id",
      "code","orgunit","org_unit","orgunit_code"),
    nm
  )
  # score each candidate for UID-likeness
  uid_scores <- vapply(cand_cols, function(cn) {
    v <- df[[cn]]
    v <- v[!is.na(v) & nzchar(as.character(v))]
    if (!length(v)) return(0)
    v <- head(v, 2000) # sample
    mean(.is_uid_like(v))
  }, numeric(1))
  # choose id_col only if it looks UID-like
  id_col <- if (length(uid_scores) && max(uid_scores) >= uid_threshold) {
    names(which.max(uid_scores))
  } else NA_character_
  
  # name candidates
  name_cands <- intersect(
    c("orgunit", "orgunit_name","orgunitname","org_unit_name","organisation_unit_name",
      "organization_unit_name","facility_name","name","org_unit"),
    nm
  )
  # prefer something not chosen as id_col
  name_cands <- setdiff(name_cands, id_col)
  name_col <- if (length(name_cands)) name_cands[1] else NA_character_
  
  list(id_col = id_col, name_col = name_col)
}

.read_orgunit_metadata_from_file <- function() {
  if (!requireNamespace("readxl", quietly = TRUE)) return(NULL)
  cands <- c(
    here::here("data","metadata","Org Unit Metadata.xlsx"),
    "data/metadata/Org Unit Metadata.xlsx",
    here::here("data","metadata","org unit metadata.xlsx"),
    here::here("data","metadata","OrgUnitMetadata.xlsx")
  )
  cands <- unique(cands[file.exists(cands)])
  if (!length(cands)) return(NULL)
  df <- tryCatch(readxl::read_excel(cands[1]), error = function(e) NULL)
  if (is.null(df) || !nrow(df)) return(NULL)
  names(df) <- .norm_names(names(df))
  df
}

# --- Validator ---------------------------------------------------------------
validate_orgunit_metadata <- function(
    register,
    orgunit_meta = NULL,
    required_cols = c("region","admin1","admin2","admin3","facility_type")
) {
  if (!is.data.frame(register) || !nrow(register)) {
    return(list(valid = FALSE, issues = tibble::tibble(),
                warnings = "Empty or invalid register.",
                summary = list(n_facilities = 0, n_with_issues = NA_integer_)))
  }
  
  reg <- register; names(reg) <- .norm_names(names(reg))
  reg_cols <- .detect_ou_cols(reg)
  if (is.na(reg_cols$id_col) && is.na(reg_cols$name_col) && !"orgunit" %in% names(reg)) {
    return(list(valid = FALSE, issues = tibble::tibble(),
                warnings = "No org unit UID or name column found in register.",
                summary = list(n_facilities = 0, n_with_issues = NA_integer_)))
  }
  
  # Always provide 'orgunit' as the display name in outputs
  reg_display_orgunit <- if ("orgunit" %in% names(reg)) as.character(reg$orgunit) else {
    if (!is.na(reg_cols$name_col)) as.character(reg[[reg_cols$name_col]])
    else as.character(reg[[reg_cols$id_col]])
  }
  
  facs <- reg |>
    dplyr::transmute(
      orgunit      = reg_display_orgunit,
      orgunit_uid  = if (!is.na(reg_cols$id_col))  as.character(.data[[reg_cols$id_col]])  else NA_character_,
      orgunit_name = if (!is.na(reg_cols$name_col)) as.character(.data[[reg_cols$name_col]]) else NA_character_,
      name_key     = .str_norm(dplyr::coalesce(orgunit_name, orgunit))
    ) |>
    dplyr::filter(!(is.na(orgunit_uid) & is.na(orgunit_name) & is.na(orgunit))) |>
    dplyr::distinct()
  
  n_fac <- nrow(facs)
  
  meta <- if (!is.null(orgunit_meta)) orgunit_meta else .read_orgunit_metadata_from_file()
  if (is.null(meta) || !is.data.frame(meta) || !nrow(meta)) {
    return(list(valid = FALSE, issues = tibble::tibble(),
                warnings = "Org unit metadata not provided and could not be read from the Excel file.",
                summary = list(n_facilities = n_fac, n_with_issues = NA_integer_)))
  }
  names(meta) <- .norm_names(names(meta))
  meta_cols <- .detect_ou_cols(meta)
  
  req_cols_present <- intersect(required_cols, names(meta))
  req_cols_missing <- setdiff(required_cols, names(meta))
  
  meta2 <- meta |>
    dplyr::mutate(
      meta_uid  = if (!is.na(meta_cols$id_col))  as.character(.data[[meta_cols$id_col]])  else NA_character_,
      meta_name = if (!is.na(meta_cols$name_col)) as.character(.data[[meta_cols$name_col]]) else NA_character_,
      name_key  = ifelse(is.na(meta_name), NA_character_, .str_norm(meta_name))
    )
  
  # Try BOTH joins and pick the better match
  join_uid <- NULL; match_uid <- NA_real_
  if (!is.na(reg_cols$id_col) && !is.na(meta_cols$id_col)) {
    join_uid <- dplyr::left_join(
      facs, dplyr::select(meta2, meta_uid, meta_name, name_key, dplyr::all_of(req_cols_present)),
      by = c("orgunit_uid" = "meta_uid")
    )
    match_uid <- if (n_fac) mean(!is.na(join_uid$meta_uid) | !is.na(join_uid$meta_name)) else NA_real_
  }
  
  join_name <- NULL; match_name <- NA_real_
  # Only if we have names on both sides
  if (!all(is.na(facs$name_key)) && "name_key" %in% names(meta2)) {
    join_name <- dplyr::left_join(
      facs, dplyr::select(meta2, name_key, meta_uid, meta_name, dplyr::all_of(req_cols_present)),
      by = "name_key"
    )
    match_name <- if (n_fac) mean(!is.na(join_name$meta_uid) | !is.na(join_name$meta_name)) else NA_real_
  }
  
  # Choose best
  if (is.na(match_uid) && is.na(match_name)) {
    used_key <- "none"
    joined <- facs
  } else if (!is.na(match_uid) && (is.na(match_name) || match_uid >= match_name)) {
    used_key <- "uid"
    joined <- join_uid
  } else {
    used_key <- "name"
    joined <- join_name
  }
  match_rate <- if (used_key == "uid") match_uid else if (used_key == "name") match_name else 0
  
  # Missingness check
  is_missing <- function(x) { x <- as.character(x); is.na(x) | (trimws(x) == "") }
  miss_mat <- if (length(req_cols_present)) {
    as.data.frame(lapply(joined[req_cols_present], is_missing))
  } else data.frame()
  no_match <- is.null(joined$meta_uid) || (isTRUE(all(is.na(joined$meta_uid))) & isTRUE(all(is.na(joined$meta_name))))
  # build missing_fields vector
  missing_fields <- if (nrow(joined)) {
    if (length(req_cols_present)) {
      mf <- apply(miss_mat, 1, function(r) {
        miss <- req_cols_present[isTRUE(r)]
        if (length(miss) == 0) "" else paste(miss, collapse = ", ")
      })
      # if this row didn't match at all, mark all required + metadata_row
      unmatched_row <- is.na(joined$meta_uid) & is.na(joined$meta_name)
      mf[unmatched_row] <- if (length(req_cols_present)) paste(c(req_cols_present, "metadata_row"), collapse = ", ") else "metadata_row"
      mf
    } else {
      rep("metadata_row", nrow(joined))
    }
  } else character(0)
  
  issues <- if (nrow(joined)) {
    dplyr::bind_cols(joined, missing_fields = missing_fields) |>
      dplyr::filter(missing_fields != "") |>
      dplyr::mutate(facility_name = orgunit) |>
      dplyr::select(
        orgunit, orgunit_uid, orgunit_name, facility_name,
        dplyr::all_of(req_cols_present), missing_fields
      )
  } else tibble::tibble()
  
  warnings <- character(0)
  if (!is.na(match_uid))  warnings <- c(warnings, sprintf("UID match rate: %.1f%%.", 100*match_uid))
  if (!is.na(match_name)) warnings <- c(warnings, sprintf("Name match rate: %.1f%%.", 100*match_name))
  if (length(req_cols_missing)) {
    warnings <- c(warnings, paste0("Metadata is missing required columns: ", paste(req_cols_missing, collapse = ", "), "."))
  }
  if (!is.na(match_rate) && match_rate < 0.9) {
    warnings <- c(warnings, sprintf("Using %s key. Only %.1f%% of facilities matched.", used_key, 100*match_rate))
  }
  
  list(
    valid   = nrow(issues) == 0 && length(req_cols_missing) == 0 && !is.na(match_rate) && match_rate >= 0.9,
    issues  = issues,
    warnings = warnings,
    summary = list(n_facilities = n_fac, n_with_issues = nrow(issues),
                   match_rate = match_rate, join_key = used_key,
                   match_uid = match_uid, match_name = match_name)
  )
}

#' Validate clinical data quality
#'
#' @param register data.frame. Register data
#' @return List. Clinical validation results
#'
validate_clinical_data <- function(register) {
  
  results <- list(
    morbidity_categories = 0,
    warnings = c()
  )
  
  # Validate morbidity/disease data
  morbidity_col <- case_when(
    "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
    "morbidity" %in% names(register) ~ "morbidity",
    "icd11_title" %in% names(register) ~ "icd_11_title",
    "category_canonical_disease_imc" %in% names(register) ~ "category_canonical_disease_imc",
    "disease" %in% names(register) ~ "disease",
    TRUE ~ NA_character_
  )
  
  if (!is.na(morbidity_col)) {
    morbidity_data <- register[[morbidity_col]]
    results$morbidity_categories <- length(unique(morbidity_data[!is.na(morbidity_data)]))
    
    missing_morbidity <- sum(is.na(morbidity_data) | morbidity_data == "")
    if (missing_morbidity > 0) {
      missing_pct <- round(missing_morbidity / nrow(register) * 100, 1)
      if (missing_pct > 1) {
        results$warnings <- c(results$warnings, 
                              paste0("Missing disease/morbidity data: ", missing_pct, "%"))
      }
    }
    
    # Check for suspicious categories
    unknown_categories <- sum(grepl("unknown|unspecified|uncategorized|other", morbidity_data, ignore.case = TRUE), na.rm = TRUE)
    if (unknown_categories > 0) {
      unknown_pct <- round(unknown_categories / nrow(register) * 100, 1)
      if (unknown_pct > 10) {
        results$warnings <- c(results$warnings, 
                              paste0("High percentage of unspecified diseases: ", unknown_pct, "%"))
      }
    }
  } else {
    results$warnings <- c(results$warnings, "No morbidity/disease column found")
  }
  
  return(results)
}

#' Generate comprehensive ICD-11 mapping quality report for manual updates
#' 
#' @param data Optional data.frame. If NULL, uses register from environment
#' @param output_file Character. Output Excel filename 
#' @param include_low_confidence Logical. Include low confidence mappings
#' @return List. Summary statistics and path to generated Excel file
#' 
export_unmapped_diseases <- function(data = NULL, 
                                   output_file = "icd11_mapping_quality_report.xlsx",
                                   include_low_confidence = TRUE) {
  
  # Load required libraries
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("openxlsx package required. Install with: install.packages('openxlsx')")
  }
  
  cat("🔍 Generating comprehensive ICD-11 mapping quality report...\n")
  
  # Load data if not provided
  if (is.null(data)) {
    if (exists("register") && !is.null(register)) {
      data <- register
      cat("📊 Using loaded register data:", nrow(data), "records\n")
    } else {
      stop("No data provided and no register data available in environment")
    }
  }
  
  # Check for required ICD-11 columns
  icd11_cols <- c("icd11_code", "icd11_title", "icd11_category", "confidence")
  available_cols <- icd11_cols[icd11_cols %in% names(data)]
  
  if (length(available_cols) == 0) {
    stop("No ICD-11 mapping columns found in data. Expected: ", paste(icd11_cols, collapse = ", "))
  }
  
  cat("📋 Available ICD-11 columns:", paste(available_cols, collapse = ", "), "\n")
  
  # Comprehensive mapping quality assessment
  mapping_quality <- data %>%
    mutate(
      has_code = !is.na(icd11_code) & icd11_code != "",
      has_title = !is.na(icd11_title) & icd11_title != "",
      has_category = if("icd11_category" %in% names(.)) !is.na(icd11_category) & icd11_category != "" else FALSE,
      confidence_level = case_when(
        is.na(confidence) | confidence == "" ~ "none",
        confidence == "low" ~ "low", 
        confidence == "medium" ~ "medium",
        confidence == "high" ~ "high",
        TRUE ~ "unknown"
      ),
      mapping_status = case_when(
        has_code & has_title & confidence_level == "high" ~ "Complete_High",
        has_code & has_title & confidence_level == "medium" ~ "Complete_Medium", 
        has_code & has_title & confidence_level == "low" ~ "Complete_Low",
        has_code & !has_title ~ "Code_Only",
        !has_code & has_title ~ "Title_Only",
        !has_code & !has_title ~ "Unmapped",
        TRUE ~ "Partial"
      )
    )
  
  # Detailed analysis by disease
  disease_analysis <- mapping_quality %>%
    group_by(morbidity, mapping_status, confidence_level) %>%
    summarise(
      total_cases = n(),
      case_percentage = round(n() / nrow(data) * 100, 2),
      has_code = any(has_code),
      has_title = any(has_title), 
      has_category = any(has_category),
      first_occurrence = min(datevisit, na.rm = TRUE),
      last_occurrence = max(datevisit, na.rm = TRUE),
      unique_facilities = n_distinct(orgunit, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_cases)) %>%
    mutate(
      priority_level = case_when(
        total_cases >= 1000 ~ "HIGH - Map Immediately",
        total_cases >= 100 ~ "MEDIUM - Map This Week", 
        TRUE ~ "LOW - Map When Possible"
      ),
      action_needed = case_when(
        mapping_status == "Unmapped" ~ "Add ICD-11 code and title",
        mapping_status == "Code_Only" ~ "Add ICD-11 title", 
        mapping_status == "Title_Only" ~ "Add ICD-11 code",
        mapping_status == "Complete_Low" ~ "Review and improve confidence",
        mapping_status == "Complete_Medium" ~ "Verify accuracy, upgrade if appropriate",
        TRUE ~ "Verify completeness"
      )
    )
  
  # Identify diseases needing updates
  needs_updates <- disease_analysis %>%
    filter(mapping_status %in% c("Unmapped", "Code_Only", "Title_Only", "Partial") |
           (include_low_confidence & confidence_level %in% c("low", "none"))) %>%
    mutate(
      current_icd11_code = "",
      current_icd11_title = "", 
      current_icd11_category = "",
      new_icd11_code = "",     # Empty for manual entry
      new_icd11_title = "",    # Empty for manual entry  
      new_icd11_category = "", # Empty for manual entry
      new_confidence = "",     # Empty for manual entry
      notes = ""               # Empty for notes
    ) %>%
    select(morbidity, mapping_status, confidence_level, total_cases, case_percentage,
           priority_level, action_needed, current_icd11_code, current_icd11_title, 
           current_icd11_category, new_icd11_code, new_icd11_title, 
           new_icd11_category, new_confidence, notes)
  
  # Calculate comprehensive quality statistics
  total_diseases <- n_distinct(data$morbidity, na.rm = TRUE)
  total_cases <- nrow(data)
  
  quality_stats <- mapping_quality %>%
    summarise(
      complete_high = sum(mapping_status == "Complete_High"),
      complete_medium = sum(mapping_status == "Complete_Medium"),
      complete_low = sum(mapping_status == "Complete_Low"),
      code_only = sum(mapping_status == "Code_Only"),
      title_only = sum(mapping_status == "Title_Only"), 
      unmapped = sum(mapping_status == "Unmapped"),
      partial = sum(mapping_status == "Partial"),
      .groups = "drop"
    )
  
  high_quality_cases <- quality_stats$complete_high
  all_mapped_cases <- quality_stats$complete_high + quality_stats$complete_medium + quality_stats$complete_low
  needs_update_cases <- sum(needs_updates$total_cases)
  
  # Create Excel workbook
  wb <- openxlsx::createWorkbook()
  
  # Enhanced Summary sheet
  openxlsx::addWorksheet(wb, "Summary")
  summary <- data.frame(
    Metric = c(
      "Total Diseases in Dataset",
      "Total Consultation Records", 
      "High Quality Mappings (Complete + High Confidence)",
      "All Mapped Records (Any Confidence)",
      "Records Needing Updates",
      "High Quality Coverage",
      "Overall Mapping Coverage",
      "Diseases Needing Updates",
      "Unmapped Diseases",
      "Code-Only Diseases",
      "Title-Only Diseases", 
      "Low Confidence Diseases"
    ),
    Value = c(
      total_diseases,
      format(total_cases, big.mark = ","),
      format(high_quality_cases, big.mark = ","),
      format(all_mapped_cases, big.mark = ","), 
      format(needs_update_cases, big.mark = ","),
      paste0(round(high_quality_cases / total_cases * 100, 1), "%"),
      paste0(round(all_mapped_cases / total_cases * 100, 1), "%"),
      nrow(needs_updates),
      sum(disease_analysis$mapping_status == "Unmapped"),
      sum(disease_analysis$mapping_status == "Code_Only"),
      sum(disease_analysis$mapping_status == "Title_Only"),
      sum(disease_analysis$confidence_level == "low")
    ),
    Priority = c(
      "", "",
      ifelse(high_quality_cases / total_cases < 0.8, "IMPROVE", "GOOD"),
      ifelse(all_mapped_cases / total_cases < 0.9, "CRITICAL", "GOOD"),
      ifelse(needs_update_cases / total_cases > 0.2, "HIGH", "MEDIUM"),
      "", "", "",
      ifelse(sum(disease_analysis$mapping_status == "Unmapped") > 20, "HIGH", "MEDIUM"),
      ifelse(sum(disease_analysis$mapping_status == "Code_Only") > 10, "MEDIUM", "LOW"),
      ifelse(sum(disease_analysis$mapping_status == "Title_Only") > 5, "MEDIUM", "LOW"), 
      ifelse(sum(disease_analysis$confidence_level == "low") > 15, "MEDIUM", "LOW")
    )
  )
  openxlsx::writeData(wb, "Summary", summary)
  
  # Format summary sheet
  summary_header_style <- openxlsx::createStyle(fontColour = "white", fgFill = "#2E75B6", 
                                               textDecoration = "Bold")
  openxlsx::addStyle(wb, "Summary", summary_header_style, rows = 1, cols = 1:3)
  
  # Diseases needing updates sheet
  if (nrow(needs_updates) > 0) {
    openxlsx::addWorksheet(wb, "Diseases_Needing_Updates")
    openxlsx::writeData(wb, "Diseases_Needing_Updates", needs_updates)
    
    # Format headers
    header_style <- openxlsx::createStyle(fontColour = "white", fgFill = "#4472C4", 
                                         textDecoration = "Bold")
    openxlsx::addStyle(wb, "Diseases_Needing_Updates", header_style, rows = 1, 
                      cols = 1:ncol(needs_updates))
    
    # Color-code priorities (only if data exists)
    if (nrow(needs_updates) > 0) {
      high_style <- openxlsx::createStyle(fgFill = "#FFE6E6")
      medium_style <- openxlsx::createStyle(fgFill = "#FFF2CC")
      low_style <- openxlsx::createStyle(fgFill = "#E6F3E6")
      
      high_rows <- which(grepl("HIGH", needs_updates$priority_level)) + 1
      medium_rows <- which(grepl("MEDIUM", needs_updates$priority_level)) + 1
      low_rows <- which(grepl("LOW", needs_updates$priority_level)) + 1
      
      # Apply styles row by row to avoid length mismatch
      for (row in high_rows) {
        openxlsx::addStyle(wb, "Diseases_Needing_Updates", high_style, 
                          rows = row, cols = seq_len(ncol(needs_updates)), gridExpand = TRUE)
      }
      for (row in medium_rows) {
        openxlsx::addStyle(wb, "Diseases_Needing_Updates", medium_style, 
                          rows = row, cols = seq_len(ncol(needs_updates)), gridExpand = TRUE)
      }
      for (row in low_rows) {
        openxlsx::addStyle(wb, "Diseases_Needing_Updates", low_style, 
                          rows = row, cols = seq_len(ncol(needs_updates)), gridExpand = TRUE)
      }
    }
    
    # Set column widths for readability
    openxlsx::setColWidths(wb, "Diseases_Needing_Updates", cols = 1:ncol(needs_updates), 
                          widths = "auto")
  }
  
  # Complete quality analysis sheet
  openxlsx::addWorksheet(wb, "Complete_Analysis")
  openxlsx::writeData(wb, "Complete_Analysis", disease_analysis)
  
  analysis_header_style <- openxlsx::createStyle(fontColour = "white", fgFill = "#70AD47", 
                                               textDecoration = "Bold")
  openxlsx::addStyle(wb, "Complete_Analysis", analysis_header_style, rows = 1, 
                    cols = 1:ncol(disease_analysis))
  
  # Set column widths
  openxlsx::setColWidths(wb, "Complete_Analysis", cols = 1:ncol(disease_analysis), 
                        widths = "auto")
  
  # Enhanced Instructions sheet
  openxlsx::addWorksheet(wb, "Instructions")
  instructions <- c(
    "COMPREHENSIVE ICD-11 DISEASE MAPPING UPDATE GUIDE",
    "==================================================",
    "",
    "OVERVIEW:",
    "This report identifies diseases needing ICD-11 mapping improvements across",
    "three key areas: icd11_code, icd11_title, and icd11_category fields.",
    "",
    "EXCEL SHEETS EXPLAINED:",
    "• Summary: Overall quality statistics and coverage metrics",
    "• Diseases_Needing_Updates: Priority-ranked diseases for manual work", 
    "• Complete_Analysis: Full quality assessment of all diseases",
    "• Instructions: This guide",
    "",
    "STEP-BY-STEP UPDATE PROCESS:",
    "",
    "1. PRIORITIZE YOUR WORK:",
    "   → Start with HIGH priority diseases (RED rows) - highest case volume",
    "   → Work through MEDIUM priority (YELLOW rows) next",  
    "   → Address LOW priority (GREEN rows) when time permits",
    "",
    "2. UNDERSTAND MAPPING STATUS CODES:",
    "   • Unmapped: No ICD-11 code or title - needs complete mapping",
    "   • Code_Only: Has code but missing title - add title",
    "   • Title_Only: Has title but missing code - add code", 
    "   • Complete_Low: Has code+title but low confidence - verify accuracy",
    "   • Complete_Medium: Medium confidence - consider upgrading to high",
    "",
    "3. RESEARCH ICD-11 CODES:",
    "   Primary source: https://icd.who.int/browse11",
    "   • Search by disease name, symptoms, or body system",
    "   • Look for exact matches first, then broader categories",
    "   • Use 'unspecified' variants when specific codes unavailable",
    "   • Note both code (e.g., 'CA41.Z') and official title",
    "",
    "4. UPDATE TAXONOMY FILE:",
    "   Edit: taxonomy/icd11/disease_mappings.yml",
    "   Add under appropriate confidence section:",
    "",
    "   high_confidence:",
    "     'Disease Name':",
    "       icd11_code: 'CA41.Z'",
    "       icd11_title: 'Acute bronchiolitis, unspecified'", 
    "       icd11_category: 'Respiratory diseases'",
    "       confidence: 'high'",
    "       source: 'manual'",
    paste0("       mapped_date: '", as.character(Sys.Date()), "'"),
    "",
    "5. QUALITY CONTROL CHECKLIST:",
    "   ☐ ICD-11 code format correct (e.g., 1A40.Z, CA41.Z)",
    "   ☐ Title matches official WHO ICD-11 terminology",
    "   ☐ Category reflects primary disease classification", 
    "   ☐ Confidence level appropriate (high for exact matches)",
    "   ☐ Clinical mapping is medically accurate",
    "",
    "6. AFTER MAKING UPDATES:",
    "   a) Clean HTML markup: cleanup_existing_mappings()",
    "   b) Reload data to apply new mappings",
    "   c) Re-run this report to verify improvements:",
    "      export_unmapped_diseases(data = register)",
    "   d) Check coverage improvement in new summary",
    "",
    "7. CONFIDENCE LEVEL GUIDELINES:",
    "   • HIGH: Exact ICD-11 match, clinically precise",
    "   • MEDIUM: Close match, appropriate category, minor uncertainty",
    "   • LOW: Broad category mapping, significant uncertainty",
    "",
    "8. COMMON MAPPING SCENARIOS:",
    "   → Exact disease match: Use specific ICD-11 code + high confidence",
    "   → Similar condition: Use closest ICD-11 code + medium confidence", 
    "   → General symptoms: Use broader category + low confidence",
    "   → Local terminology: Map to standard WHO terminology",
    "",
    "9. EXAMPLE MAPPINGS:",
    "   'Acute bronchitis bronchiolitis' → CA41.Z (Acute bronchiolitis, unspecified)",
    "   'Influenza Like Illness' → 1E32 (Influenza, virus not identified)",
    "   'Gastrointestinal infection' → 1A40.Z (Infectious gastroenteritis...)",
    "",
    "10. VALIDATION AND TESTING:",
    "    • Test mappings with subset of data before full deployment",
    "    • Verify clinical appropriateness with medical staff", 
    "    • Monitor coverage improvement and quality metrics",
    "    • Document rationale for complex mapping decisions",
    "",
    "SUPPORT:",
    "Contact technical team for:",
    "• Complex disease mapping decisions",
    "• ICD-11 code verification", 
    "• System integration issues",
    "• Bulk mapping updates",
    "",
    paste0("Generated: ", as.character(Sys.Date())),
    "Report version: 2.0 (Comprehensive Quality Assessment)"
  )
  openxlsx::writeData(wb, "Instructions", data.frame(Instructions = instructions))
  
  # Set column width for instructions
  openxlsx::setColWidths(wb, "Instructions", cols = 1, widths = 120)
  
  # Save file
  output_path <- here::here(output_file)
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  
  cat("✅ Comprehensive ICD-11 quality report saved:", output_path, "\n")
  cat("\n📊 MAPPING QUALITY SUMMARY:\n")
  cat("   Total diseases:", total_diseases, "\n")
  cat("   High quality mappings:", format(high_quality_cases, big.mark = ","), 
      "(", round(high_quality_cases / total_cases * 100, 1), "%)\n")
  cat("   All mapped records:", format(all_mapped_cases, big.mark = ","),
      "(", round(all_mapped_cases / total_cases * 100, 1), "%)\n") 
  cat("   Diseases needing updates:", nrow(needs_updates), "\n")
  cat("   Records needing updates:", format(needs_update_cases, big.mark = ","),
      "(", round(needs_update_cases / total_cases * 100, 1), "%)\n")
  
  # Return comprehensive summary
  return(list(
    output_file = output_path,
    summary_stats = list(
      total_diseases = total_diseases,
      total_cases = total_cases,
      high_quality_cases = high_quality_cases,
      all_mapped_cases = all_mapped_cases, 
      needs_update_cases = needs_update_cases,
      high_quality_coverage = round(high_quality_cases / total_cases * 100, 1),
      overall_coverage = round(all_mapped_cases / total_cases * 100, 1),
      diseases_needing_updates = nrow(needs_updates)
    ),
    mapping_breakdown = quality_stats
  ))
}

#' Validate demographic data
#'
#' @param register data.frame. Register data
#' @return List. Demographic validation results
#'
validate_demographic_data <- function(register) {
  
  results <- list(
    warnings = c()
  )
  
  # Validate age data
  age_col <- case_when(
    "age_group_new" %in% names(register) ~ "age_group_new",
    "age_group" %in% names(register) ~ "age_group",
    "age_group2" %in% names(register) ~ "age_group2",
    "age_group3" %in% names(register) ~ "age_group3",
    "age" %in% names(register) ~ "age",
    TRUE ~ NA_character_
  )
  
  if (!is.na(age_col)) {
    age_data <- register[[age_col]]
    missing_age <- sum(is.na(age_data) | age_data == "")
    if (missing_age > 0) {
      missing_pct <- round(missing_age / nrow(register) * 100, 1)
      if (missing_pct > 10) {
        results$warnings <- c(results$warnings, 
                              paste0("Missing age data: ", missing_pct, "%"))
      }
    }
  } else {
    results$warnings <- c(results$warnings, "No age column found")
  }
  
  # Validate sex data
  if ("sex" %in% names(register)) {
    sex_data <- register$sex
    missing_sex <- sum(is.na(sex_data) | sex_data == "")
    if (missing_sex > 0) {
      missing_pct <- round(missing_sex / nrow(register) * 100, 1)
      if (missing_pct > 10) {
        results$warnings <- c(results$warnings, 
                              paste0("Missing sex data: ", missing_pct, "%"))
      }
    }
    
    # Check for valid sex values
    valid_sex_values <- c("Male", "Female", "M", "F", "male", "female")
    invalid_sex <- sum(!sex_data %in% valid_sex_values & !is.na(sex_data))
    if (invalid_sex > 0) {
      results$warnings <- c(results$warnings, 
                            paste0(invalid_sex, " records have invalid sex values"))
    }
  } else {
    results$warnings <- c(results$warnings, "No sex column found")
  }
  
  return(results)
}

#' Validate data consistency
#'
#' @param register data.frame. Register data
#' @return List. Consistency validation results
#'
validate_data_consistency <- function(register) {
  
  results <- list(
    warnings = c()
  )
  
  # Check for duplicate records
  if (all(c("datevisit", "orgunit") %in% names(register)) ||
      all(c("eventdate", "facility") %in% names(register))) {
    
    date_col <- if ("datevisit" %in% names(register)) "datevisit" else "eventdate"
    facility_col <- if ("orgunit" %in% names(register)) "orgunit" else "facility"
    
    duplicate_check <- register %>%
      filter(!is.na(.data[[date_col]]) & !is.na(.data[[facility_col]])) %>%
      group_by(.data[[date_col]], .data[[facility_col]]) %>%
      summarise(count = n(), .groups = "drop") %>%
      filter(count > 50)  # More than 50 consultations per day per facility seems suspicious
    
    if (nrow(duplicate_check) > 0) {
      results$warnings <- c(results$warnings, 
                            paste0(nrow(duplicate_check), " facility-days with >50 consultations (possible data quality issue)"))
    }
  }
  
  # Check for temporal consistency
  if ("datevisit" %in% names(register) && "quarter" %in% names(register)) {
    quarter_check <- register %>%
      filter(!is.na(datevisit) & !is.na(quarter)) %>%
      mutate(
        expected_quarter = paste0(year(datevisit), "Q", quarter(datevisit)),
        quarter_match = (quarter == expected_quarter)
      ) %>%
      summarise(mismatched = sum(!quarter_match, na.rm = TRUE))
    
    if (quarter_check$mismatched > 0) {
      results$warnings <- c(results$warnings, 
                            paste0(quarter_check$mismatched, " records have inconsistent quarter assignments"))
    }
  }
  
  return(results)
}

#' Calculate quality scores
#'
#' @param register data.frame. Register data
#' @param validation_results List. Validation results
#' @return List. Quality scores
#'
calculate_quality_scores <- function(register, validation_results) {
  
  scores <- list()
  
  # Completeness score (based on missing data)
  total_records <- nrow(register)
  missing_dates <- validation_results$missing_dates %||% 0
  
  completeness_score <- max(0, 100 - (missing_dates / total_records * 100))
  scores$completeness <- round(completeness_score, 1)
  
  # Consistency score (based on warnings)
  consistency_score <- max(0, 100 - (length(validation_results$warnings) * 10))
  scores$consistency <- round(consistency_score, 1)
  
  # Coverage score (based on facilities and geography)
  coverage_score <- if (validation_results$unique_facilities > 10) 100 else 
    (validation_results$unique_facilities / 10) * 100
  scores$coverage <- round(coverage_score, 1)
  
  # Overall score (weighted average)
  scores$overall <- round((scores$completeness * 0.4 + 
                             scores$consistency * 0.3 + 
                             scores$coverage * 0.3), 1)
  
  return(scores)
}

#' Alternative validation function for compatibility
#'
#' @param register data.frame. Register data
#' @return List. Validation results
#'
validate_epi_data <- function(register) {
  
  # Call the main validation function
  results <- validate_register_data(register)
  
  # Add legacy fields for compatibility
  results$valid <- results$validation_passed
  results$errors <- results$warnings  # Map warnings to errors for compatibility
  results$data_quality_score <- results$quality_scores$overall %||% 0
  
  return(results)
}

#' Quick data quality check
#'
#' @param register data.frame. Register data
#' @return List. Quick quality assessment
#'
quick_quality_check <- function(register) {
  
  if (is.null(register) || nrow(register) == 0) {
    return(list(score = 0, status = "No Data"))
  }
  
  # Quick checks
  total_records <- nrow(register)
  
  # Check date completeness
  date_col <- case_when(
    "datevisit" %in% names(register) ~ "datevisit",
    "eventdate" %in% names(register) ~ "eventdate",
    TRUE ~ NA_character_
  )
  
  date_completeness <- if (!is.na(date_col)) {
    1 - (sum(is.na(register[[date_col]])) / total_records)
  } else { 0.5 }
  
  # Check facility completeness
  facility_col <- case_when(
    "orgunit" %in% names(register) ~ "orgunit",
    "facility" %in% names(register) ~ "facility",
    TRUE ~ NA_character_
  )
  
  facility_completeness <- if (!is.na(facility_col)) {
    1 - (sum(is.na(register[[facility_col]])) / total_records)
  } else { 0.5 }
  
  # Check disease completeness
  disease_col <- case_when(
    "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
    "morbidity" %in% names(register) ~ "morbidity",
    "category_canonical_disease_imc" %in% names(register) ~ "category_canonical_disease_imc",
    TRUE ~ NA_character_
  )
  
  disease_completeness <- if (!is.na(disease_col)) {
    1 - (sum(is.na(register[[disease_col]]) | register[[disease_col]] == "") / total_records)
  } else { 0.5 }
  
  # Calculate overall score
  overall_score <- round((date_completeness * 0.4 + 
                            facility_completeness * 0.3 + 
                            disease_completeness * 0.3) * 100, 0)
  
  status <- case_when(
    overall_score >= 90 ~ "Excellent",
    overall_score >= 80 ~ "Good",
    overall_score >= 70 ~ "Fair",
    overall_score >= 60 ~ "Poor",
    TRUE ~ "Critical"
  )
  
  return(list(score = overall_score, status = status))
}

#' Validate specific epidemic diseases
#'
#' @param register data.frame. Register data
#' @param epidemic_diseases List. Epidemic disease definitions
#' @return List. Epidemic validation results
#'
validate_epidemic_diseases <- function(register, epidemic_diseases = NULL) {
  
  if (is.null(epidemic_diseases)) {
    # Load from disease categories if available
    if (exists("get_epidemic_diseases")) {
      epidemic_diseases <- get_epidemic_diseases()
    } else {
      return(list(warnings = "Epidemic disease definitions not available"))
    }
  }
  
  results <- list(
    epidemic_cases_found = 0,
    diseases_detected = 0,
    warnings = c()
  )
  
  # Find disease column
  disease_col <- case_when(
    "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
    "morbidity" %in% names(register) ~ "morbidity",
    "category_canonical_disease_imc" %in% names(register) ~ "category_canonical_disease_imc",
    TRUE ~ NA_character_
  )
  
  if (is.na(disease_col)) {
    results$warnings <- c(results$warnings, "No disease column found for epidemic validation")
    return(results)
  }
  
  # Check for epidemic diseases
  all_epidemic_conditions <- unlist(epidemic_diseases, use.names = FALSE)
  epidemic_cases <- register %>%
    filter(.data[[disease_col]] %in% all_epidemic_conditions)
  
  results$epidemic_cases_found <- nrow(epidemic_cases)
  
  if (results$epidemic_cases_found > 0) {
    results$diseases_detected <- length(unique(epidemic_cases[[disease_col]]))
    
    # Check for high-priority diseases
    high_priority <- c("Acute Watery Diarrhea", "Measles", "Acute Flaccid Paralysis", "Meningitis")
    high_priority_conditions <- unlist(epidemic_diseases[high_priority], use.names = FALSE)
    
    high_priority_cases <- sum(register[[disease_col]] %in% high_priority_conditions, na.rm = TRUE)
    if (high_priority_cases > 0) {
      results$warnings <- c(results$warnings, 
                            paste0(high_priority_cases, " high-priority epidemic disease cases detected"))
    }
  }
  
  return(results)
}

#' Generate data quality report
#'
#' @param register data.frame. Register data
#' @return Character. Formatted quality report
#'
generate_quality_report <- function(register) {
  
  validation_results <- validate_register_data(register)
  
  report <- paste0(
    "=== DATA QUALITY REPORT ===\n",
    "Generated: ", Sys.time(), "\n\n",
    "Dataset Overview:\n",
    "- Total Records: ", format(validation_results$total_rows, big.mark = ","), "\n",
    "- Unique Facilities: ", validation_results$unique_facilities, "\n",
    "- Date Range: ", 
    if (all(!is.na(validation_results$date_range))) {
      paste(format(validation_results$date_range, "%Y-%m-%d"), collapse = " to ")
    } else {
      "Not available"
    }, "\n",
    "- Disease Categories: ", validation_results$morbidity_categories, "\n\n"
  )
  
  if (length(validation_results$quality_scores) > 0) {
    report <- paste0(report,
                     "Quality Scores:\n",
                     "- Completeness: ", validation_results$quality_scores$completeness, "/100\n",
                     "- Consistency: ", validation_results$quality_scores$consistency, "/100\n",
                     "- Coverage: ", validation_results$quality_scores$coverage, "/100\n",
                     "- Overall: ", validation_results$quality_scores$overall, "/100\n\n"
    )
  }
  
  if (length(validation_results$warnings) > 0) {
    report <- paste0(report,
                     "Quality Issues:\n",
                     paste(paste("-", validation_results$warnings), collapse = "\n"), "\n\n"
    )
  } else {
    report <- paste0(report, "✅ No quality issues detected\n\n")
  }
  
  report <- paste0(report, "=== END REPORT ===\n")
  
  return(report)
}

#' Basic data validation for minimal datasets
#'
#' @param data data.frame. Any dataset
#' @return List. Basic validation results
#'
basic_data_validation <- function(data) {
  
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      valid = FALSE,
      message = "Dataset is empty or null",
      row_count = 0,
      col_count = 0
    ))
  }
  
  results <- list(
    valid = TRUE,
    message = "Basic validation passed",
    row_count = nrow(data),
    col_count = ncol(data),
    missing_data_pct = round(sum(is.na(data)) / (nrow(data) * ncol(data)) * 100, 1)
  )
  
  # Check for excessive missing data
  if (results$missing_data_pct > 50) {
    results$valid <- FALSE
    results$message <- paste0("High missing data percentage: ", results$missing_data_pct, "%")
  }
  
  # Check for minimum viable dataset
  if (results$row_count < 10) {
    results$valid <- FALSE
    results$message <- "Dataset too small for analysis (< 10 records)"
  }
  
  return(results)
}

# Machine Learning Disease Mapping Functions ====================================

#' Prepare training data from existing high-confidence mappings
#'
#' @param data data.frame. Dataset with disease mappings
#' @return data.frame. Training data with features for ML model
#'
prepare_ml_training_data <- function(data) {
  
  cat("🤖 Preparing ML training data from existing mappings...\n")
  
  # Filter for high-confidence mappings to use as training data
  training_data <- data %>%
    filter(
      !is.na(icd11_code) & icd11_code != "",
      !is.na(icd11_title) & icd11_title != "",
      confidence %in% c("high", "medium")
    ) %>%
    mutate(
      # Convert to character and text preprocessing features
      morbidity = as.character(morbidity),
      icd11_title = as.character(icd11_title),
      icd11_code = as.character(icd11_code),
      morbidity_clean = tolower(gsub("[^a-zA-Z0-9\\s]", " ", morbidity)),
      morbidity_clean = gsub("\\s+", " ", trimws(morbidity_clean)),
      morbidity_length = nchar(morbidity),
      word_count = lengths(strsplit(morbidity_clean, " ")),
      
      # ICD-11 target features  
      icd11_title_clean = tolower(gsub("[^a-zA-Z0-9\\s]", " ", icd11_title)),
      icd11_title_clean = gsub("\\s+", " ", trimws(icd11_title_clean)),
      
      # Extract features for similarity matching
      first_word = sapply(strsplit(morbidity_clean, " "), function(x) if(length(x) > 0) x[1] else ""),
      last_word = sapply(strsplit(morbidity_clean, " "), function(x) if(length(x) > 0) x[length(x)] else ""),
      
      # ICD-11 chapter from code (first character/digit pattern)
      icd11_chapter = substr(icd11_code, 1, 2),
      
      # Confidence as numeric for weighting
      confidence_weight = case_when(
        confidence == "high" ~ 1.0,
        confidence == "medium" ~ 0.7,
        TRUE ~ 0.3
      )
    ) %>%
    select(
      morbidity, morbidity_clean, morbidity_length, word_count,
      first_word, last_word, icd11_code, icd11_title, icd11_title_clean,
      icd11_category, icd11_chapter, confidence, confidence_weight
    )
  
  cat("✅ Training data prepared:", nrow(training_data), "high-confidence mappings\n")
  return(training_data)
}

#' Apply machine learning to suggest ICD-11 mappings for unmapped diseases
#'
#' @param data data.frame. Full dataset
#' @param min_cases numeric. Minimum cases to consider for ML mapping
#' @param similarity_threshold numeric. Minimum similarity score (0-1)
#' @return data.frame. ML suggestions for unmapped diseases
#'
apply_ml_disease_mapping <- function(data, min_cases = 10, similarity_threshold = 0.3) {
  
  cat("🤖 Applying ML-based disease mapping...\n")
  
  # Prepare training data
  training_data <- prepare_ml_training_data(data)
  
  if (nrow(training_data) < 10) {
    cat("⚠️  Insufficient training data (<10 mappings). Cannot proceed with ML.\n")
    return(data.frame())
  }
  
  # Create a focused training set by removing duplicates and keeping unique disease patterns
  unique_training <- training_data %>%
    group_by(morbidity_clean) %>%
    slice_head(n = 1) %>%  # Keep only first occurrence of each unique disease
    ungroup()
  
  cat("📚 Unique training patterns:", nrow(unique_training), "from", nrow(training_data), "total mappings\n")
  
  # Identify diseases needing mapping improvements (unmapped or incomplete)
  unmapped_diseases <- data %>%
    mutate(
      # Convert to character first
      morbidity = as.character(morbidity),
      icd11_code = as.character(icd11_code),
      icd11_title = as.character(icd11_title),
      confidence = as.character(confidence),
      has_code = !is.na(icd11_code) & icd11_code != "",
      has_title = !is.na(icd11_title) & icd11_title != "",
      has_category = if("icd11_category" %in% names(.)) !is.na(icd11_category) & icd11_category != "" else FALSE,
      confidence_level = case_when(
        is.na(confidence) | confidence == "" ~ "none",
        confidence == "low" ~ "low",
        confidence == "medium" ~ "medium",
        confidence == "high" ~ "high",
        TRUE ~ "unknown"
      ),
      needs_improvement = !has_code | !has_title | confidence_level %in% c("none", "low")
    ) %>%
    filter(needs_improvement) %>%
    group_by(morbidity) %>%
    summarise(
      total_cases = n(),
      .groups = "drop"
    ) %>%
    filter(total_cases >= min_cases) %>%
    mutate(
      morbidity_clean = tolower(gsub("[^a-zA-Z0-9\\s]", " ", morbidity)),
      morbidity_clean = gsub("\\s+", " ", trimws(morbidity_clean)),
      morbidity_length = nchar(morbidity),
      word_count = lengths(strsplit(morbidity_clean, " ")),
      first_word = sapply(strsplit(morbidity_clean, " "), function(x) if(length(x) > 0) x[1] else ""),
      last_word = sapply(strsplit(morbidity_clean, " "), function(x) if(length(x) > 0) x[length(x)] else "")
    )
  
  cat("📊 Found", nrow(unmapped_diseases), "diseases needing improvement with >=", min_cases, "cases\n")
  
  # Debug: show which diseases need improvement
  if (nrow(unmapped_diseases) > 0) {
    cat("Diseases needing improvement:\n")
    for (j in seq_len(min(5, nrow(unmapped_diseases)))) {
      cat("  ", j, ":", unmapped_diseases$morbidity[j], "(", unmapped_diseases$total_cases[j], "cases )\n")
    }
  }
  
  if (nrow(unmapped_diseases) == 0) {
    cat("✅ No diseases found needing improvement with minimum case threshold\n")
    return(data.frame())
  }
  
  # Generate ML suggestions using string similarity and pattern matching
  ml_suggestions <- list()
  
  for (i in seq_len(nrow(unmapped_diseases))) {
    disease <- unmapped_diseases[i, ]
    
    cat("Processing disease", i, "of", nrow(unmapped_diseases), ":", disease$morbidity, "\n")
    
    # Calculate similarities with unique training patterns (faster and more focused)
    similarities <- unique_training %>%
      mutate(
        # Enhanced similarity metrics for medical terminology
        
        # 1. Word overlap similarity (key medical terms)
        jaccard_sim = sapply(seq_len(nrow(.)), function(j) {
          words1 <- unlist(strsplit(disease$morbidity_clean, " "))
          words2 <- unlist(strsplit(.$morbidity_clean[j], " "))
          intersection <- length(intersect(words1, words2))
          union <- length(union(words1, words2))
          if (union == 0) 0 else intersection / union
        }),
        
        # 2. Substring containment (partial matches)
        substring_sim = sapply(morbidity_clean, function(x) {
          # Check if key words from one disease appear in the other
          words1 <- unlist(strsplit(disease$morbidity_clean, " "))
          words2 <- unlist(strsplit(x, " "))
          
          # Remove common words that don't help with matching
          common_words <- c("acute", "chronic", "suspected", "unspecified", "other", "nos")
          words1 <- words1[!words1 %in% common_words]
          words2 <- words2[!words2 %in% common_words]
          
          if (length(words1) == 0 || length(words2) == 0) return(0)
          
          # Check if any significant words match
          max_match <- 0
          for (w1 in words1) {
            for (w2 in words2) {
              if (nchar(w1) >= 4 && nchar(w2) >= 4) {  # Only meaningful words
                sim <- 1 - stringdist::stringdist(w1, w2, method = "jw")
                max_match <- max(max_match, sim)
              }
            }
          }
          max_match
        }),
        
        # 3. Medical pattern matching (common disease patterns)
        pattern_sim = sapply(morbidity_clean, function(x) {
          # Check for common medical patterns
          patterns <- c("infection", "bronchitis", "pneumonia", "diarrhea", "fever", 
                       "respiratory", "gastrointestinal", "malaria", "tuberculosis")
          
          disease_patterns <- sapply(patterns, function(p) grepl(p, disease$morbidity_clean))
          training_patterns <- sapply(patterns, function(p) grepl(p, x))
          
          if (sum(disease_patterns) == 0 && sum(training_patterns) == 0) return(0)
          
          # Jaccard similarity of pattern matches
          intersection <- sum(disease_patterns & training_patterns)
          union <- sum(disease_patterns | training_patterns)
          
          if (union == 0) 0 else intersection / union
        }),
        
        # 4. String distance similarity
        string_sim = sapply(morbidity_clean, function(x) {
          tryCatch(1 - stringdist::stringdist(disease$morbidity_clean, x, method = "jw"),
                   error = function(e) 0)
        }),
        
        # 5. Enhanced combined similarity with better weighting
        combined_sim = (0.25 * jaccard_sim + 0.25 * substring_sim + 0.25 * pattern_sim + 0.25 * string_sim) * confidence_weight
      ) %>%
      arrange(desc(combined_sim))
    
    # Debug: show top similarities before filtering
    if (nrow(similarities) >= 1) {
      cat("  Top similarity:", round(similarities$combined_sim[1], 3), "vs threshold:", similarity_threshold, "\n")
    }
    
    similarities <- similarities %>%
      filter(combined_sim >= similarity_threshold) %>%
      slice_head(n = 3)  # Top 3 matches
    
    if (nrow(similarities) > 0) {
      # Create suggestion with confidence level based on similarity
      best_match <- similarities[1, ]
      
      ml_confidence <- case_when(
        best_match$combined_sim >= 0.8 ~ "medium",
        best_match$combined_sim >= 0.6 ~ "low",
        TRUE ~ "very_low"
      )
      
      suggestion <- data.frame(
        morbidity = disease$morbidity,
        total_cases = disease$total_cases,
        ml_suggested_code = best_match$icd11_code,
        ml_suggested_title = best_match$icd11_title,
        ml_suggested_category = best_match$icd11_category,
        ml_confidence = ml_confidence,
        similarity_score = round(best_match$combined_sim, 3),
        matched_with = best_match$morbidity,
        matched_confidence = best_match$confidence,
        alternative_1_code = if(nrow(similarities) > 1) similarities$icd11_code[2] else NA,
        alternative_1_title = if(nrow(similarities) > 1) similarities$icd11_title[2] else NA,
        alternative_2_code = if(nrow(similarities) > 2) similarities$icd11_code[3] else NA,
        alternative_2_title = if(nrow(similarities) > 2) similarities$icd11_title[3] else NA,
        stringsAsFactors = FALSE
      )
      
      ml_suggestions[[i]] <- suggestion
    }
  }
  
  if (length(ml_suggestions) > 0) {
    ml_results <- do.call(rbind, ml_suggestions) %>%
      arrange(desc(total_cases), desc(similarity_score))
  } else {
    ml_results <- data.frame()
  }
  
  cat("🎯 ML generated", nrow(ml_results), "mapping suggestions\n")
  return(ml_results)
}

#' Export ML mapping suggestions to Excel for manual review
#'
#' @param ml_suggestions data.frame. Output from apply_ml_disease_mapping
#' @param output_file character. Output Excel filename
#' @return list. Summary of export process
#'
export_ml_suggestions <- function(ml_suggestions, output_file = "ml_disease_mapping_suggestions.xlsx") {
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("openxlsx package required. Install with: install.packages('openxlsx')")
  }
  
  cat("📄 Exporting ML suggestions for manual review...\n")
  
  if (nrow(ml_suggestions) == 0) {
    cat("⚠️  No ML suggestions to export\n")
    return(list(suggestions_exported = 0))
  }
  
  wb <- openxlsx::createWorkbook()
  
  # Main suggestions sheet
  openxlsx::addWorksheet(wb, "ML_Suggestions")
  
  # Prepare export format with review columns
  export_suggestions <- ml_suggestions %>%
    mutate(
      review_status = "",           # For manual review: APPROVE/REJECT/MODIFY
      final_icd11_code = "",        # For manual entry if modifying
      final_icd11_title = "",       # For manual entry if modifying  
      final_icd11_category = "",    # For manual entry if modifying
      final_confidence = "",        # For manual entry: high/medium/low
      reviewer_notes = "",          # For reviewer comments
      review_date = ""             # For tracking review completion
    ) %>%
    select(
      morbidity, total_cases, ml_suggested_code, ml_suggested_title, 
      ml_suggested_category, ml_confidence, similarity_score,
      matched_with, matched_confidence, alternative_1_code, alternative_1_title,
      alternative_2_code, alternative_2_title, review_status,
      final_icd11_code, final_icd11_title, final_icd11_category,
      final_confidence, reviewer_notes, review_date
    )
  
  openxlsx::writeData(wb, "ML_Suggestions", export_suggestions)
  
  # Format headers
  header_style <- openxlsx::createStyle(fgFill = "#4472C4", fontColour = "white", textDecoration = "bold")
  openxlsx::addStyle(wb, "ML_Suggestions", header_style, rows = 1, cols = seq_len(ncol(export_suggestions)), gridExpand = TRUE)
  
  # Color code by confidence
  high_conf_style <- openxlsx::createStyle(fgFill = "#E2EFDA")
  med_conf_style <- openxlsx::createStyle(fgFill = "#FFF2CC") 
  low_conf_style <- openxlsx::createStyle(fgFill = "#FCE4D6")
  
  for (i in seq_len(nrow(export_suggestions))) {
    row_num <- i + 1
    conf <- export_suggestions$ml_confidence[i]
    
    if (conf == "medium") {
      openxlsx::addStyle(wb, "ML_Suggestions", high_conf_style, rows = row_num, cols = seq_len(ncol(export_suggestions)), gridExpand = TRUE)
    } else if (conf == "low") {
      openxlsx::addStyle(wb, "ML_Suggestions", med_conf_style, rows = row_num, cols = seq_len(ncol(export_suggestions)), gridExpand = TRUE)
    } else {
      openxlsx::addStyle(wb, "ML_Suggestions", low_conf_style, rows = row_num, cols = seq_len(ncol(export_suggestions)), gridExpand = TRUE)
    }
  }
  
  # Set column widths
  openxlsx::setColWidths(wb, "ML_Suggestions", cols = 1:20, widths = c(25, 8, 12, 30, 25, 12, 10, 25, 12, 12, 30, 12, 30, 12, 15, 30, 25, 12, 30, 12))
  
  # Instructions sheet
  openxlsx::addWorksheet(wb, "Review_Instructions")
  
  instructions <- c(
    "MACHINE LEARNING DISEASE MAPPING - MANUAL REVIEW INSTRUCTIONS",
    "",
    "This workbook contains AI-generated suggestions for mapping unmapped diseases to ICD-11 codes.",
    "Each suggestion is based on similarity analysis with existing high-confidence mappings.",
    "",
    "REVIEW PROCESS:",
    "1. Review each suggested mapping in the 'ML_Suggestions' sheet",
    "2. Check the similarity score and confidence level",
    "3. Compare with alternative suggestions provided",
    "4. Complete the review columns as follows:",
    "",
    "REVIEW_STATUS OPTIONS:",
    "• APPROVE: Accept the ML suggestion as-is",
    "• REJECT: Reject the suggestion entirely", 
    "• MODIFY: Accept but modify the ICD-11 code/title/category",
    "",
    "FOR APPROVED MAPPINGS:",
    "• Set review_status = 'APPROVE'", 
    "• Leave final_* columns empty (will use ML suggestions)",
    "• Set final_confidence = 'high', 'medium', or 'low'",
    "• Add any notes in reviewer_notes",
    "",
    "FOR MODIFIED MAPPINGS:",
    "• Set review_status = 'MODIFY'",
    "• Fill in final_icd11_code, final_icd11_title, final_icd11_category", 
    "• Set final_confidence = 'high', 'medium', or 'low'",
    "• Explain changes in reviewer_notes",
    "",
    "FOR REJECTED MAPPINGS:",
    "• Set review_status = 'REJECT'",
    "• Leave final_* columns empty",
    "• Explain reason in reviewer_notes",
    "",
    "CONFIDENCE LEVEL GUIDELINES:",
    "• HIGH: Exact or near-exact match, clinically appropriate",
    "• MEDIUM: Good match with minor uncertainty", 
    "• LOW: Broad category match, significant uncertainty",
    "",
    "SIMILARITY SCORE INTERPRETATION:",
    "• 0.80-1.00: Very high similarity (usually safe to approve)",
    "• 0.60-0.79: Medium similarity (review carefully)",
    "• 0.40-0.59: Lower similarity (likely needs modification)",
    "",
    "QUALITY CHECKS:",
    "✓ Verify clinical appropriateness of suggested mappings",
    "✓ Check alternative suggestions before final decision", 
    "✓ Consider disease severity and specificity requirements",
    "✓ Ensure consistency with existing mapping patterns",
    "✓ Document rationale for complex decisions",
    "",
    "COMPLETING REVIEW:",
    "1. Fill review_date when completing each row",
    "2. Save this file when review is complete", 
    "3. Use apply_reviewed_ml_mappings() function to apply approved changes",
    "",
    "SUPPORT: Contact technical team for complex mapping decisions or ICD-11 verification"
  )
  
  openxlsx::writeData(wb, "Review_Instructions", data.frame(Instructions = instructions))
  openxlsx::setColWidths(wb, "Review_Instructions", cols = 1, widths = 120)
  
  # Summary sheet
  openxlsx::addWorksheet(wb, "Summary")
  
  summary_stats <- ml_suggestions %>%
    summarise(
      total_suggestions = n(),
      high_cases = sum(total_cases >= 100),
      medium_confidence = sum(ml_confidence == "medium"),
      low_confidence = sum(ml_confidence == "low"),
      very_low_confidence = sum(ml_confidence == "very_low"),
      avg_similarity = round(mean(similarity_score), 3),
      total_cases_covered = sum(total_cases)
    )
  
  summary_data <- data.frame(
    Metric = c(
      "Total ML Suggestions",
      "High-Impact Diseases (>=100 cases)", 
      "Medium Confidence Suggestions",
      "Low Confidence Suggestions",
      "Very Low Confidence Suggestions",
      "Average Similarity Score",
      "Total Cases Covered by Suggestions"
    ),
    Value = c(
      summary_stats$total_suggestions,
      summary_stats$high_cases,
      summary_stats$medium_confidence, 
      summary_stats$low_confidence,
      summary_stats$very_low_confidence,
      summary_stats$avg_similarity,
      format(summary_stats$total_cases_covered, big.mark = ",")
    )
  )
  
  openxlsx::writeData(wb, "Summary", summary_data)
  openxlsx::addStyle(wb, "Summary", header_style, rows = 1, cols = 1:2, gridExpand = TRUE)
  openxlsx::setColWidths(wb, "Summary", cols = 1:2, widths = c(40, 20))
  
  # Save workbook
  output_path <- here::here(output_file)
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  
  cat("✅ ML suggestions exported:", output_path, "\n")
  cat("📊 SUMMARY:\n")
  cat("   Total suggestions:", summary_stats$total_suggestions, "\n")
  cat("   Medium confidence:", summary_stats$medium_confidence, "\n") 
  cat("   Low confidence:", summary_stats$low_confidence, "\n")
  cat("   Average similarity:", summary_stats$avg_similarity, "\n")
  cat("   Cases covered:", format(summary_stats$total_cases_covered, big.mark = ","), "\n")
  
  return(list(
    suggestions_exported = summary_stats$total_suggestions,
    output_file = output_path,
    summary_stats = summary_stats
  ))
}

#' Apply reviewed ML mappings back to the dataset
#'
#' @param reviewed_file character. Path to reviewed Excel file
#' @param data data.frame. Original dataset to update
#' @return list. Updated dataset and application summary
#'
apply_reviewed_ml_mappings <- function(reviewed_file, data) {
  
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("readxl package required. Install with: install.packages('readxl')")
  }
  
  cat("📥 Applying reviewed ML mappings...\n")
  
  # Read reviewed suggestions
  reviewed_suggestions <- readxl::read_excel(reviewed_file, sheet = "ML_Suggestions")
  
  # Filter for approved and modified mappings
  approved_mappings <- reviewed_suggestions %>%
    filter(review_status %in% c("APPROVE", "MODIFY")) %>%
    mutate(
      # Use final_* columns if modified, otherwise use ml_suggested_*
      apply_code = ifelse(review_status == "MODIFY" & !is.na(final_icd11_code) & final_icd11_code != "",
                         final_icd11_code, ml_suggested_code),
      apply_title = ifelse(review_status == "MODIFY" & !is.na(final_icd11_title) & final_icd11_title != "",
                          final_icd11_title, ml_suggested_title),
      apply_category = ifelse(review_status == "MODIFY" & !is.na(final_icd11_category) & final_icd11_category != "",
                             final_icd11_category, ml_suggested_category),
      apply_confidence = ifelse(!is.na(final_confidence) & final_confidence != "",
                               final_confidence, ml_confidence)
    ) %>%
    select(morbidity, apply_code, apply_title, apply_category, apply_confidence, review_status)
  
  cat("📊 Found", nrow(approved_mappings), "approved mappings to apply\n")
  
  if (nrow(approved_mappings) == 0) {
    cat("⚠️  No approved mappings found in review file\n")
    return(list(updated_data = data, mappings_applied = 0))
  }
  
  # Apply mappings to dataset
  updated_data <- data
  mappings_applied <- 0
  
  for (i in seq_len(nrow(approved_mappings))) {
    mapping <- approved_mappings[i, ]
    
    # Find matching records in dataset that need improvement
    matching_rows <- which(updated_data$morbidity == mapping$morbidity & 
                          (is.na(updated_data$icd11_code) | updated_data$icd11_code == "" |
                           is.na(updated_data$icd11_title) | updated_data$icd11_title == "" |
                           is.na(updated_data$confidence) | updated_data$confidence %in% c("", "low", "none")))
    
    if (length(matching_rows) > 0) {
      # Apply the mapping
      updated_data$icd11_code[matching_rows] <- mapping$apply_code
      updated_data$icd11_title[matching_rows] <- mapping$apply_title
      if ("icd11_category" %in% names(updated_data)) {
        updated_data$icd11_category[matching_rows] <- mapping$apply_category
      }
      updated_data$confidence[matching_rows] <- mapping$apply_confidence
      
      mappings_applied <- mappings_applied + length(matching_rows)
      cat("✅ Applied", mapping$apply_code, "to", length(matching_rows), "records of", mapping$morbidity, "\n")
    }
  }
  
  # Generate application summary
  summary_stats <- list(
    total_reviewed = nrow(approved_mappings),
    records_updated = mappings_applied,
    approved_count = sum(approved_mappings$review_status == "APPROVE"),
    modified_count = sum(approved_mappings$review_status == "MODIFY")
  )
  
  cat("🎯 ML MAPPING APPLICATION COMPLETE:\n")
  cat("   Mappings reviewed and approved:", summary_stats$total_reviewed, "\n")
  cat("   Records updated:", summary_stats$records_updated, "\n") 
  cat("   Approved as-is:", summary_stats$approved_count, "\n")
  cat("   Modified by reviewer:", summary_stats$modified_count, "\n")
  
  return(list(
    updated_data = updated_data,
    mappings_applied = mappings_applied,
    summary_stats = summary_stats
  ))
}

# Export functions
log_info("Validation rules module loaded successfully")
log_info("Available functions: validate_register_data, validate_epi_data, quick_quality_check, generate_quality_report, export_unmapped_diseases, apply_ml_disease_mapping, export_ml_suggestions, apply_reviewed_ml_mappings")