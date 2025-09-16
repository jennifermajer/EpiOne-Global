# R/geographic_helpers.R
# Helper functions for enhanced geographic analysis

#' Create enhanced geographic summary statistics
#'
#' @param data Data frame with health consultation data
#' @return Data frame with comprehensive geographic metrics
create_geographic_summary <- function(data) {
  
  # Check required columns
  required_cols <- c("admin1")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Get epidemic diseases if function exists
  epidemic_conditions <- if (exists("get_epidemic_diseases_standardized")) {
    unlist(get_epidemic_diseases_standardized(), use.names = FALSE)
  } else {
    c("Acute Watery Diarrhoea (Suspected Cholera)", "Cholera (Confirmed)", "Measles", 
      "Pneumonia", "Severe Acute Respiratory Infection", "Influenza-Like Illness",
      "COVID-19", "Malaria (Suspected)", "Dengue")
  }
  
  # Get malnutrition conditions
  malnut_conditions <- c("Complicated severe acute Malnutrition", "Moderate acute Malnutrition", 
                        "Severe Malnutrition", "Uncomplicated severe acute Malnutrition", "Underweight")
  
  # Create comprehensive summary
  summary_data <- data %>%
    group_by(admin1) %>%
    summarise(
      # Basic metrics
      total_consultations = n(),
      unique_facilities = if ("orgunit" %in% colnames(data)) n_distinct(orgunit, na.rm = TRUE) else 0,
      consultations_per_facility = if ("orgunit" %in% colnames(data)) {
        round(n() / n_distinct(orgunit, na.rm = TRUE), 1)
      } else 0,
      
      # Disease diversity
      disease_diversity = if ("morbidity_category" %in% colnames(data)) {
        n_distinct(morbidity_category, na.rm = TRUE)
      } else if ("morbidity" %in% colnames(data)) {
        n_distinct(morbidity, na.rm = TRUE)
      } else 0,
      
      # Top disease category
      top_disease_category = if ("morbidity_category" %in% colnames(data)) {
        names(sort(table(morbidity_category), decreasing = TRUE))[1]
      } else if ("morbidity" %in% colnames(data)) {
        names(sort(table(morbidity), decreasing = TRUE))[1]
      } else "Unknown",
      
      # Trauma analysis
      trauma_cases = if ("type_case" %in% colnames(data)) {
        sum(type_case == "Trauma", na.rm = TRUE)
      } else 0,
      trauma_percentage = if ("type_case" %in% colnames(data)) {
        round(sum(type_case == "Trauma", na.rm = TRUE) / n() * 100, 1)
      } else 0,
      
      # Epidemic cases
      epidemic_cases = if ("morbidity" %in% colnames(data)) {
        sum(morbidity %in% epidemic_conditions, na.rm = TRUE)
      } else 0,
      epidemic_percentage = if ("morbidity" %in% colnames(data)) {
        round(sum(morbidity %in% epidemic_conditions, na.rm = TRUE) / n() * 100, 1)
      } else 0,
      
      # Malnutrition cases
      malnutrition_cases = if ("morbidity" %in% colnames(data)) {
        sum(morbidity %in% malnut_conditions, na.rm = TRUE)
      } else 0,
      malnutrition_percentage = if ("morbidity" %in% colnames(data)) {
        round(sum(morbidity %in% malnut_conditions, na.rm = TRUE) / n() * 100, 1)
      } else 0,
      
      # Demographics
      female_percentage = if ("sex" %in% colnames(data)) {
        round(sum(sex %in% c("Female", "F", "female"), na.rm = TRUE) / n() * 100, 1)
      } else 0,
      
      pediatric_percentage = if ("age_group_new" %in% colnames(data)) {
        round(sum(age_group_new %in% c("0-5 y", "<5", "0-4"), na.rm = TRUE) / n() * 100, 1)
      } else if ("age_group" %in% colnames(data)) {
        round(sum(age_group %in% c("0-5 y", "<5", "0-4"), na.rm = TRUE) / n() * 100, 1)
      } else 0,
      
      # Facility types (if available)
      facility_types = if ("facilitytype" %in% colnames(data)) {
        n_distinct(facilitytype, na.rm = TRUE)
      } else 0,
      
      # Date range (if available)
      date_range_days = if ("datevisit" %in% colnames(data)) {
        as.numeric(max(datevisit, na.rm = TRUE) - min(datevisit, na.rm = TRUE))
      } else if ("datevisitnew" %in% colnames(data)) {
        as.numeric(max(datevisitnew, na.rm = TRUE) - min(datevisitnew, na.rm = TRUE))
      } else 0,
      
      .groups = "drop"
    ) %>%
    arrange(desc(total_consultations))
  
  return(summary_data)
}

#' Create risk profile analysis by region
#'
#' @param data Data frame with health consultation data
#' @return Data frame with risk categories by region
create_risk_profile_analysis <- function(data) {
  if (!"admin1" %in% colnames(data) || nrow(data) == 0) {
    return(tibble(admin1 = character(), risk_category = character(), 
                  cases = integer(), percentage = numeric()))
  }

  n_rows <- nrow(data)

  is_high_risk_age <- rep(FALSE, n_rows)
  if ("age_group_new" %in% colnames(data)) {
    is_high_risk_age <- is_high_risk_age | data$age_group_new %in% c("0-5 y", "60+ y")
  }
  if ("age_group" %in% colnames(data)) {
    is_high_risk_age <- is_high_risk_age | data$age_group %in% c("0-5 y", "60+ y")
  }

  is_displaced <- if ("resident" %in% colnames(data)) {
    data$resident %in% c("IDP", "Displaced", "IDP/Returnee")
  } else rep(FALSE, n_rows)

  is_trauma <- if ("type_case" %in% colnames(data)) {
    vals <- tolower(as.character(data$type_case))
    stringr::str_detect(vals, "trauma") & !stringr::str_detect(vals, "non")
  } else rep(FALSE, n_rows)

  chronic_conditions <- c(
    "Diabetes mellitus", "Hypertension", "Chronic obstructive pulmonary disease (COPD)",
    "Stroke/cerebrovascular accident", "Asthma", "Heart failure", "Cancer", 
    "Chronic renal Failure"
  )
  is_chronic <- if ("morbidity" %in% colnames(data)) {
    data$morbidity %in% chronic_conditions
  } else rep(FALSE, n_rows)

  epidemic_conditions <- c(
    "Acute Watery Diarrhea", "Suspected Cholera", "Suspected Measles", 
    "Pneumonia", "SARI"
  )
  is_epidemic <- if ("morbidity" %in% colnames(data)) {
    data$morbidity %in% epidemic_conditions
  } else rep(FALSE, n_rows)

  risk_factors <- data %>%
    mutate(
      risk_category = case_when(
        is_high_risk_age ~ "High Risk Age",
        is_displaced ~ "Displaced Population",
        is_trauma ~ "Trauma Risk",
        is_chronic ~ "Chronic Disease Risk",
        is_epidemic ~ "Epidemic Risk",
        TRUE ~ "Standard Risk"
      )
    ) %>%
    filter(!is.na(admin1)) %>%
    count(admin1, risk_category, name = "cases") %>%
    group_by(admin1) %>%
    mutate(percentage = round(cases / sum(cases) * 100, 1)) %>%
    ungroup()

  return(risk_factors)
}

#' Create disease burden matrix analysis
#'
#' @param data Data frame with health consultation data
#' @param top_n Number of top diseases to include
#' @return Data frame with disease burden metrics
create_disease_burden_matrix <- function(data, top_n = 20) {
  
  if (!"morbidity" %in% colnames(data)) {
    return(tibble(morbidity = character(), cases = integer(), 
                  severity = character(), frequency_rank = integer(), 
                  burden_score = numeric()))
  }
  
  # Disease severity mapping
  disease_severity_mapping <- list(
    "High Severity" = c(
      "Death", "Acute myocardial infarction", "Stroke/cerebrovascular accident", 
      "Complicated severe acute Malnutrition", "Suspected Meningitis", "Meningitis",
      "Acute Flaccid Paralysis", "Cancer", "Chronic renal Failure"
    ),
    "Medium Severity" = c(
      "Pneumonia", "Acute Watery Diarrhea", "Suspected Measles", "Measles",
      "Diabetes mellitus", "Hypertension", "SARI", "Heart failure",
      "Severe Malnutrition", "Uncomplicated severe acute Malnutrition"
    ),
    "Low Severity" = c(
      "URTI", "Acute Upper respiratory infection", "Headaches/Migraine", 
      "Dermatitis", "Conjunctivitis", "Moderate acute Malnutrition"
    )
  )
  
  # Calculate disease burden
  disease_burden <- data %>%
    filter(!is.na(morbidity)) %>%
    count(morbidity, sort = TRUE) %>%
    slice_head(n = top_n) %>%
    mutate(
      severity = case_when(
        morbidity %in% disease_severity_mapping[["High Severity"]] ~ "High",
        morbidity %in% disease_severity_mapping[["Medium Severity"]] ~ "Medium",
        TRUE ~ "Low"
      ),
      frequency_rank = rank(-n),
      burden_score = case_when(
        severity == "High" ~ n * 3,
        severity == "Medium" ~ n * 2,
        TRUE ~ n * 1
      ),
      cases = n
    ) %>%
    arrange(desc(burden_score))
  
  return(disease_burden)
}

#' Create facility utilization analysis
#'
#' @param data Data frame with health consultation data
#' @return Data frame with facility utilization metrics
create_facility_utilization_analysis <- function(data) {
  
  if (!all(c("admin1", "orgunit") %in% colnames(data))) {
    return(tibble(admin1 = character(), avg_consultations = numeric(), 
                  total_facilities = integer(), utilization_category = character()))
  }
  
  # Determine date column
  date_col <- if ("datevisit" %in% colnames(data)) "datevisit" else 
              if ("datevisitnew" %in% colnames(data)) "datevisitnew" else NULL
  
  if (!is.null(date_col)) {
    # Calculate daily averages
    facility_utilization <- data %>%
      group_by(admin1, orgunit) %>%
      summarise(
        total_consultations = n(),
        unique_days = n_distinct(.data[[date_col]], na.rm = TRUE),
        daily_average = total_consultations / pmax(unique_days, 1),
        .groups = "drop"
      ) %>%
      group_by(admin1) %>%
      summarise(
        avg_consultations = round(mean(daily_average), 1),
        total_facilities = n_distinct(orgunit),
        utilization_category = case_when(
          mean(daily_average) >= 50 ~ "High Utilization",
          mean(daily_average) >= 20 ~ "Medium Utilization",
          TRUE ~ "Low Utilization"
        ),
        .groups = "drop"
      )
  } else {
    # Fallback without date information
    facility_utilization <- data %>%
      group_by(admin1, orgunit) %>%
      summarise(consultations = n(), .groups = "drop") %>%
      group_by(admin1) %>%
      summarise(
        avg_consultations = round(mean(consultations), 1),
        total_facilities = n_distinct(orgunit),
        utilization_category = case_when(
          mean(consultations) >= 200 ~ "High Utilization",
          mean(consultations) >= 100 ~ "Medium Utilization",
          TRUE ~ "Low Utilization"
        ),
        .groups = "drop"
      )
  }
  
  return(facility_utilization)
}

#' Create accessibility trends analysis
#'
#' @param data Data frame with health consultation data
#' @param top_regions Number of top regions to include
#' @return Data frame with accessibility trends over time
create_accessibility_trends <- function(data, top_regions = 5) {
  
  if (!all(c("admin1", "orgunit") %in% colnames(data))) {
    return(tibble(month = as.Date(character()), admin1 = character(), 
                  consultations_per_facility = numeric()))
  }
  
  # Determine date column
  date_col <- if ("datevisit" %in% colnames(data)) "datevisit" else 
              if ("datevisitnew" %in% colnames(data)) "datevisitnew" else NULL
  
  if (is.null(date_col)) {
    return(tibble(month = as.Date(character()), admin1 = character(), 
                  consultations_per_facility = numeric()))
  }
  
  # Calculate trends over time
  accessibility_data <- data %>%
    filter(!is.na(.data[[date_col]])) %>%
    mutate(month = lubridate::floor_date(.data[[date_col]], "month")) %>%
    group_by(month, admin1) %>%
    summarise(
      consultations = n(),
      facilities = n_distinct(orgunit, na.rm = TRUE),
      consultations_per_facility = consultations / pmax(facilities, 1),
      .groups = "drop"
    ) %>%
    filter(!is.na(month))
  
  # Get top regions by total consultations
  top_admin1 <- accessibility_data %>%
    group_by(admin1) %>%
    summarise(total_consultations = sum(consultations), .groups = "drop") %>%
    slice_max(total_consultations, n = top_regions) %>%
    pull(admin1)
  
  # Filter to top regions
  result <- accessibility_data %>%
    filter(admin1 %in% top_admin1) %>%
    arrange(month, admin1)
  
  return(result)
}

#' Format geographic summary for display
#'
#' @param summary_data Data frame from create_geographic_summary
#' @return Formatted data frame for display
format_geographic_summary_display <- function(summary_data) {
  
  if (nrow(summary_data) == 0) {
    return(data.frame(Message = "No geographic data available"))
  }
  
  display_data <- summary_data %>%
    select(
      `Governorate` = admin1,
      `Total Consultations` = total_consultations,
      `Health Facilities` = unique_facilities,
      `Consultations per Facility` = consultations_per_facility,
      `Top Disease Category` = top_disease_category,
      `Trauma Cases` = trauma_cases,
      `Trauma Percentage` = trauma_percentage,
      `Female %` = female_percentage,
      `Pediatric %` = pediatric_percentage
    ) %>%
    mutate(
      `Total Consultations` = scales::comma(`Total Consultations`),
      `Trauma Percentage` = paste0(`Trauma Percentage`, "%"),
      `Female %` = paste0(`Female %`, "%"),
      `Pediatric %` = paste0(`Pediatric %`, "%")
    )
  
  return(display_data)
}

#' Create trauma analysis by region
#'
#' @param data Data frame with health consultation data
#' @return Data frame with trauma analysis
create_trauma_analysis <- function(data) {
  
  if (!all(c("admin1", "type_case") %in% colnames(data))) {
    return(tibble(admin1 = character(), type_case_clean = character(), 
                  cases = integer(), percentage = numeric()))
  }
  
  trauma_summary <- data %>%
    filter(!is.na(type_case), !is.na(admin1)) %>%
    mutate(
      type_case_clean = case_when(
        str_detect(tolower(type_case), "trauma") & !str_detect(tolower(type_case), "non") ~ "Trauma",
        str_detect(tolower(type_case), "non.trauma|non trauma") ~ "Non-trauma",
        type_case == "Trauma" ~ "Trauma",
        type_case == "Non-trauma" ~ "Non-trauma",
        TRUE ~ as.character(type_case)
      )
    ) %>%
    filter(type_case_clean %in% c("Trauma", "Non-trauma")) %>%
    group_by(admin1, type_case_clean) %>%
    summarise(cases = n(), .groups = "drop") %>%
    group_by(admin1) %>%
    mutate(
      percentage = round(cases / sum(cases) * 100, 1),
      total_cases = sum(cases)
    ) %>%
    ungroup()
  
  return(trauma_summary)
}
