# R/visualization_helpers.R
# Complete visualization helper functions for epidemiological report

# Load required libraries
suppressPackageStartupMessages({
  library(ggplot2)
  library(ggforce)
  library(dplyr)
  library(scales)
  library(viridis)
  library(RColorBrewer)
  library(logger)
  library(lubridate)
  library(tidyr)
  library(stringr)
  library(flextable)
  library(tibble)
})

# Define AI-compatible color scheme
ai_colors <- list(
  primary = "#1f77b4",
  secondary = "#ff7f0e", 
  success = "#2ca02c",
  danger = "#d62728",
  warning = "#ff6f00",
  info = "#17becf",
  muted = "#666666"
)

#' Detect if data is line list or aggregate format and aggregate appropriately
#' @param data Data frame to analyze
#' @param group_vars Character vector of grouping variables
#' @param weight_var Character name of weight/value variable (for aggregate data)
#' @return Data frame with counts/sums by group variables
smart_aggregate <- function(data, group_vars, weight_var = "value") {

  # Remove any group_vars that don't exist in the data
  existing_vars <- group_vars[group_vars %in% names(data)]

  if (length(existing_vars) == 0) {
    stop("None of the specified grouping variables exist in the data")
  }

  # Auto-detect the weight variable if not specified or not found
  potential_weight_cols <- c("value", "cases", "count", "total", "n", "freq")

  if (!weight_var %in% names(data)) {
    # Try to find an appropriate weight column
    found_weight_col <- NULL
    for (col in potential_weight_cols) {
      if (col %in% names(data)) {
        found_weight_col <- col
        break
      }
    }

    if (!is.null(found_weight_col)) {
      weight_var <- found_weight_col
      cat("📊 Auto-detected weight column:", weight_var, "\n")
    }
  }

  # Check if this appears to be aggregate data (has value/count columns)
  has_value_col <- any(potential_weight_cols %in% names(data))

  if (has_value_col && weight_var %in% names(data)) {
    # Aggregate data: sum the weight variable by groups
    cat("📊 Detected aggregate data structure - summing", weight_var, "by groups\n")

    result <- data %>%
      filter(!is.na(!!sym(weight_var))) %>%
      group_by(across(all_of(existing_vars))) %>%
      summarise(
        count = sum(!!sym(weight_var), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(count))

  } else {
    # Line list data: count rows by groups
    cat("📊 Detected line list data structure - counting rows by groups\n")

    result <- data %>%
      group_by(across(all_of(existing_vars))) %>%
      summarise(
        count = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(count))
  }

  return(result)
}

# Define consistent theme for AI-enhanced reports
theme_ai_report <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", color = ai_colors$primary),
      plot.subtitle = element_text(size = 12, color = ai_colors$muted),
      axis.title = element_text(size = 11, face = "bold"),
      legend.title = element_text(size = 11, face = "bold"),
      strip.text = element_text(size = 10, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# Alias for backwards compatibility
theme_report <- function() {
  theme_ai_report()
}

# Safe date parser utility function
.to_date_safe <- function(x) {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x_chr <- as.character(x)
  d1 <- suppressWarnings(lubridate::ymd(x_chr, quiet = TRUE))
  d2 <- suppressWarnings(lubridate::dmy(x_chr, quiet = TRUE))
  d3 <- suppressWarnings(lubridate::mdy(x_chr, quiet = TRUE))
  as.Date(dplyr::coalesce(d1, d2, d3), origin = "1970-01-01")
}

#' Plot consultations over time
#' 
#' @param data data.frame. Register data  
#' @param title Character. Plot title
#' @return ggplot object
#' 
plot_consultations_over_time <- function(data, title = "Consultations Over Time") {
  
  # Determine date column
  date_col <- case_when(
    "datevisit" %in% names(data) ~ "datevisit",
    "eventdate" %in% names(data) ~ "eventdate",
    "date_visit" %in% names(data) ~ "date_visit",
    TRUE ~ NA_character_
  )
  
  if (is.na(date_col)) {
    stop("No date column found in data")
  }
  
  # Create time series data using smart aggregation
  time_data <- data %>%
    filter(!is.na(.data[[date_col]])) %>%
    smart_aggregate(group_vars = date_col, weight_var = "value") %>%
    rename(consultations = count) %>%
    arrange(.data[[date_col]])
  
  # Calculate moving average
  time_data <- time_data %>%
    mutate(
      ma_7day = zoo::rollmean(consultations, k = 7, fill = NA, align = "right"),
      ma_30day = zoo::rollmean(consultations, k = 30, fill = NA, align = "right")
    )
  
  ggplot(time_data, aes(x = .data[[date_col]])) +
    geom_line(aes(y = consultations), color = "gray70", alpha = 0.7) +
    geom_line(aes(y = ma_7day), color = ai_colors$primary, size = 1, na.rm = TRUE) +
    geom_line(aes(y = ma_30day), color = ai_colors$danger, size = 1.2, na.rm = TRUE) +
    labs(
      title = title,
      subtitle = "Gray: daily counts, Blue: 7-day average, Red: 30-day average",
      x = "Date",
      y = "Number of Consultations"
    ) +
    theme_ai_report() +
    scale_y_continuous(labels = comma_format()) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}





#' Plot geographic distribution
#' 
#' @param data data.frame. Register data
#' @param geo_col Character. Geographic column name  
#' @param metric Character. Metric to plot ("count" or other)
#' @param title Character. Plot title
#' @return ggplot object
#' 
plot_geographic_distribution <- function(data, geo_col = "admin1", metric = "count", 
                                         title = "Geographic Distribution") {
  
  if (!geo_col %in% names(data)) {
    # Try alternative column names
    geo_col <- case_when(
      "admin1" %in% names(data) ~ "admin1",
      "governorate" %in% names(data) ~ "governorate",
      "region" %in% names(data) ~ "region",
      TRUE ~ NA_character_
    )
    
    if (is.na(geo_col)) {
      stop("No geographic column found in data")
    }
  }
  
  geo_data <- data %>%
    filter(!is.na(.data[[geo_col]])) %>%
    smart_aggregate(group_vars = geo_col, weight_var = "value") %>%
    slice_head(n = 15) %>%  # Limit to top 15 for readability
    mutate(
      percentage = round(count / sum(count) * 100, 1),
      !!geo_col := str_wrap(.data[[geo_col]], 20)
    )
  
  ggplot(geo_data, aes(x = reorder(.data[[geo_col]], count), y = count)) +
    geom_col(fill = ai_colors$primary, alpha = 0.8, color = "white", size = 0.5) +
    geom_text(aes(label = paste0(comma(count), "\n(", percentage, "%)")), 
              hjust = -0.1, size = 3, fontface = "bold") +
    coord_flip() +
    labs(
      title = title,
      subtitle = paste("Distribution across", length(unique(data[[geo_col]])), "regions"),
      x = "Administrative Region",
      y = "Number of Consultations"
    ) +
    theme_ai_report() +
    scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.15)))
}


#' Plot age-sex pyramid
#' 
#' @param data data.frame. Register data
#' @param title Character. Plot title
#' @return ggplot object
#' 
plot_age_sex_pyramid <- function(data, title = "Age-Sex Distribution") {
  
  # Try to find appropriate age and sex columns
  age_col <- case_when(
    "age_group" %in% names(data) ~ "age_group",
    "age_group_new" %in% names(data) ~ "age_group_new",
    "age_group2" %in% names(data) ~ "age_group2",
    "age_group3" %in% names(data) ~ "age_group3",
    "age_group4" %in% names(data) ~ "age_group4",
    "age_category" %in% names(data) ~ "age_category",
    TRUE ~ NA_character_
  )

  sex_col <- case_when(
    "sex" %in% names(data) ~ "sex",
    "gender" %in% names(data) ~ "gender",
    TRUE ~ NA_character_
  )

  if (is.na(age_col) || is.na(sex_col)) {
    stop("Age and sex columns not found in data. Available columns: ", paste(names(data), collapse = ", "))
  }
  
  # Prepare age-sex data
  age_sex_data <- data %>%
    filter(!is.na(.data[[age_col]]) & !is.na(.data[[sex_col]])) %>%
    smart_aggregate(group_vars = c(age_col, sex_col), weight_var = "value") %>%
    mutate(
      count = ifelse(.data[[sex_col]] == "Male", -count, count)
    )

  # Get the actual age group levels present in the data and sort them logically
  age_levels <- unique(age_sex_data[[age_col]])
  age_levels <- age_levels[!is.na(age_levels)]

  # Try to sort age groups logically (handle different formats)
  if (length(age_levels) > 0) {
    # Common age group patterns - add more as needed
    standard_order <- c(
      "0-4", "5-14", "15-24", "25-49", "50-64", "65+",  # Standard format
      "0-5 y", "6-14 y", "15-49 y", "50+ y",           # Synthetic data format
      "< 5 yrs", "5 - 14 yrs", "15 - 18 yrs", "19 - 49 yrs", ">= 50 yrs"  # Legacy format
    )

    # Order age levels based on standard order where possible, keep others at end
    ordered_levels <- c()
    for (level in standard_order) {
      if (level %in% age_levels) {
        ordered_levels <- c(ordered_levels, level)
      }
    }
    # Add any remaining levels not in standard order
    remaining_levels <- setdiff(age_levels, ordered_levels)
    age_levels <- c(ordered_levels, sort(remaining_levels))

    age_sex_data <- age_sex_data %>%
      mutate(!!age_col := factor(.data[[age_col]], levels = age_levels)) %>%
      arrange(.data[[age_col]])
  }
  
  ggplot(age_sex_data, aes(x = .data[[age_col]], y = count, fill = .data[[sex_col]])) +
    geom_col(alpha = 0.8, color = "white", size = 0.5) +
    coord_flip() +
    scale_fill_manual(
      values = c("Female" = ai_colors$secondary, "Male" = ai_colors$primary),
      name = "Sex"
    ) +
    scale_y_continuous(
      labels = function(x) comma(abs(x)),
      breaks = pretty_breaks(n = 6)
    ) +
    labs(
      title = title,
      subtitle = "Distribution of consultations by age group and sex",
      x = "Age Group",
      y = "Number of Consultations"
    ) +
    theme_ai_report() +
    theme(legend.position = "bottom") +
    geom_vline(xintercept = 0, color = "black", size = 0.5)
}


#' Plot disease distribution
#' 
#' @param data data.frame. Register data
#' @param disease_col Character. Disease column name
#' @param top_n Numeric. Number of top diseases to show
#' @param title Character. Plot title
#' @return ggplot object
#' 
plot_disease_distribution <- function(data, disease_col = "canonical_disease_imc", top_n = 10,
                                      title = "Disease Distribution") {
  
  # Determine disease column
  if (!disease_col %in% names(data)) {
    disease_col <- case_when(
      "canonical_disease_imc" %in% names(data) ~ "canonical_disease_imc",
      "morbidity" %in% names(data) ~ "morbidity",
      "icd11_title" %in% names(data) ~ "icd11_title",
      "category_canonical_disease_imc" %in% names(data) ~ "category_canonical_disease_imc",
      TRUE ~ NA_character_
    )
    
    if (is.na(disease_col)) {
      stop("No disease column found in data")
    }
  }
  
  disease_data <- data %>%
    filter(!is.na(.data[[disease_col]]) & .data[[disease_col]] != "") %>%
    smart_aggregate(group_vars = disease_col, weight_var = "value") %>%
    slice_head(n = top_n) %>%
    mutate(
      percentage = round(count / sum(count, na.rm = TRUE) * 100, 1),
      !!disease_col := str_wrap(.data[[disease_col]], 25),
      !!disease_col := fct_reorder(.data[[disease_col]], count)
    )
  
  ggplot(disease_data, aes(x = .data[[disease_col]], y = count)) +
    geom_col(fill = ai_colors$secondary, alpha = 0.8, color = "white", size = 0.5) +
    geom_text(aes(label = paste0(comma(count), "\n(", percentage, "%)")), 
              hjust = -0.1, size = 3, fontface = "bold") +
    coord_flip() +
    labs(
      title = title,
      subtitle = paste("Top", top_n, "disease categories by consultation volume"),
      x = "Disease Category",
      y = "Number of Cases"
    ) +
    theme_ai_report() +
    scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.15)))
}




#' Plot time series by category
#' 
#' @param data data.frame. Register data
#' @param date_col Character. Date column name
#' @param category_col Character. Category column name
#' @param top_n Numeric. Number of top categories to show
#' @param title Character. Plot title
#' @return ggplot object
#' 
plot_time_series_by_category <- function(data,
                                         date_col = "month2",
                                         category_col = "category_canonical_disease_imc",
                                         top_n = 6,
                                         title = "Disease Trends Over Time") {
  
  # Prefer a daily date column for proper 7/30-day rolling averages
  daily_date_col <- dplyr::case_when(
    "datevisit"  %in% names(data) ~ "datevisit",
    "eventdate"  %in% names(data) ~ "eventdate",
    "date_visit" %in% names(data) ~ "date_visit",
    TRUE ~ NA_character_
  )
  
  # If no explicit date_col provided or it's monthly, switch to daily when available
  if (!is.na(daily_date_col)) {
    date_col_used <- daily_date_col
  } else {
    # Fall back to monthly; create month2 if needed
    if (!date_col %in% names(data)) {
      stop("No usable date column found (tried daily columns and 'month2').")
    }
    date_col_used <- date_col
  }
  
  # Ensure category column exists
  if (!category_col %in% names(data)) {
    category_col <- dplyr::case_when(
      "category_canonical_disease_imc" %in% names(data) ~ "category_canonical_disease_imc",
      "canonical_disease_imc" %in% names(data) ~ "canonical_disease_imc",
      "morbidity"          %in% names(data) ~ "morbidity",
      TRUE ~ NA_character_
    )
    if (is.na(category_col)) stop("No category column found in data")
  }
  
  # Top categories by overall count
  top_categories <- data %>%
    dplyr::filter(!is.na(.data[[category_col]])) %>%
    dplyr::count(.data[[category_col]], sort = TRUE) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::pull(.data[[category_col]])
  
  # If we only have monthly dates, compute rolling averages in months (k=1,3) as a fallback
  monthly_mode <- is.na(daily_date_col)
  
  if (monthly_mode) {
    # Monthly aggregation
    ts_data <- data %>%
      dplyr::filter(.data[[category_col]] %in% top_categories,
                    !is.na(.data[[date_col_used]])) %>%
      dplyr::count(.data[[date_col_used]], .data[[category_col]], name = "cases") %>%
      dplyr::rename(date = !!date_col_used, category = !!category_col) %>%
      dplyr::arrange(category, date) %>%
      dplyr::group_by(category) %>%
      tidyr::complete(date = seq(min(date), max(date), by = "1 month"), fill = list(cases = 0)) %>%
      dplyr::mutate(
        ma_7day  = zoo::rollmean(cases, k = 1,  fill = NA, align = "right"),
        ma_30day = zoo::rollmean(cases, k = 3,  fill = NA, align = "right")
      ) %>%
      dplyr::ungroup()
    
    subtitle_txt <- "Gray: monthly counts, Blue: 1-month MA (fallback), Red: 3-month MA (fallback)"
    x_breaks <- "2 months"
    
  } else {
    # Daily aggregation with per-category completion of missing days
    ts_data <- data %>%
      dplyr::filter(.data[[category_col]] %in% top_categories,
                    !is.na(.data[[date_col_used]])) %>%
      dplyr::count(.data[[date_col_used]], .data[[category_col]], name = "cases") %>%
      dplyr::rename(date = !!date_col_used, category = !!category_col) %>%
      dplyr::arrange(category, date) %>%
      dplyr::group_by(category) %>%
      tidyr::complete(date = seq(min(date), max(date), by = "day"), fill = list(cases = 0)) %>%
      dplyr::mutate(
        ma_7day  = zoo::rollmean(cases, k = 7,  fill = NA, align = "right"),
        ma_30day = zoo::rollmean(cases, k = 30, fill = NA, align = "right")
      ) %>%
      dplyr::ungroup()
    
    subtitle_txt <- "Gray: daily counts, Blue: 7-day average, Red: 30-day average"
    x_breaks <- "1 month"
  }
  
  ggplot(ts_data, aes(x = date)) +
    # daily/monthly counts (gray)
    geom_line(aes(y = cases), color = "gray70", alpha = 0.7, linewidth = 0.6, na.rm = TRUE) +
    # moving averages (fixed colors like your consultations plot)
    geom_line(aes(y = ma_7day),  color = ai_colors$primary, linewidth = 0.9, na.rm = TRUE) +
    geom_line(aes(y = ma_30day), color = ai_colors$danger,  linewidth = 1.1, na.rm = TRUE) +
    facet_wrap(~ stringr::str_wrap(category, 20), scales = "free_y", ncol = 2) +
    labs(
      title = title,
      subtitle = subtitle_txt,
      x = if (monthly_mode) "Month" else "Date",
      y = "Number of Cases"
    ) +
    theme_ai_report() +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_x_date(date_breaks = x_breaks, date_labels = "%b %Y") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      strip.text = element_text(size = 10, face = "bold")
    )
}



#' Get consultation counts for a specific morbidity over time
#' 
#' @param data data.frame. Register data  
#' @param morbidity Character. Specific morbidity condition
#' @return data.frame. Daily counts for the specified morbidity
#' 
get_morbidity_counts <- function(data, morbidity) {
  
  # Determine morbidity and date columns
  morbidity_col <- case_when(
    "canonical_disease_imc" %in% names(data) ~ "canonical_disease_imc",
    "morbidity" %in% names(data) ~ "morbidity",
    "icd11_title" %in% names(data) ~ "icd11_title",
    "category_canonical_disease_imc" %in% names(data) ~ "category_canonical_disease_imc",
    TRUE ~ NA_character_
  )
  
  date_col <- case_when(
    "datevisit" %in% names(data) ~ "datevisit",
    "eventdate" %in% names(data) ~ "eventdate",
    TRUE ~ NA_character_
  )
  
  if (is.na(morbidity_col) || is.na(date_col)) {
    stop("Required columns for morbidity and date not found")
  }
  
  data %>%
    filter(.data[[morbidity_col]] == !!morbidity) %>%
    group_by(.data[[date_col]]) %>%
    summarise(count = n(), .groups = 'drop') %>%
    rename(date = !!date_col)
}

#' Get consultation counts for a specific morbidity by age group
#' 
#' @param data data.frame. Register data
#' @param morbidity Character. Specific morbidity condition
#' @return data.frame. Daily counts by age group for the specified morbidity
#' 
get_morbidity_counts_by_age <- function(data, morbidity) {
  
  # Determine required columns
  morbidity_col <- case_when(
    "canonical_disease_imc" %in% names(data) ~ "canonical_disease_imc",
    "morbidity" %in% names(data) ~ "morbidity",
    "icd11_title" %in% names(data) ~ "icd11_title",
    "category_canonical_disease_imc" %in% names(data) ~ "category_canonical_disease_imc",
    TRUE ~ NA_character_
  )
  
  date_col <- case_when(
    "datevisit" %in% names(data) ~ "datevisit",
    "eventdate" %in% names(data) ~ "eventdate",
    TRUE ~ NA_character_
  )
  
  age_col <- case_when(
    "age_group_new" %in% names(data) ~ "age_group_new",
    "age_group" %in% names(data) ~ "age_group",
    TRUE ~ NA_character_
  )
  
  if (any(is.na(c(morbidity_col, date_col, age_col)))) {
    stop("Required columns for morbidity, date, and age not found")
  }
  
  data %>%
    filter(.data[[morbidity_col]] == !!morbidity) %>%
    group_by(.data[[date_col]], .data[[age_col]]) %>%
    summarise(count = n(), .groups = 'drop') %>%
    rename(
      date = !!date_col,
      age_group = !!age_col
    )
}

#' Get calendar data for heatmap visualization
#' 
#' @param register data.frame. Register data
#' @param start_year Numeric. Starting year for filtering (default: 2021)
#' @return data.frame. Monthly consultation counts by year
#' 
get_calendar_data <- function(register, start_year = 2021) {
  
  # Create year and month columns if they don't exist
  date_col <- case_when(
    "datevisit" %in% names(register) ~ "datevisit",
    "eventdate" %in% names(register) ~ "eventdate",
    TRUE ~ NA_character_
  )
  
  if (is.na(date_col)) {
    stop("No date column found for calendar data")
  }
  
  calendar_data <- register %>%
    filter(!is.na(.data[[date_col]])) %>%
    mutate(
      year = year(.data[[date_col]]),
      month2 = floor_date(.data[[date_col]], "month")
    ) %>%
    filter(year >= start_year) %>%
    group_by(year, month2) %>%
    summarize(consultations = n(), .groups = "drop")
  
  log_info("Generated calendar data: {nrow(calendar_data)} months, {start_year}-{max(calendar_data$year)}")
  
  return(calendar_data)
}

#' Get register data for current year by disease category
#' 
#' @param register data.frame. Register data
#' @param target_year Numeric. Year to filter for (default: current year)
#' @return data.frame. Monthly consultations by disease category
#' 
get_register_data <- function(register, target_year = NULL) {
  
  if (is.null(target_year)) {
    # Determine year from available date column
    date_col <- case_when(
      "datevisit" %in% names(register) ~ "datevisit",
      "eventdate" %in% names(register) ~ "eventdate",
      TRUE ~ NA_character_
    )
    
    if (!is.na(date_col)) {
      target_year <- max(year(register[[date_col]]), na.rm = TRUE)
    } else {
      target_year <- year(Sys.Date())
    }
  }
  
  # Determine disease column
  disease_col <- case_when(
    "canonical_disease_imc" %in% names(data) ~ "canonical_disease_imc",
    "morbidity" %in% names(data) ~ "morbidity",
    "icd11_title" %in% names(data) ~ "icd11_title",
    "category_canonical_disease_imc" %in% names(data) ~ "category_canonical_disease_imc",
    TRUE ~ NA_character_
  )
  
  if (is.na(disease_col)) {
    stop("No disease category column found")
  }
  
  # Create year and month columns if needed
  if (!"year" %in% names(register) || !"month2" %in% names(register)) {
    date_col <- case_when(
      "datevisit" %in% names(register) ~ "datevisit",
      "eventdate" %in% names(register) ~ "eventdate",
      TRUE ~ NA_character_
    )
    
    if (!is.na(date_col)) {
      register <- register %>%
        mutate(
          year = year(.data[[date_col]]),
          month2 = floor_date(.data[[date_col]], "month")
        )
    }
  }
  
  register %>%
    filter(year == target_year) %>%
    group_by(month2, .data[[disease_col]]) %>%
    summarise(consultations = n(), .groups = "drop") %>%
    rename(disease_category = !!disease_col)
}


#' Plot syndromic surveillance trends
#'
#' Tracks monthly/quarterly/daily counts for HMIS-aligned syndrome groups.
#'
#' @param data data.frame with at least a diagnosis and date
#' @param groups list from get_syndromic_groups(); pass extra/overrides via get_syndromic_groups(extra_groups=...)
#' @param period "month", "quarter", or "date" (daily). Ignored if `period_col` is provided.
#' @param period_col optional existing column name for period (e.g., "month2")
#' @param date_cols candidate date columns to derive the period from (used if `period_col` is NULL)
#' @param diagnosis_cols candidate columns containing morbidity labels
#' @param window_days integer; limit to past N days relative to `ref_date` (default 365)
#' @param ref_date Date to anchor the window (default Sys.Date())
#' @param palette brewer palette name or NULL to use default qualitative palette
#' @return ggplot object (or invisible(NULL) if no data)
#' @export
plot_syndromic_surveillance <- function(
    data,
    groups = NULL,
    period = c("month", "quarter", "date"),
    period_col = NULL,
    date_cols = c("month2", "datevisit", "eventdate", "date_visit"),
    diagnosis_cols = c("canonical_disease_imc", "morbidity", "icd11_title", "category_canonical_disease_imc"),
    window_days = 365,
    ref_date = Sys.Date(),
    palette = "Dark2"
) {
  stopifnot(is.data.frame(data))
  period <- match.arg(period)
  
  # Enhanced fallback groups if get_syndromic_groups() fails or returns empty
  if (is.null(groups) || length(groups) == 0 || all(sapply(groups, length) == 0)) {
    groups <- get_epidemic_surveillance_fallback_groups()
    message("Using fallback syndromic groups: ", paste(names(groups), collapse = ", "))
  } else {
    message("Using primary syndromic groups: ", paste(names(groups), collapse = ", "))
  }
  
  # Pick columns
  dis_col  <- diagnosis_cols[diagnosis_cols %in% names(data)][1]
  if (is.na(dis_col)) stop("No diagnosis/morbidity column found in data.")
  
  # Build a period column
  df <- dplyr::as_tibble(data)
  
  if (!is.null(period_col) && period_col %in% names(df)) {
    df <- dplyr::mutate(df, .period = .data[[period_col]])
  } else {
    # Find a date column and derive period
    date_col <- date_cols[date_cols %in% names(df)][1]
    if (is.na(date_col)) stop("No date column found to derive the period.")
    
    df <- df |>
      dplyr::mutate(.dt = .to_date_safe(.data[[date_col]]))
    
    if (period == "month") {
      df <- dplyr::mutate(df, .period = lubridate::floor_date(.dt, "month"))
    } else if (period == "quarter") {
      df <- dplyr::mutate(df, .period = lubridate::floor_date(.dt, "quarter"))
    } else { # "date"
      df <- dplyr::mutate(df, .period = .dt)
    }
  }
  
  # Past-N-days filter if period is date or month/quarter (uses period start as proxy)
  # Determine a Date to compare
  period_date <- if (inherits(df$.period, "Date")) df$.period else {
    # try to coerce if it's POSIXct or something date-like; otherwise skip windowing
    suppressWarnings(as.Date(df$.period))
  }
  
  if (!all(is.na(period_date)) && is.finite(window_days) && window_days > 0) {
    cutoff <- as.Date(ref_date) - window_days
    df <- df[which(period_date >= cutoff), , drop = FALSE]
  }
  
  # Build long table by syndrome
  # For each syndrome, count cases per period with fuzzy matching
  out_list <- lapply(names(groups), function(syndrome) {
    conditions <- groups[[syndrome]]
    
    # Try exact match first
    exact_matches <- df |>
      dplyr::filter(!is.na(.period), .data[[dis_col]] %in% conditions)
    
    # If no exact matches, try fuzzy matching (case-insensitive and partial)
    if (nrow(exact_matches) == 0) {
      # Create patterns for fuzzy matching
      patterns <- paste(conditions, collapse = "|")
      fuzzy_matches <- df |>
        dplyr::filter(!is.na(.period), 
                     grepl(patterns, .data[[dis_col]], ignore.case = TRUE))
      
      # Use fuzzy matches if found
      if (nrow(fuzzy_matches) > 0) {
        exact_matches <- fuzzy_matches
      }
    }
    
    exact_matches |>
      dplyr::count(.period, name = "cases") |>
      dplyr::mutate(syndrome = syndrome)
  })
  
  syndromic_data <- dplyr::bind_rows(out_list)
  
  if (!nrow(syndromic_data)) {
    message("No syndromic cases found for the specified period/window.")
    
    # Get sample diseases from the data for diagnostics
    sample_diseases <- if (!is.na(dis_col)) {
      data %>%
        dplyr::filter(!is.na(.data[[dis_col]])) %>%
        dplyr::count(.data[[dis_col]], sort = TRUE) %>%
        dplyr::slice_head(n = 5) %>%
        dplyr::pull(.data[[dis_col]])
    } else character(0)
    
    # Return a diagnostic plot instead of NULL
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.9, 
                        label = "No syndromic surveillance data found",
                        size = 4, hjust = 0.5, face = "bold") +
      ggplot2::annotate("text", x = 0.5, y = 0.75,
                        label = paste("Searched", length(groups), "syndromic groups with",
                                     length(unlist(groups)), "disease patterns"),
                        size = 3, hjust = 0.5, color = "blue") +
      ggplot2::annotate("text", x = 0.5, y = 0.6,
                        label = paste("Top diseases in data:",
                                     paste(sample_diseases[seq_len(min(3, length(sample_diseases)))], 
                                           collapse = ", ")),
                        size = 3, hjust = 0.5, color = "darkgreen") +
      ggplot2::annotate("text", x = 0.5, y = 0.45,
                        label = if(length(sample_diseases) > 0) {
                          paste("Sample diseases found:", 
                                paste(sample_diseases[seq_len(min(3, length(sample_diseases)))], 
                                      collapse = ", "))
                        } else "No disease data available",
                        size = 3, hjust = 0.5, color = "gray60") +
      ggplot2::annotate("text", x = 0.5, y = 0.2,
                        label = paste("Using column:", dis_col),
                        size = 2, hjust = 0.5, color = "gray50") +
      ggplot2::labs(title = "Syndromic Surveillance Diagnostic") +
      ggplot2::theme_void() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")))
  }
  
  # Complete missing periods per syndrome for smooth lines (zero-fill)
  # Only if .period is a Date
  if (inherits(syndromic_data$.period, "Date")) {
    min_d <- min(syndromic_data$.period, na.rm = TRUE)
    max_d <- max(syndromic_data$.period, na.rm = TRUE)
    by_unit <- if (period == "date") "day" else if (period == "quarter") "quarter" else "month"
    
    syndromic_data <- syndromic_data |>
      tidyr::complete(
        syndrome,
        .period = seq(min_d, max_d, by = by_unit),
        fill = list(cases = 0)
      )
  }
  
  # Pick theme if present
  .theme <-
    if (exists("theme_report")) theme_report else
      if (exists("theme_ai_report")) theme_ai_report else
        ggplot2::theme_minimal
  
  p <- ggplot2::ggplot(syndromic_data, ggplot2::aes(x = .period, y = cases, color = syndrome)) +
    ggplot2::geom_line(linewidth = 1.1, alpha = 0.85) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = "Syndromic Surveillance Trends",
      subtitle = paste0(
        "Patterns in key syndrome groups (past ",
        ifelse(is.finite(window_days), window_days, "N"),
        " days)"
      ),
      x = if (!is.null(period_col)) period_col else switch(period, month = "Month", quarter = "Quarter", date = "Date"),
      y = "Number of Cases",
      color = "Syndrome Group"
    ) +
    .theme() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  # Date scale if applicable
  if (inherits(syndromic_data$.period, "Date")) {
    p <- p + ggplot2::scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")
  }
  
  # Palette
  if (!is.null(palette)) {
    p <- p + ggplot2::scale_color_brewer(type = "qual", palette = palette)
  }
  
  p
}

# Helper function for syndromic surveillance fallback groups
get_epidemic_surveillance_fallback_groups <- function() {
  list(
    "Respiratory" = c(
      "Acute respiratory infection", "Pneumonia", "Bronchitis", "Asthma",
      "Tuberculosis", "Suspected Tuberculosis", "Upper respiratory tract infection"
    ),
    "Gastrointestinal" = c(
      "Diarrhea", "Acute Watery Diarrhea", "Dysentery", "Gastroenteritis",
      "Suspected Cholera", "Abdominal pain"
    ),
    "Febrile_Illness" = c(
      "Fever", "Malaria", "Suspected Malaria", "Typhoid", "Typhus",
      "Brucellosis", "Leishmaniasis"
    ),
    "Vaccine_Preventable" = c(
      "Measles", "Suspected Measles", "Meningitis", "Hepatitis",
      "Pertussis", "Diphtheria"
    ),
    "Skin_Conditions" = c(
      "Scabies", "Skin infection", "Wound infection", "Dermatitis"
    )
  )
}

#' Enhanced malnutrition analysis with comprehensive demographics
#' @param data Register data frame
#' @param date_cols Vector of possible date column names
#' @param diagnosis_cols Vector of possible diagnosis column names  
#' @param age_cols Vector of possible age column names
#' @param sex_cols Vector of possible sex column names
#' @return ggplot object or grid of plots
#' @export
plot_malnutrition_demographics_enhanced <- function(
  data,
  date_cols = c("month2", "datevisit", "eventdate"),
  diagnosis_cols = c("canonical_disease_imc", "morbidity", "icd11_title", "category_canonical_disease_imc"),
  age_cols = c("age", "age_group", "age_group_new", "age_group4"),
  sex_cols = c("sex", "gender")
) {
  
  # Helper function to get malnutrition categories
  get_malnut_conditions <- function() {
    if (exists("get_malnutrition_categories")) {
      get_malnutrition_categories()
    } else {
      c("Malnutrition", "Severe Malnutrition", "Moderate acute Malnutrition",
        "Complicated severe acute Malnutrition", "Uncomplicated severe acute Malnutrition",
        "Kwashiorkor", "Marasmus", "Underweight")
    }
  }
  
  # Detect available columns
  date_col <- date_cols[date_cols %in% names(data)][1]
  diag_col <- diagnosis_cols[diagnosis_cols %in% names(data)][1]
  age_col <- age_cols[age_cols %in% names(data)][1]
  sex_col <- sex_cols[sex_cols %in% names(data)][1]
  
  # Enhanced malnutrition detection and classification
  malnut_analysis <- data %>%
    dplyr::mutate(
      # Multiple detection methods with safer column access
      is_malnutrition = if (!is.na(diag_col)) {
        grepl("malnutr|kwashior|marasmus|undernourish|wast|stunt|pem|sam|mam|protein.energy", 
              tolower(.data[[diag_col]]), ignore.case = TRUE)
      } else {
        FALSE
      },
      
      # Enhanced malnutrition type classification
      malnut_type = if (!is.na(diag_col)) {
        dplyr::case_when(
          grepl("severe.*acute|\\bsam\\b", tolower(.data[[diag_col]]), ignore.case = TRUE) ~ "Severe Acute (SAM)",
          grepl("moderate.*acute|\\bmam\\b", tolower(.data[[diag_col]]), ignore.case = TRUE) ~ "Moderate Acute (MAM)",
          grepl("complicated.*severe", tolower(.data[[diag_col]]), ignore.case = TRUE) ~ "Complicated SAM",
          grepl("uncomplicated.*severe", tolower(.data[[diag_col]]), ignore.case = TRUE) ~ "Uncomplicated SAM",
          grepl("chronic|stunt", tolower(.data[[diag_col]]), ignore.case = TRUE) ~ "Chronic/Stunting",
          grepl("kwashior", tolower(.data[[diag_col]]), ignore.case = TRUE) ~ "Kwashiorkor",
          grepl("marasmus", tolower(.data[[diag_col]]), ignore.case = TRUE) ~ "Marasmus",
          grepl("underweight", tolower(.data[[diag_col]]), ignore.case = TRUE) ~ "Underweight",
          TRUE ~ "General Malnutrition"
        )
      } else {
        "General Malnutrition"
      },
      
      # Enhanced age groups
      age_group_detailed = if (!is.na(age_col)) {
        if (is.numeric(.data[[age_col]])) {
          dplyr::case_when(
            .data[[age_col]] < 0.5 ~ "0-6 months",
            .data[[age_col]] < 2 ~ "6-24 months",
            .data[[age_col]] < 5 ~ "2-5 years",
            .data[[age_col]] < 15 ~ "5-15 years",
            .data[[age_col]] < 50 ~ "15-50 years",
            TRUE ~ "50+ years"
          )
        } else {
          as.character(.data[[age_col]])
        }
      } else {
        "Unknown"
      },
      
      # Standardized sex variable
      sex_clean = if (!is.na(sex_col)) {
        dplyr::case_when(
          tolower(.data[[sex_col]]) %in% c("female", "f", "femme", "woman") ~ "Female",
          tolower(.data[[sex_col]]) %in% c("male", "m", "homme", "man") ~ "Male",
          TRUE ~ "Other/Unknown"
        )
      } else {
        "Unknown"
      },
      
      # Time periods with better date handling
      month_year = if (!is.na(date_col)) {
        if (inherits(.data[[date_col]], "Date")) {
          .data[[date_col]]
        } else {
          suppressWarnings(lubridate::floor_date(safe_date_parse(.data[[date_col]]), "month"))
        }
      } else {
        as.Date(NA)
      }
    ) %>%
    dplyr::filter(is_malnutrition == TRUE, !is.na(month_year))
  
  total_malnut_cases <- nrow(malnut_analysis)
  
  if (total_malnut_cases == 0) {
    # No malnutrition cases found
    sample_diseases <- if (!is.na(diag_col)) {
      data %>%
        dplyr::count(.data[[diag_col]], sort = TRUE) %>%
        dplyr::slice_head(n = 5) %>%
        dplyr::pull(.data[[diag_col]])
    } else {
      character(0)
    }
    
    return(ggplot2::ggplot() + 
      ggplot2::annotate("text", x = 0.5, y = 0.7, 
                        label = paste("No malnutrition cases detected in", scales::comma(nrow(data)), "total records"), 
                        size = 4, hjust = 0.5) +
      ggplot2::annotate("text", x = 0.5, y = 0.3,
                        label = if(length(sample_diseases) > 0) {
                          paste("Sample diseases found:\n", paste(sample_diseases, collapse = ", "))
                        } else {
                          "Consider checking disease terminology\nor enabling malnutrition flags"
                        },
                        size = 3, hjust = 0.5, color = "gray60") +
      ggplot2::labs(title = "Enhanced Malnutrition Analysis") +
      ggplot2::theme_void() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")))
  }
  
  cat("Found", total_malnut_cases, "malnutrition cases for enhanced demographic analysis\n")
  
  # Create multi-panel visualization
  
  # 1. Trend over time by malnutrition type
  p1 <- malnut_analysis %>%
    dplyr::count(month_year, malnut_type, name = "cases") %>%
    ggplot2::ggplot(ggplot2::aes(x = month_year, y = cases, color = malnut_type)) +
    ggplot2::geom_line(size = 1.1, alpha = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = "Malnutrition Cases by Type Over Time",
      x = "Month", y = "Cases", color = "Malnutrition Type"
    ) +
    ggplot2::scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  # 2. Age and sex distribution
  p2 <- if (sum(!is.na(malnut_analysis$age_group_detailed)) > 5 && 
            sum(!is.na(malnut_analysis$sex_clean)) > 5) {
    malnut_analysis %>%
      dplyr::filter(age_group_detailed != "Unknown", sex_clean != "Unknown") %>%
      dplyr::count(age_group_detailed, sex_clean, name = "cases") %>%
      ggplot2::ggplot(ggplot2::aes(x = age_group_detailed, y = cases, fill = sex_clean)) +
      ggplot2::geom_col(position = "dodge", alpha = 0.7) +
      ggplot2::labs(
        title = "Malnutrition Cases by Age Group and Sex",
        x = "Age Group", y = "Cases", fill = "Sex"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::scale_fill_brewer(type = "qual", palette = "Set2")
  } else {
    ggplot2::ggplot() + 
      ggplot2::annotate("text", x = 0.5, y = 0.5, 
                        label = "Insufficient age/sex data for demographic analysis", 
                        size = 3) + 
      ggplot2::theme_void()
  }
  
  # 3. Type distribution by sex
  p3 <- if (sum(!is.na(malnut_analysis$sex_clean)) > 5) {
    malnut_analysis %>%
      dplyr::filter(sex_clean != "Unknown") %>%
      dplyr::count(malnut_type, sex_clean, name = "cases") %>%
      ggplot2::ggplot(ggplot2::aes(x = malnut_type, y = cases, fill = sex_clean)) +
      ggplot2::geom_col(position = "dodge", alpha = 0.7) +
      ggplot2::labs(
        title = "Malnutrition Types by Sex",
        x = "Malnutrition Type", y = "Cases", fill = "Sex"
      ) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::scale_fill_brewer(type = "qual", palette = "Set2")
  } else {
    ggplot2::ggplot() + 
      ggplot2::annotate("text", x = 0.5, y = 0.5, 
                        label = "Insufficient sex data for analysis", 
                        size = 3) + 
      ggplot2::theme_void()
  }
  
  # Combine plots if patchwork is available
  if (requireNamespace("patchwork", quietly = TRUE)) {
    p1 / (p2 | p3) + patchwork::plot_layout(heights = c(2, 1.5))
  } else {
    # Return main trend plot if patchwork not available
    p1
  }
}


#' Plot malnutrition trends by type and sex with configurable time period - FIXED VERSION
#'
#' @param data data.frame with at least a date and a morbidity column
#' @param period character: "month", "quarter", or "date" (daily). If period_col provided, this is ignored.
#' @param period_col optional character: name of an existing period column (e.g., "month2", "quarter")
#' @param date_cols candidate date columns to derive period from if period_col is NULL
#' @param diagnosis_cols candidate diagnosis columns to identify malnutrition cases
#' @param sex_col optional sex column name to facet by ("sex" by default)
#' @param use_flag_if_present logical; if TRUE and 'malnutrition_case' exists (0/1), use it
#' @param colors named vector for fill colors
#' @return ggplot object (or invisible(NULL) if no data)
#' @export
plot_malnutrition_trends <- function(
    data,
    period = "month",
    period_col = NULL,
    date_cols      = c("datevisit", "eventdate", "datevisitnew", "date_visit"),
    diagnosis_cols = c("canonical_disease_imc", "morbidity", "icd11_title", "category_canonical_disease_imc"),
    sex_col        = "sex",
    use_flag_if_present = TRUE,
    colors = c(
      "Moderate Acute" = "#FFA500",
      "Severe Acute"   = "#FF4500", 
      "Underweight"    = "#90EE90",
      "General Malnutrition" = "#87CEEB"
    )
) {
  # Input validation
  stopifnot(is.data.frame(data))
  
  # Validate period argument
  valid_periods <- c("month", "quarter", "date", "datevisit", "month2")
  if (!period %in% valid_periods) {
    period <- "month"
    warning("Invalid period specified, defaulting to 'month'")
  }
  
  cat("🔍 Malnutrition plot debug:\n")
  cat("   - Input data rows:", nrow(data), "\n")
  cat("   - Period:", period, "\n")
  cat("   - Period column:", period_col %||% "NULL", "\n")
  
  # --- Pick core columns safely ------------------------------------------------
  dis_col  <- diagnosis_cols[diagnosis_cols %in% names(data)][1]
  date_col <- date_cols[date_cols %in% names(data)][1]
  
  if (is.na(dis_col)) {
    cat("   ❌ No diagnosis column found\n")
    stop("No diagnosis column found in data. Available: ", paste(names(data), collapse = ", "))
  }
  
  if (is.null(period_col) && is.na(date_col)) {
    cat("   ❌ No date column found\n")
    stop("No date column found to derive the period. Available: ", paste(names(data), collapse = ", "))
  }
  
  cat("   - Disease column:", dis_col, "\n")
  cat("   - Date column:", date_col %||% "NULL", "\n")
  
  # --- Get malnutrition categories  -------------------------
  if (exists("get_malnutrition_categories", envir = .GlobalEnv)) {
    mal_cat <- get_malnutrition_categories()
    cat("   - Malnutrition categories loaded:", length(mal_cat), "\n")
  } else {
    # Fallback categories
    mal_cat <- c(
      "Complicated severe acute Malnutrition",
      "Moderate acute Malnutrition",
      "Severe Malnutrition", 
      "Uncomplicated severe acute Malnutrition",
      "Underweight",
      "Malnutrition"
    )
    cat("   - Using fallback malnutrition categories:", length(mal_cat), "\n")
  }
  
  # Test if any malnutrition diseases exist in data
  diseases_in_data <- unique(data[[dis_col]])
  mal_diseases_found <- intersect(diseases_in_data, mal_cat)
  cat("   - Malnutrition diseases in data:", length(mal_diseases_found), "\n")
  if (length(mal_diseases_found) > 0) {
    cat("   - Found:", paste(head(mal_diseases_found, 3), collapse = ", "), "\n")
  }
  
  df <- dplyr::as_tibble(data)
  
  # --- Safe date parsing function ----------------------------------------------
  safe_date_parse <- function(x) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))
    x <- as.character(x)
    d <- suppressWarnings(as.Date(x, "%Y-%m-%d"))
    if (all(is.na(d))) d <- suppressWarnings(as.Date(x, "%d/%m/%Y"))
    if (all(is.na(d))) d <- suppressWarnings(lubridate::ymd(x))
    if (all(is.na(d))) d <- suppressWarnings(lubridate::dmy(x))
    d
  }
  
  # --- Build a period column ---------------------------------------------------
  if (!is.null(period_col) && period_col %in% names(df)) {
    cat("   - Using existing period column:", period_col, "\n")
    df <- df |>
      dplyr::mutate(
        .dt = if (!is.na(date_col)) safe_date_parse(.data[[date_col]]) else as.Date(NA),
        .period = .data[[period_col]]
      )
    period_sym <- rlang::sym(".period")
  } else {
    # Derive from date
    cat("   - Deriving period from date column\n")
    df <- df |>
      dplyr::mutate(.dt = safe_date_parse(.data[[date_col]]))
    
    # Handle different period types
    if (period == "month" || period == "month2") {
      df <- df |>
        dplyr::mutate(.period = lubridate::floor_date(.dt, "month"))
    } else if (period == "quarter") {
      df <- df |>
        dplyr::mutate(.period = lubridate::floor_date(.dt, "quarter"))
    } else if (period == "datevisit" || period == "date") {
      df <- df |>
        dplyr::mutate(.period = .dt)
    } else {
      # Default to month
      df <- df |>
        dplyr::mutate(.period = lubridate::floor_date(.dt, "month"))
    }
    period_sym <- rlang::sym(".period")
  }
  
  # Check how many valid periods we have
  valid_periods_count <- sum(!is.na(df$.period))
  cat("   - Valid periods:", valid_periods_count, "\n")
  
  # --- Normalize sex labels for clean faceting --------------------------------
  if (sex_col %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        sex_norm = dplyr::case_when(
          is.na(.data[[sex_col]]) ~ "Unknown",
          .data[[sex_col]] %in% c("F","f","Female","female") ~ "Female",
          .data[[sex_col]] %in% c("M","m","Male","male")     ~ "Male",
          TRUE ~ as.character(.data[[sex_col]])
        )
      )
  } else {
    df <- dplyr::mutate(df, sex_norm = "All")
    cat("   - No sex column found, using 'All'\n")
  }
  
  # --- Filter to malnutrition cases --------------------------------------------
  has_flag <- use_flag_if_present && "malnutrition_case" %in% names(df)
  cat("   - Using malnutrition flag:", has_flag, "\n")
  
  df_mal <- df |>
    dplyr::filter(
      !is.na(!!period_sym),
      if (has_flag) .data[["malnutrition_case"]] == 1 else .data[[dis_col]] %in% mal_cat
    ) |>
    dplyr::mutate(
      malnutrition_type = dplyr::case_when(
        .data[[dis_col]] == "Moderate acute Malnutrition" ~ "Moderate Acute",
        .data[[dis_col]] %in% c(
          "Severe Malnutrition",
          "Complicated severe acute Malnutrition", 
          "Uncomplicated severe acute Malnutrition"
        ) ~ "Severe Acute",
        .data[[dis_col]] == "Underweight" ~ "Underweight",
        .data[[dis_col]] == "Malnutrition" ~ "General Malnutrition",
        TRUE ~ "Other"
      )
    ) |>
    dplyr::filter(!is.na(malnutrition_type), malnutrition_type != "Other")
  
  cat("   - Malnutrition cases after filtering:", nrow(df_mal), "\n")
  
  if (nrow(df_mal) == 0) {
    cat("   ❌ No malnutrition cases found for plotting\n")
    message("No malnutrition cases available for plotting.")
    return(invisible(NULL))
  }
  
  # Show breakdown of malnutrition types found
  type_counts <- table(df_mal$malnutrition_type)
  cat("   - Types found:", paste(names(type_counts), "=", type_counts, collapse = ", "), "\n")
  
  # --- Aggregate data for plotting ---------------------------------------------
  df_plot <- df_mal |>
    dplyr::group_by(!!period_sym, sex_norm, malnutrition_type) |>
    dplyr::summarise(cases = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(!!period_sym)
  
  cat("   - Final plot data rows:", nrow(df_plot), "\n")
  
  if (nrow(df_plot) == 0) {
    cat("   ❌ No data for plotting after aggregation\n")
    return(invisible(NULL))
  }
  
  # --- Create the plot ---------------------------------------------------------
  # Choose appropriate theme
  base_theme <- if (exists("theme_report", envir = .GlobalEnv)) {
    theme_report
  } else if (exists("theme_ai_report", envir = .GlobalEnv)) {
    theme_ai_report 
  } else {
    ggplot2::theme_minimal
  }
  
  # Dynamic x-axis label
  x_lab <- if (!is.null(period_col)) {
    period_col
  } else {
    switch(period,
           "month" = "Month",
           "month2" = "Month", 
           "quarter" = "Quarter",
           "datevisit" = "Date",
           "date" = "Date",
           "Time Period")
  }
  
  # Check if we should use date scale
  use_date_scale <- inherits(df_plot[[rlang::as_string(period_sym)]], "Date")
  cat("   - Using date scale:", use_date_scale, "\n")
  
  # Ensure colors are available for all malnutrition types in data
  types_in_plot <- unique(df_plot$malnutrition_type)
  missing_colors <- setdiff(types_in_plot, names(colors))
  if (length(missing_colors) > 0) {
    # Add default colors for missing types
    default_colors <- rainbow(length(missing_colors))
    names(default_colors) <- missing_colors
    colors <- c(colors, default_colors)
    cat("   - Added default colors for:", paste(missing_colors, collapse = ", "), "\n")
  }
  
  # Create the plot
  p <- ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = !!period_sym, y = cases, fill = malnutrition_type)
  ) +
    ggplot2::geom_area(alpha = 0.75, color = "white", size = 0.2) +
    ggplot2::scale_fill_manual(
      values = colors,
      name = "Malnutrition Type"
    ) +
    ggplot2::labs(
      title = "Malnutrition Case Trends by Type and Sex",
      subtitle = paste0("HMIS Categories (Period: ", x_lab, ")"),
      x = x_lab,
      y = "Number of Cases",
      fill = "Malnutrition Type"
    ) +
    base_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Add sex faceting only if we have multiple sex categories
  sex_categories <- unique(df_plot$sex_norm)
  if (length(sex_categories) > 1 && !"All" %in% sex_categories) {
    p <- p + ggplot2::facet_wrap(~ sex_norm, scales = "free_y")
    cat("   - Added sex faceting for:", paste(sex_categories, collapse = ", "), "\n")
  }
  
  # Configure x-axis for dates
  if (use_date_scale) {
    date_range <- range(df_plot[[rlang::as_string(period_sym)]], na.rm = TRUE)
    date_span <- as.numeric(difftime(date_range[2], date_range[1], units = "days"))
    
    if (date_span > 365) {
      # More than a year - use quarterly breaks
      p <- p + ggplot2::scale_x_date(
        date_breaks = "3 months", 
        date_labels = "%b\n%Y"
      )
    } else if (date_span > 90) {
      # More than 3 months - use monthly breaks  
      p <- p + ggplot2::scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b\n%Y"
      )
    } else {
      # Less than 3 months - use weekly breaks
      p <- p + ggplot2::scale_x_date(
        date_breaks = "2 weeks",
        date_labels = "%d %b"
      )
    }
  }
  
  # Add scale for y-axis
  p <- p + ggplot2::scale_y_continuous(
    labels = scales::comma_format(),
    expand = ggplot2::expansion(mult = c(0, 0.05))
  )
  
  cat("✅ Malnutrition plot created successfully\n")
  return(p)
}

# Wrapper function for backward compatibility with your existing call
plot_malnutrition_trends_wrapper <- function(register, period = "datevisit") {
  # Handle the period parameter from your original call
  if (period == "datevisit") {
    return(plot_malnutrition_trends(
      data = register,
      period = "month",  # Convert to sensible default
      period_col = NULL,
      date_cols = c("datevisit", "eventdate", "datevisitnew"),
      diagnosis_cols = c("canonical_disease_imc", "morbidity", "icd11_title", "category_canonical_disease_imc")
    ))
  } else {
    return(plot_malnutrition_trends(
      data = register,
      period = period,
      period_col = NULL,
      date_cols = c("datevisit", "eventdate", "datevisitnew"), 
      diagnosis_cols = c("canonical_disease_imc", "morbidity", "icd11_title", "category_canonical_disease_imc")
    ))
  }
}


display_malnutrition_summary <- function(malnutrition_summary, summary_vars) {
  fmt <- function(x) if (is.null(x) || is.na(x)) "0" else scales::comma(x)
  
  out <- character()
  
  if (malnutrition_summary$total_cases > 0) {
    out <- c(out, paste0("**Total Cases Identified**: ", fmt(malnutrition_summary$total_cases),
                         " malnutrition cases across all HMIS categories\n"))
    
    # Case distribution by type using HMIS categories
    if (malnutrition_summary$severe_cases > 0 || malnutrition_summary$moderate_cases > 0) {
      out <- c(out, "**Case Distribution by HMIS Categories:**")
      
      if (malnutrition_summary$severe_cases > 0) {
        out <- c(out, paste0("- **Severe Acute Malnutrition**: ", malnutrition_summary$severe_cases, " cases"))
      }
      if (malnutrition_summary$moderate_cases > 0) {
        out <- c(out, paste0("- **Moderate Acute Malnutrition**: ", malnutrition_summary$moderate_cases, " cases"))
      }
      if (malnutrition_summary$underweight_cases > 0) {
        out <- c(out, paste0("- **Underweight**: ", malnutrition_summary$underweight_cases, " cases"))
      }
      if (!is.null(malnutrition_summary$general_malnutrition_cases) && malnutrition_summary$general_malnutrition_cases > 0) {
        out <- c(out, paste0("- **General Malnutrition**: ", malnutrition_summary$general_malnutrition_cases, " cases"))
      }
      out <- c(out, "")
    }
    
    # Age and sex distribution
    if (!is.null(malnutrition_summary$under5_cases) && !is.na(malnutrition_summary$under5_cases)) {
      under5_pct <- round(malnutrition_summary$under5_cases / malnutrition_summary$total_cases * 100, 1)
      out <- c(out, paste0("**Age Distribution**: ", malnutrition_summary$under5_cases, 
                           " cases in children under 5 years (", under5_pct, "%)\n"))
    }
    
    if (!is.null(malnutrition_summary$female_cases) && !is.null(malnutrition_summary$male_cases)) {
      out <- c(out, paste0("**Sex Distribution**: ", 
                           malnutrition_summary$female_cases, " female, ",
                           malnutrition_summary$male_cases, " male cases\n"))
    }
    
    # Geographic coverage
    if (!is.null(malnutrition_summary$regions_affected) && !is.na(malnutrition_summary$regions_affected)) {
      out <- c(out, paste0("**Geographic Coverage**: ", malnutrition_summary$regions_affected, 
                           " administrative regions affected\n"))
    }
    
  } else {
    out <- c(out, "**No malnutrition cases detected** in the current reporting period using HMIS categories.\n")
  }
  
  # Print all as markdown
  cat(paste(out, collapse = "\n"))
}


#' Add severity and burden scores to morbidity data
#' @param data data.frame with morbidity names (or counts)
#' @param morbidity_col character; column with morbidity labels
#' @param count_col optional; if NULL, counts are computed
#' @param mapping output of get_severity_mapping()
#' @param weights output of severity_weights()
#' @param top_n optional; keep top N by count for plotting
#' @return data.frame with columns: morbidity, n, severity, frequency_rank, burden_score
add_burden_scores <- function(data,
                              morbidity_col = "canonical_disease_imc",
                              count_col = NULL,
                              mapping = get_severity_mapping(),
                              weights = severity_weights(),
                              top_n = 20) {
  stopifnot(morbidity_col %in% names(data))
  
  df <- dplyr::as_tibble(data)
  
  # counts
  if (is.null(count_col)) {
    df_counts <- df %>%
      dplyr::filter(!is.na(.data[[morbidity_col]])) %>%
      dplyr::count(.data[[morbidity_col]], name = "n") %>%
      dplyr::rename(morbidity = !!morbidity_col)
  } else {
    stopifnot(all(c(morbidity_col, count_col) %in% names(df)))
    df_counts <- df %>%
      dplyr::select(morbidity = !!morbidity_col, n = !!count_col) %>%
      dplyr::group_by(canonical_disease_imc) %>%
      dplyr::summarise(n = sum(n, na.rm = TRUE), .groups = "drop")
  }
  
  # map severities, fallback to default
  default_sev <- attr(mapping, "default_severity")
  df_scored <- df_counts %>%
    dplyr::mutate(
      severity = dplyr::if_else(canonical_disease_imc %in% names(mapping),
                                mapping[canonical_disease_imc],
                                default_sev),
      severity = factor(severity, levels = c("Low","Medium","High"), ordered = TRUE),
      frequency_rank = rank(-n, ties.method = "first"),
      burden_score = n * unname(weights[as.character(severity)])
    ) %>%
    dplyr::arrange(dplyr::desc(burden_score), frequency_rank)
  
  if (!is.null(top_n)) df_scored <- dplyr::slice_head(df_scored, n = top_n)
  df_scored
}

#' Plot disease burden matrix (helper)
#' @param scored_df output from add_burden_scores()
#' @return ggplot
plot_disease_burden_matrix <- function(scored_df) {
  stopifnot(!is.null(scored_df), nrow(scored_df) > 0)
  
  df <- scored_df %>%
    dplyr::filter(is.finite(frequency_rank), is.finite(burden_score)) %>%
    dplyr::mutate(label = stringr::str_wrap(canonical_disease_imc, 15))
  
  if (!nrow(df)) stop("No finite points to plot after filtering.")
  
  # axis padding to avoid zero-size panels
  xr <- range(df$frequency_rank, na.rm = TRUE)
  yr <- range(df$burden_score,   na.rm = TRUE)
  if (diff(xr) == 0) xr <- xr + c(-0.5, 0.5)
  if (diff(yr) == 0) {
    pad <- max(1, 0.05 * (abs(yr[1]) + 1))
    yr <- yr + c(-pad, pad)
  }
  
  base <- ggplot2::ggplot(df, ggplot2::aes(x = frequency_rank, y = burden_score)) +
    ggplot2::geom_point(ggplot2::aes(size = n, color = severity), alpha = 0.7) +
    ggplot2::scale_color_manual(values = c("High"="#D32F2F","Medium"="#FF6F00","Low"="#388E3C")) +
    ggplot2::scale_size_continuous(range = c(3, 12), labels = scales::comma_format()) +
    ggplot2::labs(
      title = "Disease Burden Matrix",
      subtitle = "Bubble size = frequency | Color = severity | Position = overall burden",
      x = "Frequency Rank (1 = most common)",
      y = "Burden Score (frequency × severity weight)",
      color = "Severity Level",
      size  = "Cases"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      plot.margin = ggplot2::margin(t = 10, r = 40, b = 10, l = 10)
    ) +
    ggplot2::coord_cartesian(xlim = xr, ylim = yr, clip = "off", expand = TRUE)
  
  # If too few/degenerate points, avoid ggrepel (can trigger zero-viewport errors)
  use_repel <- nrow(df) >= 3 &&
    (length(unique(df$frequency_rank)) > 1 || length(unique(df$burden_score)) > 1)
  
  if (use_repel) {
    base +
      ggrepel::geom_text_repel(
        ggplot2::aes(label = label, color = severity),
        size = 2.6, box.padding = 0.25, point.padding = 0.2,
        segment.size = 0.2, segment.color = "grey55",
        max.overlaps = Inf, min.segment.length = 0, na.rm = TRUE
      )
  } else {
    base +
      ggplot2::geom_text(
        ggplot2::aes(label = label, color = severity),
        size = 2.8, vjust = -0.6, na.rm = TRUE
      )
  }
}



# ─────────────────────────────────────────────────────────────────────────────
# Trauma plots + wrapper
# ─────────────────────────────────────────────────────────────────────────────

#' Utility: safe date range text for subtitles
#' @param data data.frame
#' @param date_cols candidate date columns to try, in order
#' @return string like "Jan 2024 to Aug 2025" or ""
date_range_label <- function(data, date_cols = c("datevisit","eventdate","date_visit","datevisitnew")) {
  dc <- date_cols[date_cols %in% names(data)][1]
  if (is.na(dc)) return("")
  x <- data[[dc]]
  if (!inherits(x, "Date")) {
    suppressWarnings(x <- as.Date(x))
  }
  if (all(is.na(x))) return("")
  paste(format(min(x, na.rm = TRUE), "%b %Y"),
        "to",
        format(max(x, na.rm = TRUE), "%b %Y"))
}

#' Plot trauma vs non-trauma distribution by region
#'
#' Chooses donut facets when regions <= max_donut_regions, else horizontal stacked bars.
#' @param summary_tbl output of build_trauma_summary()$summary
#' @param title plot title
#' @param subtitle optional subtitle
#' @param colors named vector for fills
#' @param max_donut_regions integer threshold for donut switch
#' @param percent_label_min minimum percent to show label on bars
#' @return ggplot object
#' @export
plot_trauma_distribution <- function(summary_tbl,
                                     title = "Trauma vs Non-Trauma Cases by Governorate",
                                     subtitle = NULL,
                                     colors = c("Trauma" = "#D32F2F", "Non-trauma" = "#1976D2"),
                                     max_donut_regions = 6,
                                     percent_label_min = 5) {
  stopifnot(is.data.frame(summary_tbl))
  if (!nrow(summary_tbl)) stop("Empty summary table provided to plot_trauma_distribution().")
  if (!all(c("admin1","type_case_clean","cases","percentage","total_cases") %in% names(summary_tbl)))
    stop("summary_tbl is missing required columns.")
  
  n_regions <- length(unique(summary_tbl$admin1))
  
  if (is.null(subtitle)) subtitle <- ""
  
  if (n_regions <= max_donut_regions) {
    ggplot2::ggplot(summary_tbl, ggplot2::aes(x = 1, y = cases, fill = type_case_clean)) +
      ggplot2::geom_col(width = 0.5, color = "white", linewidth = 1) +
      ggplot2::facet_wrap(~ paste0(admin1, "\n(", scales::comma(total_cases), " cases)"),
                          ncol = min(3, max(1, n_regions)), scales = "free") +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::scale_fill_manual(values = colors, name = "Case Type") +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(percentage, "%")),
        position = ggplot2::position_stack(vjust = 0.5),
        color = "white", fontface = "bold", size = 3
      ) +
      ggplot2::labs(title = title, subtitle = subtitle) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 15)),
        plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5, colour = "#666666", margin = ggplot2::margin(b = 20)),
        strip.text = ggplot2::element_text(size = 10, face = "bold", margin = ggplot2::margin(b = 10)),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(size = 12, face = "bold"),
        panel.spacing = grid::unit(2, "cm"),
        plot.margin = ggplot2::margin(20, 20, 20, 20)
      )
  } else {
    summary_tbl |>
      dplyr::mutate(admin1 = stats::reorder(admin1, total_cases)) |>
      ggplot2::ggplot(ggplot2::aes(x = admin1, y = cases, fill = type_case_clean)) +
      ggplot2::geom_col(position = "stack", alpha = 0.85, color = "white", linewidth = 0.4) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = colors, name = "Case Type") +
      ggplot2::geom_text(
        ggplot2::aes(label = ifelse(percentage >= percent_label_min, paste0(percentage, "%"), "")),
        position = ggplot2::position_stack(vjust = 0.5),
        color = "white", fontface = "bold", size = 3
      ) +
      ggplot2::scale_y_continuous(labels = scales::comma_format()) +
      ggplot2::labs(
        title = "Trauma vs Non-Trauma Cases Distribution by Governorate",
        subtitle = paste0("Stacked bar chart showing case distribution", ifelse(nchar(subtitle), paste0(" | ", subtitle), "")),
        x = "Region",
        y = "Number of Cases",
        caption = paste0("Percent labels shown when ≥", percent_label_min, "% of governorate total")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 10)),
        plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5, colour = "#666666", margin = ggplot2::margin(b = 15)),
        plot.caption = ggplot2::element_text(size = 10, colour = "#888888", hjust = 0.5, margin = ggplot2::margin(t = 15)),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(size = 12, face = "bold"),
        axis.title = ggplot2::element_text(size = 12, face = "bold"),
        axis.text = ggplot2::element_text(size = 10),
        panel.grid.minor = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(20, 20, 20, 20)
      )
  }
}



generate_basic_epidemic_alerts <- function(register) {
  
  # Get epidemic diseases
  if (exists("get_epidemic_diseases_standardized")) {
    epidemic_diseases <- get_epidemic_diseases_standardized()
  } else {
    return("**Epidemic disease definitions not available** for alert generation.")
  }
  
  disease_col <- case_when(
    "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
    "icd11_title" %in% names(register) ~ "icd11_title",
    "morbidity" %in% names(register) ~ "morbidity",
    "category_canonical_disease_imc" %in% names(register) ~ "category_canonical_disease_imc",
    TRUE ~ NA_character_
  )
  
  if (is.na(disease_col)) {
    return("**Disease data not available** for alert generation.")
  }
  
  # Calculate alerts for each disease
  alert_summary <- map_dfr(names(epidemic_diseases), function(disease_name) {
    disease_conditions <- epidemic_diseases[[disease_name]]
    
    recent_cases <- register %>%
      filter(.data[[disease_col]] %in% disease_conditions)
    
    if (nrow(recent_cases) == 0) {
      return(NULL)
    }
    
    # Count recent cases (if date available)
    if ("datevisit" %in% names(recent_cases)) {
      current_date <- max(register$datevisit, na.rm = TRUE)
      recent_count <- recent_cases %>%
        filter(datevisit >= current_date - 30) %>%
        nrow()
    } else {
      recent_count <- nrow(recent_cases)
    }
    
    # Determine alert level
    alert_level <- case_when(
      disease_name == "Acute Watery Diarrhea" & recent_count >= 10 ~ "HIGH",
      disease_name == "Measles" & recent_count >= 3 ~ "HIGH",
      disease_name == "Acute Flaccid Paralysis" & recent_count >= 1 ~ "HIGH",
      disease_name == "Meningitis" & recent_count >= 3 ~ "HIGH",
      recent_count >= 15 ~ "HIGH",
      recent_count >= 8 ~ "MEDIUM",
      recent_count >= 3 ~ "LOW",
      TRUE ~ "NONE"
    )
    
    if (alert_level != "NONE") {
      tibble(
        disease = disease_name,
        cases = recent_count,
        alert_level = alert_level
      )
    } else {
      NULL
    }
  })
  
  if (nrow(alert_summary) == 0) {
    return("✅ **No epidemic alerts detected** - Current disease patterns are within expected ranges.")
  }
  
  # Format alerts
  alert_text <- "🚨 **EPIDEMIC SURVEILLANCE ALERTS**\n\n"
  
  # High priority alerts
  high_alerts <- alert_summary %>% filter(alert_level == "HIGH")
  if (nrow(high_alerts) > 0) {
    alert_text <- paste0(alert_text, "**HIGH PRIORITY ALERTS:**\n")
    for (i in 1:nrow(high_alerts)) {
      alert_text <- paste0(alert_text, "- **", high_alerts$disease[i], "**: ", 
                           high_alerts$cases[i], " cases (IMMEDIATE ACTION REQUIRED)\n")
    }
    alert_text <- paste0(alert_text, "\n")
  }
  
  # Medium priority alerts
  medium_alerts <- alert_summary %>% filter(alert_level == "MEDIUM")
  if (nrow(medium_alerts) > 0) {
    alert_text <- paste0(alert_text, "**MEDIUM PRIORITY ALERTS:**\n")
    for (i in 1:nrow(medium_alerts)) {
      alert_text <- paste0(alert_text, "- **", medium_alerts$disease[i], "**: ", 
                           medium_alerts$cases[i], " cases (Enhanced monitoring recommended)\n")
    }
    alert_text <- paste0(alert_text, "\n")
  }
  
  # Add recommendations
  if (nrow(high_alerts) > 0) {
    alert_text <- paste0(alert_text, "**IMMEDIATE ACTIONS REQUIRED:**\n")
    alert_text <- paste0(alert_text, "- Activate outbreak response protocols\n")
    alert_text <- paste0(alert_text, "- Implement enhanced case finding and reporting\n")
    alert_text <- paste0(alert_text, "- Consider laboratory confirmation\n")
    alert_text <- paste0(alert_text, "- Review and update case management protocols\n")
  }
  
  return(alert_text)
}





# ─────────────────────────────────────────────────────────────────────────────
# Geospatial Visualization Helper Functions
# ─────────────────────────────────────────────────────────────────────────────

#' Load shapefile with error handling
#' 
#' @param path Character. Path to shapefile
#' @param level Character. Administrative level name for logging
#' @return sf object or NULL if loading fails
#' @export
load_shapefile <- function(path, level) {
  
  if (is.null(path) || is.na(path) || path == "") {
    warning("Invalid path provided for ", level, " shapefile")
    return(NULL)
  }
  
  if (file.exists(path)) {
    tryCatch({
      message("Attempting to load ", level, " shapefile from: ", path)
      shapefile <- sf::st_read(path, quiet = TRUE)
      message("✓ Successfully loaded ", level, " shapefile with ", nrow(shapefile), " features")
      
      # Print column names for debugging
      message("  Available columns: ", paste(colnames(shapefile), collapse = ", "))
      
      return(shapefile)
    }, error = function(e) {
      warning("✗ Failed to load ", level, " shapefile: ", e$message)
      return(NULL)
    })
  } else {
    # Check if it's a directory listing issue
    dir_path <- dirname(path)
    if (dir.exists(dir_path)) {
      files_in_dir <- list.files(dir_path, pattern = "\\.shp$", full.names = FALSE)
      if (length(files_in_dir) > 0) {
        message("✗ Shapefile not found at: ", path)
        message("  Available .shp files in ", dir_path, ":")
        for (file in files_in_dir) {
          message("    - ", file)
        }
      } else {
        message("✗ No .shp files found in directory: ", dir_path)
      }
    } else {
      message("✗ Directory does not exist: ", dir_path)
    }
    return(NULL)
  }
}

#' Create fallback bar chart when shapefiles aren't available
#' 
#' @param data data.frame with geographic data
#' @param x_col Character. Column name for x-axis (default "admin1")
#' @param y_col Character. Column name for y-axis (default "n")
#' @param title Character. Chart title
#' @return ggplot object
#' @export
create_fallback_map <- function(data, x_col = "admin1", y_col = "n", title) {
  ggplot(data, aes(x = reorder(.data[[x_col]], -.data[[y_col]]), y = .data[[y_col]])) +
    geom_col(fill = "#2E86AB", alpha = 0.8) +
    coord_flip() +
    labs(
      title = paste("Fallback:", title),
      subtitle = "Geographic shapefiles not available - showing bar chart",
      x = tools::toTitleCase(gsub("_", " ", x_col)),
      y = tools::toTitleCase(gsub("_", " ", y_col))
    ) +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma_format())
}

#' Create geographic summary table
#' 
#' @param data data.frame. Health register data
#' @param admin_col Character. Administrative column name (default "admin1")
#' @param facility_col Character. Facility column name (default "orgunit")
#' @param morbidity_col Character. Morbidity category column (default "category_canonical_disease_imc")
#' @param trauma_col Character. Case type column for trauma (default "type_case")
#' @return flextable object
#' @export
create_geographic_summary_table <- function(data, 
                                            admin_col = "admin1",
                                            facility_col = "orgunit", 
                                            morbidity_col = "category_canonical_disease_imc",
                                            trauma_col = "type_case") {
  
  if (!admin_col %in% colnames(data)) {
    stop("Administrative level data not available for summary table.")
  }
  
  geo_summary <- data %>%
    dplyr::group_by(.data[[admin_col]]) %>%
    dplyr::summarise(
      `Total Consultations` = n(),
      `Health Facilities` = dplyr::n_distinct(.data[[facility_col]], na.rm = TRUE),
      `Consultations per Facility` = round(n() / dplyr::n_distinct(.data[[facility_col]], na.rm = TRUE), 1),
      `Top Disease Category` = {
        if (morbidity_col %in% colnames(data)) {
          top_disease <- names(sort(table(.data[[morbidity_col]]), decreasing = TRUE))[1]
          if (is.na(top_disease)) "Unknown" else top_disease
        } else {
          "N/A"
        }
      },
      `Trauma Cases` = if(trauma_col %in% colnames(data)) {
        sum(.data[[trauma_col]] == "Trauma", na.rm = TRUE)
      } else {
        NA_integer_
      },
      `Trauma Percentage` = if(trauma_col %in% colnames(data)) {
        round(sum(.data[[trauma_col]] == "Trauma", na.rm = TRUE) / n() * 100, 1)
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(`Total Consultations`))
  
  # Create formatted table
  ft <- geo_summary %>%
    flextable::flextable() %>%
    flextable::autofit() %>%
    flextable::theme_vanilla() %>%
    flextable::bold(part = "header")
  
  # Color trauma percentages if available
  if (trauma_col %in% colnames(data)) {
    ft <- ft %>%
      flextable::color(~ `Trauma Percentage` > 20, ~ `Trauma Percentage`, color = "red") %>%
      flextable::color(~ `Trauma Percentage` > 10 & `Trauma Percentage` <= 20, ~ `Trauma Percentage`, color = "orange") %>%
      flextable::footnote(
        i = 1, j = "Trauma Percentage",
        value = flextable::as_paragraph("Red: >20%, Orange: 10-20% trauma cases"),
        ref_symbols = "*"
      )
  }
  
  ft <- ft %>%
    flextable::add_header_lines("Geographic Summary of Health Consultations") %>%
    flextable::set_caption("Summary statistics by governorate showing consultation volume, facility distribution, and trauma burden")
  
  return(ft)
}

#' Create governorate consultation choropleth map
#' 
#' @param data data.frame. Health register data
#' @param shapefile sf object. Admin1 shapefile
#' @param admin_col Character. Administrative column name (default "admin1") 
#' @param facility_col Character. Facility column name (default "orgunit")
#' @param year_col Character. Year column name (default "year")
#' @return ggplot object
#' @export
create_consultation_choropleth <- function(data, 
                                           shapefile = NULL,
                                           admin_col = "admin1",
                                           facility_col = "orgunit",
                                           year_col = "year") {
  
  # Prepare consultation data by admin level
  admin_data <- data %>%
    dplyr::filter(.data[[year_col]] == max(.data[[year_col]], na.rm = TRUE)) %>%
    dplyr::group_by(.data[[admin_col]]) %>%
    dplyr::summarise(
      consultations = n(),
      unique_facilities = dplyr::n_distinct(.data[[facility_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  if (is.null(shapefile)) {
    return(create_fallback_map(admin_data, admin_col, "consultations", "Consultations by Governorate"))
  }
  
  # Determine the correct column name for joining
  admin_cols <- colnames(shapefile)
  join_col <- dplyr::case_when(
    "NAME_EN" %in% admin_cols ~ "NAME_EN",
    "ADM1_EN" %in% admin_cols ~ "ADM1_EN",
    "admin1" %in% admin_cols ~ "admin1",
    TRUE ~ admin_cols[1]
  )
  
  # Join data with shapefile
  map_data <- shapefile %>%
    dplyr::left_join(admin_data, by = setNames(admin_col, join_col))
  
  # Calculate centroids for labeling
  map_data <- map_data %>%
    dplyr::mutate(
      centroid = sf::st_centroid(geometry),
      centroid_x = sf::st_coordinates(centroid)[, 1],
      centroid_y = sf::st_coordinates(centroid)[, 2],
      label_text = .data[[join_col]]
    )
  
  # Create choropleth map
  ggplot(map_data) +
    geom_sf(aes(fill = consultations), color = "white", size = 0.3) +
    geom_text(
      aes(x = centroid_x, y = centroid_y, label = label_text), 
      size = 3, color = "black", fontface = "bold", 
      check_overlap = TRUE
    ) +
    scale_fill_gradient2(
      low = "#E3F2FD", 
      mid = "#2196F3", 
      high = "#0D47A1",
      midpoint = median(map_data$consultations, na.rm = TRUE),
      na.value = "gray90",
      labels = scales::comma_format(),
      breaks = scales::pretty_breaks(n = 6)
    ) +
    labs(
      title = paste("Health Consultations by Governorate (", max(data[[year_col]], na.rm = TRUE), ")", sep = ""),
      subtitle = paste("Total consultations:", scales::comma(sum(admin_data$consultations))),
      fill = "Consultations"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.margin = margin(20, 20, 20, 20)
    )
}

#' Create facility distribution map with donut charts
#' 
#' @param data data.frame. Health register data
#' @param shapefile sf object. Admin1 shapefile  
#' @param admin_col Character. Administrative column name (default "admin1")
#' @param facility_col Character. Facility column name (default "orgunit")
#' @param facility_type_col Character. Facility type column (default "facilitytype")
#' @param year_col Character. Year column name (default "year")
#' @return ggplot object
#' @export
create_facility_distribution_map <- function(data,
                                             shapefile = NULL,
                                             admin_col = "admin1",
                                             facility_col = "orgunit",
                                             facility_type_col = "facility_type",
                                             year_col = "year") {
  
  if (!facility_type_col %in% colnames(data)) {
    stop("Facility type data not available for visualization.")
  }
  
  # Prepare facility type data
  facility_summary <- data %>%
    dplyr::filter(.data[[year_col]] == max(.data[[year_col]], na.rm = TRUE)) %>%
    dplyr::distinct(.data[[facility_col]], .data[[admin_col]], .data[[facility_type_col]]) %>%
    dplyr::filter(!is.na(.data[[facility_type_col]]) & !is.na(.data[[admin_col]])) %>%
    dplyr::count(.data[[admin_col]], .data[[facility_type_col]]) %>%
    dplyr::group_by(.data[[admin_col]]) %>%
    dplyr::mutate(
      total_facilities = sum(n),
      percentage = n / sum(n) * 100
    ) %>%
    dplyr::filter(total_facilities >= 2) %>%
    dplyr::ungroup()
  
  if (is.null(shapefile)) {
    # Fallback: stacked bar chart
    return(
      ggplot(facility_summary, aes(x = reorder(.data[[admin_col]], n), y = percentage, fill = .data[[facility_type_col]])) +
        geom_col(position = "stack") +
        coord_flip() +
        scale_fill_brewer(palette = "Set3") +
        labs(
          title = "Facility Type Distribution by Governorate",
          subtitle = "Geographic shapefiles not available - showing stacked bar chart",
          x = "Governorate",
          y = "Percentage of Facilities",
          fill = "Facility Type"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
    )
  }
  
  # Check if ggforce is available for donut charts
  if (!requireNamespace("ggforce", quietly = TRUE)) {
    warning("ggforce package required for donut charts. Creating alternative visualization...")
    
    # Get join column
    admin_cols <- colnames(shapefile)
    join_col <- dplyr::case_when(
      "NAME_EN" %in% admin_cols ~ "NAME_EN",
      "ADM1_EN" %in% admin_cols ~ "ADM1_EN",
      "admin1" %in% admin_cols ~ "admin1",
      TRUE ~ admin_cols[1]
    )
    
    # Create centroids
    centroids <- shapefile %>%
      dplyr::mutate(
        centroid_x = sf::st_coordinates(sf::st_centroid(geometry))[, 1],
        centroid_y = sf::st_coordinates(sf::st_centroid(geometry))[, 2],
        admin_name = .data[[join_col]]
      ) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(admin_name, centroid_x, centroid_y)
    
    # Join with facility data
    facility_with_coords <- facility_summary %>%
      dplyr::left_join(centroids, by = setNames("admin_name", admin_col)) %>%
      dplyr::filter(!is.na(centroid_x))
    
    # Create map with proportional symbols
    return(
      ggplot() +
        geom_sf(data = shapefile, fill = "lightgray", color = "white", size = 0.5) +
        geom_point(
          data = facility_with_coords,
          aes(x = centroid_x, y = centroid_y, size = n, color = .data[[facility_type_col]]),
          alpha = 0.7
        ) +
        geom_text(
          data = facility_with_coords %>% dplyr::group_by(.data[[admin_col]]) %>% dplyr::slice_head(n = 1),
          aes(x = centroid_x, y = centroid_y, label = .data[[admin_col]]),
          size = 3, color = "black", fontface = "bold",
          nudge_y = 0.1, check_overlap = TRUE
        ) +
        scale_size_continuous(range = c(2, 12), name = "Facilities") +
        scale_color_brewer(palette = "Set2", name = "Facility Type") +
        labs(
          title = paste("Health Facility Distribution by Governorate (", max(data[[year_col]], na.rm = TRUE), ")"),
          subtitle = "Point size represents number of facilities by type"
        ) +
        theme_void() +
        theme(
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12)
        )
    )
  }
  
  # Full donut chart implementation
  library(ggforce)
  
  # Calculate angles for donut charts
  facility_type_data <- facility_summary %>%
    dplyr::arrange(.data[[admin_col]], .data[[facility_type_col]]) %>%
    dplyr::group_by(.data[[admin_col]]) %>%
    dplyr::mutate(
      cum_percent = cumsum(percentage),
      start_angle = (dplyr::lag(cum_percent, default = 0) / 100) * 2 * pi,
      end_angle = (cum_percent / 100) * 2 * pi
    ) %>%
    dplyr::ungroup()
  
  # Get join column
  admin_cols <- colnames(shapefile)
  join_col <- dplyr::case_when(
    "NAME_EN" %in% admin_cols ~ "NAME_EN",
    "ADM1_EN" %in% admin_cols ~ "ADM1_EN",
    "admin1" %in% admin_cols ~ "admin1",
    TRUE ~ admin_cols[1]
  )
  
  # Create centroids with proper coordinate handling
  centroids <- shapefile %>%
    dplyr::mutate(
      geometry_4326 = sf::st_transform(geometry, 4326),
      centroid_geom = sf::st_centroid(geometry_4326),
      centroid_coords = sf::st_coordinates(centroid_geom),
      centroid_x = centroid_coords[, 1],
      centroid_y = centroid_coords[, 2],
      admin_name = .data[[join_col]]
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(admin_name, centroid_x, centroid_y)
  
  # Join with facility data
  facility_type_data <- facility_type_data %>%
    dplyr::left_join(centroids, by = setNames("admin_name", admin_col)) %>%
    dplyr::filter(!is.na(centroid_x) & !is.na(centroid_y))
  
  # Calculate appropriate radius based on map extent
  bbox <- sf::st_bbox(sf::st_transform(shapefile, 4326))
  map_width <- bbox$xmax - bbox$xmin
  map_height <- bbox$ymax - bbox$ymin
  
  # Scale radius based on map size
  base_radius <- min(map_width, map_height) * 0.04  # Increased size as requested
  inner_radius <- base_radius * 0.5
  outer_radius <- base_radius
  
  # Add facility count scaling
  facility_type_data <- facility_type_data %>%
    dplyr::mutate(
      scale_factor = sqrt(total_facilities) / sqrt(max(total_facilities)),
      inner_r = inner_radius * scale_factor,
      outer_r = outer_radius * scale_factor
    )
  
  # Create the map
  ggplot() +
    # Base map
    geom_sf(
      data = sf::st_transform(shapefile, 4326), 
      fill = "lightblue", color = "white", size = 0.5, alpha = 0.3
    ) +
    # Donut charts
    ggforce::geom_arc_bar(
      data = facility_type_data,
      aes(
        x0 = centroid_x, y0 = centroid_y,
        r0 = inner_r, r = outer_r,
        start = start_angle, end = end_angle,
        fill = .data[[facility_type_col]]
      ),
      alpha = 0.8
    ) +
    # Facility count labels
    geom_text(
      data = facility_type_data %>% 
        dplyr::group_by(.data[[admin_col]], centroid_x, centroid_y) %>% 
        dplyr::summarise(total = dplyr::first(total_facilities), .groups = "drop"),
      aes(x = centroid_x, y = centroid_y, label = total),
      size = 3, color = "black", fontface = "bold"
    ) +
    # Governorate labels
    geom_text(
      data = facility_type_data %>% 
        dplyr::group_by(.data[[admin_col]], centroid_x, centroid_y) %>% 
        dplyr::slice_head(n = 1),
      aes(x = centroid_x, y = centroid_y, label = .data[[admin_col]]),
      size = 2.5, color = "black", fontface = "bold",
      nudge_y = outer_radius * 1.5, check_overlap = TRUE
    ) +
    scale_fill_brewer(palette = "Set3", name = "Facility Type") +
    labs(
      title = paste("Health Facility Distribution by Governorate (", max(data[[year_col]], na.rm = TRUE), ")"),
      subtitle = "Donut size represents total facilities, segments show facility type distribution"
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.title = element_text(size = 12, face = "bold")
    ) +
    guides(fill = guide_legend(nrow = 2)) +
    coord_sf(crs = 4326)
}

#' Create district-level consultation heatmap
#' 
#' @param data data.frame. Health register data
#' @param shapefile sf object. Admin2 shapefile
#' @param admin_col Character. Administrative column name (default "admin2")
#' @param facility_col Character. Facility column name (default "orgunit")
#' @param year_col Character. Year column name (default "year")
#' @return ggplot object
#' @export
create_district_heatmap <- function(data,
                                    shapefile = NULL,
                                    admin_col = "admin2",
                                    facility_col = "orgunit",
                                    year_col = "year") {
  
  if (!admin_col %in% colnames(data)) {
    stop("District-level administrative data not available.")
  }
  
  # Prepare district-level data
  admin_data <- data %>%
    dplyr::filter(.data[[year_col]] == max(.data[[year_col]], na.rm = TRUE)) %>%
    dplyr::group_by(.data[[admin_col]]) %>%
    dplyr::summarise(
      consultations = n(),
      facilities = dplyr::n_distinct(.data[[facility_col]], na.rm = TRUE),
      consultations_per_facility = n() / dplyr::n_distinct(.data[[facility_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  if (is.null(shapefile)) {
    admin_summary <- admin_data %>%
      dplyr::arrange(dplyr::desc(consultations)) %>%
      dplyr::slice_head(n = 15)
    
    return(create_fallback_map(admin_summary, admin_col, "consultations", "Consultations by District"))
  }
  
  # Determine join column
  admin_cols <- colnames(shapefile)
  join_col <- dplyr::case_when(
    "NAME_EN" %in% admin_cols ~ "NAME_EN",
    "ADM2_EN" %in% admin_cols ~ "ADM2_EN", 
    "admin2" %in% admin_cols ~ "admin2",
    TRUE ~ admin_cols[1]
  )
  
  # Join with shapefile
  map_data <- shapefile %>%
    dplyr::left_join(admin_data, by = setNames(admin_col, join_col))
  
  # Create heatmap
  ggplot(map_data) +
    geom_sf(aes(fill = consultations), color = "white", size = 0.1) +
    scale_fill_viridis_c(
      option = "plasma", 
      na.value = "gray90",
      labels = scales::comma_format(),
      breaks = scales::pretty_breaks(n = 6),
      trans = "sqrt"  # Square root transformation
    ) +
    labs(
      title = paste("Consultation Density by District (", max(data[[year_col]], na.rm = TRUE), ")", sep = ""),
      subtitle = "Square root scale used to enhance visualization of variation",
      fill = "Consultations"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      plot.margin = margin(20, 20, 20, 20)
    )
}

#' Create disease-specific geographic pattern maps
#' 
#' @param data data.frame. Health register data
#' @param shapefile sf object. Admin1 shapefile
#' @param admin_col Character. Administrative column name (default "admin1")
#' @param morbidity_col Character. Morbidity category column (default "category_canonical_disease_imc")
#' @param year_col Character. Year column name (default "year")
#' @param top_n Integer. Number of top diseases to map (default 6)
#' @return ggplot object (requires patchwork for multiple panels)
#' @export
create_disease_geography_maps <- function(data,
                                          shapefile = NULL,
                                          admin_col = "admin1", 
                                          morbidity_col = "category_canonical_disease_imc",
                                          year_col = "year",
                                          top_n = 6) {
  
  if (!morbidity_col %in% colnames(data)) {
    stop("Disease category data not available for mapping.")
  }
  
  # Get top disease categories
  top_diseases <- data %>%
    dplyr::count(.data[[morbidity_col]], sort = TRUE) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::pull(.data[[morbidity_col]])
  
  # Prepare disease data by governorate
  disease_geo_data <- data %>%
    dplyr::filter(.data[[morbidity_col]] %in% top_diseases & !is.na(.data[[admin_col]])) %>%
    dplyr::group_by(.data[[admin_col]], .data[[morbidity_col]]) %>%
    dplyr::summarise(cases = n(), .groups = "drop")
  
  if (is.null(shapefile)) {
    # Fallback: faceted bar charts
    return(
      ggplot(disease_geo_data, aes(x = reorder(.data[[admin_col]], cases), y = cases)) +
        geom_col(fill = "#D32F2F", alpha = 0.7) +
        facet_wrap(~ .data[[morbidity_col]], scales = "free_y") +
        coord_flip() +
        labs(
          title = "Geographic Distribution of Top Disease Categories",
          subtitle = "Geographic shapefiles not available - showing bar charts",
          x = "Governorate",
          y = "Cases"
        ) +
        theme_minimal() +
        theme(
          strip.text = element_text(size = 10, face = "bold"),
          axis.text.x = element_text(size = 8)
        )
    )
  }
  
  # Get join column
  admin_cols <- colnames(shapefile)
  join_col <- dplyr::case_when(
    "NAME_EN" %in% admin_cols ~ "NAME_EN",
    "ADM1_EN" %in% admin_cols ~ "ADM1_EN",
    "admin1" %in% admin_cols ~ "admin1",
    TRUE ~ admin_cols[1]
  )
  
  # Create maps for each disease category
  disease_maps <- purrr::map(top_diseases, function(disease) {
    disease_data <- disease_geo_data %>%
      dplyr::filter(.data[[morbidity_col]] == disease)
    
    map_data <- shapefile %>%
      dplyr::left_join(disease_data, by = setNames(admin_col, join_col))
    
    ggplot(map_data) +
      geom_sf(aes(fill = cases), color = "white", size = 0.2) +
      scale_fill_gradient(
        low = "white", 
        high = "#D32F2F",
        na.value = "gray95",
        labels = scales::comma_format()
      ) +
      labs(
        title = stringr::str_wrap(disease, width = 20),
        fill = "Cases"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.4, "cm")
      )
  })
  
  # Return list of maps (to be combined with patchwork in R Markdown)
  return(disease_maps)
}

#' Create trauma geographic analysis map
#' 
#' @param data data.frame. Health register data
#' @param shapefile sf object. Admin1 shapefile
#' @param admin_col Character. Administrative column name (default "admin1")
#' @param trauma_col Character. Case type column for trauma (default "type_case")
#' @param year_col Character. Year column name (default "year")
#' @return ggplot object
#' @export
create_trauma_geography_map <- function(data,
                                        shapefile = NULL,
                                        admin_col = "admin1",
                                        trauma_col = "type_case", 
                                        year_col = "year") {
  
  if (!trauma_col %in% colnames(data)) {
    stop("Trauma case data not available.")
  }
  
  # Prepare trauma data
  trauma_data <- data %>%
    dplyr::filter(!is.na(.data[[trauma_col]]) & !is.na(.data[[admin_col]])) %>%
    dplyr::group_by(.data[[admin_col]], .data[[trauma_col]]) %>%
    dplyr::summarise(cases = n(), .groups = "drop") %>%
    dplyr::group_by(.data[[admin_col]]) %>%
    dplyr::mutate(
      total_cases = sum(cases),
      trauma_percentage = ifelse(.data[[trauma_col]] == "Trauma", cases / total_cases * 100, NA)
    ) %>%
    dplyr::filter(.data[[trauma_col]] == "Trauma") %>%
    dplyr::select(.data[[admin_col]], trauma_cases = cases, trauma_percentage, total_cases)
  
  if (is.null(shapefile)) {
    return(create_fallback_map(trauma_data, admin_col, "trauma_percentage", "Trauma Percentage by Governorate"))
  }
  
  # Get join column
  admin_cols <- colnames(shapefile)
  join_col <- dplyr::case_when(
    "NAME_EN" %in% admin_cols ~ "NAME_EN",
    "ADM1_EN" %in% admin_cols ~ "ADM1_EN",
    "admin1" %in% admin_cols ~ "admin1",
    TRUE ~ admin_cols[1]
  )
  
  # Join with shapefile
  map_data <- shapefile %>%
    dplyr::left_join(trauma_data, by = setNames(admin_col, join_col))
  
  # Create trauma percentage map
  ggplot(map_data) +
    geom_sf(aes(fill = trauma_percentage), color = "white", size = 0.3) +
    scale_fill_gradient2(
      low = "#E8F5E8", 
      mid = "#FFA726", 
      high = "#D32F2F",
      midpoint = 15,  # Typical trauma percentage threshold
      na.value = "gray90",
      labels = function(x) paste0(x, "%"),
      breaks = scales::pretty_breaks(n = 6)
    ) +
    labs(
      title = "Trauma Cases as Percentage of Total Consultations",
      subtitle = paste("Geographic distribution of trauma burden (", max(data[[year_col]], na.rm = TRUE), ")", sep = ""),
      fill = "Trauma %"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      plot.margin = margin(20, 20, 20, 20)
    )
}

#' Setup geospatial environment and load shapefiles
#' 
#' @param shapefile_dir Character. Directory containing shapefiles (default uses here::here("data", "shapefiles"))
#' @param admin1_file Character. Admin1 shapefile name (default "syr_admin1.shp")
#' @param admin2_file Character. Admin2 shapefile name (default "syr_admin2.shp") 
#' @param admin3_file Character. Admin3 shapefile name (default "syr_admin3.shp")
#' @param use_here Logical. Whether to use here::here() for path construction (default TRUE)
#' @return List of loaded shapefiles (admin1, admin2, admin3)
#' @export
setup_geospatial_environment <- function(shapefile_dir = NULL,
                                         admin1_file = "syr_admin1.shp",
                                         admin2_file = "syr_admin2.shp", 
                                         admin3_file = "syr_admin3.shp",
                                         use_here = TRUE) {
  
  # Load required packages
  suppressPackageStartupMessages({
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("sf package is required for geospatial analysis. Please install it with: install.packages('sf')")
    }
    library(sf)
    
    if (requireNamespace("viridis", quietly = TRUE)) {
      library(viridis)
    }
    if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      library(RColorBrewer)
    }
  })
  
  # Determine shapefile directory
  if (is.null(shapefile_dir)) {
    if (use_here && requireNamespace("here", quietly = TRUE)) {
      shapefile_dir <- here::here("data", "shapefiles", "syria")
    } else {
      shapefile_dir <- file.path("data", "shapefiles", "syria")
    }
  }
  
  # Print the directory being used for debugging
  message("Looking for shapefiles in: ", shapefile_dir)
  
  # Check if directory exists
  if (!dir.exists(shapefile_dir)) {
    warning("Shapefile directory does not exist: ", shapefile_dir)
    message("Please ensure shapefiles are located in the correct directory or specify the correct path.")
    return(list(admin1 = NULL, admin2 = NULL, admin3 = NULL))
  }
  
  # Define shapefile paths
  shapefile_paths <- list(
    admin1 = file.path(shapefile_dir, admin1_file),
    admin2 = file.path(shapefile_dir, admin2_file),
    admin3 = file.path(shapefile_dir, admin3_file)
  )
  
  # Print paths for debugging
  message("Shapefile paths:")
  for (name in names(shapefile_paths)) {
    path <- shapefile_paths[[name]]
    exists <- file.exists(path)
    message("  ", name, ": ", path, " (", ifelse(exists, "EXISTS", "NOT FOUND"), ")")
  }
  
  # Load shapefiles
  shapefiles <- list(
    admin1 = load_shapefile(shapefile_paths$admin1, "admin1"),
    admin2 = load_shapefile(shapefile_paths$admin2, "admin2"),
    admin3 = load_shapefile(shapefile_paths$admin3, "admin3")
  )
  
  # Summary message
  loaded_count <- sum(!sapply(shapefiles, is.null))
  message("Successfully loaded ", loaded_count, " out of 3 shapefiles")
  
  return(shapefiles)
}

#' Setup geospatial environment with custom paths
#' 
#' Alternative setup function that allows full path specification
#' @param admin1_path Character. Full path to admin1 shapefile
#' @param admin2_path Character. Full path to admin2 shapefile  
#' @param admin3_path Character. Full path to admin3 shapefile
#' @return List of loaded shapefiles (admin1, admin2, admin3)
#' @export
setup_geospatial_with_paths <- function(admin1_path = NULL, 
                                        admin2_path = NULL, 
                                        admin3_path = NULL) {
  
  # Load required packages
  suppressPackageStartupMessages({
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("sf package is required for geospatial analysis. Please install it with: install.packages('sf')")
    }
    library(sf)
  })
  
  # Load shapefiles with provided paths
  shapefiles <- list(
    admin1 = if (!is.null(admin1_path)) load_shapefile(admin1_path, "admin1") else NULL,
    admin2 = if (!is.null(admin2_path)) load_shapefile(admin2_path, "admin2") else NULL,
    admin3 = if (!is.null(admin3_path)) load_shapefile(admin3_path, "admin3") else NULL
  )
  
  return(shapefiles)
}

#' Create comprehensive geospatial analysis
#' 
#' Wrapper function that creates all geospatial visualizations
#' @param data data.frame. Health register data
#' @param shapefiles List. Output from setup_geospatial_environment()
#' @param output_format Character. "list" returns list of plots, "rmd" returns R Markdown code
#' @return List of ggplot objects or character string with R Markdown code
#' @export
create_geospatial_analysis <- function(data, shapefiles, output_format = "list") {
  
  plots <- list()
  
  # 1. Geographic summary table
  if (output_format == "list") {
    plots$summary_table <- create_geographic_summary_table(data)
  }
  
  # 2. Consultation choropleth
  plots$consultation_map <- create_consultation_choropleth(
    data = data,
    shapefile = shapefiles$admin1
  )
  
  # 3. Facility distribution map
  if ("facility_type" %in% colnames(data)) {
    plots$facility_map <- create_facility_distribution_map(
      data = data,
      shapefile = shapefiles$admin1
    )
  }
  
  # 4. District heatmap
  if ("admin2" %in% colnames(data)) {
    plots$district_heatmap <- create_district_heatmap(
      data = data,
      shapefile = shapefiles$admin2
    )
  }
  
  # 5. Disease geography maps
  if ("category_canonical_disease_imc" %in% colnames(data)) {
    plots$disease_maps <- create_disease_geography_maps(
      data = data,
      shapefile = shapefiles$admin1
    )
  }
  
  # 6. Trauma geography
  if ("type_case" %in% colnames(data)) {
    plots$trauma_map <- create_trauma_geography_map(
      data = data,
      shapefile = shapefiles$admin1
    )
  }
  
  if (output_format == "rmd") {
    return(generate_geospatial_rmd_code())
  }
  
  return(plots)
}



# ─────────────────────────────────────────────────────────────────────────────
# Health Service Utilization Visualization Functions
# ─────────────────────────────────────────────────────────────────────────────

#' Create facility type distribution chart faceted by governorate
#' 
#' Shows service utilization by facility type across different governorates
#' @param data data.frame. Health register data
#' @param facility_col Character. Facility type column name (default "facility_type")
#' @param admin_col Character. Administrative column name (default "admin1")
#' @param orgunit_col Character. Organization unit column (default "orgunit")
#' @param top_facilities Integer. Number of top facility types to show (default 12)
#' @param top_admin Integer. Number of top governorates to show (default 8)
#' @return ggplot object
#' @export
create_facility_type_distribution_chart <- function(data,
                                                    facility_col = "facility_type",
                                                    admin_col = "admin1", 
                                                    orgunit_col = "orgunit",
                                                    top_facilities = 12,
                                                    top_admin = 8) {
  
  # Check required columns
  required_cols <- c(facility_col, admin_col)
  if (!all(required_cols %in% colnames(data))) {
    stop("Required columns not available: ", paste(required_cols, collapse = ", "))
  }
  
  # Calculate facility-governorate summary
  facility_govt <- data %>%
    dplyr::filter(!is.na(.data[[facility_col]]), !is.na(.data[[admin_col]])) %>%
    dplyr::group_by(.data[[facility_col]], .data[[admin_col]]) %>%
    dplyr::summarise(
      consultations = dplyr::n(),
      unique_facilities = if (orgunit_col %in% colnames(data)) {
        dplyr::n_distinct(.data[[orgunit_col]], na.rm = TRUE)
      } else {
        NA_integer_
      },
      .groups = "drop"
    )
  
  if (nrow(facility_govt) == 0) {
    stop("No data available after filtering.")
  }
  
  # Get top facility types
  top_facility_types <- facility_govt %>%
    dplyr::group_by(.data[[facility_col]]) %>%
    dplyr::summarise(total = sum(consultations), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(total)) %>%
    dplyr::slice_head(n = top_facilities) %>%
    dplyr::pull(.data[[facility_col]])
  
  # Get top governorates
  top_admin_levels <- facility_govt %>%
    dplyr::group_by(.data[[admin_col]]) %>%
    dplyr::summarise(total = sum(consultations), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(total)) %>%
    dplyr::slice_head(n = top_admin) %>%
    dplyr::pull(.data[[admin_col]])
  
  # Prepare plot data
  plot_df <- facility_govt %>%
    dplyr::filter(.data[[facility_col]] %in% top_facility_types, 
                  .data[[admin_col]] %in% top_admin_levels) %>%
    dplyr::mutate(
      facility_type_ordered = forcats::fct_reorder(.data[[facility_col]], consultations)
    )
  
  if (nrow(plot_df) == 0) {
    stop("No data available after filtering for top facility types and governorates.")
  }
  
  # Create the plot
  ggplot(plot_df, aes(x = facility_type_ordered, y = consultations)) +
    geom_col(fill = "#2E86AB", alpha = 0.9) +
    coord_flip() +
    facet_wrap(~ .data[[admin_col]], scales = "free_y") +
    labs(
      title = "Service Utilization by Facility Type",
      subtitle = "Faceted by Governorate (top facility types and governorates)",
      x = "Facility Type",
      y = "Number of Consultations"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 8)
    ) +
    scale_y_continuous(labels = scales::comma_format())
}

#' Create facility daily consultation distribution histogram
#' 
#' Shows distribution of average daily consultations across facilities
#' @param data data.frame. Health register data
#' @param orgunit_col Character. Organization unit column (default "orgunit")
#' @param admin_col Character. Administrative column name (default "admin1")
#' @param date_col Character. Date column (default "datevisit")
#' @param morbidity_col Character. Morbidity category column (default "category_canonical_disease_imc")
#' @param bins Integer. Number of histogram bins (default 20)
#' @return ggplot object
#' @export
create_facility_utilization_histogram <- function(data,
                                                  orgunit_col = "orgunit",
                                                  admin_col = "admin1",
                                                  date_col = "datevisit", 
                                                  morbidity_col = "category_canonical_disease_imc",
                                                  bins = 20) {
  
  # Check required columns
  required_cols <- c(orgunit_col, admin_col, date_col)
  if (!all(required_cols %in% colnames(data))) {
    stop("Required columns not available: ", paste(required_cols, collapse = ", "))
  }
  
  # Calculate facility utilization metrics
  facility_utilization <- data %>%
    dplyr::group_by(.data[[orgunit_col]], .data[[admin_col]]) %>%
    dplyr::summarise(
      total_consultations = dplyr::n(),
      unique_days = dplyr::n_distinct(.data[[date_col]], na.rm = TRUE),
      daily_average = total_consultations / pmax(unique_days, 1),
      disease_diversity = if (morbidity_col %in% colnames(data)) {
        dplyr::n_distinct(.data[[morbidity_col]], na.rm = TRUE)
      } else {
        NA_integer_
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      utilization_category = dplyr::case_when(
        daily_average >= 50 ~ "High Utilization",
        daily_average >= 20 ~ "Medium Utilization",
        TRUE ~ "Low Utilization"
      )
    ) %>%
    dplyr::filter(!is.na(.data[[admin_col]]))
  
  if (nrow(facility_utilization) == 0) {
    stop("No facility utilization data available.")
  }
  
  # Create histogram
  ggplot(facility_utilization, aes(x = daily_average, fill = .data[[admin_col]])) +
    geom_histogram(bins = bins, alpha = 0.7, color = "white") +
    facet_wrap(~ .data[[admin_col]]) +
    labs(
      title = "Facility Daily Consultation Distribution",
      subtitle = "Distribution of average daily consultations per facility",
      x = "Average Daily Consultations",
      y = "Number of Facilities"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 10, face = "bold")
    )
}

#' Create service accessibility trends line chart
#' 
#' Shows consultations per facility over time by governorate
#' @param data data.frame. Health register data
#' @param time_col Character. Time column (default "month2")
#' @param admin_col Character. Administrative column name (default "admin1")
#' @param orgunit_col Character. Organization unit column (default "orgunit")
#' @param top_admin Integer. Number of top governorates to show (default NULL for all)
#' @return ggplot object
#' @export
create_accessibility_trends_chart <- function(data,
                                              time_col = "month2",
                                              admin_col = "admin1",
                                              orgunit_col = "orgunit",
                                              top_admin = NULL) {
  
  # Check required columns
  required_cols <- c(time_col, admin_col, orgunit_col)
  if (!all(required_cols %in% colnames(data))) {
    stop("Required columns not available: ", paste(required_cols, collapse = ", "))
  }
  
  # Filter to top governorates if specified
  plot_data <- data
  if (!is.null(top_admin) && is.numeric(top_admin)) {
    top_admin_levels <- data %>%
      dplyr::count(.data[[admin_col]], sort = TRUE) %>%
      dplyr::slice_head(n = top_admin) %>%
      dplyr::pull(.data[[admin_col]])
    
    plot_data <- data %>%
      dplyr::filter(.data[[admin_col]] %in% top_admin_levels)
  }
  
  # Calculate accessibility trends
  accessibility_trends <- plot_data %>%
    dplyr::filter(!is.na(.data[[time_col]]), !is.na(.data[[admin_col]])) %>%
    dplyr::group_by(.data[[time_col]], .data[[admin_col]]) %>%
    dplyr::summarise(
      consultations = dplyr::n(),
      facilities = dplyr::n_distinct(.data[[orgunit_col]], na.rm = TRUE),
      consultations_per_facility = consultations / pmax(facilities, 1),
      .groups = "drop"
    )
  
  if (nrow(accessibility_trends) == 0) {
    stop("No accessibility trend data available.")
  }
  
  # Create line chart
  ggplot(accessibility_trends, aes(x = .data[[time_col]], y = consultations_per_facility, color = .data[[admin_col]])) +
    geom_line(size = 1.2, alpha = 0.8) +
    geom_point(size = 2) +
    labs(
      title = "Service Accessibility Trends",
      subtitle = "Average consultations per facility by region",
      x = "Month",
      y = "Consultations per Facility",
      color = "Governorate"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    guides(color = guide_legend(nrow = 2))
}

#' Create combined health system performance metrics
#' 
#' Combines facility utilization histogram and accessibility trends
#' @param data data.frame. Health register data
#' @param layout Character. "vertical" or "horizontal" (default "vertical")
#' @param ... Additional arguments passed to individual chart functions
#' @return Combined plot object (patchwork) or list of plots
#' @export
create_health_system_performance_charts <- function(data, layout = "vertical", ...) {
  
  plots <- list()
  
  # Create facility utilization histogram
  tryCatch({
    plots$utilization <- create_facility_utilization_histogram(data, ...)
  }, error = function(e) {
    warning("Could not create facility utilization histogram: ", e$message)
    plots$utilization <<- NULL
  })
  
  # Create accessibility trends chart
  tryCatch({
    plots$accessibility <- create_accessibility_trends_chart(data, ...)
  }, error = function(e) {
    warning("Could not create accessibility trends chart: ", e$message)
    plots$accessibility <<- NULL
  })
  
  # Filter out NULL plots
  plots <- plots[!sapply(plots, is.null)]
  
  if (length(plots) == 0) {
    stop("No performance plots could be created with the available data.")
  }
  
  # Try to combine with patchwork if available
  if (requireNamespace("patchwork", quietly = TRUE) && length(plots) > 1) {
    library(patchwork)
    
    if (layout == "vertical") {
      return(plots$utilization / plots$accessibility + patchwork::plot_layout(heights = c(1, 1)))
    } else {
      return(plots$utilization | plots$accessibility)
    }
  } else {
    # Return individual plots if patchwork not available or only one plot
    if (length(plots) == 1) {
      return(plots[[1]])
    } else {
      return(plots)
    }
  }
}

#' Safe null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

#' Create enhanced empty register
#' 
#' @return data.frame. Empty register with standard columns
#' 
create_empty_register <- function() {
  data.frame(
    orgunit = character(),
    datevisit = as.Date(character()),
    morbidity = character(),
    sex = character(),
    age_group_new = character(),
    admin1 = character(),
    stringsAsFactors = FALSE
  )
}

#' Basic theme for reports (if not already defined)
#' 
theme_report <- function() {
  if (exists("theme_ai_report")) {
    return(theme_ai_report())
  }
  
  theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 11),
      legend.title = element_text(size = 11),
      strip.text = element_text(size = 10, face = "bold")
    )
}

cat("✅ Remaining missing functions loaded for epi_report.Rmd compatibility\n")

# Export functions for use in reports
log_info("Visualization helpers loaded successfully")
log_info("Available functions: plot_consultations_over_time, plot_geographic_distribution, plot_disease_distribution, plot_time_series_by_category, plot_age_sex_pyramid")
#' Epidemic surveillance table using new taxonomy system
#' @param register Register data with epidemic_group taxonomy flags
#' @param period_days Number of days for recent period
#' @param min_cases Minimum cases to include disease
#' @return flextable object
epidemic_surveillance_table_taxonomy <- function(register, period_days = 30, min_cases = 1) {
  # Identify columns
  date_col <- dplyr::case_when(
    "datevisit" %in% names(register) ~ "datevisit",
    "eventdate" %in% names(register) ~ "eventdate", 
    "date_visit" %in% names(register) ~ "date_visit",
    TRUE ~ NA_character_
  )
  admin_col <- dplyr::case_when(
    "admin1" %in% names(register) ~ "admin1",
    "governorate" %in% names(register) ~ "governorate",
    "region" %in% names(register) ~ "region",
    TRUE ~ NA_character_
  )
  
  if (is.na(date_col)) {
    return(flextable::flextable(tibble::tibble(
      `Disease` = "Date column not found", 
      `Recent Cases` = NA, `Regions Affected` = NA, `Most Recent Case` = NA
    )) %>% flextable::autofit())
  }
  
  # Filter to recent period and epidemic-prone diseases
  recent_data <- register %>%
    dplyr::mutate(date_parsed = .to_date_safe(.data[[date_col]])) %>%
    dplyr::filter(!is.na(date_parsed),
                  date_parsed >= (max(date_parsed, na.rm = TRUE) - period_days),
                  !is.na(epidemic_group))
  
  if (nrow(recent_data) == 0) {
    return(flextable::flextable(tibble::tibble(
      `Disease` = "No epidemic-prone diseases detected",
      `Recent Cases` = NA, `Regions Affected` = NA, `Most Recent Case` = NA
    )) %>% flextable::autofit())
  }
  
  # Create detailed summary with both epidemic groups and specific diseases
  group_summary <- recent_data %>%
    dplyr::group_by(epidemic_group) %>%
    dplyr::summarise(
      total_cases = n(),
      regions = if (!is.na(admin_col)) dplyr::n_distinct(.data[[admin_col]], na.rm = TRUE) else NA_integer_,
      most_recent = as.character(max(date_parsed, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::filter(total_cases >= min_cases) %>%
    dplyr::arrange(dplyr::desc(total_cases))
  
  # Create detailed rows showing groups and specific diseases
  rows <- list()
  
  for (i in seq_len(nrow(group_summary))) {
    group_name <- group_summary$epidemic_group[i]
    group_cases <- group_summary$total_cases[i]
    group_regions <- group_summary$regions[i]
    group_recent <- group_summary$most_recent[i]
    
    # Add group header row
    rows <- append(rows, list(tibble::tibble(
      `Disease` = paste0("**", group_name, "** (", group_cases, " total cases)"),
      `Recent Cases` = "",
      `Regions Affected` = "",
      `Most Recent Case` = ""
    )))
    
    # Get specific diseases within this group
    disease_details <- recent_data %>%
      dplyr::filter(epidemic_group == group_name) %>%
      dplyr::group_by(canonical_disease_imc) %>%
      dplyr::summarise(
        cases = n(),
        regions = if (!is.na(admin_col)) dplyr::n_distinct(.data[[admin_col]], na.rm = TRUE) else NA_integer_,
        most_recent = as.character(max(date_parsed, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(cases)) %>%
      dplyr::mutate(
        `Disease` = paste0("  • ", canonical_disease_imc),
        `Recent Cases` = as.character(cases),
        `Regions Affected` = if (!is.na(admin_col)) as.character(regions) else "",
        `Most Recent Case` = most_recent
      ) %>%
      dplyr::select(`Disease`, `Recent Cases`, `Regions Affected`, `Most Recent Case`)
    
    # Add disease detail rows
    for (j in seq_len(nrow(disease_details))) {
      rows <- append(rows, list(disease_details[j, ]))
    }
    
    # Add empty separator row except for the last group
    if (i < nrow(group_summary)) {
      rows <- append(rows, list(tibble::tibble(
        `Disease` = "",
        `Recent Cases` = "",
        `Regions Affected` = "",
        `Most Recent Case` = ""
      )))
    }
  }
  
  # Combine all rows
  if (length(rows) > 0) {
    rows <- dplyr::bind_rows(rows)
  } else {
    rows <- tibble::tibble(
      `Disease` = character(0),
      `Recent Cases` = character(0),
      `Regions Affected` = character(0),
      `Most Recent Case` = character(0)
    )
  }
  
  if (nrow(rows) == 0) {
    return(flextable::flextable(tibble::tibble(
      `Disease` = "No epidemic-prone diseases detected",
      `Recent Cases` = NA, `Regions Affected` = NA, `Most Recent Case` = NA
    )) %>% flextable::autofit())
  }
  
  # Format as flextable
  ft <- flextable::flextable(rows) %>%
    flextable::autofit() %>%
    flextable::theme_vanilla() %>%
    flextable::bold(part = "header") %>%
    flextable::set_caption(paste0("Epidemic Surveillance Summary (last ", period_days, " days)"))
  
  # Apply formatting for group headers and disease items
  for (i in seq_len(nrow(rows))) {
    disease_name <- rows$Disease[i]
    
    # Format group headers (contain ** markdown)
    if (grepl("^\\*\\*.*\\*\\*", disease_name)) {
      ft <- flextable::bold(ft, i = i, j = "Disease")
      ft <- flextable::bg(ft, i = i, bg = "#f0f0f0")
      # Remove markdown formatting for display
      ft <- flextable::compose(ft, i = i, j = "Disease", 
                               value = flextable::as_paragraph(gsub("\\*\\*(.*?)\\*\\*", "\\1", disease_name)))
    }
    
    # Add conditional formatting for case counts (only for disease rows with actual numbers)
    recent_cases <- rows$`Recent Cases`[i]
    if (!is.na(recent_cases) && nzchar(recent_cases) && grepl("^\\d+$", recent_cases)) {
      case_count <- as.numeric(recent_cases)
      if (case_count >= 10) {
        ft <- flextable::color(ft, i = i, j = "Recent Cases", color = "red")
        ft <- flextable::bold(ft, i = i, j = "Recent Cases")
      } else if (case_count >= 3) {
        ft <- flextable::color(ft, i = i, j = "Recent Cases", color = "orange")
      }
    }
  }
  
  ft
}

#' Plot facility burden by disease severity distribution
#' @param data Register data with disease taxonomy and facility/admin2 information
#' @param admin_level Character. "facility" or "admin2" (default: "facility")
#' @param period_days Number of days for recent period (default: 60)
#' @return ggplot object
plot_facility_burden_severity <- function(data, admin_level = "facility", period_days = 60) {
  # Identify columns
  date_col <- dplyr::case_when(
    "datevisit" %in% names(data) ~ "datevisit",
    "eventdate" %in% names(data) ~ "eventdate",
    TRUE ~ "datevisit"
  )
  
  admin_col <- if (admin_level == "admin2") {
    dplyr::case_when(
      "admin2" %in% names(data) ~ "admin2",
      "district" %in% names(data) ~ "district",
      "sub_district" %in% names(data) ~ "sub_district",
      TRUE ~ "admin2"
    )
  } else {
    dplyr::case_when(
      "orgunit" %in% names(data) ~ "orgunit",
      "facility" %in% names(data) ~ "facility",
      "facility_name" %in% names(data) ~ "facility_name",
      TRUE ~ "orgunit"
    )
  }
  
  if (!admin_col %in% names(data)) {
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, 
                   label = paste(admin_level, "information not available"), 
                   size = 4) + theme_void())
  }
  
  # Filter recent data and create severity categories
  recent_data <- data %>%
    dplyr::mutate(date_parsed = .to_date_safe(.data[[date_col]])) %>%
    dplyr::filter(!is.na(date_parsed),
                  date_parsed >= (max(date_parsed, na.rm = TRUE) - period_days)) %>%
    dplyr::mutate(
      severity = dplyr::case_when(
        (if("trauma_related" %in% names(.)) trauma_related else FALSE) %||% FALSE ~ "Critical (Trauma)",
        (if("vaccine_preventable" %in% names(.)) vaccine_preventable else FALSE) %||% 
        (if("outbreak_prone" %in% names(.)) outbreak_prone else FALSE) %||%
        (if("epidemic_prone" %in% names(.)) epidemic_prone else FALSE) %||% FALSE ~ "High (Epidemic Risk)",
        (if("chronic_condition" %in% names(.)) chronic_condition else FALSE) %||% 
        (if("ncd_related" %in% names(.)) ncd_related else FALSE) %||% FALSE ~ "Moderate (Chronic Care)",
        TRUE ~ "Standard (Primary Care)"
      ),
      admin_unit = .data[[admin_col]]
    ) %>%
    dplyr::filter(!is.na(admin_unit))
  
  if (nrow(recent_data) == 0) {
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, label = "No recent data available", 
                   size = 4) + theme_void())
  }
  
  # Calculate burden by admin unit and severity
  burden_data <- recent_data %>%
    dplyr::group_by(admin_unit, severity) %>%
    dplyr::summarise(cases = n(), .groups = "drop") %>%
    dplyr::group_by(admin_unit) %>%
    dplyr::mutate(
      total_cases = sum(cases),
      percentage = round(cases / total_cases * 100, 1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(total_cases >= 5) %>%  # Only show units with meaningful volume
    dplyr::arrange(dplyr::desc(total_cases))
  
  # Take top facilities/districts
  top_units <- burden_data %>%
    dplyr::distinct(admin_unit, total_cases) %>%
    dplyr::slice_head(n = 15) %>%
    dplyr::pull(admin_unit)
  
  plot_data <- burden_data %>%
    dplyr::filter(admin_unit %in% top_units)
  
  # Create stacked bar chart
  severity_colors <- c(
    "Critical (Trauma)" = "#d62728",
    "High (Epidemic Risk)" = "#ff7f0e", 
    "Moderate (Chronic Care)" = "#2ca02c",
    "Standard (Primary Care)" = "#1f77b4"
  )
  
  unit_label <- if (admin_level == "admin2") "District" else "Facility"
  
  ggplot(plot_data, aes(x = reorder(admin_unit, total_cases), y = cases, fill = severity)) +
    geom_col(alpha = 0.8) +
    geom_text(aes(label = ifelse(percentage >= 8, paste0(percentage, "%"), "")), 
              position = position_stack(vjust = 0.5), size = 2.5, color = "white", fontface = "bold") +
    scale_fill_manual(values = severity_colors, name = "Disease Severity") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
    labs(
      title = paste("Healthcare Burden by Disease Severity -", unit_label, "Level"),
      subtitle = paste("Case distribution by severity category (last", period_days, "days)"),
      x = unit_label,
      y = "Number of Consultations",
      caption = "Only showing units with ≥5 total cases"
    ) +
    theme_ai_report() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.major.y = element_line(color = "grey90")
    ) +
    guides(fill = guide_legend(nrow = 2)) +
    coord_flip()
}

#' Plot high-risk groups consultation burden
#' @param data Register data with demographic and taxonomy information
#' @param period_days Number of days for analysis period (default: 30)
#' @return ggplot object
plot_high_risk_groups_burden <- function(data, period_days = 30) {
  # Identify columns
  date_col <- dplyr::case_when(
    "datevisit" %in% names(data) ~ "datevisit",
    "eventdate" %in% names(data) ~ "eventdate",
    TRUE ~ "datevisit"
  )
  
  age_col <- dplyr::case_when(
    "age" %in% names(data) ~ "age",
    "ageyears" %in% names(data) ~ "ageyears",
    "age_years" %in% names(data) ~ "age_years",
    TRUE ~ "age"
  )
  
  sex_col <- dplyr::case_when(
    "sex" %in% names(data) ~ "sex",
    "gender" %in% names(data) ~ "gender",
    TRUE ~ "sex"
  )
  
  # Filter recent data and define risk groups
  recent_data <- data %>%
    dplyr::mutate(date_parsed = .to_date_safe(.data[[date_col]])) %>%
    dplyr::filter(!is.na(date_parsed),
                  date_parsed >= (max(date_parsed, na.rm = TRUE) - period_days))
  
  if (nrow(recent_data) == 0) {
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, label = "No recent data available", 
                   size = 4) + theme_void())
  }
  
  # Calculate high-risk group classifications
  risk_data <- recent_data %>%
    dplyr::mutate(
      age_value = if (age_col %in% names(.)) as.numeric(.data[[age_col]]) else NA_real_,
      sex_value = if (sex_col %in% names(.)) .data[[sex_col]] else NA_character_,
      # Define risk groups
      under5 = (!is.na(age_value) & age_value < 5) |
                (if("high_risk_under5" %in% names(.)) high_risk_under5 else FALSE) %||% FALSE |
                (if("pediatric_case" %in% names(.)) pediatric_case else FALSE) %||% FALSE,
      elderly = (!is.na(age_value) & age_value >= 65) |
                (if("high_risk_elderly" %in% names(.)) high_risk_elderly else FALSE) %||% FALSE |
                (if("elderly_care_case" %in% names(.)) elderly_care_case else FALSE) %||% FALSE,
      pregnant_women = !is.na(sex_value) & tolower(sex_value) %in% c("f", "female") & 
                      ((if("pregnancy_related" %in% names(.)) pregnancy_related else FALSE) %||% FALSE | 
                       (if("maternal_health" %in% names(.)) maternal_health else FALSE) %||% FALSE |
                       (if("maternal_health_case" %in% names(.)) maternal_health_case else FALSE) %||% FALSE |
                       (if("high_risk_pregnant" %in% names(.)) high_risk_pregnant else FALSE) %||% FALSE),
      malnutrition_cases = (if("malnutrition_complication" %in% names(.)) malnutrition_complication else FALSE) %||% FALSE | 
                          (if("malnutrition_category" %in% names(.)) (!is.na(malnutrition_category) & malnutrition_category != "Normal") else FALSE) |
                          (if("emergency_syndrome_case" %in% names(.)) emergency_syndrome_case else FALSE) %||% FALSE,
      trauma_cases = (if("trauma_related" %in% names(.)) trauma_related else FALSE) %||% FALSE |
                     (if("emergency_syndrome_case" %in% names(.)) emergency_syndrome_case else FALSE) %||% FALSE,
      chronic_disease = (if("chronic_condition" %in% names(.)) chronic_condition else FALSE) %||% FALSE | 
                        (if("ncd_related" %in% names(.)) ncd_related else FALSE) %||% FALSE,
      epidemic_risk = (if("vaccine_preventable" %in% names(.)) vaccine_preventable else FALSE) %||% FALSE | 
                      (if("outbreak_prone_case" %in% names(.)) outbreak_prone_case else FALSE) %||% FALSE |
                      (if("epidemic_prone" %in% names(.)) epidemic_prone else FALSE) %||% FALSE |
                      (if("vpd_case" %in% names(.)) vpd_case else FALSE) %||% FALSE
    )
  
  # Create summary by risk group
  risk_summary <- tibble::tibble(
    risk_group = c("Under 5 Years", "Elderly (65+)", "Pregnant Women", "Malnutrition", 
                  "Trauma Cases", "Chronic Disease", "Epidemic Risk"),
    cases = c(
      sum(risk_data$under5, na.rm = TRUE),
      sum(risk_data$elderly, na.rm = TRUE),
      sum(risk_data$pregnant_women, na.rm = TRUE),
      sum(risk_data$malnutrition_cases, na.rm = TRUE),
      sum(risk_data$trauma_cases, na.rm = TRUE),
      sum(risk_data$chronic_disease, na.rm = TRUE),
      sum(risk_data$epidemic_risk, na.rm = TRUE)
    ),
    percentage = round(cases / nrow(risk_data) * 100, 1),
    priority = c("High", "High", "High", "Critical", "Critical", "Moderate", "High")
  ) %>%
    dplyr::filter(cases > 0) %>%
    dplyr::arrange(dplyr::desc(cases))
  
  if (nrow(risk_summary) == 0) {
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, label = "No high-risk group data available", 
                   size = 4) + theme_void())
  }
  
  # Define colors by priority
  priority_colors <- c("Critical" = "#d62728", "High" = "#ff7f0e", "Moderate" = "#2ca02c")
  
  # Create horizontal bar chart
  ggplot(risk_summary, aes(x = reorder(risk_group, cases), y = cases, fill = priority)) +
    geom_col(alpha = 0.8, width = 0.7) +
    geom_text(aes(label = paste0(cases, " (", percentage, "%)")), 
              hjust = -0.1, size = 3.5, fontface = "bold") +
    scale_fill_manual(values = priority_colors, name = "Priority Level") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "High-Risk Groups Consultation Burden",
      subtitle = paste("Healthcare utilization by vulnerable populations (last", period_days, "days)"),
      x = "High-Risk Group",
      y = "Number of Consultations",
      caption = "Percentages shown are of total consultations in period"
    ) +
    theme_ai_report() +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    ) +
    coord_flip()
}

#' Epidemic surveillance summary table (AI helper)
#'
#' Generates a formatted table summarizing recent epidemic-prone disease activity.
#'
#' @param register data.frame. Health register data
#' @param period_days integer. Number of days to consider for "recent" cases (default: 30)
#' @param epidemic_diseases list. Output of get_epidemic_diseases_standardized(); if NULL, tries to call that function
#' @param min_cases integer. Minimum number of recent cases to display a row (default: 1)
#' @return flextable object (summary table)
epidemic_surveillance_table <- function(register, period_days = 30, epidemic_diseases = NULL, min_cases = 1) {
  # Check if we have epidemic taxonomy flags in the data (new system)
  if ("epidemic_group" %in% names(register) || "epidemic_groups_flag" %in% names(register)) {
    return(epidemic_surveillance_table_taxonomy(register, period_days, min_cases))
  }
  
  # Fallback to old system
  if (is.null(epidemic_diseases)) {
    if (exists("get_epidemic_diseases_standardized", mode = "function")) {
      epidemic_diseases <- get_epidemic_diseases_standardized()
    } else if (exists("get_epidemic_diseases", mode = "function")) {
      epidemic_diseases <- list("General" = get_epidemic_diseases())
    } else {
      # Final fallback with common epidemic diseases
      epidemic_diseases <- list(
        "Respiratory_Epidemic" = c("Pneumonia", "Influenza-Like Illness", "COVID-19", "Tuberculosis"),
        "Water_Food_Borne" = c("Acute Watery Diarrhea", "Suspected Cholera", "Typhoid Fever"),
        "Vaccine_Preventable" = c("Measles", "Suspected Measles", "Diphtheria", "Suspected Diphtheria")
      )
    }
  }
  # Pick date and disease columns
  date_col <- dplyr::case_when(
    "datevisit" %in% names(register) ~ "datevisit",
    "eventdate" %in% names(register) ~ "eventdate",
    "date_visit" %in% names(register) ~ "date_visit",
    TRUE ~ NA_character_
  )
  disease_col <- dplyr::case_when(
    "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
    "morbidity" %in% names(register) ~ "morbidity",
    "icd11_title" %in% names(register) ~ "icd11_title",
    "category_canonical_disease_imc" %in% names(register) ~ "category_canonical_disease_imc",
    TRUE ~ NA_character_
  )
  admin_col <- dplyr::case_when(
    "admin1" %in% names(register) ~ "admin1",
    "governorate" %in% names(register) ~ "governorate",
    "region" %in% names(register) ~ "region",
    TRUE ~ NA_character_
  )
  if (is.na(date_col) || is.na(disease_col)) {
    stop("Required columns (date/disease) not found in register.")
  }
  # Restrict to recent period
  latest_date <- suppressWarnings(max(register[[date_col]], na.rm = TRUE))
  if (is.infinite(latest_date) || is.na(latest_date)) latest_date <- Sys.Date()
  cutoff <- as.Date(latest_date) - period_days
  recent <- register %>%
    dplyr::filter(!is.na(.data[[date_col]]), .data[[date_col]] >= cutoff)
  # Build summary table
  rows <- purrr::map_dfr(names(epidemic_diseases), function(disease_name) {
    these_conditions <- epidemic_diseases[[disease_name]]
    df <- recent %>% dplyr::filter(.data[[disease_col]] %in% these_conditions)
    n_cases <- nrow(df)
    if (n_cases < min_cases) return(NULL)
    n_regions <- if (!is.na(admin_col)) dplyr::n_distinct(df[[admin_col]], na.rm = TRUE) else NA_integer_
    peak_date <- if (n_cases > 0) as.character(max(df[[date_col]], na.rm = TRUE)) else NA_character_
    tibble::tibble(
      `Disease` = disease_name,
      `Recent Cases` = n_cases,
      `Regions Affected` = n_regions,
      `Most Recent Case` = peak_date
    )
  })
  if (nrow(rows) == 0) {
    return(flextable::flextable(tibble::tibble(
      `Disease` = "No epidemic-prone diseases detected",
      `Recent Cases` = NA, `Regions Affected` = NA, `Most Recent Case` = NA
    )) %>%
      flextable::autofit() %>%
      flextable::set_caption("Epidemic Surveillance Summary") %>%
      flextable::bold(part = "header"))
  }
  # Format as flextable
  ft <- flextable::flextable(rows) %>%
    flextable::autofit() %>%
    flextable::theme_vanilla() %>%
    flextable::bold(part = "header") %>%
    flextable::set_caption(paste0("Epidemic Surveillance Summary (last ", period_days, " days)"))
  # For markdown rendering, return a simple data frame instead of flextable to avoid issues
  if (exists(".knitr") && !is.null(getOption("knitr.in.progress"))) {
    # In R Markdown context, return kable
    return(knitr::kable(rows, format = "markdown", 
                       caption = paste0("Epidemic Surveillance Summary (last ", period_days, " days)")))
  }
  
  # Otherwise return flextable with basic formatting
  return(flextable::flextable(rows) %>%
    flextable::autofit() %>%
    flextable::bold(part = "header") %>%
    flextable::set_caption(paste0("Epidemic Surveillance Summary (last ", period_days, " days)")))
}

#' Plot epidemic disease category trends over time using new taxonomy flags
#' @param data Register data with epidemic_group taxonomy flags
#' @param date_col Date column name
#' @param period_days Number of days to include (default: 90)
#' @return ggplot object
plot_epidemic_category_trends <- function(data, date_col = "datevisit", period_days = 90) {
  # Validate inputs
  if (!date_col %in% names(data)) {
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, label = paste("Date column", date_col, "not found"), 
                   size = 4) +
           theme_void())
  }
  
  if (!"epidemic_group" %in% names(data)) {
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, label = "Epidemic group taxonomy not available", 
                   size = 4) +
           theme_void())
  }
  
  # Filter to recent period and epidemic-prone diseases only
  recent_data <- data %>%
    dplyr::mutate(date_parsed = .to_date_safe(.data[[date_col]])) %>%
    dplyr::filter(!is.na(date_parsed),
                  date_parsed >= (max(date_parsed, na.rm = TRUE) - period_days),
                  !is.na(epidemic_group)) %>%
    dplyr::mutate(week_start = floor_date(date_parsed, "week"))
  
  if (nrow(recent_data) == 0) {
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, label = "No epidemic-prone diseases in recent period", 
                   size = 4) +
           theme_void())
  }
  
  # Aggregate by week and epidemic category
  trend_data <- recent_data %>%
    dplyr::group_by(week_start, epidemic_group) %>%
    dplyr::summarise(cases = n(), .groups = "drop") %>%
    tidyr::complete(week_start, epidemic_group, fill = list(cases = 0))
  
  # Create trend plot
  ggplot(trend_data, aes(x = week_start, y = cases, color = epidemic_group, fill = epidemic_group)) +
    geom_line(size = 1.2, alpha = 0.8) +
    geom_point(size = 2.5, alpha = 0.9) +
    geom_area(alpha = 0.2) +
    scale_color_viridis_d(name = "Epidemic Category", option = "plasma") +
    scale_fill_viridis_d(name = "Epidemic Category", option = "plasma") +
    scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
    labs(
      title = "Epidemic Disease Category Trends",
      subtitle = paste("Weekly case counts over last", period_days, "days"),
      x = "Week",
      y = "Number of Cases",
      caption = "Data: WHO-standardized epidemic disease categories"
    ) +
    theme_ai_report() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    facet_wrap(~ epidemic_group, scales = "free_y", ncol = 2)
}

#' Create priority disease alert dashboard using taxonomy flags
#' @param data Register data with taxonomy flags
#' @param date_col Date column name  
#' @param period_days Number of days for recent period (default: 30)
#' @return ggplot object
plot_priority_disease_alerts <- function(data, date_col = "datevisit", period_days = 30) {
  # Check required columns
  required_flags <- c("vaccine_preventable", "climate_sensitive", "outbreak_prone_case", "trauma_related")
  missing_flags <- setdiff(required_flags, names(data))
  
  if (length(missing_flags) > 0) {
    # Try alternate flag names that might exist
    alt_mapping <- c(
      "vaccine_preventable" = "is_vaccine_preventable", 
      "climate_sensitive" = "climate_sensitive_case",
      "outbreak_prone_case" = "epidemic_prone", 
      "trauma_related" = "trauma_related"
    )
    
    for (flag in missing_flags) {
      if (alt_mapping[[flag]] %in% names(data)) {
        data[[flag]] <- data[[alt_mapping[[flag]]]]
      }
    }
  }
  
  if (!date_col %in% names(data)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Date column not found") + theme_void())
  }
  
  # Filter to recent period
  recent_data <- data %>%
    dplyr::mutate(date_parsed = .to_date_safe(.data[[date_col]])) %>%
    dplyr::filter(!is.na(date_parsed),
                  date_parsed >= (max(date_parsed, na.rm = TRUE) - period_days))
  
  if (nrow(recent_data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data in recent period") + theme_void())
  }
  
  # Calculate alert metrics
  alert_summary <- tibble::tibble(
    alert_type = c("Vaccine Preventable", "Climate Sensitive", "Outbreak Prone", "Trauma Related"),
    flag_col = c("vaccine_preventable", "climate_sensitive", "outbreak_prone_case", "trauma_related"),
    cases = c(
      sum(recent_data$vaccine_preventable %||% FALSE, na.rm = TRUE),
      sum(recent_data$climate_sensitive %||% FALSE, na.rm = TRUE), 
      sum(recent_data$outbreak_prone_case %||% recent_data$epidemic_prone %||% FALSE, na.rm = TRUE),
      sum(recent_data$trauma_related %||% FALSE, na.rm = TRUE)
    ),
    percentage = round(cases / nrow(recent_data) * 100, 1),
    alert_level = case_when(
      cases >= 10 ~ "High",
      cases >= 3 ~ "Medium", 
      cases > 0 ~ "Low",
      TRUE ~ "None"
    )
  ) %>%
    dplyr::arrange(desc(cases))
  
  # Create alert level colors
  alert_colors <- c("High" = "#d62728", "Medium" = "#ff7f0e", "Low" = "#2ca02c", "None" = "#cccccc")
  
  # Create dashboard plot
  ggplot(alert_summary, aes(x = reorder(alert_type, cases), y = cases, fill = alert_level)) +
    geom_col(width = 0.7, alpha = 0.8) +
    geom_text(aes(label = paste0(cases, " (", percentage, "%)")), 
              hjust = -0.1, size = 3.5, fontface = "bold") +
    scale_fill_manual(values = alert_colors, name = "Alert Level") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Priority Disease Alert Dashboard", 
      subtitle = paste("Cases in last", period_days, "days by surveillance priority"),
      x = "Priority Disease Category",
      y = "Number of Cases",
      caption = "Alert levels: High (≥10), Medium (3-9), Low (1-2), None (0)"
    ) +
    theme_ai_report() +
    theme(
      legend.position = "right",
      panel.grid.major.y = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 10)
    ) +
    coord_flip()
}

#' Plot malnutrition trends with improved detection
#' @param data Register data
#' @param period Period column or date aggregation level
#' @return ggplot object
#' @export
plot_malnutrition_trends <- function(data, period = "month2") {
  cat("🔍 Malnutrition plot debug:\n")
  cat("   - Input data rows:", nrow(data), "\n")
  cat("   - Period:", period, "\n")
  
  # Check for period column
  if (!period %in% names(data)) {
    cat("   - Period column:", period, "NULL \n")
    # Try to create from date column
    date_col <- case_when(
      "datevisit" %in% names(data) ~ "datevisit",
      "eventdate" %in% names(data) ~ "eventdate", 
      TRUE ~ NA_character_
    )
    if (!is.na(date_col)) {
      cat("   - Date column:", date_col, "\n")
      data <- data %>%
        mutate(!!period := floor_date(.to_date_safe(.data[[date_col]]), "month"))
    }
  } else {
    cat("   - Period column:", period, "found\n")
  }
  
  # Check for disease column
  disease_col <- case_when(
    "canonical_disease_imc" %in% names(data) ~ "canonical_disease_imc",
    "morbidity" %in% names(data) ~ "morbidity",
    "icd11_title" %in% names(data) ~ "icd11_title",
    "category_canonical_disease_imc" %in% names(data) ~ "category_canonical_disease_imc",
    TRUE ~ NA_character_
  )
  cat("   - Disease column:", disease_col, "\n")
  
  # Improved malnutrition detection using multiple methods
  malnutrition_labels <- c(
    "malnutrition", "severe acute malnutrition", "moderate acute malnutrition", 
    "kwashiorkor", "marasmus", "nutritional deficiency", "protein energy malnutrition",
    "sam", "mam", "pem", "underweight", "wasting", "stunting"
  )
  
  cat("   - Using expanded malnutrition patterns\n")
  
  # Check for malnutrition flag column first
  malnut_col <- case_when(
    "malnutrition_flag" %in% names(data) ~ "malnutrition_flag",
    "is_malnutrition" %in% names(data) ~ "is_malnutrition",
    "malnutrition_complication" %in% names(data) ~ "malnutrition_complication",
    TRUE ~ NA_character_
  )
  
  use_flag <- !is.na(malnut_col)
  cat("   - Using malnutrition flag:", use_flag, "\n")
  
  if (is.na(disease_col) && !use_flag) {
    cat("❌ No disease column or malnutrition flag available\n")
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Malnutrition data not identifiable") + theme_void())
  }
  
  if (!period %in% names(data)) {
    cat("❌ Period column could not be created\n")
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Date information not available") + theme_void())
  }
  
  # Filter and prepare data
  malnut_data <- data %>%
    filter(!is.na(.data[[period]]))
  
  cat("   - Valid periods:", nrow(malnut_data), "\n")
  
  if (use_flag) {
    # Use flag-based detection
    malnut_data <- malnut_data %>%
      filter(.data[[malnut_col]] == TRUE | .data[[malnut_col]] == 1 | .data[[malnut_col]] == "yes")
  } else if (!is.na(disease_col)) {
    # Use pattern-based detection with both exact matches and partial matches
    malnut_data <- malnut_data %>%
      filter(
        tolower(.data[[disease_col]]) %in% tolower(malnutrition_labels) |
        grepl("malnutr|kwashior|marasmus|undernourish|wast|stunt", tolower(.data[[disease_col]]))
      )
  }
  
  cat("   - Malnutrition cases after filtering:", nrow(malnut_data), "\n")
  
  if (nrow(malnut_data) == 0) {
    cat("❌ No malnutrition cases found for plotting\n")
    
    # Show sample of diseases to help debug
    if (!is.na(disease_col) && nrow(data) > 0) {
      sample_diseases <- data %>%
        count(.data[[disease_col]], sort = TRUE) %>%
        slice_head(n = 10) %>%
        pull(.data[[disease_col]])
      cat("   - Sample diseases in data:", paste(sample_diseases[1:min(5, length(sample_diseases))], collapse = ", "), "\n")
    }
    
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, 
                   label = paste("No malnutrition cases found in dataset\nChecked", nrow(data), "total records"), 
                   size = 4, hjust = 0.5) + 
           theme_void())
  }
  
  # Aggregate by period
  trend_data <- malnut_data %>%
    count(.data[[period]], name = "cases") %>%
    arrange(.data[[period]])
  
  # Create trend plot
  ggplot(trend_data, aes(x = .data[[period]], y = cases)) +
    geom_line(size = 1.2, color = "#d62728") +
    geom_point(size = 3, color = "#d62728") +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.3, color = "#ff7f0e") +
    labs(
      title = "Malnutrition Case Trends",
      subtitle = paste("Total cases:", sum(trend_data$cases), "| Periods:", nrow(trend_data)),
      x = "Period",
      y = "Number of Cases"
    ) +
    scale_y_continuous(labels = comma_format()) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# ==============================================================================
# GEOGRAPHIC TREEMAP VISUALIZATION FUNCTION
# ==============================================================================

#' Create Geographic Disease Burden Treemap
#' 
#' Creates an interactive treemap showing disease distribution across geographic regions
#' with improved color scheme and readability
#' 
#' @param data Health register data frame
#' @param disease_col Name of disease column (default: auto-detected)
#' @param region_col Name of region/geographic column (default: auto-detected)
#' @param title Plot title
#' @param color_scheme Color palette: "viridis", "plasma", "cividis", "accessible", or "custom"
#' @param min_cases Minimum number of cases to include in treemap
#' @param max_diseases Maximum number of diseases to show (top diseases by count)
#' @return ggplot object or message if columns not available
#' 
create_geographic_disease_treemap <- function(data, 
                                             disease_col = NULL,
                                             region_col = NULL,
                                             title = "Disease Distribution by Geographic Region",
                                             color_scheme = "accessible",
                                             min_cases = 5,
                                             max_diseases = 15) {
  
  # Check if treemapify is available
  if (!requireNamespace("treemapify", quietly = TRUE)) {
    message("treemapify package required but not installed. Run: install.packages('treemapify')")
    return(NULL)
  }
  
  # Auto-detect disease column if not provided
  if (is.null(disease_col)) {
    disease_candidates <- c("canonical_disease_imc", "morbidity", "icd11_title", "disease", "diagnosis")
    disease_col <- NA_character_
    for (col in disease_candidates) {
      if (col %in% names(data)) {
        disease_col <- col
        break
      }
    }
  }
  
  # Auto-detect region column if not provided
  if (is.null(region_col)) {
    region_candidates <- c("admin1", "governorate", "region", "admin2", "district")
    region_col <- NA_character_
    for (col in region_candidates) {
      if (col %in% names(data)) {
        region_col <- col
        break
      }
    }
  }
  
  # Check if required columns are available
  if (is.na(disease_col) || is.na(region_col)) {
    message("Required columns not found: disease_col=", disease_col, ", region_col=", region_col)
    return(NULL)
  }
  
  if (!disease_col %in% names(data) || !region_col %in% names(data)) {
    message("Specified columns not found in data")
    return(NULL)
  }
  
  # Prepare data
  tryCatch({
    # Filter and aggregate data
    df_treemap <- data %>%
      dplyr::filter(!is.na(.data[[disease_col]]), !is.na(.data[[region_col]])) %>%
      dplyr::count(.data[[region_col]], .data[[disease_col]], name = "cases") %>%
      dplyr::filter(cases >= min_cases) %>%
      # Keep only top diseases by total count
      dplyr::group_by(.data[[disease_col]]) %>%
      dplyr::mutate(disease_total = sum(cases)) %>%
      dplyr::ungroup() %>%
      dplyr::slice_max(disease_total, n = max_diseases * 10) %>%  # Buffer for filtering
      dplyr::select(-disease_total) %>%
      # Clean up long disease names
      dplyr::mutate(
        disease_short = stringr::str_trunc(.data[[disease_col]], 25),
        region_short = stringr::str_trunc(.data[[region_col]], 15)
      )
    
    if (nrow(df_treemap) == 0) {
      message("No data available for treemap after filtering")
      return(NULL)
    }
    
    # Define color palettes
    color_palettes <- list(
      "accessible" = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                      "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                      "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5"),
      "viridis" = viridis::viridis(15),
      "plasma" = viridis::plasma(15),
      "cividis" = viridis::cividis(15),
      "custom" = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#592E83",
                   "#6A994E", "#BC4749", "#F2B705", "#386641", "#A4161A",
                   "#4EA5D9", "#8B5A2B", "#06FFA5", "#FFE66D", "#FF9F1C")
    )
    
    selected_colors <- color_palettes[[color_scheme]]
    if (is.null(selected_colors)) {
      selected_colors <- color_palettes[["accessible"]]
    }
    
    # Create treemap
    p <- ggplot(df_treemap, aes(
      area = cases,
      fill = cases,
      label = disease_short,
      subgroup = region_short
    )) +
      treemapify::geom_treemap(color = "white", size = 1.5) +
      treemapify::geom_treemap_subgroup_border(
        colour = "#2c3e50", 
        size = 3, 
        alpha = 0.8
      ) +
      treemapify::geom_treemap_subgroup_text(
        place = "centre",
        grow = TRUE,
        alpha = 0.9,
        colour = "white",
        fontface = "bold",
        min.size = 8,
        reflow = FALSE
      ) +
      treemapify::geom_treemap_text(
        colour = "white",
        place = "centre",
        reflow = TRUE,
        min.size = 6,
        fontface = "bold",
        alpha = 0.9,
        grow = FALSE
      ) +
      scale_fill_viridis_c(
        option = "plasma",
        name = "Cases",
        labels = scales::comma_format(),
        trans = "sqrt",
        guide = guide_colorbar(
          barwidth = 1,
          barheight = 8,
          title.position = "top"
        )
      ) +
      labs(
        title = title,
        subtitle = paste("Showing", nrow(df_treemap), "disease-region combinations with ≥", min_cases, "cases"),
        caption = paste("Total consultations:", scales::comma(sum(df_treemap$cases)),
                       "| Regions:", length(unique(df_treemap[[region_col]])),
                       "| Diseases:", length(unique(df_treemap[[disease_col]])))
      ) +
      theme_void() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5)),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40", margin = margin(b = 10)),
        plot.caption = element_text(size = 10, hjust = 0.5, color = "grey50", margin = margin(t = 10)),
        legend.position = "right",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(20, 20, 20, 20)
      )
    
    return(p)
    
  }, error = function(e) {
    message("Error creating treemap: ", e$message)
    return(NULL)
  })
}

#' Create simplified geographic treemap with preset options
#' 
#' @param data Health register data
#' @param title Optional custom title
#' @return ggplot treemap or NULL if error
#' 
create_simple_disease_treemap <- function(data, title = NULL) {
  if (is.null(title)) {
    title <- "Disease Category Distribution by Governorate"
  }
  
  create_geographic_disease_treemap(
    data = data,
    title = title,
    color_scheme = "accessible",
    min_cases = 3,
    max_diseases = 12
  )
}

cat("✅ Enhanced malnutrition plotting function loaded\n")
cat("✅ Geographic disease treemap functions loaded\n")