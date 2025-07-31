# R/visualization_helpers.R
# Helper functions for creating visualizations

# Load required libraries
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(viridis)
  library(RColorBrewer)
  library(logger)
})

#' Get consultation counts for a specific morbidity over time
#' 
#' @param data data.frame. Register data  
#' @param morbidity Character. Specific morbidity condition
#' @return data.frame. Daily counts for the specified morbidity
#' 
get_morbidity_counts <- function(data, morbidity) {
  if (!"morbidity" %in% colnames(data) || !"datevisitnew" %in% colnames(data)) {
    stop("Required columns 'morbidity' and 'datevisitnew' not found")
  }
  
  data %>%
    filter(morbidity == !!morbidity) %>%
    group_by(datevisitnew) %>%
    summarise(count = n(), .groups = 'drop')
}

#' Get consultation counts for a specific morbidity by age group
#' 
#' @param data data.frame. Register data
#' @param morbidity Character. Specific morbidity condition
#' @return data.frame. Daily counts by age group for the specified morbidity
#' 
get_morbidity_counts_by_age <- function(data, morbidity) {
  if (!"morbidity" %in% colnames(data) || !"datevisitnew" %in% colnames(data) || 
      !"age_group_new" %in% colnames(data)) {
    stop("Required columns not found")
  }
  
  data %>%
    filter(morbidity == !!morbidity) %>%
    group_by(datevisitnew, age_group_new) %>%
    summarise(count = n(), .groups = 'drop')
}

#' Get calendar data for heatmap visualization
#' 
#' @param register data.frame. Register data
#' @param start_year Numeric. Starting year for filtering (default: 2021)
#' @return data.frame. Monthly consultation counts by year
#' 
get_calendar_data <- function(register, start_year = 2021) {
  if (!"year" %in% colnames(register) || !"month2" %in% colnames(register)) {
    stop("Required columns 'year' and 'month2' not found")
  }
  
  calendar_data <- register %>%
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
    target_year <- max(register$year, na.rm = TRUE)
  }
  
  if (!"disease_cat" %in% colnames(register)) {
    stop("Required column 'disease_cat' not found")
  }
  
  register_filtered <- register %>%
    filter(year == target_year) %>%
    group_by(month2, disease_cat) %>%
    summarize(consultations = n(), .groups = "drop")
  
  log_info("Generated register data for {target_year}: {nrow(register_filtered)} records")
  
  return(register_filtered)
}

#' Create consultation timeline plot
#' 
#' @param data data.frame. Register data
#' @param title Character. Plot title
#' @param date_col Character. Date column name (default: "datevisitnew")
#' @param binwidth Numeric. Histogram bin width in days (default: 30)
#' @return ggplot. Timeline plot
#' 
plot_consultations_over_time <- function(data, title = "Consultations Over Time", 
                                       date_col = "datevisitnew", binwidth = 30) {
  
  if (!date_col %in% colnames(data)) {
    stop(paste("Date column", date_col, "not found"))
  }
  
  ggplot(data, aes_string(x = date_col)) +
    geom_histogram(binwidth = binwidth, fill = "#2E86AB", color = "white", alpha = 0.8) +
    labs(
      title = title,
      x = "Date of Visit",
      y = "Number of Consultations"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")
}

#' Create disease category distribution plot
#' 
#' @param data data.frame. Register data
#' @param category_col Character. Disease category column name
#' @param top_n Numeric. Number of top categories to show (default: 10)
#' @return ggplot. Bar plot of disease categories
#' 
plot_disease_distribution <- function(data, category_col = "disease_cat", top_n = 10) {
  
  if (!category_col %in% colnames(data)) {
    stop(paste("Category column", category_col, "not found"))
  }
  
  # Get top categories
  top_categories <- data %>%
    count(.data[[category_col]], sort = TRUE) %>%
    slice_head(n = top_n) %>%
    pull(.data[[category_col]])
  
  # Filter and plot
  data %>%
    filter(.data[[category_col]] %in% top_categories) %>%
    mutate(!!category_col := factor(.data[[category_col]], levels = rev(top_categories))) %>%
    ggplot(aes_string(x = category_col)) +
    geom_bar(fill = "#A23B72", color = "white", alpha = 0.8) +
    coord_flip() +
    labs(
      title = paste("Top", top_n, "Disease Categories"),
      x = "Disease Category",
      y = "Number of Consultations"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text.y = element_text(size = 10)
    ) +
    scale_y_continuous(labels = comma_format())
}

#' Create geographic distribution plot
#' 
#' @param data data.frame. Register data
#' @param geo_col Character. Geographic column name (default: "admin1")
#' @param metric Character. Metric to plot ("count" or "rate")
#' @return ggplot. Geographic distribution plot
#' 
plot_geographic_distribution <- function(data, geo_col = "admin1", metric = "count") {
  
  if (!geo_col %in% colnames(data)) {
    stop(paste("Geographic column", geo_col, "not found"))
  }
  
  geo_summary <- data %>%
    group_by(.data[[geo_col]]) %>%
    summarise(
      consultations = n(),
      unique_facilities = n_distinct(orgunit, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      rate_per_facility = consultations / pmax(unique_facilities, 1)
    )
  
  y_var <- if (metric == "rate") "rate_per_facility" else "consultations"
  y_label <- if (metric == "rate") "Consultations per Facility" else "Total Consultations"
  
  ggplot(geo_summary, aes_string(x = geo_col, y = y_var)) +
    geom_col(fill = "#F18F01", color = "white", alpha = 0.8) +
    labs(
      title = paste("Consultations by", str_to_title(gsub("admin", "Administrative Level ", geo_col))),
      x = str_to_title(gsub("admin", "Administrative Level ", geo_col)),
      y = y_label
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_y_continuous(labels = comma_format())
}

#' Create age-sex pyramid
#' 
#' @param data data.frame. Register data
#' @param age_col Character. Age group column name
#' @param sex_col Character. Sex column name
#' @return ggplot. Age-sex pyramid
#' 
plot_age_sex_pyramid <- function(data, age_col = "age_group_new", sex_col = "sex") {
  
  if (!age_col %in% colnames(data) || !sex_col %in% colnames(data)) {
    stop("Required age or sex columns not found")
  }
  
  pyramid_data <- data %>%
    filter(!is.na(.data[[age_col]]) & !is.na(.data[[sex_col]])) %>%
    count(.data[[age_col]], .data[[sex_col]]) %>%
    mutate(
      n = ifelse(.data[[sex_col]] == "Male", -n, n),
      abs_n = abs(n)
    )
  
  ggplot(pyramid_data, aes_string(x = age_col, y = "n", fill = sex_col)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(
      labels = function(x) comma_format()(abs(x)),
      breaks = pretty_breaks(n = 6)
    ) +
    scale_fill_manual(values = c("Male" = "#2E86AB", "Female" = "#A23B72")) +
    labs(
      title = "Age-Sex Distribution of Consultations",
      x = "Age Group",
      y = "Number of Consultations",
      fill = "Sex"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.position = "bottom"
    )
}

#' Create time series plot by category
#' 
#' @param data data.frame. Register data
#' @param date_col Character. Date column name
#' @param category_col Character. Category column name
#' @param top_n Numeric. Number of top categories to show
#' @return ggplot. Time series plot
#' 
plot_time_series_by_category <- function(data, date_col = "month2", 
                                       category_col = "disease_cat", top_n = 8) {
  
  if (!date_col %in% colnames(data) || !category_col %in% colnames(data)) {
    stop("Required date or category columns not found")
  }
  
  # Get top categories
  top_categories <- data %>%
    count(.data[[category_col]], sort = TRUE) %>%
    slice_head(n = top_n) %>%
    pull(.data[[category_col]])
  
  # Prepare time series data
  ts_data <- data %>%
    filter(.data[[category_col]] %in% top_categories) %>%
    group_by(.data[[date_col]], .data[[category_col]]) %>%
    summarise(consultations = n(), .groups = "drop")
  
  ggplot(ts_data, aes_string(x = date_col, y = "consultations", color = category_col)) +
    geom_line(size = 1.2, alpha = 0.8) +
    geom_point(size = 2, alpha = 0.8) +
    scale_color_brewer(type = "qual", palette = "Set2") +
    labs(
      title = paste("Consultation Trends by", str_to_title(gsub("_", " ", category_col))),
      x = "Time Period",
      y = "Number of Consultations",
      color = str_to_title(gsub("_", " ", category_col))
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    ) +
    guides(color = guide_legend(ncol = 2)) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")
}

#' Get summary statistics for reporting
#' 
#' @param data data.frame. Register data
#' @return List. Summary statistics
#' 
get_summary_statistics <- function(data) {
  
  summary_stats <- list(
    total_consultations = nrow(data),
    date_range = if("datevisitnew" %in% colnames(data)) {
      range(data$datevisitnew, na.rm = TRUE)
    } else {
      c(NA, NA)
    },
    unique_facilities = if("orgunit" %in% colnames(data)) {
      length(unique(data$orgunit))
    } else {
      NA
    },
    top_diseases = if("morbidity" %in% colnames(data)) {
      data %>%
        count(morbidity, sort = TRUE) %>%
        slice_head(n = 5) %>%
        pull(morbidity)
    } else {
      character(0)
    },
    geographic_distribution = if("admin1" %in% colnames(data)) {
      data %>%
        count(admin1, sort = TRUE) %>%
        rename(region = admin1, consultations = n)
    } else {
      data.frame()
    }
  )
  
  return(summary_stats)
}

#' Create custom color palette for visualizations
#' 
#' @param n Numeric. Number of colors needed
#' @param type Character. Type of palette ("categorical", "sequential", "diverging")
#' @return Character vector. Color codes
#' 
get_custom_palette <- function(n, type = "categorical") {
  
  if (type == "categorical" && n <= 12) {
    # Custom categorical palette
    colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#6A994E", 
                "#9B5DE5", "#F15BB5", "#00BBF9", "#00F5FF", "#7209B7",
                "#A4133C", "#FF8500")
    return(colors[1:n])
  } else if (type == "sequential") {
    return(viridis::viridis(n))
  } else if (type == "diverging") {
    return(RColorBrewer::brewer.pal(n, "RdYlBu"))
  } else {
    # Fallback to viridis for large categorical
    return(viridis::viridis(n, discrete = TRUE))
  }
}

#' Apply consistent theme to ggplot objects
#' 
#' @param base_size Numeric. Base font size
#' @return ggplot theme object
#' 
theme_report <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5, 
                               margin = margin(b = 20)),
      plot.subtitle = element_text(size = base_size, hjust = 0.5, 
                                  margin = margin(b = 15)),
      axis.title = element_text(size = base_size, face = "bold"),
      axis.text = element_text(size = base_size - 1),
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      strip.text = element_text(size = base_size, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )
}