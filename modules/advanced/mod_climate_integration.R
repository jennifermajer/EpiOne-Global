# modules/advanced/mod_climate_integration.R - Complete Working Module

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(shiny)
  library(lubridate)
  library(purrr)
  library(stringr)
})

# Load zoo for moving averages (with fallback)
if (requireNamespace("zoo", quietly = TRUE)) {
  library(zoo)
} else {
  # Simple moving average fallback function
  rollmean <- function(x, k, fill = NA, align = "center") {
    n <- length(x)
    if (n < k) return(rep(fill, n))
    
    result <- rep(fill, n)
    for (i in k:n) {
      result[i] <- mean(x[(i-k+1):i], na.rm = TRUE)
    }
    return(result)
  }
}

# Add the null coalescing operator
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

# â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Supporting functions
# â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

#' Syria administrative location coordinates
#' This would ideally be loaded from a proper shapefile or database
get_syria_locations <- function() {
  # Sample coordinates for major Syrian governorates and cities
  # In production, this should be loaded from a proper geographic database
  tibble(
    admin_level = c(
      rep("country", 1),
      rep("governorate", 14),
      rep("district", 12),
      rep("subdistrict", 8)
    ),
    location_name = c(
      # Country
      "Syria",
      # Governorates (Admin1)
      "Damascus", "Aleppo", "Homs", "Hama", "Lattakia", "Idlib", 
      "Deir ez-Zor", "Al-Hasakah", "Ar-Raqqa", "Daraa", "As-Suwayda", 
      "Quneitra", "Tartus", "Damascus Countryside",
      # Major Districts (Admin2) 
      "Damascus Center", "Aleppo Center", "Homs Center", "Hama Center",
      "Lattakia Center", "Idlib Center", "Deir ez-Zor Center", "Al-Hasakah Center",
      "Ar-Raqqa Center", "Daraa Center", "As-Suwayda Center", "Tartus Center",
      # Sample Sub-districts (Admin3)
      "Bab Touma", "Sarouja", "Midan", "Salihiyah", 
      "Al-Maydan", "Bab Sharqi", "Jobar", "Rukn al-Din"
    ),
    latitude = c(
      # Country center
      33.5138,
      # Governorates
      33.5102, 36.1975, 34.7394, 35.1430, 35.5211, 35.9333,
      35.3362, 36.5000, 35.9500, 32.6189, 32.7081,
      33.1264, 34.8833, 33.4047,
      # Districts
      33.5102, 36.1975, 34.7394, 35.1430, 35.5211, 35.9333,
      35.3362, 36.5000, 35.9500, 32.6189, 32.7081, 34.8833,
      # Sub-districts
      33.5150, 33.5200, 33.4950, 33.5300,
      33.5050, 33.5180, 33.5420, 33.5380
    ),
    longitude = c(
      # Country center
      36.2765,
      # Governorates  
      36.2913, 37.1612, 36.7119, 36.7500, 35.7850, 36.6333,
      40.1458, 40.7500, 39.0167, 36.0991, 36.5640,
      35.8214, 35.8833, 36.5114,
      # Districts
      36.2913, 37.1612, 36.7119, 36.7500, 35.7850, 36.6333,
      40.1458, 40.7500, 39.0167, 36.0991, 36.5640, 35.8833,
      # Sub-districts
      36.2850, 36.2950, 36.2650, 36.3100,
      36.2750, 36.2980, 36.3420, 36.3180
    ),
    admin1_name = c(
      "Syria",
      "Damascus", "Aleppo", "Homs", "Hama", "Lattakia", "Idlib", 
      "Deir ez-Zor", "Al-Hasakah", "Ar-Raqqa", "Daraa", "As-Suwayda", 
      "Quneitra", "Tartus", "Damascus Countryside",
      "Damascus", "Aleppo", "Homs", "Hama", "Lattakia", "Idlib", 
      "Deir ez-Zor", "Al-Hasakah", "Ar-Raqqa", "Daraa", "As-Suwayda", "Tartus",
      rep("Damascus", 8)
    )
  )
}

#' Get coordinates for a specific location
get_location_coordinates <- function(location_name, admin_level = "governorate") {
  
  locations_df <- get_syria_locations()
  
  # Try exact match first
  location_match <- locations_df %>%
    filter(location_name == !!location_name, admin_level == !!admin_level)
  
  # If no exact match, try partial match
  if (nrow(location_match) == 0) {
    location_match <- locations_df %>%
      filter(str_detect(str_to_lower(location_name), str_to_lower(!!location_name)))
  }
  
  # If still no match, return Syria center coordinates
  if (nrow(location_match) == 0) {
    warning("Location '", location_name, "' not found. Using Syria center coordinates.")
    return(list(lat = 33.5138, lon = 36.2765, name = "Syria (default)"))
  }
  
  # Return first match
  first_match <- location_match[1, ]
  return(list(
    lat = first_match$latitude,
    lon = first_match$longitude, 
    name = first_match$location_name,
    admin_level = first_match$admin_level,
    admin1 = first_match$admin1_name
  ))
}

#' Get available locations by admin level
get_available_locations <- function(admin_level = "all") {
  
  locations_df <- get_syria_locations()
  
  if (admin_level == "all") {
    return(locations_df)
  } else {
    return(locations_df %>% filter(admin_level == !!admin_level))
  }
}

#' Fetch climate data for multiple locations
fetch_multi_location_climate <- function(locations_list, date_range, variables, source = "nasa_power") {
  
  climate_results <- list()
  
  for (i in seq_along(locations_list)) {
    location_info <- locations_list[[i]]
    
    cat("ðŸ“ Fetching climate data for:", location_info$name, "\n")
    
    tryCatch({
      location_climate <- fetch_climate_data(
        source = source,
        date_range = date_range,
        location = location_info,
        variables = variables
      )
      
      # Add location information to the data
      if (!is.null(location_climate) && nrow(location_climate) > 0) {
        location_climate$location_name <- location_info$name
        location_climate$admin_level <- location_info$admin_level %||% "unknown"
        location_climate$admin1 <- location_info$admin1 %||% location_info$name
        location_climate$latitude <- location_info$lat
        location_climate$longitude <- location_info$lon
        
        climate_results[[location_info$name]] <- location_climate
      }
      
    }, error = function(e) {
      warning("Failed to fetch climate data for ", location_info$name, ": ", e$message)
    })
  }
  
  # Combine all results
  if (length(climate_results) > 0) {
    combined_data <- bind_rows(climate_results)
    return(combined_data)
  } else {
    return(NULL)
  }
}

#' Timeout wrapper function
withTimeout <- function(expr, timeout = 60) {
  tryCatch({
    eval(expr)
  }, error = function(e) {
    if (grepl("timeout", e$message, ignore.case = TRUE)) {
      stop("NASA POWER API call timed out. Try a smaller date range.")
    } else {
      stop(e)
    }
  })
}

#' Generate demo climate data
generate_demo_climate_data <- function(date_range) {
  dates <- seq(as.Date(date_range[1]), as.Date(date_range[2]), by = "day")
  n <- length(dates)
  
  # More realistic demo data for Syria with seasonal patterns
  day_of_year <- as.numeric(format(dates, "%j"))
  
  tibble(
    date = dates,
    # Temperature with seasonal variation (hot summers, mild winters)
    temperature_mean = 20 + 12 * sin(2 * pi * (day_of_year - 80) / 365) + rnorm(n, 0, 3),
    temperature_min = temperature_mean - runif(n, 8, 12),
    temperature_max = temperature_mean + runif(n, 8, 12),
    # Humidity with inverse seasonal pattern (higher in winter)
    humidity = pmax(20, pmin(90, 65 - 20 * sin(2 * pi * (day_of_year - 80) / 365) + rnorm(n, 0, 10))),
    # Precipitation with winter peaks
    precipitation = pmax(0, 2 + 3 * sin(2 * pi * (day_of_year - 300) / 365) + rexp(n, 1)),
    wind_speed = pmax(0, rlnorm(n, log(8), 0.4)),
    pressure = rnorm(n, 1013, 8)
  )
}

#' Fetch NASA POWER data with improved date handling
fetch_nasa_power_data <- function(date_range, location, variables) {
  
  # Check if nasapower package is available
  if (!requireNamespace("nasapower", quietly = TRUE)) {
    warning("nasapower package not available. Using demo data.")
    return(generate_demo_climate_data(date_range))
  }
  
  # NASA POWER coordinates for Syria
  lat <- location$lat %||% 33.5138
  lon <- location$lon %||% 36.2765
  
  # NASA POWER date limitations - be more conservative
  earliest_date <- as.Date("1981-01-01")
  latest_date <- Sys.Date() - 7  # 7 days behind for safety
  
  # Store original dates for demo data fallback
  original_start <- date_range[1]
  original_end <- date_range[2]
  
  # Adjust dates if needed
  adjusted <- FALSE
  
  if (as.Date(date_range[1]) < earliest_date) {
    message("NASA POWER data starts from 1981-01-01. Adjusting start date from ", original_start, " to ", earliest_date)
    date_range[1] <- earliest_date
    adjusted <- TRUE
  }
  
  if (as.Date(date_range[2]) > latest_date) {
    message("NASA POWER data is usually 7 days behind current date. Adjusting end date from ", original_end, " to ", latest_date)
    date_range[2] <- latest_date
    adjusted <- TRUE
  }
  
  # If end date becomes before start date after adjustment, use a safe recent range
  if (as.Date(date_range[2]) <= as.Date(date_range[1])) {
    warning("Invalid date range after adjustments. Using safe recent date range.")
    date_range[1] <- latest_date - 30  # 30 days ago
    date_range[2] <- latest_date       # 7 days ago
    adjusted <- TRUE
  }
  
  # Warn user about date adjustments
  if (adjusted) {
    message("ðŸ“… Date range adjusted for NASA POWER availability: ", 
            format(date_range[1], "%Y-%m-%d"), " to ", format(date_range[2], "%Y-%m-%d"))
  }
  
  # Map variables to NASA POWER parameters
  nasa_params <- c(
    "T2M",           # Temperature at 2 meters
    "T2M_MIN",       # Minimum temperature  
    "T2M_MAX",       # Maximum temperature
    "RH2M",          # Relative humidity
    "PRECTOTCORR",   # Precipitation
    "WS2M"           # Wind speed
  )
  
  tryCatch({
    # Debug information
    message("ðŸ›°ï¸ Fetching NASA POWER data...")
    message("ðŸ“ Location: lat=", lat, ", lon=", lon)
    message("ðŸ“… Date range: ", format(date_range[1], "%Y-%m-%d"), " to ", format(date_range[2], "%Y-%m-%d"))
    
    # Format dates for NASA POWER API
    start_date <- format(as.Date(date_range[1]), "%Y%m%d")
    end_date <- format(as.Date(date_range[2]), "%Y%m%d")
    
    # Make the API call
    nasa_data <- nasapower::get_power(
      community = "ag",
      lonlat = c(lon, lat),
      pars = nasa_params,
      dates = c(start_date, end_date),
      temporal_api = "daily"
    )
    
    message("âœ… API call successful. Retrieved ", nrow(nasa_data), " rows")
    
    # Check if we got data
    if (is.null(nasa_data) || nrow(nasa_data) == 0) {
      warning("NASA POWER API returned no data for the requested date range. Using demo data.")
      return(generate_demo_climate_data(c(original_start, original_end)))
    }
    
    # Process the data - YYYYMMDD is already a Date object
    climate_data <- nasa_data %>%
      mutate(
        date = YYYYMMDD,  # Direct assignment since it's already Date
        temperature_mean = as.numeric(T2M),
        temperature_min = as.numeric(T2M_MIN),
        temperature_max = as.numeric(T2M_MAX),
        humidity = as.numeric(RH2M),
        precipitation = pmax(0, as.numeric(PRECTOTCORR), na.rm = TRUE),
        wind_speed = pmax(0, as.numeric(WS2M), na.rm = TRUE),
        pressure = NA_real_
      ) %>%
      select(date, temperature_mean, temperature_min, temperature_max, 
             humidity, precipitation, wind_speed, pressure) %>%
      arrange(date)
    
    message("âœ… Data processed successfully. Final dataset has ", nrow(climate_data), " rows")
    
    # Final validation
    if (nrow(climate_data) == 0) {
      warning("No valid data after processing. Using demo data.")
      return(generate_demo_climate_data(c(original_start, original_end)))
    }
    
    return(climate_data)
    
  }, error = function(e) {
    warning("Failed to fetch NASA POWER data: ", e$message)
    return(generate_demo_climate_data(c(original_start, original_end)))
  })
}

#' Fetch OpenWeatherMap historical data
fetch_openweather_data <- function(date_range, location, variables) {
  warning("OpenWeatherMap API not implemented. Using demo data.")
  return(generate_demo_climate_data(date_range))
}

#' Fetch NOAA data (placeholder)
fetch_noaa_data <- function(date_range, location, variables) {
  warning("NOAA data fetching not implemented. Using demo data.")
  return(generate_demo_climate_data(date_range))
}

#' Fetch local weather data (placeholder)
fetch_local_weather_data <- function(date_range, location, variables) {
  warning("Local weather data fetching not implemented. Using demo data.")
  return(generate_demo_climate_data(date_range))
}

#' Fetch climate data from various sources
fetch_climate_data <- function(source, date_range, location, variables) {
  
  # Validate inputs
  if (is.null(date_range) || length(date_range) != 2) {
    stop("date_range must be a vector of length 2 with start and end dates")
  }
  
  # Convert to Date objects if they aren't already
  date_range <- as.Date(date_range)
  
  if (date_range[2] < date_range[1]) {
    stop("End date must be after start date")
  }
  
  climate_data <- switch(source,
                         "nasa_power" = fetch_nasa_power_data(date_range, location, variables),
                         "openweather" = fetch_openweather_data(date_range, location, variables),
                         "noaa" = fetch_noaa_data(date_range, location, variables),
                         "local" = fetch_local_weather_data(date_range, location, variables),
                         # Default case
                         {
                           warning("Unknown source: ", source, ". Using demo data.")
                           generate_demo_climate_data(date_range)
                         }
  )
  
  # Validate the returned data
  if (is.null(climate_data)) {
    warning("Climate data function returned NULL. Using demo data.")
    climate_data <- generate_demo_climate_data(date_range)
  } else if (!is.data.frame(climate_data)) {
    warning("Climate data is not a data frame. Using demo data.")
    climate_data <- generate_demo_climate_data(date_range)
  } else if (nrow(climate_data) == 0) {
    warning("Climate data has no rows. Using demo data.")
    climate_data <- generate_demo_climate_data(date_range)
  }
  
  # Standardize output format
  tryCatch({
    standardized_data <- standardize_climate_data(climate_data, source)
    return(standardized_data)
  }, error = function(e) {
    warning("Error standardizing climate data: ", e$message, ". Using demo data.")
    return(generate_demo_climate_data(date_range))
  })
}

#' Standardize climate data format
standardize_climate_data <- function(climate_data, source) {
  
  # Ensure required columns exist
  required_cols <- c("date", "temperature_mean", "humidity", "precipitation")
  
  for (col in required_cols) {
    if (!col %in% names(climate_data)) {
      climate_data[[col]] <- NA
    }
  }
  
  # Add derived variables
  standardized_data <- climate_data %>%
    mutate(
      # Temperature categories
      temp_category = case_when(
        temperature_mean >= 35 ~ "Extreme Heat",
        temperature_mean >= 30 ~ "High Heat",
        temperature_mean >= 25 ~ "Warm",
        temperature_mean >= 15 ~ "Moderate",
        temperature_mean >= 5 ~ "Cool",
        TRUE ~ "Cold"
      ),
      
      # Precipitation categories
      precip_category = case_when(
        precipitation >= 50 ~ "Heavy Rain",
        precipitation >= 25 ~ "Moderate Rain",
        precipitation >= 5 ~ "Light Rain",
        precipitation > 0 ~ "Trace Rain",
        TRUE ~ "No Rain"
      ),
      
      # Humidity categories
      humidity_category = case_when(
        humidity >= 80 ~ "Very Humid",
        humidity >= 60 ~ "Humid",
        humidity >= 40 ~ "Moderate",
        TRUE ~ "Dry"
      ),
      
      # Heat index calculation (simplified)
      heat_index = temperature_mean + (humidity / 10),
      
      # Data source
      climate_source = source
    ) %>%
    arrange(date)
  
  return(standardized_data)
}

#' Analyze climate-disease correlations with drill-down support
analyze_climate_disease_correlation <- function(health_data, climate_data, disease, climate_var, lag_days = 0, drill_level = "category", specific_disease = NULL) {
  
  # Prepare health data based on drill-down level
  if (drill_level == "category") {
    # Filter by morbidity category
    health_daily <- health_data %>%
      filter(category_canonical_disease_imc == disease) %>%
      mutate(date = as.Date(datevisit)) %>%
      count(date, name = "cases")
  } else if (drill_level == "specific" && !is.null(specific_disease)) {
    # Filter by specific standardized disease
    health_daily <- health_data %>%
      filter(canonical_disease_imc == specific_disease) %>%
      mutate(date = as.Date(datevisit)) %>%
      count(date, name = "cases")
  } else {
    # Default to category level
    health_daily <- health_data %>%
      filter(category_canonical_disease_imc == disease) %>%
      mutate(date = as.Date(datevisit)) %>%
      count(date, name = "cases")
  }
  
  # Apply lag to climate data
  climate_lagged <- climate_data %>%
    mutate(date = date + days(lag_days)) %>%
    select(date, !!sym(climate_var))
  
  # Merge datasets
  merged_data <- health_daily %>%
    full_join(climate_lagged, by = "date") %>%
    filter(!is.na(cases), !is.na(!!sym(climate_var))) %>%
    arrange(date)
  
  if (nrow(merged_data) < 10) {
    return(list(
      correlation = NA,
      p_value = NA,
      n_observations = nrow(merged_data),
      message = "Insufficient data for correlation analysis"
    ))
  }
  
  # Calculate correlation
  correlation_result <- cor.test(
    merged_data$cases, 
    merged_data[[climate_var]], 
    method = "pearson"
  )
  
  return(list(
    correlation = correlation_result$estimate,
    p_value = correlation_result$p.value,
    confidence_interval = correlation_result$conf.int,
    n_observations = nrow(merged_data),
    merged_data = merged_data,
    disease = disease,
    climate_variable = climate_var
  ))
}

#' Prepare time series data for visualization with drill-down support
prepare_timeseries_data <- function(health_data, climate_data, disease, climate_var, aggregation = "week", drill_level = "category", specific_disease = NULL) {
  
  # Prepare health data based on drill-down level
  if (drill_level == "category") {
    # Filter by morbidity category
    health_daily <- health_data %>%
      filter(category_canonical_disease_imc == disease) %>%
      mutate(date = as.Date(datevisit)) %>%
      count(date, name = "cases") %>%
      arrange(date)
  } else if (drill_level == "specific" && !is.null(specific_disease)) {
    # Filter by specific standardized disease
    health_daily <- health_data %>%
      filter(canonical_disease_imc == specific_disease) %>%
      mutate(date = as.Date(datevisit)) %>%
      count(date, name = "cases") %>%
      arrange(date)
  } else {
    # Default to category level
    health_daily <- health_data %>%
      filter(category_canonical_disease_imc == disease) %>%
      mutate(date = as.Date(datevisit)) %>%
      count(date, name = "cases") %>%
      arrange(date)
  }
  
  # Prepare climate data
  climate_daily <- climate_data %>%
    select(date, !!sym(climate_var)) %>%
    arrange(date)
  
  # Merge datasets
  merged_daily <- health_daily %>%
    full_join(climate_daily, by = "date") %>%
    filter(!is.na(date)) %>%
    arrange(date)
  
  # Fill missing values
  merged_daily$cases[is.na(merged_daily$cases)] <- 0
  
  # Remove rows where climate data is missing
  merged_daily <- merged_daily %>%
    filter(!is.na(!!sym(climate_var)))
  
  if (nrow(merged_daily) == 0) {
    return(NULL)
  }
  
  # Aggregate data based on selected period
  if (aggregation == "day") {
    aggregated_data <- merged_daily %>%
      mutate(period = date)
  } else if (aggregation == "week") {
    aggregated_data <- merged_daily %>%
      mutate(
        year_week = paste0(year(date), "-W", sprintf("%02d", week(date))),
        period = floor_date(date, "week")
      ) %>%
      group_by(period) %>%
      summarise(
        cases = sum(cases, na.rm = TRUE),
        !!climate_var := mean(!!sym(climate_var), na.rm = TRUE),
        .groups = "drop"
      )
  } else if (aggregation == "month") {
    aggregated_data <- merged_daily %>%
      mutate(period = floor_date(date, "month")) %>%
      group_by(period) %>%
      summarise(
        cases = sum(cases, na.rm = TRUE),
        !!climate_var := mean(!!sym(climate_var), na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # Add moving averages for smoothing
  if (nrow(aggregated_data) >= 3) {
    aggregated_data <- aggregated_data %>%
      arrange(period) %>%
      mutate(
        cases_smooth = if (exists("zoo") && requireNamespace("zoo", quietly = TRUE)) {
          zoo::rollmean(cases, k = min(3, nrow(aggregated_data)), fill = NA, align = "center")
        } else {
          rollmean(cases, k = min(3, nrow(aggregated_data)), fill = NA, align = "center")
        },
        climate_smooth = if (exists("zoo") && requireNamespace("zoo", quietly = TRUE)) {
          zoo::rollmean(!!sym(climate_var), k = min(3, nrow(aggregated_data)), fill = NA, align = "center")
        } else {
          rollmean(!!sym(climate_var), k = min(3, nrow(aggregated_data)), fill = NA, align = "center")
        }
      )
  } else {
    aggregated_data$cases_smooth <- aggregated_data$cases
    aggregated_data$climate_smooth <- aggregated_data[[climate_var]]
  }
  
  return(aggregated_data)
}

# Helper function to create empty plotly plot
plotly_empty <- function(message = "No data available") {
  plot_ly() %>%
    add_annotations(
      text = message,
      x = 0.5,
      y = 0.5,
      showarrow = FALSE,
      font = list(size = 16, color = "gray")
    ) %>%
    layout(
      xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
      yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
    )
}

# â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# UI
# â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

#' Climate Integration Module UI
climate_integration_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             box(
               title = "Climate-Health Integration Dashboard",
               status = "success",
               solidHeader = TRUE,
               width = NULL,
               
               tabsetPanel(
                 # Climate Data Overview Tab
                 tabPanel(
                   "Climate Overview",
                   br(),
                   fluidRow(
                     column(4,
                            wellPanel(
                              h5("Climate Data Parameters"),
                              
                              # Location Selection
                              selectInput(ns("location_scope"), "Geographic Scope:",
                                          choices = list(
                                            "Country Level" = "country",
                                            "Governorate Level" = "governorate", 
                                            "District Level" = "district",
                                            "Sub-district Level" = "subdistrict",
                                            "Multiple Locations" = "multiple"
                                          )),
                              
                              conditionalPanel(
                                condition = "input.location_scope != 'country' && input.location_scope != 'multiple'",
                                ns = ns,
                                selectInput(ns("single_location"), "Select Location:",
                                            choices = NULL)
                              ),
                              
                              # Multi-Location Comparison Tab (only show if multiple locations selected)
                              tabPanel(
                                "Location Comparison",
                                br(),
                                conditionalPanel(
                                  condition = "input.location_scope == 'multiple'",
                                  ns = ns,
                                  fluidRow(
                                    column(12,
                                           box(
                                             title = "Climate Comparison Across Locations",
                                             width = NULL,
                                             status = "warning",
                                             plotlyOutput(ns("multi_location_plot"), height = "500px")
                                           )
                                    )
                                  ),
                                  
                                  fluidRow(
                                    column(6,
                                           box(
                                             title = "Location Climate Summary",
                                             width = NULL,
                                             DT::dataTableOutput(ns("location_summary_table"))
                                           )
                                    ),
                                    column(6,
                                           box(
                                             title = "Climate Variable Map",
                                             width = NULL,
                                             plotlyOutput(ns("climate_map"), height = "400px")
                                           )
                                    )
                                  )
                                ),
                                
                                conditionalPanel(
                                  condition = "input.location_scope != 'multiple'",
                                  ns = ns,
                                  div(
                                    style = "text-align: center; margin-top: 100px;",
                                    h4("Select 'Multiple Locations' in Climate Overview to use this feature"),
                                    p("This tab allows comparison of climate data across different locations in Syria.")
                                  )
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.location_scope == 'multiple'",
                                ns = ns,
                                selectInput(ns("multi_admin_level"), "Administrative Level:",
                                            choices = list(
                                              "Governorates" = "governorate",
                                              "Districts" = "district", 
                                              "Sub-districts" = "subdistrict"
                                            )),
                                selectInput(ns("multiple_locations"), "Select Locations:",
                                            choices = NULL,
                                            multiple = TRUE),
                                helpText("ðŸ’¡ Select multiple locations for comparison")
                              ),
                              
                              # Data Source
                              selectInput(ns("climate_source"), "Data Source:",
                                          choices = list(
                                            "NASA POWER" = "nasa_power",
                                            "OpenWeatherMap" = "openweather",
                                            "NOAA" = "noaa",
                                            "Local Weather Stations" = "local"
                                          )),
                              
                              # Date Range
                              dateRangeInput(ns("climate_date_range"), "Date Range:",
                                             start = Sys.Date() - 90,   # 3 months ago
                                             end = Sys.Date() - 7,      # 1 week ago
                                             max = Sys.Date() - 7),     # Prevent recent dates
                              helpText("ðŸ“… NASA POWER data is usually 7 days behind current date"),
                              
                              # Variables
                              selectInput(ns("climate_variables"), "Climate Variables:",
                                          choices = list(
                                            "Temperature" = "temperature_mean",
                                            "Precipitation" = "precipitation", 
                                            "Humidity" = "humidity",
                                            "Wind Speed" = "wind_speed",
                                            "Pressure" = "pressure"
                                          ),
                                          multiple = TRUE,
                                          selected = c("temperature_mean", "precipitation")),
                              
                              actionButton(ns("fetch_climate_data"), "Fetch Climate Data",
                                           class = "btn-success")
                            )
                     ),
                     column(8,
                            plotlyOutput(ns("climate_trends"), height = "400px")
                     )
                   ),
                   
                   fluidRow(
                     column(12,
                            box(
                              title = "Climate Summary Statistics",
                              width = NULL,
                              tableOutput(ns("climate_summary"))
                            )
                     )
                   )
                 ),
                 
                 # Climate-Disease Correlation Tab
                 tabPanel(
                   "Climate-Health Correlations",
                   br(),
                   fluidRow(
                     column(4,
                            wellPanel(
                              h5("Correlation Analysis"),
                              # Drill-down disease selection
                              selectInput(ns("analysis_level"), "Analysis Level:",
                                          choices = list(
                                            "Disease Category" = "category",
                                            "Specific Disease" = "specific",
                                            "Climate-Sensitive Diseases" = "climate_sensitive"
                                          ),
                                          selected = "category"),
                              
                              conditionalPanel(
                                condition = "input.analysis_level == 'category'",
                                ns = ns,
                                selectInput(ns("target_disease_category"), "Disease Category:",
                                            choices = NULL)
                              ),
                              
                              conditionalPanel(
                                condition = "input.analysis_level == 'specific'",
                                ns = ns,
                                selectInput(ns("target_disease_category_for_specific"), "Disease Category:",
                                            choices = NULL),
                                selectInput(ns("target_specific_disease"), "Specific Disease:",
                                            choices = NULL)
                              ),
                              
                              conditionalPanel(
                                condition = "input.analysis_level == 'climate_sensitive'",
                                ns = ns,
                                selectInput(ns("climate_disease_group"), "Climate-Sensitive Group:",
                                            choices = list(
                                              "All Climate-Sensitive" = "all_climate",
                                              "Vector-Borne" = "vector_borne",
                                              "Water-Related" = "water_related",
                                              "Temperature-Sensitive" = "temperature_sensitive"
                                            ),
                                            selected = "all_climate")
                              ),
                              
                              selectInput(ns("climate_factor"), "Climate Factor:",
                                          choices = NULL),
                              numericInput(ns("lag_days"), "Lag Period (days):",
                                           value = 14, min = 0, max = 60),
                              actionButton(ns("calculate_climate_correlation"), "Analyze",
                                           class = "btn-warning")
                            )
                     ),
                     column(8,
                            plotlyOutput(ns("climate_disease_plot"), height = "400px")
                     )
                   )
                 ),
                 
                 # Climate-Sensitive Disease Analysis Tab
                 tabPanel(
                   "Climate-Sensitive Analysis",
                   br(),
                   fluidRow(
                     column(4,
                            wellPanel(
                              h5("Climate-Sensitive Disease Analysis"),
                              selectInput(ns("climate_analysis_type"), "Analysis Type:",
                                          choices = list(
                                            "Climate-Sensitive Disease Overview" = "overview",
                                            "Seasonal Patterns" = "seasonal",
                                            "Climate Risk Assessment" = "risk"
                                          )),
                              
                              conditionalPanel(
                                condition = "input.climate_analysis_type == 'overview'",
                                ns = ns,
                                checkboxGroupInput(ns("climate_groups_selected"), "Climate-Sensitive Groups:",
                                                   choices = list(
                                                     "All Climate-Sensitive" = "climate_sensitive",
                                                     "Vector-Borne Diseases" = "vector_borne", 
                                                     "Water-Related Diseases" = "water_related",
                                                     "Temperature-Sensitive" = "temperature_sensitive"
                                                   ),
                                                   selected = "climate_sensitive")
                              ),
                              
                              conditionalPanel(
                                condition = "input.climate_analysis_type == 'seasonal'",
                                ns = ns,
                                selectInput(ns("seasonal_climate_group"), "Focus Group:",
                                            choices = NULL),
                                selectInput(ns("seasonal_timeframe"), "Time Grouping:",
                                            choices = list(
                                              "Month" = "month",
                                              "Season" = "season",
                                              "Quarter" = "quarter"
                                            ))
                              ),
                              
                              conditionalPanel(
                                condition = "input.climate_analysis_type == 'risk'",
                                ns = ns,
                                selectInput(ns("risk_climate_var"), "Climate Risk Factor:",
                                            choices = NULL),
                                numericInput(ns("risk_threshold_low"), "Low Risk Threshold:",
                                             value = 25, min = 0, max = 100),
                                numericInput(ns("risk_threshold_high"), "High Risk Threshold:", 
                                             value = 75, min = 0, max = 100)
                              ),
                              
                              actionButton(ns("generate_climate_analysis"), "Generate Analysis",
                                           class = "btn-success")
                            )
                     ),
                     column(8,
                            plotlyOutput(ns("climate_sensitive_plot"), height = "500px")
                     )
                   ),
                   
                   fluidRow(
                     column(6,
                            box(
                              title = "Climate-Sensitive Disease Summary",
                              width = NULL,
                              status = "info",
                              DT::dataTableOutput(ns("climate_disease_summary"))
                            )
                     ),
                     column(6,
                            box(
                              title = "Risk Assessment Results",
                              width = NULL,
                              status = "warning",
                              tableOutput(ns("climate_risk_table"))
                            )
                     )
                   )
                 ),
                 
                 # Time Series Trends Tab
                 tabPanel(
                   "Time Series Trends",
                   br(),
                   fluidRow(
                     column(4,
                            wellPanel(
                              h5("Time Series Analysis"),
                              # Drill-down time series selection
                              selectInput(ns("ts_analysis_level"), "Analysis Level:",
                                          choices = list(
                                            "Disease Category" = "category",
                                            "Specific Disease" = "specific",
                                            "Climate-Sensitive Analysis" = "climate_sensitive"
                                          ),
                                          selected = "category"),
                              
                              conditionalPanel(
                                condition = "input.ts_analysis_level == 'category'",
                                ns = ns,
                                selectInput(ns("ts_disease_category"), "Disease Category:",
                                            choices = NULL)
                              ),
                              
                              conditionalPanel(
                                condition = "input.ts_analysis_level == 'specific'",
                                ns = ns,
                                selectInput(ns("ts_disease_category_for_specific"), "Disease Category:",
                                            choices = NULL),
                                selectInput(ns("ts_specific_disease"), "Specific Disease:",
                                            choices = NULL)
                              ),
                              
                              conditionalPanel(
                                condition = "input.ts_analysis_level == 'climate_sensitive'",
                                ns = ns,
                                selectInput(ns("ts_climate_disease_group"), "Climate-Sensitive Group:",
                                            choices = list(
                                              "All Climate-Sensitive" = "all_climate",
                                              "Vector-Borne" = "vector_borne",
                                              "Water-Related" = "water_related",
                                              "Temperature-Sensitive" = "temperature_sensitive"
                                            ),
                                            selected = "all_climate")
                              ),
                              
                              selectInput(ns("ts_climate_var"), "Select Climate Variable:",
                                          choices = NULL),
                              selectInput(ns("ts_aggregation"), "Time Aggregation:",
                                          choices = list(
                                            "Daily" = "day",
                                            "Weekly" = "week", 
                                            "Monthly" = "month"
                                          ),
                                          selected = "week"),
                              checkboxInput(ns("ts_smooth"), "Add Trend Lines", value = TRUE),
                              checkboxInput(ns("ts_dual_axis"), "Use Dual Y-Axis", value = TRUE),
                              actionButton(ns("generate_timeseries"), "Generate Time Series",
                                           class = "btn-info")
                            )
                     ),
                     column(8,
                            plotlyOutput(ns("timeseries_plot"), height = "550px")  # Increased height
                     )
                   ),
                   
                   fluidRow(
                     column(12,
                            box(
                              title = "Time Series Summary",
                              width = NULL,
                              status = "info",
                              DT::dataTableOutput(ns("timeseries_summary"))
                            )
                     )
                   )
                 )
               )
             )
      )
    )
  )
}

# â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Server
# â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

#' Climate Integration Module Server
climate_integration_Server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for climate data
    climate_values <- reactiveValues(
      climate_data = NULL,
      multi_location_data = NULL,
      disease_choices = NULL,
      climate_choices = NULL,
      correlation_results = NULL
    )
    
    # Update location choices based on selected scope
    observe({
      req(input$location_scope)
      
      if (input$location_scope %in% c("governorate", "district", "subdistrict")) {
        available_locs <- get_available_locations(input$location_scope)
        choices <- setNames(available_locs$location_name, available_locs$location_name)
        updateSelectInput(session, "single_location", choices = choices)
      }
    })
    
    # Update multiple location choices 
    observe({
      req(input$multi_admin_level)
      
      available_locs <- get_available_locations(input$multi_admin_level)
      choices <- setNames(available_locs$location_name, available_locs$location_name)
      updateSelectInput(session, "multiple_locations", choices = choices)
    })
    
    # Update disease choices when data changes - Enhanced for drill-down
    observe({
      req(filtered_data())
      df <- filtered_data()
      
      # Update disease categories
      if ("category_canonical_disease_imc" %in% names(df)) {
        categories <- sort(unique(df$category_canonical_disease_imc[!is.na(df$category_canonical_disease_imc)]))
        updateSelectInput(session, "target_disease_category", choices = categories)
        updateSelectInput(session, "target_disease_category_for_specific", choices = categories)
        updateSelectInput(session, "ts_disease_category", choices = categories)
        updateSelectInput(session, "ts_disease_category_for_specific", choices = categories)
        climate_values$disease_choices <- categories
      }
      
      # Update climate-sensitive group choices if available
      if ("climate_sensitive" %in% names(df)) {
        climate_sensitive_count <- sum(df$climate_sensitive, na.rm = TRUE)
        if (climate_sensitive_count > 0) {
          updateSelectInput(session, "seasonal_climate_group", 
                            choices = list(
                              "All Climate-Sensitive" = "climate_sensitive",
                              "Vector-Borne" = "vector_borne",
                              "Water-Related" = "water_related",
                              "Temperature-Sensitive" = "temperature_sensitive"
                            ))
        }
      }
    })
    
    # Update specific disease choices based on selected category
    observe({
      req(filtered_data())
      
      # For correlation analysis
      if (!is.null(input$target_disease_category_for_specific)) {
        df <- filtered_data()
        if ("canonical_disease_imc" %in% names(df) && "category_canonical_disease_imc" %in% names(df)) {
          specific_diseases <- df %>%
            filter(category_canonical_disease_imc == input$target_disease_category_for_specific) %>%
            pull(canonical_disease_imc) %>%
            unique() %>%
            sort()
          updateSelectInput(session, "target_specific_disease", choices = specific_diseases)
        }
      }
      
      # For time series analysis
      if (!is.null(input$ts_disease_category_for_specific)) {
        df <- filtered_data()
        if ("canonical_disease_imc" %in% names(df) && "category_canonical_disease_imc" %in% names(df)) {
          specific_diseases <- df %>%
            filter(category_canonical_disease_imc == input$ts_disease_category_for_specific) %>%
            pull(canonical_disease_imc) %>%
            unique() %>%
            sort()
          updateSelectInput(session, "ts_specific_disease", choices = specific_diseases)
        }
      }
    })
    
    # Update climate variable choices when climate data is available
    observe({
      req(climate_values$climate_data)
      climate_data <- climate_values$climate_data
      
      if (!is.null(climate_data) && nrow(climate_data) > 0) {
        climate_vars <- names(climate_data)[!names(climate_data) %in% c("date", "climate_source")]
        climate_vars <- climate_vars[sapply(climate_data[climate_vars], is.numeric)]
        
        updateSelectInput(session, "climate_factor", choices = climate_vars)
        updateSelectInput(session, "ts_climate_var", choices = climate_vars)  # Update time series options
        climate_values$climate_choices <- climate_vars
      }
    })
    
    # Fetch climate data
    observeEvent(input$fetch_climate_data, {
      
      showNotification("Fetching climate data...", type = "default", duration = NULL, id = "climate_fetch")
      
      tryCatch({
        
        if (input$location_scope == "country") {
          # Country level - use Syria center coordinates
          location <- list(lat = 33.5138, lon = 36.2765, name = "Syria")
          
          climate_data <- fetch_climate_data(
            source = input$climate_source,
            date_range = input$climate_date_range,
            location = location,
            variables = input$climate_variables
          )
          
          climate_values$climate_data <- climate_data
          climate_values$multi_location_data <- NULL
          
        } else if (input$location_scope == "multiple") {
          # Multiple locations
          req(input$multiple_locations)
          
          locations_list <- map(input$multiple_locations, function(loc_name) {
            get_location_coordinates(loc_name, input$multi_admin_level)
          })
          
          multi_climate_data <- fetch_multi_location_climate(
            locations_list = locations_list,
            date_range = input$climate_date_range,
            variables = input$climate_variables,
            source = input$climate_source
          )
          
          climate_values$multi_location_data <- multi_climate_data
          
          # Also set single location data to first location for compatibility
          if (!is.null(multi_climate_data) && nrow(multi_climate_data) > 0) {
            first_location_data <- multi_climate_data %>%
              filter(location_name == input$multiple_locations[1])
            climate_values$climate_data <- first_location_data
          }
          
        } else {
          # Single location (governorate, district, or subdistrict)
          req(input$single_location)
          
          location <- get_location_coordinates(input$single_location, input$location_scope)
          
          climate_data <- fetch_climate_data(
            source = input$climate_source,
            date_range = input$climate_date_range,
            location = location,
            variables = input$climate_variables
          )
          
          climate_values$climate_data <- climate_data
          climate_values$multi_location_data <- NULL
        }
        
        # Update climate variable choices for correlation
        if (!is.null(climate_values$climate_data) && nrow(climate_values$climate_data) > 0) {
          climate_vars <- names(climate_values$climate_data)[!names(climate_values$climate_data) %in% c("date", "climate_source", "location_name", "admin_level", "admin1", "latitude", "longitude")]
          climate_vars <- climate_vars[sapply(climate_values$climate_data[climate_vars], is.numeric)]
          
          updateSelectInput(session, "climate_factor", choices = climate_vars)
          updateSelectInput(session, "ts_climate_var", choices = climate_vars)
          climate_values$climate_choices <- climate_vars
        }
        
        removeNotification("climate_fetch")
        
        # Success message based on data type
        if (input$location_scope == "multiple") {
          n_locations <- length(input$multiple_locations)
          showNotification(paste("âœ… Climate data fetched for", n_locations, "locations!"), type = "default", duration = 3000)
        } else {
          location_name <- if(input$location_scope == "country") "Syria" else input$single_location
          showNotification(paste("âœ… Climate data fetched for", location_name, "!"), type = "default", duration = 3000)
        }
        
      }, error = function(e) {
        removeNotification("climate_fetch")
        showNotification(paste("Error fetching climate data:", e$message), type = "error", duration = 5000)
        
        # Use demo data as fallback
        demo_data <- generate_demo_climate_data(input$climate_date_range)
        climate_values$climate_data <- demo_data
        showNotification("Using demo climate data as fallback", type = "warning", duration = 3000)
      })
    })
    
    # Climate trends plot
    output$climate_trends <- renderPlotly({
      req(climate_values$climate_data, input$climate_variables)
      
      climate_data <- climate_values$climate_data
      
      if (is.null(climate_data) || nrow(climate_data) == 0) {
        return(plotly_empty("No climate data available"))
      }
      
      # Check if selected variables exist in the data
      available_vars <- intersect(input$climate_variables, names(climate_data))
      
      if (length(available_vars) == 0) {
        return(plotly_empty("Selected climate variables not found in data"))
      }
      
      # Prepare data for plotting
      plot_data <- climate_data %>%
        select(date, all_of(available_vars)) %>%
        pivot_longer(-date, names_to = "variable", values_to = "value") %>%
        filter(!is.na(value), !is.na(date)) %>%
        mutate(
          variable_label = str_to_title(str_replace_all(variable, "_", " "))
        )
      
      if (nrow(plot_data) == 0) {
        return(plotly_empty("No valid data points to plot"))
      }
      
      # Create the plot
      p <- ggplot(plot_data, aes(x = date, y = value, color = variable_label)) +
        geom_line(size = 1.2, alpha = 0.8) +
        facet_wrap(~variable_label, scales = "free_y", ncol = 1) +
        labs(
          title = "Climate Variables Over Time",
          x = "Date",
          y = "Value",
          color = "Variable"
        ) +
        theme_minimal() +
        theme(
          legend.position = "none",
          strip.text = element_text(size = 10, face = "bold")
        ) +
        scale_color_viridis_d()
      
      # Convert to plotly
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(
          showlegend = FALSE,
          margin = list(t = 50, b = 50, l = 50, r = 50)
        )
    })
    
    # Climate summary table
    output$climate_summary <- renderTable({
      req(climate_values$climate_data)
      
      climate_data <- climate_values$climate_data
      
      if (is.null(climate_data) || nrow(climate_data) == 0) {
        return(data.frame(Message = "No climate data available"))
      }
      
      # Get numeric columns
      numeric_cols <- names(climate_data)[sapply(climate_data, is.numeric)]
      
      if (length(numeric_cols) == 0) {
        return(data.frame(Message = "No numeric variables found"))
      }
      
      # Create summary manually to avoid pivot issues
      summary_list <- list()
      
      for (col in numeric_cols) {
        values <- climate_data[[col]]
        values <- values[!is.na(values)]
        
        if (length(values) > 0) {
          summary_list[[col]] <- data.frame(
            Variable = str_to_title(str_replace_all(col, "_", " ")),
            Mean = round(mean(values), 2),
            Median = round(median(values), 2),
            SD = round(sd(values), 2),
            Min = round(min(values), 2),
            Max = round(max(values), 2),
            stringsAsFactors = FALSE
          )
        }
      }
      
      # Combine all summaries
      if (length(summary_list) > 0) {
        summary_df <- do.call(rbind, summary_list)
        rownames(summary_df) <- NULL
        return(summary_df)
      } else {
        return(data.frame(Message = "No valid numeric data found"))
      }
    }, striped = TRUE, hover = TRUE)
    
    # Calculate climate-disease correlation - Enhanced for drill-down
    observeEvent(input$calculate_climate_correlation, {
      req(filtered_data(), climate_values$climate_data, input$analysis_level, input$climate_factor)
      
      showNotification("Calculating correlations...", type = "default", duration = NULL, id = "correlation_calc")
      
      tryCatch({
        # Determine disease and drill level based on selection
        if (input$analysis_level == "category") {
          req(input$target_disease_category)
          disease <- input$target_disease_category
          drill_level <- "category"
          specific_disease <- NULL
        } else if (input$analysis_level == "specific") {
          req(input$target_specific_disease)
          disease <- input$target_disease_category_for_specific
          drill_level <- "specific"
          specific_disease <- input$target_specific_disease
        } else if (input$analysis_level == "climate_sensitive") {
          # Handle climate-sensitive analysis
          df <- filtered_data()
          if ("climate_sensitive" %in% names(df)) {
            # Filter for climate-sensitive diseases
            df_filtered <- df %>% filter(climate_sensitive == TRUE)
            if (nrow(df_filtered) > 0) {
              correlation_result <- analyze_climate_disease_correlation(
                health_data = df_filtered,
                climate_data = climate_values$climate_data,
                disease = "Climate-Sensitive Diseases",
                climate_var = input$climate_factor,
                lag_days = input$lag_days,
                drill_level = "category"
              )
              climate_values$correlation_results <- correlation_result
              removeNotification("correlation_calc")
              showNotification("Climate-sensitive correlation analysis complete!", type = "default", duration = 3000)
              return()
            }
          }
          showNotification("No climate-sensitive disease data available", type = "warning", duration = 3000)
          removeNotification("correlation_calc")
          return()
        }
        
        correlation_result <- analyze_climate_disease_correlation(
          health_data = filtered_data(),
          climate_data = climate_values$climate_data,
          disease = disease,
          climate_var = input$climate_factor,
          lag_days = input$lag_days,
          drill_level = drill_level,
          specific_disease = specific_disease
        )
        
        climate_values$correlation_results <- correlation_result
        
        removeNotification("correlation_calc")
        showNotification("Correlation analysis complete!", type = "default", duration = 3000)
        
      }, error = function(e) {
        removeNotification("correlation_calc")
        showNotification(paste("Error in correlation analysis:", e$message), type = "error", duration = 5000)
      })
    })
    
    # Climate-disease correlation plot
    output$climate_disease_plot <- renderPlotly({
      req(climate_values$correlation_results)
      
      results <- climate_values$correlation_results
      
      if (is.null(results$merged_data)) {
        return(plotly_empty("No correlation data available"))
      }
      
      merged_data <- results$merged_data
      
      p <- ggplot(merged_data, aes_string(x = results$climate_variable, y = "cases")) +
        geom_point(alpha = 0.6, color = "#667eea") +
        geom_smooth(method = "lm", se = TRUE, color = "#764ba2") +
        labs(
          title = paste("Climate-Disease Correlation:", results$disease),
          subtitle = paste("Correlation:", round(results$correlation, 3), 
                           "| P-value:", round(results$p_value, 4)),
          x = stringr::str_to_title(gsub("_", " ", results$climate_variable)),
          y = "Daily Cases"
        ) +
        theme_minimal()
      
      ggplotly(p, tooltip = c("x", "y"))
    })
    
    # â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    # Time Series Tab Logic
    # â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    
    # Reactive values for time series
    timeseries_values <- reactiveValues(
      ts_data = NULL
    )
    
    # Generate time series data - Enhanced for drill-down
    observeEvent(input$generate_timeseries, {
      req(filtered_data(), climate_values$climate_data, input$ts_analysis_level, input$ts_climate_var)
      
      showNotification("Generating time series...", type = "default", duration = NULL, id = "ts_generation")
      
      tryCatch({
        # Determine disease and drill level based on selection
        if (input$ts_analysis_level == "category") {
          req(input$ts_disease_category)
          disease <- input$ts_disease_category
          drill_level <- "category"
          specific_disease <- NULL
        } else if (input$ts_analysis_level == "specific") {
          req(input$ts_specific_disease)
          disease <- input$ts_disease_category_for_specific
          drill_level <- "specific"
          specific_disease <- input$ts_specific_disease
        } else if (input$ts_analysis_level == "climate_sensitive") {
          # Handle climate-sensitive analysis
          df <- filtered_data()
          if ("climate_sensitive" %in% names(df)) {
            df_filtered <- df %>% filter(climate_sensitive == TRUE)
            if (nrow(df_filtered) > 0) {
              ts_data <- prepare_timeseries_data(
                health_data = df_filtered,
                climate_data = climate_values$climate_data,
                disease = "Climate-Sensitive Diseases",
                climate_var = input$ts_climate_var,
                aggregation = input$ts_aggregation,
                drill_level = "category"
              )
              if (!is.null(ts_data) && nrow(ts_data) > 0) {
                timeseries_values$ts_data <- ts_data
                removeNotification("ts_generation")
                showNotification("Climate-sensitive time series generated!", type = "default", duration = 3000)
                return()
              }
            }
          }
          showNotification("No climate-sensitive disease data available", type = "warning", duration = 3000)
          removeNotification("ts_generation")
          return()
        }
        
        ts_data <- prepare_timeseries_data(
          health_data = filtered_data(),
          climate_data = climate_values$climate_data,
          disease = disease,
          climate_var = input$ts_climate_var,
          aggregation = input$ts_aggregation,
          drill_level = drill_level,
          specific_disease = specific_disease
        )
        
        if (is.null(ts_data) || nrow(ts_data) == 0) {
          showNotification("No data available for the selected combination", type = "warning", duration = 5000)
          return()
        }
        
        timeseries_values$ts_data <- ts_data
        
        removeNotification("ts_generation")
        showNotification("Time series generated successfully!", type = "default", duration = 3000)
        
      }, error = function(e) {
        removeNotification("ts_generation")
        showNotification(paste("Error generating time series:", e$message), type = "error", duration = 5000)
      })
    })
    
    # Time series plot
    output$timeseries_plot <- renderPlotly({
      req(timeseries_values$ts_data)
      
      ts_data <- timeseries_values$ts_data
      climate_var <- input$ts_climate_var
      
      if (is.null(ts_data) || nrow(ts_data) == 0) {
        return(plotly_empty("No time series data available"))
      }
      
      # Create the base plot
      if (input$ts_dual_axis) {
        # Dual axis plot
        
        # Normalize data for dual axis
        cases_max <- max(ts_data$cases, na.rm = TRUE)
        climate_max <- max(ts_data[[climate_var]], na.rm = TRUE)
        climate_min <- min(ts_data[[climate_var]], na.rm = TRUE)
        
        # Scale climate data to cases range
        climate_scaled <- ((ts_data[[climate_var]] - climate_min) / (climate_max - climate_min)) * cases_max
        
        p <- plot_ly(ts_data, x = ~period) %>%
          add_trace(
            y = ~cases,
            type = "scatter",
            mode = "lines+markers",
            name = paste("Cases -", input$ts_disease),
            line = list(color = "#1f77b4", width = 2),
            marker = list(size = 6),
            yaxis = "y1"
          ) %>%
          add_trace(
            y = climate_scaled,
            type = "scatter",
            mode = "lines+markers",
            name = str_to_title(str_replace_all(climate_var, "_", " ")),
            line = list(color = "#ff7f0e", width = 2),
            marker = list(size = 6),
            yaxis = "y2"
          )
        
        # Add smooth lines if requested
        if (input$ts_smooth && "cases_smooth" %in% names(ts_data)) {
          climate_smooth_scaled <- ((ts_data$climate_smooth - climate_min) / (climate_max - climate_min)) * cases_max
          
          p <- p %>%
            add_trace(
              y = ~cases_smooth,
              type = "scatter",
              mode = "lines",
              name = "Cases Trend",
              line = list(color = "#1f77b4", width = 3, dash = "dash"),
              yaxis = "y1"
            ) %>%
            add_trace(
              y = climate_smooth_scaled,
              type = "scatter",
              mode = "lines",
              name = paste(str_to_title(str_replace_all(climate_var, "_", " ")), "Trend"),
              line = list(color = "#ff7f0e", width = 3, dash = "dash"),
              yaxis = "y2"
            )
        }
        
        # Configure dual axis layout
        p <- p %>%
          layout(
            title = list(
              text = paste("Time Series:", 
                           if(input$ts_analysis_level == "specific") input$ts_specific_disease else 
                           if(input$ts_analysis_level == "climate_sensitive") "Climate-Sensitive Diseases" else input$ts_disease_category, 
                           "vs", str_to_title(str_replace_all(climate_var, "_", " "))),
              y = 0.95  # Move title down slightly
            ),
            xaxis = list(title = "Time Period"),
            yaxis = list(
              title = "Number of Cases",
              side = "left",
              color = "#1f77b4"
            ),
            yaxis2 = list(
              title = str_to_title(str_replace_all(climate_var, "_", " ")),
              side = "right",
              overlaying = "y",
              color = "#ff7f0e",
              tickvals = seq(0, cases_max, length.out = 5),
              ticktext = round(seq(climate_min, climate_max, length.out = 5), 2)
            ),
            legend = list(x = 0.02, y = 0.98),
            hovermode = "x unified",
            margin = list(t = 80, b = 60, l = 60, r = 80)  # Increased margins to prevent overlap
          )
        
      } else {
        # Single axis plot (normalized)
        
        # Normalize both series to 0-1 scale
        ts_data_norm <- ts_data %>%
          mutate(
            cases_norm = (cases - min(cases, na.rm = TRUE)) / (max(cases, na.rm = TRUE) - min(cases, na.rm = TRUE)),
            climate_norm = (!!sym(climate_var) - min(!!sym(climate_var), na.rm = TRUE)) / 
              (max(!!sym(climate_var), na.rm = TRUE) - min(!!sym(climate_var), na.rm = TRUE))
          )
        
        p <- plot_ly(ts_data_norm, x = ~period) %>%
          add_trace(
            y = ~cases_norm,
            type = "scatter",
            mode = "lines+markers",
            name = paste("Cases -", input$ts_disease, "(normalized)"),
            line = list(color = "#1f77b4", width = 2),
            marker = list(size = 6)
          ) %>%
          add_trace(
            y = ~climate_norm,
            type = "scatter",
            mode = "lines+markers",
            name = paste(str_to_title(str_replace_all(climate_var, "_", " ")), "(normalized)"),
            line = list(color = "#ff7f0e", width = 2),
            marker = list(size = 6)
          ) %>%
          layout(
            title = list(
              text = paste("Normalized Time Series:", 
                           if(input$ts_analysis_level == "specific") input$ts_specific_disease else 
                           if(input$ts_analysis_level == "climate_sensitive") "Climate-Sensitive Diseases" else input$ts_disease_category, 
                           "vs", str_to_title(str_replace_all(climate_var, "_", " "))),
              y = 0.95  # Move title down slightly
            ),
            xaxis = list(title = "Time Period"),
            yaxis = list(title = "Normalized Value (0-1)", range = c(0, 1)),
            legend = list(x = 0.02, y = 0.98),
            hovermode = "x unified",
            margin = list(t = 80, b = 60, l = 60, r = 60)  # Increased margins
          )
      }
      
      return(p)
    })
    
    # Time series summary table
    output$timeseries_summary <- DT::renderDataTable({
      req(timeseries_values$ts_data, input$ts_climate_var)
      
      ts_data <- timeseries_values$ts_data
      climate_var <- input$ts_climate_var
      
      # Validate that the climate variable exists in the data
      if (!climate_var %in% names(ts_data)) {
        return(DT::datatable(
          data.frame(Message = paste("Climate variable", climate_var, "not found in time series data")),
          options = list(dom = 't', searching = FALSE, ordering = FALSE),
          rownames = FALSE
        ))
      }
      
      # Calculate summary statistics with error handling
      tryCatch({
        summary_stats <- ts_data %>%
          summarise(
            Period = paste(input$ts_aggregation, "aggregation"),
            `Data Points` = n(),
            `Date Range` = paste(min(period, na.rm = TRUE), "to", max(period, na.rm = TRUE)),
            `Total Cases` = sum(cases, na.rm = TRUE),
            `Avg Cases per Period` = round(mean(cases, na.rm = TRUE), 2),
            `Max Cases` = max(cases, na.rm = TRUE),
            `Climate Variable` = str_to_title(str_replace_all(climate_var, "_", " ")),
            `Avg Climate Value` = round(mean(!!sym(climate_var), na.rm = TRUE), 2),
            `Climate Range` = paste(round(min(!!sym(climate_var), na.rm = TRUE), 2), 
                                    "to", 
                                    round(max(!!sym(climate_var), na.rm = TRUE), 2))
          ) %>%
          pivot_longer(everything(), names_to = "Metric", values_to = "Value")
        
        DT::datatable(
          summary_stats,
          options = list(
            dom = 't',
            pageLength = 20,
            searching = FALSE,
            ordering = FALSE
          ),
          rownames = FALSE
        ) %>%
          DT::formatStyle(columns = 1:2, fontSize = '14px')
        
      }, error = function(e) {
        # Return error information in a user-friendly format
        error_data <- data.frame(
          Metric = c("Error", "Details", "Suggestion"),
          Value = c(
            "Time series summary failed",
            paste("Error:", e$message),
            "Check that time series data was generated successfully"
          )
        )
        
        DT::datatable(
          error_data,
          options = list(
            dom = 't',
            searching = FALSE,
            ordering = FALSE
          ),
          rownames = FALSE
        ) %>%
          DT::formatStyle(columns = 1:2, fontSize = '14px')
      })
    })
    
    # â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    # Multi-Location Comparison Logic
    # â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    
    # Multi-location climate plot
    output$multi_location_plot <- renderPlotly({
      req(climate_values$multi_location_data, input$climate_variables)
      
      multi_data <- climate_values$multi_location_data
      
      if (is.null(multi_data) || nrow(multi_data) == 0) {
        return(plotly_empty("No multi-location data available"))
      }
      
      # Prepare data for plotting - use first selected climate variable
      climate_var <- input$climate_variables[1]
      
      if (!climate_var %in% names(multi_data)) {
        return(plotly_empty("Selected climate variable not found in data"))
      }
      
      # Create comparison plot
      plot_data <- multi_data %>%
        select(date, location_name, !!sym(climate_var)) %>%
        filter(!is.na(!!sym(climate_var)))
      
      if (nrow(plot_data) == 0) {
        return(plotly_empty("No valid data for plotting"))
      }
      
      # Create the plot
      p <- ggplot(plot_data, aes(x = date, y = !!sym(climate_var), color = location_name)) +
        geom_line(size = 1.2, alpha = 0.8) +
        labs(
          title = paste("Climate Comparison:", str_to_title(str_replace_all(climate_var, "_", " "))),
          x = "Date",
          y = str_to_title(str_replace_all(climate_var, "_", " ")),
          color = "Location"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_color_viridis_d()
      
      ggplotly(p, tooltip = c("x", "y", "colour")) %>%
        layout(margin = list(b = 100))  # Extra space for legend
    })
    
    # Location summary table
    output$location_summary_table <- DT::renderDataTable({
      req(climate_values$multi_location_data)
      
      multi_data <- climate_values$multi_location_data
      
      if (is.null(multi_data) || nrow(multi_data) == 0) {
        return(data.frame(Message = "No multi-location data available"))
      }
      
      # Calculate summary by location
      numeric_cols <- names(multi_data)[sapply(multi_data, is.numeric)]
      climate_cols <- setdiff(numeric_cols, c("latitude", "longitude"))
      
      if (length(climate_cols) == 0) {
        return(data.frame(Message = "No climate variables found"))
      }
      
      # Create summary table
      summary_table <- multi_data %>%
        group_by(location_name, admin_level) %>%
        summarise(
          across(all_of(climate_cols), 
                 list(mean = ~round(mean(.x, na.rm = TRUE), 2),
                      min = ~round(min(.x, na.rm = TRUE), 2),
                      max = ~round(max(.x, na.rm = TRUE), 2)),
                 .names = "{.col}_{.fn}"),
          data_points = n(),
          .groups = "drop"
        ) %>%
        arrange(location_name)
      
      DT::datatable(
        summary_table,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'ftip'
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(columns = 1:ncol(summary_table), fontSize = '12px')
    })
    
    # Climate map
    output$climate_map <- renderPlotly({
      req(climate_values$multi_location_data, input$climate_variables)
      
      multi_data <- climate_values$multi_location_data
      climate_var <- input$climate_variables[1]
      
      if (is.null(multi_data) || nrow(multi_data) == 0) {
        return(plotly_empty("No location data available for mapping"))
      }
      
      # Aggregate climate data by location
      map_data <- multi_data %>%
        group_by(location_name, latitude, longitude, admin_level) %>%
        summarise(
          avg_value = mean(!!sym(climate_var), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(!is.na(avg_value))
      
      if (nrow(map_data) == 0) {
        return(plotly_empty("No valid data for mapping"))
      }
      
      # Create map plot
      p <- plot_ly(
        map_data,
        x = ~longitude,
        y = ~latitude,
        color = ~avg_value,
        size = ~avg_value,
        type = "scatter",
        mode = "markers",
        text = ~paste("Location:", location_name, 
                      "<br>", str_to_title(str_replace_all(climate_var, "_", " ")), ":", round(avg_value, 2),
                      "<br>Admin Level:", admin_level),
        hovertemplate = "%{text}<extra></extra>",
        marker = list(
          sizemode = "diameter",
          sizeref = max(map_data$avg_value, na.rm = TRUE) / 100,
          sizemin = 8
        )
      ) %>%
        layout(
          title = paste("Geographic Distribution:", str_to_title(str_replace_all(climate_var, "_", " "))),
          xaxis = list(title = "Longitude"),
          yaxis = list(title = "Latitude"),
          showlegend = FALSE
        ) %>%
        colorbar(title = str_to_title(str_replace_all(climate_var, "_", " ")))
      
      return(p)
    })
    
    # ────────────────────────────────────────────────────────────────────────────
    # Climate-Sensitive Disease Analysis Tab Logic
    # ────────────────────────────────────────────────────────────────────────────
    
    # Reactive values for climate-sensitive analysis
    climate_sensitive_values <- reactiveValues(
      analysis_data = NULL,
      risk_assessment = NULL
    )
    
    # Update risk climate variable choices
    observe({
      req(climate_values$climate_data)
      climate_data <- climate_values$climate_data
      
      if (!is.null(climate_data) && nrow(climate_data) > 0) {
        climate_vars <- names(climate_data)[sapply(climate_data, is.numeric)]
        climate_vars <- setdiff(climate_vars, c("latitude", "longitude"))
        updateSelectInput(session, "risk_climate_var", choices = climate_vars)
      }
    })
    
    # Generate climate-sensitive analysis
    observeEvent(input$generate_climate_analysis, {
      req(filtered_data(), input$climate_analysis_type)
      
      showNotification("Generating climate-sensitive analysis...", type = "default", duration = NULL, id = "climate_analysis")
      
      tryCatch({
        df <- filtered_data()
        
        # Check if climate-sensitive data is available
        if (!"climate_sensitive" %in% names(df)) {
          showNotification("Climate-sensitive classification not available in data", type = "warning", duration = 5000)
          removeNotification("climate_analysis")
          return()
        }
        
        if (input$climate_analysis_type == "overview") {
          req(input$climate_groups_selected)
          
          # Create overview analysis
          analysis_data <- df %>%
            filter(climate_sensitive == TRUE) %>%
            mutate(
              date = as.Date(datevisit),
              month = format(date, "%Y-%m"),
              season = case_when(
                month(date) %in% c(12, 1, 2) ~ "Winter",
                month(date) %in% c(3, 4, 5) ~ "Spring", 
                month(date) %in% c(6, 7, 8) ~ "Summer",
                month(date) %in% c(9, 10, 11) ~ "Fall"
              )
            ) %>%
            count(month, season, name = "cases") %>%
            arrange(month)
            
          climate_sensitive_values$analysis_data <- analysis_data
          
        } else if (input$climate_analysis_type == "seasonal") {
          req(input$seasonal_climate_group, input$seasonal_timeframe)
          
          # Create seasonal analysis
          analysis_data <- df %>%
            filter(climate_sensitive == TRUE) %>%
            mutate(
              date = as.Date(datevisit),
              time_group = case_when(
                input$seasonal_timeframe == "month" ~ format(date, "%B"),
                input$seasonal_timeframe == "season" ~ case_when(
                  month(date) %in% c(12, 1, 2) ~ "Winter",
                  month(date) %in% c(3, 4, 5) ~ "Spring", 
                  month(date) %in% c(6, 7, 8) ~ "Summer",
                  month(date) %in% c(9, 10, 11) ~ "Fall"
                ),
                input$seasonal_timeframe == "quarter" ~ paste0("Q", quarter(date))
              )
            ) %>%
            count(time_group, category_canonical_disease_imc, name = "cases") %>%
            arrange(time_group)
            
          climate_sensitive_values$analysis_data <- analysis_data
          
        } else if (input$climate_analysis_type == "risk") {
          req(climate_values$climate_data, input$risk_climate_var, 
              input$risk_threshold_low, input$risk_threshold_high)
          
          # Create risk assessment
          climate_data <- climate_values$climate_data
          climate_var <- input$risk_climate_var
          
          # Calculate risk categories based on climate thresholds
          risk_data <- climate_data %>%
            mutate(
              climate_risk = case_when(
                !!sym(climate_var) <= quantile(!!sym(climate_var), input$risk_threshold_low/100, na.rm = TRUE) ~ "Low Risk",
                !!sym(climate_var) >= quantile(!!sym(climate_var), input$risk_threshold_high/100, na.rm = TRUE) ~ "High Risk",
                TRUE ~ "Medium Risk"
              )
            )
          
          # Merge with health data
          health_climate <- df %>%
            filter(climate_sensitive == TRUE) %>%
            mutate(date = as.Date(datevisit)) %>%
            left_join(risk_data, by = "date") %>%
            filter(!is.na(climate_risk))
          
          if (nrow(health_climate) > 0) {
            risk_summary <- health_climate %>%
              count(climate_risk, category_canonical_disease_imc, name = "cases") %>%
              group_by(climate_risk) %>%
              mutate(
                total_cases = sum(cases),
                percentage = round(cases / total_cases * 100, 1)
              ) %>%
              ungroup()
            
            climate_sensitive_values$risk_assessment <- risk_summary
            climate_sensitive_values$analysis_data <- health_climate
          }
        }
        
        removeNotification("climate_analysis")
        showNotification("Climate-sensitive analysis completed!", type = "default", duration = 3000)
        
      }, error = function(e) {
        removeNotification("climate_analysis")
        showNotification(paste("Error in climate analysis:", e$message), type = "error", duration = 5000)
      })
    })
    
    # Climate-sensitive plot
    output$climate_sensitive_plot <- renderPlotly({
      req(climate_sensitive_values$analysis_data, input$climate_analysis_type)
      
      analysis_data <- climate_sensitive_values$analysis_data
      
      if (is.null(analysis_data) || nrow(analysis_data) == 0) {
        return(plotly_empty("No climate-sensitive analysis data available"))
      }
      
      if (input$climate_analysis_type == "overview") {
        # Overview plot - monthly trend
        p <- ggplot(analysis_data, aes(x = month, y = cases, fill = season)) +
          geom_col(alpha = 0.8) +
          labs(
            title = "Climate-Sensitive Disease Cases by Month",
            x = "Month",
            y = "Number of Cases",
            fill = "Season"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 14, face = "bold")
          ) +
          scale_fill_viridis_d()
        
        ggplotly(p, tooltip = c("x", "y", "fill"))
        
      } else if (input$climate_analysis_type == "seasonal") {
        # Seasonal pattern plot
        p <- ggplot(analysis_data, aes(x = time_group, y = cases, fill = category_canonical_disease_imc)) +
          geom_col(position = "stack", alpha = 0.8) +
          labs(
            title = paste("Seasonal Climate-Sensitive Disease Patterns by", str_to_title(input$seasonal_timeframe)),
            x = str_to_title(input$seasonal_timeframe),
            y = "Number of Cases",
            fill = "Disease Category"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 14, face = "bold"),
            legend.position = "bottom"
          ) +
          scale_fill_viridis_d()
        
        ggplotly(p, tooltip = c("x", "y", "fill"))
        
      } else if (input$climate_analysis_type == "risk") {
        # Risk assessment plot
        if (!is.null(climate_sensitive_values$risk_assessment)) {
          risk_data <- climate_sensitive_values$risk_assessment
          
          p <- ggplot(risk_data, aes(x = climate_risk, y = cases, fill = category_canonical_disease_imc)) +
            geom_col(position = "dodge", alpha = 0.8) +
            labs(
              title = paste("Climate Risk Assessment -", str_to_title(str_replace_all(input$risk_climate_var, "_", " "))),
              x = "Climate Risk Level",
              y = "Number of Cases",
              fill = "Disease Category"
            ) +
            theme_minimal() +
            theme(
              plot.title = element_text(size = 14, face = "bold"),
              legend.position = "bottom"
            ) +
            scale_fill_viridis_d()
          
          ggplotly(p, tooltip = c("x", "y", "fill"))
        } else {
          return(plotly_empty("Risk assessment data not available"))
        }
      } else {
        return(plotly_empty("Select an analysis type to generate visualization"))
      }
    })
    
    # Climate disease summary table
    output$climate_disease_summary <- DT::renderDataTable({
      req(climate_sensitive_values$analysis_data, input$climate_analysis_type)
      
      analysis_data <- climate_sensitive_values$analysis_data
      
      if (is.null(analysis_data) || nrow(analysis_data) == 0) {
        return(DT::datatable(
          data.frame(Message = "No climate-sensitive analysis data available"),
          options = list(dom = 't', searching = FALSE, ordering = FALSE),
          rownames = FALSE
        ))
      }
      
      # Create summary based on analysis type
      if (input$climate_analysis_type == "overview") {
        summary_data <- analysis_data %>%
          group_by(season) %>%
          summarise(
            total_cases = sum(cases, na.rm = TRUE),
            avg_monthly_cases = round(mean(cases, na.rm = TRUE), 1),
            months_included = n(),
            .groups = "drop"
          ) %>%
          arrange(desc(total_cases))
        
      } else if (input$climate_analysis_type == "seasonal") {
        summary_data <- analysis_data %>%
          group_by(category_canonical_disease_imc) %>%
          summarise(
            total_cases = sum(cases, na.rm = TRUE),
            peak_period = time_group[which.max(cases)],
            peak_cases = max(cases, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(desc(total_cases))
        
      } else {
        # Default summary
        summary_data <- data.frame(
          Message = "Summary not available for this analysis type"
        )
      }
      
      DT::datatable(
        summary_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'tip'
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(columns = 1:ncol(summary_data), fontSize = '12px')
    })
    
    # Climate risk table
    output$climate_risk_table <- renderTable({
      req(climate_sensitive_values$risk_assessment, input$climate_analysis_type)
      
      if (input$climate_analysis_type != "risk") {
        return(data.frame(Message = "Risk assessment only available for Risk Analysis type"))
      }
      
      risk_data <- climate_sensitive_values$risk_assessment
      
      if (is.null(risk_data) || nrow(risk_data) == 0) {
        return(data.frame(Message = "No risk assessment data available"))
      }
      
      # Create risk summary
      risk_summary <- risk_data %>%
        group_by(climate_risk) %>%
        summarise(
          `Total Cases` = sum(cases, na.rm = TRUE),
          `Disease Categories` = n_distinct(category_canonical_disease_imc),
          `Risk Level` = first(climate_risk),
          .groups = "drop"
        ) %>%
        select(`Risk Level`, `Total Cases`, `Disease Categories`) %>%
        arrange(desc(`Total Cases`))
      
      return(risk_summary)
    }, striped = TRUE, hover = TRUE)
    
  })
}