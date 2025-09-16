# ai_core.R - Core AI functionality and initialization
# Consolidated from modules/ai_config.R and R/enhanced_ai.R
# VERSION: 1.0.0

library(dplyr)

# =============================================================================
# CORE AI CONFIGURATION AND INITIALIZATION
# =============================================================================

#' Configure enhanced AI system with data context
#' @param config Main config list
#' @param params Report parameters
#' @param register Optional health register data for context
#' @return AI system configuration
configure_enhanced_ai <- function(config, params, register = NULL) {
  tryCatch({
    # Load and validate AI customization
    ai_config_file <- here::here("config", "ai_customization.yml")
    if (file.exists(ai_config_file)) {
      ai_customization <- yaml::read_yaml(ai_config_file)
    } else {
      ai_customization <- list()
    }
    
    # Build base AI configuration
    ai_config <- build_ai_config(ai_customization, register, config)
    
    # Check if AI features are enabled in main config
    ai_enabled <- isTRUE(config$ai_features$enabled) || isTRUE(config$ai$enabled)
    
    provider <- tolower(ai_config$api$provider %||% config$ai_features$provider %||% "ollama")
    ai_connection_test <- NULL
    if (ai_enabled && !is.null(ai_config$api$api_endpoint)) {
      if (provider == "openai") {
        ai_connection_test <- list(success = TRUE, message = "OpenAI connection not pre-tested")
      } else {
        ai_connection_test <- test_ai_connection(ai_config$api$api_endpoint, ai_config$api$model)
        if (!ai_connection_test$success) {
          cat("⚠️ AI API connection test failed:", ai_connection_test$message, "- will try fallback during actual calls\n")
        } else {
          cat("✅ AI API connection test successful\n")
        }
      }
    }
    
    return(list(
      available = ai_enabled,
      enhanced = ai_enabled,  # Set enhanced flag same as available
      config = ai_config,
      customization = ai_customization,
      register_context = if(!is.null(register)) get_register_context_summary(register) else NULL,
      connection_test = ai_connection_test
    ))
  }, error = function(e) {
    warning("AI configuration error: ", e$message)
    return(list(
      available = FALSE,
      enhanced = FALSE,
      config = NULL,
      error_message = e$message
    ))
  })
}

#' Safe AI initialization with data context
#' @param config Main config
#' @param params Parameters
#' @param register Health register data
#' @return Initialized AI system
safe_initialize_ai_with_data <- function(config, params, register = NULL) {
  tryCatch({
    configure_enhanced_ai(config, params, register)
  }, error = function(e) {
    list(
      available = FALSE,
      enhanced = FALSE,
      config = NULL,
      error_message = paste("AI initialization failed:", e$message)
    )
  })
}

#' Build AI configuration with epidemiological context
#' @param customization User customization settings
#' @param register Optional register data for context
#' @param main_config Main system configuration
#' @return AI configuration list
build_ai_config <- function(customization = list(), register = NULL, main_config = NULL) {
  # Base epidemiological context
  epi_context <- list(
    setting = customization$epidemiological_context$setting %||% "humanitarian health surveillance",
    population = customization$epidemiological_context$population %||% "conflict-affected populations",
    priorities = customization$epidemiological_context$surveillance_priorities %||% 
      c("epidemic diseases", "trauma surveillance", "malnutrition monitoring"),
    constraints = customization$epidemiological_context$operational_constraints %||%
      c("limited resources", "security challenges", "displaced populations")
  )
  
  # Add register context if available
  if (!is.null(register)) {
    register_ctx <- get_register_context_summary(register)
    epi_context$data_summary <- register_ctx
  }
  
  provider <- tolower(main_config$ai_features$provider %||% "ollama")
  if (provider == "openai") {
    base_url <- main_config$ai_features$openai$base_url %||% "https://api.openai.com/v1/chat/completions"
    api_config <- list(
      provider = "openai",
      api_endpoint = base_url,
      model = main_config$ai_features$openai$model %||% "gpt-4o-mini",
      max_tokens = main_config$ai_features$openai$max_tokens %||% 800,
      temperature = main_config$ai_features$openai$temperature %||% 0.2,
      timeout = main_config$ai_features$openai$timeout %||% 120,
      api_key = Sys.getenv("OPENAI_API_KEY", ""),
      system_prompt = main_config$epibot_features$system_prompt %||%
        "You are EpiBot, an AI assistant specializing in epidemiological data analysis."
    )
    if (!nzchar(api_config$api_key)) {
      warning("OPENAI_API_KEY environment variable not set. OpenAI calls will fail without it.")
    }
  } else {
    host <- main_config$ai_features$ollama$host %||% "http://localhost:11434"
    api_config <- list(
      provider = "ollama",
      api_endpoint = main_config$ai_features$ollama$host %||% 
                    customization$ai_settings$api_endpoint %||% 
                    "http://localhost:11434/api/generate",
      model = main_config$ai_features$ollama$model %||% 
             customization$ai_settings$model %||% 
             "llama3.2:3b",
      max_tokens = main_config$ai_features$ollama$max_tokens %||%
                  customization$ai_settings$max_tokens %||% 
                  2048,
      temperature = main_config$ai_features$ollama$temperature %||%
                   customization$ai_settings$temperature %||% 
                   0.3,
      timeout = main_config$ai_features$ollama$timeout %||% 120,
      host = host,
      system_prompt = main_config$epibot_features$system_prompt %||%
        "You are EpiBot, an AI assistant specializing in epidemiological data analysis."
    )
  }
  
  return(list(
    epi_context = epi_context,
    api = api_config,
    diseases = customization$disease_lists %||% list(),
    prompts = customization$prompts %||% list()
  ))
}

#' Extract context summary from health register
#' @param register Health register data
#' @return Context summary list
get_register_context_summary <- function(register) {
  if (is.null(register) || nrow(register) == 0) {
    return(list(
      total_cases = 0,
      date_range = "No data",
      top_diseases = character(0),
      regions = character(0)
    ))
  }
  
  # Detect key columns
  disease_col <- dplyr::case_when(
    "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
    "morbidity" %in% names(register) ~ "morbidity",
    "icd11_title" %in% names(register) ~ "icd_11_title",
    "category_canonical_disease_imc" %in% names(register) ~ "category_canonical_disease_imc",
    "disease" %in% names(register) ~ "disease",
    TRUE ~ NA_character_
  )
  
  region_col <- dplyr::case_when(
    "admin1" %in% names(register) ~ "admin1",
    "region" %in% names(register) ~ "region",
    "governorate" %in% names(register) ~ "governorate",
    TRUE ~ NA_character_
  )
  
  date_col <- dplyr::case_when(
    "datevisit" %in% names(register) ~ "datevisit",
    "date" %in% names(register) ~ "date",
    "visit_date" %in% names(register) ~ "visit_date",
    TRUE ~ NA_character_
  )
  
  # Extract summary information
  summary <- list(
    total_cases = nrow(register),
    date_range = "Unknown period",
    top_diseases = character(0),
    regions = character(0)
  )
  
  # Date range
  if (!is.na(date_col) && date_col %in% names(register)) {
    date_data <- register[[date_col]]
    if (any(!is.na(date_data))) {
      date_range <- range(date_data, na.rm = TRUE)
      summary$date_range <- paste(format(date_range[1], "%Y-%m"), "to", format(date_range[2], "%Y-%m"))
    }
  }
  
  # Top diseases
  if (!is.na(disease_col) && disease_col %in% names(register)) {
    top_diseases <- register %>%
      dplyr::count(.data[[disease_col]], sort = TRUE) %>%
      dplyr::slice_head(n = 5) %>%
      dplyr::pull(.data[[disease_col]])
    summary$top_diseases <- top_diseases
  }
  
  # Regions
  if (!is.na(region_col) && region_col %in% names(register)) {
    regions <- unique(register[[region_col]])
    regions <- regions[!is.na(regions)]
    summary$regions <- head(regions, 10)
  }
  
  return(summary)
}

#' Test AI API connection
#' @param host API endpoint
#' @param model Model name
#' @return List with success status and message
test_ai_connection <- function(host, model) {
  tryCatch({
    # Simple test query
    body <- list(
      model = model,
      prompt = "Test connection. Respond with: OK",
      stream = FALSE,
      options = list(temperature = 0.1, num_predict = 10)
    )
    
    # Use the full endpoint if it already includes /api/generate, otherwise append it
    endpoint <- if (grepl("/api/generate", host)) host else paste0(host, "/api/generate")
    
    response <- httr::POST(
      url = endpoint,
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      httr::add_headers(`Content-Type` = "application/json"),
      httr::timeout(10)  # Short timeout for connection test
    )
    
    if (httr::status_code(response) == 200) {
      return(list(success = TRUE, message = "Connection successful"))
    } else {
      return(list(success = FALSE, message = paste("HTTP", httr::status_code(response))))
    }
  }, error = function(e) {
    return(list(success = FALSE, message = e$message))
  })
}

#' Safe logical check with fallback
#' @param value Value to check
#' @param fallback Fallback value if check fails
#' @return Single logical value
safe_logical_check <- function(value, fallback = FALSE) {
  tryCatch({
    if (is.logical(value) && length(value) == 1 && !is.na(value)) {
      return(value)
    }
    return(fallback)
  }, error = function(e) {
    return(fallback)
  })
}

#' Summarize register for AI analysis
#' @param register Health register data
#' @return Summary list for AI prompts
summarize_register_for_ai <- function(register) {
  if (is.null(register) || nrow(register) == 0) {
    return(list(
      case_count = 0,
      facilities = 0,
      regions = 0,
      diseases = 0,
      recent_trends = "No data available"
    ))
  }
  
  # Basic counts
  basic_summary <- list(
    case_count = nrow(register),
    facilities = dplyr::n_distinct(register$health_facility, na.rm = TRUE),
    regions = dplyr::n_distinct(register$admin1, na.rm = TRUE),
    diseases = dplyr::n_distinct(register$canonical_disease_imc, na.rm = TRUE)
  )
  
  # Recent trends (if date available)
  if ("datevisit" %in% names(register)) {
    recent_data <- register %>%
      dplyr::filter(!is.na(datevisit)) %>%
      dplyr::arrange(dplyr::desc(datevisit))
    
    if (nrow(recent_data) > 0) {
      last_month <- recent_data %>%
        dplyr::filter(datevisit >= (max(datevisit, na.rm = TRUE) - 30)) %>%
        nrow()
      
      basic_summary$recent_trends <- paste("Recent month activity:", last_month, "cases")
    } else {
      basic_summary$recent_trends <- "No recent trend data available"
    }
  } else {
    basic_summary$recent_trends <- "Date information not available"
  }
  
  return(basic_summary)
}

cat("✅ AI Core module loaded successfully\n")
