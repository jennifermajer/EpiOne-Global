# modules/ai_config.R
# AI Configuration and Integration Module
# Provides enhanced AI configuration with robust error handling and user customization
#
# REGISTER INTEGRATION: Key features:
# - Extracts disease patterns, date ranges, and geographic coverage from actual data
# - Adapts endemic disease lists to diseases present in the dataset
# - Adjusts AI prompts based on actual consultation patterns and demographics
# - Merges user customization from ai_customization.yml with data-driven context

library(httr)
library(jsonlite)
library(dplyr)
library(yaml)

#' Configure Enhanced AI System
#' 
#' Initializes the enhanced AI system with robust error handling and fallback options
#' 
#' @param config Configuration object from config.yml
#' @param params Report parameters
#' @param register Optional health register data for context-aware configuration
#' @return List containing AI system configuration and status
#' 
configure_enhanced_ai <- function(config, params, register = NULL) {
  
  cat("🤖 Initializing Enhanced AI System with User Customization...\n")
  
  # Load user customization first
  config <- load_user_ai_customization(config)
  
  # Initialize AI system status
  ai_system <- list(
    available = FALSE,
    enhanced = FALSE,
    provider = NULL,
    model = NULL,
    host = NULL,
    error_message = NULL,
    fallback_mode = FALSE,
    config = list(),
    user_customization = config$user_customization,
    register_context = if(!is.null(register)) get_register_context_summary(register) else NULL
  )
  
  # Check if AI is enabled in parameters
  if (!isTRUE(params$enable_ai)) {
    ai_system$error_message <- "AI disabled in report parameters"
    cat("ℹ️ AI features disabled via report parameters\n")
    return(ai_system)
  }
  
  # Check if AI is configured
  if (is.null(config$ai_features) || !isTRUE(config$ai_features$enabled)) {
    ai_system$error_message <- "AI not enabled in configuration"
    cat("ℹ️ AI features not enabled in configuration\n")
    return(ai_system)
  }
  
  # Test Ollama connection
  ollama_test <- test_ollama_connection(config)
  
  if (!ollama_test$connected) {
    ai_system$error_message <- ollama_test$error
    cat("❌ Failed to connect to Ollama:", ollama_test$error, "\n")
    return(ai_system)
  }
  
  # Test model availability with fallback
  model_test <- test_model_availability_with_fallback(config, ollama_test$available_models)
  
  if (!model_test$available) {
    ai_system$error_message <- model_test$error
    cat("❌", model_test$error, "\n")
    return(ai_system)
  }
  
  # Configure AI system
  ai_system$available <- TRUE
  ai_system$enhanced <- TRUE
  ai_system$provider <- "ollama"
  ai_system$model <- model_test$selected_model
  ai_system$host <- config$ai_features$ollama$host %||% "http://localhost:11434"
  ai_system$fallback_mode <- model_test$is_fallback
  
  # Build configuration with register context
  ai_system$config <- build_ai_config(config, model_test$selected_model, ai_system$register_context)
  
  cat("✅ Enhanced AI System initialized successfully\n")
  cat("🎯 Provider: ollama | Model:", model_test$selected_model)
  if (model_test$is_fallback) {
    cat(" (fallback model)")
  }
  cat("\n")
  
  return(ai_system)
}

#' Test Ollama Connection
#' 
#' Tests connection to Ollama server and gets available models
#' 
#' @param config Configuration object
#' @return List with connection status and available models
#' 
test_ollama_connection <- function(config) {
  
  host <- config$ai_features$ollama$host %||% "http://localhost:11434"
  
  tryCatch({
    # Test basic connection
    response <- GET(paste0(host, "/api/tags"), timeout(10))
    
    if (status_code(response) != 200) {
      return(list(
        connected = FALSE,
        error = paste("Ollama server returned status:", status_code(response))
      ))
    }
    
    # Parse available models
    content_text <- content(response, "text", encoding = "UTF-8")
    models_data <- fromJSON(content_text)
    
    if (is.null(models_data$models) || length(models_data$models) == 0) {
      return(list(
        connected = TRUE,
        available_models = character(0),
        error = "No models installed in Ollama"
      ))
    }
    
    # Extract model names
    available_models <- if (is.data.frame(models_data$models)) {
      models_data$models$name
    } else {
      sapply(models_data$models, function(x) x$name %||% x$model %||% "unknown")
    }
    
    cat("🔗 Connected to Ollama successfully\n")
    cat("📋 Available models:", paste(available_models, collapse = ", "), "\n")
    
    return(list(
      connected = TRUE,
      available_models = available_models
    ))
    
  }, error = function(e) {
    return(list(
      connected = FALSE,
      error = paste("Connection failed:", e$message)
    ))
  })
}

#' Test Model Availability with Fallback
#' 
#' Tests for requested model and provides fallback options
#' 
#' @param config Configuration object
#' @param available_models Vector of available model names
#' @return List with model availability and selection
#' 
test_model_availability_with_fallback <- function(config, available_models) {
  
  # Get requested model
  requested_model <- config$ai_features$ollama$model %||% "llama3.2:3b"
  
  # Priority list of models to try (in order of preference)
  fallback_models <- c(
    requested_model,
    "llama3.2:3b",
    "llama3.2:1b", 
    "llama3.2",
    "llama3:8b",
    "llama3",
    "llama2:7b",
    "llama2",
    "mistral:7b",
    "mistral",
    "tinyllama",
    "phi3:mini",
    "phi3"
  )
  
  # Test each model in priority order
  for (model in fallback_models) {
    # Check if model exists (partial matching)
    model_match <- available_models[grepl(paste0("^", gsub(":", ".*", model)), available_models)]
    
    if (length(model_match) > 0) {
      selected_model <- model_match[1]  # Use first match
      
      # Test if model actually works
      test_result <- test_model_functionality(config, selected_model)
      
      if (test_result$working) {
        cat("✅ Using model:", selected_model)
        if (selected_model != requested_model) {
          cat(" (fallback from", requested_model, ")")
        }
        cat("\n")
        
        return(list(
          available = TRUE,
          selected_model = selected_model,
          is_fallback = selected_model != requested_model
        ))
      }
    }
  }
  
  # If no models work, provide helpful error message
  return(list(
    available = FALSE,
    error = paste(
      "No working models found. Requested:", requested_model,
      "| Available:", paste(available_models, collapse = ", "),
      "| To install a model, run: ollama pull llama3.2:3b"
    )
  ))
}

#' Test Model Functionality
#' 
#' Tests if a specific model can generate responses
#' 
#' @param config Configuration object
#' @param model Model name to test
#' @return List with test results
#' 
test_model_functionality <- function(config, model) {
  
  host <- config$ai_features$ollama$host %||% "http://localhost:11434"
  
  tryCatch({
    # Simple test prompt
    test_prompt <- "Say 'test successful' in one word."
    
    body <- list(
      model = model,
      prompt = test_prompt,
      stream = FALSE,
      options = list(
        temperature = 0.1,
        max_tokens = 10
      )
    )
    
    response <- POST(
      paste0(host, "/api/generate"),
      body = toJSON(body, auto_unbox = TRUE),
      add_headers("Content-Type" = "application/json"),
      timeout(30)
    )
    
    if (status_code(response) != 200) {
      return(list(working = FALSE, error = paste("API error:", status_code(response))))
    }
    
    result <- fromJSON(content(response, "text"))
    
    if (is.null(result$response) || nchar(result$response) < 1) {
      return(list(working = FALSE, error = "Empty response from model"))
    }
    
    return(list(working = TRUE, test_response = result$response))
    
  }, error = function(e) {
    return(list(working = FALSE, error = paste("Test failed:", e$message)))
  })
}

#' Build AI Configuration with User Customization
#' 
#' Builds complete AI configuration object with user customizations applied
#' 
#' @param config Base configuration (potentially with user_customization)
#' @param model Selected model name
#' @return AI configuration object with customizations applied
#' 
build_ai_config <- function(config, model, register_context = NULL) {
  provider <- tolower(config$ai_features$provider %||% "ollama")
  host <- config$ai_features$ollama$host %||% "http://localhost:11434"
  
  # Load user customization if available
  user_custom <- config$user_customization
  
  # Build context-aware epidemiological context with register data
  epi_context <- build_customized_epi_context(user_custom, register_context)
  
  # Detect deployment type for context-specific behavior
  deployment_type <- if (!is.null(user_custom)) {
    detect_deployment_type(user_custom)
  } else {
    "general"
  }
  
  # Add data-driven context adjustments
  if (!is.null(register_context) && register_context$available) {
    epi_context$data_context <- list(
      period_days = register_context$date_range$span_days %||% 365,
      total_consultations = register_context$total_records,
      active_diseases = length(register_context$top_diseases),
      geographic_coverage = length(register_context$geographic_areas),
      data_columns = register_context$columns_available
    )
    
    # Adjust endemic diseases list based on actual data
    if (length(register_context$top_diseases) > 0) {
      epi_context$endemic_diseases <- unique(c(
        epi_context$endemic_diseases,
        register_context$top_diseases[1:min(5, length(register_context$top_diseases))]
      ))
    }
  }
  
  if (provider == "openai") {
    base_url <- config$ai_features$openai$base_url %||% "https://api.openai.com/v1/chat/completions"
    api_config <- list(
      provider = "openai",
      api_endpoint = base_url,
      model = config$ai_features$openai$model %||% model,
      timeout = config$ai_features$openai$timeout %||% 120,
      temperature = get_custom_value(user_custom, "ai_params.temperature", config$ai_features$openai$temperature %||% 0.2),
      max_tokens = get_custom_value(user_custom, "ai_params.max_tokens", config$ai_features$openai$max_tokens %||% 800),
      api_key = Sys.getenv("OPENAI_API_KEY", config$ai_features$openai$api_key %||% ""),
      system_prompt = config$epibot_features$system_prompt %||% "You are EpiBot, an AI assistant specializing in epidemiological data analysis."
    )
    if (!nzchar(api_config$api_key)) warning("OPENAI_API_KEY not set; OpenAI requests will fail.")
  } else {
    api_config <- list(
      provider = "ollama",
      host = host,
      model = model,
      api_endpoint = paste0(host, "/api/generate"),
      timeout = config$ai_features$ollama$timeout %||% 60,
      temperature = get_custom_value(user_custom, "ai_params.temperature", 0.1),
      max_tokens = get_custom_value(user_custom, "ai_params.max_tokens", 1000),
      api_key = NULL,
      system_prompt = config$epibot_features$system_prompt %||% "You are EpiBot, an AI assistant specializing in epidemiological data analysis."
    )
  }

  ai_config <- list(
    # Connection settings
    host = host,
    model = model,
    api_endpoint = api_config$api_endpoint,
    timeout = api_config$timeout,
    temperature = api_config$temperature,
    max_tokens = api_config$max_tokens,
    provider = api_config$provider,
    api_key = api_config$api_key,
    
    # Enhanced context with customization
    epi_context = epi_context,
    context_setting = get_custom_value(user_custom, "context.context_name", "syria_humanitarian"),
    deployment_type = deployment_type,
    
    # Customizable quality settings
    quality_checks = TRUE,
    expert_review_flags = TRUE,
    minimum_quality_score = get_custom_value(user_custom, "quality.minimum_score", 60),
    expert_review_threshold = get_custom_value(user_custom, "quality.expert_review_threshold", 70),
    
    # Customizable security settings
    security = list(
      data_anonymization = get_custom_value(user_custom, "security.anonymize_data", TRUE),
      audit_enabled = get_custom_value(user_custom, "security.audit_all_interactions", TRUE),
      audit_file = "logs/ai_audit.log",
      max_query_length = get_custom_value(user_custom, "security.max_query_length", 500)
    ),
    
    # User customization reference
    user_customization = user_custom
  )
  
  return(ai_config)
}

#' Generate All AI Summaries
#' 
#' Orchestrates generation of all AI-powered analysis summaries
#' 
#' @param register Health register data
#' @param ai_system AI system configuration
#' @return List of generated summaries
#' 
generate_all_ai_summaries <- function(register, ai_system) {
  
  if (!ai_system$available) {
    return(list(error = "AI system not available"))
  }
  
  summaries <- list()
  
  tryCatch({
    # Load enhanced AI module
    if (!exists("initialize_enhanced_ai_system")) {
      if (file.exists(here::here("R", "enhanced_ai.R"))) {
        source(here::here("R", "enhanced_ai.R"))
      } else {
        stop("Enhanced AI module not found")
      }
    }
    
    # Generate comprehensive analysis
    cat("🔍 Starting comprehensive AI analysis...\n")
    
    # Multi-stage analysis
    analysis_result <- conduct_multistage_analysis(register, ai_system$config, "comprehensive")
    
    if ("error" %in% names(analysis_result)) {
      summaries$error <- analysis_result$error
    } else {
      # Extract different types of summaries
      if ("patterns" %in% names(analysis_result)) {
        summaries$monthly_summary <- analysis_result$patterns
      }
      
      if ("risk_assessment" %in% names(analysis_result)) {
        summaries$risk_assessment <- analysis_result$risk_assessment
      }
      
      if ("geographic_insights" %in% names(analysis_result)) {
        summaries$geographic_insights <- analysis_result$geographic_insights
      }
      
      if ("demographic_insights" %in% names(analysis_result)) {
        summaries$demographic_insights <- analysis_result$demographic_insights
      }
      
      # Add quality metrics
      if ("overall_quality" %in% names(analysis_result)) {
        summaries$analysis_quality <- analysis_result$overall_quality
      }
    }
    
    # Generate disease-specific narratives for top diseases
    top_diseases <- register %>%
      count(canonical_disease_imc, sort = TRUE) %>%
      slice_head(n = 3) %>%
      pull(canonical_disease_imc)
    
    disease_narratives <- list()
    for (disease in top_diseases) {
      cat("📈 Analyzing", disease, "...\n")
      narrative <- generate_disease_trend_narrative(register, disease, ai_system$config)
      disease_narratives[[disease]] <- narrative
    }
    summaries$disease_narratives <- disease_narratives
    
    cat("✅ AI analysis completed successfully\n")
    
  }, error = function(e) {
    cat("❌ AI analysis failed:", e$message, "\n")
    summaries$error <- paste("Analysis failed:", e$message)
  })
  
  return(summaries)
}

#' Generate Enhanced Epidemic Alerts
#' 
#' Creates epidemic alerts using enhanced AI analysis
#' 
#' @param register Health register data
#' @param ai_config AI configuration
#' @return Formatted alert text
#' 
generate_enhanced_epidemic_alerts <- function(register, ai_config) {
  
  tryCatch({
    # Detect statistical alerts
    alerts <- detect_enhanced_statistical_alerts(register, ai_config)
    
    if (nrow(alerts) == 0) {
      return("✅ **No epidemic alerts detected** - Current disease patterns are within expected ranges for the monitoring period.")
    }
    
    # Generate alert summary
    high_priority <- sum(alerts$risk_level == "High")
    medium_priority <- sum(alerts$risk_level == "Medium")
    
    alert_text <- paste0(
      "🚨 **", nrow(alerts), " surveillance alert(s) detected:**\n\n",
      if (high_priority > 0) paste0("- **", high_priority, " HIGH PRIORITY** alert(s) requiring immediate investigation\n"),
      if (medium_priority > 0) paste0("- **", medium_priority, " MEDIUM PRIORITY** alert(s) for enhanced monitoring\n"),
      "\n**Alert Details:**\n\n"
    )
    
    # Add details for each alert
    for (i in 1:min(nrow(alerts), 5)) {  # Limit to top 5 alerts
      alert <- alerts[i, ]
      alert_text <- paste0(
        alert_text,
        "**", alert$canonical_disease_imc, "** - ", alert$risk_level, " Risk\n",
        "- Current trend: ", round(alert$alert_ratio, 2), "x baseline (", round(alert$recent_mean, 1), " vs ", round(alert$baseline_mean, 1), " cases/week)\n",
        "- Status: ", alert$endemic_status, "\n\n"
      )
    }
    
    if (nrow(alerts) > 5) {
      alert_text <- paste0(alert_text, "*... and ", nrow(alerts) - 5, " additional alert(s)*\n\n")
    }
    
    alert_text <- paste0(
      alert_text,
      "**Recommended Actions:**\n",
      "- Verify data quality and case definitions\n",
      "- Conduct enhanced surveillance for high-priority alerts\n", 
      "- Consider epidemiological investigation if trends persist\n",
      "- Review prevention and control measures\n\n"
    )
    
    return(alert_text)
    
  }, error = function(e) {
    return(paste("❌ Alert detection failed:", e$message, "- Manual review recommended."))
  })
}

#' Format AI Summary for Display
#' 
#' Formats AI-generated summaries for markdown display
#' 
#' @param summary AI-generated summary text
#' @param type Type of summary
#' @param add_disclaimer Whether to add AI disclaimer
#' @return Formatted markdown text
#' 
format_ai_summary <- function(summary, type = "general", add_disclaimer = TRUE) {
  
  if (is.null(summary) || summary == "" || grepl("Error|Failed", summary, ignore.case = TRUE)) {
    return("*AI analysis not available - using standard statistical methods.*")
  }
  
  formatted <- paste0(
    "### 🤖 AI-Enhanced Analysis\n\n",
    summary, "\n\n"
  )
  
  if (add_disclaimer) {
    formatted <- paste0(
      formatted,
      "*AI-generated insights based on statistical pattern analysis. ",
      "Professional epidemiological review recommended for all findings.*\n\n"
    )
  }
  
  return(formatted)
}

# ============================================================================
# HELPER FUNCTIONS FOR CUSTOMIZATION
# Utility functions to support user customization system
# ============================================================================

#' Get Custom Value with Nested Key Support
#' 
#' Safely extracts values from nested user customization with fallback
#' 
#' @param user_custom User customization object
#' @param key_path Dot-separated path (e.g., "ai_params.temperature")
#' @param default Default value if key not found
#' @return Value from customization or default
#' 
get_custom_value <- function(user_custom, key_path, default = NULL) {
  if (is.null(user_custom)) return(default)
  
  keys <- strsplit(key_path, "\\.")[[1]]
  current <- user_custom
  
  for (key in keys) {
    if (is.list(current) && key %in% names(current)) {
      current <- current[[key]]
    } else {
      return(default)
    }
  }
  
  return(if (is.null(current)) default else current)
}

#' Build Customized Epidemiological Context
#' 
#' Creates epidemiological context using user customizations
#' 
#' @param user_custom User customization object
#' @return Epidemiological context list
#' 
build_customized_epi_context <- function(user_custom, register_context = NULL) {
  
  # Default context (fallback)
  default_context <- list(
    setting = "Humanitarian health surveillance in conflict-affected Syria",
    population = "Internally displaced persons, refugees, conflict-affected populations",
    constraints = c("Limited healthcare access", "Security challenges", "Resource constraints"),
    priorities = c("Outbreak detection", "vulnerable populations", "cost-effective interventions"),
    endemic_diseases = c("Acute watery diarrhea", "Respiratory infections", "Skin diseases", "Malnutrition"),
    alert_thresholds = list(
      outbreak_multiplier = 2.0,
      high_burden_cases = 100,
      geographic_spread = 2
    )
  )
  
  if (is.null(user_custom)) return(default_context)
  
  # Build customized context
  epi_context <- list(
    setting = get_custom_value(user_custom, "context.setting_description", default_context$setting),
    population = get_custom_value(user_custom, "context.population_description", default_context$population),
    constraints = get_custom_value(user_custom, "context.operational_constraints", default_context$constraints),
    priorities = get_custom_value(user_custom, "context.surveillance_priorities", default_context$priorities),
    endemic_diseases = get_custom_value(user_custom, "diseases.endemic_diseases", default_context$endemic_diseases),
    epidemic_diseases = get_custom_value(user_custom, "diseases.epidemic_diseases", c("Cholera", "Measles", "Meningitis")),
    priority_populations = get_custom_value(user_custom, "populations.high_priority", c("children under 5", "pregnant women", "elderly")),
    alert_thresholds = list(
      outbreak_multiplier = get_custom_value(user_custom, "thresholds.outbreak_multiplier", 2.0),
      high_burden_cases = get_custom_value(user_custom, "thresholds.high_burden_threshold", 100),
      geographic_spread = get_custom_value(user_custom, "thresholds.geographic_spread_threshold", 2),
      minimum_cases = get_custom_value(user_custom, "thresholds.minimum_cases_for_alert", 5)
    ),
    temporal_settings = list(
      surveillance_cycle = get_custom_value(user_custom, "temporal.surveillance_cycle", "weekly"),
      baseline_period_weeks = get_custom_value(user_custom, "temporal.baseline_period_weeks", 8),
      recent_period_days = get_custom_value(user_custom, "temporal.recent_period_days", 30)
    )
  )
  
  return(epi_context)
}

# ============================================================================
# USER CUSTOMIZATION SYSTEM
# Functions for loading and applying user-defined AI customizations
# ============================================================================

#' Load User AI Customization Settings
#' 
#' Loads user-provided customization from YAML files
#' Implements the concepts from ai_customization_guide.md with current structure
#' 
#' @param config Base configuration object
#' @return Merged configuration with user customizations
#' 
load_user_ai_customization <- function(config) {
  
  cat("📋 Loading user AI customization settings...\n")
  
  # Priority order for customization files
  custom_config_paths <- c(
    here::here("config", "ai_customization.yml"),
    here::here("ai_customization", "user_settings.yml"),
    here::here("ai_customization", "deployment_config.yml")
  )
  
  user_config <- NULL
  config_file_used <- NULL
  
  # Try to load user customization file
  for (path in custom_config_paths) {
    if (file.exists(path)) {
      tryCatch({
        user_config <- yaml::read_yaml(path)
        config_file_used <- path
        cat("✅ Loaded user AI customization from:", basename(path), "\n")
        break
      }, error = function(e) {
        cat("⚠️ Failed to load customization from", basename(path), ":", e$message, "\n")
      })
    }
  }
  
  if (is.null(user_config)) {
    cat("ℹ️ No user customization file found, using default AI configuration\n")
    return(config)
  }
  
  # Validate user configuration
  validation_result <- validate_user_customization(user_config)
  if (!validation_result$valid) {
    cat("❌ User customization validation failed:", validation_result$errors, "\n")
    cat("ℹ️ Falling back to default configuration\n")
    return(config)
  }
  
  # Merge configurations
  merged_config <- merge_ai_configurations(config, user_config)
  
  cat("🔧 Applied user customizations from:", basename(config_file_used), "\n")
  cat("   - Context:", user_config$context$context_name %||% "default", "\n")
  cat("   - Deployment type:", detect_deployment_type(user_config), "\n")
  
  return(merged_config)
}

#' Validate User Customization
#' 
#' Validates user-provided customization against defined rules
#' 
#' @param user_config User customization object
#' @return List with validation status and any errors
#' 
validate_user_customization <- function(user_config) {
  
  errors <- c()
  
  # Check required fields
  required_fields <- c("context", "thresholds", "diseases")
  for (field in required_fields) {
    if (!(field %in% names(user_config))) {
      errors <- c(errors, paste("Missing required field:", field))
    }
  }
  
  # Validate numeric ranges
  if (!is.null(user_config$thresholds$outbreak_multiplier)) {
    if (user_config$thresholds$outbreak_multiplier < 1.0 || user_config$thresholds$outbreak_multiplier > 5.0) {
      errors <- c(errors, "outbreak_multiplier must be between 1.0 and 5.0")
    }
  }
  
  if (!is.null(user_config$ai_params$temperature)) {
    if (user_config$ai_params$temperature < 0.0 || user_config$ai_params$temperature > 1.0) {
      errors <- c(errors, "temperature must be between 0.0 and 1.0")
    }
  }
  
  # Validate allowed values
  if (!is.null(user_config$temporal$surveillance_cycle)) {
    allowed_cycles <- c("daily", "weekly", "monthly")
    if (!(user_config$temporal$surveillance_cycle %in% allowed_cycles)) {
      errors <- c(errors, paste("surveillance_cycle must be one of:", paste(allowed_cycles, collapse = ", ")))
    }
  }
  
  return(list(
    valid = length(errors) == 0,
    errors = if (length(errors) > 0) paste(errors, collapse = "; ") else NULL
  ))
}

#' Merge AI Configurations
#' 
#' Merges base configuration with user customizations
#' 
#' @param base_config Base AI configuration
#' @param user_config User customization settings
#' @return Merged configuration object
#' 
merge_ai_configurations <- function(base_config, user_config) {
  
  # Start with base configuration
  merged_config <- base_config
  
  # Add user customization as a new section
  merged_config$user_customization <- user_config
  
  # Override specific AI parameters if provided
  if (!is.null(user_config$ai_params)) {
    if (!is.null(user_config$ai_params$temperature)) {
      merged_config$ai_features$ollama$temperature <- user_config$ai_params$temperature
    }
    if (!is.null(user_config$ai_params$max_tokens)) {
      merged_config$ai_features$ollama$max_tokens <- user_config$ai_params$max_tokens
    }
  }
  
  # Override timeout if specified
  if (!is.null(user_config$ai_params$timeout)) {
    merged_config$ai_features$ollama$timeout <- user_config$ai_params$timeout
  }
  
  return(merged_config)
}

#' Detect Deployment Type
#' 
#' Automatically detects deployment context from user customization
#' 
#' @param user_customization User customization object
#' @return String indicating deployment type
#' 
detect_deployment_type <- function(user_customization) {
  
  if (is.null(user_customization$context)) return("general")
  
  context_name <- user_customization$context$context_name %||% ""
  setting_desc <- user_customization$context$setting_description %||% ""
  
  # Combine text for pattern matching
  combined_text <- paste(context_name, setting_desc, collapse = " ")
  combined_text <- tolower(combined_text)
  
  # Pattern matching for deployment types
  if (grepl("refugee|camp|displacement|idp", combined_text)) {
    return("refugee_camp")
  } else if (grepl("urban|city|network|multiple|clinic", combined_text)) {
    return("urban_clinic")
  } else if (grepl("rural|remote|village|countryside", combined_text)) {
    return("rural_health")
  } else if (grepl("emergency|crisis|outbreak|rapid|acute", combined_text)) {
    return("emergency_response")
  } else {
    return("general")
  }
}

#' Apply Deployment Preset
#' 
#' Applies predefined settings for specific deployment types
#' 
#' @param user_config User configuration object
#' @param deployment_type Detected or specified deployment type
#' @return Updated configuration with preset values
#' 
apply_deployment_preset <- function(user_config, deployment_type = NULL) {
  
  if (is.null(deployment_type)) {
    deployment_type <- detect_deployment_type(user_config)
  }
  
  # Get preset if available
  if (!is.null(user_config$deployment_presets) && 
      deployment_type %in% names(user_config$deployment_presets)) {
    
    preset <- user_config$deployment_presets[[deployment_type]]
    
    cat("🎯 Applying", deployment_type, "preset configuration\n")
    
    # Apply preset values (only if not already customized)
    if (is.null(user_config$thresholds$outbreak_multiplier)) {
      user_config$thresholds$outbreak_multiplier <- preset$outbreak_multiplier
    }
    if (is.null(user_config$thresholds$high_burden_threshold)) {
      user_config$thresholds$high_burden_threshold <- preset$high_burden_threshold
    }
    if (is.null(user_config$temporal$surveillance_cycle)) {
      user_config$temporal$surveillance_cycle <- preset$surveillance_cycle
    }
  }
  
  return(user_config)
}

#' Get Register Context Summary
#' 
#' Extracts relevant context from health register data for AI configuration
#' 
#' @param register Health register data frame
#' @return List containing data context summary
#' 
get_register_context_summary <- function(register) {
  if (is.null(register) || !is.data.frame(register) || nrow(register) == 0) {
    return(list(
      available = FALSE,
      total_records = 0,
      date_range = NULL,
      top_diseases = character(),
      geographic_areas = character(),
      demographic_summary = NULL
    ))
  }
  
  tryCatch({
    # Get date column
    date_col <- dplyr::case_when(
      "datevisit" %in% names(register) ~ "datevisit",
      "datevisitnew" %in% names(register) ~ "datevisitnew",
      "visit_date" %in% names(register) ~ "visit_date",
      TRUE ~ NA_character_
    )
    
    # Get disease column
    disease_col <- dplyr::case_when(
      "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
      "morbidity" %in% names(register) ~ "morbidity",
      TRUE ~ NA_character_
    )
    
    # Get geographic column
    geo_col <- dplyr::case_when(
      "admin1" %in% names(register) ~ "admin1",
      "governorate" %in% names(register) ~ "governorate",
      TRUE ~ NA_character_
    )
    
    # Date range
    date_range <- NULL
    if (!is.na(date_col)) {
      dates <- as.Date(register[[date_col]])
      if (any(!is.na(dates))) {
        date_range <- list(
          start = min(dates, na.rm = TRUE),
          end = max(dates, na.rm = TRUE),
          span_days = as.numeric(max(dates, na.rm = TRUE) - min(dates, na.rm = TRUE))
        )
      }
    }
    
    # Top diseases
    top_diseases <- character()
    if (!is.na(disease_col)) {
      top_diseases <- register %>%
        count(.data[[disease_col]], sort = TRUE) %>%
        slice_head(n = 10) %>%
        pull(1)
    }
    
    # Geographic areas
    geographic_areas <- character()
    if (!is.na(geo_col)) {
      geographic_areas <- register %>%
        count(.data[[geo_col]], sort = TRUE) %>%
        slice_head(n = 5) %>%
        pull(1)
    }
    
    # Demographic summary
    demographic_summary <- list()
    if ("sex" %in% names(register)) {
      sex_dist <- register %>%
        filter(!is.na(sex)) %>%
        count(sex, sort = TRUE)
      demographic_summary$sex_distribution <- sex_dist
    }
    
    if ("age_group" %in% names(register)) {
      age_dist <- register %>%
        filter(!is.na(age_group)) %>%
        count(age_group, sort = TRUE)
      demographic_summary$age_distribution <- age_dist
    }
    
    return(list(
      available = TRUE,
      total_records = nrow(register),
      date_range = date_range,
      top_diseases = top_diseases,
      geographic_areas = geographic_areas,
      demographic_summary = demographic_summary,
      columns_available = names(register)
    ))
    
  }, error = function(e) {
    cat("⚠️ Failed to extract register context:", e$message, "\n")
    return(list(
      available = FALSE,
      total_records = nrow(register),
      error = e$message
    ))
  })
}

cat("✅ AI Configuration module with user customization loaded successfully\n")
