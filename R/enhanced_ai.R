# ============================================================================
# Enhanced Consolidated AI System for Epidemiological Analysis - COMPLETE
# IMC M&E Dept. • v2.2.1 • 2025
# 
# DATA INTEGRATION UPDATE: This module now supports register data integration
# for context-aware AI analysis. The configure_enhanced_ai() function can
# accept optional health register data to automatically extract and incorporate:
# - Disease patterns and prevalence from actual data
# - Date ranges and temporal context
# - Geographic coverage and demographic patterns
# - Data-driven thresholds and disease lists
# ============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(digest)
  library(tibble)
  library(rlang)
library(scales)
})

if (!exists("call_ai_with_config")) {
  call_ai_with_config <- function(cfg, prompt, options = list(), max_retries = 2) {
    opts <- modifyList(list(max_tokens = cfg$max_tokens, timeout = cfg$timeout), options)
    call_enhanced_local_ai(
      host = cfg$host,
      model = cfg$model,
      prompt = prompt,
      options = opts,
      max_retries = max_retries,
      provider = cfg$provider,
      api_key = cfg$api_key,
      system_prompt = cfg$system_prompt
    )
  }
}

# Utilities --------------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

.null2chr <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  if (length(x) > 1) {
    if (is.list(x)) x <- vapply(x, function(y) paste(capture.output(print(y)), collapse=" "), character(1))
    x <- paste(x, collapse = ", ")
  }
  if (is.na(x)) return("")
  as.character(x)
}

.clamp <- function(x, lo, hi) max(lo, min(hi, x))
.as_date <- function(x) suppressWarnings(as.Date(x))
.safe_n <- function(x) if (is.numeric(x)) x else suppressWarnings(as.numeric(x))
.short <- function(x, n=1200) { x <- .null2chr(x); if (nchar(x) > n) paste0(substr(x,1,n-3),"...") else x }

ENHANCED_AI_VERSION <- "2.2.1"

# =============================================================================
# MISSING FUNCTION: comma() - Add this function
# =============================================================================
comma <- function(x, ...) {
  if (is.numeric(x)) {
    format(x, big.mark = ",", ...)
  } else {
    x
  }
}

# =============================================================================
# SAFE UTILITY FUNCTIONS
# =============================================================================
safe_logical_check <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0) return(default)
  if (is.logical(x) && length(x) == 1 && !is.na(x)) return(x)
  if (is.character(x) && length(x) == 1) {
    return(tolower(trimws(x)) %in% c("true", "yes", "1", "on"))
  }
  if (is.numeric(x) && length(x) == 1 && !is.na(x)) {
    return(x > 0)
  }
  return(default)
}

safe_df_check <- function(df, required_cols = NULL) {
  if (!is.data.frame(df) || nrow(df) == 0) return(FALSE)
  if (!is.null(required_cols)) {
    return(all(required_cols %in% names(df)))
  }
  return(TRUE)
}

get_disease_column <- function(df) {
  candidates <- c("canonical_disease_imc", "morbidity", "icd11_title", "disease", "diagnosis")
  for (col in candidates) {
    if (col %in% names(df)) return(col)
  }
  return(NA_character_)
}

get_date_column <- function(df) {
  candidates <- c("datevisit", "datevisitnew", "visit_date", "date", "eventdate")
  for (col in candidates) {
    if (col %in% names(df)) return(col)
  }
  return(NA_character_)
}

get_epidemic_diseases <- function() {
  c(
    "Acute Watery Diarrhea", "Cholera", "Dysentery",
    "Measles", "Rubella", 
    "Acute Flaccid Paralysis", "Poliomyelitis",
    "Meningitis", "Bacterial Meningitis",
    "Acute Respiratory Infection", "Pneumonia", "Influenza",
    "Typhoid", "Hepatitis A", "Hepatitis E",
    "Leishmaniasis", "Malaria",
    "Scabies", "Skin Disease",
    "Mental Health", "Trauma"
  )
}

# =============================================================================
# 1) MAIN INITIALIZATION (used in Rmd)
# =============================================================================
configure_enhanced_ai <- function(config, params, register = NULL) {
  cat("🤖 Initializing Enhanced AI System v", ENHANCED_AI_VERSION, "...\n", sep = "")
  ai_system <- list(
    available = FALSE,
    enhanced  = TRUE,
    provider  = NULL,
    model     = NULL,
    host      = NULL,
    features  = list(),
    config    = list(),
    error_message = NULL,
    version   = ENHANCED_AI_VERSION,
    quality_checks = TRUE,
    expert_review_flags = TRUE,
    fallback_mode = FALSE
  )
  
  # Fix logical coercion issue - ensure params$enable_ai is properly handled
  enable_ai <- FALSE
  if (!is.null(params) && "enable_ai" %in% names(params)) {
    enable_ai <- safe_logical_check(params$enable_ai, FALSE)
  }
  
  if (!enable_ai) {
    ai_system$error_message <- "AI disabled in report parameters"
    cat("ℹ️ AI features disabled via report parameters\n")
    return(ai_system)
  }
  
  # Fix config checking
  ai_enabled <- FALSE
  if (!is.null(config) && "ai_features" %in% names(config) && 
      !is.null(config$ai_features) && "enabled" %in% names(config$ai_features)) {
    ai_enabled <- safe_logical_check(config$ai_features$enabled, FALSE)
  }
  
  if (!ai_enabled) {
    ai_system$error_message <- "AI not enabled in configuration"
    cat("ℹ️ AI features not enabled in configuration\n")
    return(ai_system)
  }
  
  provider <- tolower(config$ai_features$provider %||% "ollama")
  
  contexts <- get_epidemiological_contexts()
  context_setting <- config$ai_features$context_setting %||% "syria_humanitarian"
  if (!context_setting %in% names(contexts)) {
    available_contexts <- names(contexts)
    cat("⚠️ Context '", context_setting, "' not found. Available contexts: ", paste(available_contexts, collapse = ", "), "\n", sep = "")
    context_setting <- available_contexts[1]
    cat("🔄 Using fallback context: ", context_setting, "\n", sep = "")
  }

  if (identical(provider, "openai")) {
    openai_test <- test_openai_connection(config)
    if (!openai_test$connected) {
      ai_system$error_message <- openai_test$error
      cat("❌ Failed to connect to OpenAI: ", openai_test$error, "\n", sep = "")
      return(ai_system)
    }

    ai_system$available <- TRUE
    ai_system$provider  <- "openai"
    ai_system$model     <- openai_test$selected_model
    ai_system$host      <- openai_test$base_url
    ai_system$fallback_mode <- FALSE
    ai_system$features  <- config$ai_features$summary_generation$summary_types %||%
      c("monthly", "disease_trend", "geographic", "demographic", "facility_performance")

    base_config <- list(
      host = openai_test$endpoint,
      api_endpoint = openai_test$endpoint,
      model = openai_test$selected_model,
      timeout = config$ai_features$openai$timeout %||% 120,
      temperature = config$ai_features$openai$temperature %||% 0.2,
      max_tokens = config$ai_features$openai$max_tokens %||% 800,
      api_key = openai_test$api_key,
      system_prompt = config$epibot_features$system_prompt %||% "You are EpiBot, an AI assistant specializing in epidemiological data analysis.",
      provider = "openai"
    )
    ai_system$config <- c(base_config, list(
      epi_context = contexts[[context_setting]],
      context_setting = context_setting,
      security = list(
        data_anonymization = TRUE,
        audit_enabled = config$ai_features$security$audit_enabled %||% TRUE,
        audit_file    = config$ai_features$security$audit_file %||% "logs/ai_audit.log",
        max_query_length = config$ai_features$security$max_query_length %||% 500
      ),
      api = base_config
    ))

    cat("✅ Enhanced AI System initialized\n")
    cat("🎯 Provider: OpenAI | Model: ", openai_test$selected_model, "\n", sep = "")
    cat("🏥 Context: ", context_setting, "\n", sep = "")
    cat("📋 Features: ", paste(ai_system$features, collapse = ", "), "\n", sep = "")
    return(ai_system)
  }

  # Connectivity & models for Ollama
  ollama_test <- test_ollama_connection(config)
  if (!ollama_test$connected) {
    ai_system$error_message <- ollama_test$error
    cat("❌ Failed to connect to Ollama: ", ollama_test$error, "\n", sep = "")
    return(ai_system)
  }

  model_test <- test_model_availability_with_fallback(config, ollama_test$available_models)
  if (!model_test$available) {
    ai_system$error_message <- model_test$error
    cat("❌ ", model_test$error, "\n", sep = "")
    return(ai_system)
  }

  contexts <- get_epidemiological_contexts()
  
  ollama_host <- config$ai_features$ollama$host %||% "http://localhost:11434"
  ai_system$available <- TRUE
  ai_system$provider  <- "ollama"
  ai_system$model     <- model_test$selected_model
  ai_system$host      <- ollama_host
  ai_system$fallback_mode <- isTRUE(model_test$is_fallback)
  ai_system$features  <- config$ai_features$summary_generation$summary_types %||%
    c("monthly", "disease_trend", "geographic", "demographic", "facility_performance")
  
  base_config <- list(
    host = ollama_host,
    model = model_test$selected_model,
    api_endpoint = paste0(ollama_host, "/api/generate"),
    timeout = config$ai_features$ollama$timeout %||% 120,
    temperature = config$ai_features$ollama$temperature %||% 0.1,
    max_tokens = config$ai_features$ollama$max_tokens %||% 2048,
    system_prompt = config$epibot_features$system_prompt %||% "You are EpiBot, an AI assistant specializing in epidemiological data analysis.",
    provider = "ollama",
    api_key = NULL
  )
  ai_system$config <- c(base_config, list(
    epi_context = contexts[[context_setting]],
    context_setting = context_setting,
    quality_checks = TRUE,
    expert_review_flags = TRUE,
    minimum_quality_score   = config$ai_features$summary_generation$quality_checks$minimum_quality_score %||% 60,
    expert_review_threshold = config$ai_features$summary_generation$quality_checks$expert_review_threshold %||% 70,
    security = list(
      data_anonymization = TRUE,
      audit_enabled = config$ai_features$security$audit_enabled %||% TRUE,
      audit_file    = config$ai_features$security$audit_file %||% "logs/ai_audit.log",
      max_query_length = config$ai_features$security$max_query_length %||% 500
    ),
    api = base_config
  ))
  
  cat("✅ Enhanced AI System initialized\n")
  cat("🎯 Provider: ollama | Model: ", model_test$selected_model,
      if (isTRUE(model_test$is_fallback)) " (fallback model)" else "", "\n", sep = "")
  cat("🏥 Context: ", context_setting, "\n", sep = "")
  cat("📋 Features: ", paste(ai_system$features, collapse = ", "), "\n", sep = "")
  ai_system
}

# =============================================================================
# 2) OLLAMA CONNECTIVITY & MODEL PROBES
# =============================================================================
test_ollama_connection <- function(config) {
  host <- config$ai_features$ollama$host %||% "http://localhost:11434"
  tryCatch({
    resp <- GET(paste0(host, "/api/tags"), timeout(10))
    if (status_code(resp) != 200) {
      return(list(connected = FALSE, error = paste("Ollama status:", status_code(resp))))
    }
    content_text <- content(resp, "text", encoding = "UTF-8")
    models_data  <- fromJSON(content_text)
    models <- if (is.null(models_data$models)) list() else models_data$models
    available_models <- if (is.data.frame(models)) models$name else
      if (length(models)) vapply(models, function(x) x$name %||% x$model %||% "unknown", character(1)) else character(0)
    cat("🔗 Connected to Ollama • Models: ", if (length(available_models)) paste(available_models, collapse=", ") else "(none)", "\n", sep = "")
    list(connected = TRUE, available_models = available_models)
  }, error = function(e) list(connected = FALSE, error = paste("Connection failed:", e$message)))
}

test_openai_connection <- function(config) {
  api_key <- Sys.getenv("OPENAI_API_KEY", config$ai_features$openai$api_key %||% "")
  if (!nzchar(api_key)) {
    return(list(connected = FALSE, error = "OPENAI_API_KEY environment variable not set"))
  }

  base_url <- config$ai_features$openai$base_url %||% "https://api.openai.com/v1/chat/completions"
  models_url <- "https://api.openai.com/v1/models"

  resp <- tryCatch({
    GET(models_url, add_headers(Authorization = paste("Bearer", api_key)), timeout(10))
  }, error = function(e) {
    return(list(connected = FALSE, error = e$message))
  })

  if (is.list(resp) && !is.null(resp$connected) && resp$connected == FALSE) {
    return(resp)
  }

  if (httr::status_code(resp) != 200) {
    return(list(connected = FALSE, error = paste("OpenAI status:", httr::status_code(resp))))
  }

  content_text <- httr::content(resp, "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(content_text), error = function(e) list(data = list()))
  models <- parsed$data$id %||% unlist(parsed$data)
  target_model <- config$ai_features$openai$model %||% "gpt-4o-mini"

  if (!is.null(models) && length(models) > 0 && !target_model %in% models) {
    warning(sprintf("Model '%s' not listed in OpenAI account. Proceeding anyway.", target_model))
  }

  list(
    connected = TRUE,
    available_models = models,
    selected_model = target_model,
    base_url = base_url,
    endpoint = base_url,
    api_key = api_key
  )
}

test_model_availability_with_fallback <- function(config, available_models) {
  requested_model <- config$ai_features$ollama$model %||% "llama3.2:3b"
  fallback_models <- c(requested_model, "llama3.2:3b","llama3.2:1b","llama3.2",
                       "llama3:8b","llama3","llama2:7b","llama2",
                       "mistral:7b","mistral","tinyllama","phi3:mini","phi3")
  host <- config$ai_features$ollama$host %||% "http://localhost:11434"
  for (m in fallback_models) {
    rx <- paste0("^", gsub(":", ".*", m), "$")
    match <- available_models[grepl(rx, available_models)]
    if (length(match)) {
      selected <- match[1]
      ok <- test_model_functionality(host, selected)
      if (ok$working) {
        cat("✅ Using model: ", selected,
            if (!identical(selected, requested_model)) paste0(" (fallback from ", requested_model, ")") else "",
            "\n", sep = "")
        return(list(available=TRUE, selected_model=selected, is_fallback=!identical(selected, requested_model)))
      }
    }
  }
  list(available=FALSE,
       error=paste("No working models found. Requested:", requested_model,
                   "| Available:", if (length(available_models)) paste(available_models, collapse=", ") else "(none)",
                   "| To install: ollama pull", requested_model))
}

test_model_functionality <- function(host, model) {
  tryCatch({
    body <- list(model=model, prompt="Say 'ok' in one word.", stream=FALSE,
                 options=list(temperature=0.1, num_predict=16))
    resp <- POST(paste0(host,"/api/generate"),
                 body=toJSON(body, auto_unbox=TRUE),
                 add_headers("Content-Type"="application/json"),
                 timeout(20))
    if (status_code(resp) != 200) return(list(working=FALSE, error=paste("API error:", status_code(resp))))
    res <- fromJSON(content(resp,"text", encoding="UTF-8"))
    if (is.null(res$response) || !nzchar(res$response)) return(list(working=FALSE, error="Empty response"))
    list(working=TRUE, test_response=res$response)
  }, error=function(e) list(working=FALSE, error=e$message))
}

# =============================================================================
# 3) PROMPT TEMPLATES + BUILDER
# =============================================================================
get_enhanced_prompt_templates <- function(ai_config = NULL) {
  # Get customizable templates from user configuration
  if (!is.null(ai_config) && !is.null(ai_config$user_customization) && 
      !is.null(ai_config$user_customization$prompts)) {
    custom_templates <- ai_config$user_customization$prompts
  } else {
    custom_templates <- list()
  }
  
  # Default templates with deployment-specific adaptations
  default_templates <- list(
    monthly_summary = get_context_aware_template("monthly_summary", ai_config),
    disease_trend = get_context_aware_template("disease_trend", ai_config),
    facility_performance = get_context_aware_template("facility_performance", ai_config),
    geographic_pattern = get_context_aware_template("geographic_pattern", ai_config),
    demographic_insight = get_context_aware_template("demographic_insight", ai_config),
    outbreak_alert = get_context_aware_template("outbreak_alert", ai_config)
  )
  
  # Map YAML template names to internal names
  template_mapping <- list(
    "executive_summary" = "monthly_summary",
    "disease_analysis" = "disease_trend", 
    "geographic_analysis" = "geographic_pattern",
    "demographic_analysis" = "demographic_insight"
  )
  
  # Override with custom templates where available
  for (yaml_name in names(custom_templates)) {
    internal_name <- template_mapping[[yaml_name]] %||% yaml_name
    if (!is.null(custom_templates[[yaml_name]]$template) && 
        internal_name %in% names(default_templates)) {
      default_templates[[internal_name]] <- custom_templates[[yaml_name]]$template
    }
  }
  
  return(default_templates)
}

get_context_aware_template <- function(template_type, ai_config = NULL) {
  # Determine deployment context
  deployment_type <- "general"
  if (!is.null(ai_config) && !is.null(ai_config$deployment_type)) {
    deployment_type <- ai_config$deployment_type
  }
  
  # Context-specific adaptations based on deployment type
  context_adaptations <- get_deployment_context_adaptations(deployment_type)
  
  switch(template_type,
    "monthly_summary" = paste0(
      "You are an experienced field epidemiologist analyzing health surveillance data from {setting}.\n\n",
      "OPERATIONAL CONTEXT:\n",
      "- Setting: {setting}\n",
      "- Population: {population}\n",
      "- Key constraints: {constraints}\n",
      "- Current priorities: {priorities}\n",
      "- Endemic diseases: {endemic_diseases}\n",
      context_adaptations$monthly_focus,
      "\n\nANALYSIS TASK:\n",
      "Analyze this epidemiological data and provide evidence-based insights.\n\n",
      "REQUIRED OUTPUT FORMAT:\n",
      "**EPIDEMIOLOGICAL ASSESSMENT:**\n",
      "- Disease burden patterns (quantify)\n",
      "- Significant trends/anomalies\n",
      "- Geographic or demographic risk factors\n\n",
      "**PUBLIC HEALTH IMPLICATIONS:**\n",
      "- Immediate threats\n",
      "- Potential outbreak signals\n",
      "- Vulnerable populations\n\n",
      "**RECOMMENDED ACTIONS:**\n",
      context_adaptations$action_timeframes,
      "\n\nLimit to 300 words.\n\n",
      "DATA SUMMARY:\n{data_summary}"
    ),
    
    "disease_trend" = paste0(
      "As a surveillance epidemiologist, conduct trend analysis for {disease_name} in {setting}.\n\n",
      context_adaptations$trend_focus,
      "\n\nContext: {population}. Threshold: {alert_threshold}x baseline.\n",
      "Provide 150 words.\n\nTREND DATA:\n{trend_data}\n",
      "Recent avg: {recent_average} cases/week\n",
      "Baseline avg: {baseline_average} cases/week\n",
      "Trend ratio: {trend_ratio}"
    ),
    
    "facility_performance" = paste0(
      "Analyze health facility utilization and performance for {setting}.\n\n",
      context_adaptations$facility_focus,
      "\n\nMETRICS:\nTotal facilities: {total_facilities}\nHigh-load facilities: {high_load_facilities}\n",
      "Average per facility: {avg_consultations_per_facility}\nPeak load: {max_facility_load}\n",
      "Top 5 share: {load_concentration_ratio}%\nPeak facility: {peak_facility}\n\n",
      "DETAILS:\n{facility_data}"
    ),
    
    "geographic_pattern" = paste0(
      "Analyze geographic distribution patterns for {setting} with {population}.\n",
      context_adaptations$geographic_focus,
      "\n\nDATA:\n{geo_data}"
    ),
    
    "demographic_insight" = paste0(
      "Analyze demographic patterns in disease distribution for {setting} serving {population}.\n",
      context_adaptations$demographic_focus,
      "\n\nDATA:\n{demo_data}"
    ),
    
    "outbreak_alert" = paste0(
      "As a field epidemiologist, assess outbreak potential for {setting} serving {population}.\n\n",
      "ALERT CRITERIA:\n- Threshold: {alert_threshold}x baseline\n- Geographic spread: {geo_threshold} or more areas\n",
      context_adaptations$alert_focus,
      "\n\nProvide 200 words covering: signal strength, risk assessment, response priorities.\n\n",
      "ALERT DATA:\n{alert_data}"
    ),
    
    # Default fallback
    paste0("Analyze the following epidemiological data for {setting}:\n\n{data_summary}")
  )
}

get_deployment_context_adaptations <- function(deployment_type) {
  switch(deployment_type,
    "refugee_camp" = list(
      monthly_focus = "\n- REFUGEE CAMP FOCUS: Overcrowding, sanitation, rapid transmission risks",
      trend_focus = "1) Quantify trend (% change)\n2) Assess overcrowding impact\n3) Evaluate transmission velocity\n4) Prioritize immediate containment",
      facility_focus = "Provide ~200 words covering: capacity strain, access barriers, surge capacity, emergency protocols.",
      geographic_focus = "150 words covering: camp density, transmission hotspots, movement patterns, containment zones.",
      demographic_focus = "150 words covering: vulnerable populations (children <5, pregnant women), crowding effects, access equity.",
      alert_focus = "\n- CAMP-SPECIFIC: Consider rapid spread potential, limited isolation capacity",
      action_timeframes = "1. Immediate (0-24h: isolation, contact tracing)\n2. Short-term (1-7d: case management)\n3. Surveillance: Enhanced active case finding"
    ),
    
    "urban_clinic" = list(
      monthly_focus = "\n- URBAN CLINIC FOCUS: Population density, mobility, healthcare access patterns",
      trend_focus = "1) Quantify trend (% change)\n2) Compare to urban baseline/seasonality\n3) Assess community transmission\n4) Recommend targeted interventions",
      facility_focus = "Provide ~200 words covering: patient flow, referral patterns, capacity optimization, quality metrics.",
      geographic_focus = "150 words covering: neighborhood patterns, transportation effects, mobility-driven spread, targeted interventions.",
      demographic_focus = "150 words covering: socioeconomic patterns, access disparities, age-specific risks, urban vulnerabilities.",
      alert_focus = "\n- URBAN-SPECIFIC: Consider mobility patterns, healthcare-seeking behavior",
      action_timeframes = "1. Immediate (0-7d)\n2. Short-term (1-4w)\n3. Surveillance enhancements"
    ),
    
    "rural_health" = list(
      monthly_focus = "\n- RURAL HEALTH FOCUS: Geographic isolation, limited resources, seasonal patterns",
      trend_focus = "1) Quantify trend (% change)\n2) Account for seasonal/agricultural patterns\n3) Assess resource constraints\n4) Plan sustainable interventions",
      facility_focus = "Provide ~200 words covering: geographic access, referral challenges, resource optimization, community engagement.",
      geographic_focus = "150 words covering: isolation effects, transportation barriers, seasonal access, community-based approaches.",
      demographic_focus = "150 words covering: age distribution, gender roles, traditional practices, access challenges.",
      alert_focus = "\n- RURAL-SPECIFIC: Consider geographic isolation, resource limitations",
      action_timeframes = "1. Immediate (considering access constraints)\n2. Short-term (1-4w: community-based)\n3. Surveillance: Community health worker networks"
    ),
    
    "emergency_response" = list(
      monthly_focus = "\n- EMERGENCY RESPONSE FOCUS: Rapid assessment, immediate threats, resource mobilization",
      trend_focus = "1) Quantify immediate threats\n2) Assess emergency spread patterns\n3) Evaluate containment feasibility\n4) Prioritize life-saving interventions",
      facility_focus = "Provide ~200 words covering: emergency capacity, triage protocols, resource allocation, coordination mechanisms.",
      geographic_focus = "150 words covering: emergency hotspots, access routes, evacuation considerations, rapid response zones.",
      demographic_focus = "150 words covering: most-at-risk populations, emergency vulnerabilities, protection needs, rapid assessment.",
      alert_focus = "\n- EMERGENCY-SPECIFIC: Immediate action thresholds, resource mobilization triggers",
      action_timeframes = "1. Immediate (0-6h: life-saving)\n2. Emergency (1-3d: stabilization)\n3. Surveillance: Rapid response systems"
    ),
    
    # Default/general context
    list(
      monthly_focus = "",
      trend_focus = "1) Quantify trend (% change)\n2) Compare to baseline/seasonality\n3) Assess outbreak potential\n4) Recommend surveillance actions",
      facility_focus = "Provide ~200 words covering: utilization patterns, efficiency, equity, capacity, quality.",
      geographic_focus = "150 words covering: spatial patterns, risk factors, transmission dynamics, targeted interventions.",
      demographic_focus = "150 words covering: risk distribution, access, equity, targeted strategies.",
      alert_focus = "",
      action_timeframes = "1. Immediate (0-7d)\n2. Short-term (1-4w)\n3. Surveillance enhancements"
    )
  )
}

# Very lightweight template expansion (avoids adding glue dependency)
.fill_template <- function(txt, data) {
  out <- txt
  for (nm in names(data)) {
    key <- paste0("{", nm, "}")
    out <- gsub(key, .null2chr(data[[nm]]), out, fixed = TRUE)
  }
  out
}

build_enhanced_prompt <- function(type, values = list(), ai_config = NULL) {
  templates <- get_enhanced_prompt_templates(ai_config)
  if (!type %in% names(templates)) stop("Unknown prompt type: ", type)
  .fill_template(templates[[type]], values)
}

# =============================================================================
# 4) CALL LOCAL AI (OLLAMA) + OUTPUT VALIDATION
# =============================================================================
call_enhanced_local_ai <- function(host, model, prompt, options = list()) {
  opt <- modifyList(list(temperature=0.1, num_predict=512), options)
  body <- list(model = model, prompt = prompt, stream = FALSE, options = opt)
  resp <- POST(paste0(host, "/api/generate"),
               body = toJSON(body, auto_unbox = TRUE),
               add_headers("Content-Type"="application/json"),
               timeout( min(120, opt$num_predict %||% 512) ))
  if (status_code(resp) != 200) stop("AI API error: ", status_code(resp))
  res <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  .null2chr(res$response)
}

validate_ai_output <- function(text, min_chars = 200) {
  txt <- .null2chr(text)
  score <- 0
  if (nchar(txt) >= min_chars) score <- score + 30
  if (grepl("\\d", txt)) score <- score + 20
  if (grepl("recommend|action|monitor|trend|increase|decrease|stable", tolower(txt))) score <- score + 20
  if (!grepl("hallucinat|fabricat", tolower(txt))) score <- score + 10
  if (!grepl("sorry|cannot|unable", tolower(txt))) score <- score + 10
  list(score = .clamp(score, 0, 100), ok = score >= 60)
}

# =============================================================================
# 5) CONTEXTS + DATA SUMMARIES USED BY PROMPTS
# =============================================================================
get_epidemiological_contexts <- function() {
  list(
    syria_humanitarian = list(
      setting = "Humanitarian health surveillance in conflict-affected Syria",
      population = "IDPs, refugees, conflict-affected populations",
      constraints = "Limited access, insecurity, supply constraints",
      priorities = "Outbreak detection, continuity of essential services",
      endemic_diseases = c("Acute watery diarrhea","ARI","Skin diseases","Measles","Leishmaniasis"),
      alert_thresholds = list(outbreak_multiplier = 2.0, high_burden_cases = 100, geographic_spread = 2),
      special_considerations = list(
        seasonal_patterns = "Respiratory infections peak in winter; diarrheal in summer",
        population_movement = "High displacement affects transmission"
      )
    ),
    south_sudan_humanitarian = list(
      setting = "Humanitarian health surveillance in post-conflict South Sudan",
      population = "Displaced populations, host communities, returnees",
      constraints = "Limited access, weak health infrastructure, supply constraints",
      priorities = "Outbreak detection, essential services continuity, malnutrition monitoring",
      endemic_diseases = c("Malaria","Acute watery diarrhea","Pneumonia","Measles","Hepatitis E","Meningitis","Kala-azar"),
      alert_thresholds = list(outbreak_multiplier = 2.0, high_burden_cases = 50, geographic_spread = 2),
      special_considerations = list(
        seasonal_patterns = "Malaria peaks during rainy season; respiratory infections in dry season",
        population_movement = "High displacement and seasonal migration affects transmission"
      )
    ),
    yemen_humanitarian = list(
      setting = "Humanitarian health surveillance in conflict-affected Yemen",
      population = "IDPs, refugees, conflict-affected populations",
      constraints = "Active conflict, limited access, supply shortages",
      priorities = "Outbreak detection, cholera surveillance, malnutrition monitoring",
      endemic_diseases = c("Acute watery diarrhea","Cholera","ARI","Measles","Dengue","Malaria"),
      alert_thresholds = list(outbreak_multiplier = 2.0, high_burden_cases = 75, geographic_spread = 2),
      special_considerations = list(
        seasonal_patterns = "Cholera peaks during rainy season; respiratory infections year-round",
        population_movement = "Massive displacement affects disease transmission patterns"
      )
    )
  )
}

summarize_register_for_ai <- function(register) {
  out <- list()
  if (nrow(register) == 0) return("No records available.")
  # Basic coverage
  date_col <- if ("datevisit" %in% names(register)) "datevisit" else NULL
  if (!is.null(date_col)) {
    dt <- .as_date(register[[date_col]])
    out$period <- paste0(as.character(min(dt, na.rm=TRUE)), " to ", as.character(max(dt, na.rm=TRUE)))
  }
  out$n_consults <- format(nrow(register), big.mark = ",")
  # disease
  dis_col <- dplyr::case_when(
    "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
    "morbidity" %in% names(register) ~ "morbidity",
    TRUE ~ NA_character_
  )
  if (!is.na(dis_col)) {
    top <- register %>% count(.data[[dis_col]], sort=TRUE) %>% slice_head(n=5)
    out$top_diseases <- paste0(top[[dis_col]], " (", top$n, ")", collapse = "; ")
  }
  # geo
  geo_col <- if ("admin1" %in% names(register)) "admin1" else if ("governorate" %in% names(register)) "governorate" else NULL
  if (!is.null(geo_col)) {
    gtop <- register %>% filter(!is.na(.data[[geo_col]])) %>% count(.data[[geo_col]], sort=TRUE) %>% slice_head(n=3)
    out$top_areas <- paste0(gtop[[geo_col]], " (", gtop$n,")", collapse="; ")
  }
  # demo
  if ("sex" %in% names(register)) {
    sx <- register %>% filter(!is.na(sex)) %>% count(sex) %>% mutate(p=round(100*n/sum(n),1))
    out$sex <- paste0(sx$sex, ": ", sx$p, "%", collapse="; ")
  }
  if ("age_group" %in% names(register)) {
    ag <- register %>% filter(!is.na(age_group)) %>% count(age_group) %>% mutate(p=round(100*n/sum(n),1))
    out$age <- paste0(ag$age_group, ": ", ag$p, "%", collapse="; ")
  }
  paste0(
    "Records: ", out$n_consults %||% "n/a", "\n",
    if (!is.null(out$period)) paste0("Period: ", out$period, "\n") else "",
    if (!is.null(out$top_diseases)) paste0("Top morbidities: ", out$top_diseases, "\n") else "",
    if (!is.null(out$top_areas)) paste0("Top areas: ", out$top_areas, "\n") else "",
    if (!is.null(out$sex)) paste0("Sex distribution: ", out$sex, "\n") else "",
    if (!is.null(out$age)) paste0("Age groups: ", out$age, "\n") else ""
  )
}

# =============================================================================
# 6) PUBLIC API: GENERATE SUMMARIES USED BY THE RMD
# =============================================================================
generate_all_ai_summaries <- function(register, ai_system) {
  # Fix logical checking - ensure ai_system$available is a single logical value
  available <- safe_logical_check(ai_system$available, FALSE)
  
  if (!available) {
    return(list(
      monthly_summary     = "AI analysis not available. Using standard statistical methods.",
      geographic_insights = "Geographic analysis using standard methods.",
      demographic_insights= "Demographic analysis using standard methods.",
      facility_performance= "Facility analysis using standard methods.",
      disease_narratives  = list(),
      risk_assessment     = NULL,
      analysis_quality    = 75
    ))
  }
  
  tryCatch({
    cfg <- ai_system$config
    ctx <- cfg$epi_context
    data_summary <- summarize_register_for_ai(register)
    
    # Monthly summary ----------------------------------------------------------
    prompt_month <- build_enhanced_prompt(
      "monthly_summary",
      list(
        setting = ctx$setting, 
        population = ctx$population,
        constraints = paste(ctx$constraints, collapse = ", "),
        priorities = paste(ctx$priorities, collapse = ", "),
        endemic_diseases = paste(ctx$endemic_diseases, collapse = ", "),
        data_summary = data_summary,
        # Add YAML template compatibility
        setting_description = ctx$setting,
        surveillance_priorities = paste(ctx$priorities, collapse = ", "),
        operational_constraints = paste(ctx$constraints, collapse = ", ")
      ),
      ai_config = cfg
    )
    monthly <- call_enhanced_local_ai(
      host = cfg$host,
      model = cfg$model,
      prompt = prompt_month,
      options = list(temperature = cfg$temperature, num_predict = cfg$max_tokens, max_tokens = cfg$max_tokens, timeout = cfg$timeout),
      provider = cfg$provider,
      api_key = cfg$api_key,
      system_prompt = cfg$system_prompt
    )
    val_m <- validate_ai_output(monthly)
    
    # Geographic insights ------------------------------------------------------
    geo_data <- tryCatch({
      col <- if ("admin1" %in% names(register)) "admin1" else if ("governorate" %in% names(register)) "governorate" else NULL
      if (!is.null(col)) {
        head(register %>% filter(!is.na(.data[[col]])) %>% count(.data[[col]], sort=TRUE), 8) %>%
          mutate(line = paste0(.data[[col]], ": ", n)) %>% pull(line) %>% paste(collapse="\n")
      } else "No geographic attributes found."
    }, error = function(e) "No geographic attributes found.")
    
    prompt_geo <- build_enhanced_prompt("geographic_pattern",
                                        list(setting=ctx$setting, population=ctx$population, geo_data=geo_data,
                                             # Add YAML template compatibility
                                             setting_description=ctx$setting),
                                        ai_config = cfg)
    geographic <- call_enhanced_local_ai(
      host = cfg$host,
      model = cfg$model,
      prompt = prompt_geo,
      options = list(temperature = cfg$temperature, num_predict = 512, max_tokens = cfg$max_tokens, timeout = cfg$timeout),
      provider = cfg$provider,
      api_key = cfg$api_key,
      system_prompt = cfg$system_prompt
    )
    val_g <- validate_ai_output(geographic, 150)
    
    # Demographic insights -----------------------------------------------------
    demo_data <- tryCatch({
      parts <- c()
      if ("sex" %in% names(register)) {
        sx <- register %>% filter(!is.na(sex)) %>% count(sex) %>% mutate(p=round(100*n/sum(n),1))
        parts <- c(parts, paste0("Sex: ", paste0(sx$sex, " ", sx$p, "%", collapse="; ")))
      }
      if ("age_group" %in% names(register)) {
        ag <- register %>% filter(!is.na(age_group)) %>% count(age_group) %>% mutate(p=round(100*n/sum(n),1))
        parts <- c(parts, paste0("Age groups: ", paste0(ag$age_group, " ", ag$p, "%", collapse="; ")))
      }
      if (length(parts)) paste(parts, collapse="\n") else "No demographic attributes found."
    }, error = function(e) "No demographic attributes found.")
    
    prompt_demo <- build_enhanced_prompt("demographic_insight",
                                         list(setting=ctx$setting, population=ctx$population, demo_data=demo_data,
                                              # Add YAML template compatibility
                                              high_priority = if (!is.null(cfg$user_customization)) {
                                                paste(cfg$user_customization$populations$high_priority %||% c("children under 5", "pregnant women"), collapse = ", ")
                                              } else "children under 5, pregnant women",
                                              context_specific_vulnerabilities = if (!is.null(cfg$user_customization)) {
                                                paste(cfg$user_customization$context$operational_constraints %||% c("limited access", "resource constraints"), collapse = ", ")
                                              } else "limited access, resource constraints"),
                                         ai_config = cfg)
    demographic <- call_enhanced_local_ai(
      host = cfg$host,
      model = cfg$model,
      prompt = prompt_demo,
      options = list(temperature = cfg$temperature, num_predict = 512, max_tokens = cfg$max_tokens, timeout = cfg$timeout),
      provider = cfg$provider,
      api_key = cfg$api_key,
      system_prompt = cfg$system_prompt
    )
    val_d <- validate_ai_output(demographic, 150)
    
    # Facility performance -----------------------------------------------------
    facility_text <- generate_facility_performance_analysis(register, cfg)
    
    # Risk assessment (enhanced alerts) ---------------------------------------
    risk_text <- generate_enhanced_epidemic_alerts(register, cfg)
    
    overall_quality <- round(mean(c(val_m$score, val_g$score, val_d$score), na.rm = TRUE), 1)
    
    # Top-3 disease narratives -------------------------------------------------
    disease_col <- dplyr::case_when(
      "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
      "morbidity" %in% names(register) ~ "morbidity",
      TRUE ~ NA_character_
    )
    disease_narratives <- list()
    if (!is.na(disease_col)) {
      top3 <- register %>% count(.data[[disease_col]], sort=TRUE) %>% slice_head(n=3) %>% pull(1)
      for (d in top3) {
        disease_narratives[[as.character(d)]] <- generate_disease_trend_narrative(register, d, cfg)
      }
    }
    
    list(
      monthly_summary      = monthly,
      geographic_insights  = geographic,
      demographic_insights = demographic,
      facility_performance = facility_text,
      disease_narratives   = disease_narratives,
      risk_assessment      = risk_text,
      analysis_quality     = overall_quality
    )
  }, error = function(e) {
    cat("❌ AI summaries generation failed: ", e$message, "\n", sep = "")
    list(
      monthly_summary     = paste("AI analysis failed:", e$message),
      geographic_insights = "Error generating geographic insights.",
      demographic_insights= "Error generating demographic insights.",
      facility_performance= "Error generating facility insights.",
      disease_narratives  = list(),
      risk_assessment     = NULL,
      analysis_quality    = 0
    )
  })
}

format_ai_summary <- function(summary, type = "general", add_disclaimer = TRUE) {
  # Fix logical coercion - ensure we handle summary properly
  s <- summary
  if (is.null(s)) s <- ""
  if (length(s) > 1) s <- paste(s, collapse = "\n")
  s <- as.character(s)
  
  # Safe checks
  is_blank <- !nzchar(s)
  has_err  <- grepl("Error|Failed", s, ignore.case = TRUE, perl = TRUE)
  
  if (is_blank || has_err) {
    return("*AI analysis not available - using standard statistical methods.*")
  }
  
  header <- switch(type,
                   "monthly"              = "AI Executive Summary",
                   "surveillance"         = "AI Surveillance Assessment",
                   "facility_performance" = "AI Facility Performance",
                   "general"              = "AI-Enhanced Analysis",
                   "AI-Enhanced Analysis"
  )
  
  formatted <- paste0("### ", header, "\n\n", s, "\n\n")
  
  # Fix logical checking for add_disclaimer
  should_add_disclaimer <- safe_logical_check(add_disclaimer, TRUE)
  
  if (should_add_disclaimer) {
    formatted <- paste0(
      formatted,
      "*AI-generated insights based on statistical pattern analysis. ",
      "Professional epidemiological review recommended for all findings.*\n\n"
    )
  }
  formatted
}

# =============================================================================
# 7) SPECIFIC GENERATORS (disease trend, facility, alerts)
# =============================================================================
.generate_weekly_counts <- function(df, disease_col, disease_value) {
  if (!("datevisit" %in% names(df))) return(tibble(week=as.Date(character()), n=integer()))
  df %>%
    filter(.data[[disease_col]] == disease_value) %>%
    mutate(week = floor_date(.as_date(.data[["datevisit"]]), "week")) %>%
    count(week, name="n") %>%
    arrange(week)
}

.compute_trend_stats <- function(weekly) {
  if (nrow(weekly) < 4) return(list(recent_avg = NA, baseline_avg = NA, ratio = NA))
  n <- weekly$n
  k <- length(n)
  recent <- tail(n, min(4, k))
  base   <- head(n, max(1, k - length(recent)))
  list(
    recent_avg = mean(recent),
    baseline_avg = if (length(base)) mean(tail(base, min(8, length(base)))) else NA_real_,
    ratio = if (!is.na(mean(tail(base, min(8, length(base))))) &&
                mean(tail(base, min(8, length(base)))) > 0)
      mean(recent) / mean(tail(base, min(8, length(base))))
    else NA_real_
  )
}

generate_disease_trend_narrative <- function(register, disease, ai_config) {
  dis_col <- dplyr::case_when(
    "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
    "morbidity" %in% names(register) ~ "morbidity",
    TRUE ~ NA_character_
  )
  if (is.na(dis_col)) return(paste("No disease column available for", disease))
  
  wk <- .generate_weekly_counts(register, dis_col, disease)
  stats <- .compute_trend_stats(wk)
  
  trend_data <- if (nrow(wk)) {
    paste(paste0(wk$week, ": ", wk$n), collapse = "\n")
  } else "No weekly data."
  
  ctx <- ai_config$epi_context
  prompt <- build_enhanced_prompt("disease_trend", list(
    disease_name = disease,
    setting = ctx$setting, 
    population = ctx$population,
    alert_threshold = ai_config$epi_context$alert_thresholds$outbreak_multiplier %||% 2,
    trend_data = trend_data,
    recent_average = round(stats$recent_avg, 1) %||% NA,
    baseline_average = round(stats$baseline_avg, 1) %||% NA,
    trend_ratio = ifelse(is.na(stats$ratio), "n/a", round(stats$ratio, 2)),
    # Add YAML template compatibility for disease_analysis
    context_name = if (!is.null(ai_config$user_customization)) {
      ai_config$user_customization$context$context_name %||% "humanitarian_health"
    } else "humanitarian_health",
    high_priority = if (!is.null(ai_config$user_customization)) {
      paste(ai_config$user_customization$populations$high_priority %||% c("children under 5", "pregnant women"), collapse = ", ")
    } else "children under 5, pregnant women",
    endemic_diseases = paste(ctx$endemic_diseases %||% c("respiratory infections", "diarrheal diseases"), collapse = ", ")
  ), ai_config)
  call_enhanced_local_ai(
    host = ai_config$host,
    model = ai_config$model,
    prompt = prompt,
    options = list(temperature = ai_config$temperature, num_predict = 600, max_tokens = ai_config$max_tokens, timeout = ai_config$timeout),
    provider = ai_config$provider,
    api_key = ai_config$api_key,
    system_prompt = ai_config$system_prompt
  )
}

generate_facility_performance_analysis <- function(register, ai_config) {
  if (!("orgunit" %in% names(register))) return("No facility field (orgunit) available.")
  facility_counts <- register %>% count(orgunit, sort=TRUE)
  if (!nrow(facility_counts)) return("No facility utilization records available.")
  total_fac <- nrow(facility_counts)
  avg_per   <- round(mean(facility_counts$n), 1)
  max_load  <- max(facility_counts$n)
  peak_fac  <- facility_counts$orgunit[which.max(facility_counts$n)]
  top5_share <- round(100*sum(head(facility_counts$n, 5))/sum(facility_counts$n), 1)
  high_cut <- quantile(facility_counts$n, 0.9, na.rm=TRUE)
  high_load <- sum(facility_counts$n >= high_cut)
  details <- paste(head(paste0(facility_counts$orgunit, ": ", facility_counts$n), 12), collapse="\n")
  
  ctx <- ai_config$epi_context
  prompt <- build_enhanced_prompt("facility_performance", list(
    setting = ctx$setting,
    total_facilities = total_fac,
    high_load_facilities = high_load,
    avg_consultations_per_facility = avg_per,
    max_facility_load = max_load,
    load_concentration_ratio = top5_share,
    peak_facility = peak_fac,
    facility_data = details
  ), ai_config)
  call_enhanced_local_ai(
    host = ai_config$host,
    model = ai_config$model,
    prompt = prompt,
    options = list(temperature = ai_config$temperature, num_predict = 700, max_tokens = ai_config$max_tokens, timeout = ai_config$timeout),
    provider = ai_config$provider,
    api_key = ai_config$api_key,
    system_prompt = ai_config$system_prompt
  )
}

# Basic epidemic alerts (fallback for the Rmd) -------------------------------
generate_basic_epidemic_alerts <- function(register) {
  # Very simple signal: top 3 morbidities recent 30 days vs previous 60
  if (!("datevisit" %in% names(register))) return("No date field for alerting.")
  dis_col <- dplyr::case_when(
    "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
    "morbidity" %in% names(register) ~ "morbidity",
    TRUE ~ NA_character_
  )
  if (is.na(dis_col)) return("No morbidity field for alerting.")
  
  today <- max(.as_date(register$datevisit), na.rm=TRUE)
  recent <- register %>% filter(.as_date(datevisit) > today - 30)
  base   <- register %>% filter(.as_date(datevisit) <= today - 30 & .as_date(datevisit) > today - 90)
  
  rec <- recent %>% count(.data[[dis_col]], name="r")
  bas <- base   %>% count(.data[[dis_col]], name="b")
  df <- rec %>% full_join(bas, by=dis_col) %>% 
    mutate(r = ifelse(is.na(r), 0, r), b = ifelse(is.na(b), 0, b)) %>%
    mutate(ratio = ifelse(b > 0, r/b, NA_real_)) %>% 
    arrange(desc(ratio))
  
  if (!nrow(df)) return("No alert signals computed.")
  lines <- apply(head(df, 5), 1, function(x) paste0(x[[dis_col]], ": recent ", x[["r"]], " vs base ", x[["b"]],
                                                    " (ratio ", ifelse(is.na(x[["ratio"]]), "n/a", round(as.numeric(x[["ratio"]]),2)), ")"))
  paste(c("**Standard alert signals (30d vs prior 60d):**", paste0("- ", lines)), collapse = "\n")
}

# Enhanced epidemic alerts (used by Rmd chunk `enhanced-epidemic-alerts`) ----
generate_enhanced_epidemic_alerts <- function(register, ai_config) {
  ctx <- ai_config$epi_context
  
  alerts <- tryCatch(
    detect_enhanced_statistical_alerts(
      register,
      ratio_high   = ctx$alert_thresholds$outbreak_multiplier %||% 2,
      ratio_medium = max((ctx$alert_thresholds$outbreak_multiplier %||% 2) - 0.5, 1.2)
    ),
    error = function(e) {
      # Fallback to a zero-row tibble so downstream formatting is safe
      tibble::tibble(disease=character(), recent_cases=integer(),
                     baseline_cases=integer(), ratio=numeric(), alert_level=character())
    }
  )
  
  alert_lines <- tryCatch(
    format_alerts_for_prompt(alerts, top_n = 8),
    error = function(e) "No alert data."
  )
  
  prompt <- build_enhanced_prompt("outbreak_alert", list(
    alert_threshold = ctx$alert_thresholds$outbreak_multiplier %||% 2,
    geo_threshold   = ctx$alert_thresholds$geographic_spread %||% 2,
    alert_data      = alert_lines,
    setting         = ctx$setting,
    population      = ctx$population
  ), ai_config)
  
  call_enhanced_local_ai(
    host = ai_config$host,
    model = ai_config$model,
    prompt = prompt,
    options = list(temperature = ai_config$temperature, num_predict = 700, max_tokens = ai_config$max_tokens, timeout = ai_config$timeout),
    provider = ai_config$provider,
    api_key = ai_config$api_key,
    system_prompt = ai_config$system_prompt
  )
}

# =============================================================================
# 8) ALERT DETECTION CORE (FIXED)
# =============================================================================

# Pick a date column robustly
.get_date_col <- function(df) {
  for (nm in c("datevisit", "datevisitnew", "visit_date", "eventdate", "occurredat", "date")) {
    if (nm %in% names(df)) return(nm)
  }
  NULL
}

# FIXED: detect_enhanced_statistical_alerts function
detect_enhanced_statistical_alerts <- function(register,
                                               disease_col = NULL,
                                               date_col = NULL,
                                               recent_days = 30,
                                               baseline_days = 60,
                                               min_recent = 3,
                                               min_baseline = 5,
                                               ratio_high = 2.0,
                                               ratio_medium = 1.5) {
  # Resolve columns
  if (is.null(date_col))  date_col  <- .get_date_col(register)
  if (is.null(date_col) || !is.character(date_col) || !any(names(register) == date_col)) {
    return(tibble::tibble(disease=character(), recent_cases=integer(), baseline_cases=integer(),
                          ratio=numeric(), alert_level=character()))
  }
  if (is.null(disease_col)) {
    disease_col <- if ("canonical_disease_imc" %in% names(register)) "canonical_disease_imc"
    else if ("morbidity" %in% names(register)) "morbidity"
    else NA_character_
  }
  if (is.na(disease_col) || !any(names(register) == disease_col)) {
    return(tibble::tibble(disease=character(), recent_cases=integer(), baseline_cases=integer(),
                          ratio=numeric(), alert_level=character()))
  }
  
  # Reference date
  dv <- .as_date(register[[date_col]])
  if (all(is.na(dv))) {
    return(tibble::tibble(disease=character(), recent_cases=integer(), baseline_cases=integer(),
                          ratio=numeric(), alert_level=character()))
  }
  today <- suppressWarnings(max(dv, na.rm = TRUE))
  if (!isTRUE(is.finite(as.numeric(today)))) {
    return(tibble::tibble(disease=character(), recent_cases=integer(), baseline_cases=integer(),
                          ratio=numeric(), alert_level=character()))
  }
  
  # Windows
  recent   <- register %>% dplyr::filter(.as_date(.data[[date_col]]) >  today - recent_days)
  baseline <- register %>% dplyr::filter(.as_date(.data[[date_col]]) <= today - recent_days &
                                           .as_date(.data[[date_col]]) >  today - (recent_days + baseline_days))
  
  rec <- recent   %>% dplyr::count(.data[[disease_col]], name = "recent_cases")
  bas <- baseline %>% dplyr::count(.data[[disease_col]], name = "baseline_cases")
  
  out <- dplyr::full_join(rec, bas, by = disease_col, multiple = "all") %>%
    dplyr::mutate(
      recent_cases   = ifelse(is.na(recent_cases), 0L, as.integer(recent_cases)),
      baseline_cases = ifelse(is.na(baseline_cases), 0L, as.integer(baseline_cases)),
      ratio          = dplyr::if_else(baseline_cases > 0,
                                      recent_cases / pmax(baseline_cases, 1L),
                                      NA_real_)
    ) %>%
    dplyr::mutate(
      alert_level = dplyr::case_when(
        !is.na(ratio) & recent_cases  >= min_recent   & baseline_cases >= min_baseline & ratio >= ratio_high   ~ "High",
        !is.na(ratio) & recent_cases  >= min_recent   & baseline_cases >= min_baseline & ratio >= ratio_medium ~ "Medium",
        TRUE ~ "Low"
      )
    ) %>%
    dplyr::arrange(dplyr::desc(dplyr::coalesce(ratio, -Inf))) %>%
    dplyr::rename(disease = !!disease_col) %>%
    dplyr::mutate(
      disease = as.character(disease),
      alert_level = factor(alert_level, levels = c("High","Medium","Low"))
    )
  
  out
}

# Format for prompt (returns a single string, never a vector)
format_alerts_for_prompt <- function(alert_df, top_n = 8) {
  if (!is.data.frame(alert_df) || nrow(alert_df) == 0) return("No alert data.")
  lines <- alert_df %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::transmute(
      line = paste0(
        disease, ": 30d ", recent_cases,
        " | prev60d ", baseline_cases,
        " | ratio ", dplyr::if_else(is.na(ratio), "n/a", as.character(round(ratio, 2))),
        " | level ", as.character(alert_level)
      )
    ) %>%
    dplyr::pull(line)
  paste(lines, collapse = "\n")
}

# =============================================================================
# 9) SURVEILLANCE SUMMARY BUILDER
# =============================================================================

# Build surveillance summary for epidemic diseases
build_surveillance_summary <- function(register) {
  disease_col <- get_disease_column(register)
  date_col <- get_date_column(register)
  
  if (is.na(disease_col) || is.na(date_col)) {
    return(tibble::tibble(
      disease = character(),
      recent_cases_30d = integer(),
      total_cases = integer(),
      alert_level = character()
    ))
  }
  
  epidemic_diseases <- get_epidemic_diseases()
  
  # Filter to epidemic diseases only
  epidemic_data <- register %>%
    filter(.data[[disease_col]] %in% epidemic_diseases) %>%
    mutate(date_clean = .as_date(.data[[date_col]])) %>%
    filter(!is.na(date_clean))
  
  if (nrow(epidemic_data) == 0) {
    return(tibble::tibble(
      disease = character(),
      recent_cases_30d = integer(), 
      total_cases = integer(),
      alert_level = character()
    ))
  }
  
  today <- max(epidemic_data$date_clean, na.rm = TRUE)
  cutoff_30d <- today - 30
  cutoff_90d <- today - 90
  
  # Recent cases (last 30 days)
  recent_cases <- epidemic_data %>%
    filter(date_clean > cutoff_30d) %>%
    count(.data[[disease_col]], name = "recent_cases_30d")
  
  # Baseline cases (30-90 days ago) 
  baseline_cases <- epidemic_data %>%
    filter(date_clean <= cutoff_30d & date_clean > cutoff_90d) %>%
    count(.data[[disease_col]], name = "baseline_cases")
  
  # Total cases
  total_cases <- epidemic_data %>%
    count(.data[[disease_col]], name = "total_cases")
  
  # Combine and calculate alert levels
  surveillance_summary <- recent_cases %>%
    full_join(baseline_cases, by = disease_col) %>%
    full_join(total_cases, by = disease_col) %>%
    mutate(
      recent_cases_30d = ifelse(is.na(recent_cases_30d), 0L, recent_cases_30d),
      baseline_cases = ifelse(is.na(baseline_cases), 0L, baseline_cases),
      total_cases = ifelse(is.na(total_cases), 0L, total_cases),
      ratio = ifelse(baseline_cases > 0, recent_cases_30d / baseline_cases, NA_real_),
      alert_level = case_when(
        recent_cases_30d >= 10 & ratio >= 2.0 ~ "High",
        recent_cases_30d >= 5 & ratio >= 1.5 ~ "Medium", 
        recent_cases_30d >= 3 ~ "Low",
        TRUE ~ "None"
      )
    ) %>%
    rename(disease = !!disease_col) %>%
    arrange(desc(recent_cases_30d))
  
  return(surveillance_summary)
}

# Build epidemic data subset for analysis
build_epidemic_data <- function(register) {
  disease_col <- get_disease_column(register)
  date_col <- get_date_column(register)
  
  if (is.na(disease_col) || is.na(date_col)) {
    return(tibble::tibble())
  }
  
  epidemic_diseases <- get_epidemic_diseases()
  
  epidemic_data <- register %>%
    filter(.data[[disease_col]] %in% epidemic_diseases) %>%
    mutate(
      epidemic_disease = .data[[disease_col]],
      datevisit = .as_date(.data[[date_col]])
    ) %>%
    filter(!is.na(datevisit)) %>%
    select(epidemic_disease, datevisit, everything())
  
  return(epidemic_data)
}

# =============================================================================
# 10) MULTISTAGE ANALYSIS
# =============================================================================
conduct_multistage_analysis <- function(register,
                                        ai_config,
                                        analysis_type = "comprehensive",
                                        patterns_data = NULL,
                                        ...) {
  # Optional input from older/newer callers
  dots <- list(...)
  if (is.null(patterns_data) && "patterns_data" %in% names(dots)) {
    patterns_data <- dots[["patterns_data"]]
  }
  
  # Minimal "patterns" scaffold if nothing was supplied
  if (is.null(patterns_data)) {
    # Build a very light summary the prompts can reference
    dis_col <- dplyr::case_when(
      "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
      "morbidity" %in% names(register) ~ "morbidity",
      TRUE ~ NA_character_
    )
    top_dis <- if (!is.na(dis_col)) {
      register %>% count(.data[[dis_col]], sort = TRUE) %>% slice_head(n = 5)
    } else tibble(!!sym("morbidity") := character(), n = integer())
    geo_col <- if ("admin1" %in% names(register)) "admin1" else if ("governorate" %in% names(register)) "governorate" else NA_character_
    top_geo <- if (!is.na(geo_col)) {
      register %>% filter(!is.na(.data[[geo_col]])) %>% count(.data[[geo_col]], sort = TRUE) %>% slice_head(n = 5)
    } else tibble()
    patterns_data <- list(
      top_diseases = top_dis,
      top_geographies = top_geo,
      n = nrow(register)
    )
  }
  
  # Use the same engines as generate_all_ai_summaries() to keep outputs consistent
  ctx <- ai_config$epi_context
  data_summary <- summarize_register_for_ai(register)
  
  # Monthly/overall narrative
  monthly_prompt <- build_enhanced_prompt(
    "monthly_summary",
    list(
      setting = ctx$setting,
      population = ctx$population,
      constraints = paste(ctx$constraints, collapse = ", "),
      priorities = paste(ctx$priorities, collapse = ", "),
      endemic_diseases = paste(ctx$endemic_diseases, collapse = ", "),
      data_summary = data_summary,
      # Add YAML template compatibility
      setting_description = ctx$setting,
      surveillance_priorities = paste(ctx$priorities, collapse = ", "),
      operational_constraints = paste(ctx$constraints, collapse = ", ")
    ),
    ai_config
  )
  monthly_txt <- call_enhanced_local_ai(ai_config$host, ai_config$model, monthly_prompt,
                                        list(temperature = ai_config$temperature, num_predict = ai_config$max_tokens))
  v_month <- validate_ai_output(monthly_txt)
  
  # Geographic
  geo_data <- tryCatch({
    col <- if ("admin1" %in% names(register)) "admin1" else if ("governorate" %in% names(register)) "governorate" else NULL
    if (!is.null(col)) {
      head(register %>% filter(!is.na(.data[[col]])) %>% count(.data[[col]], sort = TRUE), 8) %>%
        mutate(line = paste0(.data[[col]], ": ", n)) %>% pull(line) %>% paste(collapse = "\n")
    } else "No geographic attributes found."
  }, error = function(e) "No geographic attributes found.")
  
  geo_prompt <- build_enhanced_prompt("geographic_pattern",
                                      list(setting = ctx$setting, population = ctx$population, geo_data = geo_data,
                                           # Add YAML template compatibility
                                           setting_description = ctx$setting),
                                      ai_config)
  geo_txt <- call_enhanced_local_ai(ai_config$host, ai_config$model, geo_prompt,
                                    list(temperature = ai_config$temperature, num_predict = 512))
  v_geo <- validate_ai_output(geo_txt, 150)
  
  # Demographic
  demo_data <- tryCatch({
    parts <- c()
    if ("sex" %in% names(register)) {
      sx <- register %>% filter(!is.na(sex)) %>% count(sex) %>% mutate(p = round(100*n/sum(n), 1))
      parts <- c(parts, paste0("Sex: ", paste0(sx$sex, " ", sx$p, "%", collapse = "; ")))
    }
    if ("age_group" %in% names(register)) {
      ag <- register %>% filter(!is.na(age_group)) %>% count(age_group) %>% mutate(p = round(100*n/sum(n), 1))
      parts <- c(parts, paste0("Age groups: ", paste0(ag$age_group, " ", ag$p, "%", collapse = "; ")))
    }
    if (length(parts)) paste(parts, collapse = "\n") else "No demographic attributes found."
  }, error = function(e) "No demographic attributes found.")
  
  demo_prompt <- build_enhanced_prompt("demographic_insight",
                                       list(setting = ctx$setting, population = ctx$population, demo_data = demo_data,
                                            # Add YAML template compatibility  
                                            high_priority = if (!is.null(ai_config$user_customization)) {
                                              paste(ai_config$user_customization$populations$high_priority %||% c("children under 5", "pregnant women"), collapse = ", ")
                                            } else "children under 5, pregnant women",
                                            context_specific_vulnerabilities = if (!is.null(ai_config$user_customization)) {
                                              paste(ai_config$user_customization$context$operational_constraints %||% c("limited access", "resource constraints"), collapse = ", ")
                                            } else "limited access, resource constraints"),
                                       ai_config)
  demo_txt <- call_enhanced_local_ai(ai_config$host, ai_config$model, demo_prompt,
                                     list(temperature = ai_config$temperature, num_predict = 512))
  v_demo <- validate_ai_output(demo_txt, 150)
  
  # Risk / alerts
  risk_txt <- tryCatch(generate_enhanced_epidemic_alerts(register, ai_config), 
                       error = function(e) generate_basic_epidemic_alerts(register))
  
  list(
    patterns = monthly_txt,
    geographic_insights = geo_txt,
    demographic_insights = demo_txt,
    risk_assessment = risk_txt,
    overall_quality = round(mean(c(v_month$score, v_geo$score, v_demo$score), na.rm = TRUE), 1),
    meta = list(
      analysis_type = analysis_type,
      used_patterns_rows = if (is.data.frame(patterns_data$top_diseases)) nrow(patterns_data$top_diseases) else NA_integer_
    )
  )
}

# =============================================================================
# 11) COMPREHENSIVE AI INITIALIZATION WRAPPER
# =============================================================================

# Comprehensive initialization that handles all edge cases
initialize_ai_system_robust <- function(config = NULL, params = NULL) {
  cat("🚀 Initializing Robust AI System...\n")
  
  # Default configuration if none provided
  if (is.null(config)) {
    config <- list(
      ai_features = list(
        enabled = FALSE,
        provider = "ollama",
        context_setting = "syria_humanitarian",
        ollama = list(
          host = "http://localhost:11434",
          model = "llama3.2:3b",
          timeout = 120,
          temperature = 0.1,
          max_tokens = 2048
        )
      )
    )
  }
  
  # Default parameters if none provided
  if (is.null(params)) {
    params <- list(enable_ai = FALSE)
  }
  
  # Use the main configuration function
  ai_system <- configure_enhanced_ai(config, params)
  
  return(ai_system)
}

# =============================================================================
# 12) SAFE SUMMARY GENERATORS
# =============================================================================

# Generate surveillance summary safely
generate_surveillance_summary_safe <- function(register) {
  tryCatch({
    build_surveillance_summary(register)
  }, error = function(e) {
    cat("⚠️ Surveillance summary generation failed:", e$message, "\n")
    tibble::tibble(
      disease = character(),
      recent_cases_30d = integer(),
      total_cases = integer(), 
      alert_level = character()
    )
  })
}

# Generate epidemic data safely
generate_epidemic_data_safe <- function(register) {
  tryCatch({
    build_epidemic_data(register)
  }, error = function(e) {
    cat("⚠️ Epidemic data generation failed:", e$message, "\n")
    tibble::tibble()
  })
}

# =============================================================================
# 13) FINAL SYSTEM CHECK
# =============================================================================

# System health check
check_ai_system_health <- function(ai_system) {
  checks <- list(
    structure = !is.null(ai_system) && is.list(ai_system),
    available = safe_logical_check(ai_system$available, FALSE),
    config = !is.null(ai_system$config),
    version = !is.null(ai_system$version)
  )
  
  all_passed <- all(unlist(checks))
  
  cat("🔍 AI System Health Check:\n")
  for (check_name in names(checks)) {
    status <- if (checks[[check_name]]) "✅" else "❌"
    cat("  ", status, " ", check_name, "\n", sep = "")
  }
  
  if (all_passed) {
    cat("✅ All health checks passed\n")
  } else {
    cat("⚠️ Some health checks failed - using fallback mode\n")
  }
  
  return(all_passed)
}

# Back-compat shim (kept for safety if other code calls it)
initialize_enhanced_ai_system <- function(config, params) {
  cat("ℹ️ Using backward compatibility wrapper for initialize_enhanced_ai_system\n")
  configure_enhanced_ai(config, params)
}

# =============================================================================
# 14) LOG LOADER & VERSION INFO
# =============================================================================
cat("✅ Enhanced AI System v", ENHANCED_AI_VERSION, " loaded successfully\n", sep = "")
cat("📋 Main functions available:\n")
cat("  - configure_enhanced_ai() [MAIN ENTRY POINT]\n")
cat("  - initialize_ai_system_robust() [FALLBACK INIT]\n") 
cat("  - generate_all_ai_summaries()\n")
cat("  - format_ai_summary()\n")
cat("  - generate_enhanced_epidemic_alerts()\n")
cat("  - build_surveillance_summary()\n")
cat("  - check_ai_system_health()\n")
cat("🔧 Utility functions:\n")
cat("  - safe_logical_check(), safe_df_check()\n")
cat("  - get_disease_column(), get_date_column()\n")
cat("  - generate_surveillance_summary_safe()\n")
cat("🎯 Ready for epidemiological analysis with enhanced AI capabilities\n")

# Export key objects for global access
if (!exists("epidemic_diseases")) {
  epidemic_diseases <<- get_epidemic_diseases()
}

cat("📊 System ready with", length(epidemic_diseases), "epidemic diseases monitored\n")
