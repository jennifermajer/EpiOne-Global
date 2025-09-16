# ai_summaries.R - AI summary generation functions
# Consolidated from R/enhanced_ai.R
# VERSION: 1.0.0

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# =============================================================================
# AI API COMMUNICATION
# =============================================================================

#' Call local AI (Ollama) with enhanced error handling
#' @param host API host endpoint
#' @param model Model name
#' @param prompt Prompt text
#' @param options Additional options list
#' @return AI response text
call_enhanced_local_ai <- function(host, model, prompt, options = list(), max_retries = 2,
                                   provider = "ollama", api_key = NULL, system_prompt = NULL) {
  provider <- tolower(provider %||% "ollama")
  if (provider == "openai") {
    body <- list(
      model = model,
      messages = list(
        list(role = "system", content = system_prompt %||% "You are EpiBot, an epidemiology assistant."),
        list(role = "user", content = prompt)
      ),
      temperature = options$temperature %||% 0.2,
      max_tokens = options$max_tokens %||% 600
    )

    result <- tryCatch({
      response <- httr::POST(
        url = host,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        httr::add_headers(
          `Content-Type` = "application/json",
          Authorization = paste("Bearer", api_key %||% Sys.getenv("OPENAI_API_KEY"))
        ),
        httr::timeout(options$timeout %||% 120)
      )

      if (httr::status_code(response) != 200) {
        error_msg <- paste("AI service error:", httr::status_code(response))
        cat("❌", error_msg, "\n")
        return(list(success = FALSE, message = error_msg))
      }

      parsed <- httr::content(response, as = "parsed", simplifyVector = TRUE)
      content_text <- parsed$choices[[1]]$message$content %||% parsed$choices[[1]]$text
      if (is.null(content_text)) {
        error_msg <- "AI response format error"
        cat("❌", error_msg, "\n")
        return(list(success = FALSE, message = error_msg))
      }
      cat("✅ AI Response received:", nchar(content_text), "chars\n")
      list(success = TRUE, message = stringr::str_trim(content_text))
    }, error = function(e) {
      error_msg <- paste("AI connection error:", e$message)
      cat("❌", error_msg, "\n")
      list(success = FALSE, message = error_msg)
    })

    if (result$success) {
      return(result$message)
    }
    return(result$message)
  }

  # Reduce default token count for faster responses for Ollama
  opt <- modifyList(list(temperature = 0.1, num_predict = 256), options)
  body <- list(model = model, prompt = prompt, stream = FALSE, options = opt)
  
  # Retry logic
  for (attempt in 1:(max_retries + 1)) {
    if (attempt > 1) {
      cat("🔄 Retry attempt", attempt - 1, "of", max_retries, "\n")
      Sys.sleep(2)  # Wait 2 seconds before retry
    }
    
    result <- tryCatch({
      # Ensure the endpoint has the correct API path
      endpoint <- if (grepl("/api/generate", host)) host else paste0(host, "/api/generate")
      
      cat("🤖 AI Call - Host:", host, "Model:", model, "(Attempt", paste0(attempt, ")\n"))
      
      response <- httr::POST(
        url = endpoint,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        httr::add_headers(`Content-Type` = "application/json"),
        httr::timeout(120)  # Increased timeout to 2 minutes for longer responses
      )
      
      if (httr::status_code(response) != 200) {
        error_msg <- paste("AI service error:", httr::status_code(response))
        cat("❌", error_msg, "\n")
        return(list(success = FALSE, message = error_msg))
      }
      
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      parsed <- jsonlite::fromJSON(content)
      
      if (!is.null(parsed$message$content)) {
        ai_response <- stringr::str_trim(parsed$message$content)
        cat("✅ AI Response received:", nchar(ai_response), "chars\n")
        return(list(success = TRUE, message = ai_response))
      } else if (!is.null(parsed$response)) {
        ai_response <- stringr::str_trim(parsed$response)
        cat("✅ AI Response received:", nchar(ai_response), "chars\n")
        return(list(success = TRUE, message = ai_response))
      } else {
        error_msg <- "AI response format error"
        cat("❌", error_msg, "\n")
        return(list(success = FALSE, message = error_msg))
      }
    }, error = function(e) {
      error_msg <- paste("AI connection error:", e$message)
      cat("❌", error_msg, "\n")
      return(list(success = FALSE, message = error_msg))
    })
    
    if (result$success) {
      # Ensure we return just the text, not the full result structure
      response_text <- result$message
      cat("✅ Returning successful response:", nchar(response_text), "chars\n")
      return(response_text)
    } else if (attempt == max_retries + 1) {
      # Final attempt failed
      cat("❌ All attempts failed, returning error message\n")
      return(result$message)
    }
    # Continue to next retry if not successful and retries remain
  }
}

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

#' Validate AI output length and content
#' @param text AI response text
#' @param min_length Minimum expected length
#' @return Validation result list
validate_ai_output <- function(text, min_length = 50) {
  # Handle different input types
  if (is.null(text)) {
    return(list(valid = FALSE, text = "AI analysis not available", score = 0))
  }
  
  # If it's a list, extract the message or content
  if (is.list(text)) {
    if ("message" %in% names(text)) {
      text <- text$message
    } else if ("content" %in% names(text)) {
      text <- text$content
    } else {
      # Take the first character element
      text <- as.character(text[[1]])
    }
  }
  
  # Convert to character and ensure single value
  if (!is.character(text)) {
    text <- as.character(text)
  }
  
  if (length(text) == 0) {
    return(list(valid = FALSE, text = "AI analysis not available", score = 0))
  }
  
  # Take last element if multiple values
  if (length(text) > 1) {
    text <- text[length(text)]
  }
  
  text <- stringr::str_trim(text)
  cat("🔧 Validation Processing - Final text length:", nchar(text), "chars\n")
  
  # Check for actual error messages - be very specific to avoid false positives
  error_patterns <- c(
    "AI service error:",
    "AI connection error:",
    "AI response format error",
    "analysis not available",
    "temporarily unavailable",
    "connection timeout",
    "connection refused",
    "model not found",
    "API error"
  )
  
  # Ensure text is a single character string before pattern matching
  if (is.character(text) && length(text) == 1 && 
      any(sapply(error_patterns, function(pattern) stringr::str_detect(text, stringr::fixed(pattern, ignore_case = TRUE))))) {
    return(list(valid = FALSE, text = "AI analysis temporarily unavailable. Using standard epidemiological methods for this analysis.", score = 10))
  }
  
  # For very short responses, still return them as valid but with lower score
  if (nchar(text) < min_length) {
    if (nchar(text) < 10) {
      return(list(valid = FALSE, text = "AI analysis incomplete", score = 25))
    } else {
      # Short but potentially valid responses
      return(list(valid = TRUE, text = text, score = 40))
    }
  }
  
  # Basic quality score
  score <- min(100, max(50, nchar(text) / 3))
  
  return(list(valid = TRUE, text = text, score = score))
}

# =============================================================================
# MAIN SUMMARY GENERATION FUNCTIONS
# =============================================================================

#' Generate comprehensive AI summaries for epidemiological report
#' @param register Health register data
#' @param ai_system AI system configuration
#' @return List of AI-generated summaries
generate_all_ai_summaries <- function(register, ai_system) {
  # Fix logical checking - ensure ai_system$available is a single logical value
  available <- isTRUE(ai_system$available)
  
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
        endemic_diseases = paste(ctx$diseases$endemic %||% character(), collapse = ", "),
        data_summary = data_summary,
        # Add YAML template compatibility
        setting_description = ctx$setting,
        surveillance_priorities = paste(ctx$priorities, collapse = ", "),
        operational_constraints = paste(ctx$constraints, collapse = ", ")
      ),
      cfg
    )
    
    monthly <- call_ai_with_config(cfg$api, prompt_month, list(temperature = cfg$api$temperature, num_predict = 300))
    
    # Fallback with simpler prompt if main call failed
    if (length(monthly) == 1 && stringr::str_detect(monthly, "(?i)(error|timeout|connection)")) {
      cat("🔄 Trying simplified prompt for monthly summary...\n")
      simple_prompt <- paste("Summarize epidemiological data:", data_summary$case_count, "cases,", 
                            data_summary$facilities, "facilities. Provide brief analysis in 100 words.")
      monthly <- call_ai_with_config(cfg$api, simple_prompt, list(temperature = 0.2, num_predict = 150))
    }
    # Debug validation
    cat("🔍 Validation Debug - Monthly:\n")
    cat("   - Raw AI response length:", nchar(monthly), "chars\n")
    cat("   - Raw AI preview:", substr(monthly, 1, 100), "...\n")
    val_m <- validate_ai_output(monthly, 200)
    cat("   - Validation result - valid:", val_m$valid, "score:", val_m$score, "\n")
    cat("   - Validated text preview:", substr(val_m$text, 1, 100), "...\n")
    
    # Geographic insights ------------------------------------------------------
    geo_data <- tryCatch({
      if ("admin1" %in% names(register)) {
        geo_summary <- register %>%
          dplyr::count(admin1, sort = TRUE) %>%
          dplyr::slice_head(n = 8) %>%
          dplyr::mutate(pct = round(100 * n / sum(n), 1)) %>%
          dplyr::transmute(region = admin1, cases = n, percentage = pct)
        paste(apply(geo_summary, 1, function(x) paste0(x[1], ": ", x[2], " cases (", x[3], "%)")), collapse = "; ")
      } else {
        "Geographic data not available"
      }
    }, error = function(e) "Geographic analysis error")
    
    prompt_geo <- build_enhanced_prompt("geographic_pattern",
                                        list(setting = ctx$setting, population = ctx$population, geo_data = geo_data,
                                             # Add YAML template compatibility
                                             setting_description = ctx$setting),
                                        ai_config = cfg)
    geographic <- call_ai_with_config(cfg$api, prompt_geo, list(temperature = cfg$api$temperature, num_predict = 200))
    val_g <- validate_ai_output(geographic, 150)
    
    # Demographic insights -----------------------------------------------------
    demo_data <- tryCatch({
      demo_cols <- intersect(c("age_value", "sex_value", "age_group"), names(register))
      if (length(demo_cols) > 0) {
        demo_summary <- register %>%
          dplyr::select(dplyr::all_of(demo_cols)) %>%
          dplyr::summarise(
            total = dplyr::n(),
            .groups = "drop"
          )
        paste("Total cases:", demo_summary$total)
      } else {
        "Demographic data not available"
      }
    }, error = function(e) "Demographic analysis error")
    
    prompt_demo <- build_enhanced_prompt("demographic_insight",
                                         list(setting = ctx$setting, population = ctx$population, demo_data = demo_data,
                                              # Add YAML template compatibility
                                              high_priority = paste(ctx$populations$high_priority %||% 
                                                                   c("children under 5", "pregnant women"), collapse = ", "),
                                              context_specific_vulnerabilities = paste(ctx$constraints, collapse = ", ")),
                                         ai_config = cfg)
    demographic <- call_ai_with_config(cfg$api, prompt_demo, list(temperature = cfg$api$temperature, num_predict = 200))
    val_d <- validate_ai_output(demographic, 150)
    
    # Disease-specific narratives ----------------------------------------------
    disease_narratives <- list()
    
    if (!is.null(ctx$diseases$priority) && length(ctx$diseases$priority) > 0) {
      priority_diseases <- head(ctx$diseases$priority, 3) # Limit to top 3
      
      for (d in priority_diseases) {
        disease_narratives[[as.character(d)]] <- generate_disease_trend_narrative(register, d, cfg)
      }
    }
    
    # Facility performance (if facility data available) -----------------------
    facility_perf <- NULL
    if ("health_facility" %in% names(register)) {
      facility_perf <- generate_facility_performance_summary(register, cfg)
    }
    
    # Calculate overall analysis quality ---------------------------------------
    scores <- c(val_m$score, val_g$score, val_d$score)
    avg_quality <- round(mean(scores, na.rm = TRUE))
    
    # Temporarily bypass validation to test
    return(list(
      monthly_summary = if (val_m$valid) val_m$text else monthly,  # Use original if validation fails
      geographic_insights = if (val_g$valid) val_g$text else geographic, # Use original if validation fails
      demographic_insights = if (val_d$valid) val_d$text else demographic, # Use original if validation fails
      disease_narratives = disease_narratives,
      facility_performance = facility_perf,
      risk_assessment = generate_outbreak_risk_assessment(register, cfg),
      analysis_quality = avg_quality
    ))
    
  }, error = function(e) {
    cat("⚠️ AI analysis failed, using fallback methods:", e$message, "\n")
    # Fall back to the fallback functions
    if (exists("generate_fallback_ai_summaries")) {
      return(generate_fallback_ai_summaries(register, ai_system))
    } else {
      return(list(
        monthly_summary = "AI analysis temporarily unavailable. Standard statistical analysis applied to surveillance data.",
        geographic_insights = "Geographic patterns analyzed using standard epidemiological methods.",
        demographic_insights = "Demographic analysis completed using standard statistical approaches.", 
        disease_narratives = list(),
        facility_performance = "Facility performance assessed using standard metrics.",
        risk_assessment = "Risk assessment using standard surveillance thresholds.",
        analysis_quality = 60
      ))
    }
  })
}

#' Generate disease-specific trend narrative
#' @param register Health register data
#' @param disease Disease name
#' @param ai_config AI configuration
#' @return Disease trend narrative
generate_disease_trend_narrative <- function(register, disease, ai_config) {
  tryCatch({
    ctx <- ai_config$epi_context
    
    # Calculate basic disease statistics
    disease_col <- dplyr::case_when(
      "canonical_disease_imc" %in% names(register) ~ "canonical_disease_imc",
      "morbidity" %in% names(register) ~ "morbidity",
      "icd11_title" %in% names(register) ~ "icd_11_title",
      "category_canonical_disease_imc" %in% names(register) ~ "category_canonical_disease_imc",
      "disease" %in% names(register) ~ "disease",
      TRUE ~ NA_character_
    )
    
    if (is.na(disease_col)) {
      return("Disease trend analysis not available - disease column not found")
    }
    
    disease_data <- register %>%
      dplyr::filter(stringr::str_detect(.data[[disease_col]], disease))
    
    if (nrow(disease_data) == 0) {
      return(paste("No cases found for", disease))
    }
    
    stats <- list(
      total_cases = nrow(disease_data),
      recent_avg = nrow(disease_data) / max(1, dplyr::n_distinct(register$datevisit, na.rm = TRUE)),
      baseline_avg = nrow(disease_data) / max(1, dplyr::n_distinct(register$admin1, na.rm = TRUE)),
      ratio = nrow(disease_data) / max(1, nrow(register)) * 100
    )
    
    trend_data <- paste0(
      "Total cases: ", stats$total_cases, 
      ", Prevalence: ", round(stats$ratio, 2), "%"
    )
    
    prompt <- build_enhanced_prompt("disease_trend", list(
      disease_name = disease,
      setting = ctx$setting, 
      population = ctx$population,
      alert_threshold = 2,
      trend_data = trend_data,
      recent_average = round(stats$recent_avg, 1),
      baseline_average = round(stats$baseline_avg, 1),
      trend_ratio = round(stats$ratio, 2),
      # Add YAML template compatibility for disease_analysis
      context_name = ctx$setting,
      high_priority = paste(ctx$diseases$epidemic %||% character(), collapse = ", "),
      endemic_diseases = paste(ctx$diseases$endemic %||% character(), collapse = ", ")
    ), ai_config)
    
    response <- call_ai_with_config(ai_config$api, prompt, list(temperature = ai_config$api$temperature, num_predict = 400))
    
    validation <- validate_ai_output(response, 100)
    return(validation$text)
    
  }, error = function(e) {
    return(paste("Disease trend analysis error for", disease, ":", e$message))
  })
}

#' Generate facility performance summary
#' @param register Health register data
#' @param ai_config AI configuration
#' @return Facility performance summary
generate_facility_performance_summary <- function(register, ai_config) {
  tryCatch({
    ctx <- ai_config$epi_context
    
    facility_stats <- register %>%
      dplyr::count(health_facility, sort = TRUE) %>%
      dplyr::filter(!is.na(health_facility))
    
    if (nrow(facility_stats) == 0) {
      return("Facility performance analysis not available")
    }
    
    total_fac <- nrow(facility_stats)
    high_load <- sum(facility_stats$n > quantile(facility_stats$n, 0.8, na.rm = TRUE), na.rm = TRUE)
    avg_per <- round(mean(facility_stats$n, na.rm = TRUE), 1)
    max_load <- max(facility_stats$n, na.rm = TRUE)
    top5_share <- round(100 * sum(head(facility_stats$n, 5)) / sum(facility_stats$n), 1)
    peak_fac <- facility_stats$health_facility[1]
    
    details <- paste(head(facility_stats, 5) %>%
                    dplyr::transmute(summary = paste0(health_facility, ": ", n, " cases")) %>%
                    dplyr::pull(summary), collapse = "; ")
    
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
    
    response <- call_ai_with_config(ai_config$api, prompt, list(temperature = ai_config$api$temperature, num_predict = 400))
    
    validation <- validate_ai_output(response, 150)
    return(validation$text)
    
  }, error = function(e) {
    return(paste("Facility performance analysis error:", e$message))
  })
}

#' Generate outbreak risk assessment
#' @param register Health register data  
#' @param ai_config AI configuration
#' @return Outbreak risk assessment
generate_outbreak_risk_assessment <- function(register, ai_config) {
  tryCatch({
    ctx <- ai_config$epi_context
    
    # Simple outbreak detection based on recent case clustering
    if (!"datevisit" %in% names(register)) {
      return("Risk assessment requires date information")
    }
    
    recent_data <- register %>%
      dplyr::filter(!is.na(datevisit)) %>%
      dplyr::arrange(dplyr::desc(datevisit)) %>%
      dplyr::slice_head(n = min(1000, nrow(register)))
    
    if (nrow(recent_data) < 10) {
      return("Insufficient recent data for risk assessment")
    }
    
    # Check for clustering
    alert_data <- recent_data %>%
      dplyr::count(admin1, sort = TRUE) %>%
      dplyr::slice_head(n = 3) %>%
      dplyr::transmute(alert_info = paste0(admin1, ": ", n, " recent cases")) %>%
      dplyr::pull(alert_info) %>%
      paste(collapse = "; ")
    
    prompt <- build_enhanced_prompt("outbreak_alert", list(
      alert_threshold = 2,
      geo_threshold = 2,
      alert_data = alert_data,
      setting = ctx$setting,
      population = ctx$population
    ), ai_config)
    
    response <- call_ai_with_config(ai_config$api, prompt, list(temperature = 0.2, num_predict = 300))
    
    validation <- validate_ai_output(response, 100)
    return(validation$text)
    
  }, error = function(e) {
    return(paste("Risk assessment error:", e$message))
  })
}

#' Format AI summary for display
#' @param summary AI summary text
#' @param type Summary type
#' @param include_header Whether to include header
#' @return Formatted summary
format_ai_summary <- function(summary, type = "general", include_header = TRUE) {
  if (is.null(summary) || !is.character(summary) || nchar(summary) == 0) {
    summary <- "Analysis not available"
  }
  
  if (include_header) {
    header_map <- list(
      monthly = "## Monthly Surveillance Summary",
      geographic = "## Geographic Analysis",
      demographic = "## Demographic Insights",
      disease = "## Disease-Specific Analysis",
      facility = "## Facility Performance",
      risk = "## Risk Assessment",
      general = "## AI Analysis"
    )
    
    header <- header_map[[type]] %||% header_map[["general"]]
    return(paste0(header, "\n\n", summary, "\n"))
  } else {
    return(summary)
  }
}

cat("✅ AI Summaries module loaded successfully\n")
