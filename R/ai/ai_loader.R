# ai_loader.R - Single entry point for all AI functionality
# Loads all AI modules in correct order and provides main API
# VERSION: 1.0.0

# =============================================================================
# LOAD ALL AI MODULES IN CORRECT ORDER
# =============================================================================

# Load required packages
if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required for AI module loading")
}

# Load AI modules in dependency order
source(here::here("R", "ai", "ai_core.R"))
source(here::here("R", "ai", "ai_prompts.R"))
source(here::here("R", "ai", "ai_summaries.R"))
source(here::here("R", "ai", "ai_fallbacks.R"))

# =============================================================================
# MAIN API FUNCTIONS FOR REPORTS AND APPS
# =============================================================================

#' Initialize AI system for epidemiological reporting
#' @param config Main configuration list
#' @param params Report parameters
#' @param register Optional health register data for context
#' @return Initialized AI system with all capabilities
initialize_ai_for_report <- function(config, params, register = NULL) {
  tryCatch({
    # Initialize core AI system
    ai_system <- safe_initialize_ai_with_data(config, params, register)
    
    # Check if AI is available and working
    if (!isTRUE(ai_system$available)) {
      cat("ℹ️ AI system not available - using fallback functions\n")
      
      # Override with fallback functions
      assign("generate_all_ai_summaries", generate_fallback_ai_summaries, envir = .GlobalEnv)
      assign("format_ai_summary", format_fallback_summary, envir = .GlobalEnv)
      assign("generate_basic_epidemic_alerts", generate_basic_epidemic_alerts, envir = .GlobalEnv)
      
      ai_system$fallback_mode <- TRUE
    } else {
      cat("✅ AI system initialized successfully\n")
      
      # Ensure main functions are available globally for compatibility
      assign("generate_all_ai_summaries", generate_all_ai_summaries, envir = .GlobalEnv)
      assign("format_ai_summary", format_ai_summary, envir = .GlobalEnv)
      assign("generate_basic_epidemic_alerts", generate_basic_epidemic_alerts, envir = .GlobalEnv)
      
      ai_system$fallback_mode <- FALSE
    }
    
    return(ai_system)
    
  }, error = function(e) {
    cat("❌ AI initialization error:", e$message, "\n")
    
    # Force fallback mode
    assign("generate_all_ai_summaries", generate_fallback_ai_summaries, envir = .GlobalEnv)
    assign("format_ai_summary", format_fallback_summary, envir = .GlobalEnv) 
    assign("generate_basic_epidemic_alerts", generate_basic_epidemic_alerts, envir = .GlobalEnv)
    
    return(list(
      available = FALSE,
      enhanced = FALSE,
      config = NULL,
      fallback_mode = TRUE,
      error_message = e$message
    ))
  })
}

#' Initialize AI for Shiny applications (EpiBot compatibility)
#' @param config Configuration list
#' @param enable_ai Whether AI features should be enabled
#' @return AI system for Shiny app
initialize_ai_for_shiny <- function(config, enable_ai = TRUE) {
  if (!enable_ai) {
    return(list(
      available = FALSE,
      enhanced = FALSE,
      config = NULL,
      fallback_mode = TRUE
    ))
  }
  
  # For Shiny apps, we use the same initialization but without register data initially
  ai_system <- initialize_ai_for_report(config, list(enable_ai = enable_ai), register = NULL)
  
  # Store in global environment for EpiBot module compatibility
  assign("ai_system", ai_system, envir = .GlobalEnv)
  
  return(ai_system)
}

#' Load AI system with backward compatibility
#' @param ... Arguments passed to configure_enhanced_ai for backward compatibility
#' @return AI system configuration
load_ai_system_compat <- function(...) {
  # This function maintains backward compatibility with existing calls
  args <- list(...)
  
  if (length(args) >= 2) {
    config <- args[[1]]
    params <- args[[2]]
    register <- if (length(args) >= 3) args[[3]] else NULL
    
    return(initialize_ai_for_report(config, params, register))
  } else {
    # Fallback for incomplete arguments
    return(list(
      available = FALSE,
      enhanced = FALSE,
      config = NULL,
      fallback_mode = TRUE,
      error_message = "Insufficient arguments for AI initialization"
    ))
  }
}

# =============================================================================
# COMPATIBILITY ALIASES
# =============================================================================

# Maintain backward compatibility with existing function names
configure_enhanced_ai_compat <- function(...) {
  load_ai_system_compat(...)
}

safe_initialize_ai_with_data_compat <- function(...) {
  load_ai_system_compat(...)
}

cat("✅ AI Loader module loaded successfully\n")
cat("📚 Available AI functions:\n")
cat("   - initialize_ai_for_report(config, params, register)\n")
cat("   - initialize_ai_for_shiny(config, enable_ai)\n")
cat("   - generate_all_ai_summaries(register, ai_system)\n")
cat("   - format_ai_summary(summary, type, include_header)\n")
cat("   - generate_basic_epidemic_alerts(register)\n")
cat("   - Backward compatibility functions available\n")