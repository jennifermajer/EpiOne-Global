# app/global.R — Consolidated, robust startup for the Epidemiological Dashboard
# VERSION: 2.2.1 - FIXED safe_require function
# - Safe, explicit package loading
# - Single source of truth for config + paths
# - Early helpers (%||%, logging, theme)
# - NEW: Modular sourcing with mod_ pattern
# - Optional heavy packages handled lazily
# - AI fallback flags read from config

# ============================================================================
# CRAN mirror + general options
# ============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
options(
  scipen = 999,
  shiny.sanitize.errors = TRUE,
  shiny.maxRequestSize = 100 * 1024^2 # 100MB
)

# Optional: auto-install missing packages on startup (guarded by config/env)
auto_install_requirements_if_needed <- function() {
  # Gate by env or config flag
  allow <- FALSE
  env_flag <- tolower(Sys.getenv("AUTO_INSTALL_DEPS", "false")) %in% c("1","true","yes")
  if (env_flag) allow <- TRUE
  # If config already loaded later, we won't have it yet — try reading minimal YAML directly
  if (!allow) {
    cfg_path <- NULL
    cand <- c(here::here("config","config.yml"), "config/config.yml", here::here("config.yml"), "config.yml")
    for (p in cand) { if (file.exists(p)) { cfg_path <- p; break } }
    if (!is.null(cfg_path)) {
      try({
        yc <- yaml::read_yaml(cfg_path)
        def <- yc[[Sys.getenv("R_CONFIG_ACTIVE", "default")]] %||% yc[["default"]]
        allow <- isTRUE(def$setup$auto_install_deps)
      }, silent = TRUE)
    }
  }
  if (!allow) return(invisible(FALSE))
  # Detect a small set of critical packages
  critical <- c("shiny","dplyr","ggplot2","rmarkdown")
  missing <- critical[!sapply(critical, function(p) requireNamespace(p, quietly = TRUE))]
  if (length(missing)) {
    inst <- file.path(getwd(), "scripts", "install_requirements.R")
    if (file.exists(inst)) {
      message("[startup] Auto-installing requirements (guarded) ...")
      try(source(inst), silent = FALSE)
    }
  }
  invisible(TRUE)
}

try(auto_install_requirements_if_needed(), silent = TRUE)

# ============================================================================
# Helper operators & tiny utils (must be available to all modules)
# ============================================================================
if (!exists("%||%")) `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
nzchar1 <- function(x) is.character(x) && length(x) == 1 && nzchar(x)

# FIXED: Robust safe_require function with better error handling
safe_require <- function(pkg) {
  # Validate input
  if (is.null(pkg) || !is.character(pkg) || length(pkg) != 1 || is.na(pkg) || nzchar(pkg) == FALSE) {
    message(sprintf("Invalid package name provided to safe_require: %s", deparse(substitute(pkg))))
    return(FALSE)
  }
  
  # Trim whitespace and validate again
  pkg <- trimws(pkg)
  if (nzchar(pkg) == FALSE) {
    message("Empty package name provided to safe_require")
    return(FALSE)
  }
  
  # Try to require the package
  tryCatch({
    result <- requireNamespace(pkg, quietly = TRUE)
    if (!result) {
      message(sprintf("NOTE: package '%s' not available; features may be limited.", pkg))
    }
    return(result)
  }, error = function(e) {
    message(sprintf("ERROR loading package '%s': %s", pkg, e$message))
    return(FALSE)
  })
}

# Lightweight log wrappers that degrade gracefully
.log_level <- function() {
  if (safe_require("logger")) getOption("app.log.level", "INFO") else "INFO"
}

# FIXED: More robust logging functions
log_info <- function(...) {
  tryCatch({
    if (.log_level() %in% c("INFO","DEBUG")) {
      if (safe_require("logger")) {
        logger::log_info(...)
      } else {
        cat("[INFO]", ..., "\n")
      }
    }
  }, error = function(e) {
    cat("[INFO]", ..., "\n")  # Fallback to simple cat
  })
}

log_warn <- function(...) {
  tryCatch({
    if (.log_level() %in% c("WARN","INFO","DEBUG")) {
      if (safe_require("logger")) {
        logger::log_warn(...)
      } else {
        cat("[WARN]", ..., "\n")
      }
    }
  }, error = function(e) {
    cat("[WARN]", ..., "\n")  # Fallback to simple cat
  })
}

log_error <- function(...) {
  tryCatch({
    if (safe_require("logger")) {
      logger::log_error(...)
    } else {
      cat("[ERROR]", ..., "\n")
    }
  }, error = function(e) {
    cat("[ERROR]", ..., "\n")  # Fallback to simple cat
  })
}

# ============================================================================
# Runtime package sets (lean core; heavy libs optional)
# ============================================================================
.runtime_pkgs <- c(
  # Core Shiny
  "shiny","shinydashboard","shinydashboardPlus","shinyjs","shinyWidgets",
  # Viz
  "ggplot2","plotly","leaflet","DT","viridis","RColorBrewer","ggforce","patchwork","treemapify",
  # Data
  "dplyr","tidyr","tibble","stringr","forcats","lubridate","readr","readxl","zoo","scales",
  # Geospatial
  "sf",
  # Config / IO / utils
  "here", "yaml", "logger", "httr", "jsonlite", "digest", "markdown", "fs"
)

.optional_pkgs <- c(
  # Output & deployment
  "flextable","rmarkdown","rsconnect","openxlsx",
  # Modeling / extras
  "forecast","prophet","dbscan","nasapower","NeuralNetTools","randomForest"
)

# FIXED: Better package loading with validation
cat("[INFO] Loading core runtime packages...\n")
failed_packages <- character(0)

for (p in .runtime_pkgs) {
  if (is.null(p) || !is.character(p) || length(p) != 1 || is.na(p) || !nzchar(p)) {
    cat("[WARN] Invalid package in runtime list:", deparse(p), "\n")
    next
  }
  
  ok <- safe_require(p)
  if (!ok) {
    failed_packages <- c(failed_packages, p)
    cat("[ERROR] Required package failed to load:", p, "\n")
  }
}

# Stop if critical packages failed
critical_packages <- c("shiny", "dplyr", "here", "config")
critical_failed <- intersect(failed_packages, critical_packages)
if (length(critical_failed) > 0) {
  stop(sprintf("Critical packages failed to load: %s. Please install them and restart.", 
               paste(critical_failed, collapse = ", ")))
}

if (length(failed_packages) > 0) {
  cat("[WARN] Some non-critical packages failed to load:", paste(failed_packages, collapse = ", "), "\n")
  cat("[WARN] Some features may be limited.\n")
}

# Optional packages are lazy-required inside features that need them

# Use a non-glue formatter everywhere to avoid `{var}` evaluation errors
tryCatch({
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_formatter(logger::formatter_paste)  # <- no parentheses
  }
}, error = function(e) {
  cat("[WARN] Could not set logger formatter:", e$message, "\n")
})

# ============================================================================
# Load configuration (config/config.yml) and expose as 'app_cfg'
# ============================================================================
# FIXED: Use yaml package instead of config package to avoid conflicts
tryCatch({
  if (requireNamespace("yaml", quietly = TRUE)) {
    # Load config file using yaml package directly
    config_file <- here::here("config", "config.yml")
    if (file.exists(config_file)) {
      all_configs <- yaml::read_yaml(config_file)
      config_env <- Sys.getenv("R_CONFIG_ACTIVE", "default")
      app_cfg <- all_configs[[config_env]] %||% all_configs[["default"]]
      
      if (is.null(app_cfg)) {
        stop("Configuration environment not found")
      }
      
      assign("app_cfg", app_cfg, envir = .GlobalEnv)
      cat("[INFO] Configuration loaded successfully using yaml package\n")
    } else {
      stop("Configuration file not found")
    }
  } else {
    stop("yaml package not available")
  }
}, error = function(e) {
  cat("[ERROR] Failed to load configuration:", e$message, "\n")
  # Create minimal fallback config
  app_cfg <- list(
    app = list(title = "Epidemiological Dashboard", version = "dev", contact = ""),
    paths = list(data_dir = "data", output_dir = "output"),
    ai_features = list(enabled = FALSE),
    epibot_features = list(enabled = FALSE),
    dhis2 = list()
  )
  assign("app_cfg", app_cfg, envir = .GlobalEnv)
  cat("[WARN] Using fallback configuration\n")
})

# ============================================================================
# Paths & directories
# ============================================================================
paths <- app_cfg$paths %||% list()
data_dir      <- paths$data_dir      %||% "data"
output_dir    <- paths$output_dir    %||% "output"
metadata_file <- paths$metadata_file %||% file.path("data","metadata","Org Unit Metadata.xlsx")
shapefile_dir <- paths$shapefile_dir %||% file.path("data","shapefiles")

# Ensure dirs exist (don't fail if they don't)
for (d in unique(c(data_dir, output_dir, dirname(metadata_file), shapefile_dir))) {
  tryCatch({
    if (!fs::dir_exists(d)) {
      fs::dir_create(d, recurse = TRUE)
    }
  }, error = function(e) {
    cat("[WARN] Could not create directory", d, ":", e$message, "\n")
  })
}

# ============================================================================
# Global ggplot theme & palette
# ============================================================================
theme_report <- function(base_size = 12, base_family = "sans") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(linetype = "dotted"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold")
    )
}
scale_fill_report  <- function(...) ggplot2::scale_fill_viridis_d(...)
scale_color_report <- function(...) ggplot2::scale_color_viridis_d(...)

# Make theme available globally
assign("theme_report", theme_report, envir = .GlobalEnv)
assign("scale_fill_report",  scale_fill_report,  envir = .GlobalEnv)
assign("scale_color_report", scale_color_report, envir = .GlobalEnv)

# ============================================================================
# Shared helpers used across modules
# ============================================================================
detect_date_col <- function(df) {
  if (!is.data.frame(df) || !ncol(df)) return(NULL)
  cands <- c("datevisit","datevisitnew","visit_date","date","eventdate")
  hit <- intersect(cands, names(df))
  if (length(hit)) hit[1] else NULL
}
assign("detect_date_col", detect_date_col, envir = .GlobalEnv)

get_epidemic_diseases <- function() {
  # Single source of truth; used by surveillance + report
  c("Acute Watery Diarrhea","Acute bloody diarrhea","Cholera","Suspected Cholera (Acute Watery diarrhea)",
    "Influenza Like Illness","Acute Upper respiratory infection","URTI","LRTI",
    "Measles","Suspected Measles","Diphtheria","Suspected Diphtheria",
    "Hepatitis A","Hepatitis E","Hepatitis B","Hepatitis C",
    "Scabies/other acariasis","Typhoid","Malaria (Suspected)","COVID-19 suspected case","Pneumonia","Tuberculosis","Suspected Tuberculosis")
}
assign("get_epidemic_diseases", get_epidemic_diseases, envir = .GlobalEnv)

# ============================================================================
# Minimal app meta
# ============================================================================
app_meta <- list(
  title   = app_cfg$app$title   %||% "Epidemiological Dashboard",
  version = app_cfg$app$version %||% "dev",
  contact = app_cfg$app$contact %||% ""
)
assign("app_meta", app_meta, envir = .GlobalEnv)

# ============================================================================
# AI flags (EpiBot + summarization) — do NOT force external providers here
# ============================================================================
ai_flags <- list(
  enabled         = isTRUE(app_cfg$ai_features$enabled) || isTRUE(app_cfg$epibot_features$enabled),
  chat_enabled    = isTRUE(app_cfg$ai_features$chat_assistance) || isTRUE(app_cfg$epibot_features$enabled),
  summary_enabled = isTRUE(app_cfg$ai_features$summary_generation)
)
assign("ai_flags", ai_flags, envir = .GlobalEnv)

# ============================================================================
# UPDATED: Source order for modular structure
# ============================================================================
# Helper function for safe sourcing
source_if_exists <- function(...) {
  f <- here::here(...)
  tryCatch({
    if (fs::file_exists(f)) {
      log_info(sprintf("sourcing: %s", f))
      # IMPORTANT: load into the global environment
      sys.source(f, envir = globalenv())
      TRUE
    } else {
      log_warn(sprintf("missing: %s", f))
      FALSE
    }
  }, error = function(e) {
    log_error(sprintf("Error sourcing %s: %s", f, e$message))
    FALSE
  })
}

# 1. Source utility files first (data loading, preprocessing, etc.)
cat("[INFO] Loading utility files...\n")
found <- FALSE
found <- source_if_exists("R","utils_dhis2_api.R")        || found
found <- source_if_exists("R","data_loader.R")           || found
found <- source_if_exists("R","disease_categories_taxaware.R") || found
found <- source_if_exists("R","disease_categories.R")    || found
found <- source_if_exists("R","summary_functions.R")     || found
found <- source_if_exists("R","preprocessing.R")         || found

# 2. Source helper modules (consolidated in modules/ directory)
found <- source_if_exists("modules","shiny_modules.R")   || found

# 3. NEW: Source all new modular components (mod_ pattern)
# Define the new modules in dependency order
new_modules <- c(
  "mod_summary_cards.R",           # Basic components first
  "mod_overview.R",                # Dashboard overview
  "mod_epibot.R",                 # AI assistant
  "mod_disease_surveillance.R",   # Disease monitoring
  "mod_time_trends.R",            # Time analysis
  "mod_geographic.R",             # Geographic analysis
  "mod_settings.R",               # Configuration
  # NEW: Advanced analytics modules
  "advanced/mod_predictive_analytics.R",      # Predictive modeling
  "advanced/mod_multi_disease_correlation.R", # Disease correlations
  "advanced/mod_climate_integration.R"        # Climate-health integration
)

log_info("Loading new modular components...")
modules_loaded <- 0
advanced_modules_loaded <- 0

for (module_file in new_modules) {
  if (source_if_exists("modules", module_file)) {
    modules_loaded <- modules_loaded + 1
    if (grepl("advanced/", module_file)) {
      advanced_modules_loaded <- advanced_modules_loaded + 1
    }
  }
}

log_info(sprintf("Successfully loaded %d/%d new modules (%d advanced)", 
                modules_loaded, length(new_modules), advanced_modules_loaded))

# Add helper function for calculating forecast accuracy (missing from predictive analytics)
calculate_forecast_accuracy <- function(ts_obj, forecast_result, model_type) {
  
  if (length(ts_obj) < 10) {
    return(list(mae = NA, rmse = NA, mape = NA))
  }
  
  # Use last 20% of data for validation
  n <- length(ts_obj)
  train_size <- floor(0.8 * n)
  
  if (train_size < 5) {
    return(list(mae = NA, rmse = NA, mape = NA))
  }
  
  train_data <- ts_obj[1:train_size]
  test_data <- ts_obj[(train_size + 1):n]
  
  # Generate predictions for test period
  if (model_type == "prophet") {
    # Handle prophet differently
    return(list(mae = 0, rmse = 0, mape = 0))  # Placeholder
  } else {
    # For ARIMA/ETS models
    if (requireNamespace("forecast", quietly = TRUE)) {
      test_forecast <- forecast::forecast(forecast::auto.arima(train_data), h = length(test_data))
      predicted <- as.numeric(test_forecast$mean)
    } else {
      return(list(MAE = NA, RMSE = NA, MAPE = NA))
    }
  }
  
  if (length(predicted) != length(test_data)) {
    return(list(mae = NA, rmse = NA, mape = NA))
  }
  
  # Calculate accuracy metrics
  mae <- mean(abs(test_data - predicted), na.rm = TRUE)
  rmse <- sqrt(mean((test_data - predicted)^2, na.rm = TRUE))
  mape <- mean(abs((test_data - predicted) / test_data), na.rm = TRUE) * 100
  
  list(
    MAE = round(mae, 2),
    RMSE = round(rmse, 2),
    MAPE = round(mape, 2)
  )
}

# Make forecast accuracy function available globally
assign("calculate_forecast_accuracy", calculate_forecast_accuracy, envir = .GlobalEnv)

# Add placeholder for empty plotly plots
plotly_empty <- function(message = "No data available") {
  plotly::plot_ly() %>%
    plotly::add_annotations(
      text = message,
      x = 0.5, y = 0.5,
      xref = "paper", yref = "paper",
      showarrow = FALSE,
      font = list(size = 16, color = "#666")
    ) %>%
    plotly::layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)'
    )
}

assign("plotly_empty", plotly_empty, envir = .GlobalEnv)

# 4. Backward compatibility: Try to load old modules if new ones not found
if (modules_loaded == 0) {
  log_warn("No new modules found, falling back to legacy modules...")
  
  # Try old module names
  legacy_modules <- c(
    "disease_surveillance.R",
    "geographic_analysis.R", 
    "summary_cards.R",
    "time_trends.R",
    "chatbot_assistant.R"
  )
  
  for (legacy_file in legacy_modules) {
    source_if_exists("R", legacy_file) || source_if_exists("modules", legacy_file)
  }
}

# 5. Special handling for chatbot (backward compatibility)
# Check if new EpiBot module loaded, otherwise try legacy
if (!exists("mod_epibot_ui") || !exists("mod_epibot_server")) {
  log_info("New EpiBot module not found, trying legacy chatbot...")
  
  chat_sourced <- source_if_exists("R","chatbot_assistant.R") ||
                 source_if_exists("modules","chatbot_assistant.R")
  
  # Auto-discover pattern if not found
  if (!chat_sourced) {
    tryCatch({
      cand <- c(
        list.files(here::here("modules"), pattern = "chat.*assistant.*\\.R$", full.names = TRUE, ignore.case = TRUE, recursive = TRUE),
        list.files(here::here("R"),       pattern = "chat.*assistant.*\\.R$", full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
      )
      if (length(cand)) {
        log_info(sprintf("auto-discovered chatbot module: %s", cand[1]))
        source(cand[1], local = TRUE)
        chat_sourced <- TRUE
      } else {
        log_warn("chatbot module file not found anywhere under R/ or modules/")
      }
    }, error = function(e) {
      log_warn(sprintf("Error during chatbot auto-discovery: %s", e$message))
    })
  }
  
  # Final check for chatbot functions
  if (!exists("chatbotAssistantUI") && !exists("mod_epibot_ui")) {
    log_warn("No EpiBot/chatbot functions available after sourcing.")
  }
}

# ============================================================================
# Module validation and reporting
# ============================================================================
# INITIALIZE available_modules FIRST
available_modules <- list()

# New modular structure
if (exists("mod_overview_ui") && exists("mod_overview_server")) {
  available_modules$overview <- "✅ New modular"
}
if (exists("mod_summary_cards_ui") && exists("mod_summary_cards_server")) {
  available_modules$summary_cards <- "✅ New modular"
}
if (exists("mod_epibot_ui") && exists("mod_epibot_server")) {
  available_modules$epibot <- "✅ New modular"
} else if (exists("chatbotAssistantUI") && exists("chatbotAssistantServer")) {
  available_modules$epibot <- "⚠️ Legacy"
}
if (exists("mod_disease_surveillance_ui") && exists("mod_disease_surveillance_server")) {
  available_modules$disease_surveillance <- "✅ New modular"
} else if (exists("disease_surveillance_UI") && exists("disease_surveillance_Server")) {
  available_modules$disease_surveillance <- "⚠️ Legacy"
}
if (exists("mod_time_trends_ui") && exists("mod_time_trends_server")) {
  available_modules$time_trends <- "✅ New modular"
} else if (exists("time_trends_UI") && exists("time_trends_Server")) {
  available_modules$time_trends <- "⚠️ Legacy"
}
if (exists("mod_geographic_ui") && exists("mod_geographic_server")) {
  available_modules$geographic <- "✅ New modular"
} else if (exists("geographic_analysis_UI") && exists("geographic_analysis_Server")) {
  available_modules$geographic <- "⚠️ Legacy"
}
if (exists("mod_settings_ui") && exists("mod_settings_server")) {
  available_modules$settings <- "✅ New modular"
}

# Advanced analytics modules
if (exists("predictive_analytics_UI") && exists("predictive_analytics_Server")) {
  available_modules$predictive_analytics <- "✅ New advanced"
}
if (exists("multi_disease_correlation_UI") && exists("multi_disease_correlation_Server")) {
  available_modules$multi_disease_correlation <- "✅ New advanced"
}
if (exists("climate_integration_UI") && exists("climate_integration_Server")) {
  available_modules$climate_integration <- "✅ New advanced"
}

# Report module status
log_info("Module availability report:")
for (module_name in names(available_modules)) {
  log_info(sprintf("  %s: %s", module_name, available_modules[[module_name]]))
}

if (length(available_modules) == 0) {
  log_warn("No modules loaded! Check module files and dependencies.")
} else {
  new_modules_count <- sum(grepl("New modular|New advanced", available_modules))
  legacy_modules_count <- sum(grepl("Legacy", available_modules))
  log_info(sprintf("Module summary: %d new modular, %d legacy", new_modules_count, legacy_modules_count))
}

# ============================================================================
# DHIS2 defaults in a simple list (used by loaders/modules)
# ============================================================================
dhis2_cfg <- app_cfg$dhis2 %||% list()
assign("dhis2_cfg", dhis2_cfg, envir = .GlobalEnv)

# Validate base_url form (should end with /api/ per your config)
if (nzchar1(dhis2_cfg$base_url) && !grepl("/api/?$", dhis2_cfg$base_url)) {
  log_warn(sprintf("dhis2.base_url does not appear to end with '/api/'. Current: %s", dhis2_cfg$base_url))
}

# ============================================================================
# UI constants (icons, image paths, etc.)
# ============================================================================
IMAGES <- list(
  epibot = here::here("app","www","epibot_bmo_1024.png")
)
assign("IMAGES", IMAGES, envir = .GlobalEnv)

# ============================================================================
# Helper function to check if we're using new modular structure
# ============================================================================
is_modular_structure <- function() {
  new_module_functions <- c("mod_overview_ui", "mod_summary_cards_ui", "mod_epibot_ui")
  sum(sapply(new_module_functions, exists)) >= 2  # At least 2 new modules
}

assign("is_modular_structure", is_modular_structure, envir = .GlobalEnv)

# ============================================================================
# Final startup note
# ============================================================================
structure_type <- if (is_modular_structure()) "NEW MODULAR" else "LEGACY"
log_info(sprintf("Global startup complete. App: %s v%s (%s structure)", 
                app_meta$title, app_meta$version, structure_type))
