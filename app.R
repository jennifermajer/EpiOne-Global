# app.R — layout-aware, crash-safe startup with new modules
suppressPackageStartupMessages({
  library(shiny)
  library(glue)
  library(here)
})

options(repos = c(CRAN = "https://cloud.r-project.org"))

app_env <- environment()
log <- function(...) cat("[startup]", sprintf(...), "\n")

# Source all new modules
module_files <- c(
  "modules/mod_overview.R",
  "modules/mod_summary_cards.R", 
  "modules/mod_epibot.R",
  "R/disease_categories_taxaware.R",
  "modules/mod_disease_surveillance.R",
  "modules/mod_geographic.R",
  "modules/mod_time_trends.R",
  "modules/mod_settings.R",
  "modules/advanced/mod_predictive_analytics.R",
  "modules/advanced/mod_climate_integration.R",
  "modules/advanced/mod_multi_disease_correlation.R"
)

for (f in module_files) {
  if (file.exists(f)) {
    log("Sourcing module: %s", f)
    tryCatch({
      sys.source(f, envir = app_env)
    }, error = function(e) {
      message("Error in module ", f, ": ", conditionMessage(e))
      stop(e)
    })
  } else {
    log("(skip) module not found: %s", f)
  }
}

# 2) Detect where the app files live (root vs app/)
root_set <- all(file.exists(c("global.R","ui.R","server.R")))
app_set  <- all(file.exists(c("app/global.R","app/ui.R","app/server.R")))

base <- if (root_set) "." else if (app_set) "app" else NA_character_
if (is.na(base)) {
  stop("Could not find global.R/ui.R/server.R in root or app/ folder.")
}
log("Using base: %s", base)

# 3) Source global/ui/server with error trapping
safe_source <- function(path) {
  log("Sourcing: %s", path)
  tryCatch({
    sys.source(path, envir = app_env)
  }, error = function(e) {
    message("❌ Error while sourcing ", path, ": ", conditionMessage(e))
    stop(e)
  })
}

safe_source(file.path(base, "global.R"))
safe_source(file.path(base, "ui.R"))
safe_source(file.path(base, "server.R"))

# Alias if files used different object names
if (!exists("ui", envir = app_env) && exists("app_ui", envir = app_env))       assign("ui", get("app_ui", envir = app_env), envir = app_env)
if (!exists("server", envir = app_env) && exists("app_server", envir = app_env)) assign("server", get("app_server", envir = app_env), envir = app_env)

# 4) Make browseURL safe on server
if (!interactive()) {
  if (!exists("browseURL")) {
    browseURL <- function(...) invisible(NULL)
  }
}

if (!exists("ui", envir = app_env))     stop("`ui` not defined after sourcing ui.R")
if (!exists("server", envir = app_env)) stop("`server` not defined after sourcing server.R")

log("Startup complete — launching app")
shinyApp(ui = get("ui", envir = app_env), server = get("server", envir = app_env))
