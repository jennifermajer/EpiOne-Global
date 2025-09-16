
# deploy.R — Secure, consistent deployment for the Shiny app
# Usage (locally): source("deploy.R")
suppressPackageStartupMessages({
  library(rsconnect)
})

cat("🚀 IMC Epi Dashboard — Deployment\n")
cat("====================================\n\n")

# -------------------------------
# 1) Read credentials from env
# -------------------------------
acct  <- Sys.getenv("SHINYAPPS_NAME",  unset = NA_character_)
token <- Sys.getenv("SHINYAPPS_TOKEN", unset = NA_character_)
secret<- Sys.getenv("SHINYAPPS_SECRET",unset = NA_character_)

if (any(is.na(c(acct, token, secret)))) {
  stop("Missing shinyapps.io credentials. Please set SHINYAPPS_NAME, SHINYAPPS_TOKEN, SHINYAPPS_SECRET in your .Renviron or environment.")
}

# Configure rsconnect account (no hard-coded secrets in the repo)
rsconnect::setAccountInfo(name = acct, token = token, secret = secret)

# -------------------------------
# 2) App metadata
# -------------------------------
app_title <- Sys.getenv("APP_TITLE", "IMC Syria Epidemiological Dashboard")
app_name  <- Sys.getenv("APP_NAME",  "imc-syria-epi-dashboard")

# -------------------------------
# 3) Validate required files
# -------------------------------
required_files <- c(
  "app.R",
  "app/ui.R",
  "app/server.R",
  "app/global.R"
)

optional_modules <- c(
  "modules/ai_config.R",
  "modules/mod_overview.R",
  "modules/mod_settings.R",
  "modules/mod_epibot.R",
  "modules/mod_disease_surveillance.R",
  "modules/mod_geographic.R",
  "modules/mod_summary_cards.R",
  "modules/mod_time_trends.R",
  "modules/advanced_mod_predictive_analytics.R",
  "modules/advanced_mod_climate_integration.R",
  "modules/advanced_mod_multi_disease_correlation.R"
)

missing <- required_files[!file.exists(required_files)]
if (length(missing) > 0) {
  stop("Missing required file(s): ", paste(missing, collapse = ", "))
}

# -------------------------------
# 4) Build the file list to upload
# -------------------------------
app_files <- c(required_files, optional_modules[file.exists(optional_modules)])

# Also include config directory if present (for DHIS2/API settings)
if (dir.exists("config")) {
  # include all non-secret text files under config/
  cfg <- list.files("config", recursive = TRUE, full.names = TRUE)
  app_files <- c(app_files, cfg)
}

# -------------------------------
# 5) Deploy
# -------------------------------
cat("Files to deploy (", length(app_files), "):\n", paste0(" - ", app_files, collapse="\n"), "\n\n")

rsconnect::deployApp(
  appDir   = ".",
  appFiles = app_files,
  appName  = app_name,
  appTitle = app_title,
  account  = acct,
  logLevel = "verbose",
  forceUpdate = TRUE
)

cat("\n✅ Deployment initiated. Check your shinyapps.io dashboard for status.\n")