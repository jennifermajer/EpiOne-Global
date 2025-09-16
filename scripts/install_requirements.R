# scripts/install_requirements.R — one-time dependency bootstrap

message("== EpiOne: installing required R packages if missing ==")

# Prefer renv restore first if available
use_renv <- FALSE
if (requireNamespace("renv", quietly = TRUE) && file.exists("renv.lock")) {
  use_renv <- TRUE
  message("• renv detected — running renv::restore() (this may take a while)...")
  tryCatch({ renv::restore(prompt = FALSE) }, error = function(e) {
    message("  ! renv restore failed: ", e$message)
  })
}

# Read requirements.txt and install any packages still missing
req_file <- file.path(getwd(), "requirements.txt")
if (!file.exists(req_file)) {
  stop("requirements.txt not found at ", req_file)
}
pkgs <- scan(req_file, what = character(), quiet = TRUE, sep = "\n")
pkgs <- pkgs[nzchar(trimws(pkgs))]

if (length(pkgs)) {
  missing <- pkgs[!sapply(pkgs, function(p) requireNamespace(p, quietly = TRUE))]
  if (length(missing)) {
    message("• Installing ", length(missing), " missing CRAN packages: ", paste(missing, collapse = ", "))
    repos <- getOption("repos"); if (is.null(repos) || is.na(repos) || !nzchar(repos["CRAN"])) {
      options(repos = c(CRAN = "https://cloud.r-project.org"))
    }
    install.packages(missing)
  } else {
    message("• All packages listed in requirements.txt are already installed")
  }
}

# Helpful system checks (non-fatal)
if (requireNamespace("rmarkdown", quietly = TRUE)) {
  ok <- rmarkdown::pandoc_available()
  if (!ok) {
    message("! Pandoc not found. Install Pandoc or Quarto, or set RSTUDIO_PANDOC. See ?rmarkdown::pandoc_available")
  }
}

message("== Done. You can now run the dashboard or render the report. ==")

