# Reports — Epi Report (R Markdown)

Purpose: Generate a comprehensive HTML epidemiological report using the same preprocessing as the dashboard.

Quick render
```r
# Synthetic
rmarkdown::render("reports/epi_report.Rmd", params = list(country_code = "synthetic"))

# Country (if configured): syria | yemen | south_sudan
rmarkdown::render("reports/epi_report.Rmd", params = list(country_code = "syria"))
```

Key params
- `country_code`: `synthetic` or configured country
- `config_env`: config environment (default/development/production)
- `force_refresh`: bypass cache if applicable

Outputs
- HTML report (`html_document`) with figures and mapping

Notes
- Honors taxonomy pipeline; will report ICD‑11 mapping quality if available
- AI sections are optional and skip gracefully if not configured

