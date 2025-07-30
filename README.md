# Epi Dashboard Test with DHIS2

This project includes:

- A Shiny app (`app/`) to visualize and explore consultation data from health facilities
- An R Markdown report (`reports/`) for static reporting
- A shared `data_loader.R` function to use either live API data or cached local `.rds` files
- Secure configuration using `config/config.yml`

## Usage

- Set `use_api` in `config/config.yml` to `true` or `false`
- Run the Shiny app via `app/app.R`
- Knit the R Markdown report via `reports/epi_report.Rmd`
