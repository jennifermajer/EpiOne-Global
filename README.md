markdown# **Epi Dashboard Test with DHIS2**

A comprehensive health surveillance dashboard and reporting system for analyzing consultation data from health facilities using DHIS2 data.

## **Features**

- **Interactive Shiny Dashboard** (`app/`) - Real-time visualization and exploration of health facility data
- **Automated R Markdown Reports** (`reports/`) - Professional epidemiological reports with key insights
- **Flexible Data Loading** - Support for both live DHIS2 API data and cached local files
- **Secure Configuration** - Centralized configuration management with environment-specific settings
- **Comprehensive Analysis** - Disease surveillance, geographic analysis, demographic patterns, and outbreak detection

## **Project Structure**
epidashboard/
├── config/
│   └── config.yml              # Configuration settings
├── R/
│   ├── data_loader.R           # Data loading and caching functions
│   ├── utils_dhis2_api.R       # DHIS2 API utilities
│   ├── preprocessing.R         # Data cleaning and transformation
│   └── visualization_helpers.R # Plotting and summary functions
├── app/
│   └── app.R                   # Shiny application
├── reports/
│   └── epi_report.Rmd         # R Markdown report template
├── data/
│   ├── raw/                   # Cached API responses
│   ├── processed/             # Cleaned datasets
│   └── metadata/              # Organizational metadata
└── output/
├── reports/               # Generated reports
└── plots/                 # Saved visualizations

## **Prerequisites**

### Required Software
1. **R** (>= 4.0.0) - [Download R](https://cran.r-project.org/)
2. **RStudio** (recommended) - [Download RStudio](https://www.rstudio.com/products/rstudio/download/)
3. **Pandoc** (>= 1.12.3) - Required for R Markdown reports

### Installing Pandoc

**Option 1: Install/Update RStudio (Recommended)**
- RStudio includes Pandoc automatically
- Download latest version from [RStudio website](https://www.rstudio.com/products/rstudio/download/)

**Option 2: Install Pandoc Separately**

*macOS:*
```bash
brew install pandoc
Windows:
bashchoco install pandoc
Linux:
bashsudo apt-get install pandoc  # Ubuntu/Debian
sudo yum install pandoc      # CentOS/RHEL
Verify Installation:
r# In R console
rmarkdown::pandoc_available()  # Should return TRUE
rmarkdown::pandoc_version()    # Should be >= 1.12.3
Required R Packages
r# Install required packages
install.packages(c(
  "shiny", "shinydashboard", "DT", "plotly",
  "config", "httr", "jsonlite", "logger",
  "dplyr", "tidyr", "ggplot2", "scales",
  "readxl", "lubridate", "zoo",
  "gtsummary", "flextable", "viridis",
  "RColorBrewer", "treemapify", "here"
))
Setup Instructions
1. Clone Repository
bashgit clone <repository-url>
cd epidashboard
2. Configure Settings
Edit config/config.yml:
yamldefault:
  dhis2:
    base_url: "https://your-dhis2-instance.org/api/"
    username: "your_username"
    password: "your_password"  # Consider using environment variables
  
  cache:
    use_cache: true           # Set to false for always-fresh data
    cache_expiry_hours: 24    # How long to keep cached data
  
  paths:
    metadata_file: "data/metadata/Org Unit Metadata.xlsx"
3. Add Metadata Files
Place your organizational unit metadata Excel file in:
data/metadata/Org Unit Metadata.xlsx
Usage
Running the Shiny Dashboard
r# Launch interactive dashboard
shiny::runApp("app/app.R")
Generating Reports
r# Generate epidemiological report
rmarkdown::render("reports/epi_report.Rmd")

# With custom parameters
rmarkdown::render(
  "reports/epi_report.Rmd",
  params = list(
    config_env = "production",
    force_refresh = TRUE,
    target_year = 2024
  )
)
Data Loading Options
Option 1: Live API Data
yaml# In config.yml
cache:
  use_cache: false
Option 2: Cached Data
yaml# In config.yml  
cache:
  use_cache: true
  cache_expiry_hours: 24
Key Functions
Data Loading

load_dhis2_data() - Main data loading function with caching
fetch_dhis2_data() - Direct API calls to DHIS2
prepare_register() - Clean and process raw data

Analysis

create_disease_categories() - Categorize morbidity conditions
create_indicator_variables() - Generate surveillance indicators
get_summary_statistics() - Generate report summaries

Visualization

plot_consultations_over_time() - Temporal trend analysis
plot_disease_distribution() - Disease burden visualization
plot_geographic_distribution() - Geographic analysis

Configuration Options
Environment Variables
For production, consider using environment variables:
r# Set environment variables
Sys.setenv(DHIS2_USERNAME = "your_username")
Sys.setenv(DHIS2_PASSWORD = "your_secure_password")
Cache Management
r# Force refresh data
load_dhis2_data(force_refresh = TRUE)

# Check cache status
file.info("data/raw/dhis2_data.rds")

Contact
[Jennifer Majer: jlmajer@internationalmedicalcorps.org]