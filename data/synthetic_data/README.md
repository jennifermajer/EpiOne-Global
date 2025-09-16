# Synthetic Data for IMC Epidemiological Dashboard

This directory contains synthetic health surveillance data for demonstration and testing purposes.

## Files

- `synthetic_register.csv` - Main synthetic health consultation register (1000 records)

## Data Structure

The synthetic data mimics the structure of real health surveillance data used by International Medical Corps (IMC) health programs. It includes:

### Core Fields
- **Demographics**: age, sex, geographic location (admin levels)
- **Temporal**: consultation dates spanning 12 months
- **Clinical**: disease/morbidity data with realistic epidemiological patterns
- **Organizational**: health facility identifiers

### Taxonomy & Classification
- **Canonical Diseases**: Mapped to global base canonical disease taxonomy
- **Disease Categories**: Respiratory, enteric, vector-borne, vaccine-preventable, etc.
- **ICD-11 Integration**: Mapped to WHO ICD-11 classification system
- **Epidemiological Flags**:
  - Vaccine-preventable diseases (VPD)
  - Climate-sensitive conditions
  - Outbreak-prone diseases
  - Trauma-related cases
  - AMR-relevant infections

### Quality Indicators
- **Completeness**: All required fields populated
- **Consistency**: Realistic age/disease associations
- **Temporal Patterns**: Seasonal disease trends
- **Geographic Distribution**: Multi-level administrative boundaries

## Generation

The synthetic data is generated using `R/synthetic_data.R` which:
1. Creates realistic demographic and temporal distributions
2. Applies epidemiological disease patterns
3. Integrates with the taxonomy mapping system
4. Ensures compatibility with all dashboard features

## Privacy & Ethics

This synthetic data:
- Contains **NO real patient information**
- Uses **artificially generated** consultation records
- Maintains **realistic epidemiological patterns** for demo purposes
- Enables **safe sharing** and **public repository** hosting

## Usage

To generate the report with synthetic data:

```r
# Generate synthetic data (if needed)
source("R/synthetic_data.R")
synthetic_data <- generate_synthetic_register(n = 1000, country = "synthetic")

# Render report
rmarkdown::render(
  "reports/epi_report.Rmd",
  params = list(country_code = "synthetic"),
  output_file = "synthetic_report.html"
)
```

## Data Quality

- **1000 consultation records** across 12 months
- **50 health facilities** in multi-level geographic hierarchy
- **25+ disease categories** with realistic prevalence patterns
- **Complete taxonomy mapping** with ICD-11 integration
- **Epidemiological flags** applied consistently

This synthetic dataset demonstrates the full capabilities of the epidemiological surveillance dashboard while ensuring complete data privacy and enabling open-source collaboration.