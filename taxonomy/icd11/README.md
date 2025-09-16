# ICD-11 Taxonomy System

This directory contains the ICD-11 based disease taxonomy system for the multi-country epidemiological surveillance dashboard.

## Overview

The system uses the WHO ICD-11 (2023-01 release) as the base taxonomy for disease classification. Local disease names from different countries are mapped to standardized ICD-11 codes.

## Files

- `disease_mappings.yml` - Human-readable disease mappings to ICD-11 codes
- `README.md` - This documentation file

## Usage

```r
# Load ICD-11 system
source(here('R', 'taxonomy', 'icd11_taxonomy_mapper.R'))

# Apply to data
mapped_data <- apply_icd11_taxonomy(data, country_config)
```

## API Configuration

To enable automatic ICD-11 API updates:

1. Register at: https://icd.who.int/icdapi
2. Set environment variables:
   - `ICD11_CLIENT_ID`
   - `ICD11_CLIENT_SECRET`

## Data Structure

Each disease mapping includes:

- **local_name**: Original disease name from country data
- **icd11_code**: ICD-11 classification code
- **icd11_title**: Official ICD-11 disease name
- **icd11_category**: ICD-11 chapter/category
- **confidence**: Mapping confidence (high/medium/low)
- **source**: Mapping source (icd11_api/manual)

## Maintenance

- Review diseases with `needs_review: true`
- Update mappings with new local disease names
- Refresh API mappings periodically
- Validate mapping quality using `validate_icd11_mappings()`

