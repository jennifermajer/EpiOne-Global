# Geographic Module — Overview

Purpose: Geographic distribution and mapping (admin1/admin2, facilities).

Inputs/Outputs
- Input: preprocessed register with `admin1`, `admin2`, `orgunit`, `datevisit`
- Output: choropleths, heatmaps, facility summaries

Key files
- `modules/mod_geographic.R`

Quick usage
- Loaded by the app automatically; no manual calls needed.

Gotchas
- Ensure shapefiles present under `data/shapefiles/<country>`
- Missing geography reduces available visuals but module still loads

