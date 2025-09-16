# Disease Surveillance Module — Overview

Purpose: Morbidity surveillance views, prioritization, and alerts.

Inputs/Outputs
- Input: register with `morbidity`, `canonical_disease_imc`, flags (e.g., `epidemic_prone`)
- Output: trend plots, category breakdowns, basic alerts

Key files
- `modules/mod_disease_surveillance.R`

Quick usage
- Auto‑loaded by the app; uses the current filtered dataset.

Gotchas
- If canonical/category fields are missing, the module falls back with limited views

