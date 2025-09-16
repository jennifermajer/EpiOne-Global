# Synthetic Data — Quick Guide

Purpose: Generate realistic test data with all fields needed to run the dashboard and report.

Enable
- Config: `config/config.yml` → `testing.use_synthetic_data: yes`
- Env: `USE_SYNTHETIC_DATA=true`
- URL (app): `?country=synthetic`

Generate programmatically
```r
source("R/synthetic_data.R")
args <- resolve_synthetic_args(get("app_cfg", envir = .GlobalEnv))
syn  <- do.call(generate_synthetic_register, args)
write.csv(syn, "synthetic_register.csv", row.names = FALSE)
```

Config keys (excerpt)
```yaml
default:
  testing:
    use_synthetic_data: yes
    synthetic:
      country: syria
      n: 5000
      months: 12
      facilities: 60
```

Coverage
- Demographics, geography (admin0–admin3), time fields, morbidity
- Canonical disease + category, feature flags (VPD, epidemic‑prone, climate‑sensitive, AMR), ICD‑11 columns

Gotchas
- If taxonomy files are unavailable, ICD‑11 columns are filled with safe defaults
- Map layers assume Syria names by default; set `testing.synthetic.country` to adjust

