# Architecture (Overview)

- App shell (`app/`): config load, module sourcing, UI/server
- Modules (`modules/`): self‑contained UIs/servers by analytic area
- Analysis (`R/`): preprocessing, taxonomy mapping, loaders, helpers
- Reports (`reports/`): R Markdown using the same preprocessing as the app
- Taxonomy (`taxonomy/`): base and country extensions, ICD‑11 mapping helpers

Data Flow
- Load → Preprocess → Taxonomy → Features/flags → Views/Report

Config Hierarchy
- `config/config.yml` (envs → default/development/production)
- Country configs: `config/countries/*.yml`
- AI customization: `config/ai_customization.yml`

Notes
- Synthetic data can short‑circuit loaders for local demos
- Optional AI layers are off by default and degrade gracefully

