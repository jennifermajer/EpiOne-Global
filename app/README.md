# App (Shiny) — Quick Guide

Purpose: Host the dashboard UI/server and global config for EpiOne.

- Entry: `app.R` (detects `app/` vs root files)
- Core: `app/global.R`, `app/ui.R`, `app/server.R`

Run
- `shiny::runApp("app")`
- Synthetic mode: set `testing.use_synthetic_data: yes` or `USE_SYNTHETIC_DATA=true`.

Key files
- `app/global.R`: loads config, sets globals, sources modules
- `app/ui.R`: header/sidebar/body; adds synthetic badge and export controls
- `app/server.R`: optimized data loader, filter reactives, module init

EpiBot
- EpiBot appears if available; see `docs/epibot.md`.

Gotchas
- If DHIS2 creds not set, app auto-falls back to synthetic when enabled
- URL override: `?country=synthetic` enables synthetic for the session
