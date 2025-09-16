# Deployment (Overview)

Local
- `shiny::runApp("app")`
- Optional AI disabled by default; see `config/config.yml`.

Shiny Server / ShinyApps.io
- Deploy the `app/` directory; ensure `renv` lock is honored.
- Configure environment variables securely (e.g., DHIS2, AI providers).

With/without AI
- AI features controlled under `ai_features` and `epibot_features` in config.

