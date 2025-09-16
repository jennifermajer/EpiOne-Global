# Troubleshooting

No data loads
- Enable synthetic mode to verify UI: `USE_SYNTHETIC_DATA=true`
- Check `config/config.yml` and country configs for typos

Dates look wrong
- Ensure `datevisit` or a recognizable date column is present

Maps are empty
- Verify `data/shapefiles/<country>/` and `admin1/admin2` values

ICD‑11 fields missing
- Ensure taxonomy files under `taxonomy/` are present; otherwise placeholders are used

AI failures
- Leave AI disabled; dashboard/report work without it

