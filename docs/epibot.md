# EpiBot – Epidemiology Assistant

EpiBot is the optional AI assistant embedded in the dashboard. The module lives in `modules/mod_epibot.R` and provides both the UI and the server logic for the chat experience. The assistant combines deterministic heuristics with a locally hosted large language model (via [Ollama](https://ollama.com/)) to interpret analyst questions, guard sensitive data, and return epidemiology-focused summaries.

## Enabling EpiBot

1. Ensure the AI feature flag is enabled in `config/ai_customization.yml` or the site configuration you deploy:

   ```yaml
default:
  ai_features:
    enabled: yes
    summary_generation:
      enabled: yes
  epibot_features:
    enabled: yes
   ```

2. Provide the Ollama endpoint and model in the R environment. The defaults expect a local instance listening on `http://localhost:11434` with a model called `llama3.2:3b`. Override by setting `app_cfg$ai_features$ollama` before the app loads.

3. When the app starts it sources `modules/mod_epibot.R`. If the AI system is unavailable, the module runs in demo mode and falls back to keyword-based answers.

## How It Works

- **Intent detection** – `query_llm_for_intent()` calls the configured Ollama model with SME-informed hints and maps responses into internal intents (`summary`, `trends`, `diseases`, `geography`, `facilities`, `surveillance`, `help`). Fallback heuristics cover disconnected/offline scenarios.
- **Context filters** – `filter_data_by_entities()` narrows the reactive dataset using the diseases, locations, and time periods mentioned by the analyst. If no matches are found, the assistant returns a scoped warning.
- **Response builders** – Scenario-specific helpers (e.g. `generate_trends_response_llm()`) produce concise, grounded bullet summaries. They always hedge when confidence is low and avoid row-level data.
- **Guardrails** – `apply_response_guardrails()` scans generated prose for sensitive strings and flags hallucinated columns in the audit log.
- **Memory** – `add_to_enhanced_memory()` stores the last fifteen turns so references like “that disease” resolve correctly.

## Development Notes

- The module is a self-contained Shiny module: `mod_epibot_ui(id)` renders the panel and `mod_epibot_server(id, data)` wires the chat to a reactive dataset.
- All helper functions live in the same file for now, but they are grouped by concern (`SME`, `intent`, `memory`, `response`, `UI`) with clear section headers. The earlier `modules/epibot/` subfolder has been removed to eliminate redundant implementations.
- To test outside the app, run `source("modules/mod_epibot.R")` and call the exported functions in a standalone Shiny playground.
- Audit entries land in `values$audit_log` and, when `options(epibot.audit.enabled = TRUE)`, append to `epibot_audit.log`.

## Troubleshooting

| Symptom | Likely Cause | Fix |
| --- | --- | --- |
| “EpiBot offline / demo mode” banner | Ollama not reachable or AI feature flag disabled | Start Ollama (`ollama serve`) or update config |
| Chat replies are generic | Intent detection confidence < 0.6 or dataset empty | Provide more context or ensure filtered dataset has rows |
| Privacy flag warning | Guardrails detected sensitive text or missing columns | Review logged message and adjust prompt/data |

For further guidance open an issue or ping the data science team.
