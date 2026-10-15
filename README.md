# Horizons / World Model

An interactive 4-tab dashboard projecting 52 global indicators from 1990 to 2100 across 30 countries, with a coupled integrated assessment model (IAM) linking climate, economics, health, and demographics.

## Pipeline

```
config/indicators.csv           ← single source of truth (52 indicators)
       |
Scripts/00_helpers.R            paths, schema contract, HTTP retry, token loader
Scripts/01_fetch_data.R         fetch raw data → data/raw/*.csv
Scripts/02_process_project.R    harmonise + project → panel_full.csv + coupling_spec.json
Scripts/03_Dash.R               inject into template → docs/dashboard.html
Scripts/04_diagnose.R           read-only: coverage, gaps, scenario differentiation
```

**Run order is load-bearing:** `01 → 02 → 03`. Running `03` alone after a model change ships a dashboard built from a stale `coupling_spec.json`.

## Setup

```r
# Install dependencies (R + pacman)
source("Scripts/00_helpers.R")
```

### Credentials

- **UN WPP API token:** set `WPP_API_TOKEN` environment variable, or save the token to `config/.wpp_token` (gitignored). Never hardcode.
- World Bank, OWID, and CCKP require no credentials.

## Running

```r
# Full rebuild
source("Scripts/01_fetch_data.R")
source("Scripts/02_process_project.R")
source("Scripts/03_Dash.R")
# Output: docs/dashboard.html

# Diagnose data coverage
source("Scripts/04_diagnose.R")
```

## Data sources

| Source | Indicators | Access |
|--------|-----------|--------|
| World Bank Indicators API v2 | 28 | open |
| Our World in Data grapher CSV | 17 | open |
| UN WPP 2024 | 4 | bearer token |
| World Bank CCKP (CMIP6) | 3 | open |

## Dashboard tabs

1. **World view** — 6 interpretive panels: population/aging, thermal future, inequality, health, democracy, civic fabric
2. **Coupled model** — IAM sliders (9 parameters, 5 edges), naive vs. coupled comparison, uncertainty fans
3. **Plot** — pick any module/indicator, all selected countries
4. **Scorecard** — one country, all indicators by domain

## Canonical data schema

`country_iso3, year, indicator, series_type, scenario, value, lower, upper`

See `README_current.md` for full technical detail: indicator registry, processing pipeline, IAM coupling spec, known limitations.
