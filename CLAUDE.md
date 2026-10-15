# Horizons / World Model: contributor and AI coding guidance

## Purpose

An R-based pipeline that fetches 52 global indicators from 4 data sources,
projects them 1990–2100 for 30 countries, builds an integrated assessment
model (IAM) linking climate → GDP → health/demographics, and renders a
self-contained interactive dashboard as `docs/dashboard.html`.

## Architecture

```
config/
  indicators.csv            ← SINGLE SOURCE OF TRUTH: all 52 indicators
  .wpp_token                ← gitignored; WPP bearer token

Scripts/
  00_helpers.R              paths, schema contract, HTTP retry, token reader
  01_fetch_data.R           4 source families → data/raw/*.csv
  02_process_project.R      harmonise → panel.csv → projections.csv →
                            panel_full.csv + coupling_spec.json + calibration ledger
  03_Dash.R                 filter/thin/inject → docs/dashboard.html
  04_diagnose.R             read-only; coverage, gaps, scenario differentiation

Scripts/dashboard_template.html   app (HTML + CSS + vanilla JS, zero dependencies)

data/
  raw/                      fetched source files (gitignored)
  calibration/ledger.csv    forecast ledger (issue date, horizon, claimed interval)

docs/
  dashboard.html            ← GENERATED — do not edit by hand
```

## Run order is load-bearing

`02` writes both `panel_full.csv` **and** `coupling_spec.json`. `03` consumes
both. Running `03` alone after a model change ships a dashboard whose UI is
built from a stale coupling spec. Always run `01 → 02 → 03` in full after
any model or data change.

## Canonical schema

`country_iso3, year, indicator, series_type, scenario, value, lower, upper`

`series_type` values: `observed`, `projection_demographic`, `projection_climate`,
`projection_structural`. Bands are not comparable across layers; never blend them.

## Credentials

- **WPP token:** read from `WPP_API_TOKEN` env var or gitignored `config/.wpp_token`.
  Never hardcode. The `.Renviron` file in the repo is gitignored and safe to use locally.
- All other sources are open.

## `config/indicators.csv` is the single source of truth

Each row is one indicator. Fields: `indicator, module, source, source_id, unit,
direction, description, weight`. Adding or renaming an indicator here is the
only change required — the pipeline reads it dynamically. Do not hardcode
indicator names anywhere else.

**OWID slugs are not stable.** A renamed slug 404s silently and produces zero
rows. `04_diagnose.R` will flag it. Verify slugs against the live OWID site
before adding them to the registry.

## Projection model (02)

**Structural:** OLS on a bound-respecting transform (logit, log, or identity
per indicator), shrunk toward a cross-country pooled slope via empirical Bayes
(DerSimonian–Laird). Cap: 5 years. Minimum: 6 real data points, 5 fittable
countries (else falls back to per-country OLS).

**Staleness rule:** project only if a country's series reaches within 3 years
of the freshest country for that indicator. This prevents orphan segments.

## IAM coupling (`coupling_spec.json`)

Defined once in R (`build_coupling_spec()` in `02`), serialised to JSON,
injected into the dashboard as `const COUPLING`. The JS evaluator is generic;
all model logic stays in R. To add an edge that reuses an existing JS form,
only `02` needs to change.

**Never edit `coupling_spec.json` by hand.** Regenerate by running `02`.

## Change guidelines

- `docs/dashboard.html` is a build artifact. Never hand-edit it.
- `Scripts/dashboard_template.html` is the source for the app shell. Edit
  this, then rerun `03` to rebuild the dashboard.
- Adding a new indicator: add a row to `config/indicators.csv`. Run
  `01 → 02 → 03`. No other change required.
- Adding a new coupling edge: implement in `build_coupling_spec()` in `02`;
  add the evaluator form to `dashboard_template.html` if it's a new functional
  form. Run `02 → 03`.
- Do not suppress errors from HTTP fetches. Log failures explicitly; a
  missing indicator must be visible in `04_diagnose.R`.
- `data/raw/` and `docs/dashboard.html` are gitignored (large generated files).
  The calibration ledger (`data/calibration/ledger.csv`) is tracked.
