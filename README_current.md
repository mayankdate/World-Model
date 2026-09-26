# Horizons / World Model — Current Build

*Detailed reference for the build as it stands. Companion to `README_base.md` (design principles and hard constraints). This document describes what actually exists: every data source, every indicator, how each is processed, and what is known to be wrong.*

**Status:** all four symptoms reported against the last build are diagnosed and fixed. Requires a full `01 → 02 → 03` rerun; the model is spec-driven, so a stale `coupling_spec.json` will reintroduce the dead-slider behaviour.

---

## 1. Pipeline at a glance

```
config/indicators.csv         52 indicators — the single source of truth for what gets fetched
        |
Scripts/00_helpers.R          paths, schema contract, HTTP retry, registry loader, WPP token
Scripts/01_fetch_data.R       4 source families -> data/raw/*.csv (one file per thing)
Scripts/02_process_project.R  harmonise -> panel.csv
                              multilevel projection -> projections.csv -> panel_full.csv
                              build_coupling_spec() -> coupling_spec.json
                              append -> data/calibration/ledger.csv
Scripts/03_Dash.R             filter, clip, thin -> inject META+DATA+COUPLING -> dashboard.html
Scripts/04_diagnose.R         read-only: coverage, gaps, scenario differentiation
Scripts/dashboard_template.html   the app (HTML + CSS + vanilla JS, zero dependencies)
```

**Run order is load-bearing.** `02` writes *both* `panel_full.csv` and `coupling_spec.json`; `03` consumes both. Running `03` alone after a model change ships a dashboard whose UI is built from a stale spec.

---

## 2. The data

### 2.1 Sources

| Source | Access | Indicators | Layer produced | Reach |
|---|---|---|---|---|
| **World Bank** Indicators API v2 | open, no key | 28 | `observed` | history → structural projection, 5 yr cap |
| **Our World in Data** grapher CSV | open, no key | 17 | `observed` | history → structural projection, 5 yr cap |
| **UN WPP 2024** data portal API | **bearer token required** | 4 | `observed` + `projection_demographic` | 1990–2100, UN's own 95% PI |
| **World Bank CCKP** (CMIP6) | open, no key | 3 | `projection_climate` | 2015–2100, per SSP, model spread |

The WPP token is read from `WPP_API_TOKEN` or a gitignored `config/.wpp_token` — never hardcoded. WPP's `locations/all` endpoint returns HTTP 500, so locations are fetched per country; the returned list mixes real countries with UN analytical aggregates, filtered out by name pattern plus the rule that any ISO3 mapping to more than one location id is not a sovereign state.

OWID is fetched as `https://ourworldindata.org/grapher/{slug}.csv?csvType=full`. Slugs are **not stable** — OWID renames charts, and V-Dem charts have been progressively suffixed `-vdem`. A renamed slug 404s, is logged, and the run continues, so a dead indicator is silent unless you check. `04_diagnose.R` reports indicators with zero rows.

### 2.2 Full indicator registry (52)

Registry columns: `indicator, module, source, source_id, unit, direction, description, weight`. `series_type` and `horizon_cap_years` are **derived** from `source` at load (`00_helpers.R`), not stored — World Bank and OWID get a 5-year cap, WPP and CCKP get 75.

**economy (8)** — all World Bank
`gdp_per_capita_ppp` NY.GDP.PCAP.PP.KD · `gdp_growth` NY.GDP.MKTP.KD.ZG · `inflation_cpi` FP.CPI.TOTL.ZG · `gini_index` SI.POV.GINI · `poverty_headcount_ratio` SI.POV.DDAY · `gross_savings_pct_gdp` NY.GNS.ICTR.ZS · `gross_capital_formation_pct_gdp` NE.GDI.TOTL.ZS · `trade_pct_gdp` NE.TRD.GNFS.ZS

**labor (5)** — all World Bank
`unemployment_rate` SL.UEM.TOTL.ZS · `youth_unemployment_rate` SL.UEM.1524.ZS · `labor_force_participation` SL.TLF.CACT.ZS · `female_labor_participation` SL.TLF.CACT.FE.ZS · `vulnerable_employment` SL.EMP.VULN.ZS

**health (6)** — all World Bank
`life_expectancy` SP.DYN.LE00.IN · `infant_mortality` SP.DYN.IMRT.IN · `maternal_mortality` SH.STA.MMRT · `physicians_per_1000` SH.MED.PHYS.ZS · `health_expenditure_pct_gdp` SH.XPD.CHEX.GD.ZS · `hospital_beds_per_1000` SH.MED.BEDS.ZS

**demographics (8)** — 4 World Bank + 4 UN WPP
`population_growth` SP.POP.GROW · `fertility_rate` SP.DYN.TFRT.IN · `dependency_ratio` SP.POP.DPND · `urban_population_share` SP.URB.TOTL.IN.ZS · **WPP:** `population_total` (id 49) · `median_age` (67) · `fertility_rate_proj` (19) · `life_expectancy_proj` (61)

**rights (9)** — all OWID / V-Dem, 0–1 unless noted
`liberal_democracy_index` · `electoral_democracy_index` · `participatory_democracy_index` · `deliberative_democracy_index` (slug `…-index-vdem`) · `egalitarian_democracy_index` (slug `…-index-vdem`) · `freedom_of_expression_index` · `freedom_of_association_index` · `political_polarization` (slug `political-polarization-score`, **0–4 scale**, higher = worse) · `academic_freedom` (slug `academic-freedom-index`)

**stability (4)** — all OWID / V-Dem, 0–1
`liberal_component_index` (slug `liberal-political-institutions-index`) · `political_corruption_index` (higher = worse) · `rule_of_law_index` · `judicial_constraints_executive`

**climate (8)** — 2 World Bank + 3 OWID + 3 CCKP
`forest_area_share` AG.LND.FRST.ZS · `pm25_exposure` EN.ATM.PM25.MC.M3 · `co2_per_capita` (OWID `co-emissions-per-capita`) · `renewable_electricity_share` (OWID) · `air_pollution_death_rate` (OWID `outdoor-pollution-death-rate`) · **CCKP:** `mean_temperature` tas · `max_temperature` tasmax · `heat_index_days_over_35c` hi35

**technology (4)** — 1 OWID + 3 World Bank
`internet_users_share` (OWID) · `mobile_subscriptions` IT.CEL.SETS.P2 · `rd_expenditure_pct_gdp` GB.XPD.RSDV.GD.ZS · `high_tech_exports_share` TX.VAL.TECH.MF.ZS

### 2.3 Registry changes made in this pass

Four slug problems were identified and verified against OWID:

| Indicator | Was | Now | Why |
|---|---|---|---|
| `egalitarian_democracy_index` | `egalitarian-democracy-index` | `egalitarian-democracy-index-vdem` | OWID renamed it; 404 → zero rows, silently |
| `air_pollution_death_rate` | `death-rates-from-air-pollution` | `outdoor-pollution-death-rate` | old slug is a **multi-series** chart (outdoor / household / ozone in separate columns); the parser takes column [1] blindly |
| `political_polarization` | *absent* | `political-polarization-score` | the civic-fabric tab had a button for it but the registry never had the row |
| `academic_freedom` | *absent* | `academic-freedom-index` | same |

Verify the two new ones on first fetch — a slug that resolves in a browser can still change column names.

### 2.4 Countries and window

30 countries embedded (`03_Dash.R`): the 19 G20 member states plus NGA, EGY, IRN, VNM, POL, ESP, NLD, PAK, BGD, ETH, THA. Six toggled on at open: **USA, CAN, IND, GBR, JPN, RUS** (note: five of six sit at or below the BHM growth optimum — see §5.3).

Time axis fixed **1990–2100**. Observed history clipped to 1990+ (`HISTORY_START`) — V-Dem reconstructs to 1789 and OWID CO₂ to 1750, which are historians' codings rather than measurements. Personalised to birth year **1996**, driving the age-milestone lines.

### 2.5 Canonical schema

`country_iso3, year, indicator, series_type, scenario, value, lower, upper`

| `series_type` | Band means | Source |
|---|---|---|
| `observed` | nothing (`lower = upper = value`) | WB / OWID / WPP ≤2023 |
| `projection_demographic` | UN's own 95% prediction interval | WPP ≥2024 |
| `projection_climate` | CMIP6 **inter-model spread** p10–p90 — *not* a forecast PI | CCKP |
| `projection_structural` | our own fitted interval — the only one we're accountable for | `02` |

Bands are not comparable across layers. Layers are kept as distinct rows, never blended.

---

## 3. Processing

### 3.1 Harmonisation

Each source's shape is normalised into the canonical schema, and the observed/projection split is drawn here so layers are distinct rows from the first table onward.

- **World Bank / OWID** → `observed`. OWID CSVs now log a warning when more than one metric column is present, instead of silently taking the first.
- **UN WPP** → split at `WPP_ESTIMATE_END = 2023`. The API's `estimateType` field reads "Model-based Estimates" for *all* years including projections, so it cannot be used to split; the reliable signals are the year boundary and the presence of variant bounds. Requires a "Both sexes" filter and an age-total filter before dedup, or values silently mix.
- **CCKP** → `projection_climate`, one row per (country, year, scenario) with p10/p90 as the band.

### 3.2 Structural projection — multilevel, transform-aware

Applies to the 45 World Bank + OWID indicators, capped at 5 years.

**Level 1 (within country):** OLS of the indicator on a bound-respecting transform scale, over a 15-year window (`FIT_WINDOW`), needing ≥6 real points (`MIN_POINTS`), centred at the country's last observed year so the intercept *is* the current level.

**Level 2 (across countries):** country slopes treated as draws from a common distribution; between-country variance estimated by DerSimonian–Laird; each country's slope shrunk toward the pooled mean by empirical-Bayes weights. Noisy short series borrow strength; precise ones barely move. Below 5 fittable countries (`MIN_COUNTRIES_POOL`) it falls back to exact per-country OLS with full covariance.

**Transforms** — bounds are respected by construction, not by clamping (clamping a prediction interval at a bound destroys its stated coverage):

| Transform | Applied to |
|---|---|
| `logit01` | `index_0_1` — all V-Dem indices |
| `logit04` | `index_0_4` — polarization score |
| `logit100` | `index_0_100` and true 0–100 shares (unemployment, urban share, internet share, …) |
| `log` | strictly-positive quantities (dollars, rates per 1000/100k, years, µg/m³, `percent_gdp`) |
| `identity` | everything else (growth rates, inflation — these can legitimately go negative) |

`percent_gdp` is deliberately **log**, not a share: trade as % of GDP routinely exceeds 100.

**Intervals:** t-quantile with per-country dof at `CONF_LEVEL = 0.95`. (z-quantiles under-covered at ~91% in simulation; t gives 93.7%.)

**Staleness rule (`MAX_STALENESS = 3`):** a country is projected only if its series reaches within 3 years of the freshest country for that same indicator. This kills orphan segments — `gross_savings_pct_gdp` previously had a country whose data ended in 2000 still receiving a 2001–2005 projection, floating 19 years short of the panel's data edge and connected to nothing.

**Calibration ledger:** every forecast is appended to `data/calibration/ledger.csv` with issue date, horizon, and claimed interval, plus an empty `realized` slot. **The scoring pass that fills `realized` and checks coverage does not exist yet** — see §6.

---

## 4. The coupled model (IAM)

Defined once in R (`build_coupling_spec()`), serialised to `coupling_spec.json`, injected as `const COUPLING`. The JS holds only an evaluator library of named forms. Adding an edge that reuses an existing form requires no JavaScript.

### 4.1 Parameters (9)

| Parameter | Default | Range | Sampled in MC? |
|---|---|---|---|
| `Topt` GDP optimum temperature | 13.0 °C | 13.0 – 16.0 | yes |
| `bhmCurv` GDP–temp curvature | 0.0005 /°C² | 0 – 0.0010 | yes |
| `bhmGainCap` cap on climate growth **gain** | 0.02 /yr | 0 – 0.05 | **no** (structural bound) |
| `bhmGainShare` cold-country gain share | 1.0 | 0 – 1 | **no** (structural switch) |
| `prestonSlope` Preston curve slope | 5.0 yr/ln$ | 3 – 7 | yes |
| `carletonMort` direct heat mortality | 0.20 yr/°C | 0 – 0.60 | yes |
| `fertIncome` fertility–income response | −0.20 births/ln$ | −0.40 – 0 | yes |
| `mortPopFeedback` heat-mortality → population | 0.0008 frac/yr/°C | 0 – 0.0030 | yes |
| `gFrontier` long-run frontier growth | 0.015 /yr | 0.005 – 0.025 | yes |
| `tauConv` growth convergence timescale | 40 yr | 15 – 80 | yes |

### 4.2 Edges (5)

| Edge | Form | Source | Kind |
|---|---|---|---|
| temp → GDP | `bhm_quadratic` | Burke, Hsiang & Miguel 2015, *Nature* 527:235 | empirical |
| GDP → life expectancy | `preston_log` | Preston 1975 | empirical |
| temp → life expectancy | `linear_marginal_scaled` | Carleton et al. 2022, *QJE* | empirical |
| GDP → population | `fertility_income` | Herzer, Strulik & Vollmer 2012 | empirical |
| temp → population | `heat_mortality_pop` | derived from Carleton 2022 | **structural** |

Governance couplings remain deliberately **absent** — no defensible estimate.

### 4.3 Mechanics

- **Driver:** `mean_temperature` (CMIP6 tas) only. `max_temperature` is display-only — BHM's optimum is defined on annual-mean temperature.
- **Baseline climate:** 20-year climatology (`CLIM_WIN = 20`), not a single year. Since the response is quadratic in (T − baseline), one anchor year turned ordinary CMIP6 interannual wiggle into damage — Canada was losing 1.6% of 2100 income to noise, Japan 5%.
- **Baseline growth:** converges from each country's recent observed rate toward `gFrontier` with e-folding time `tauConv`. Constant-growth extrapolation put a 5%/yr country at 45× income by 2100.
- **Asymmetric cap:** damages are uncapped (they self-limit); gains are capped at `bhmGainCap`. Calibrated against BHM's published country table — India −87%, Nigeria −90%, Russia/Canada +265% against BHM's −87% / −92% / +419% / +247%.
- **Adaptation:** heat vulnerability scales inversely with the model's *own* coupled income path, reference $40k, clamped [0.4, 3.0].
- **Uncertainty:** 32 deterministic stratified draws (`MC_N`) over sampled coefficients × CMIP6 spread, propagated through the whole chain, 5–95% envelope. Coefficient uncertainty is each parameter's *disputed range* read as a 95% interval, **not** the papers' own standard errors — stated openly in the spec notes.
- **Naive line:** same convergent baseline with all coupling coefficients zeroed, so the naive-vs-coupled gap isolates the coupling.

---

## 5. The four tabs

**World view** — six interpretive panels: population & aging (line thickness = dependency ratio, node labels = median age); thermal future (mean temp line + max envelope + heat-index stripe); Gini/unemployment; health line-bubble (life expectancy with physician-count bubbles); democracy thick-vs-thin (liberal vs electoral, red gap-shading where electoral exceeds liberal); civic fabric (selectable V-Dem index). Plus a difference-vs-reference toggle.

**Coupled model** — sliders for all spec parameters, one plot per coupled quantity, naive vs coupled, MC fan, spec-generated citation panel.

**Plot** — pick a module, every indicator renders for the selected countries with all bands shown.

**Scorecard** — one country, all indicators; tiles by domain on the left, pinned plot on the right.

### 5.1 Fixes to presentation in this pass

- **Climate axis is scenario-invariant.** It was refitting to whichever SSP was selected, so all three landed in near-identical pixels. The heat stripe likewise renormalised its colour ramp to each scenario's own worst year, making SSP1-2.6 as red as SSP5-8.5. Both now normalise across all three.
- **Civic buttons are built from data.** Two buttons named indicators that weren't in the registry, rendering empty charts with no error. The segment is now generated from what's present in `DATA`; missing ones log a warning and are omitted.
- **Observed lines break across gaps > 5 years** (`MAX_GAP_YEARS`). Four indicators are missing a *median* of 14–18 years inside their own span; a straight polyline across those holes rendered interpolation as measurement. Lone observations become dots.
- **IAM y-domain from central lines only**, bands clipped to the plot area — one wide band no longer squashes every other country flat.
- **Drag performance.** Renders coalesced to one per animation frame (a drag fires ~60 events/sec at ~24 ms each). Bands are held during the drag rather than vanishing and reappearing, which was the strobing. `pointerup` and `blur` handled alongside `change`, which doesn't fire if the pointer is released outside the slider.
- **Slider legibility.** Default markers are ticks at their true position (the label was centred even when the default sat at an end); values rounded to each slider's step precision.

### 5.2 WPP cache coverage guard

`01_fetch_data.R` now reuses a cached WPP file only if it *covers* `WPP_START_YEAR = 1990`, not merely if it exists. Three caches were stuck at 2010+ from an earlier run while `REFRESH_DEMOGRAPHIC = FALSE` prevented re-fetching — which is why population started mid-chart.

### 5.3 Known presentational tension

The default six countries are USA, CAN, IND, GBR, JPN, RUS. Five sit at or below the 13 °C optimum, so `bhmCurv` and `Topt` move them far less than they move India. The slider is alive (5 of 6 respond measurably under SSP5-8.5) but the panel still reads as India-dominated. Adding a hot country to the default selection would help more than any code change.

---

## 6. Known limitations

1. **Climate is projection-only.** CCKP provides no observed history; the climate panel is entirely modelled, correctly labelled, with no observational anchor.
2. **`max_temperature` understates lived extremes** — a national annual mean of daily maxima. Display only; never drives the model.
3. **Coefficient uncertainty is range-based**, not the papers' standard errors. Coefficients were encoded from published headline values via literature search, not supplementary tables. Verify before citing.
4. **BHM's cold-country gains cannot be reconciled** with its published country projections using the headline coefficients — the raw quadratic gives Canada/Russia a multi-point permanent annual growth bonus. `bhmGainCap` bounds this empirically rather than deriving it. USA lands near 0% because early-century gains offset late-century damages; BHM's own USA figure is more negative.
5. **Preston applies a cross-sectional curve to within-country change.** Preston's own decomposition attributes most secular life-expectancy gain to the curve shifting, so this edge bounds the income channel only.
6. **`heat_mortality_pop` at default**, times income scale, times end-century warming, sits at or above the top of Carleton's published range. Read the slider's upper half as a limited-adaptation world.
7. **Projecting expert-coded V-Dem indices is dubious as a model choice** even though the logit transform now makes the arithmetic well-behaved.
8. **`WPP_ESTIMATE_END = 2023` is hardcoded.** Correct for WPP 2024, silently wrong for the next revision.
9. **The calibration ledger is never scored.** ~30 lines of R, and the only mechanism that can empirically discipline any of the above.
10. **Per-country coupling is less standard than global/regional IAMs** (DICE, FUND); return edges are less cleanly identified at country level.

---

## 7. Verification practice

No R runtime in the dev sandbox, so R is checked by comment- and string-aware bracket balancing plus Python harnesses mirroring the statistics on synthetic data with known truth (shrinkage behaviour, bound respect, interval coverage by simulation).

The dashboard is verified by `realharness.js`, which executes the template's real script in Node with DOM stubs against the **actual injected payload** from a built `dashboard.html`. This is what located the dead sliders, the GDP explosion, and the missing civic indicators — synthetic archetypes had passed all of them. Rendering in a real browser remains necessary before shipping; the harness verifies model logic, not paint.

## 8. Next steps, in order

1. Rerun `01 → 02 → 03`. Confirm `gFrontier` and `bhmGainCap` appear in `coupling_spec.json`.
2. Run `04_diagnose.R` and confirm the four slug fixes produce rows.
3. Write the ledger scoring pass (§6.9).
4. Make the WPP estimate/projection split data-driven rather than a hardcoded year (§6.8).
5. Settle the direction questions in `README_base.md` — what the dashboard is *for* — before adding features.
