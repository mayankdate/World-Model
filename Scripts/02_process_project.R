# R/02_process.R
# =============================================================================
# Reads the four raw caches and unifies them into ONE canonical table:
#   data/output/panel.csv  (country_iso3, year, indicator, series_type,
#                           scenario, value, lower, upper)
#
# This is where each source's idiosyncratic shape is normalised into the
# common schema -- and crucially where the observed/projection SPLIT is drawn,
# so the horizon-layers stay distinct rows from the very first table.
#
# Structural projections (the OLS extrapolation) are NOT made here -- they need
# the harmonised observed history as input, so they live in 03_project.R.
# =============================================================================

source("Scripts/00_helpers.R")
ensure_dirs()

indicators <- read_indicators()

# -- Structural (World Bank + OWID) -> observed rows --------------------------
harmonize_structural <- function() {
  wb <- indicators |> filter(source == "worldbank")
  wb_rows <- map_dfr(wb$indicator, function(name) {
    p <- raw_path_wb(name); if (!file.exists(p)) return(NULL)
    df <- read_csv(p, show_col_types = FALSE); if (nrow(df) == 0) return(NULL)
    df |> transmute(country_iso3, year = as.integer(year), indicator = name, value)
  })
  
  ow <- indicators |> filter(source == "owid")
  ow_rows <- map2_dfr(ow$indicator, ow$source_id, function(name, slug) {
    p <- raw_path_owid(slug); if (!file.exists(p)) return(NULL)
    df <- read_csv(p, show_col_types = FALSE)
    if (!all(c("Code", "Year") %in% names(df))) return(NULL)
    metrics <- setdiff(names(df), c("Entity", "Code", "Year"))
    metric  <- metrics[1]
    if (is.na(metric)) return(NULL)
    # Multi-series OWID charts (e.g. death-rates-from-air-pollution splits
    # outdoor / household / ozone into separate columns) silently resolved to
    # whichever column happened to come first, which is how an indicator ended
    # up with zero usable rows and no error. Say so instead of guessing.
    if (length(metrics) > 1)
      log_info(sprintf("  WARN %s: %d metric columns [%s] -- using '%s'. Prefer a single-series slug.",
                       slug, length(metrics), paste(metrics, collapse = " | "), metric))
    df |> transmute(country_iso3 = Code, year = as.integer(Year),
                    indicator = name, value = as.numeric(.data[[metric]])) |>
      filter(is_iso3(country_iso3), !is.na(year), !is.na(value))
  })
  
  bind_rows(wb_rows, ow_rows) |>
    mutate(series_type = "observed", scenario = "none", lower = value, upper = value)
}

# -- Demographic (UN WPP) -> observed + projection_demographic ----------------
# UN estimates run through 2023, projections from 2024. CONFIRMED against the
# live API: the `estimateType` field is "Model-based Estimates" for ALL years
# (including projections), so it CANNOT be used to split observed vs projection.
# The reliable signal is the year boundary plus the band: estimate years carry
# only the "Median" variant (no interval); projection years (2024+) add the
# 95%/80% bounds. We split on the year: <= 2023 observed, > 2023 projection.
WPP_ESTIMATE_END <- 2023
harmonize_demographic <- function() {
  demo <- indicators |> filter(source == "unwpp")
  map_dfr(demo$indicator, function(name) {
    p <- raw_path_wpp(name); if (!file.exists(p)) return(NULL)
    df <- read_csv(p, show_col_types = FALSE); if (nrow(df) == 0) return(NULL)
    
    # Confirmed real variant labels from the portal: "Median", "95% Lower",
    # "95% Upper", "80% Lower", "80% Upper", plus ~12 deterministic variants
    # (High-fert., Low-fert., Constant-mort., No change, ...) which we ignore.
    # We keep the probabilistic median + 95% prediction interval only.
    v <- tolower(df$variant)
    is_med <- v == "median"
    is_lo  <- str_detect(v, "95") & str_detect(v, "lower")
    is_hi  <- str_detect(v, "95") & str_detect(v, "upper")
    if (!any(is_med)) {
      log_info(sprintf("  [%s] no median variant matched. Saw: %s",
                       name, paste(unique(df$variant), collapse = " | ")))
      if (n_distinct(df$variant) == 1) is_med <- rep(TRUE, nrow(df)) else return(NULL)
    }
    
    # Dedup each slice to one row per country-year before joining, so a stray
    # duplicate location (should be filtered upstream, but defensive) can never
    # cause a many-to-many join that multiplies rows.
    med <- df[is_med, ] |> transmute(country_iso3 = iso3, year, value) |>
      distinct(country_iso3, year, .keep_all = TRUE)
    lo  <- df[is_lo, ]  |> transmute(country_iso3 = iso3, year, lower = value) |>
      distinct(country_iso3, year, .keep_all = TRUE)
    hi  <- df[is_hi, ]  |> transmute(country_iso3 = iso3, year, upper = value) |>
      distinct(country_iso3, year, .keep_all = TRUE)
    
    out <- med |>
      left_join(lo, by = c("country_iso3", "year"), relationship = "one-to-one") |>
      left_join(hi, by = c("country_iso3", "year"), relationship = "one-to-one") |>
      mutate(
        indicator   = name,
        series_type = if_else(year <= WPP_ESTIMATE_END, "observed", "projection_demographic"),
        scenario    = "none",
        # observed rows carry no band; projection rows get the 95% PI if present,
        # else fall back to the central value (degenerate band, flagged in QC).
        lower = if_else(series_type == "observed", value, coalesce(lower, value)),
        upper = if_else(series_type == "observed", value, coalesce(upper, value))
      )
    n_band <- sum(out$series_type == "projection_demographic" & out$lower != out$upper)
    log_info(sprintf("  [%s] %d rows (%d projection rows with a real band)",
                     name, nrow(out), n_band))
    out
  })
}

# -- Climate (CCKP) -> projection_climate, one set of rows per scenario -------
SCENARIOS <- c("ssp126", "ssp245", "ssp585")
harmonize_climate <- function() {
  clim <- indicators |> filter(source == "cckp")
  cross <- tidyr::expand_grid(indicator = clim$indicator, scenario = SCENARIOS) |>
    left_join(clim |> select(indicator, source_id), by = "indicator")
  pmap_dfr(cross, function(indicator, scenario, source_id) {
    p <- raw_path_cckp(source_id, scenario); if (!file.exists(p)) return(NULL)
    df <- read_csv(p, show_col_types = FALSE); if (nrow(df) == 0) return(NULL)
    df |> transmute(
      country_iso3 = iso3, year = as.integer(year), indicator = indicator,
      series_type = "projection_climate", scenario = scenario,
      value, lower = coalesce(lower, value), upper = coalesce(upper, value)
    ) |> filter(is_iso3(country_iso3))
  })
}

log_info("Harmonizing sources into canonical schema...")
parts <- list(
  structural  = harmonize_structural(),
  demographic = harmonize_demographic(),
  climate     = harmonize_climate()
)
for (nm in names(parts)) {
  n <- if (is.null(parts[[nm]])) 0 else nrow(parts[[nm]])
  log_info(sprintf("  %-12s %s rows", nm, format(n, big.mark = ",")))
}

panel <- bind_rows(parts) |>
  as_canonical() |>
  distinct(country_iso3, year, indicator, series_type, scenario, .keep_all = TRUE)

out_path <- file.path(PATHS$output, "panel.csv")
write_csv(panel, out_path)
log_info("Wrote ", out_path, " (", format(nrow(panel), big.mark = ","), " rows)")
log_info("series_type distribution:")
print(panel |> count(series_type))



# PROJECT ---------------------------------------------------------------------
# STRUCTURAL PROJECTIONS, v2: transform-aware MULTILEVEL (partial-pooling)
# trend model instead of independent per-country OLS.
#
#   Level 1 (within country):  z_it = a_i + b_i * h + e_it,   h = year - last_year
#   Level 2 (across countries): b_i ~ Normal(mu, tau^2)
#
# where z is the indicator on a bound-respecting TRANSFORM scale (logit for
# 0-1 / 0-100 shares, log for strictly-positive quantities, identity
# otherwise). Country slopes are shrunk toward the cross-country mean with
# empirical-Bayes weights (DerSimonian-Laird tau^2), so noisy short series
# borrow strength from the panel instead of extrapolating their own noise.
#
# What this fixes, concretely:
#   - Bounded V-Dem indices no longer produce linear ramps that slam into 0/1:
#     the logit transform makes projections asymptote, and the back-transformed
#     interval stays inside the bounds by construction (limitation #3).
#   - Strictly-positive indicators (maternal mortality, PM2.5, $) can no longer
#     be extrapolated below zero (the old clamp was a symptom patch; the log
#     scale removes the disease). clamp_bounds is kept as a final safety net.
#   - Countries with thin/noisy windows get shrunken slopes + honest wider
#     intervals rather than confident nonsense.
#
# Interval on the transform scale at horizon h:
#   var = Var(a_i) + h^2 * Var(b*_i) + sigma_i^2
# with Var(b*_i) the EB posterior variance 1/(1/se_i^2 + 1/tau^2) plus the
# variance of the pooled mean. Back-transformation maps the interval through
# the monotone inverse (so the central value is the MEDIAN on the original
# scale — stated, not hidden). When fewer than MIN_COUNTRIES_POOL countries
# are fittable, we fall back to the exact per-country OLS prediction interval.

FIT_WINDOW <- 15   # years of recent history to fit
MIN_POINTS <- 6    # need at least this many real points in the window to fit
CONF_LEVEL <- 0.95 # interval coverage (t-quantile per country: sigma is
# estimated from few dof, z-quantiles under-cover ~91%)
MIN_COUNTRIES_POOL <- 5  # below this, partial pooling is meaningless
# A country whose observed series ended years ago must NOT get a projection: it
# produces an orphan 5-year segment floating in the middle of the chart, cut off
# from both the country's own line and the panel's data edge (gross_savings had
# one running 2001-2005 while the panel's edge was 2024). Rule: only project if
# the series reaches within MAX_STALENESS years of the FRESHEST country for that
# same indicator -- an indicator-relative test, since sources legitimately lag by
# different amounts. Stale series simply stop where their data stops.
MAX_STALENESS <- 3

indicators <- read_indicators()
panel_path <- file.path(PATHS$output, "panel.csv")
if (!file.exists(panel_path)) stop("panel.csv not found — run 02_process.R first")
panel <- read_csv(panel_path, show_col_types = FALSE)

proj_spec <- indicators |>
  filter(projection_method == "projection_structural") |>
  select(indicator, horizon_cap_years, direction, unit)

log_info("Structural projection v2 (multilevel): ", nrow(proj_spec),
         " indicators, ", FIT_WINDOW, "-year fit window.")

# -- Transform layer ----------------------------------------------------------
EPS01 <- 0.005   # keep logits finite at the bounds

# Percent indicators that are true 0-100 SHARES (growth/inflation are not).
SHARE_PERCENT <- c("poverty_headcount_ratio", "unemployment_rate",
                   "youth_unemployment_rate", "labor_force_participation",
                   "female_labor_participation", "vulnerable_employment",
                   "urban_population_share", "renewable_electricity_share",
                   "internet_users_share")

# Strictly-positive units -> log scale. NOTE percent_gdp is here (trade % GDP
# routinely exceeds 100, so it is positive-unbounded, NOT a share).
POSITIVE_UNITS <- c("constant_2021_intl_dollars", "per_1000_live_births",
                    "per_100k_live_births", "per_1000_people", "per_100k",
                    "per_100_people", "tonnes_per_capita", "ug_per_m3",
                    "years", "births_per_woman", "percent_gdp")

transform_kind <- function(indicator, unit) {
  # V-Dem's polarization score (v2cacamps) is a 0-4 ordinal scale, not 0-1.
  if (unit == "index_0_4") return("logit04")
  if (unit == "index_0_1") return("logit01")
  if (unit == "index_0_100" || indicator %in% SHARE_PERCENT ||
      unit %in% c("percent_land", "percent_manufactured_exports")) return("logit100")
  if (unit %in% POSITIVE_UNITS) return("log")
  "identity"
}
t_fwd <- function(x, kind) switch(kind,
                                  logit01  = qlogis(pmin(pmax(x, EPS01), 1 - EPS01)),
                                  logit100 = qlogis(pmin(pmax(x / 100, EPS01), 1 - EPS01)),
                                  log      = log(pmax(x, 1e-9)),
                                  x)
t_bwd <- function(z, kind) switch(kind,
                                  logit01  = plogis(z),
                                  logit100 = 100 * plogis(z),
                                  log      = exp(z),
                                  z)

# -- Final-safety value bounds (transforms should make these no-ops) ----------
unit_bounds <- function(unit) {
  switch(unit,
         "index_0_1"                    = c(0, 1),
         "index_0_4"                    = c(0, 4),
         "index_0_100"                  = c(0, 100),
         "per_1000_live_births"         = c(0, NA),
         "per_100k_live_births"         = c(0, NA),
         "per_1000_people"              = c(0, NA),
         "per_100k"                     = c(0, NA),
         "per_100_people"               = c(0, NA),
         "years"                        = c(0, NA),
         "constant_2021_intl_dollars"   = c(0, NA),
         "tonnes_per_capita"            = c(0, NA),
         "ug_per_m3"                    = c(0, NA),
         "births_per_woman"             = c(0, NA),
         "percent_gdp"                  = c(0, NA),
         "percent_land"                 = c(0, 100),
         "percent_manufactured_exports" = c(0, 100),
         c(NA, NA))
}
clamp_bounds <- function(df, indicator, unit) {
  b <- unit_bounds(unit)
  if (indicator %in% SHARE_PERCENT) b <- c(0, 100)
  lo_b <- b[1]; hi_b <- b[2]
  df |> mutate(
    value = pmin(pmax(value, if (is.na(lo_b)) -Inf else lo_b), if (is.na(hi_b)) Inf else hi_b),
    lower = pmin(pmax(lower, if (is.na(lo_b)) -Inf else lo_b), if (is.na(hi_b)) Inf else hi_b),
    upper = pmin(pmax(upper, if (is.na(lo_b)) -Inf else lo_b), if (is.na(hi_b)) Inf else hi_b)
  )
}

# -- Level 1: per-country OLS on the transform scale --------------------------
# Centered at the country's LAST observed year, so the intercept IS the current
# level (anchoring) and slope/intercept are estimated where we need them.
fit_country <- function(hist, kind) {
  hist <- hist |> arrange(year) |> filter(!is.na(value))
  if (nrow(hist) < MIN_POINTS) return(NULL)
  last_year <- max(hist$year)
  window <- hist |> filter(year > last_year - FIT_WINDOW)
  if (nrow(window) < MIN_POINTS) return(NULL)
  if (n_distinct(window$year) < MIN_POINTS) return(NULL)
  window <- window |> mutate(z = t_fwd(value, kind), h = year - last_year)
  if (sd(window$z) == 0) return(NULL)
  fit <- tryCatch(lm(z ~ h, data = window), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  sm <- tryCatch(summary(fit), error = function(e) NULL)
  if (is.null(sm) || nrow(sm$coefficients) < 2) return(NULL)
  co <- sm$coefficients
  V  <- vcov(fit)
  list(last_year = last_year,
       a = co[1, 1], a_var = V[1, 1],
       b = co[2, 1], b_se = co[2, 2], b_var = V[2, 2],
       ab_cov = V[1, 2], sigma = sm$sigma, n = nrow(window))
}

# -- Level 2: pool slopes across countries (DerSimonian-Laird) ----------------
pool_slopes <- function(fits) {
  b  <- vapply(fits, function(f) f$b,    numeric(1))
  se <- vapply(fits, function(f) f$b_se, numeric(1))
  ok <- is.finite(b) & is.finite(se) & se > 0
  b <- b[ok]; se <- se[ok]; k <- length(b)
  if (k < MIN_COUNTRIES_POOL) return(NULL)
  w     <- 1 / se^2
  mu_fe <- sum(w * b) / sum(w)
  Q     <- sum(w * (b - mu_fe)^2)
  tau2  <- max(0, (Q - (k - 1)) / (sum(w) - sum(w^2) / sum(w)))
  wr    <- 1 / (se^2 + tau2)
  mu    <- sum(wr * b) / sum(wr)
  list(mu = mu, tau2 = tau2, mu_var = 1 / sum(wr), k = k)
}

# -- Combine: project one country with (or without) pooling -------------------
project_one_ml <- function(f, pool, cap_years, kind) {
  h <- seq_len(cap_years)
  if (!is.null(pool) && is.finite(f$b_se) && f$b_se > 0) {
    # EB posterior for this country's slope
    prec_i <- 1 / f$b_se^2
    prec_p <- if (pool$tau2 > 0) 1 / pool$tau2 else Inf
    if (is.finite(prec_p)) {
      b_star   <- (f$b * prec_i + pool$mu * prec_p) / (prec_i + prec_p)
      var_star <- 1 / (prec_i + prec_p) + pool$mu_var
    } else {          # tau2 == 0: full pooling to the common mean
      b_star   <- pool$mu
      var_star <- pool$mu_var
    }
    mean_z <- f$a + b_star * h
    var_z  <- f$a_var + h^2 * var_star + f$sigma^2
  } else {
    # Fallback: exact per-country OLS prediction variance (incl. covariance)
    mean_z <- f$a + f$b * h
    var_z  <- f$a_var + h^2 * f$b_var + 2 * h * f$ab_cov + f$sigma^2
  }
  sd_z   <- sqrt(pmax(var_z, 0))
  t_crit <- qt(1 - (1 - CONF_LEVEL) / 2, df = max(f$n - 2, 1))
  tibble(
    year  = f$last_year + h,
    value = t_bwd(mean_z, kind),
    lower = t_bwd(mean_z - t_crit * sd_z, kind),
    upper = t_bwd(mean_z + t_crit * sd_z, kind)
  )
}

# -- Run across every structural indicator ------------------------------------
observed_struct <- panel |>
  filter(series_type == "observed", indicator %in% proj_spec$indicator) |>
  select(country_iso3, indicator, year, value)

results <- list()
ledger_rows <- list()
issue_date <- Sys.Date()

for (i in seq_len(nrow(proj_spec))) {
  ind  <- proj_spec$indicator[i]
  cap  <- proj_spec$horizon_cap_years[i]
  unit <- proj_spec$unit[i]
  if (is.na(cap) || cap <= 0) next
  kind <- transform_kind(ind, unit)
  
  ind_hist  <- observed_struct |> filter(indicator == ind)
  countries <- unique(ind_hist$country_iso3)
  
  # Freshness reference: the most recent observed year ANY country has for this
  # indicator. Countries lagging it by more than MAX_STALENESS are not projected.
  fresh_year <- suppressWarnings(max(ind_hist$year, na.rm = TRUE))
  cutoff <- fresh_year - MAX_STALENESS
  
  # Level 1: fit every country first
  fits <- list(); n_stale <- 0
  for (cc in countries) {
    h <- ind_hist |> filter(country_iso3 == cc)
    if (suppressWarnings(max(h$year, na.rm = TRUE)) < cutoff) { n_stale <- n_stale + 1; next }
    f <- fit_country(h, kind)
    if (!is.null(f)) fits[[cc]] <- f
  }
  if (length(fits) == 0) { log_info(sprintf("  %-28s no fittable countries", ind)); next }
  
  # Level 2: pool slopes (NULL -> per-country fallback inside project_one_ml)
  pool <- pool_slopes(fits)
  pool_tag <- if (is.null(pool)) "unpooled"
  else sprintf("pooled k=%d mu=%.4g tau=%.4g", pool$k, pool$mu, sqrt(pool$tau2))
  
  n_ok <- 0
  for (cc in names(fits)) {
    pr <- project_one_ml(fits[[cc]], pool, cap, kind)
    pr <- clamp_bounds(pr, ind, unit)   # final safety net (should be a no-op)
    n_ok <- n_ok + 1
    pr <- pr |> mutate(
      country_iso3 = cc, indicator = ind,
      series_type = "projection_structural", scenario = "none"
    )
    results[[length(results) + 1]] <- pr
    ledger_rows[[length(ledger_rows) + 1]] <- pr |>
      mutate(issue_date = issue_date,
             horizon = year - fits[[cc]]$last_year,
             realized = NA_real_) |>
      select(issue_date, country_iso3, indicator, year, horizon,
             value, lower, upper, realized)
  }
  log_info(sprintf("  %-28s cap=%dy  %s  projected %d/%d countries (%d stale, fresh=%d) [%s]",
                   ind, cap, kind, n_ok, length(countries), n_stale, fresh_year, pool_tag))
}

projections <- bind_rows(results) |> as_canonical()
log_info("Total structural projection rows: ", format(nrow(projections), big.mark = ","))

# -- Write projections + a unified full panel ---------------------------------
proj_path <- file.path(PATHS$output, "projections.csv")
write_csv(projections, proj_path)

panel_full <- bind_rows(panel |> as_canonical(), projections) |>
  distinct(country_iso3, year, indicator, series_type, scenario, .keep_all = TRUE)
full_path <- file.path(PATHS$output, "panel_full.csv")
write_csv(panel_full, full_path)
log_info("Wrote ", full_path, " (", format(nrow(panel_full), big.mark = ","), " rows)")
log_info("series_type distribution:")
print(panel_full |> count(series_type))

# -- Coupled-model specification ----------------------------------------------
# THE CANONICAL COUPLED-MODEL DEFINITION lives here, with the rest of the
# modelling. R owns it; 03_Dash.R serializes it and the dashboard's JS only
# EVALUATES the named functional forms it describes — no model logic is written
# in JS. To change the model (a coefficient, a disputed range, a citation, or a
# whole edge) edit this function and rerun.
#
# v2 changes:
#   - bhm_quadratic is now SIGNED (colder-than-optimum countries GAIN from
#     marginal warming — BHM 2015's own published pattern; the old max(0,·)
#     clip silenced the gain side and made the slider look dead for temperate
#     countries).
#   - Every parameter carries `se`: its disputed literature range read as a
#     95% interval (se = (max-min)/4). The dashboard runs a joint Monte-Carlo
#     over coefficient draws x CMIP6 model spread, addressing "coefficient CIs
#     not propagated". This is stated as-is: it is range-based uncertainty, not
#     the papers' own standard errors.
#   - New baseline parameters gFrontier + tauConv: GDP baseline growth
#     converges from each country's recent observed rate to a long-run
#     frontier rate. Constant-growth extrapolation to 2100 is indefensible
#     (Pritchett & Summers 2014 "regression to the mean"; SSP GDP pathways,
#     Dellink et al. 2017 use the same convergence logic).
#   - Heat-vulnerability income scaling now follows the model's OWN coupled
#     income path (adaptation channel), instead of freezing at the last
#     observed GDP.
build_coupling_spec <- function() {
  rng_se <- function(mn, mx) (mx - mn) / 4   # disputed range read as ±2σ
  params <- list(
    Topt = list(name = "GDP optimum temperature", unit = "\u00b0C",
                default = 13.0, min = 13.0, max = 16.0, step = 0.1,
                se = rng_se(13.0, 16.0),
                cite = "Burke, Hsiang & Miguel 2015 (Nature 527:235); Kotz et al. ~15.8\u00b0C"),
    bhmCurv = list(name = "GDP\u2013temp curvature", unit = "/\u00b0C\u00b2",
                   default = 0.0005, min = 0.0, max = 0.0010, step = 0.00005,
                   se = rng_se(0.0, 0.0010),
                   cite = "BHM 2015 quadratic term, SIGNED: colder-than-optimum countries gain from marginal warming (BHM's own pattern); 0 = Barker 2024 (Econ Journal Watch) null"),
    bhmGainCap = list(name = "Cap on climate growth GAIN", unit = "/yr",
                      default = 0.02, min = 0.0, max = 0.05, step = 0.005,
                      mc = FALSE,   # structural bound, not an uncertain coefficient
                      cite = "Bounds the ANNUAL growth BONUS a colder-than-optimum country may receive. Damages are deliberately left uncapped (they self-limit; capping them broke the India -87% validation anchor). Calibrated so BHM's published country pattern is reproduced: at 0.02 we get India -87% / Nigeria -90% / Russia-Canada +265% against BHM's -87% / -92% / +419%,+247%."),
    bhmGainShare = list(name = "BHM cold-country gain", unit = "\u00d7",
                        default = 1.0, min = 0.0, max = 1.0, step = 0.05,
                        mc = FALSE,   # structural SWITCH, not an uncertain coefficient: never sampled
                        cite = "How much of BHM's GAIN side to apply (0 = damages only, 1 = full signed quadratic). BHM's raw quadratic gives countries far below the optimum a permanent multi-point annual growth bonus (Canada/Russia -> 800-1800x income by 2100), which is the literal form of the 'climate determinism' critique (Barker 2024). Default 1 (gains on, bounded by bhmGainCap). Setting it to 0 gives a damages-only world, which zeroes every country whose baseline sits below the optimum."),
    prestonSlope = list(name = "Preston curve slope", unit = "yr/ln$",
                        default = 5.0, min = 3.0, max = 7.0, step = 0.1,
                        se = rng_se(3.0, 7.0),
                        cite = "Preston 1975; replications give ~3\u20137 yr life-exp per ln(GDP/cap). Applied to WITHIN-country income changes \u2014 a structural reading of a cross-sectional curve (Preston's own decomposition attributes most secular LE gain to curve shifts, not movement along it)."),
    carletonMort = list(name = "Direct heat mortality", unit = "yr/\u00b0C",
                        default = 0.20, min = 0.0, max = 0.60, step = 0.01,
                        se = rng_se(0.0, 0.60),
                        cite = "Carleton et al. 2022 (QJE). Coefficient is the effect AT the $40k reference income; the income scaler moves it between 0.4x (rich) and 3x (poor) along the model's own coupled income path."),
    fertIncome = list(name = "Fertility\u2013income response", unit = "births/ln$",
                      default = -0.20, min = -0.40, max = 0.0, step = 0.01,
                      se = rng_se(-0.40, 0.0),
                      cite = "Demographic transition: TFR falls with income (Herzer et al. 2012)"),
    mortPopFeedback = list(name = "Heat-mortality \u2192 population", unit = "frac/yr/\u00b0C",
                           default = 0.0008, min = 0.0, max = 0.0030, step = 0.0001,
                           se = rng_se(0.0, 0.0030),
                           cite = "Carleton et al. 2022 mortality response mapped to population loss; structural. Default \u00d7 income scale \u00d7 warming sits at/above Carleton's high-adaptation-gap range \u2014 read the upper half of this slider as 'limited adaptation'."),
    gFrontier = list(name = "Long-run frontier growth", unit = "/yr",
                     default = 0.015, min = 0.005, max = 0.025, step = 0.001,
                     se = rng_se(0.005, 0.025),
                     cite = "Baseline (not a coupling): per-capita growth every country converges toward. SSP2 GDP logic (Dellink et al. 2017); Pritchett & Summers 2014."),
    tauConv = list(name = "Growth convergence timescale", unit = "yr",
                   default = 40, min = 15, max = 80, step = 5,
                   se = rng_se(15, 80),
                   cite = "Baseline (not a coupling): e-folding time for recent national growth to fade toward the frontier rate (Pritchett & Summers 2014 regression to the mean).")
  )
  edges <- list(
    list(id = "temp_to_gdp", from = "mean_temperature", to = "gdp_per_capita_ppp",
         form = "bhm_quadratic", uses = c("Topt", "bhmCurv"), kind = "empirical",
         desc = "Quadratic growth effect of warming relative to each country's baseline climate (BHM 2015). Damages (hotter-than-optimum countries) always apply; the GAIN side for colder-than-optimum countries is gated by bhmGainShare (default 0) and capped, because the raw quadratic compounds to implausible 2100 income levels for cold countries.",
         cite = "Burke, Hsiang & Miguel 2015, Nature 527:235",
         contested = "Country magnitudes large & contested (Barker 2024; 'climate determinism'). Curvature\u21920 reproduces the null."),
    list(id = "gdp_to_le", from = "gdp_per_capita_ppp", to = "life_expectancy",
         form = "preston_log", uses = c("prestonSlope"), kind = "empirical",
         desc = "Life expectancy rises with log income (Preston curve), anchored to observed LE.",
         cite = "Preston 1975",
         contested = "Cross-sectional curve applied to within-country change; Preston's decomposition says most secular LE gain comes from curve shifts (technology), so this edge bounds the income channel only."),
    list(id = "temp_to_le", from = "mean_temperature", to = "life_expectancy",
         form = "linear_marginal_scaled", uses = c("carletonMort"), kind = "empirical",
         desc = "Direct heat-mortality effect on life expectancy, scaled by the model's own coupled income path so impoverishment raises vulnerability and growth lowers it (adaptation).",
         cite = "Carleton et al. 2022, QJE (income-adjusted; national approximation)", contested = NA),
    list(id = "gdp_to_pop", from = "gdp_per_capita_ppp", to = "population_total",
         form = "fertility_income", uses = c("fertIncome"), kind = "empirical",
         desc = "Income change shifts fertility (demographic transition): rising GDP lowers fertility, bending population vs the UN baseline.",
         cite = "Demographic transition (Herzer, Strulik & Vollmer 2012)",
         contested = "Direction robust; magnitude varies by development stage."),
    list(id = "temp_to_pop", from = "mean_temperature", to = "population_total",
         form = "heat_mortality_pop", uses = c("mortPopFeedback"), kind = "structural",
         desc = "Heat mortality removes population at an annual fraction rising with warming beyond baseline, income-scaled along the coupled income path.",
         cite = "Derived from Carleton et al. 2022; structural population mapping",
         contested = "Structural choice, not a directly fitted population coefficient.")
  )
  notes <- list(
    driver = "Driven by mean_temperature (CMIP6 tas), never max_temperature \u2014 the BHM optimum is defined on annual-mean temperature.",
    uncertainty = "Bands are a joint Monte-Carlo (32 stratified draws): each coefficient's disputed range read as a 95% interval, crossed with the CMIP6 inter-model spread, propagated through the whole chain. Structural switches (mc = FALSE) are held at their set value, not sampled. Range-based coefficient uncertainty, not the papers' own SEs \u2014 stated, not hidden.",
    baseline = "GDP baseline growth converges from each country's recent observed rate to the frontier rate over ~tauConv years; the naive line uses the same baseline with all couplings zeroed, so the naive-vs-coupled gap isolates the coupling.",
    provenance = "Coefficients encoded from published headline values \u2014 verify against source before citing."
  )
  list(params = params, edges = edges, notes = notes, version = as.character(Sys.Date()))
}
coupling_spec <- build_coupling_spec()
spec_path <- file.path(PATHS$output, "coupling_spec.json")
writeLines(jsonlite::toJSON(coupling_spec, auto_unbox = TRUE, pretty = TRUE, null = "null"), spec_path)
log_info("Wrote coupled-model spec (", length(coupling_spec$edges), " edges) -> ", spec_path)

# -- Append to the calibration ledger -----------------------------------------
# Append-only: each run adds its forecasts so you build a track record over
# time. Later, a scoring pass fills `realized` from new observed data and
# computes whether your 95% intervals actually covered ~95% of outcomes. If
# they didn't, the bands are too narrow and should be widened — this is the
# single thing that keeps the engine honest rather than confidently wrong.
today_issue <- issue_date
ledger_new <- bind_rows(ledger_rows)
ledger_path <- file.path(PATHS$calibration, "ledger.csv")
if (file.exists(ledger_path)) {
  existing <- read_csv(ledger_path, show_col_types = FALSE)
  # Don't double-log today's issue_date if the script is re-run same day.
  existing <- existing |> filter(as.Date(issue_date) != today_issue)
  ledger_all <- bind_rows(existing, ledger_new)
} else {
  ledger_all <- ledger_new
}
write_csv(ledger_all, ledger_path)
log_info("Calibration ledger: +", format(nrow(ledger_new), big.mark = ","),
         " forecasts logged -> ", ledger_path)
log_info("Done. The honest forecastable layer is built; bands widen with horizon,")
log_info("nothing reaches past its cap, and every forecast is logged for scoring.")




library(readr); library(dplyr)
pf <- read_csv("data/output/panel_full.csv", show_col_types = FALSE)

# 1. India population sanity (should be ~1.43B observed 2023, ~1.45B projected 2024+)
cat("=== India population ===\n")
pf |> filter(country_iso3=="IND", indicator=="population_total", year %in% 2023:2026) |>
  select(year, series_type, value, lower, upper) |> arrange(year) |> print()

# 2. A structural projection — confirm the band WIDENS with horizon (the whole point)
cat("\n=== USA life expectancy projection (band should widen each year) ===\n")
pf |> filter(country_iso3=="USA", indicator=="life_expectancy",
             series_type=="projection_structural") |>
  mutate(band_width = upper - lower) |>
  select(year, value, lower, upper, band_width) |> arrange(year) |> print()