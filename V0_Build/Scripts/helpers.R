# R/helpers.R
# -----------------------------------------------------------------------------
# Shared functions used by the numbered scripts.
# Sourced at the top of each script with: source("Scripts/helpers.R")
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(httr2)
  library(jsonlite)
})

# Paths --------------------------------------------------------------------
#
# Flat directory layout. Raw per-indicator caches all live in data/raw/ with
# a source prefix (wb_<indicator>.csv, owid_<slug>.csv) so a single ls of
# that directory tells you everything we've fetched. No nested source folders.

PATHS <- list(
  raw    = "data/raw",
  output = "data/output",
  config = "config"
)

ensure_dirs <- function() {
  for (p in PATHS) dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

# Filename helpers for the raw cache. Centralised here so fetch and combine
# agree on the scheme.
raw_path_wb   <- function(indicator_name) file.path(PATHS$raw, paste0("wb_", indicator_name, ".csv"))
raw_path_owid <- function(slug)           file.path(PATHS$raw, paste0("owid_", slug, ".csv"))
raw_path_country_list <- function()       file.path(PATHS$raw, "_wb_countries.csv")


# Config loader ------------------------------------------------------------
#
# Single source of truth: config/indicators.csv. Each row defines one indicator
# end-to-end: where it comes from, what module it belongs to, which direction
# is "better", a human-readable description, and its weight inside its module.
#
# Weights are renormalised per module so they sum to 1, so the raw weights in
# the CSV can be expressed on any convenient scale.

read_indicators <- function() {
  path <- file.path(PATHS$config, "indicators.csv")
  raw <- read_csv(path, show_col_types = FALSE)
  
  required_cols <- c("indicator", "module", "source", "source_id",
                     "unit", "direction", "description", "weight")
  missing <- setdiff(required_cols, names(raw))
  if (length(missing) > 0) {
    stop(sprintf("indicators.csv is missing required column(s): %s",
                 paste(missing, collapse = ", ")))
  }
  
  raw |>
    mutate(
      across(where(is.character), str_trim),
      weight = suppressWarnings(as.numeric(weight))
    ) |>
    mutate(
      direction = if_else(direction %in% c("higher_is_better", "lower_is_better"),
                          direction, NA_character_),
      source    = if_else(source %in% c("worldbank", "owid"),
                          source, NA_character_)
    ) |>
    filter(!is.na(direction), !is.na(source),
           !is.na(weight), weight > 0) |>
    group_by(module) |>
    mutate(weight = weight / sum(weight)) |>
    ungroup()
}


# Logging helper -----------------------------------------------------------

log_info <- function(...) {
  msg <- paste0(...)
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))
}


# Normalization ------------------------------------------------------------
#
# Two normalizations are added to every row of the indicator panel:
#   pct_global  per-year cross-country percentile rank, 0..100
#               (relative: where does this country rank THIS year?)
#   pct_fixed   global min-max across the whole panel, 0..100
#               (absolute: where does this country sit on the all-time scale?)
#
# Direction is applied: lower_is_better indicators are inverted so that
# higher always means "better" in the normalized columns. The raw `value`
# column is preserved.
#
# Expects the panel to have a `direction` column. The processing script joins
# it on from the indicators registry just before calling this. We don't store
# `direction` in the output panel — it's a property of the indicator, not the
# observation, and is one join away in the registry.

normalize_indicators <- function(panel) {
  stopifnot(all(c("indicator", "country_iso3", "year", "value", "direction") %in% names(panel)))
  
  panel |>
    filter(!is.na(value), is.finite(value)) |>
    group_by(indicator, year) |>
    mutate(pct_global = rank(value, na.last = "keep", ties.method = "average") /
             sum(!is.na(value)) * 100) |>
    group_by(indicator) |>
    mutate(
      .vmin = min(value, na.rm = TRUE),
      .vmax = max(value, na.rm = TRUE),
      pct_fixed = if_else(.vmax > .vmin,
                          (value - .vmin) / (.vmax - .vmin) * 100,
                          NA_real_)
    ) |>
    ungroup() |>
    mutate(
      pct_global = if_else(direction == "lower_is_better", 100 - pct_global, pct_global),
      pct_fixed  = if_else(direction == "lower_is_better", 100 - pct_fixed,  pct_fixed),
      pct_global = pmin(pmax(pct_global, 0), 100),
      pct_fixed  = pmin(pmax(pct_fixed,  0), 100)
    ) |>
    select(-.vmin, -.vmax, -direction)
}


# Scoring ------------------------------------------------------------------
#
# Combine normalized indicators into module scores.
# Coverage = how many of the module's expected indicators were available.
# A module score from 6/8 indicators is less trustworthy than from 8/8.
#
# The panel doesn't carry `module` or `weight` — they're properties of the
# indicator, joined on from the registry inside this function. `weights`
# defaults to the registry; supply a custom (module, indicator, weight)
# tibble only for what-if analyses.

compute_scores <- function(normalized, weights = NULL) {
  if (is.null(weights)) {
    weights <- read_indicators() |> select(module, indicator, weight)
  }
  
  # Join weights onto the panel; this also brings in the module assignment.
  scored <- normalized |>
    inner_join(weights, by = "indicator")
  
  # Per (country, year, module): weighted average, with weights renormalized
  # over indicators that are actually present.
  scores <- scored |>
    group_by(country_iso3, year, module) |>
    summarise(
      score_pct_global   = sum(pct_global * weight) / sum(weight),
      score_pct_fixed    = sum(pct_fixed  * weight, na.rm = TRUE) /
        sum(weight[!is.na(pct_fixed)]),
      contributing_count = n(),
      .groups = "drop"
    )
  
  expected <- weights |>
    count(module, name = "expected_count")
  
  scores |>
    left_join(expected, by = "module") |>
    mutate(coverage = contributing_count / expected_count)
}


# Trajectories -------------------------------------------------------------
#
# For each (country, module), summarise the latest position:
#   level         most recent score_pct_fixed
#   trend_5y      OLS slope over last 5 years (pct_fixed points / year)
#   trend_10y     same, last 10 years
#   volatility_5y std dev of score_pct_fixed over last 5 years
#   flag          improving / deteriorating / stable / volatile / insufficient_data
#
# Trends run on pct_fixed (absolute scale), not pct_global, because slope on
# a percentile would just measure other countries moving.

SLOPE_IMPROVING <- 1.0      # pct_fixed points / year
SLOPE_DETERIORATING <- -1.0
VOLATILITY_HIGH <- 5.0      # std dev of pct_fixed over 5y

ols_slope <- function(years, values) {
  ok <- !is.na(years) & !is.na(values)
  years <- years[ok]; values <- values[ok]
  if (length(years) < 2 || var(years) == 0) return(NA_real_)
  unname(coef(lm(values ~ years))[2])
}

classify_trajectory <- function(slope, vol, n_years) {
  # Trend takes precedence over volatility — a steady linear trend has high
  # std dev by construction, but isn't volatile, it's trending.
  if (n_years < 3 || is.na(slope)) return("insufficient_data")
  if (slope >=  SLOPE_IMPROVING)     return("improving")
  if (slope <=  SLOPE_DETERIORATING) return("deteriorating")
  if (!is.na(vol) && vol > VOLATILITY_HIGH) return("volatile")
  "stable"
}

compute_trajectories <- function(scores) {
  scores |>
    group_by(country_iso3, module) |>
    arrange(year, .by_group = TRUE) |>
    summarise(
      latest_year   = max(year),
      level         = last(score_pct_fixed),
      trend_5y      = ols_slope(year[year >  latest_year - 5],
                                score_pct_fixed[year > latest_year - 5]),
      trend_10y     = ols_slope(year[year > latest_year - 10],
                                score_pct_fixed[year > latest_year - 10]),
      volatility_5y = sd(score_pct_fixed[year > latest_year - 5], na.rm = TRUE),
      n_years_5y    = sum(year >  latest_year - 5),
      n_years_10y   = sum(year > latest_year - 10),
      .groups = "drop"
    ) |>
    rowwise() |>
    mutate(flag = classify_trajectory(trend_5y, volatility_5y, n_years_5y)) |>
    ungroup()
}


# ISO3 helpers -------------------------------------------------------------

is_iso3 <- function(x) {
  !is.na(x) & nchar(x) == 3 & str_detect(x, "^[A-Z]{3}$")
}


# Quality check ------------------------------------------------------------
#
# Sanity checks on the three output tables. Each check either PASSES, WARNS,
# or FAILS. If any FAIL, fix it before trusting downstream analysis.
# Writes a small text report to data/output/quality_report.txt.

run_quality_check <- function(panel, scores, trajectories, indicators) {
  report_path <- file.path(PATHS$output, "quality_report.txt")
  expected_indicators <- indicators$indicator
  
  results <- list()
  add <- function(name, ok, severity, detail) {
    results[[length(results) + 1]] <<-
      list(name = name, ok = ok, severity = severity, detail = detail)
  }
  
  add("iso3_format",
      all(is_iso3(panel$country_iso3)), "error",
      sprintf("%d malformed ISO3 codes",
              sum(!is_iso3(panel$country_iso3))))
  
  dupes <- panel |> count(country_iso3, year, indicator) |> filter(n > 1) |> nrow()
  add("no_duplicates", dupes == 0, "error",
      sprintf("%d duplicate (country, year, indicator) rows", dupes))
  
  bad_year <- sum(panel$year < 1990 | panel$year > 2030, na.rm = TRUE)
  add("year_in_range", bad_year == 0, "error",
      sprintf("%d rows outside [1990, 2030]", bad_year))
  
  bad_val <- sum(!is.finite(panel$value), na.rm = TRUE)
  add("value_finite", bad_val == 0, "error",
      sprintf("%d non-finite values", bad_val))
  
  bad_g <- sum(panel$pct_global < 0 | panel$pct_global > 100, na.rm = TRUE)
  bad_f <- sum(panel$pct_fixed  < 0 | panel$pct_fixed  > 100, na.rm = TRUE)
  add("normalization_bounds", (bad_g + bad_f) == 0, "error",
      sprintf("%d rows outside [0, 100]", bad_g + bad_f))
  
  missing_inds <- setdiff(expected_indicators, unique(panel$indicator))
  add("indicator_coverage", length(missing_inds) == 0, "warning",
      sprintf("%d registered indicators have zero rows: %s",
              length(missing_inds),
              paste(head(missing_inds, 10), collapse = ", ")))
  
  n_countries <- n_distinct(panel$country_iso3)
  add("country_coverage", n_countries >= 50,
      if (n_countries >= 25) "warning" else "error",
      sprintf("%d unique countries", n_countries))
  
  n_modules <- n_distinct(scores$module)
  add("score_table_present", n_modules >= 3, "error",
      sprintf("%d score rows across %d modules", nrow(scores), n_modules))
  
  # Print to console and collect for the report file
  log_info("Quality check results:")
  report_lines <- character()
  for (r in results) {
    marker <- if (r$ok) "PASS" else if (r$severity == "warning") "WARN" else "FAIL"
    line <- sprintf("  [%s] %-25s %s", marker, r$name, r$detail)
    cat(line, "\n")
    report_lines <- c(report_lines, line)
  }
  
  # Join the source from the registry so the per-source row count still works
  panel_with_source <- panel |>
    left_join(indicators |> select(indicator, source), by = "indicator")
  
  report_lines <- c(
    sprintf("Life Projection — Quality Report (%s)", format(Sys.time(), "%Y-%m-%d %H:%M")),
    strrep("-", 60),
    report_lines,
    "",
    "Rows per source:",
    capture.output(print(panel_with_source |> count(source))),
    "",
    "Module coverage:",
    capture.output(print(scores |> group_by(module) |>
                           summarise(country_years = n(),
                                     countries     = n_distinct(country_iso3),
                                     avg_coverage  = round(mean(coverage), 2),
                                     latest_year   = max(year))))
  )
  writeLines(report_lines, report_path)
  log_info("Saved quality report to ", report_path)
  invisible(results)
}