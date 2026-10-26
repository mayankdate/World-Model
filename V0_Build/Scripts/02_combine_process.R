# ----------------------------------------------------------------------------- #
#  02 COMBINE AND PROCESS RAW DATA  -------------------
# ----------------------------------------------------------------------------- #
# Reads the per-indicator raw CSVs cached by 01_fetch_data.R, combines them
# into a single long panel, normalises every value, computes module scores
# and trajectories, and runs a quality check.
#
# Outputs (all under data/output/):
#   country_year_indicators.csv   long panel: country_iso3, year, indicator,
#                                 value, pct_global, pct_fixed
#                                 (metadata like module/unit/direction lives
#                                 in config/indicators.csv; join when needed)
#   country_year_scores.csv       module composites per country-year
#   country_trajectories.csv      one row per (country, module): level + trend + flag
#   quality_report.txt            text summary of sanity checks
# ----------------------------------------------------------------------------- #

source("Scripts/helpers.R")
ensure_dirs()

indicators <- read_indicators()
log_info("Processing ", nrow(indicators), " indicators from the registry.")


# ----------------------------------------------------------------------------- #
#  1. COMBINE RAW FILES INTO ONE LONG PANEL  -------------------
# ----------------------------------------------------------------------------- #
# Two source-specific readers, because the on-disk shapes differ. Both return
# the same minimal schema: country_iso3, year, indicator, value.

read_worldbank <- function() {
  wb <- indicators |> filter(source == "worldbank")
  
  rows <- map(wb$indicator, function(name) {
    path <- raw_path_wb(name)
    if (!file.exists(path)) return(NULL)
    df <- read_csv(path, show_col_types = FALSE)
    if (nrow(df) == 0) return(NULL)
    df |> mutate(indicator = name)
  })
  
  out <- bind_rows(rows)
  log_info("  World Bank: ", format(nrow(out), big.mark = ","), " rows")
  out
}

read_owid <- function() {
  ow <- indicators |> filter(source == "owid")
  
  rows <- map2(ow$indicator, ow$source_id, function(name, slug) {
    path <- raw_path_owid(slug)
    if (!file.exists(path)) return(NULL)
    df <- read_csv(path, show_col_types = FALSE)
    if (!all(c("Entity", "Code", "Year") %in% names(df))) return(NULL)
    
    # The metric column is whatever isn't Entity/Code/Year
    metric_cols <- setdiff(names(df), c("Entity", "Code", "Year"))
    if (length(metric_cols) == 0) return(NULL)
    metric <- metric_cols[1]
    
    df |>
      transmute(
        country_iso3 = Code,
        year         = as.integer(Year),
        value        = as.numeric(.data[[metric]]),
        indicator    = name
      ) |>
      filter(is_iso3(country_iso3), !is.na(year), !is.na(value))
  })
  
  out <- bind_rows(rows)
  log_info("  OWID: ", format(nrow(out), big.mark = ","), " rows")
  out
}

panel <- bind_rows(read_worldbank(), read_owid()) |>
  filter(is_iso3(country_iso3)) |>
  distinct(country_iso3, year, indicator, .keep_all = TRUE)

log_info(sprintf("Combined panel: %s rows, %d indicators, %d countries.",
                 format(nrow(panel), big.mark = ","),
                 n_distinct(panel$indicator),
                 n_distinct(panel$country_iso3)))


# ----------------------------------------------------------------------------- #
#  2. NORMALISE  -------------------
# ----------------------------------------------------------------------------- #
# Adds pct_global (per-year percentile rank) and pct_fixed (global min-max).
# Direction is applied here: lower_is_better indicators are inverted so that
# higher always means "better" in the normalised columns.
#
# Direction is joined on transiently — it's a property of the indicator, not
# the observation, and is dropped again before the panel is written to disk.

normalized <- panel |>
  inner_join(indicators |> select(indicator, direction), by = "indicator") |>
  normalize_indicators()

panel_path <- file.path(PATHS$output, "country_year_indicators.csv")
write_csv(normalized, panel_path)
log_info("Wrote: ", panel_path)


# ----------------------------------------------------------------------------- #
#  3. SCORE  -------------------
# ----------------------------------------------------------------------------- #
# Weighted average of indicator scores within each module. compute_scores()
# pulls weights from the registry by default.

log_info("Computing scores for ", n_distinct(indicators$module), " modules.")
scores <- compute_scores(normalized)

scores_path <- file.path(PATHS$output, "country_year_scores.csv")
write_csv(scores, scores_path)
log_info("Wrote: ", scores_path)


# ----------------------------------------------------------------------------- #
#  4. TRAJECTORIES  -------------------
# ----------------------------------------------------------------------------- #
# For each (country, module): latest level, 5- and 10-year OLS slope of
# score_pct_fixed, volatility, and a flag (improving/stable/etc).

log_info("Computing trajectories for ",
         n_distinct(scores$country_iso3), " countries x ",
         n_distinct(scores$module), " modules.")

trajectories <- compute_trajectories(scores)

trajectories_path <- file.path(PATHS$output, "country_trajectories.csv")
write_csv(trajectories, trajectories_path)
log_info("Wrote: ", trajectories_path)

log_info("Trajectory flag distribution:")
print(trajectories |> count(flag, sort = TRUE))


# ----------------------------------------------------------------------------- #
#  5. QUALITY CHECK  -------------------
# ----------------------------------------------------------------------------- #

run_quality_check(panel = normalized,
                  scores = scores,
                  trajectories = trajectories,
                  indicators = indicators)

log_info("Pipeline complete.")