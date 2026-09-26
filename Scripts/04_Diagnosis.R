# Scripts/04_diagnose.R
# =============================================================================
# READ-ONLY diagnostic. Writes nothing, changes nothing. Answers the three
# questions the printed dashboard raised, from the actual cached data:
#
#   Q1  Do the three SSP scenarios actually contain different numbers?
#   Q2  Where does each indicator's data start/end, and where are the gaps?
#   Q3  Do the WPP raw caches cover 1990+, or only recent years?
#
#   source("Scripts/04_diagnose.R")
#
# Paste the output back and the remaining bugs become locatable instead of
# guessable.
# =============================================================================

source("Scripts/00_helpers.R")
indicators <- read_indicators()

hdr <- function(x) cat("\n", strrep("=", 72), "\n", x, "\n", strrep("=", 72), "\n", sep = "")

# -- Q1: are the scenarios actually different? --------------------------------
# If these three numbers are identical, the CCKP fetch returned the same product
# for all three SSPs and the scenario toggle is cosmetic at the DATA level.
hdr("Q1  CCKP scenario differentiation (raw cache)")
clim <- indicators |> filter(source == "cckp")
for (i in seq_len(nrow(clim))) {
  var <- clim$source_id[i]
  cat("\n", clim$indicator[i], " (", var, ")\n", sep = "")
  for (scen in c("ssp126", "ssp245", "ssp585")) {
    p <- raw_path_cckp(var, scen)
    if (!file.exists(p)) { cat(sprintf("  %-8s MISSING FILE %s\n", scen, p)); next }
    d <- read_csv(p, show_col_types = FALSE)
    usa <- d |> filter(iso3 == "USA")
    cat(sprintf("  %-8s rows=%-6d years %s-%s   USA 2050=%s  USA 2100=%s\n",
                scen, nrow(d), min(d$year), max(d$year),
                format(round(usa$value[usa$year == 2050], 2), nsmall = 2),
                format(round(usa$value[usa$year == 2100], 2), nsmall = 2)))
  }
}
cat("\n>> If USA 2100 is the SAME across the three rows above, the fetch is the bug.\n")
cat(">> If it DIFFERS, the data is fine and the problem was the auto-fitting y-axis.\n")

# -- Q3: WPP raw cache coverage ----------------------------------------------
# The dashboard's population line appears to start ~2015 rather than 1990.
# WPP_START_YEAR is 1990, but REFRESH_DEMOGRAPHIC=FALSE means a cache fetched
# under an older start year is never refreshed.
hdr("Q3  UN WPP raw cache year coverage")
demo <- indicators |> filter(source == "unwpp")
for (nm in demo$indicator) {
  p <- raw_path_wpp(nm)
  if (!file.exists(p)) { cat(sprintf("  %-24s MISSING FILE\n", nm)); next }
  d <- read_csv(p, show_col_types = FALSE)
  med <- d |> filter(tolower(variant) == "median")
  cat(sprintf("  %-24s rows=%-8d years %d-%d  (median-variant years %d-%d, %d countries)\n",
              nm, nrow(d), min(d$year), max(d$year),
              min(med$year), max(med$year), n_distinct(med$iso3)))
}
cat("\n>> If these start at 2015-ish rather than 1990, delete data/raw/wpp_*.csv,\n")
cat(">> set REFRESH_DEMOGRAPHIC <- TRUE in 01_fetch_data.R, and re-fetch.\n")

# -- Q2: per-indicator coverage and gaps in the built panel -------------------
hdr("Q2  panel_full.csv coverage, per indicator (dashboard countries only)")
pf <- read_csv(file.path(PATHS$output, "panel_full.csv"), show_col_types = FALSE)
DASH <- c("ARG","AUS","BRA","CAN","CHN","FRA","DEU","IND","IDN","ITA","JPN","MEX",
          "RUS","SAU","ZAF","KOR","TUR","GBR","USA","NGA","EGY","IRN","VNM","POL",
          "ESP","NLD","PAK","BGD","ETH","THA")
pf <- pf |> filter(country_iso3 %in% DASH)

summ <- pf |>
  group_by(indicator, series_type) |>
  summarise(y0 = min(year), y1 = max(year), n_cty = n_distinct(country_iso3),
            .groups = "drop") |>
  tidyr::pivot_wider(names_from = series_type,
                     values_from = c(y0, y1, n_cty), names_sep = "_")
print(as.data.frame(summ), row.names = FALSE)

cat("\n-- Series that START LATE (first observed year > 1995) --\n")
late <- pf |> filter(series_type == "observed") |>
  group_by(indicator) |> summarise(first_obs = min(year), .groups = "drop") |>
  filter(first_obs > 1995) |> arrange(desc(first_obs))
if (nrow(late) == 0) cat("  none\n") else print(as.data.frame(late), row.names = FALSE)

cat("\n-- Series with INTERIOR GAPS (missing years inside their own range) --\n")
cat("   (a few is normal for survey-based indicators like Gini; many is a bug)\n")
gaps <- pf |> filter(series_type == "observed") |>
  group_by(indicator, country_iso3) |>
  summarise(span = max(year) - min(year) + 1, have = n(), .groups = "drop") |>
  mutate(missing = span - have) |>
  group_by(indicator) |>
  summarise(worst_missing = max(missing),
            median_missing = median(missing),
            n_series = n(), .groups = "drop") |>
  filter(worst_missing > 0) |> arrange(desc(median_missing))
if (nrow(gaps) == 0) cat("  none\n") else print(as.data.frame(gaps), row.names = FALSE)

cat("\n-- OBSERVED-to-PROJECTION handoff (should be contiguous: proj starts obs+1) --\n")
hand <- pf |>
  filter(series_type %in% c("observed", "projection_structural")) |>
  group_by(indicator, country_iso3, series_type) |>
  summarise(y0 = min(year), y1 = max(year), .groups = "drop") |>
  tidyr::pivot_wider(names_from = series_type, values_from = c(y0, y1)) |>
  filter(!is.na(y0_projection_structural)) |>
  mutate(gap = y0_projection_structural - y1_observed - 1) |>
  group_by(indicator) |>
  summarise(n = n(), bad_handoff = sum(gap != 0, na.rm = TRUE),
            worst_gap = max(gap, na.rm = TRUE), .groups = "drop") |>
  filter(bad_handoff > 0)
if (nrow(hand) == 0) cat("  all handoffs contiguous\n") else print(as.data.frame(hand), row.names = FALSE)

cat("\n-- Indicators in the registry with NO rows in the panel at all --\n")
missing_ind <- setdiff(indicators$indicator, unique(pf$indicator))
if (length(missing_ind) == 0) cat("  none\n") else cat(" ", paste(missing_ind, collapse = ", "), "\n")

hdr("done")