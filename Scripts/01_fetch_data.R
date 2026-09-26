# R/01_fetch.R
# =============================================================================
# FETCH — everything that touches the outside world, in one place.
#
# Four source families, each in its own clearly-marked section below. They
# share one skeleton (cache-check -> get_json with retry -> parse -> write one
# CSV per thing under data/raw/) and differ only in per-source parsing. Each
# section is independent: a dead endpoint logs and moves on, it never aborts
# the run.
#
#   source("R/helpers.R") first — it defines the schema contract, paths,
#   get_json(), the config loader, and is shared with 02_process.R.
#
# Set the REFRESH flags below to force re-download; otherwise cached raw files
# are reused so the rest of the pipeline is cheap to iterate on.
# =============================================================================

source("Scripts/00_helpers.R")
ensure_dirs()

REFRESH_STRUCTURAL  <- FALSE
REFRESH_DEMOGRAPHIC <- FALSE
REFRESH_CLIMATE     <- FALSE
REFRESH_PULSE       <- TRUE    # pulse is time-sensitive; refresh by default

START_YEAR <- 1970             # wide history helps the structural OLS later
END_YEAR   <- as.integer(format(Sys.Date(), "%Y"))

indicators <- read_indicators()


# =============================================================================
# SECTION 1 — STRUCTURAL HISTORY  (World Bank + OWID/V-Dem)  -> observed rows
# =============================================================================

WB_API <- "https://api.worldbank.org/v2"

# Canonical country list — drops aggregates like WLD, EUU (region.id == "NA").
fetch_country_list <- function() {
  path <- raw_path_country()
  if (file.exists(path) && !REFRESH_STRUCTURAL) return(read_csv(path, show_col_types = FALSE))
  payload <- get_json(sprintf("%s/country?format=json&per_page=400", WB_API))
  if (is.null(payload) || length(payload) < 2) stop("WB /country failed")
  rows <- payload[[2]]
  df <- tibble(
    country_iso3 = map_chr(rows, ~ .x$id %||% NA_character_),
    name         = map_chr(rows, ~ .x$name %||% NA_character_),
    region_id    = map_chr(rows, ~ .x$region$id %||% NA_character_)
  ) |> mutate(is_real_country = !is.na(region_id) & region_id != "NA")
  write_csv(df, path); df
}

fetch_one_wb <- function(source_id, name) {
  out <- raw_path_wb(name)
  if (file.exists(out) && !REFRESH_STRUCTURAL) return(invisible(out))
  rows <- list(); page <- 1
  repeat {
    url <- sprintf("%s/country/all/indicator/%s?format=json&date=%d:%d&per_page=1000&page=%d",
                   WB_API, source_id, START_YEAR, END_YEAR, page)
    payload <- get_json(url)
    if (is.null(payload) || length(payload) < 2 || is.null(payload[[2]])) break
    rows <- c(rows, payload[[2]])
    if (page >= as.integer(payload[[1]]$pages %||% 1)) break
    page <- page + 1
  }
  if (length(rows) == 0) { log_info("  EMPTY: ", name); return(invisible(NULL)) }
  df <- tibble(
    country_iso3 = map_chr(rows, ~ .x$countryiso3code %||% NA_character_),
    year         = suppressWarnings(as.integer(map_chr(rows, ~ .x$date %||% NA_character_))),
    value        = map_dbl(rows, ~ { v <- .x$value; if (is.null(v)) NA_real_ else as.numeric(v) })
  ) |> filter(is_iso3(country_iso3), country_iso3 %in% real_countries,
              !is.na(year), !is.na(value))
  write_csv(df, out); invisible(out)
}

fetch_one_owid <- function(slug) {
  out <- raw_path_owid(slug)
  if (file.exists(out) && !REFRESH_STRUCTURAL) return(invisible(out))
  resp <- tryCatch(
    request("https://ourworldindata.org") |>
      req_url_path("grapher", paste0(slug, ".csv")) |>
      req_url_query(v = "1", csvType = "full", useColumnShortNames = "false") |>
      req_user_agent(USER_AGENT) |> req_timeout(60) |>
      req_error(is_error = function(r) FALSE) |> req_perform(),
    error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) { log_info("  FAIL: ", slug); return(invisible(NULL)) }
  writeBin(resp_body_raw(resp), out); invisible(out)
}

log_info(strrep("=", 60)); log_info("SECTION 1 — structural history")
log_info("Fetching World Bank country list...")
country_dir    <- fetch_country_list()
real_countries <- country_dir |> filter(is_real_country) |> pull(country_iso3)
log_info(sprintf("  %d real countries.", length(real_countries)))

wb <- indicators |> filter(source == "worldbank")
log_info("World Bank: ", nrow(wb), " indicators.")
for (i in seq_len(nrow(wb))) {
  log_info(sprintf("  [%2d/%2d] %s", i, nrow(wb), wb$indicator[i]))
  fetch_one_wb(wb$source_id[i], wb$indicator[i])
}

ow <- indicators |> filter(source == "owid")
log_info("OWID: ", nrow(ow), " datasets.")
for (i in seq_len(nrow(ow))) {
  log_info(sprintf("  [%2d/%2d] %s", i, nrow(ow), ow$source_id[i]))
  fetch_one_owid(ow$source_id[i])
}


# =============================================================================
# SECTION 2 — DEMOGRAPHIC  (UN World Population Prospects 2024)
# =============================================================================
# Easiest projection layer: the UN already computed the projections AND their
# uncertainty. We only cache; the observed/projection split happens in process.
#
# Portal indicator IDs are NUMERIC, not the registry short codes. Pinned below
# so a silent renumber fails loudly (empty fetch) rather than fetching wrong
# data. VERIFY on first run against GET /indicators?format=json. Rock-solid
# alternative if the portal is flaky: the wpp2024 R package, whose projection
# tables already carry pop/pop_95l/pop_95u — swap fetch_wpp_indicator() for a
# package read and process.R is unchanged.

WPP_API <- "https://population.un.org/dataportalapi/api/v1"
WPP_INDICATOR_IDS <- c(
  population_total       = 49,
  median_age             = 67,
  # KEY MUST MATCH the registry indicator name: the unwpp fertility row is
  # `fertility_rate_proj` (the World Bank one is `fertility_rate`). Under the
  # old key `fertility_rate` the projection was silently never fetched.
  fertility_rate_proj    = 19,
  life_expectancy_proj   = 61
)

# IMPORTANT shape facts confirmed against the live API:
#  - The data endpoints REQUIRE a bearer token (Authorization header), read via
#    get_wpp_token() from env var WPP_API_TOKEN or config/.wpp_token. Never
#    hardcoded here.
#  - `locations/all` returns HTTP 500 (server can't handle the full cross
#    product). We instead request explicit numeric location IDs in BATCHES,
#    pulled from the location map. Each batch paginates internally via nextPage.
#  - Records carry iso3 directly, plus numeric locationId. Year is `timeLabel`.
#  - Real variant labels (confirmed): "Median", "95% Lower", "95% Upper",
#    "80% Lower", "80% Upper", plus many deterministic variants we ignore.
#    02_process.R keeps only Median + the 95% bounds.

WPP_TOKEN <- get_wpp_token()
WPP_HEADERS <- if (!is.null(WPP_TOKEN))
  c(Authorization = paste("Bearer", WPP_TOKEN)) else NULL

WPP_BATCH_SIZE <- 50   # (unused since switching to per-country fetch; kept for
# reference). Per-country fetching makes each response a single page, so there
# is no batching/pagination to fail.

# One-time location map: numeric id + iso3 for REAL COUNTRIES ONLY. Cached.
# The /locations endpoint mixes true countries with UN analytical aggregates
# (e.g. iso3 "LLD" = Land-locked Developing Countries, "SID" = Small Island
# Developing States, "ANZ" = Australia/NZ, "LMC" = lower-middle-income), several
# of which reuse the same 3-letter code across regional sub-aggregates. Those
# pass the is_iso3() regex and, if fetched, stack multiple values onto one
# iso3-year-variant cell -> the duplication bug. We drop them two ways:
#   1. by NAME: anything that reads like a grouping, not a country.
#   2. by RULE: any iso3 mapping to >1 location_id is not a sovereign country.
wpp_location_map <- function() {
  path <- file.path(PATHS$raw, "_wpp_locations.csv")
  if (file.exists(path) && !REFRESH_DEMOGRAPHIC) {
    cached <- read_csv(path, show_col_types = FALSE)
    if (all(c("location_id", "iso3") %in% names(cached)) &&
        anyDuplicated(cached$iso3) == 0) return(cached)
    log_info("  stale/duplicated location cache — rebuilding")
  }
  rows <- list(); url <- sprintf("%s/locations?sort=id&pageSize=1000&format=json", WPP_API); guard <- 0
  repeat {
    payload <- get_json(url, timeout = 90, headers = WPP_HEADERS); if (is.null(payload)) break
    rows <- c(rows, payload$data %||% list())
    nxt <- payload$nextPage; if (is.null(nxt) || is.na(nxt) || nxt == "") break
    url <- sub("^http://", "https://", nxt); guard <- guard + 1; if (guard > 50) break
  }
  if (length(rows) == 0) { log_info("  WARN: could not fetch WPP location list"); return(NULL) }
  
  raw_df <- tibble(
    location_id = map_int(rows, ~ as.integer(.x$id %||% NA)),
    iso2 = map_chr(rows, ~ as.character(.x$iso2 %||% NA_character_)),
    iso3 = map_chr(rows, ~ as.character(.x$iso3 %||% NA_character_)),
    name = map_chr(rows, ~ as.character(.x$name %||% NA_character_))
  ) |> filter(!is.na(location_id), is_iso3(iso3))
  
  # 1. Drop by name pattern: UN aggregate groupings, not countries.
  agg_pattern <- regex(paste(
    "developing|developed|income|least developed|landlocked|land-locked",
    "small island|SIDS|LLDC|world|region|sub-saharan|subregion|aggregate",
    "more developed|less developed|OECD|high-income|middle-income|low-income",
    sep = "|"), ignore_case = TRUE)
  df <- raw_df |> filter(!str_detect(name, agg_pattern))
  
  # 2. Drop by rule: any iso3 still mapping to >1 id is not a sovereign country.
  dup_iso3 <- df |> count(iso3) |> filter(n > 1) |> pull(iso3)
  if (length(dup_iso3) > 0) {
    log_info("  dropping ", length(dup_iso3), " non-country iso3 with multiple ids: ",
             paste(head(dup_iso3, 10), collapse = ", "))
    df <- df |> filter(!iso3 %in% dup_iso3)
  }
  
  df <- df |> select(location_id, iso2, iso3) |> distinct(iso3, .keep_all = TRUE)
  log_info(sprintf("  location map: %d real countries (from %d raw entries)",
                   nrow(df), nrow(raw_df)))
  write_csv(df, path); df
}

.wpp_dumped_fields <- FALSE  # so we only dump the field-name diagnostic once

# Fetch one batch of location IDs for one indicator. Returns a list with the
# records AND whether the batch is COMPLETE (collected == total). The old
# version broke silently on any null page and capped at 50 pages, so large
# batches truncated and whole countries vanished -> 73/272 coverage. Now we:
#   - read `total` from page 1 and keep paging until we have that many records
#   - retry a failed page a few times before giving up
#   - report completeness so the caller can retry/shrink a short batch
WPP_START_YEAR <- 1990   # full dashboard window (1990-2100); WPP provides
# observed estimates back to 1950, projections after the estimate cutoff.

# Fetch ONE country for one indicator. A single-country / single-indicator slice
# fits in ONE page (~90 years x a handful of variants x 3 sexes < 1000 rows),
# so there is no pagination to fail, no batch to come up short, and no retry
# storm. We follow nextPage defensively in case a slice ever exceeds one page,
# but in practice it never does. get_json already retries internally (3x); we do
# NOT wrap it in a second retry loop — that nesting was what turned a slow API
# patch into multi-minute hangs per page.
wpp_fetch_country <- function(ind_id, loc_id) {
  url <- sprintf("%s/data/indicators/%d/locations/%d/start/%d/end/2100?pageSize=1000&format=json",
                 WPP_API, ind_id, loc_id, WPP_START_YEAR)
  out <- list(); page_guard <- 0
  repeat {
    payload <- get_json(url, timeout = 60, headers = WPP_HEADERS)
    if (is.null(payload)) break          # get_json already retried; give up on this country
    out <- c(out, payload$data %||% list())
    nxt <- payload$nextPage
    if (is.null(nxt) || is.na(nxt) || nxt == "") break
    url <- sub("^http://", "https://", nxt)
    page_guard <- page_guard + 1; if (page_guard > 20) break
  }
  out
}

# A cached file is only reusable if it actually COVERS the window we now ask
# for. Without this check, a cache written under an older WPP_START_YEAR is
# reused forever (REFRESH_DEMOGRAPHIC is FALSE by default) and the dashboard
# silently loses decades of history -- population/median_age/life_expectancy_proj
# were all stuck at 2010+ this way while the one newly-fetched indicator reached
# 1990. Coverage, not mere existence, is the reuse condition.
wpp_cache_usable <- function(path) {
  if (!file.exists(path)) return(FALSE)
  d <- tryCatch(read_csv(path, show_col_types = FALSE), error = function(e) NULL)
  if (is.null(d) || !"year" %in% names(d) || nrow(d) == 0) return(FALSE)
  if (min(d$year, na.rm = TRUE) > WPP_START_YEAR) {
    log_info(sprintf("  cache starts %d but we need %d -- refetching",
                     min(d$year, na.rm = TRUE), WPP_START_YEAR))
    return(FALSE)
  }
  TRUE
}

fetch_wpp_indicator <- function(reg_key, loc_map) {
  out <- raw_path_wpp(reg_key)
  if (!REFRESH_DEMOGRAPHIC && wpp_cache_usable(out)) return(invisible(out))
  ind_id <- WPP_INDICATOR_IDS[[reg_key]]
  if (is.null(ind_id)) { log_info("  no portal id for ", reg_key); return(invisible(NULL)) }
  if (is.null(loc_map) || nrow(loc_map) == 0) { log_info("  no location map; skip ", reg_key); return(invisible(NULL)) }
  if (!"location_id" %in% names(loc_map)) {
    log_info("  location map missing 'location_id' — delete data/raw/_wpp_locations.csv and re-run")
    return(invisible(NULL))
  }
  
  ids <- unique(loc_map$location_id)
  expected_countries <- nrow(loc_map)
  rows <- list()
  for (k in seq_along(ids)) {
    rows <- c(rows, wpp_fetch_country(ind_id, ids[k]))
    if (k %% 50 == 0) log_info(sprintf("    ...%d/%d countries", k, length(ids)))
  }
  if (length(rows) == 0) { log_info("  EMPTY: ", reg_key, " (token? network? ID?)"); return(invisible(NULL)) }
  
  if (!.wpp_dumped_fields) {
    log_info("  [first-record fields] ", paste(names(rows[[1]]), collapse = ", "))
    # Dump the sex/age values present so the total-aggregate filter is verifiable.
    sx <- unique(map_chr(rows, ~ as.character(.x$sex %||% NA)))
    ag <- unique(map_chr(rows, ~ as.character(.x$ageLabel %||% NA)))
    log_info("  [sex values] ", paste(head(sx, 8), collapse = " | "))
    log_info("  [age values] ", paste(head(ag, 12), collapse = " | "))
    .wpp_dumped_fields <<- TRUE
  }
  
  pick <- function(r, keys) { for (k in keys) if (!is.null(r[[k]])) return(r[[k]]); NA }
  
  # CRITICAL: WPP indicator 49 returns population split by SEX (Male/Female/Both
  # sexes) at ageLabel "Total". Confirmed from the raw API: India/2024/Median has
  # 3 rows (sexId 1/2/3); only sexId==3 "Both sexes" is the country total
  # (1,450,935,791 for India). Without this filter we mix all three and dedup
  # keeps an arbitrary one -> wrong values. Age is already "Total" here, but we
  # also guard the age dimension for indicators that do break it down.
  raw_tbl <- tibble(
    iso3    = map_chr(rows, ~ as.character(pick(.x, c("iso3")))),
    year    = map_int(rows, ~ as.integer(pick(.x, c("timeLabel", "timeMid", "time")))),
    variant = map_chr(rows, ~ as.character(pick(.x, c("variantLabel", "variant")))),
    sex_id  = map_int(rows, ~ as.integer(.x$sexId %||% NA)),
    sex     = map_chr(rows, ~ as.character(.x$sex %||% NA_character_)),
    age     = map_chr(rows, ~ as.character(.x$ageLabel %||% NA_character_)),
    value   = map_dbl(rows, ~ { v <- pick(.x, c("value")); if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v) })
  )
  
  # Sex filter: keep "Both sexes" (sexId 3) when a sex breakdown exists.
  if (any(raw_tbl$sex_id == 3, na.rm = TRUE)) {
    raw_tbl <- raw_tbl |> filter(sex_id == 3 | is.na(sex_id))
  } else if (any(raw_tbl$sex == "Both sexes", na.rm = TRUE)) {
    raw_tbl <- raw_tbl |> filter(sex == "Both sexes" | is.na(sex))
  }
  # Age filter: keep an explicit total label when one exists; else keep all.
  total_age_labels <- c("Total", "All ages", "0+", "Total all ages")
  if (any(raw_tbl$age %in% total_age_labels, na.rm = TRUE)) {
    raw_tbl <- raw_tbl |> filter(age %in% total_age_labels | is.na(age))
  }
  
  df <- raw_tbl |>
    select(iso3, year, variant, value) |>
    filter(is_iso3(iso3), !is.na(year), !is.na(value))
  
  # Sanity: after filtering there should be exactly ONE row per iso3-year-variant.
  dup <- df |> count(iso3, year, variant) |> filter(n > 1)
  if (nrow(dup) > 0) {
    log_info(sprintf("  WARN %s: %d iso3-year-variant cells still have >1 row (sex/age not fully collapsed)",
                     reg_key, nrow(dup)))
  }
  
  if (nrow(df) == 0) { log_info("  EMPTY after parse: ", reg_key); return(invisible(NULL)) }
  
  # COMPLETENESS GUARD: this is the check that was missing and let 73/272
  # coverage slip through silently. A demographic indicator should cover nearly
  # all countries; if it's well short, the fetch lost data and the cache should
  # not be trusted. Warn loudly with the count so it's impossible to miss.
  n_countries <- n_distinct(df$iso3)
  coverage <- n_countries / expected_countries
  if (coverage < 0.9) {
    log_info(sprintf("  *** WARN %s: only %d/%d countries (%.0f%%) — INCOMPLETE FETCH, do not trust ***",
                     reg_key, n_countries, expected_countries, 100 * coverage))
  } else {
    log_info(sprintf("  %s coverage OK: %d/%d countries", reg_key, n_countries, expected_countries))
  }
  
  log_info(sprintf("  %s: %d rows, variants: %s", reg_key, nrow(df),
                   paste(head(unique(df$variant), 8), collapse = " | ")))
  write_csv(df, out); invisible(out)
}

log_info(strrep("=", 60)); log_info("SECTION 2 — demographic (UN WPP)")
if (is.null(WPP_TOKEN)) {
  log_info("  WARN: no WPP token found (set WPP_API_TOKEN or config/.wpp_token).")
  log_info("        Demographic calls will 401 until a token is provided.")
} else {
  log_info("  WPP token loaded.")
}
log_info("  building iso2->iso3 location map...")
WPP_LOC_MAP <- wpp_location_map()
demo <- indicators |> filter(source == "unwpp")
log_info("UN WPP: ", nrow(demo), " indicators.")
for (i in seq_len(nrow(demo))) {
  log_info(sprintf("  [%2d/%2d] %s", i, nrow(demo), demo$indicator[i]))
  fetch_wpp_indicator(demo$indicator[i], WPP_LOC_MAP)
}


# =============================================================================
# SECTION 3 — CLIMATE  (World Bank CCKP, CMIP6)  — SCENARIO-AWARE
# =============================================================================
# Fetched once per SSP pathway, so the chart can draw one line per scenario.
# Band = multi-MODEL spread (median + p10/p90), i.e. how much the climate models
# disagree — NOT a forecast prediction interval. BAND_MEANING in helpers.R
# records that distinction so nobody conflates it with the UN's band.
#
# API shape (from CCKP docs):
#   cckpapi.worldbank.org/cckp/v1/
#     cmip6-x0.25_timeseries_<var>_timeseries_annual_2015-2100_<pct>_<ssp>_ensemble_all_mean/
#     all_countries?_format=json
# Response: data -> iso3 -> {"YYYY-07": value}.

CCKP_API  <- "https://cckpapi.worldbank.org/cckp/v1"
SCENARIOS <- c("ssp126", "ssp245", "ssp585")   # optimistic / middle / high

cckp_url <- function(var, scenario, percentile) {
  sprintf(paste0("%s/cmip6-x0.25_timeseries_%s_timeseries_annual_2015-2100_",
                 "%s_%s_ensemble_all_mean/all_countries?_format=json"),
          CCKP_API, var, percentile, scenario)
}

fetch_one_cckp <- function(var, scenario) {
  out <- raw_path_cckp(var, scenario)
  if (file.exists(out) && !REFRESH_CLIMATE) return(invisible(out))
  pulls <- list(value = "median", lower = "p10", upper = "p90")
  parsed <- list()
  for (band_col in names(pulls)) {
    payload <- get_json(cckp_url(var, scenario, pulls[[band_col]]), timeout = 120)
    if (is.null(payload)) {
      log_info(sprintf("  FAIL: %s/%s (%s)", var, scenario, pulls[[band_col]]))
      if (band_col == "value") return(invisible(NULL)) else next
    }
    data_node <- payload$data %||% payload
    long <- imap_dfr(data_node, function(series, iso3) {
      if (!is.list(series) || length(series) == 0) return(NULL)
      tibble(iso3 = iso3,
             year = suppressWarnings(as.integer(substr(names(series), 1, 4))),
             v    = map_dbl(series, ~ { x <- .x; if (is.null(x)) NA_real_ else as.numeric(x) }))
    })
    if (nrow(long) == 0) next
    names(long)[names(long) == "v"] <- band_col
    parsed[[band_col]] <- long
  }
  if (is.null(parsed$value)) { log_info("  no median for ", var, "/", scenario); return(invisible(NULL)) }
  df <- parsed$value
  if (!is.null(parsed$lower)) df <- left_join(df, parsed$lower, by = c("iso3", "year"))
  if (!is.null(parsed$upper)) df <- left_join(df, parsed$upper, by = c("iso3", "year"))
  if (!"lower" %in% names(df)) df$lower <- df$value
  if (!"upper" %in% names(df)) df$upper <- df$value
  df <- df |> filter(is_iso3(iso3), !is.na(year), !is.na(value))
  write_csv(df, out); invisible(out)
}

log_info(strrep("=", 60)); log_info("SECTION 3 — climate (CCKP CMIP6)")
clim <- indicators |> filter(source == "cckp")
log_info("CCKP: ", nrow(clim), " indicators x ", length(SCENARIOS), " scenarios.")
for (i in seq_len(nrow(clim))) {
  var <- clim$source_id[i]
  for (scen in SCENARIOS) {
    log_info(sprintf("  %s / %s", var, scen))
    fetch_one_cckp(var, scen)
  }
}

log_info(strrep("=", 60))
log_info("Fetch complete. Raw caches in ", PATHS$raw)
log_info("Next: source('R/02_process.R') to harmonize into the canonical panel.")