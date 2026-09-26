# R/helpers.R
# =============================================================================
# Shared functions and the CANONICAL SCHEMA CONTRACT for the global-trends
# projection engine. Sourced at the top of every numbered script.
#
#   source("R/helpers.R")
#
# The single most important thing in this file is CANONICAL_COLS: the shape
# every layer must conform to. Keeping all four horizon-layers in one schema
# is what lets a chart draw them together; keeping them in DISTINCT ROWS
# (never blended) is what stops a wide-band guess masquerading as a fact.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(httr2)
  library(jsonlite)
})

# -- The canonical schema -----------------------------------------------------
#
# Every fact in this system, observed or projected, is one row of:
#
#   country_iso3  ISO3 code ("IND", "USA", ...)
#   year          integer
#   indicator     registry key ("gdp_per_capita_ppp", "population_total", ...)
#   series_type   one of SERIES_TYPES below -- WHICH horizon-layer this is
#   scenario      climate SSP code, or "none" for everything else
#   value         central estimate
#   lower, upper  uncertainty band. For observed rows, lower=upper=value.
#                 For projections, the meaning of the band depends on layer
#                 (see BAND_MEANING) -- do not compare bands across layers
#                 as if they meant the same thing.
#
# series_type is the contract that keeps layers honest. The chart maps it to
# line style + how far the line is allowed to reach.

SERIES_TYPES <- c(
  "observed",                # historical fact
  "projection_demographic",  # UN WPP -- band is a real prediction interval
  "projection_climate",      # CCKP CMIP6 -- band is inter-MODEL spread (p10/p90)
  "projection_structural",   # our own OLS extrapolation -- band is a fitted PI
  "nowcast"                  # GDELT pulse -- present only, no forward reach
)

# Human-readable note on what each band actually means. Surfaced in the
# quality report so nobody mistakes a model-spread for a prediction interval.
BAND_MEANING <- c(
  observed               = "no uncertainty (lower=upper=value)",
  projection_demographic = "UN 80/95% prediction interval (externally authored)",
  projection_climate     = "CMIP6 inter-model spread p10-p90 (NOT a forecast PI)",
  projection_structural  = "OLS prediction interval from our own fit (calibrate!)",
  nowcast                = "present-moment estimate, no forward projection"
)

CANONICAL_COLS <- c("country_iso3", "year", "indicator",
                    "series_type", "scenario", "value", "lower", "upper")

# Coerce any layer's output into the canonical shape. Every fetch/project
# script ends by passing its data through this so the union is trivial and
# any missing column fails loudly here rather than silently downstream.
as_canonical <- function(df) {
  missing <- setdiff(CANONICAL_COLS, names(df))
  if (length(missing) > 0) {
    stop("as_canonical(): missing required column(s): ",
         paste(missing, collapse = ", "))
  }
  bad <- setdiff(unique(df$series_type), SERIES_TYPES)
  if (length(bad) > 0) {
    stop("as_canonical(): unknown series_type(s): ", paste(bad, collapse = ", "))
  }
  df |>
    mutate(
      country_iso3 = as.character(country_iso3),
      year         = as.integer(year),
      indicator    = as.character(indicator),
      series_type  = as.character(series_type),
      scenario     = if_else(is.na(scenario) | scenario == "", "none",
                             as.character(scenario)),
      value        = as.numeric(value),
      lower        = as.numeric(lower),
      upper        = as.numeric(upper)
    ) |>
    select(all_of(CANONICAL_COLS))
}


# -- Paths --------------------------------------------------------------------
# Flat raw cache: one file per (source, thing) so a single ls tells you what
# you've fetched. Prefixes keep the four sources from colliding.

PATHS <- list(
  raw         = "data/raw",
  output      = "data/output",
  calibration = "data/calibration",
  config      = "config"
)

ensure_dirs <- function() for (p in PATHS) dir.create(p, recursive = TRUE, showWarnings = FALSE)

raw_path_wb      <- function(name) file.path(PATHS$raw, paste0("wb_",     name, ".csv"))
raw_path_owid    <- function(slug) file.path(PATHS$raw, paste0("owid_",   slug, ".csv"))
raw_path_wpp     <- function(name) file.path(PATHS$raw, paste0("wpp_",    name, ".csv"))
raw_path_cckp    <- function(name, scen) file.path(PATHS$raw, paste0("cckp_", name, "_", scen, ".csv"))
raw_path_gdelt   <- function(name) file.path(PATHS$raw, paste0("gdelt_",  name, ".csv"))
raw_path_country <- function() file.path(PATHS$raw, "_wb_countries.csv")


# -- Config loader ------------------------------------------------------------
# config/indicators.csv is the single source of truth, with a lean 8-column
# schema: indicator, module, source, source_id, unit, direction, description,
# weight. Everything the old pipeline stored in extra columns is now DERIVED
# from `source`, so there is exactly one source of truth per fact:
#   source==worldbank|owid  -> observed history, projected structurally (5y cap)
#   source==unwpp           -> demographic projection to 2100 (UN's own bands)
#   source==cckp            -> climate projection to 2100, scenario-aware
# `weight` is optional and only used if a composite index is ever built.

read_indicators <- function() {
  path <- file.path(PATHS$config, "indicators.csv")
  raw  <- read_csv(path, show_col_types = FALSE)
  
  required <- c("indicator", "module", "source", "source_id",
                "unit", "direction", "description")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop("indicators.csv missing column(s): ", paste(missing, collapse = ", "))
  }
  if (!"weight" %in% names(raw)) raw$weight <- NA_real_
  
  raw |>
    mutate(
      across(where(is.character), str_trim),
      weight = suppressWarnings(as.numeric(weight)),
      direction = if_else(direction %in% c("higher_is_better", "lower_is_better"),
                          direction, NA_character_),
      source = if_else(source %in% c("worldbank", "owid", "unwpp", "cckp"),
                       source, NA_character_),
      # derived, single-source-of-truth-from-`source`:
      series_type = case_when(
        source %in% c("worldbank", "owid") ~ "observed",
        source == "unwpp"                  ~ "projection_demographic",
        source == "cckp"                   ~ "projection_climate",
        TRUE ~ NA_character_),
      projection_method = case_when(
        source %in% c("worldbank", "owid") ~ "projection_structural",
        source == "unwpp"                  ~ "projection_demographic",
        source == "cckp"                   ~ "projection_climate",
        TRUE ~ "none"),
      scenario_aware    = (source == "cckp"),
      # structural indicators get a short cap; pre-projected sources reach 2100
      horizon_cap_years = if_else(source %in% c("worldbank", "owid"), 5L, 75L)
    ) |>
    filter(!is.na(direction), !is.na(source))
}


# -- Logging ------------------------------------------------------------------

log_info <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))


# -- HTTP with retry ----------------------------------------------------------
# All four fetchers share this. Returns the response or NULL on failure --
# never throws -- so one dead endpoint can't abort a whole fetch run.
#
# `headers` is an optional named character vector of extra request headers
# (e.g. c(Authorization = "Bearer ...")). Most sources pass nothing; the UN WPP
# fetcher passes its auth token this way. We never hardcode secrets here.

USER_AGENT <- "global-trends research pipeline (personal, non-commercial)"

get_json <- function(url, timeout = 60, retries = 3, headers = NULL) {
  for (attempt in seq_len(retries)) {
    req <- request(url) |>
      req_user_agent(USER_AGENT) |>
      req_timeout(timeout) |>
      req_error(is_error = function(r) FALSE)
    if (!is.null(headers)) req <- req |> req_headers(!!!headers)
    
    resp <- tryCatch(req_perform(req), error = function(e) NULL)
    if (!is.null(resp) && resp_status(resp) == 200) {
      return(tryCatch(resp_body_json(resp, simplifyVector = FALSE),
                      error = function(e) NULL))
    }
    # 401/403 won't fix themselves on retry; bail early with a clear signal.
    if (!is.null(resp) && resp_status(resp) %in% c(401, 403)) {
      attr_msg <- sprintf("auth failed (HTTP %d)", resp_status(resp))
      return(structure(NULL, http_error = attr_msg))
    }
    if (attempt < retries) Sys.sleep(2 * attempt)
  }
  NULL
}

# Reads the UN WPP bearer token WITHOUT hardcoding it. Looks, in order, for:
#   1. env var WPP_API_TOKEN  (set in ~/.Renviron or per-session)
#   2. a gitignored file at config/.wpp_token  (single line, the raw token)
# Returns NULL if neither is present, so the caller can warn cleanly.
# To set it once, add this line to ~/.Renviron (no quotes, no "Bearer "):
#   WPP_API_TOKEN=eyJhbGciOi...
# then restart R. Never commit the token or paste it into shared files.
get_wpp_token <- function() {
  tok <- Sys.getenv("WPP_API_TOKEN", unset = "")
  if (nzchar(tok)) return(trimws(tok))
  f <- file.path(PATHS$config, ".wpp_token")
  if (file.exists(f)) {
    tok <- trimws(readLines(f, n = 1, warn = FALSE))
    if (nzchar(tok)) return(tok)
  }
  NULL
}


# -- ISO3 helper --------------------------------------------------------------

is_iso3 <- function(x) !is.na(x) & nchar(x) == 3 & str_detect(x, "^[A-Z]{3}$")