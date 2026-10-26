# ----------------------------------------------------------------------------- #
#  01 FETCH DATA  -------------------
# ----------------------------------------------------------------------------- #
# Fetches all configured indicators from the World Bank API and OWID's grapher
# CSV exports, caching one CSV per indicator under data/raw/ with a source
# prefix:
#   data/raw/wb_<indicator>.csv          (World Bank)
#   data/raw/owid_<slug>.csv             (Our World In Data)
#   data/raw/_wb_countries.csv           (canonical country list)
#
# Set REFRESH <- TRUE at the top to force re-download. Otherwise cached files
# are reused, which makes the rest of the pipeline cheap to iterate on.
# ----------------------------------------------------------------------------- #

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
cat("Working Directory Set To:", getwd(), "\n")

source("Scripts/helpers.R")
ensure_dirs()

REFRESH    <- FALSE
START_YEAR <- 2000
END_YEAR   <- as.integer(format(Sys.Date(), "%Y"))


# ----------------------------------------------------------------------------- #
# WORLD BANK  -------------------
# ----------------------------------------------------------------------------- #

API_BASE <- "https://api.worldbank.org/v2"

# Fetch the canonical country list and identify "real" countries (those with
# a region). World Bank's /country endpoint also returns aggregates like WLD,
# EUU, EMU which have region.id == "NA". We filter them out so they don't
# pollute downstream percentile rankings.
fetch_country_list <- function() {
  path <- raw_path_country_list()
  if (file.exists(path) && !REFRESH) {
    return(read_csv(path, show_col_types = FALSE))
  }
  url <- sprintf("%s/country?format=json&per_page=400", API_BASE)
  resp <- request(url) |> req_timeout(60) |> req_perform()
  payload <- resp_body_json(resp, simplifyVector = FALSE)
  if (length(payload) < 2) stop("Unexpected response from /v2/country")
  rows <- payload[[2]]
  
  df <- tibble(
    country_iso3 = map_chr(rows, ~ .x$id %||% NA_character_),
    name         = map_chr(rows, ~ .x$name %||% NA_character_),
    region_id    = map_chr(rows, ~ .x$region$id %||% NA_character_),
    income_level = map_chr(rows, ~ .x$incomeLevel$id %||% NA_character_)
  ) |>
    mutate(is_real_country = !is.na(region_id) & region_id != "NA")
  
  write_csv(df, path)
  df
}

log_info("Fetching World Bank country list (filters aggregates like WLD, EUU)...")
country_dir <- fetch_country_list()
real_countries <- country_dir |> filter(is_real_country) |> pull(country_iso3)
log_info(sprintf("Found %d real countries (and %d aggregates we'll filter out).",
                 length(real_countries),
                 sum(!country_dir$is_real_country)))

indicators <- read_indicators() |> filter(source == "worldbank")
log_info("World Bank: ", nrow(indicators), " indicators to fetch.")

fetch_one_wb_indicator <- function(source_id, indicator_name) {
  out_path <- raw_path_wb(indicator_name)
  if (file.exists(out_path) && !REFRESH) {
    return(invisible(out_path))
  }
  
  date_range <- sprintf("%d:%d", START_YEAR, END_YEAR)
  rows <- list()
  page <- 1
  repeat {
    url <- sprintf("%s/country/all/indicator/%s?format=json&date=%s&per_page=1000&page=%d",
                   API_BASE, source_id, date_range, page)
    resp <- tryCatch(
      request(url) |> req_timeout(60) |> req_perform(),
      error = function(e) NULL
    )
    if (is.null(resp)) {
      log_info("  FAIL: ", indicator_name, " (network error)")
      return(invisible(NULL))
    }
    payload <- resp_body_json(resp, simplifyVector = FALSE)
    if (length(payload) < 2 || is.null(payload[[2]])) break
    
    meta <- payload[[1]]; data <- payload[[2]]
    rows <- c(rows, data)
    if (page >= as.integer(meta$pages %||% 1)) break
    page <- page + 1
  }
  
  if (length(rows) == 0) {
    log_info("  EMPTY: ", indicator_name)
    return(invisible(NULL))
  }
  
  df <- tibble(
    country_iso3 = map_chr(rows, ~ .x$countryiso3code %||% NA_character_),
    year         = suppressWarnings(as.integer(map_chr(rows, ~ .x$date %||% NA_character_))),
    value        = map_dbl(rows, ~ {
      v <- .x$value
      if (is.null(v)) NA_real_ else as.numeric(v)
    })
  ) |>
    filter(is_iso3(country_iso3),
           country_iso3 %in% real_countries,
           !is.na(year), !is.na(value))
  
  write_csv(df, out_path)
  invisible(out_path)
}

for (i in seq_len(nrow(indicators))) {
  ind <- indicators[i, ]
  log_info(sprintf("[%2d/%2d] %s (%s)", i, nrow(indicators),
                   ind$indicator, ind$source_id))
  fetch_one_wb_indicator(ind$source_id, ind$indicator)
}

log_info("World Bank fetch complete. Files in: ", PATHS$raw)


# ----------------------------------------------------------------------------- #
# OWID  -------------------
# ----------------------------------------------------------------------------- #

USER_AGENT <- "life-projection R pipeline (personal research use)"

owid_indicators <- read_indicators() |> filter(source == "owid")
log_info("OWID: ", nrow(owid_indicators), " datasets to fetch.")

fetch_one_owid <- function(slug, target_path) {
  if (file.exists(target_path) && !REFRESH) return(invisible(target_path))
  
  resp <- tryCatch(
    request("https://ourworldindata.org") |>
      req_url_path("grapher", paste0(slug, ".csv")) |>
      req_url_query(
        v                   = "1",
        csvType             = "full",
        useColumnShortNames = "false"
      ) |>
      req_user_agent(USER_AGENT) |>
      req_timeout(60) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform(),
    error = function(e) {
      log_info("  FAIL: ", slug, " (", conditionMessage(e), ")")
      NULL
    }
  )
  if (is.null(resp)) return(invisible(NULL))
  
  status <- resp_status(resp)
  if (status == 200) {
    body <- resp_body_raw(resp)
    writeBin(body, target_path)
    invisible(target_path)
  } else if (status == 404) {
    log_info("  FAIL: ", slug, " (404 — slug renamed? check at ",
             "https://ourworldindata.org/grapher/", slug, ")")
    invisible(NULL)
  } else {
    log_info(sprintf("  FAIL: %s (HTTP %d)", slug, status))
    invisible(NULL)
  }
}

for (i in seq_len(nrow(owid_indicators))) {
  ind  <- owid_indicators[i, ]
  slug <- ind$source_id
  out  <- raw_path_owid(slug)
  log_info(sprintf("[%2d/%2d] %s", i, nrow(owid_indicators), slug))
  fetch_one_owid(slug, out)
}

log_info("OWID fetch complete. Files in: ", PATHS$raw)