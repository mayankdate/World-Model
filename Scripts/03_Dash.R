# Scripts/03_Dash.R
# =============================================================================
# Builds the SELF-CONTAINED "World Model" World View dashboard.
#   - reads data/output/panel_full.csv
#   - embeds the data as one JSON blob into the HTML template
#   - NO external dependencies: no d3, no knowledge.js, no web fonts. All
#     charting is hand-rolled SVG in the template's vanilla JS.
#   - the HTML template lives RIGHT HERE in Scripts/ (dashboard_template.html),
#     not in config/, so everything dashboard-related is in one folder.
#
#   source("Scripts/03_Dash.R")   ->   data/output/dashboard.html
# =============================================================================

source("Scripts/00_helpers.R")
ensure_dirs()
library(jsonlite)

# Major players to embed (~30). The dashboard's country chips are exactly this
# set. All G20 members + pivotal/large states, as agreed. Edit freely; any ISO3
# present in panel_full.csv works. NULL = embed everything.
DASHBOARD_COUNTRIES <- c(
  # G20 (19 member states)
  "ARG","AUS","BRA","CAN","CHN","FRA","DEU","IND","IDN","ITA","JPN","MEX",
  "RUS","SAU","ZAF","KOR","TUR","GBR","USA",
  # pivotal / large states
  "NGA","EGY","IRN","VNM","POL","ESP","NLD","PAK","BGD","ETH","THA"
)
# Which countries start toggled-on when the dashboard opens (a readable handful).
DEFAULT_SELECTION <- c("USA","CAN","IND","GBR","JPN","RUS")

# Your lifespan context — drives the age-milestone lines on every chart.
BIRTH_YEAR <- 1996

# History floor: V-Dem reconstructs to 1789 (historians' codings, not
# measurements). The dashboard window is 1990-2100, so clip observed history.
HISTORY_START <- 1990

panel_path <- file.path(PATHS$output, "panel_full.csv")
if (!file.exists(panel_path)) stop("panel_full.csv not found — run 02_process_project.R first")
panel <- read_csv(panel_path, show_col_types = FALSE)
indicators <- read_indicators()

if (!is.null(DASHBOARD_COUNTRIES)) {
  before <- n_distinct(panel$country_iso3)
  panel <- panel |> filter(country_iso3 %in% DASHBOARD_COUNTRIES)
  log_info(sprintf("Filtered to %d major-player countries (from %d)",
                   n_distinct(panel$country_iso3), before))
}
panel <- panel |> filter(series_type != "observed" | year >= HISTORY_START)
log_info(sprintf("Clipped observed history to %d+ (projections unaffected)", HISTORY_START))
log_info("Building dashboard from ", format(nrow(panel), big.mark = ","), " rows")

# -- indicator metadata for the UI --------------------------------------------
ind_meta <- indicators |>
  transmute(id = indicator, label = description, unit, direction, module,
            is_climate = (source == "cckp"))

# -- country names (World Bank cached list) -----------------------------------
cty_path <- raw_path_country()
country_names <- if (file.exists(cty_path)) {
  read_csv(cty_path, show_col_types = FALSE) |>
    filter(is_real_country) |> select(country_iso3, name)
} else tibble(country_iso3 = unique(panel$country_iso3),
              name = unique(panel$country_iso3))

# -- thin + structure the payload ---------------------------------------------
round_sig <- function(x) signif(x, 5)
series <- panel |>
  filter(!is.na(value)) |>
  mutate(value = round_sig(value), lower = round_sig(lower), upper = round_sig(upper),
         has_band = series_type %in% c("projection_demographic","projection_climate","projection_structural") &
           (lower != upper))

st_code <- c(observed="o", projection_demographic="d",
             projection_climate="c", projection_structural="p", nowcast="n")

build_payload <- function(df) {
  df <- df |> mutate(tcode = unname(st_code[series_type])) |>
    arrange(indicator, country_iso3, year)
  pts <- purrr::pmap(
    list(df$year, df$value, df$tcode, df$scenario, df$has_band, df$lower, df$upper, df$series_type),
    function(y,v,t,s,band,lo,hi,st){
      o <- list(y=y, v=v, t=t)
      if (st=="projection_climate") o$s <- s
      if (isTRUE(band)) { o$lo <- lo; o$hi <- hi }
      o
    })
  idx_ind <- split(seq_along(pts), df$indicator)
  lapply(idx_ind, function(ii) split(pts[ii], df$country_iso3[ii]))
}
payload <- build_payload(series)

# -- META ---------------------------------------------------------------------
present_iso <- unique(panel$country_iso3)
meta <- list(
  indicators = ind_meta |> arrange(label),
  countries  = country_names |> filter(country_iso3 %in% present_iso) |>
    arrange(name) |> transmute(iso = country_iso3, name),
  default_selection = intersect(DEFAULT_SELECTION, present_iso),
  birth_year = BIRTH_YEAR,
  generated  = as.character(Sys.Date())
)

payload_json <- toJSON(payload, auto_unbox = TRUE, na = "null", digits = 8)
meta_json    <- toJSON(meta, auto_unbox = TRUE, na = "null", dataframe = "rows")
log_info("Payload: ", round(nchar(payload_json)/1e6, 1), " MB JSON")

# The coupled-model spec is written by 02_process_project.R (R owns the model).
# We pass it through verbatim so the dashboard's JS evaluates the named forms it
# defines. If the spec is missing (e.g. 02 not yet rerun), inject an empty spec
# so the Coupled-model tab degrades gracefully rather than erroring.
spec_path <- file.path(PATHS$output, "coupling_spec.json")
coupling_json <- if (file.exists(spec_path)) {
  paste(readLines(spec_path, warn = FALSE), collapse = "\n")
} else {
  log_info("NOTE: coupling_spec.json not found — rerun 02; injecting empty spec.")
  "{\"params\":{},\"edges\":[],\"notes\":{}}"
}

# -- inject into the template (lives in Scripts/) -----------------------------
tpl_path <- file.path("Scripts", "dashboard_template.html")
if (!file.exists(tpl_path)) stop("dashboard_template.html not found in Scripts/")
tpl <- paste(readLines(tpl_path, warn = FALSE), collapse = "\n")

# The template's data block is one token: const META; const DATA; const COUPLING.
inject <- sprintf("const META=%s;\nconst DATA=%s;\nconst COUPLING=%s;",
                  meta_json, payload_json, coupling_json)
html <- sub("__DATA__", inject, tpl, fixed = TRUE)

out_path <- file.path(PATHS$output, "dashboard.html")
writeLines(html, out_path)
log_info("Wrote ", out_path, " (", round(file.size(out_path)/1e6, 1), " MB)")
log_info("Open it directly in any browser — no server, no internet, no dependencies.")