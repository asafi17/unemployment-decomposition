# -----------------------------------------------------------------------------
# build_panels.R
#
# Central data build for this project. This script is responsible for all raw
# CPS and FRED processing needed by:
#   - ui-takeup-study.qmd
#   - unemployment_decomposition.qmd
#
# All report-facing outputs are written as parquet files under data/final/.
# -----------------------------------------------------------------------------

library(arrow)
library(fredr)
library(here)
library(ipumsr)
library(lubridate)
library(tidyverse)

here::i_am("build_panels.R")

fredr_set_key("ccd350785a210a7653a20d02ce92d83a")

final_dir <- here("data", "final")
fred_dir <- here("data", "FRED")
cps_dir <- here("data", "CPS")

cps_ddi_path <- here("data", "CPS", "cps_unified.xml")
cps_data_path <- here("data", "CPS", "cps_unified.csv.gz")

fred_panel_cache_path <- here("data", "FRED", "ui_takeup_fred_monthly_state_panel.csv")
fred_catalog_cache_path <- here("data", "FRED", "ui_takeup_fred_series_catalog.csv")
fred_meta_cache_path <- here("data", "FRED", "ui_takeup_fred_pull_version.txt")
fred_pull_version <- "earliest_available_v2_population"

stopifnot(file.exists(cps_ddi_path), file.exists(cps_data_path))

dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fred_dir, recursive = TRUE, showWarnings = FALSE)

write_final_parquet <- function(data, filename) {
  path <- here("data", "final", filename)
  arrow::write_parquet(as_tibble(data), sink = path)
  path
}

fredr_retry <- function(series_id, ..., max_tries = 5, wait_secs = 3) {
  last_err <- NULL
  for (i in seq_len(max_tries)) {
    result <- tryCatch(fredr(series_id, ...), error = function(e) e)
    if (!inherits(result, "error")) {
      return(result)
    }
    last_err <- result
    if (i < max_tries) Sys.sleep(wait_secs)
  }
  stop(
    "FRED API failed after ", max_tries, " attempts for ", series_id, ": ",
    conditionMessage(last_err)
  )
}

state_meta <- tibble(
  state = state.name,
  state_abbr = state.abb,
  fips = c(
    "01", "02", "04", "05", "06", "08", "09", "10", "12", "13",
    "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
    "25", "26", "27", "28", "29", "30", "31", "32", "33", "34",
    "35", "36", "37", "38", "39", "40", "41", "42", "44", "45",
    "46", "47", "48", "49", "50", "51", "53", "54", "55", "56"
  )
) |>
  arrange(state)

state_lookup <- state_meta |>
  transmute(
    STATEFIP = as.integer(fips),
    state,
    state_abbr
  )

cause_order <- c(
  "Temporary Layoff", "Permanent Job Losers", "Completed Temp Job",
  "Job Leavers", "Reentrants", "New Entrants"
)

group_order <- c("Job Separations", "Labor Force Entry")

cause_to_group <- setNames(
  rep(c("Job Separations", "Labor Force Entry"), c(4, 2)),
  cause_order
)

# -----------------------------------------------------------------------------
# 1. State-level FRED panel for UI analysis
# -----------------------------------------------------------------------------

weekly_catalog <- state_meta |>
  crossing(
    tibble(
      measure = c(
        "Initial Claims",
        "Continued Claims",
        "Insured Unemployment Rate",
        "Covered Employment"
      ),
      suffix = c("ICLAIMS", "CCLAIMS", "INSUREDUR", "CEMPLOY"),
      frequency = "Weekly"
    )
  ) |>
  mutate(series_id = paste0(state_abbr, suffix)) |>
  select(state, state_abbr, measure, series_id, frequency)

monthly_catalog <- state_meta |>
  crossing(
    tibble(
      measure = c("Unemployed Persons", "Unemployment Rate"),
      series_type = c("unemployed_persons", "unemployment_rate"),
      frequency = "Monthly"
    )
  ) |>
  mutate(
    series_id = case_when(
      series_type == "unemployed_persons" ~ paste0("LASST", fips, "0000000000004"),
      series_type == "unemployment_rate" ~ paste0(state_abbr, "UR")
    )
  ) |>
  select(state, state_abbr, measure, series_id, frequency)

annual_catalog <- state_meta |>
  transmute(
    state,
    state_abbr,
    measure = "Annual Population",
    series_id = paste0(state_abbr, "POP"),
    frequency = "Annual"
  )

fred_catalog <- bind_rows(weekly_catalog, monthly_catalog, annual_catalog) |>
  mutate(series_url = paste0("https://fred.stlouisfed.org/series/", series_id)) |>
  arrange(state, frequency, measure)

write_csv(fred_catalog, fred_catalog_cache_path)

pull_fred_series <- function(series_id, state, state_abbr, measure, frequency, series_url) {
  fredr_retry(
    series_id = series_id,
    observation_end = Sys.Date()
  ) |>
    transmute(
      state,
      state_abbr,
      measure,
      frequency,
      series_id,
      date,
      value
    )
}

validate_saved_fred_panel <- function(panel, state_meta) {
  required_columns <- c(
    "date", "state", "state_abbr",
    "initial_claims", "continued_claims", "insured_unemployment_rate",
    "covered_employment", "unemployed_persons", "unemployment_rate",
    "annual_population"
  )

  validation <- list(ok = TRUE, reasons = character())

  missing_columns <- setdiff(required_columns, names(panel))
  if (length(missing_columns) > 0) {
    validation$ok <- FALSE
    validation$reasons <- c(
      validation$reasons,
      paste0("Missing required columns: ", paste(missing_columns, collapse = ", "))
    )
    return(validation)
  }

  if (n_distinct(panel$state) != nrow(state_meta)) {
    validation$ok <- FALSE
    validation$reasons <- c(validation$reasons, "Saved panel does not contain all 50 states.")
  }

  if (n_distinct(panel$state_abbr) != nrow(state_meta)) {
    validation$ok <- FALSE
    validation$reasons <- c(validation$reasons, "Saved panel does not contain all 50 state abbreviations.")
  }

  if (!inherits(panel$date, "Date")) {
    validation$ok <- FALSE
    validation$reasons <- c(validation$reasons, "`date` column is not a Date.")
  }

  validation
}

panel_file_exists <- file.exists(fred_panel_cache_path)
meta_file_exists <- file.exists(fred_meta_cache_path)
meta_matches <- meta_file_exists &&
  identical(readLines(fred_meta_cache_path, warn = FALSE, n = 1), fred_pull_version)

fred_validation <- list(ok = FALSE, reasons = "Saved FRED panel not yet checked.")

if (panel_file_exists) {
  fred_state_month_panel <- read_csv(fred_panel_cache_path, show_col_types = FALSE) |>
    mutate(date = as.Date(date))
  fred_validation <- validate_saved_fred_panel(fred_state_month_panel, state_meta)
}

needs_fred_download <- !panel_file_exists || !meta_file_exists || !meta_matches || !fred_validation$ok

if (needs_fred_download) {
  cat("Refreshing cached FRED state-month panel...\n")

  fred_panel <- pmap_dfr(fred_catalog, pull_fred_series)

  weekly_state_month <- fred_panel |>
    filter(frequency == "Weekly") |>
    mutate(date = floor_date(date, unit = "month")) |>
    group_by(state, state_abbr, date, measure) |>
    summarise(
      value = if (first(measure) == "Initial Claims") {
        sum(value, na.rm = TRUE)
      } else {
        mean(value, na.rm = TRUE)
      },
      .groups = "drop"
    ) |>
    pivot_wider(names_from = measure, values_from = value)

  monthly_state_month <- fred_panel |>
    filter(frequency == "Monthly") |>
    group_by(state, state_abbr, date, measure) |>
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = measure, values_from = value)

  annual_state_year <- fred_panel |>
    filter(frequency == "Annual") |>
    transmute(
      state,
      state_abbr,
      year = year(date),
      annual_population = value
    ) |>
    distinct(state, state_abbr, year, .keep_all = TRUE)

  fred_state_month_panel <- weekly_state_month |>
    full_join(monthly_state_month, by = c("state", "state_abbr", "date")) |>
    mutate(year = year(date)) |>
    left_join(annual_state_year, by = c("state", "state_abbr", "year")) |>
    select(-year) |>
    arrange(state, date) |>
    rename(
      initial_claims = `Initial Claims`,
      continued_claims = `Continued Claims`,
      insured_unemployment_rate = `Insured Unemployment Rate`,
      covered_employment = `Covered Employment`,
      unemployed_persons = `Unemployed Persons`,
      unemployment_rate = `Unemployment Rate`
    )

  write_csv(fred_state_month_panel, fred_panel_cache_path)
  writeLines(fred_pull_version, fred_meta_cache_path)
} else {
  cat("Using validated cached FRED state-month panel.\n")
}

fred_state_month_panel <- fred_state_month_panel |>
  arrange(date, state)

write_final_parquet(fred_catalog, "ui_fred_series_catalog.parquet")
write_final_parquet(fred_state_month_panel, "ui_fred_monthly_state_panel.parquet")

# -----------------------------------------------------------------------------
# 2. National FRED panels used by ui-takeup-study.qmd
# -----------------------------------------------------------------------------

national_raw <- bind_rows(
  fredr_retry("CCSA", observation_start = as.Date("1994-01-01")) |>
    transmute(date, series = "CCSA", value),
  fredr_retry("ICSA", observation_start = as.Date("1994-01-01")) |>
    transmute(date, series = "ICSA", value),
  fredr_retry("COVEMP", observation_start = as.Date("1994-01-01")) |>
    transmute(date, series = "COVEMP", value)
)

ui_national_takeup_monthly <- national_raw |>
  mutate(month = floor_date(date, "month")) |>
  summarise(
    .by = c(series, month),
    value = case_when(
      first(series) == "ICSA" ~ sum(value, na.rm = TRUE),
      first(series) == "CCSA" ~ mean(value, na.rm = TRUE),
      first(series) == "COVEMP" ~ mean(value, na.rm = TRUE)
    )
  ) |>
  pivot_wider(names_from = series, values_from = value) |>
  filter(!is.na(COVEMP), COVEMP > 0) |>
  mutate(
    cc_ce = CCSA / COVEMP,
    ic_ce = ICSA / COVEMP
  ) |>
  filter(month >= as.Date("1994-01-01"))

urate_monthly <- bind_rows(
  fredr_retry("UNEMPLOY", observation_start = as.Date("1994-01-01")) |>
    transmute(date, series = "UNEMPLOY", value),
  fredr_retry("CLF16OV", observation_start = as.Date("1994-01-01")) |>
    transmute(date, series = "CLF16OV", value)
) |>
  pivot_wider(names_from = series, values_from = value) |>
  transmute(date, urate = UNEMPLOY / CLF16OV * 100)

cause_series <- c(
  "Temporary Layoff" = "LNS13023653",
  "Permanent Job Losers" = "LNS13026638",
  "Completed Temp Job" = "LNS13026637",
  "Job Leavers" = "LNS13023705",
  "Reentrants" = "LNS13023557",
  "New Entrants" = "LNS13023569"
)

cause_raw <- map_dfr(names(cause_series), \(nm) {
  fredr_retry(cause_series[[nm]], observation_start = as.Date("1994-01-01")) |>
    transmute(date, cause = nm, value)
})

national_cause_shares <- cause_raw |>
  pivot_wider(names_from = cause, values_from = value) |>
  mutate(
    component_sum = `Temporary Layoff` + `Permanent Job Losers` + `Completed Temp Job` +
      `Job Leavers` + Reentrants + `New Entrants`,
    temp_layoff_share = `Temporary Layoff` / component_sum,
    perm_job_loser_share = `Permanent Job Losers` / component_sum,
    completed_temp_job_share = `Completed Temp Job` / component_sum,
    job_leaver_share = `Job Leavers` / component_sum,
    reentrant_share = Reentrants / component_sum,
    new_entrant_share = `New Entrants` / component_sum
  ) |>
  select(date, ends_with("_share"))

ui_national_takeup_cause <- ui_national_takeup_monthly |>
  rename(date = month) |>
  inner_join(national_cause_shares, by = "date") |>
  inner_join(urate_monthly, by = "date") |>
  mutate(month_of_year = factor(month(date), levels = 1:12, labels = month.abb))

write_final_parquet(ui_national_takeup_monthly, "ui_national_takeup_monthly.parquet")
write_final_parquet(ui_national_takeup_cause, "ui_national_takeup_cause.parquet")

# -----------------------------------------------------------------------------
# 3. Load CPS microdata once and recode for downstream panels
# -----------------------------------------------------------------------------

cat("Reading unified CPS extract...\n")
ddi <- read_ipums_ddi(cps_ddi_path)
cps_raw <- read_ipums_micro(ddi, data_file = cps_data_path, verbose = FALSE)
cat("  Rows:", nrow(cps_raw), "\n")

cps_all <- cps_raw |>
  mutate(
    cause = factor(WHYUNEMP, levels = 1:6, labels = cause_order),
    group2 = cause_to_group[as.character(cause)],
    date = as.Date(paste(YEAR, MONTH, "01", sep = "-")),
    age_group = cut(
      AGE,
      breaks = c(15, 24, 34, 44, 54, 64, Inf),
      labels = c("16–24", "25–34", "35–44", "45–54", "55–64", "65+"),
      right = TRUE
    ),
    educ_group = case_when(
      EDUC <= 71 ~ "Below HS",
      EDUC == 73 ~ "HS Diploma",
      EDUC %in% c(81, 91, 92) ~ "Some College",
      EDUC >= 111 ~ "College+",
      TRUE ~ NA_character_
    ) |> factor(levels = c("Below HS", "HS Diploma", "Some College", "College+")),
    sex_label = factor(SEX, levels = 1:2, labels = c("Male", "Female")),
    race_eth = case_when(
      HISPAN > 0 & HISPAN < 900 ~ "Hispanic",
      RACE == 100 ~ "White non-Hisp",
      RACE == 200 ~ "Black non-Hisp",
      RACE >= 650 & RACE <= 652 ~ "Asian/PI non-Hisp",
      TRUE ~ "Other"
    ) |> factor(levels = c(
      "White non-Hisp", "Black non-Hisp",
      "Hispanic", "Asian/PI non-Hisp", "Other"
    )),
    occ_broad = case_when(
      OCC1990 >= 3 & OCC1990 <= 37 ~ "Mgmt & Business",
      OCC1990 >= 43 & OCC1990 <= 199 ~ "Professional",
      OCC1990 >= 203 & OCC1990 <= 235 ~ "Technicians",
      OCC1990 >= 243 & OCC1990 <= 290 ~ "Sales",
      OCC1990 >= 303 & OCC1990 <= 391 ~ "Admin Support",
      OCC1990 >= 403 & OCC1990 <= 469 ~ "Service",
      OCC1990 >= 473 & OCC1990 <= 499 ~ "Farm/Forest/Fish",
      OCC1990 >= 503 & OCC1990 <= 699 ~ "Precision Prod/Craft",
      OCC1990 >= 703 & OCC1990 <= 799 ~ "Operators/Fabricators",
      OCC1990 >= 803 & OCC1990 <= 890 ~ "Transport/Laborers",
      OCC1990 >= 903 & OCC1990 <= 905 ~ "Military",
      OCC1990 == 0 ~ "No Prior Job",
      TRUE ~ "Other/Unclassified"
    ) |> factor(levels = c(
      "Mgmt & Business", "Professional", "Technicians", "Sales",
      "Admin Support", "Service", "Farm/Forest/Fish",
      "Precision Prod/Craft", "Operators/Fabricators",
      "Transport/Laborers", "Military",
      "No Prior Job", "Other/Unclassified"
    )),
    occ_group4 = case_when(
      OCC1990 >= 3 & OCC1990 <= 391 ~ "White Collar",
      OCC1990 >= 403 & OCC1990 <= 469 ~ "Service",
      (OCC1990 >= 473 & OCC1990 <= 499) |
        (OCC1990 >= 503 & OCC1990 <= 890) ~ "Blue Collar",
      TRUE ~ "No Prior Job / Other"
    ) |> factor(levels = c(
      "White Collar", "Service", "Blue Collar", "No Prior Job / Other"
    )),
    class_label = case_when(
      CLASSWKR %in% c(20, 21, 22, 23) ~ "Private Sector",
      CLASSWKR %in% c(24, 25, 26, 27, 28) ~ "Government",
      CLASSWKR %in% c(10, 13, 14) ~ "Self-Employed",
      CLASSWKR == 29 ~ "Unpaid Family",
      CLASSWKR == 0 ~ "No Prior Job",
      TRUE ~ "Other"
    ) |> factor(levels = c(
      "Private Sector", "Government", "Self-Employed",
      "Unpaid Family", "No Prior Job", "Other"
    )),
    foreign_born = case_when(
      NATIVITY == 5 ~ "Foreign Born",
      NATIVITY %in% 1:4 ~ "Native Born",
      TRUE ~ NA_character_
    ) |> factor(levels = c("Foreign Born", "Native Born")),
    noncitizen = case_when(
      CITIZEN == 3 ~ "Noncitizen",
      CITIZEN %in% c(1, 2, 4, 5) ~ "Citizen",
      TRUE ~ NA_character_
    ) |> factor(levels = c("Noncitizen", "Citizen"))
  )

cps_unemp <- cps_all |>
  filter(WHYUNEMP %in% 1:6)

cps_lf <- cps_all |>
  filter(EMPSTAT %in% c(10, 12, 20, 21, 22))

cps_max_year <- max(cps_unemp$YEAR, na.rm = TRUE)
cps_recent_start <- as.Date(paste0(max(cps_max_year - 2, 1994), "-01-01"))

# -----------------------------------------------------------------------------
# 4. CPS state panels for UI analysis
# -----------------------------------------------------------------------------

cps_ui <- cps_raw |>
  filter(WHYUNEMP %in% 1:6) |>
  inner_join(state_lookup, by = "STATEFIP") |>
  mutate(
    is_new_entrant = as.integer(WHYUNEMP == 6L),
    is_foreign_born = as.integer(NATIVITY == 5L),
    is_noncitizen = as.integer(CITIZEN == 3L),
    is_working_age_25_54 = as.integer(AGE >= 25L & AGE <= 54L),
    is_prof_managerial = as.integer(OCC1990 >= 3L & OCC1990 <= 200L),
    is_white_collar = as.integer(OCC1990 >= 3L & OCC1990 <= 389L),
    is_service = as.integer(OCC1990 >= 403L & OCC1990 <= 469L),
    is_blue_collar = as.integer(OCC1990 >= 473L & OCC1990 <= 889L),
    is_other_occ = as.integer(!(is_white_collar == 1L | is_service == 1L | is_blue_collar == 1L)),
    is_other_services = as.integer(!(is_white_collar == 1L | is_blue_collar == 1L)),
    is_perm_job_loser = as.integer(WHYUNEMP == 2L),
    is_temp_layoff = as.integer(WHYUNEMP == 1L),
    is_completed_temp_job = as.integer(WHYUNEMP == 3L),
    is_job_leaver = as.integer(WHYUNEMP == 4L),
    is_reentrant = as.integer(WHYUNEMP == 5L),
    is_white_collar_perm_job_loser = as.integer(is_white_collar == 1L & WHYUNEMP == 2L),
    quarter = as.integer(ceiling(MONTH / 3))
  )

stopifnot(all(cps_ui$is_white_collar + cps_ui$is_blue_collar + cps_ui$is_other_services == 1L))
stopifnot(all(cps_ui$is_other_services == cps_ui$is_service + cps_ui$is_other_occ))

compute_cps_aggregates <- function(data, ...) {
  data |>
    group_by(..., state, state_abbr) |>
    summarise(
      unweighted_unemployed_n = n(),
      weighted_unemployed_total = sum(WTFINL),
      sum_w_sq = sum(WTFINL^2),
      new_entrant_unweighted_n = sum(is_new_entrant),
      foreign_born_unweighted_n = sum(is_foreign_born),
      noncitizen_unweighted_n = sum(is_noncitizen),
      working_age_25_54_unweighted_n = sum(is_working_age_25_54),
      prof_managerial_unweighted_n = sum(is_prof_managerial),
      white_collar_unweighted_n = sum(is_white_collar),
      service_unweighted_n = sum(is_service),
      blue_collar_unweighted_n = sum(is_blue_collar),
      other_occ_unweighted_n = sum(is_other_occ),
      other_services_unweighted_n = sum(is_other_services),
      perm_job_loser_unweighted_n = sum(is_perm_job_loser),
      temp_layoff_unweighted_n = sum(is_temp_layoff),
      completed_temp_job_unweighted_n = sum(is_completed_temp_job),
      job_leaver_unweighted_n = sum(is_job_leaver),
      reentrant_unweighted_n = sum(is_reentrant),
      white_collar_perm_job_loser_unweighted_n = sum(is_white_collar_perm_job_loser),
      new_entrant_share = sum(WTFINL * is_new_entrant) / sum(WTFINL),
      foreign_born_share = sum(WTFINL * is_foreign_born) / sum(WTFINL),
      noncitizen_share = sum(WTFINL * is_noncitizen) / sum(WTFINL),
      working_age_25_54_share = sum(WTFINL * is_working_age_25_54) / sum(WTFINL),
      prof_managerial_share = sum(WTFINL * is_prof_managerial) / sum(WTFINL),
      white_collar_share = sum(WTFINL * is_white_collar) / sum(WTFINL),
      service_share = sum(WTFINL * is_service) / sum(WTFINL),
      blue_collar_share = sum(WTFINL * is_blue_collar) / sum(WTFINL),
      other_occ_share = sum(WTFINL * is_other_occ) / sum(WTFINL),
      other_services_share = sum(WTFINL * is_other_services) / sum(WTFINL),
      perm_job_loser_share = sum(WTFINL * is_perm_job_loser) / sum(WTFINL),
      temp_layoff_share = sum(WTFINL * is_temp_layoff) / sum(WTFINL),
      completed_temp_job_share = sum(WTFINL * is_completed_temp_job) / sum(WTFINL),
      job_leaver_share = sum(WTFINL * is_job_leaver) / sum(WTFINL),
      reentrant_share = sum(WTFINL * is_reentrant) / sum(WTFINL),
      white_collar_perm_job_loser_share = sum(WTFINL * is_white_collar_perm_job_loser) / sum(WTFINL),
      .groups = "drop"
    ) |>
    mutate(
      effective_n = weighted_unemployed_total^2 / sum_w_sq,
      new_entrant_se = sqrt(new_entrant_share * (1 - new_entrant_share) / effective_n),
      foreign_born_se = sqrt(foreign_born_share * (1 - foreign_born_share) / effective_n),
      noncitizen_se = sqrt(noncitizen_share * (1 - noncitizen_share) / effective_n),
      new_entrant_moe95 = 1.96 * new_entrant_se,
      foreign_born_moe95 = 1.96 * foreign_born_se,
      noncitizen_moe95 = 1.96 * noncitizen_se
    )
}

cat("Computing CPS UI state panels...\n")

cps_month <- compute_cps_aggregates(cps_ui, YEAR, MONTH) |>
  mutate(
    period_start = as.Date(paste(YEAR, MONTH, "01", sep = "-")),
    year = YEAR,
    month = as.integer(MONTH),
    quarter = NA_integer_
  ) |>
  select(-YEAR, -MONTH)

cps_quarter <- compute_cps_aggregates(cps_ui, YEAR, quarter) |>
  mutate(
    period_start = as.Date(paste(YEAR, (quarter - 1L) * 3L + 1L, "01", sep = "-")),
    year = YEAR,
    month = NA_integer_
  ) |>
  rename(quarter_val = quarter) |>
  mutate(quarter = as.integer(quarter_val)) |>
  select(-YEAR, -quarter_val)

cps_year <- compute_cps_aggregates(cps_ui, YEAR) |>
  mutate(
    period_start = as.Date(paste(YEAR, "01", "01", sep = "-")),
    year = YEAR,
    month = NA_integer_,
    quarter = NA_integer_
  ) |>
  select(-YEAR)

cps_state_month_panel <- cps_month |>
  transmute(
    date = period_start,
    state,
    state_abbr,
    unemployed_total = weighted_unemployed_total,
    new_entrant_share,
    foreign_born_share,
    noncitizen_share,
    working_age_25_54_share,
    prof_managerial_share,
    white_collar_share,
    blue_collar_share,
    other_services_share,
    service_share,
    other_occ_share,
    perm_job_loser_share,
    temp_layoff_share,
    completed_temp_job_share,
    job_leaver_share,
    reentrant_share,
    white_collar_perm_job_loser_share
  ) |>
  arrange(date, state)

fred_month <- fred_state_month_panel |>
  transmute(
    period_start = date,
    state,
    state_abbr,
    initial_claims,
    continued_claims,
    insured_unemployment_rate,
    covered_employment,
    unemployed_persons,
    unemployment_rate,
    annual_population
  )

fred_quarter <- fred_state_month_panel |>
  mutate(year = year(date), quarter = quarter(date)) |>
  group_by(year, quarter, state, state_abbr) |>
  summarise(
    initial_claims = sum(initial_claims, na.rm = TRUE),
    across(
      c(
        continued_claims, insured_unemployment_rate, covered_employment,
        unemployed_persons, unemployment_rate
      ),
      \(x) mean(x, na.rm = TRUE)
    ),
    annual_population = first(annual_population),
    .groups = "drop"
  ) |>
  mutate(period_start = as.Date(paste(year, (quarter - 1L) * 3L + 1L, "01", sep = "-"))) |>
  select(-year, -quarter)

fred_year <- fred_state_month_panel |>
  mutate(year = year(date)) |>
  group_by(year, state, state_abbr) |>
  summarise(
    initial_claims = sum(initial_claims, na.rm = TRUE),
    across(
      c(
        continued_claims, insured_unemployment_rate, covered_employment,
        unemployed_persons, unemployment_rate
      ),
      \(x) mean(x, na.rm = TRUE)
    ),
    annual_population = first(annual_population),
    .groups = "drop"
  ) |>
  mutate(period_start = as.Date(paste(year, "01", "01", sep = "-"))) |>
  select(-year)

merge_and_derive <- function(cps_agg, fred_agg) {
  cps_agg |>
    left_join(fred_agg, by = c("period_start", "state", "state_abbr")) |>
    mutate(
      insured_unemployment_rate_share = insured_unemployment_rate / 100,
      covered_employment = coalesce(
        covered_employment,
        continued_claims / insured_unemployment_rate_share
      ),
      initial_claims_per_covered_employment = initial_claims / covered_employment,
      continued_claims_per_covered_employment = continued_claims / covered_employment,
      initial_claims_per_unemployed = initial_claims / unemployed_persons,
      continued_claims_per_unemployed = continued_claims / unemployed_persons
    )
}

panel_cols <- c(
  "period_start", "year", "quarter", "month", "state", "state_abbr",
  "unweighted_unemployed_n", "weighted_unemployed_total", "sum_w_sq", "effective_n",
  "new_entrant_unweighted_n", "foreign_born_unweighted_n", "noncitizen_unweighted_n",
  "working_age_25_54_unweighted_n",
  "prof_managerial_unweighted_n", "white_collar_unweighted_n", "service_unweighted_n",
  "blue_collar_unweighted_n", "other_occ_unweighted_n", "other_services_unweighted_n",
  "perm_job_loser_unweighted_n", "temp_layoff_unweighted_n",
  "completed_temp_job_unweighted_n", "job_leaver_unweighted_n", "reentrant_unweighted_n",
  "white_collar_perm_job_loser_unweighted_n",
  "new_entrant_share", "foreign_born_share", "noncitizen_share", "working_age_25_54_share",
  "prof_managerial_share", "white_collar_share", "blue_collar_share", "other_services_share",
  "service_share", "other_occ_share",
  "perm_job_loser_share", "temp_layoff_share", "completed_temp_job_share",
  "job_leaver_share", "reentrant_share", "white_collar_perm_job_loser_share",
  "new_entrant_se", "foreign_born_se", "noncitizen_se",
  "new_entrant_moe95", "foreign_born_moe95", "noncitizen_moe95",
  "initial_claims", "continued_claims", "insured_unemployment_rate",
  "covered_employment", "annual_population",
  "unemployed_persons", "unemployment_rate",
  "initial_claims_per_covered_employment", "continued_claims_per_covered_employment",
  "initial_claims_per_unemployed", "continued_claims_per_unemployed",
  "insured_unemployment_rate_share"
)

panel_month_full <- merge_and_derive(cps_month, fred_month) |>
  select(all_of(panel_cols)) |>
  arrange(period_start, state)

panel_quarter_full <- merge_and_derive(cps_quarter, fred_quarter) |>
  select(all_of(panel_cols)) |>
  arrange(period_start, state)

panel_year_full <- merge_and_derive(cps_year, fred_year) |>
  select(all_of(panel_cols)) |>
  arrange(period_start, state)

ar_filter <- function(panel) {
  numeric_vars <- c(
    "new_entrant_share", "foreign_born_share", "noncitizen_share", "working_age_25_54_share",
    "prof_managerial_share", "white_collar_share", "blue_collar_share", "other_services_share",
    "service_share", "other_occ_share",
    "perm_job_loser_share", "temp_layoff_share", "completed_temp_job_share",
    "job_leaver_share", "reentrant_share", "white_collar_perm_job_loser_share",
    "annual_population",
    "initial_claims_per_covered_employment", "continued_claims_per_covered_employment",
    "insured_unemployment_rate_share"
  )

  panel |>
    filter(if_all(all_of(numeric_vars), \(x) !is.na(x) & is.finite(x)))
}

panel_month_ar <- ar_filter(panel_month_full)
panel_quarter_ar <- ar_filter(panel_quarter_full)
panel_year_ar <- ar_filter(panel_year_full)

precision_cols <- c(
  "period_start", "year", "quarter", "month", "state", "state_abbr",
  "effective_n", "unweighted_unemployed_n",
  "new_entrant_share", "foreign_born_share", "noncitizen_share",
  "new_entrant_se", "foreign_born_se", "noncitizen_se",
  "new_entrant_moe95", "foreign_born_moe95", "noncitizen_moe95",
  "new_entrant_unweighted_n", "foreign_born_unweighted_n", "noncitizen_unweighted_n"
)

compute_precision_summary <- function(precision_df, freq_label) {
  measures <- c("new_entrant_share", "foreign_born_share", "noncitizen_share")
  se_cols <- c("new_entrant_se", "foreign_born_se", "noncitizen_se")
  moe_cols <- c("new_entrant_moe95", "foreign_born_moe95", "noncitizen_moe95")
  num_cols <- c("new_entrant_unweighted_n", "foreign_born_unweighted_n", "noncitizen_unweighted_n")

  map_dfr(seq_along(measures), function(i) {
    tibble(
      frequency = freq_label,
      measure = measures[i],
      periods = nrow(precision_df),
      median_effective_n = median(precision_df$effective_n, na.rm = TRUE),
      p10_effective_n = quantile(precision_df$effective_n, 0.10, na.rm = TRUE),
      median_share = median(precision_df[[measures[i]]], na.rm = TRUE),
      median_se = median(precision_df[[se_cols[i]]], na.rm = TRUE),
      p90_se = quantile(precision_df[[se_cols[i]]], 0.90, na.rm = TRUE),
      median_moe95 = median(precision_df[[moe_cols[i]]], na.rm = TRUE),
      p90_moe95 = quantile(precision_df[[moe_cols[i]]], 0.90, na.rm = TRUE),
      median_unweighted_numerator = median(precision_df[[num_cols[i]]], na.rm = TRUE),
      p10_unweighted_numerator = quantile(precision_df[[num_cols[i]]], 0.10, na.rm = TRUE)
    )
  })
}

prec_month <- panel_month_full |> select(all_of(precision_cols))
prec_quarter <- panel_quarter_full |> select(all_of(precision_cols))
prec_year <- panel_year_full |> select(all_of(precision_cols))

cps_precision_comparison <- bind_rows(
  compute_precision_summary(prec_month, "month"),
  compute_precision_summary(prec_quarter, "quarter"),
  compute_precision_summary(prec_year, "year")
)

cps_precision_summary <- compute_precision_summary(prec_month, "month") |>
  rename(state_months = periods) |>
  select(-frequency)

write_final_parquet(cps_state_month_panel, "cps_ui_state_month_panel.parquet")
write_final_parquet(panel_month_full, "ui_takeup_panel_state_month.parquet")
write_final_parquet(panel_month_ar, "ui_takeup_panel_state_month_analysis_ready.parquet")
write_final_parquet(panel_quarter_full, "ui_takeup_panel_state_quarter.parquet")
write_final_parquet(panel_quarter_ar, "ui_takeup_panel_state_quarter_analysis_ready.parquet")
write_final_parquet(panel_year_full, "ui_takeup_panel_state_year.parquet")
write_final_parquet(panel_year_ar, "ui_takeup_panel_state_year_analysis_ready.parquet")
write_final_parquet(prec_month, "cps_month_precision.parquet")
write_final_parquet(prec_quarter, "cps_quarter_precision.parquet")
write_final_parquet(prec_year, "cps_year_precision.parquet")
write_final_parquet(cps_precision_comparison, "cps_precision_comparison.parquet")
write_final_parquet(cps_precision_summary, "cps_precision_summary.parquet")

# -----------------------------------------------------------------------------
# 5. Unemployment decomposition FRED summaries
# -----------------------------------------------------------------------------

series_ids_6 <- c(
  "LNS13023653",
  "LNS13026638",
  "LNS13026637",
  "LNS13023705",
  "LNS13023557",
  "LNS13023569",
  "UNEMPLOY",
  "CLF16OV"
)

series_names_6 <- c(
  "Temporary Layoff", "Permanent Job Losers", "Completed Temp Job",
  "Job Leavers", "Reentrants", "New Entrants", "Total", "CLF"
)

raw6 <- map2_dfr(series_ids_6, series_names_6, \(id, nm) {
  fredr_retry(
    series_id = id,
    observation_start = as.Date("1994-01-01"),
    observation_end = Sys.Date()
  ) |>
    transmute(date, series = nm, value)
})

unemployment_fred_monthly <- raw6 |>
  pivot_wider(names_from = series, values_from = value) |>
  drop_na() |>
  mutate(
    across(all_of(cause_order), \(x) x / CLF * 100, .names = "{.col}_rate"),
    urate = Total / CLF * 100,
    `Job Separations` = `Temporary Layoff` + `Permanent Job Losers` +
      `Completed Temp Job` + `Job Leavers`,
    `Labor Force Entry` = Reentrants + `New Entrants`,
    `Job Separations_rate` = `Job Separations` / CLF * 100,
    `Labor Force Entry_rate` = `Labor Force Entry` / CLF * 100
  ) |>
  arrange(date)

write_final_parquet(unemployment_fred_monthly, "unemployment_fred_monthly.parquet")

# -----------------------------------------------------------------------------
# 6. Unemployment decomposition CPS nativity / citizenship summaries
# -----------------------------------------------------------------------------

nat_shares_wide <- cps_unemp |>
  filter(!is.na(foreign_born)) |>
  count(date, group2, foreign_born, wt = WTFINL, name = "n") |>
  group_by(date, group2) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  unite("col", group2, foreign_born) |>
  select(date, col, share) |>
  pivot_wider(names_from = col, values_from = share, values_fill = 0)

cit_shares_wide <- cps_unemp |>
  filter(!is.na(noncitizen)) |>
  count(date, group2, noncitizen, wt = WTFINL, name = "n") |>
  group_by(date, group2) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  unite("col", group2, noncitizen) |>
  select(date, col, share) |>
  pivot_wider(names_from = col, values_from = share, values_fill = 0)

unemployment_fourway_nativity <- unemployment_fred_monthly |>
  select(date, `Job Separations_rate`, `Labor Force Entry_rate`, urate) |>
  inner_join(nat_shares_wide, by = "date") |>
  mutate(
    `Foreign Born × Separations_rate` = `Job Separations_rate` * `Job Separations_Foreign Born`,
    `Native Born × Separations_rate` = `Job Separations_rate` * `Job Separations_Native Born`,
    `Foreign Born × Entry_rate` = `Labor Force Entry_rate` * `Labor Force Entry_Foreign Born`,
    `Native Born × Entry_rate` = `Labor Force Entry_rate` * `Labor Force Entry_Native Born`
  ) |>
  select(
    date,
    urate,
    `Foreign Born × Separations_rate`,
    `Native Born × Separations_rate`,
    `Foreign Born × Entry_rate`,
    `Native Born × Entry_rate`
  ) |>
  arrange(date)

unemployment_fourway_citizenship <- unemployment_fred_monthly |>
  select(date, `Job Separations_rate`, `Labor Force Entry_rate`, urate) |>
  inner_join(cit_shares_wide, by = "date") |>
  mutate(
    `Noncitizen × Separations_rate` = `Job Separations_rate` * `Job Separations_Noncitizen`,
    `Citizen × Separations_rate` = `Job Separations_rate` * `Job Separations_Citizen`,
    `Noncitizen × Entry_rate` = `Labor Force Entry_rate` * `Labor Force Entry_Noncitizen`,
    `Citizen × Entry_rate` = `Labor Force Entry_rate` * `Labor Force Entry_Citizen`
  ) |>
  select(
    date,
    urate,
    `Noncitizen × Separations_rate`,
    `Citizen × Separations_rate`,
    `Noncitizen × Entry_rate`,
    `Citizen × Entry_rate`
  ) |>
  arrange(date)

unemp_monthly <- cps_unemp |>
  filter(!is.na(foreign_born)) |>
  group_by(date) |>
  summarise(
    total_unemp = sum(WTFINL),
    lf_entry = sum(WTFINL[group2 == "Labor Force Entry"]),
    job_sep = sum(WTFINL[group2 == "Job Separations"]),
    fb = sum(WTFINL[foreign_born == "Foreign Born"]),
    nb = sum(WTFINL[foreign_born == "Native Born"]),
    fb_entry = sum(WTFINL[foreign_born == "Foreign Born" & group2 == "Labor Force Entry"]),
    fb_sep = sum(WTFINL[foreign_born == "Foreign Born" & group2 == "Job Separations"]),
    .groups = "drop"
  )

lf_monthly <- cps_lf |>
  filter(!is.na(foreign_born)) |>
  group_by(date) |>
  summarise(
    total_lf = sum(WTFINL),
    fb_lf = sum(WTFINL[foreign_born == "Foreign Born"]),
    .groups = "drop"
  )

series_order <- c(
  "LF Entrants", "Job Separators",
  "Foreign Born", "Native Born",
  "LF Entrants × FB", "Job Separators × FB"
)

shares_unemp <- unemp_monthly |>
  transmute(
    date,
    `LF Entrants` = lf_entry / total_unemp * 100,
    `Job Separators` = job_sep / total_unemp * 100,
    `Foreign Born` = fb / total_unemp * 100,
    `Native Born` = nb / total_unemp * 100,
    `LF Entrants × FB` = fb_entry / total_unemp * 100,
    `Job Separators × FB` = fb_sep / total_unemp * 100
  ) |>
  pivot_longer(-date, names_to = "series", values_to = "share") |>
  mutate(series = factor(series, levels = series_order)) |>
  arrange(series, date) |>
  group_by(series) |>
  mutate(ma12 = zoo::rollmean(share, k = 12, fill = NA, align = "right")) |>
  ungroup() |>
  mutate(panel = "share_of_total_unemployed")

fb_within_order <- c("All Unemployed", "LF Entrants", "Job Separators", "Labor Force")

fb_within <- unemp_monthly |>
  inner_join(lf_monthly, by = "date") |>
  transmute(
    date,
    `All Unemployed` = fb / total_unemp * 100,
    `LF Entrants` = fb_entry / lf_entry * 100,
    `Job Separators` = fb_sep / job_sep * 100,
    `Labor Force` = fb_lf / total_lf * 100
  ) |>
  pivot_longer(-date, names_to = "series", values_to = "share") |>
  mutate(series = factor(series, levels = fb_within_order)) |>
  arrange(series, date) |>
  group_by(series) |>
  mutate(ma12 = zoo::rollmean(share, k = 12, fill = NA, align = "right")) |>
  ungroup() |>
  mutate(panel = "foreign_born_share_within_group")

unemployment_nativity_shares <- bind_rows(shares_unemp, fb_within) |>
  arrange(panel, series, date)

write_final_parquet(unemployment_fourway_nativity, "unemployment_fourway_nativity.parquet")
write_final_parquet(unemployment_fourway_citizenship, "unemployment_fourway_citizenship.parquet")
write_final_parquet(unemployment_nativity_shares, "unemployment_nativity_shares.parquet")

# -----------------------------------------------------------------------------
# 7. Unemployment decomposition demographic summaries
# -----------------------------------------------------------------------------

build_composition_summary <- function(data, demo_var, dimension, frequency) {
  base <- data |>
    filter(!is.na(.data[[demo_var]]))

  out <- switch(
    frequency,
    pooled = base |>
      group_by(cause, .data[[demo_var]]) |>
      summarise(n = sum(WTFINL, na.rm = TRUE), .groups = "drop") |>
      rename(group = 2) |>
      group_by(cause) |>
      mutate(share = n / sum(n)) |>
      ungroup() |>
      mutate(YEAR = NA_integer_, date = as.Date(NA)),
    year = base |>
      group_by(YEAR, cause, .data[[demo_var]]) |>
      summarise(n = sum(WTFINL, na.rm = TRUE), .groups = "drop") |>
      rename(group = 3) |>
      group_by(YEAR, cause) |>
      mutate(share = n / sum(n)) |>
      ungroup() |>
      mutate(date = as.Date(NA)),
    month = base |>
      group_by(date, cause, .data[[demo_var]]) |>
      summarise(n = sum(WTFINL, na.rm = TRUE), .groups = "drop") |>
      rename(group = 3) |>
      group_by(date, cause) |>
      mutate(share = n / sum(n)) |>
      ungroup() |>
      mutate(YEAR = year(date))
  )

  out |>
    mutate(
      metric = "composition",
      frequency = frequency,
      dimension = dimension,
      cause_share = NA_real_,
      lf_share = NA_real_,
      ratio = NA_real_
    ) |>
    select(metric, frequency, dimension, YEAR, date, cause, group, n, share, cause_share, lf_share, ratio)
}

build_ratio_summary <- function(unemp_data, lf_data, demo_var, dimension, frequency) {
  unemp_base <- unemp_data |>
    filter(!is.na(.data[[demo_var]]))
  lf_base <- lf_data |>
    filter(!is.na(.data[[demo_var]]))

  out <- switch(
    frequency,
    pooled = {
      cause_shares <- unemp_base |>
        group_by(cause, .data[[demo_var]]) |>
        summarise(n = sum(WTFINL, na.rm = TRUE), .groups = "drop") |>
        rename(group = 2) |>
        group_by(cause) |>
        mutate(cause_share = n / sum(n)) |>
        ungroup()

      lf_shares <- lf_base |>
        group_by(.data[[demo_var]]) |>
        summarise(n_lf = sum(WTFINL, na.rm = TRUE), .groups = "drop") |>
        rename(group = 1) |>
        mutate(lf_share = n_lf / sum(n_lf)) |>
        select(group, lf_share)

      cause_shares |>
        left_join(lf_shares, by = "group") |>
        mutate(YEAR = NA_integer_, date = as.Date(NA))
    },
    year = {
      cause_shares <- unemp_base |>
        group_by(YEAR, cause, .data[[demo_var]]) |>
        summarise(n = sum(WTFINL, na.rm = TRUE), .groups = "drop") |>
        rename(group = 3) |>
        group_by(YEAR, cause) |>
        mutate(cause_share = n / sum(n)) |>
        ungroup()

      lf_shares <- lf_base |>
        group_by(YEAR, .data[[demo_var]]) |>
        summarise(n_lf = sum(WTFINL, na.rm = TRUE), .groups = "drop") |>
        rename(group = 2) |>
        group_by(YEAR) |>
        mutate(lf_share = n_lf / sum(n_lf)) |>
        ungroup() |>
        select(YEAR, group, lf_share)

      cause_shares |>
        left_join(lf_shares, by = c("YEAR", "group")) |>
        mutate(date = as.Date(NA))
    },
    month = {
      cause_shares <- unemp_base |>
        group_by(date, cause, .data[[demo_var]]) |>
        summarise(n = sum(WTFINL, na.rm = TRUE), .groups = "drop") |>
        rename(group = 3) |>
        group_by(date, cause) |>
        mutate(cause_share = n / sum(n)) |>
        ungroup()

      lf_shares <- lf_base |>
        group_by(date, .data[[demo_var]]) |>
        summarise(n_lf = sum(WTFINL, na.rm = TRUE), .groups = "drop") |>
        rename(group = 2) |>
        group_by(date) |>
        mutate(lf_share = n_lf / sum(n_lf)) |>
        ungroup() |>
        select(date, group, lf_share)

      cause_shares |>
        left_join(lf_shares, by = c("date", "group")) |>
        mutate(YEAR = year(date))
    }
  )

  out |>
    mutate(
      metric = "ratio",
      frequency = frequency,
      dimension = dimension,
      share = NA_real_,
      ratio = cause_share / lf_share
    ) |>
    select(metric, frequency, dimension, YEAR, date, cause, group, n, share, cause_share, lf_share, ratio)
}

demo_specs <- tribble(
  ~var, ~dimension,
  "age_group", "age_group",
  "educ_group", "educ_group",
  "sex_label", "sex_label",
  "race_eth", "race_eth"
)

unemployment_demo_summary <- bind_rows(
  pmap_dfr(demo_specs, \(var, dimension) build_composition_summary(cps_unemp, var, dimension, "pooled")),
  pmap_dfr(demo_specs, \(var, dimension) build_composition_summary(cps_unemp, var, dimension, "year")),
  pmap_dfr(demo_specs, \(var, dimension) build_composition_summary(cps_unemp, var, dimension, "month")),
  pmap_dfr(demo_specs, \(var, dimension) build_ratio_summary(cps_unemp, cps_lf, var, dimension, "pooled")),
  pmap_dfr(demo_specs, \(var, dimension) build_ratio_summary(cps_unemp, cps_lf, var, dimension, "year")),
  pmap_dfr(demo_specs, \(var, dimension) build_ratio_summary(cps_unemp, cps_lf, var, dimension, "month"))
) |>
  arrange(metric, frequency, dimension, cause, group, YEAR, date)

write_final_parquet(unemployment_demo_summary, "unemployment_demo_summary.parquet")

# -----------------------------------------------------------------------------
# 8. Unemployment decomposition dominant profiles
# -----------------------------------------------------------------------------

profile_shares <- function(data, demo_var, dimension_label) {
  data |>
    filter(!is.na(.data[[demo_var]])) |>
    group_by(cause, .data[[demo_var]]) |>
    summarise(n = sum(WTFINL, na.rm = TRUE), .groups = "drop") |>
    rename(group = 2) |>
    group_by(cause) |>
    mutate(share = n / sum(n)) |>
    ungroup() |>
    transmute(
      cause,
      dimension = dimension_label,
      group_key = as.character(group),
      share
    )
}

build_profile_shares <- function(data) {
  bind_rows(
    profile_shares(data, "age_group", "Age"),
    profile_shares(data, "educ_group", "Education"),
    profile_shares(data, "sex_label", "Sex"),
    profile_shares(data, "race_eth", "Race/Ethnicity"),
    profile_shares(data, "occ_group4", "Occupation"),
    profile_shares(data, "class_label", "Class"),
    profile_shares(data, "foreign_born", "Foreign Born"),
    profile_shares(data, "noncitizen", "Noncitizen")
  ) |>
    mutate(
      dimension = factor(
        dimension,
        levels = c(
          "Age", "Education", "Sex", "Race/Ethnicity",
          "Occupation", "Class", "Foreign Born", "Noncitizen"
        )
      )
    )
}

build_dominant_profile <- function(data) {
  build_profile_shares(data) |>
    group_by(cause, dimension) |>
    slice_max(order_by = share, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(
      dominant_group = group_key,
      label = paste0(dominant_group, "\n", scales::percent(share, accuracy = 1)),
      label_y = pmin(share + 0.08, 1.05)
    ) |>
    arrange(cause, dimension)
}

build_dominant_profile_delta <- function(current_data, reference_data) {
  current_profile <- build_dominant_profile(current_data) |>
    select(cause, dimension, group_key, dominant_group, share_2025 = share)

  reference_profile <- build_profile_shares(reference_data) |>
    rename(share_pooled = share)

  current_profile |>
    left_join(reference_profile, by = c("cause", "dimension", "group_key")) |>
    mutate(
      delta = share_2025 - share_pooled,
      label = paste0(
        dominant_group,
        "\n",
        if_else(delta > 0, "+", ""),
        scales::number(delta * 100, accuracy = 0.1),
        " pp"
      )
    ) |>
    arrange(cause, dimension)
}

dominant_profile_pooled <- build_dominant_profile(cps_unemp) |>
  mutate(variant = "pooled")

dominant_profile_2025 <- build_dominant_profile(cps_unemp |> filter(YEAR == 2025)) |>
  mutate(variant = "year_2025")

dominant_profile_delta_2025 <- build_dominant_profile_delta(
  cps_unemp |> filter(YEAR == 2025),
  cps_unemp
) |>
  mutate(variant = "delta_2025")

unemployment_dominant_profiles <- bind_rows(
  dominant_profile_pooled,
  dominant_profile_2025,
  dominant_profile_delta_2025
)

write_final_parquet(unemployment_dominant_profiles, "unemployment_dominant_profiles.parquet")

# -----------------------------------------------------------------------------
# 9. Metadata / codebooks for report-facing tables
# -----------------------------------------------------------------------------

unemployment_metadata <- tibble(
  cps_max_year = cps_max_year,
  cps_recent_start = cps_recent_start,
  generated_at = Sys.time()
)

write_final_parquet(unemployment_metadata, "unemployment_metadata.parquet")

cps_occ_codebook <- ipums_val_labels(ddi, "OCC1990") |>
  transmute(
    occ1990 = val,
    occ1990_name = lbl,
    broad_group = case_when(
      between(val, 3, 389) ~ "White Collar",
      between(val, 473, 889) ~ "Blue Collar",
      TRUE ~ "Other Services"
    )
  )

write_final_parquet(cps_occ_codebook, "cps_occ1990_broad_codebook.parquet")

cat("\n=== build_panels.R complete ===\n")
cat("Saved parquet files to:", final_dir, "\n")
