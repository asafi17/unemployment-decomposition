# ──────────────────────────────────────────────────────────────────────────────
# build_panels.R
#
# Rebuilds all intermediate data panels from the unified CPS extract + FRED
# panel for ui-takeup-study.qmd.
#
# Inputs:
#   data/CPS/cps_unified.xml + data/CPS/cps_unified.csv.gz  (IPUMS CPS)
#   data/FRED/ui_takeup_fred_monthly_state_panel.csv         (FRED)
#
# Outputs:
#   data/CPS/cps_ui_state_month_panel.csv           (simplified CPS panel)
#   data/final/ui_takeup_panel_state_month.csv       (full merged monthly)
#   data/final/ui_takeup_panel_state_month_analysis_ready.csv
#   data/final/ui_takeup_panel_state_quarter.csv     (full merged quarterly)
#   data/final/ui_takeup_panel_state_quarter_analysis_ready.csv
#   data/final/ui_takeup_panel_state_year.csv        (full merged yearly)
#   data/final/ui_takeup_panel_state_year_analysis_ready.csv
#   data/final/cps_month_precision.csv
#   data/final/cps_quarter_precision.csv
#   data/final/cps_year_precision.csv
#   data/final/cps_precision_comparison.csv          (9 rows: 3 freq x 3 measures)
#   data/final/cps_precision_summary.csv             (3 rows: monthly only, optional)
#
# Usage:
#   Rscript build_panels.R
# ──────────────────────────────────────────────────────────────────────────────

library(ipumsr)
library(here)
library(tidyverse)

here::i_am("build_panels.R")

# ── 0. Paths ────────────────────────────────────────────────────────────────

cps_ddi_path  <- here("data", "CPS", "cps_unified.xml")
cps_data_path <- here("data", "CPS", "cps_unified.csv.gz")
fred_path     <- here("data", "FRED", "ui_takeup_fred_monthly_state_panel.csv")
final_dir     <- here("data", "final")
cps_panel_path <- here("data", "CPS", "cps_ui_state_month_panel.csv")

stopifnot(file.exists(cps_ddi_path), file.exists(cps_data_path), file.exists(fred_path))

dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)

# ── 1. STATEFIP lookup (50 states only, no DC/territories) ─────────────────

state_lookup <- tibble(
  STATEFIP = c(1L,2L,4L,5L,6L,8L,9L,10L,12L,13L,
               15L,16L,17L,18L,19L,20L,21L,22L,23L,24L,
               25L,26L,27L,28L,29L,30L,31L,32L,33L,34L,
               35L,36L,37L,38L,39L,40L,41L,42L,44L,45L,
               46L,47L,48L,49L,50L,51L,53L,54L,55L,56L),
  state = state.name,
  state_abbr = state.abb
)

# ── 2. Load CPS microdata and filter to unemployed with valid WHYUNEMP ────

cat("Reading unified CPS extract...\n")
ddi     <- read_ipums_ddi(cps_ddi_path)
cps_raw <- read_ipums_micro(ddi, data_file = cps_data_path, verbose = FALSE)
cat("  Rows:", nrow(cps_raw), "\n")

cps <- cps_raw |>
  filter(WHYUNEMP %in% 1:6) |>
  inner_join(state_lookup, by = "STATEFIP") |>
  mutate(
    is_new_entrant = as.integer(WHYUNEMP == 6L),
    is_foreign_born = as.integer(NATIVITY == 5L),
    is_noncitizen = as.integer(CITIZEN == 5L),
    is_working_age_25_54 = as.integer(AGE >= 25L & AGE <= 54L),
    is_prof_managerial = as.integer(OCC1990 >= 3L & OCC1990 <= 200L),
    # Broad occupation groups follow discontinued BLS NCS 2007 Table 2D:
    # white-collar = professional/technical, executive/administrative/managerial,
    # sales, and administrative support; blue-collar = precision production/craft/
    # repair, machine operators/assemblers/inspectors, transportation/material
    # moving, and handlers/helpers/laborers. Other services is everyone else.
    is_white_collar = as.integer(OCC1990 >= 3L & OCC1990 <= 389L),
    is_service = as.integer(OCC1990 >= 403L & OCC1990 <= 469L),
    is_blue_collar = as.integer(OCC1990 >= 473L & OCC1990 <= 889L),
    is_other_occ = as.integer(!(is_white_collar == 1L | is_service == 1L | is_blue_collar == 1L)),
    is_other_services = as.integer(!(is_white_collar == 1L | is_blue_collar == 1L)),
    is_perm_job_loser = as.integer(WHYUNEMP == 2L),
    is_temp_layoff = as.integer(WHYUNEMP == 1L),
    is_white_collar_perm_job_loser = as.integer(is_white_collar == 1L & WHYUNEMP == 2L),
    quarter = as.integer(ceiling(MONTH / 3))
  )

stopifnot(all(cps$is_white_collar + cps$is_blue_collar + cps$is_other_services == 1L))
stopifnot(all(cps$is_other_services == cps$is_service + cps$is_other_occ))

cat("  Unemployed records (50 states):", nrow(cps), "\n")

# ── 3. Helper: compute CPS aggregates for a given grouping ────────────────

compute_cps_aggregates <- function(data, ...) {
  data |>
    group_by(..., state, state_abbr) |>
    summarise(
      unweighted_unemployed_n     = n(),
      weighted_unemployed_total   = sum(WTFINL),
      sum_w_sq                    = sum(WTFINL^2),
      new_entrant_unweighted_n    = sum(is_new_entrant),
      foreign_born_unweighted_n   = sum(is_foreign_born),
      noncitizen_unweighted_n     = sum(is_noncitizen),
      working_age_25_54_unweighted_n = sum(is_working_age_25_54),
      prof_managerial_unweighted_n = sum(is_prof_managerial),
      white_collar_unweighted_n   = sum(is_white_collar),
      service_unweighted_n        = sum(is_service),
      blue_collar_unweighted_n    = sum(is_blue_collar),
      other_occ_unweighted_n      = sum(is_other_occ),
      other_services_unweighted_n = sum(is_other_services),
      perm_job_loser_unweighted_n = sum(is_perm_job_loser),
      temp_layoff_unweighted_n    = sum(is_temp_layoff),
      white_collar_perm_job_loser_unweighted_n = sum(is_white_collar_perm_job_loser),
      new_entrant_share           = sum(WTFINL * is_new_entrant) / sum(WTFINL),
      foreign_born_share          = sum(WTFINL * is_foreign_born) / sum(WTFINL),
      noncitizen_share            = sum(WTFINL * is_noncitizen) / sum(WTFINL),
      working_age_25_54_share     = sum(WTFINL * is_working_age_25_54) / sum(WTFINL),
      prof_managerial_share       = sum(WTFINL * is_prof_managerial) / sum(WTFINL),
      white_collar_share          = sum(WTFINL * is_white_collar) / sum(WTFINL),
      service_share               = sum(WTFINL * is_service) / sum(WTFINL),
      blue_collar_share           = sum(WTFINL * is_blue_collar) / sum(WTFINL),
      other_occ_share             = sum(WTFINL * is_other_occ) / sum(WTFINL),
      other_services_share        = sum(WTFINL * is_other_services) / sum(WTFINL),
      perm_job_loser_share        = sum(WTFINL * is_perm_job_loser) / sum(WTFINL),
      temp_layoff_share           = sum(WTFINL * is_temp_layoff) / sum(WTFINL),
      white_collar_perm_job_loser_share = sum(WTFINL * is_white_collar_perm_job_loser) / sum(WTFINL),
      .groups = "drop"
    ) |>
    mutate(
      effective_n        = weighted_unemployed_total^2 / sum_w_sq,
      new_entrant_se     = sqrt(new_entrant_share * (1 - new_entrant_share) / effective_n),
      foreign_born_se    = sqrt(foreign_born_share * (1 - foreign_born_share) / effective_n),
      noncitizen_se      = sqrt(noncitizen_share * (1 - noncitizen_share) / effective_n),
      new_entrant_moe95  = 1.96 * new_entrant_se,
      foreign_born_moe95 = 1.96 * foreign_born_se,
      noncitizen_moe95   = 1.96 * noncitizen_se
    )
}

# ── 4. CPS aggregates at month / quarter / year ──────────────────────────

cat("Computing CPS state-month aggregates...\n")
cps_month <- compute_cps_aggregates(cps, YEAR, MONTH) |>
  mutate(
    period_start = as.Date(paste(YEAR, MONTH, "01", sep = "-")),
    year = YEAR, month = as.integer(MONTH), quarter = NA_integer_
  ) |>
  select(-YEAR, -MONTH)

cat("Computing CPS state-quarter aggregates...\n")
cps_quarter <- compute_cps_aggregates(cps, YEAR, quarter) |>
  mutate(
    period_start = as.Date(paste(YEAR, (quarter - 1L) * 3L + 1L, "01", sep = "-")),
    year = YEAR, month = NA_integer_
  ) |>
  rename(quarter_val = quarter) |>
  mutate(quarter = as.integer(quarter_val)) |>
  select(-YEAR, -quarter_val)

cat("Computing CPS state-year aggregates...\n")
cps_year <- compute_cps_aggregates(cps, YEAR) |>
  mutate(
    period_start = as.Date(paste(YEAR, "01", "01", sep = "-")),
    year = YEAR, month = NA_integer_, quarter = NA_integer_
  ) |>
  select(-YEAR)

# ── 5. Save simplified CPS panel (data/CPS/) ─────────────────────────────

cat("Saving data/CPS/cps_ui_state_month_panel.csv...\n")
cps_month |>
  transmute(
    date = period_start,
    state, state_abbr,
    unemployed_total = weighted_unemployed_total,
    new_entrant_share, foreign_born_share, noncitizen_share, working_age_25_54_share,
    prof_managerial_share, white_collar_share, blue_collar_share, other_services_share,
    service_share, other_occ_share,
    perm_job_loser_share, temp_layoff_share, white_collar_perm_job_loser_share
  ) |>
  arrange(date, state) |>
  write_csv(cps_panel_path)

# ── 6. Load FRED panel and aggregate ─────────────────────────────────────

cat("Loading FRED panel...\n")
fred_raw <- read_csv(fred_path, show_col_types = FALSE) |>
  mutate(date = as.Date(date))

# Monthly FRED: just rename date -> period_start for merge
fred_month <- fred_raw |>
  transmute(
    period_start = date, state, state_abbr,
    initial_claims, continued_claims, insured_unemployment_rate,
    covered_employment, unemployed_persons, unemployment_rate, annual_population
  )

# Quarterly FRED: sum monthly flows, average monthly stocks/rates
fred_quarter <- fred_raw |>
  mutate(year = year(date), quarter = quarter(date)) |>
  group_by(year, quarter, state, state_abbr) |>
  summarise(
    initial_claims = sum(initial_claims, na.rm = TRUE),
    across(c(continued_claims, insured_unemployment_rate, covered_employment,
             unemployed_persons, unemployment_rate), \(x) mean(x, na.rm = TRUE)),
    annual_population = dplyr::first(annual_population),
    .groups = "drop"
  ) |>
  mutate(
    period_start = as.Date(paste(year, (quarter - 1L) * 3L + 1L, "01", sep = "-"))
  ) |>
  select(-year, -quarter)

# Yearly FRED: sum monthly flows, average monthly stocks/rates
fred_year <- fred_raw |>
  mutate(year = year(date)) |>
  group_by(year, state, state_abbr) |>
  summarise(
    initial_claims = sum(initial_claims, na.rm = TRUE),
    across(c(continued_claims, insured_unemployment_rate, covered_employment,
             unemployed_persons, unemployment_rate), \(x) mean(x, na.rm = TRUE)),
    annual_population = dplyr::first(annual_population),
    .groups = "drop"
  ) |>
  mutate(period_start = as.Date(paste(year, "01", "01", sep = "-"))) |>
  select(-year)

# ── 7. Merge CPS + FRED and derive ratios ────────────────────────────────

merge_and_derive <- function(cps_agg, fred_agg) {
  cps_agg |>
    left_join(fred_agg, by = c("period_start", "state", "state_abbr")) |>
    mutate(
      insured_unemployment_rate_share  = insured_unemployment_rate / 100,
      covered_employment               = dplyr::coalesce(
        covered_employment,
        continued_claims / insured_unemployment_rate_share
      ),
      initial_claims_per_covered_employment   = initial_claims / covered_employment,
      continued_claims_per_covered_employment = continued_claims / covered_employment,
      initial_claims_per_unemployed    = initial_claims / unemployed_persons,
      continued_claims_per_unemployed  = continued_claims / unemployed_persons
    )
}

# Define canonical column order matching existing stale files
panel_cols <- c(
  "period_start", "year", "quarter", "month", "state", "state_abbr",
  "unweighted_unemployed_n", "weighted_unemployed_total", "sum_w_sq", "effective_n",
  "new_entrant_unweighted_n", "foreign_born_unweighted_n", "noncitizen_unweighted_n",
  "working_age_25_54_unweighted_n",
  "prof_managerial_unweighted_n", "white_collar_unweighted_n", "service_unweighted_n",
  "blue_collar_unweighted_n", "other_occ_unweighted_n", "other_services_unweighted_n",
  "perm_job_loser_unweighted_n", "temp_layoff_unweighted_n",
  "white_collar_perm_job_loser_unweighted_n",
  "new_entrant_share", "foreign_born_share", "noncitizen_share", "working_age_25_54_share",
  "prof_managerial_share", "white_collar_share", "blue_collar_share", "other_services_share",
  "service_share", "other_occ_share",
  "perm_job_loser_share", "temp_layoff_share", "white_collar_perm_job_loser_share",
  "new_entrant_se", "foreign_born_se", "noncitizen_se",
  "new_entrant_moe95", "foreign_born_moe95", "noncitizen_moe95",
  "initial_claims", "continued_claims", "insured_unemployment_rate",
  "covered_employment", "annual_population",
  "unemployed_persons", "unemployment_rate",
  "initial_claims_per_covered_employment", "continued_claims_per_covered_employment",
  "initial_claims_per_unemployed", "continued_claims_per_unemployed",
  "insured_unemployment_rate_share"
)

cat("Merging CPS + FRED...\n")
panel_month_full   <- merge_and_derive(cps_month, fred_month)     |> select(all_of(panel_cols)) |> arrange(period_start, state)
panel_quarter_full <- merge_and_derive(cps_quarter, fred_quarter)  |> select(all_of(panel_cols)) |> arrange(period_start, state)
panel_year_full    <- merge_and_derive(cps_year, fred_year)        |> select(all_of(panel_cols)) |> arrange(period_start, state)

# ── 8. Analysis-ready filter (regression variables only, no blanket NA) ───

ar_filter <- function(panel, freq = c("month", "quarter", "year")) {
  freq <- match.arg(freq)
  # Numeric columns that must be non-NA and finite for regressions
  numeric_vars <- c(
    "new_entrant_share", "foreign_born_share", "noncitizen_share", "working_age_25_54_share",
    "prof_managerial_share", "white_collar_share", "blue_collar_share", "other_services_share",
    "service_share", "other_occ_share",
    "perm_job_loser_share", "temp_layoff_share", "white_collar_perm_job_loser_share",
    "annual_population",
    "initial_claims_per_covered_employment", "continued_claims_per_covered_employment",
    "insured_unemployment_rate_share"
  )

  panel |>
    filter(if_all(all_of(numeric_vars), \(x) !is.na(x) & is.finite(x)))
}

panel_month_ar   <- ar_filter(panel_month_full,   "month")
panel_quarter_ar <- ar_filter(panel_quarter_full,  "quarter")
panel_year_ar    <- ar_filter(panel_year_full,     "year")

# ── 9. Save full and analysis-ready panels ────────────────────────────────

cat("Saving data/final/ panels...\n")
write_csv(panel_month_full,   here("data", "final", "ui_takeup_panel_state_month.csv"))
write_csv(panel_month_ar,     here("data", "final", "ui_takeup_panel_state_month_analysis_ready.csv"))
write_csv(panel_quarter_full, here("data", "final", "ui_takeup_panel_state_quarter.csv"))
write_csv(panel_quarter_ar,   here("data", "final", "ui_takeup_panel_state_quarter_analysis_ready.csv"))
write_csv(panel_year_full,    here("data", "final", "ui_takeup_panel_state_year.csv"))
write_csv(panel_year_ar,      here("data", "final", "ui_takeup_panel_state_year_analysis_ready.csv"))

# ── 10. Precision diagnostics (CPS-only columns) ─────────────────────────

precision_cols <- c(
  "period_start", "year", "quarter", "month", "state", "state_abbr",
  "effective_n", "unweighted_unemployed_n",
  "new_entrant_share", "foreign_born_share", "noncitizen_share",
  "new_entrant_se", "foreign_born_se", "noncitizen_se",
  "new_entrant_moe95", "foreign_born_moe95", "noncitizen_moe95",
  "new_entrant_unweighted_n", "foreign_born_unweighted_n", "noncitizen_unweighted_n"
)

cat("Saving precision diagnostics...\n")
write_csv(panel_month_full   |> select(all_of(precision_cols)), here("data", "final", "cps_month_precision.csv"))
write_csv(panel_quarter_full |> select(all_of(precision_cols)), here("data", "final", "cps_quarter_precision.csv"))
write_csv(panel_year_full    |> select(all_of(precision_cols)), here("data", "final", "cps_year_precision.csv"))

# ── 11. Cross-frequency precision comparison (9 rows) ────────────────────

compute_precision_summary <- function(precision_df, freq_label) {
  measures <- c("new_entrant_share", "foreign_born_share", "noncitizen_share")
  se_cols  <- c("new_entrant_se", "foreign_born_se", "noncitizen_se")
  moe_cols <- c("new_entrant_moe95", "foreign_born_moe95", "noncitizen_moe95")
  num_cols <- c("new_entrant_unweighted_n", "foreign_born_unweighted_n", "noncitizen_unweighted_n")

  map_dfr(seq_along(measures), function(i) {
    tibble(
      frequency                 = freq_label,
      measure                   = measures[i],
      periods                   = nrow(precision_df),
      median_effective_n        = median(precision_df$effective_n, na.rm = TRUE),
      p10_effective_n           = quantile(precision_df$effective_n, 0.10, na.rm = TRUE),
      median_share              = median(precision_df[[measures[i]]], na.rm = TRUE),
      median_se                 = median(precision_df[[se_cols[i]]], na.rm = TRUE),
      p90_se                    = quantile(precision_df[[se_cols[i]]], 0.90, na.rm = TRUE),
      median_moe95              = median(precision_df[[moe_cols[i]]], na.rm = TRUE),
      p90_moe95                 = quantile(precision_df[[moe_cols[i]]], 0.90, na.rm = TRUE),
      median_unweighted_numerator = median(precision_df[[num_cols[i]]], na.rm = TRUE),
      p10_unweighted_numerator  = quantile(precision_df[[num_cols[i]]], 0.10, na.rm = TRUE)
    )
  })
}

prec_month   <- panel_month_full   |> select(all_of(precision_cols))
prec_quarter <- panel_quarter_full |> select(all_of(precision_cols))
prec_year    <- panel_year_full    |> select(all_of(precision_cols))

cps_precision_comparison <- bind_rows(
  compute_precision_summary(prec_month,   "month"),
  compute_precision_summary(prec_quarter, "quarter"),
  compute_precision_summary(prec_year,    "year")
)

cat("Saving cps_precision_comparison.csv (9 rows)...\n")
write_csv(cps_precision_comparison, here("data", "final", "cps_precision_comparison.csv"))

# ── 12. Monthly-only precision summary (optional, 3 rows) ────────────────

cps_precision_summary <- compute_precision_summary(prec_month, "month") |>
  rename(state_months = periods) |>
  select(-frequency)

write_csv(cps_precision_summary, here("data", "final", "cps_precision_summary.csv"))

# ── 13. Summary ──────────────────────────────────────────────────────────

cat("\n=== build_panels.R complete ===\n")
cat("CPS panel:  ", nrow(cps_month), "state-months\n")
cat("Monthly:    ", nrow(panel_month_full), "full /", nrow(panel_month_ar), "analysis-ready\n")
cat("Quarterly:  ", nrow(panel_quarter_full), "full /", nrow(panel_quarter_ar), "analysis-ready\n")
cat("Yearly:     ", nrow(panel_year_full), "full /", nrow(panel_year_ar), "analysis-ready\n")
cat("Precision:  ", nrow(cps_precision_comparison), "comparison rows\n")
