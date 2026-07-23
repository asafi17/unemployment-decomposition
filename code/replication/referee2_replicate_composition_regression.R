#!/usr/bin/env Rscript
# Referee 2 cross-language replication — R (standalone, not the QMD).
#
# Independently re-pulls FRED series via fredr, builds the monthly
# cause-share dataset, drops the pandemic window (Mar 2020 – Dec 2021),
# and runs the same OLS specifications as drafts/cfi.qmd to verify
# that the reported numbers are not artifacts of the QMD environment.
#
# Also dumps the long-format panel to CSV so the Stata replication can
# consume an identical input and we isolate OLS-engine discrepancies
# from data-pull discrepancies.
#
# Run:
#   "/c/Program Files/R/R-4.5.3/bin/Rscript.exe" code/replication/referee2_replicate_composition_regression.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(fredr)
  library(jsonlite)
})

fredr_set_key("ccd350785a210a7653a20d02ce92d83a")

here_root <- tryCatch(rprojroot::find_root(rprojroot::is_git_root),
                      error = function(e) getwd())
out_dir <- file.path(here_root, "code", "replication")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cause_series <- c(
  "Temporary Layoff"     = "LNS13023653",
  "Permanent Job Losers" = "LNS13026638",
  "Completed Temp Job"   = "LNS13026637",
  "Job Leavers"          = "LNS13023705",
  "Reentrants"           = "LNS13023557",
  "New Entrants"         = "LNS13023569"
)

cause_monthly <- map_dfr(names(cause_series), \(nm) {
  fredr(series_id = cause_series[[nm]], observation_start = as.Date("1994-01-01")) |>
    transmute(date, cause = nm, value)
}) |>
  pivot_wider(names_from = cause, values_from = value)

total_unemp <- fredr(series_id = "UNEMPLOY", observation_start = as.Date("1994-01-01")) |>
  transmute(date, total = value)

claims_weekly <- bind_rows(
  fredr(series_id = "CCSA",   observation_start = as.Date("1994-01-01")) |>
    transmute(date, series = "CCSA",   value),
  fredr(series_id = "ICSA",   observation_start = as.Date("1994-01-01")) |>
    transmute(date, series = "ICSA",   value),
  fredr(series_id = "COVEMP", observation_start = as.Date("1994-01-01")) |>
    transmute(date, series = "COVEMP", value)
) |>
  mutate(month = floor_date(date, "month")) |>
  group_by(month, series) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = series, values_from = value) |>
  transmute(
    date  = month,
    cc_ce = CCSA / COVEMP,
    ic_ce = ICSA / COVEMP
  )

reg_df <- cause_monthly |>
  inner_join(total_unemp, by = "date") |>
  mutate(
    temp_layoff_share    = `Temporary Layoff`     / total,
    perm_job_loser_share = `Permanent Job Losers` / total,
    completed_temp_share = `Completed Temp Job`   / total,
    job_leaver_share     = `Job Leavers`          / total,
    reentrant_share      = `Reentrants`           / total,
    new_entrant_share    = `New Entrants`         / total,
    month_of_year        = factor(month.abb[month(date)], levels = month.abb),
    pandemic             = date >= as.Date("2020-03-01") & date <= as.Date("2021-12-01")
  ) |>
  inner_join(claims_weekly, by = "date") |>
  filter(!pandemic) |>
  drop_na(cc_ce, ic_ce, temp_layoff_share, perm_job_loser_share,
          job_leaver_share, reentrant_share, new_entrant_share)

# Dump the merged panel for Stata replication — this is the EXACT same
# dataset the QMD builds, written so Stata can run OLS on identical inputs.
reg_df |>
  select(date, cc_ce, ic_ce,
         temp_layoff_share, perm_job_loser_share, completed_temp_share,
         job_leaver_share, reentrant_share, new_entrant_share,
         month_of_year) |>
  mutate(month_of_year = as.character(month_of_year)) |>
  write_csv(file.path(out_dir, "referee2_regression_panel.csv"))

comp_rhs <- "temp_layoff_share + perm_job_loser_share + job_leaver_share + reentrant_share + new_entrant_share + month_of_year"

cc_mod <- lm(as.formula(paste("cc_ce ~", comp_rhs)), data = reg_df)
ic_mod <- lm(as.formula(paste("ic_ce ~", comp_rhs)), data = reg_df)

extract_rhs <- function(model) {
  k <- c("temp_layoff_share", "perm_job_loser_share", "job_leaver_share",
         "reentrant_share", "new_entrant_share")
  co <- summary(model)$coefficients
  out <- setNames(vector("list", length(k)), k)
  for (nm in k) {
    est <- co[nm, "Estimate"]
    se  <- co[nm, "Std. Error"]
    p   <- co[nm, "Pr(>|t|)"]
    out[[nm]] <- list(
      estimate_raw = unname(est),
      se_raw       = unname(se),
      coef_10pp    = unname(est * 0.10),
      se_10pp      = unname(se * 0.10),
      p_value      = unname(p)
    )
  }
  out$`_r_squared` <- summary(model)$r.squared
  out$`_n`         <- nobs(model)
  out
}

results <- list(
  language = "R",
  package  = "lm() (base R)",
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  sample_first_date = as.character(min(reg_df$date)),
  sample_last_date  = as.character(max(reg_df$date)),
  n_obs = nrow(reg_df),
  cc_ce = extract_rhs(cc_mod),
  ic_ce = extract_rhs(ic_mod)
)

# Descriptive stats on New Entrants share (full merged panel, before the
# regression filter) so we can compare to Python's numbers.
ne_full <- cause_monthly |>
  inner_join(total_unemp, by = "date") |>
  transmute(date, new_entrant_share = `New Entrants` / total * 100)

ne_non_pandemic <- ne_full |> filter(date < as.Date("2020-03-01") | date > as.Date("2021-12-01"))
ne_pre <- ne_full |> filter(date <= as.Date("2020-02-01"))
ne_last12 <- ne_full |> filter(date > max(date) - months(12))

results$new_entrant_share_pct <- list(
  latest_date = as.character(max(ne_full$date)),
  long_run_mean_1994_present_nonpandemic = mean(ne_non_pandemic$new_entrant_share, na.rm = TRUE),
  long_run_mean_1994_present_all         = mean(ne_full$new_entrant_share, na.rm = TRUE),
  pre_pandemic_mean_through_2020_02      = mean(ne_pre$new_entrant_share, na.rm = TRUE),
  last_12_months_mean                    = mean(ne_last12$new_entrant_share, na.rm = TRUE),
  latest_value                           = tail(ne_full$new_entrant_share, 1)
)

writeLines(toJSON(results, pretty = TRUE, auto_unbox = TRUE),
           file.path(out_dir, "referee2_results_R.json"))
print(toJSON(results, pretty = TRUE, auto_unbox = TRUE))
