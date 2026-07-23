#!/usr/bin/env Rscript
# Compares R and Python replication outputs coefficient-by-coefficient
# to 10 decimal places. Produces a single table highlighting any
# cross-language discrepancy.

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
})

here_root <- tryCatch(rprojroot::find_root(rprojroot::is_git_root),
                      error = function(e) getwd())
rep_dir <- file.path(here_root, "code", "replication")

r_json  <- fromJSON(file.path(rep_dir, "referee2_results_R.json"),
                    simplifyVector = FALSE)
py_json <- fromJSON(file.path(rep_dir, "referee2_results_python.json"),
                    simplifyVector = FALSE)

# Re-run R regression in memory so we can compare at full float precision
# (the saved JSON uses default jsonlite digits; the point estimates are
# stored in the model object, so we re-fit from the same CSV.)
panel <- read_csv(file.path(rep_dir, "referee2_regression_panel.csv"),
                  show_col_types = FALSE) |>
  mutate(month_of_year = factor(month_of_year,
                                levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                           "Jul","Aug","Sep","Oct","Nov","Dec")))
cc_mod_r <- lm(cc_ce ~ temp_layoff_share + perm_job_loser_share +
                       job_leaver_share + reentrant_share + new_entrant_share +
                       month_of_year, data = panel)
ic_mod_r <- lm(ic_ce ~ temp_layoff_share + perm_job_loser_share +
                       job_leaver_share + reentrant_share + new_entrant_share +
                       month_of_year, data = panel)

rhs <- c("temp_layoff_share","perm_job_loser_share","job_leaver_share",
         "reentrant_share","new_entrant_share")

rows <- list()
for (out in c("cc_ce","ic_ce")) {
  mod_r <- if (out == "cc_ce") cc_mod_r else ic_mod_r
  co_r  <- summary(mod_r)$coefficients
  for (v in rhs) {
    est_r <- unname(co_r[v, "Estimate"])
    se_r  <- unname(co_r[v, "Std. Error"])
    est_py <- py_json[[out]][[v]][["estimate_raw"]]
    se_py  <- py_json[[out]][[v]][["se_raw"]]
    rows[[length(rows)+1]] <- tibble(
      outcome = out, term = v,
      est_R      = est_r,      est_Py     = est_py,
      se_R       = se_r,       se_Py      = se_py,
      est_abs_diff = abs(est_r - est_py),
      se_abs_diff  = abs(se_r  - se_py)
    )
  }
}
cmp <- bind_rows(rows)

options(digits = 12)
print(cmp, n = Inf)

max_est_diff <- max(cmp$est_abs_diff)
max_se_diff  <- max(cmp$se_abs_diff)
cat(sprintf("\nMAX |est diff| = %.2e\n", max_est_diff))
cat(sprintf("MAX |se  diff| = %.2e\n", max_se_diff))
if (max_est_diff < 1e-9 && max_se_diff < 1e-9) {
  cat(">>> R and Python match to 9+ decimal places on all coefficients and SEs.\n")
} else if (max_est_diff < 1e-6) {
  cat(">>> R and Python match to 6+ decimal places on estimates.\n")
} else {
  cat(">>> DISCREPANCY: coefficients differ beyond 6 dp.\n")
}

# R vs Python descriptive stats (new entrant share)
ne_cmp <- tibble(
  stat = names(py_json$new_entrant_share_pct),
  python = map_chr(py_json$new_entrant_share_pct, as.character),
  R      = map_chr(r_json$new_entrant_share_pct,  as.character)
)
cat("\n=== New Entrant share descriptive stats ===\n")
print(ne_cmp)
