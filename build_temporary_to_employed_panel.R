library(data.table)

analysis_dir <- file.path("data", "CPS", "temporary_to_employed")
extract_csv_path <- file.path(analysis_dir, "cps_temp_to_employed.csv.gz")
monthly_transition_path <- file.path(analysis_dir, "monthly_transition_rates.csv")
origin_record_path <- file.path(analysis_dir, "origin_records_with_followup.csv.gz")
linkage_summary_path <- file.path(analysis_dir, "linkage_summary.csv")

if (!file.exists(extract_csv_path)) {
  stop("Temporary-to-employed extract not found in data/CPS/temporary_to_employed.")
}

month_index <- function(year, month) {
  as.integer(year) * 12L + as.integer(month)
}

weighted_share_observed <- function(x, w) {
  denom <- sum(w, na.rm = TRUE)
  if (!is.finite(denom) || denom <= 0) {
    return(NA_real_)
  }
  sum(w[!is.na(x)], na.rm = TRUE) / denom
}

weighted_mean_logical <- function(x, w) {
  keep <- !is.na(x) & !is.na(w)
  if (!any(keep)) {
    return(NA_real_)
  }
  sum(as.numeric(x[keep]) * w[keep]) / sum(w[keep])
}

cps <- fread(
  extract_csv_path,
  select = c("YEAR", "MONTH", "WTFINL", "CPSIDV", "AGE", "EMPSTAT", "WHYUNEMP"),
  showProgress = FALSE
)

origin_all <- cps[
  AGE >= 16 & WHYUNEMP %in% c(1L, 2L),
  .(
    unemployed_reason = fifelse(
      WHYUNEMP == 1L,
      "Temporary Layoff",
      "Permanent Job Loser"
    ),
    weight = as.numeric(WTFINL),
    linkable = !is.na(CPSIDV) & CPSIDV != 0
  )
]

linkage_summary <- origin_all[
  ,
  .(
    Observations = .N,
    `Weighted Linkable Share` = weighted_mean_logical(linkable, weight)
  ),
  by = .(reason = unemployed_reason)
]

cps <- cps[AGE >= 16 & !is.na(CPSIDV) & CPSIDV != 0]

origin_linkable_ids <- unique(cps[WHYUNEMP %in% c(1L, 2L), CPSIDV])

linked_panel <- cps[CPSIDV %in% origin_linkable_ids]
rm(cps)
gc()

linked_panel[
  ,
  `:=`(
    year = YEAR,
    month = MONTH,
    date = as.IDate(sprintf("%04d-%02d-01", YEAR, MONTH)),
    ym = month_index(YEAR, MONTH),
    weight = as.numeric(WTFINL),
    empstat = as.integer(EMPSTAT),
    whyunemp = as.integer(WHYUNEMP),
    link_id = CPSIDV,
    unemployed_reason = fifelse(
      WHYUNEMP == 1L,
      "Temporary Layoff",
      fifelse(WHYUNEMP == 2L, "Permanent Job Loser", NA_character_)
    ),
    is_employed = EMPSTAT %in% c(10L, 12L)
  )
]

linked_panel <- linked_panel[
  ,
  .(date, year, month, ym, weight, empstat, whyunemp, link_id, unemployed_reason, is_employed)
]

setorderv(linked_panel, c("link_id", "ym", "date"))
linked_panel <- unique(linked_panel, by = c("link_id", "ym"))

lead_ym_names <- paste0("lead_ym_", 1:7)
lead_emp_names <- paste0("lead_emp_", 1:7)

linked_panel[
  ,
  (lead_ym_names) := shift(ym, 1:7, type = "lead"),
  by = link_id
]
linked_panel[
  ,
  (lead_emp_names) := shift(is_employed, 1:7, type = "lead"),
  by = link_id
]

origin_records <- linked_panel[whyunemp %in% c(1L, 2L)]

next3_mat <- as.matrix(origin_records[, ..lead_emp_names[1:3]])
next3_obs_count <- rowSums(!is.na(next3_mat))
next3_any_employed <- rowSums(replace(next3_mat, is.na(next3_mat), FALSE)) > 0

gap_mat <- cbind(
  origin_records[[lead_ym_names[1]]] - origin_records[["ym"]],
  origin_records[[lead_ym_names[2]]] - origin_records[["ym"]],
  origin_records[[lead_ym_names[3]]] - origin_records[["ym"]],
  origin_records[[lead_ym_names[4]]] - origin_records[["ym"]],
  origin_records[[lead_ym_names[5]]] - origin_records[["ym"]],
  origin_records[[lead_ym_names[6]]] - origin_records[["ym"]],
  origin_records[[lead_ym_names[7]]] - origin_records[["ym"]]
)

emp_mat <- as.matrix(origin_records[, ..lead_emp_names])
gap_mask <- !is.na(gap_mat) & gap_mat >= 9L
has_gap_followup <- rowSums(gap_mask) > 0
first_gap_idx <- max.col(gap_mask, ties.method = "first")
employed_after_gap <- rep(NA, nrow(origin_records))

if (any(has_gap_followup)) {
  gap_lookup <- cbind(which(has_gap_followup), first_gap_idx[has_gap_followup])
  employed_after_gap[has_gap_followup] <- emp_mat[gap_lookup]
}

origin_records[
  ,
  `:=`(
    employed_next_observed = get(lead_emp_names[1]),
    employed_within_next3 = fifelse(next3_obs_count == 0, NA, next3_any_employed),
    employed_after_gap = employed_after_gap
  )
]

origin_records_out <- origin_records[
  ,
  .(
    date,
    year,
    month,
    weight,
    unemployed_reason,
    employed_next_observed,
    employed_within_next3,
    employed_after_gap
  )
]

monthly_transition_rates <- origin_records_out[
  ,
  .(
    origin_weight = sum(weight, na.rm = TRUE),
    origin_n = .N,
    next_obs_followup_share = weighted_share_observed(employed_next_observed, weight),
    next_obs_employed_share = weighted_mean_logical(employed_next_observed, weight),
    next3_followup_share = weighted_share_observed(employed_within_next3, weight),
    next3_employed_share = weighted_mean_logical(employed_within_next3, weight),
    gap_followup_share = weighted_share_observed(employed_after_gap, weight),
    gap_employed_share = weighted_mean_logical(employed_after_gap, weight)
  ),
  by = .(date, unemployed_reason)
]

setorder(monthly_transition_rates, date, unemployed_reason)

fwrite(monthly_transition_rates, monthly_transition_path)
fwrite(origin_records_out, origin_record_path)
fwrite(linkage_summary, linkage_summary_path)

cat("Saved:\n")
cat("  ", monthly_transition_path, "\n")
cat("  ", origin_record_path, "\n")
cat("  ", linkage_summary_path, "\n")
