suppressPackageStartupMessages({
  library(tidyverse)
  library(fredr)
  library(lubridate)
  library(sandwich)
  library(lmtest)
  library(car)
})

fredr_set_key("ccd350785a210a7653a20d02ce92d83a")
fredr_retry <- function(..., tries = 5, wait = 3) {
  for (i in seq_len(tries)) {
    res <- tryCatch(fredr(...), error = function(e) e)
    if (!inherits(res, "error")) return(res)
    Sys.sleep(wait)
  }
  stop(res)
}

start_date <- as.Date("1994-01-01")

ur_m <- fredr_retry(series_id = "UNRATE", observation_start = start_date) |>
  transmute(date, UR = value)

claims_monthly <- function(sid) {
  fredr_retry(series_id = sid, observation_start = start_date) |>
    mutate(month = floor_date(date, "month")) |>
    group_by(month) |>
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    rename(date = month)
}

ic_m <- claims_monthly("ICSA")   |> rename(IC = value)
cc_m <- claims_monthly("CCSA")   |> rename(CC = value)
ce_m <- claims_monthly("COVEMP") |> rename(CE = value)

total_unemp <- fredr_retry(series_id = "UNEMPLOY", observation_start = start_date) |>
  transmute(date, tot = value)

ne <- fredr_retry(series_id = "LNS13023569", observation_start = start_date) |>
  transmute(date, NE = value)

df <- ur_m |>
  inner_join(ic_m, by="date") |>
  inner_join(cc_m, by="date") |>
  inner_join(ce_m, by="date") |>
  inner_join(total_unemp, by="date") |>
  inner_join(ne, by="date") |>
  arrange(date) |>
  mutate(
    ne_share = NE / tot,
    dUR = UR - lag(UR),
    dlIC_CE = log(IC/CE) - log(lag(IC/CE)),
    dlCC_CE = log(CC/CE) - log(lag(CC/CE)),
    pandemic = date >= as.Date("2020-03-01") & date <= as.Date("2021-12-01")
  )

ne_cutoff <- df |> filter(!pandemic) |> pull(ne_share) |> quantile(0.75, na.rm=TRUE)
cat(sprintf("NE cutoff (75th pct, non-pandemic): %.5f\n\n", ne_cutoff))

est_df <- df |>
  filter(!pandemic) |>
  mutate(high_ne = as.numeric(ne_share >= ne_cutoff)) |>
  drop_na(dUR, dlIC_CE, dlCC_CE, high_ne)

cat(sprintf("N = %d months\n", nrow(est_df)))
cat(sprintf("High NE share months: %d\n", sum(est_df$high_ne)))

mod <- lm(dUR ~ high_ne + dlIC_CE + dlIC_CE:high_ne +
          dlCC_CE + dlCC_CE:high_ne, data = est_df)
nw  <- sandwich::NeweyWest(mod, prewhite = FALSE)
ct  <- lmtest::coeftest(mod, vcov = nw)
print(ct)

cat("\n=== Wald joint test gamma_IC = gamma_CC = 0 ===\n")
wt <- car::linearHypothesis(mod, c("high_ne:dlIC_CE = 0", "high_ne:dlCC_CE = 0"),
                            vcov = nw)
print(wt)

cat("\n=== Implied slopes ===\n")
b_ic <- coef(mod)["dlIC_CE"]
g_ic <- coef(mod)["high_ne:dlIC_CE"]
b_cc <- coef(mod)["dlCC_CE"]
g_cc <- coef(mod)["high_ne:dlCC_CE"]
cat(sprintf("IC slope, Rest:    %.4f\n", b_ic))
cat(sprintf("IC slope, HighNE:  %.4f  (ratio %.2f)\n", b_ic + g_ic, (b_ic + g_ic)/b_ic))
cat(sprintf("CC slope, Rest:    %.4f\n", b_cc))
cat(sprintf("CC slope, HighNE:  %.4f  (ratio %.2f)\n", b_cc + g_cc, (b_cc + g_cc)/b_cc))

cat("\n=== R^2 ===\n")
print(summary(mod)$r.squared)
