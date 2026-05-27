# ──────────────────────────────────────────────────────────────────────────────
# IPUMS CPS extract for the Recent College Graduate (RCG) FAQ.
#
# Variables needed by the RCG FAQ: monthly timing, person weight, age,
# education, labor-force status, unemployment duration, and reason for
# unemployment.
#
# Output:
#   exploring_new_entrants/data/CPS/cps_rcg.xml      (DDI)
#   exploring_new_entrants/data/CPS/cps_rcg.csv.gz   (microdata)
#
# Requires an IPUMS API key saved via:
#   ipumsr::set_ipums_api_key("YOUR_KEY", save = TRUE)
# ──────────────────────────────────────────────────────────────────────────────

library(ipumsr)
library(here)
library(tidyverse)

here::i_am("exploring_new_entrants/submit_cps_extract.R")

cps_dir    <- here("exploring_new_entrants", "data", "CPS")
target_ddi <- file.path(cps_dir, "cps_rcg.xml")
target_csv <- file.path(cps_dir, "cps_rcg.csv.gz")

dir.create(cps_dir, recursive = TRUE, showWarnings = FALSE)

# ── 0. Skip if already present ───────────────────────────────────────────────

if (file.exists(target_ddi) && file.exists(target_csv)) {
  cat("RCG CPS data already present:\n")
  cat("  DDI:", target_ddi, "\n")
  cat("  CSV:", target_csv, "\n")
  cat("Delete these files to force a fresh download.\n")

  ddi <- read_ipums_ddi(target_ddi)
  df  <- read_ipums_micro(ddi, data_file = target_csv, n_max = 3, verbose = FALSE)
  cat("\nVariables in extract:\n")
  cat(paste(names(df), collapse = ", "), "\n")
  stop("Data already present. Exiting.", call. = FALSE)
}

# ── 1. Samples: monthly CPS 1994+, excluding ASEC ───────────────────────────

all_samples <- get_sample_info("cps")

ipums_samples <- all_samples |>
  filter(
    grepl("^cps\\d{4}_\\d{2}[bs]$", name),
    as.numeric(gsub("cps(\\d{4}).*", "\\1", name)) >= 1994,
    !str_detect(description, "ASEC")
  ) |>
  pull(name)

cat("Selected", length(ipums_samples), "monthly CPS samples\n")

# ── 2. Variables ─────────────────────────────────────────────────────────────
# Minimal variable set used by markdown/rcg_faq.qmd.

extract_description <- "RCG FAQ extract v3: monthly CPS minimal vars (YEAR/MONTH/WTFINL + AGE/EDUC/EMPSTAT/LABFORCE/DURUNEMP/WHYUNEMP), 1994-present"

extract_def <- define_extract_micro(
  collection  = "cps",
  description = extract_description,
  samples     = ipums_samples,
  variables   = list(
    # Identifiers + weight actually used in the analysis
    "YEAR", "MONTH", "WTFINL",
    # Analysis vars
    "AGE", "EDUC",
    "EMPSTAT", "LABFORCE",
    "DURUNEMP",
    "WHYUNEMP"
  ),
  data_format = "csv"
)

# ── 3. Reuse a matching completed extract if one exists ─────────────────────

extract_history <- tryCatch(
  get_extract_history("cps", how_many = 10),
  error = function(e) NULL
)

downloadable <- NULL
if (!is.null(extract_history)) {
  matching_completed <- keep(
    extract_history,
    \(x) identical(x$description, extract_description) &&
         identical(x$status, "completed")
  )
  if (length(matching_completed) > 0) {
    downloadable <- matching_completed[[1]]
    cat("Found matching completed extract:", downloadable$number, "\n")
  }
}

if (is.null(downloadable)) {
  cat("Submitting new extract...\n")
  submitted    <- submit_extract(extract_def)
  cat("Extract submitted:", submitted$number, "\n")
  downloadable <- wait_for_extract(submitted)
  cat("Extract ready.\n")
}

# ── 4. Clean partial downloads, then fetch ──────────────────────────────────

partial_xml <- list.files(cps_dir, pattern = "^cps_\\d+\\.xml$",     full.names = TRUE)
partial_csv <- list.files(cps_dir, pattern = "^cps_\\d+\\.csv\\.gz$", full.names = TRUE)
if (length(c(partial_xml, partial_csv)) > 0) file.remove(c(partial_xml, partial_csv))

path_to_ddi <- NULL
for (attempt in 1:5) {
  path_to_ddi <- tryCatch(
    download_extract(downloadable, download_dir = cps_dir, overwrite = TRUE),
    error = function(e) {
      if (attempt < 5) {
        message("Download attempt ", attempt, " failed: ", conditionMessage(e))
        message("Retrying in ", attempt * 10, " seconds...")
        Sys.sleep(attempt * 10)
      }
      NULL
    }
  )
  if (!is.null(path_to_ddi)) break
}

if (is.null(path_to_ddi)) stop("All download attempts failed.")

cat("Downloaded DDI:", path_to_ddi, "\n")

# ── 5. Rename to cps_rcg.{xml,csv.gz} (Windows-safe: copy then remove) ──────

ddi_file <- path_to_ddi
csv_file <- sub("\\.xml$", ".csv.gz", ddi_file)

if (file.exists(target_ddi)) file.remove(target_ddi)
if (file.exists(target_csv)) file.remove(target_csv)

ok_ddi <- file.copy(ddi_file, target_ddi)
ok_csv <- file.copy(csv_file, target_csv)

if (!ok_ddi || !ok_csv) {
  stop("Failed to copy files to cps_rcg targets. Check permissions.")
}

file.remove(ddi_file)
file.remove(csv_file)

cat("Renamed to:\n")
cat("  DDI:", target_ddi, "\n")
cat("  CSV:", target_csv, "\n")

# ── 6. Verify ───────────────────────────────────────────────────────────────

ddi <- read_ipums_ddi(target_ddi)
df  <- read_ipums_micro(ddi, data_file = target_csv, n_max = 5, verbose = FALSE)
cat("\nVariables in extract:\n")
cat(paste(names(df), collapse = ", "), "\n")
cat("\nDone.\n")
