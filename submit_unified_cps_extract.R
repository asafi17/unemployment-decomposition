# ──────────────────────────────────────────────────────────────────────────────
# Unified IPUMS CPS extract for the unemployment decomposition project.
#
# This single extract covers all variables needed by:
#   - unemployment_decomposition.qmd  (demographics, occupation, WHYUNEMP)
#   - ui-takeup-study.qmd            (state panel, nativity/citizen, occupation)
#
# Output: data/CPS/cps_unified.csv.gz  +  data/CPS/cps_unified.xml
#
# Requires an IPUMS API key saved via:
#   ipumsr::set_ipums_api_key("YOUR_KEY", save = TRUE)
# ──────────────────────────────────────────────────────────────────────────────

library(ipumsr)
library(tidyverse)

target_ddi <- "data/CPS/cps_unified.xml"
target_csv <- "data/CPS/cps_unified.csv.gz"

# ── 0. If unified data already exists locally, skip everything ───────────────

if (file.exists(target_ddi) && file.exists(target_csv)) {
  cat("Unified CPS data already exists:\n")
  cat("  DDI:", target_ddi, "\n")
  cat("  CSV:", target_csv, "\n")
  cat("Delete these files to force a fresh download.\n")

  ddi <- read_ipums_ddi(target_ddi)
  df  <- read_ipums_micro(ddi, data_file = target_csv, n_max = 3)
  cat("\nVariables in extract:\n")
  cat(paste(names(df), collapse = ", "), "\n")
  stop("Data already present. Exiting.", call. = FALSE)
}

# ── 1. Define samples: monthly CPS from 1994+, excluding ASEC ───────────────

all_samples <- get_sample_info("cps")

ipums_samples <- all_samples |>
  filter(
    grepl("^cps\\d{4}_\\d{2}[bs]$", name),
    as.numeric(gsub("cps(\\d{4}).*", "\\1", name)) >= 1994,
    !str_detect(description, "ASEC")
  ) |>
  pull(name)

cat("Selected", length(ipums_samples), "monthly CPS samples\n")

# ── 2. Define variables ─────────────────────────────────────────────────────
# Full union of variables from both QMD files:
#   Core identifiers:  YEAR, SERIAL, MONTH, HWTFINL, CPSID, PERNUM, WTFINL,
#                      CPSIDP, CPSIDV, STATEFIP
#   Demographics:      AGE, SEX, RACE, HISPAN, EDUC
#   Labor force:       EMPSTAT, WHYUNEMP (cases 1-6 only), OCC1990, CLASSWKR
#   Immigration:       NATIVITY, CITIZEN

extract_description <- "Unified CPS extract: WHYUNEMP demographics + state + nativity + occupation, 1994-present"

extract_def <- define_extract_micro(
  collection  = "cps",
  description = extract_description,
  samples     = ipums_samples,
  variables   = list(
    "YEAR", "SERIAL", "MONTH", "HWTFINL", "CPSID", "PERNUM",
    "WTFINL", "CPSIDP", "CPSIDV", "STATEFIP",
    "AGE", "SEX", "RACE", "HISPAN", "EDUC",
    "EMPSTAT",
    var_spec("WHYUNEMP", case_selections = as.character(1:6)),
    "OCC1990", "CLASSWKR",
    "NATIVITY", "CITIZEN"
  ),
  data_format = "csv"
)

# ── 3. Check for a matching completed extract before submitting ──────────────

extract_history <- tryCatch(
  get_extract_history("cps", how_many = 10),
  error = function(e) NULL
)

downloadable <- NULL
if (!is.null(extract_history)) {
  matching_completed <- keep(
    extract_history,
    \(x) identical(x$description, extract_description) && identical(x$status, "completed")
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

# ── 4. Clean up any partial files, then download ────────────────────────────

dir.create("data/CPS", recursive = TRUE, showWarnings = FALSE)

partial_xml <- list.files("data/CPS", pattern = "^cps_\\d+\\.xml$", full.names = TRUE)
partial_csv <- list.files("data/CPS", pattern = "^cps_\\d+\\.csv\\.gz$", full.names = TRUE)
if (length(c(partial_xml, partial_csv)) > 0) file.remove(c(partial_xml, partial_csv))

for (attempt in 1:5) {
  path_to_ddi <- tryCatch(
    download_extract(downloadable, download_dir = "data/CPS"),
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

# ── 5. Rename to cps_unified.* in data/CPS/ (Windows-safe: copy then remove)

ddi_file <- path_to_ddi
csv_file <- sub("\\.xml$", ".csv.gz", ddi_file)

if (file.exists(target_ddi)) file.remove(target_ddi)
if (file.exists(target_csv)) file.remove(target_csv)

ok_ddi <- file.copy(ddi_file, target_ddi)
ok_csv <- file.copy(csv_file, target_csv)

if (!ok_ddi || !ok_csv) {
  stop("Failed to copy files to unified targets. Check permissions.")
}

file.remove(ddi_file)
file.remove(csv_file)

cat("Renamed to:\n")
cat("  DDI:", target_ddi, "\n")
cat("  CSV:", target_csv, "\n")

# ── 6. Verify ────────────────────────────────────────────────────────────────

ddi <- read_ipums_ddi(target_ddi)
df  <- read_ipums_micro(ddi, data_file = target_csv, n_max = 5)
cat("\nVariables in extract:\n")
cat(paste(names(df), collapse = ", "), "\n")
cat("\nFirst rows:\n")
print(df)
cat("\nDone. Both QMD files should read from data/CPS/cps_unified.xml\n")
