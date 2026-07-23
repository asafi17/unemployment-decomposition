* =============================================================================
* Referee 2 cross-language replication — Stata.
*
* Reads the merged monthly panel produced by the R replication script
* (code/replication/referee2_regression_panel.csv), encodes month-of-year as
* a factor, and runs OLS with the same specification as drafts/cfi.qmd:
*
*     regress cc_ce temp_layoff_share perm_job_loser_share ///
*             job_leaver_share reentrant_share new_entrant_share i.moy
*
* Completed Temp Job is the omitted reference (never enters RHS), matching
* the QMD. Compares results to the R/Python outputs. Stata's -regress-,
* R's -lm()-, and Python's -statsmodels.OLS- use different back-end
* linear-algebra paths, so matching to 6+ decimal places is meaningful.
*
* Run:
*   "/c/Program Files/Stata16/StataIC-64.exe" -e do ^
*       code/replication/referee2_replicate_composition_regression.do
* =============================================================================

clear all
set more off
capture log close

local repdir = "C:/Users/aryan/OneDrive/Research/unemployment_decomposition/code/replication"
local panel  = "`repdir'/referee2_regression_panel.csv"
local outlog = "`repdir'/referee2_results_stata.log"
log using "`outlog'", replace text

import delimited using "`panel'", clear case(preserve) varnames(1)

* Month-of-year as factor
gen moy = .
local months Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
tokenize `months'
forvalues i = 1/12 {
    replace moy = `i' if month_of_year == "``i''"
}
label define moy_lbl 1 "Jan" 2 "Feb" 3 "Mar" 4 "Apr" 5 "May" 6 "Jun" ///
                    7 "Jul" 8 "Aug" 9 "Sep" 10 "Oct" 11 "Nov" 12 "Dec"
label values moy moy_lbl

* --- Continuing claims intensity regression
regress cc_ce temp_layoff_share perm_job_loser_share ///
              job_leaver_share reentrant_share new_entrant_share i.moy
estimates store cc_mod

di "=== Continuing claims intensity (CC/CE) ==="
di "R-squared: " e(r2)
di "N: " e(N)
matrix b = e(b)
matrix V = e(V)
foreach v in temp_layoff_share perm_job_loser_share job_leaver_share reentrant_share new_entrant_share {
    local est = _b[`v']
    local se  = _se[`v']
    local p   = 2*(1 - normal(abs(`est' / `se')))
    di "`v': est=" %10.7f `est' " se=" %10.7f `se' " coef_10pp=" %10.7f (`est' * 0.10) " p=" %10.7g `p'
}

* --- Initial claims intensity regression
regress ic_ce temp_layoff_share perm_job_loser_share ///
              job_leaver_share reentrant_share new_entrant_share i.moy
estimates store ic_mod

di "=== Initial claims intensity (IC/CE) ==="
di "R-squared: " e(r2)
di "N: " e(N)
foreach v in temp_layoff_share perm_job_loser_share job_leaver_share reentrant_share new_entrant_share {
    local est = _b[`v']
    local se  = _se[`v']
    local p   = 2*(1 - normal(abs(`est' / `se')))
    di "`v': est=" %10.7f `est' " se=" %10.7f `se' " coef_10pp=" %10.7f (`est' * 0.10) " p=" %10.7g `p'
}

log close
