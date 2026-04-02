# Fix: Foreign-Born Share of Unemployed vs. Labor Force Showing Identical Series

## Problem

In `unemployment_decomposition.qmd`, the "Foreign-Born Share Within Group" chart (Section 9, tab 2) plots four series including "All Unemployed" (% of unemployed who are foreign born) and "Labor Force" (% of the labor force who are foreign born). These two series were rendering as identical lines despite using different numerators and denominators in the code.

## Diagnosis

### Bug 1: IPUMS extract case selection filtering out all employed persons

**File:** `submit_unified_cps_extract.R`, line 72

The extract definition used:
```r
var_spec("WHYUNEMP", case_selections = as.character(1:6))
```

In IPUMS, `case_selections` on a variable filters the **entire extract** to only records matching those values. WHYUNEMP (reason for unemployment) is only coded for unemployed persons (EMPSTAT 20-22). Everyone else (employed, NILF) has WHYUNEMP = 0 or NA.

This meant the downloaded `cps_unified.csv.gz` contained **only unemployed persons** — no employed people at all. Verified empirically: out of 500,000 sampled rows, every single one had EMPSTAT in {20, 21, 22}.

**Downstream impact:** The QMD code constructs `cps_lf_4way` by filtering `cps_raw` to `EMPSTAT %in% c(10, 12, 20, 21, 22)` (employed + unemployed = labor force). But since no employed persons existed in the data, `cps_lf_4way` contained only unemployed people — making it functionally identical to `cps_4way`. The "Labor Force" denominator in `fb_within` was therefore the same population as the "All Unemployed" denominator, producing identical series.

### Bug 2: Incomplete NATIVITY coding for native-born persons

**Files:** `unemployment_decomposition.qmd`, lines 187, 200, 1572

IPUMS CPS NATIVITY codes:
| Code | Meaning |
|------|---------|
| 0 | Not in universe |
| 1 | Native born, both parents native born |
| 2 | Native born, father foreign born |
| 3 | Native born, mother foreign born |
| 4 | Native born, both parents foreign born |
| 5 | Foreign born |

The code used:
```r
foreign_born = case_when(
  NATIVITY == 5 ~ "Foreign Born",
  NATIVITY == 1 ~ "Native Born"
)
```

This only classified code 1 as "Native Born", silently dropping codes 2-4 (native-born persons with at least one foreign-born parent) as NA. From the data sample: codes 2, 3, 4 accounted for ~36,305 out of 500,000 rows (~7.3%), so a meaningful share of native-born persons were excluded from all nativity-based calculations.

## Changes Made

### 1. `submit_unified_cps_extract.R`

**Removed case selection** so the extract includes all persons:

```diff
-    var_spec("WHYUNEMP", case_selections = as.character(1:6)),
+    "WHYUNEMP",
```

Updated the extract description string (used to match against IPUMS extract history) so it won't reuse the old filtered extract:

```diff
-extract_description <- "Unified CPS extract: WHYUNEMP demographics + state + nativity + occupation, 1994-present"
+extract_description <- "Unified CPS extract: all persons, demographics + state + nativity + occupation, 1994-present"
```

Updated comment to remove "(cases 1-6 only)" from the variable list.

**Note:** The WHYUNEMP filtering to codes 1-6 for the unemployed subset already happens correctly in the QMD code (`cps_4way` uses `filter(WHYUNEMP %in% 1:6)`), so removing the extract-level case selection doesn't change the unemployed analysis — it just ensures employed persons are present in the data for labor force denominators.

### 2. `unemployment_decomposition.qmd`

Fixed NATIVITY classification in all three locations (lines 187, 200, 1572):

```diff
-  NATIVITY == 5 ~ "Foreign Born", NATIVITY == 1 ~ "Native Born"
+  NATIVITY == 5 ~ "Foreign Born", NATIVITY %in% 1:4 ~ "Native Born"
```

## Action Required Before Re-Rendering

The existing `data/CPS/cps_unified.csv.gz` still contains only unemployed persons. Must delete and re-download:

```bash
rm data/CPS/cps_unified.xml data/CPS/cps_unified.csv.gz
"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" submit_unified_cps_extract.R
```

The new extract will be substantially larger (all persons, not just ~3-4% who are unemployed).

## Review Considerations

1. **Are there other parts of the QMD that assume the data only contains unemployed persons?** The `cps_4way` pipeline explicitly filters `WHYUNEMP %in% 1:6`, so it should be fine. But any code that uses `cps_raw` directly without filtering could now behave differently with the full-population extract.

2. **Does the larger extract cause memory issues?** The full CPS monthly microdata from 1994-present is ~30 years x 12 months x ~150k persons/month = ~54 million rows. The QMD already uses `cache: true` on the IPUMS loading chunk, which should help.

3. **NATIVITY = 0 (NIU) handling.** The code uses `case_when` which leaves NATIVITY = 0 as NA. These are filtered out by `filter(!is.na(foreign_born))` downstream. This is correct — NIU records (pre-1994 or missing) should be excluded from nativity analysis.
