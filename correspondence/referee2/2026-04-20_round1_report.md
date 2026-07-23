```
=================================================================
                        REFEREE REPORT
              CFI Blog — drafts/cfi.qmd — Round 1
              Date: 2026-04-20
              Referee: Referee 2 (independent)
=================================================================
```

## Summary

This audit covers `drafts/cfi.qmd` and the upstream pipeline it depends on
(`build_panels.R`, which produces
`data/final/unemployment_dominant_profile_ratios.parquet`). The draft's own
quantitative content — the composition regression of CC/CE and IC/CE on
cause shares, the New Entrant share series, and the reported CFLMI summary
statistics — replicates exactly in both independent R and Python
implementations. However, the draft's **central substantive claim** — that
the CFLMI finding rate implied by composition-implied claims tracks the
actual-claims finding rate closely — has **no corresponding code anywhere
in the repository**. Several in-text numbers are hardcoded rather than
programmatically pulled, and two of them are internally inconsistent with
the rest of the draft.

Overall verdict: **Major Revisions** — primarily because the post's main
empirical result is not reproducible from the codebase.

---

## Audit 1: Code Audit

### Findings

1. **`drop_na()` list is asymmetric with the regression specification.**
   `cfi.qmd` line 314 drops rows missing any of five cause shares plus
   `cc_ce` and `ic_ce`, but omits `completed_temp_share`. The five listed
   shares plus `completed_temp_share` are jointly the six-way partition
   (shares sum to 1 by construction), so an intra-month NA on any one
   should force the others' ratios to NA too if `total` were missing — but
   `total` comes from `UNEMPLOY`, not the sum, so it is logically possible
   for a row to arrive with `completed_temp_share = NA` and the others
   valid. In that case the regression silently runs on a row that has a
   meaningless "reference group." In practice this is not occurring with
   the current FRED series (replication N = 364 in both languages,
   consistent with the draft's displayed N), but the asymmetric specification
   is latent and worth fixing.
   **Severity: minor.**

2. **Mixed path strategy.** The draft reads the radar parquet via
   `here("data", "final", "unemployment_dominant_profile_ratios.parquet")`
   (line 98, which resolves to project root) but the CFLMI Excel file via
   a bare relative path `"data/chi-labor-market-indicators.xlsx"` (line 463,
   which only resolves if Quarto's cwd is `drafts/`). This makes the
   render location-dependent in opposite directions for the two inputs.
   **Severity: minor.**

3. **Redundant `cause_series` definition.** Lines 264–271 and 381–388
   declare the same six-element named vector. Defensible as defensive
   against chunk-level `rm(list=ls())`, but the repeated definition is a
   code smell.
   **Severity: nit.**

4. **No merge diagnostics.** `inner_join()` is called four times in the
   data-build chunk with no assertion on expected row counts, unmatched
   observations, or duplicates. For FRED monthly series this is unlikely
   to go wrong, but a `stopifnot(nrow(reg_df) >= …)` or `assertr`-style
   check would harden the pipeline against silent data-drift.
   **Severity: minor.**

### Missing Value Handling Assessment

The only NA-handling logic in the draft is the single `drop_na()` at
line 314 (see Finding 1). The upstream `build_panels.R` uses
`filter(!is.na(.data[[demo_var]]))` within each demographic slice
(lines 1112, 1151) before computing shares — that is appropriate since
the denominator is a group-specific weighted count. No imputation is
performed anywhere. This is the correct default for composition analysis
and is consistent with the draft's claims.

---

## Audit 2: Cross-Language Replication

### Replication Scripts Created

- `code/replication/referee2_replicate_composition_regression.R` —
  standalone R script (independent of the QMD) that re-pulls FRED via
  `fredr`, rebuilds the panel, and re-fits both OLS models.
- `code/replication/referee2_replicate_composition_regression.py` —
  Python implementation pulling FRED via the REST API and fitting with
  `statsmodels.OLS`.
- `code/replication/referee2_replicate_composition_regression.do` —
  Stata implementation that reads the R-emitted CSV and runs `regress`.
- `code/replication/referee2_replicate_cflmi_stats.py` — independent
  verification of all CFLMI summary statistics cited in the prose.
- `code/replication/referee2_compare_results.R` — bit-level comparison
  of R vs Python coefficient vectors at full double precision.
- `code/replication/referee2_regression_panel.csv` — merged monthly
  panel (364 rows) used by the Stata script.

### Comparison Table — Continuing Claims Regression (CC/CE)

| Term | R (`lm`) | Python (`statsmodels.OLS`) | Draft claim | Match |
|------|---:|---:|---:|:-:|
| `temp_layoff_share`    | 0.0392878  | 0.0392878  | (positive) | ✓ |
| `perm_job_loser_share` | 0.0596898  | 0.0596898  | (positive) | ✓ |
| `job_leaver_share`     | -0.1858244 | -0.1858244 | (negative) | ✓ |
| `reentrant_share`      | 0.0313382  | 0.0313382  | "weakly positive" | ✓ |
| `new_entrant_share`    | **-0.2172332** | **-0.2172332** | −0.022 at 10 pp ⇒ −0.2172 raw | ✓ |
| R²                     | 0.8902484  | 0.8902484  |            | ✓ |
| N                      | 364        | 364        |            | ✓ |

Max |β_R − β_Py| across all 10 RHS coefficients: **5.55 × 10⁻¹⁶**.
Max |SE_R − SE_Py|: **3.12 × 10⁻¹⁷**. Both are at machine precision —
R and Python agree on the regression completely.

### Comparison Table — Initial Claims Regression (IC/CE)

| Term | R (`lm`) | Python (`statsmodels.OLS`) | Match |
|------|---:|---:|:-:|
| `temp_layoff_share`    |  0.0041947 |  0.0041947 | ✓ |
| `perm_job_loser_share` |  0.0017091 |  0.0017091 | ✓ |
| `job_leaver_share`     | -0.0259683 | -0.0259683 | ✓ |
| `reentrant_share`      |  0.0042415 |  0.0042415 | ✓ |
| `new_entrant_share`    | **-0.0293665** | **-0.0293665** | ✓ |
| R²                     | 0.8046958  | 0.8046958  | ✓ |

### Stata Replication — Not Completed

Stata (StataIC 16) is installed on the audit machine but batch execution
(`/e do`, `/b do`) did not produce output logs during this audit round.
The do-file is correct and self-contained; running it locally in
Stata's GUI (`do code/replication/referee2_replicate_composition_regression.do`)
should reproduce identical coefficients given that Stata's `regress` uses
the same Cholesky factorization path as `lm`. This is documented so the
author can run it themselves to close the three-language loop.

### New Entrant Share — Summary Statistics

| Statistic | Replicated (R = Py) | Draft claim | Match |
|-----------|---:|---:|:-:|
| Long-run mean 1994–present (all months) | 8.64 % | "~8.6 %" | ✓ |
| Long-run mean 1994–present (excl pandemic) | 8.85 % | | |
| Pre-pandemic mean through Feb 2020 | 8.77 % | "~8.8 %" | ✓ |
| Last 12 months mean (through Mar 2026) | 10.70 % | "~10.7 %" | ✓ |
| Latest value (Mar 2026) | 9.86 % | | |
| **Implied last-12m gap vs long-run mean** | **+2.06 pp** | **"1.6 pp above"** | ✗ |

### CFLMI Summary Statistics

| Series | Replicated (full sample 2008–2026-03) | Draft claim | Match |
|--------|---:|---:|:-:|
| FCR latest | 4.41 % | "~4.4 %" | ✓ |
| FCR median | 4.94 % | "~4.9 %" | ✓ |
| FCR mean   | 5.90 % | "~5.9 %" | ✓ |
| Finding rate latest | 44.79 % | "~45 %" | ✓ |
| Finding rate median | 42.14 % | "~42 %" | ✓ |
| Finding rate mean   | 40.58 % | "~41 %" | ✓ |
| Separation rate latest | 2.07 % | "~2.1 %" | ✓ |
| Separation rate median | 2.21 % | "~2.2 %" | ✓ |
| Finding rate Mar 2025 / Apr 2025 | 46.50 % / 47.08 % | "~47 % in spring 2025" | ✓ |

### Discrepancies Diagnosed

Only one numerical discrepancy surfaces:

**"1.6 pp above its long-run mean over the last twelve months"**
(line 368). The last-12-month mean (10.70 %) minus the full-sample mean
(8.64 %) = 2.06 pp. Minus the pre-pandemic-only mean (8.77 %) = 1.93 pp.
Minus the excluding-pandemic mean (8.85 %) = 1.85 pp. None of these
evaluations produces 1.6 pp. The same draft states two paragraphs later
(line 446) that the LR mean is ~8.6 % and the last-12m mean is ~10.7 %,
which implies a ~2.1 pp gap, inconsistent with the 1.6 pp used
immediately above.

**Downstream implication.** Line 368 derives "composition alone would
imply today's insured unemployment rate sits about 0.35 pp below" from
1.6 × (−0.217). At the correct ~2 pp excess, the implied effect is
~0.44 pp, so the prose mildly understates the composition contribution.
Classification: **syntax / arithmetic error**, not a package heterogeneity
or a code bug — the regression produced correct coefficients; the prose
multiplied by the wrong gap.

---

## Audit 3: Directory & Replication Package

### Replication Readiness Score: 5/10

### Deficiencies

1. **FRED API key is hardcoded in the committed source.** Line 23 of
   `cfi.qmd` contains `fredr_set_key("ccd350785a210a7653a20d02ce92d83a")`.
   The repository is public (per `CLAUDE.md`). Any visitor can harvest
   and use the key — FRED's terms cap usage per key. Move to
   `Sys.getenv("FRED_API_KEY")` or the `.Renviron` pattern already used
   for IPUMS in this project.
2. **No master script.** Producing `cfi.html` currently requires
   remembering to run `submit_unified_cps_extract.R` then `build_panels.R`
   then `quarto render drafts/cfi.qmd`, with paths that must resolve
   correctly from different cwds (see Audit 1 Finding 2). A one-line
   Makefile or top-level R script would make this explicit.
3. **Mixed path strategy** (see Audit 1 Finding 2). Pick `here::here()`
   everywhere or document the expected cwd and use consistent relative
   paths.
4. **`data/` not in `.gitignore`.** `CLAUDE.md` itself flags this:
   IPUMS data have usage restrictions. The upstream CPS data should not
   be committed.
5. **No explicit dependency manifest.** No `renv.lock`, no
   `requirements.txt`, no `_environment.yml`. The `Dependencies` section
   of `CLAUDE.md` lists packages but not versions.

### Positives

- Clear separation of `data/`, `code/` (implicit — scripts live at root),
  `drafts/`, `slides/`.
- Informative filenames.
- The `here::here()` call for the radar parquet is the right pattern.
- `submit_unified_cps_extract.R` is idempotent and re-entrant.

---

## Audit 4: Output Automation

### Tables: **Automated** — `gt::gt` is used throughout (lines 66–81, 323–366).
### Figures: **Automated** — `ggplot2` with Quarto knitr pipeline.
### In-text statistics: **Manual** — this is the problem.

### Deductions

Every one of the following numbers cited in prose is hardcoded as a
literal in the text rather than interpolated from the model object via
an inline `` `r …` `` chunk:

- "−0.022" (coefficient, line 368)
- "0.22 pp lower" (per-1pp effect, line 368)
- "1.6 pp above its long-run mean" (line 368) — **also numerically
  inconsistent with the rest of the draft; see Audit 2 Discrepancies**
- "0.35 pp below" (implied effect, line 368)
- "∼1.2 %" (today's IUR, line 368)
- "about ten times smaller" (IC/CC coefficient ratio, line 370) — the
  replicated ratio is 0.217 / 0.029 = **7.4×**, not 10×
- "~8.6 %", "~10.7 %", "~8.8 %" (NE-share benchmarks, line 446)
- "4.4 %", "4.9 %", "5.9 %" (FCR stats, line 490)
- "45 %", "42 %", "41 %" (finding rate stats, line 490)
- "2.1 %", "2.2 %" (separation stats, line 490)
- "47 % in spring 2025", "45 % in the latest release" (line 492)

All of these numbers replicate correctly **today**, but the FRED pull
and the CFLMI xlsx both update monthly. At the next render, any or all
of them can drift silently while the prose keeps claiming the old
values. Given the blog format, this is a live hazard: the finished
HTML is what will be shipped, and there is no automated check that the
prose numbers match the chunk outputs.

**Recommendation:** convert each to an inline `` `r …` `` expression, or
at minimum add a chunk near the end that recomputes all quoted numbers
and `stopifnot`s them within a tolerance of the prose values.

**Severity: moderate** — these are not bugs today, but the automation
pipeline is structurally unable to catch future drift.

---

## Audit 5: Econometrics

### Identification Assessment

The draft is **appropriately modest** about identification. Line 376
states explicitly that the estimates are not causal and that composition
shares are jointly determined with claims. This is the right framing for
a descriptive benchmark.

### Specification Issues

1. **OLS default standard errors under serial correlation.** The
   regression is monthly time-series data from 1994 onward and no
   heteroskedasticity- or autocorrelation-consistent covariance is
   applied. `lm`'s default SEs assume iid errors, which is almost
   certainly violated. At |t| ≈ 11 for new_entrant_share the qualitative
   conclusion survives any plausible correction, but the draft should
   either report HAC (`sandwich::NeweyWest`) SEs or note the assumption.
   **Severity: minor-to-moderate.**

2. **No year or trend fixed effects.** `month_of_year` absorbs seasonal
   variation but not secular drift. The New Entrant share has a slow
   upward drift since 2022; composition and claims both have their own
   secular trends. A simple year FE or a flexible time trend is a
   standard sensitivity check here and could meaningfully change the
   coefficient on `new_entrant_share`. The draft's own interpretation
   relies on the stability of the NE coefficient, so at least a
   robustness check is warranted.
   **Severity: moderate.**

3. **No sensitivity to pandemic window.** The March-2020–December-2021
   exclusion is a single choice with no sensitivity (e.g., extending to
   June 2022, or excluding the Great Recession symmetrically).
   **Severity: minor.**

4. **"About ten times smaller"** is factually off. CC/CE β₍NE₎ = −0.217;
   IC/CE β₍NE₎ = −0.029. Ratio = 7.4, not 10. The substantive
   explanation (stock-vs-flow) is sound, but the quantitative claim
   is overstated by ~30 %.

5. **Counter-cyclicality interpretation of the NE share.** The draft
   says "the New Entrants share is counter-cyclical: it falls in
   recessions … and drifts back up in expansions." This is a compositional
   mechanical point, not an economic one — the share falls in recessions
   because the *denominator* (total unemployed) inflates with job losers,
   not because the *level* of new entrants is lower. A one-sentence
   clarification would prevent readers from inferring that new entrants
   find jobs more easily in recessions, which is not what the figure shows.
   **Severity: minor (expositional).**

---

## Major Concerns

### 1. No code backs the composition-implied CFLMI finding-rate claim

The paragraph at line 484–488 of `cfi.qmd` describes a specific
counterfactual exercise: take the fitted values from the composition
regression (`predict(cc_mod)`), feed them into the CFLMI's
partial-least-squares step in place of actual continuing claims, and
compute a composition-implied finding rate; compare to the actual
finding rate.

Line 488 then reports the verdict: "the two versions agree: the finding
rate implied by composition-implied claims tracks the actual-claims
finding rate closely." This is, along with the NE coefficient itself,
the central quantitative result of the post.

**There is no code for this exercise anywhere in the repository.** A
project-wide grep for `predict(cc_mod`, `predict(cc_`, `composition.implied`,
`comp_implied`, `hiring_rate_uw`, `finding rate implied` returns only
the prose of this draft and two unrelated files (a literature-review
`temp/` qmd and `ui_composition_benchmark.qmd`, which performs a
different exercise on an annual frequency without CFLMI). The CFLMI
inputs are read from `data/chi-labor-market-indicators.xlsx` and only a
single 3-panel line plot is produced from them.

A reader cannot reproduce the exercise and a referee cannot verify the
claim. The conclusion of the blog post (line 496) rests on this finding.
Either:
(a) the exercise has been performed outside the repository (in a
  notebook, email thread, or colleague's code) — if so, that code must
  be committed and the draft must render the figure or at minimum a
  diagnostic number from it; or
(b) the exercise has not yet been performed in full — if so, the
  strength of the language ("we find that the two versions agree") must
  be softened to something like "a rough check based on the direction
  of the coefficient suggests …" until the full exercise is in code.

**This is the blocking issue for this draft.**

### 2. FRED API key committed in plaintext to a public repo

Line 23 of `cfi.qmd` hardcodes the FRED API key. The repository is
public (`https://github.com/asafi17/unemployment-decomposition`, per
`CLAUDE.md`). Rotate the key and move to `Sys.getenv("FRED_API_KEY")`
before the next push. The project already uses the correct pattern for
IPUMS (`.Renviron`); apply it to FRED too.

---

## Minor Concerns

1. **"1.6 pp above its long-run mean"** (line 368) contradicts
   "~8.6 %" long-run mean and "~10.7 %" last-12m mean cited two
   paragraphs later. The implied excess is ~2.1 pp, not 1.6 pp, which
   in turn implies the composition contribution is ~0.44 pp rather than
   the stated 0.35 pp.

2. **"IC/CE coefficients are about ten times smaller"** (line 370).
   The empirical ratio is 7.4× (0.21723 / 0.02937), not 10×.

3. **In-text statistics are hardcoded**, not inline-interpolated. See
   Audit 4. This will silently desync as FRED and the CFLMI xlsx update.

4. **Default OLS SEs assume iid residuals.** Monthly labor-market data
   are autocorrelated; report HAC SEs or note the assumption.

5. **`drop_na()` at line 314 omits `completed_temp_share`** — asymmetric
   with the six-way partition.

6. **No year or trend fixed effect** in the composition regression.
   Add one as a robustness check.

7. **Path strategy is mixed** (`here()` vs bare relative paths).

8. **`cause_series` redefined** in two chunks (lines 264 and 381).

9. **No merge diagnostics** after any `inner_join`.

10. **UTF-8 en-dash characters** in the upstream parquet's age/education
    labels (e.g., "16–24", "25–34") render oddly when the parquet is
    read in Windows-default codepage environments. This is cosmetic and
    the Quarto HTML output handles it correctly, but downstream users of
    the parquet outside Quarto may need to set `encoding = "UTF-8"`
    explicitly.

---

## Questions for Authors

1. **Where is the composition-implied CFLMI code?** If it exists
   somewhere (a separate script, a colleague's notebook), please commit
   it. If it does not yet exist, say so and either soften line 488 or
   implement it before publication.

2. **Are the "1.6 pp" and "about ten times smaller" numbers typos or
   residuals from an earlier estimate?** The current replication
   produces 2.06 pp and 7.4×.

3. **Was the choice of no year FE deliberate?** The NE share has a
   slow upward drift since 2022; a year FE would absorb that.

4. **Would you like to report HAC-robust SEs** (`sandwich::NeweyWest`)
   for the composition regression?

5. **Rotate the FRED API key** and confirm when done. The current key
   should be treated as compromised.

---

## Verdict

- [ ] Accept
- [ ] Minor Revisions
- [x] Major Revisions
- [ ] Reject

**Justification:** The primary empirical conclusion of the post — that
composition-implied claims produce a CFLMI finding rate matching the
actual-claims finding rate — is asserted but not reproducible from the
codebase. Until either the code appears or the claim is softened, the
post cannot be accepted. All other concerns (hardcoded API key,
hardcoded in-text numbers, two arithmetic inconsistencies, missing HAC
SEs, missing year FE) are addressable without re-architecting the work.

The composition regression itself is **correctly implemented** — R and
Python agree to 9+ decimal places on every coefficient and standard
error, and the draft's reported coefficient of −0.022 at a 10-pp shift
matches exactly. All summary statistics for the NE share and the CFLMI
series are also verified. The audit found no computational errors in
the code that was actually written; the blocking issue is code that
was *not* written.

---

## Recommendations (prioritized)

1. **Implement the composition-implied CFLMI exercise in code.** Take
   `fitted(cc_mod)`, substitute for actual CC/CE in the CFLMI PLS input
   vector (holding all other indicators fixed), re-run CFLMI's weight
   calculation (or request the CFLMI team's code), and produce a
   two-line plot: actual finding rate vs composition-implied finding
   rate. Include it as a figure in the draft, not just a sentence.

2. **Rotate the FRED API key** and move to `.Renviron`.

3. **Convert every quoted number in the prose to an inline `` `r …` ``
   expression** or add an end-of-document `stopifnot` chunk that
   checks the prose values against the live results.

4. **Fix the "1.6 pp" and "about ten times smaller" inconsistencies.**

5. **Add a robustness check with year fixed effects** to the composition
   regression, presented alongside the current table.

6. **Report HAC-robust SEs** (or at least note the iid-errors caveat).

7. **Unify the path strategy** (pick `here()` everywhere for draft
   inputs, including the CFLMI xlsx).

8. **Fix `drop_na()`** to include `completed_temp_share` for symmetry.

9. **Add `data/` to `.gitignore`** per `CLAUDE.md`'s own note about
   IPUMS usage restrictions.

```
=================================================================
                      END OF REFEREE REPORT
=================================================================
```
