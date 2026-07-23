"""
Referee 2 cross-language replication — Python.

Independently reproduces the composition regression from drafts/cfi.qmd.
Pulls FRED series via the public API (no fredr), builds monthly cause-share
dataset, drops the pandemic window (Mar 2020 – Dec 2021), and runs:

    cc_ce ~ temp_layoff + perm_job_loser + job_leaver + reentrant + new_entrant
            + month-of-year FE    (Completed Temp Job omitted)

Also runs ic_ce on the same RHS, computes New Entrant share statistics
(long-run mean, last-12m mean, pre-pandemic mean), and dumps results to a
machine-readable comparison table.

Run:
    python code/replication/referee2_replicate_composition_regression.py
"""
from __future__ import annotations

import io
import json
import os
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import requests
import statsmodels.formula.api as smf

FRED_KEY = "ccd350785a210a7653a20d02ce92d83a"
FRED_URL = "https://api.stlouisfed.org/fred/series/observations"
START = "1994-01-01"
OUT_JSON = Path(__file__).resolve().parent / "referee2_results_python.json"

CAUSE_SERIES = {
    "Temporary Layoff":     "LNS13023653",
    "Permanent Job Losers": "LNS13026638",
    "Completed Temp Job":   "LNS13026637",
    "Job Leavers":          "LNS13023705",
    "Reentrants":           "LNS13023557",
    "New Entrants":         "LNS13023569",
}
TOTAL_SERIES = "UNEMPLOY"
CLAIMS_SERIES = ["CCSA", "ICSA", "COVEMP"]

SHARE_COLS_WITH_PANDEMIC = [
    "temp_layoff_share", "perm_job_loser_share", "completed_temp_share",
    "job_leaver_share", "reentrant_share", "new_entrant_share",
]
RHS_SHARE_COLS = [c for c in SHARE_COLS_WITH_PANDEMIC if c != "completed_temp_share"]


def fred_series(series_id: str, max_retries: int = 5) -> pd.DataFrame:
    import time
    params = {
        "series_id": series_id,
        "api_key": FRED_KEY,
        "file_type": "json",
        "observation_start": START,
    }
    last_err = None
    for attempt in range(max_retries):
        try:
            resp = requests.get(FRED_URL, params=params, timeout=60)
            resp.raise_for_status()
            obs = resp.json()["observations"]
            df = pd.DataFrame(obs)[["date", "value"]]
            df["date"] = pd.to_datetime(df["date"])
            df["value"] = pd.to_numeric(df["value"], errors="coerce")
            df = df.rename(columns={"value": series_id})
            time.sleep(0.4)  # gentle throttle
            return df
        except Exception as e:
            last_err = e
            time.sleep(2 * (attempt + 1))
    raise RuntimeError(f"FRED fetch failed after {max_retries} retries: {last_err}")


def main() -> None:
    # --- Pull monthly cause series + UNEMPLOY
    cause_dfs = []
    for name, sid in CAUSE_SERIES.items():
        d = fred_series(sid).rename(columns={sid: name})
        cause_dfs.append(d)
    cause_wide = cause_dfs[0]
    for d in cause_dfs[1:]:
        cause_wide = cause_wide.merge(d, on="date", how="outer")

    total = fred_series(TOTAL_SERIES).rename(columns={TOTAL_SERIES: "total"})

    # --- Pull weekly claims series; collapse to monthly mean
    claims_frames = []
    for sid in CLAIMS_SERIES:
        d = fred_series(sid)
        d["month"] = d["date"].dt.to_period("M").dt.to_timestamp()
        monthly = d.groupby("month")[sid].mean().reset_index()
        monthly = monthly.rename(columns={"month": "date"})
        claims_frames.append(monthly)
    claims_wide = claims_frames[0]
    for d in claims_frames[1:]:
        claims_wide = claims_wide.merge(d, on="date", how="outer")
    claims_wide["cc_ce"] = claims_wide["CCSA"] / claims_wide["COVEMP"]
    claims_wide["ic_ce"] = claims_wide["ICSA"] / claims_wide["COVEMP"]

    # --- Build regression dataset
    df = cause_wide.merge(total, on="date", how="inner")
    for name in CAUSE_SERIES:
        col = name.lower().replace(" ", "_")
        # The same column naming the QMD uses
        pretty_map = {
            "Temporary Layoff": "temp_layoff_share",
            "Permanent Job Losers": "perm_job_loser_share",
            "Completed Temp Job": "completed_temp_share",
            "Job Leavers": "job_leaver_share",
            "Reentrants": "reentrant_share",
            "New Entrants": "new_entrant_share",
        }
        df[pretty_map[name]] = df[name] / df["total"]
    df["month_of_year"] = df["date"].dt.strftime("%b")
    df["pandemic"] = (df["date"] >= pd.Timestamp("2020-03-01")) & (
        df["date"] <= pd.Timestamp("2021-12-01")
    )
    df = df.merge(claims_wide[["date", "cc_ce", "ic_ce"]], on="date", how="inner")
    reg_df = df[~df["pandemic"]].dropna(
        subset=["cc_ce", "ic_ce"] + RHS_SHARE_COLS
    ).copy()

    formula_rhs = " + ".join(RHS_SHARE_COLS) + " + C(month_of_year)"
    cc_mod = smf.ols(f"cc_ce ~ {formula_rhs}", data=reg_df).fit()
    ic_mod = smf.ols(f"ic_ce ~ {formula_rhs}", data=reg_df).fit()

    def extract(model):
        out = {}
        for col in RHS_SHARE_COLS:
            est = model.params[col]
            se = model.bse[col]
            p = model.pvalues[col]
            out[col] = {
                "estimate_raw": float(est),
                "se_raw": float(se),
                "coef_10pp": float(est * 0.10),
                "se_10pp": float(se * 0.10),
                "p_value": float(p),
            }
        out["_r_squared"] = float(model.rsquared)
        out["_n"] = int(model.nobs)
        return out

    results = {
        "language": "python",
        "package": "statsmodels.OLS",
        "statsmodels_version": __import__("statsmodels").__version__,
        "sample_first_date": str(reg_df["date"].min().date()),
        "sample_last_date": str(reg_df["date"].max().date()),
        "n_obs": int(len(reg_df)),
        "cc_ce": extract(cc_mod),
        "ic_ce": extract(ic_mod),
    }

    # --- New Entrant share descriptive stats (from the share series itself, not the regression sample)
    ne = df[["date", "new_entrant_share"]].dropna().copy()
    ne["pct"] = ne["new_entrant_share"] * 100
    latest_date = ne["date"].max()
    pre_pandemic = ne[ne["date"] <= "2020-02-01"]
    last12 = ne[ne["date"] > (latest_date - pd.DateOffset(months=12))]
    results["new_entrant_share_pct"] = {
        "latest_date": str(latest_date.date()),
        "long_run_mean_1994_present_nonpandemic": float(
            ne.loc[(ne["date"] < "2020-03-01") | (ne["date"] > "2021-12-01"), "pct"].mean()
        ),
        "long_run_mean_1994_present_all": float(ne["pct"].mean()),
        "pre_pandemic_mean_through_2020_02": float(pre_pandemic["pct"].mean()),
        "last_12_months_mean": float(last12["pct"].mean()),
        "latest_value": float(ne.iloc[-1]["pct"]),
    }

    OUT_JSON.write_text(json.dumps(results, indent=2))
    print(json.dumps(results, indent=2))


if __name__ == "__main__":
    main()
