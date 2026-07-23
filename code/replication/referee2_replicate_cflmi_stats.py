"""
Referee 2 — Python verification of CFLMI summary statistics claimed in drafts/cfi.qmd.

The draft claims (for the CFLMI "Rates" sheet):
  * Flow-consistent rate (fcr): latest ~4.4%, median ~4.9%, mean ~5.9%
  * Finding rate (hiring_rate_uw): latest ~45%, median ~42%, mean ~41%
  * Separation rate (layoffs_other_seps): latest ~2.1%, median ~2.2%
  * Finding rate drift: ~47% in spring 2025 down to ~45% in latest release
"""
from __future__ import annotations
import json
from pathlib import Path
import pandas as pd

# Raw CFLMI workbook fetched/maintained by build_panels.R section 8d (see reference_cflmi_source).
CFLMI_XLSX = Path(__file__).resolve().parents[2] / "data" / "CFLMI" / "chi-labor-market-indicators.xlsx"
OUT_JSON = Path(__file__).resolve().parent / "referee2_cflmi_results_python.json"

def main() -> None:
    xl = pd.ExcelFile(CFLMI_XLSX)
    print("Sheets:", xl.sheet_names)
    df = pd.read_excel(CFLMI_XLSX, sheet_name="1. Rates")
    print("Columns:", list(df.columns))
    print("Head:")
    print(df.head(3))
    print("Tail:")
    print(df.tail(3))

    df["date"] = pd.to_datetime(df["date"])
    df = df.sort_values("date").reset_index(drop=True)

    cols = ["hiring_rate_uw", "layoffs_other_seps", "fcr"]
    stats = {}
    for c in cols:
        s = pd.to_numeric(df[c], errors="coerce").dropna()
        latest_row = df.loc[df[c].notna()].iloc[-1]
        stats[c] = {
            "latest_date": str(latest_row["date"].date()),
            "latest": float(latest_row[c]),
            "full_sample_mean": float(s.mean()),
            "full_sample_median": float(s.median()),
            "full_sample_min": float(s.min()),
            "full_sample_max": float(s.max()),
            "n_obs": int(s.shape[0]),
        }

    # Finding rate in Spring 2025 vs latest
    find_spring = df[(df["date"] >= "2025-03-01") & (df["date"] <= "2025-05-01")]["hiring_rate_uw"].dropna()
    stats["hiring_rate_uw_spring2025_mean"] = float(find_spring.mean()) if len(find_spring) else None
    stats["hiring_rate_uw_spring2025_values"] = [
        (str(r["date"].date()), float(r["hiring_rate_uw"]))
        for _, r in df[(df["date"] >= "2025-03-01") & (df["date"] <= "2025-05-01") & df["hiring_rate_uw"].notna()].iterrows()
    ]

    OUT_JSON.write_text(json.dumps(stats, indent=2))
    print(json.dumps(stats, indent=2))

if __name__ == "__main__":
    main()
