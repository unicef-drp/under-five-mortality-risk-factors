# Equity pipeline

Run R scripts from the Equity brochure project root. Shared versions and output paths are in `code/run-config.R`. Shared raw-file loading and group-label normalization are in `code/lib/equity-common.R`; selection rules remain in their respective scripts.

## Data paths

```text
Raw data_<dimension>_<date>.csv
  + shared loading and group-name normalization
  |
  +-- 01: report filters and recent-survey selection
  |     +-- country files for publication and aggregation
  |     +-- source education two-group pairs for the selected surveys
  |     +-- derive only the missing combined group for hybrid surveys
  |              |
  |              +-- 02: birth-weighted world aggregates
  |
  +-- 03: broader historical website observations
        +-- complete source two-group and four-group education sets
        +-- supplement missing contributing hybrid pairs from 01
        +-- flag contributors using 02's actual survey identity
                 |
                 +-- 04: report-input QA and delivered-CME QA
                 +-- 05: SDMX wide export and existing wealth source XLSX
```

Script 01's year, income, visibility, birth-weight and report exclusions do not define the website's history. Script 03 retains its separate visibility preference, historical coverage and reference-date selection. Education two-group and four-group sets are validated independently; partial category sets are omitted, with baseline differences available in QA.

## Country outputs retained

Script 05 writes `output/cme/riskfactors-sdmx-upload_<RUN_DATE>.csv`, the SDMX upload file containing country observations and world aggregates. This replaces the former `riskfactors-wide_<RUN_DATE>.csv` filename; data and schema are unchanged. The existing `RISKFACTORS_WIDE_OUT_PATH` environment override remains supported.

World aggregate rows use the agreed dimension-specific `REF_AREA` codes:
`AGG_RESIDENCE`, `AGG_WEALTH`, `AGG_EDU` (both education groupings),
`AGG_MOTHERAGE`, `AGG_BIRTHINTERVAL`, and `AGG_BIRTHORDER`. Script 05
assigns these from the aggregate input's `Label`; do not export them as
`WORLD`, because the website selects aggregates by these codes.

All filenames retain the existing date suffix and remain compatible with their consumers.

| File stem | Meaning | Consumers |
|---|---|---|
| `db-all-surveys` | Report-eligible history before selecting the latest survey; already subject to report exclusions | Script 04 report-input QA and report Table 1 |
| `db-pre-aggregation` | Selected survey snapshot before later reference-date, zero-rate and birth-count restrictions and derivations | Availability maps and report tables, including rates and source metadata |
| `country-specific-database-cleaned` | Selected data including NMR, U5MR, MR1t59 and birth rows | Scripts 02, 03 supplementation, 06 and publication code |
| `country-specific-nbirth` | Explicit birth counts for aggregate weighting | Script 02 |
| `country-specific-database-cleaned-igme-wealth` | Cleaned data with wealth U5MR replaced by the existing IGME model estimates | Report tables, availability maps and Figure 3 |

Neither `db-all-surveys` nor `db-pre-aggregation` is the website's main input. No country output was retired in this cleanup. Publication scripts and script 06 still use their existing filenames/latest-file lookup; point them only at a reviewed production run.

## Education source preference

- For selected four-group surveys, script 01 looks up the supplied `noedu.primary` and `sec.higher` observations at the same survey, indicator, reference date and interval. It no longer recalculates all such pairs using birth weights.
- Companion source pairs are added after birth-count preparation and tagged `source edu 2 groups alongside 4 groups`. Script 02 excludes these companion rows from its inputs, preventing double-counting alongside the four original categories.
- Script 02 still creates the combined world education aggregate by summing deaths and births across groups. Its weighting and aggregate results are unchanged by the cleanup.
- MR1t59 is derived from matching source two-group U5MR/NMR using the existing mortality identity. It is tagged `derived edu 2 groups alongside 4 groups` and excluded from 02's NMR/U5MR loading.
- Hybrid surveys lacking a source combined category retain their narrow derivation. In the verified run, VNM MICS 2013–2014 (5580) requires `noedu.primary` from `noeducation + primary`; its `sec.higher` value is supplied.
- An unavailable or ambiguous source pair in a selected complete-four-group survey stops script 01 with the affected observation keys. Review the exception rather than silently returning to blanket recalculation.
- Script 03 prefers source observations. Its report supplementation is a missing-row fallback, not a replacement of supplied estimates. `qa/education-selection.csv` distinguishes `source`, `report-source`, and `report-derived` observations without adding columns to the exported schema.

## Aggregate flag

The long CME file's `in_aggregate` describes a survey's contribution for a dimension, indicator and group. A supplied display value need not equal a transformed value used in aggregation. Keys include country, survey name/year, dimension, group and indicator; catalog IDs alone can cover multiple series or years. National totals use contribution in any displayed dimension for that survey.

Script 05 retains its separate export rules: wealth U5MR source points are display-only, and total flags are recomputed by indicator after the wealth override. Script 04 checks long-file rules; the final wide-file comparisons check the delivered SDMX data.

## Reproducible run

Use an explicit fresh review directory to keep production outputs available while inspecting changes:

```powershell
Rscript --vanilla code/run-pipeline.R tmp/my-review/output
```

The runner executes scripts 01–05 sequentially, saves logs and settings, and verifies hashes of 19 input files and the script files before/after the run. It records package/session information and output hashes under `qa`. It clears the single-file wide-output override so every file stays in the requested run directory. Individual scripts still support `EQUITY_OUTPUT_ROOT` and `EQUITY_RUN_DATE`; absent overrides they use the existing `output` directory and configured date.

Tests and comparison commands:

```powershell
Rscript --vanilla code/tests/check-cme-flow.R
Rscript --vanilla code/tests/check-qa-failure.R tmp/my-review/output
Rscript --vanilla code/tests/compare-cme-runs.R tmp/data-flow-review-2026-09-15/baseline/output tmp/my-review/output
```

The comparison requires run manifests in both directories. Its BGD regression expectation covers the current 2025-05-01 source vintage (11 eligible surveys); review that expectation when changing the input vintage. It allows explained education-source replacements and partial-set removals while enforcing exact aggregate equality and stable non-education content. The negative QA test intentionally removes one required BGD observation in its own copied test directory and requires script 04 to fail.

## Data review versus pipeline validation

Passing pipeline tests establishes source preservation, consistency and export structure; it does not certify every source value. `qa/education-source-review.csv` flags an observed pattern where the two combined categories exactly copy the noeducation/primary source values despite distinct secondary/higher values. Review those source cases before replacing production or regenerating the report. The pipeline does not alter the raw database or silently correct those values.

Current candidate and numerical review: `tmp/data-flow-review-2026-09-15/candidate/output/qa`. Original scripts and previously saved CSVs are preserved under `tmp/data-flow-review-2026-09-15/archive`.
