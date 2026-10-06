# An Uneven Chance of Survival

Analyses and figures for the UN IGME report on risk factors associated with
under-five mortality: how child mortality differs by household wealth, mother's
education, residence, mother's age, birth interval, birth order, birth size,
ethnicity and subnational region.

Scripts run from the project root, which `Equity brochure.Rproj` sets as the
working directory.

The raw equity database, the UN IGME estimates and the IGME wealth quintile model
are held internally and are not in this repository, so the pipeline cannot be
rerun from a clone alone. The ethnicity dataset and the subnational survey
database are produced by separate projects, and the DHS and MICS microdata behind
the fertility box are subject to DHS and IPUMS terms. For any of these, please
contact the authors of the report.

```text
code/
  01-07 pipeline, run-config.R, run-pipeline.R, DATA_FLOW.md
  fertility_risk_factors_from_ipums.R
  figures and tables/     one script per published figure, map and table
  lib/                    shared loaders and validation helpers
  tests/                  checks on the website files
input/                    inputs no script here produces
output/                   pipeline outputs and the number-check report
Subnational/              subnational database build and its outputs
```

## code/

The pipeline, run in order or through `run-pipeline.R`:

| Script | Purpose |
| --- | --- |
| `01_clean-data.R` | Clean the raw equity database into the country-specific files |
| `02_produce-aggregate.R` | Birth-weighted world aggregates, benchmarked to IGME national estimates |
| `03_compile_data_for_cme.R` | Data files for the childmortality.org risk-factor pages |
| `04_check_output.R` | Quality checks; exits non-zero on failure |
| `05_make_cme_wide_format.R` | Wide SDMX file for the website upload |
| `06_trend_projection_benchmarked.R` | Projections to 2000 and 2024 for the trend claims |
| `07_inequality_indices.R` | Concentration, slope and Theil indices for Figures 31-32 |

`run-config.R` holds the data vintage and output paths shared by all of them, and
`DATA_FLOW.md` traces how the files feed each other. `lib/` holds the shared
loaders and validation helpers, and `tests/` checks the website files built by 03
to 05. `fertility_risk_factors_from_ipums.R` is standalone: it rebuilds the
Figure 18 fertility box from DHS and MICS microdata.

`figures and tables/` holds one script per published figure, map or table, each
reading the pipeline outputs and writing to the figures and tables folders.

## input/

Inputs that no script here produces: the ethnicity dataset, from a separate
analysis of DHS and MICS surveys, and the fertility model workbook.

Two inputs are excluded as their publishers' data. The DHS and MICS microdata for
the fertility script are covered by DHS and IPUMS terms. Figure 26 uses a DHS API
extract of stillbirth and live-birth counts by background characteristic
(indicators `CMPNMRCSBR` and `CMPNMRCUNN`, <https://api.dhsprogram.com/rest/dhs/data>),
read from `input/stillbirth/`.

## output/

Everything the pipeline writes: `country/` and `aggregate/` for the analysis
files, `cme/` for the website files, `qa/` for the check output, and `other/` for
the inequality indices.

It also holds the Quarto number-check report, which recomputes every number in the
brochure from these outputs and colours it against the printed PDF: black where
the two agree, red where they do not.

## Subnational/

`R/` builds the subnational mortality data for upper-middle- and high-income
countries. `00` and `01` fetch the Eurostat tables and the country sources that
have a stable URL or API; `10` and `11` turn them into the workbooks in `output/`.
The cached downloads and the Eurostat workbook they rebuild are not in the
repository; `subnational database_supplementary.xlsx` and
`subnational sources.xlsx` are.

The subnational survey database is a separate UN IGME publication, available at
<https://childmortality.org>, and the maps and tables read it alongside these
outputs.
