# Reshape the long CME file from 03 into the wide SDMX format the equity web apps
# upload. Each dimension is a column: a row sets one dimension to its SDMX code
# and the rest to "_T", and total rows are "_T" throughout.
#
# Input:  output/cme/riskfactors-data-CMEinfo.csv
# Output: output/cme/riskfactors-sdmx-upload_<date>.csv
#
# Wealth U5MR in_aggregate is forced to FALSE, because the app shows the IGME
# modelled aggregate; the survey rows themselves are replaced in section 5b.
# A total row is in_aggregate when any non-total dimension of the same survey and
# indicator is, so it drops to FALSE where wealth was the only contribution.
# Values of exactly 500, 1000 and 9999 are dropped at export and written to
# *-excluded-exact-values.csv. World rows use the dimension-specific AGG_*
# REF_AREA codes rather than WORLD.

rm(list = ls())

library(dplyr)
library(data.table)
library(openxlsx)

USERPROFILE <- Sys.getenv("USERPROFILE")
source(file.path(USERPROFILE, "Dropbox/UNICEF Work/profile.R"))
source("code/run-config.R")
cfg <- equity_run_config()


# ---- 0. Paths and parameters ----

RUN_DATE <- cfg$run_date

output_dir <- cfg$output_dir
dir_cme    <- file.path(output_dir, "cme")

in_path  <- file.path(dir_cme, "riskfactors-data-CMEinfo.csv")
agg_world_path <- file.path(dir_cme, "UNIGME2025_risk_factor_aggregate_world.csv")
default_out_path <- file.path(dir_cme, paste0("riskfactors-sdmx-upload_", RUN_DATE, ".csv"))
out_path <- Sys.getenv("RISKFACTORS_WIDE_OUT_PATH", unset = default_out_path)

# Survey points shown beneath the modelled wealth lines on the data page. These
# replace the pipeline's survey wealth U5MR rows.
wealth_u5mr_xlsx <- file.path(USERPROFILE,
  "Dropbox/UNICEF Work/Data and charts for websites/Files 2025/CME/Source data/wealthquintile-data-CMEinfo.xlsx")

# OfficialName lookup (authoritative country names; same source as 03)
cmeinfo_path <- file.path(dir_IGME, "2025 Round Estimation/Code/input/country.info.CME.csv")

DATAFLOW_ID <- "UNICEF:CME_RISK_FACTORS(1.0)"
EXPORT_DATAFLOW_ID <- "UNICEF_TEST:TEST_CME_RISK_FACTORS(1.0)"

abbrev_series_name <- function(nm) {
  m <- regmatches(nm, regexpr("\\([A-Z]{3,}\\)", nm))
  if (length(m) > 0 && nchar(m) > 0) return(gsub("[()]", "", m))
  stop_ws <- c("the","a","an","and","or","in","on","at","to","for","of","from",
               "by","with","sur","la","le","les","et","du","de","des","au","en",
               "par","el","y","los","las","del","da","e","do","na","di","il","lo")
  words <- strsplit(trimws(nm), "\\s+")[[1]]
  keep  <- nchar(words) > 2 & !tolower(words) %in% stop_ws
  abbr  <- paste0(toupper(substr(words[keep], 1, 1)), collapse = "")
  if (nchar(abbr) == 0) abbr <- toupper(substr(trimws(nm), 1, 6))
  abbr
}
abbrev_vec <- Vectorize(abbrev_series_name)

# ---- SDMX recode maps ----
indicator_code <- c(U5MR = "MRY0T4", NMR = "MRM0")

code_residence <- c(rural = "R", urban = "U")
code_wealth    <- c("1.quintile" = "Q1", "2.quintile" = "Q2", "3.quintile" = "Q3",
                    "4.quintile" = "Q4", "5.quintile" = "Q5")
code_edu       <- c(noeducation = "NONE", primary = "PRIM", secondary = "SEC",
                    higher = "HIGH", noedu.primary = "NONE_PRIM", sec.higher = "SEC_HIGH")
code_mage      <- c("<20" = "Y0T19", "20-29" = "Y20T29", "30-39" = "Y30T39",
                    "40-49" = "Y40T49")
code_binterval <- c("<2" = "INTERVAL_LT_M24", "2" = "INTERVAL_M24_35",
                    "3" = "INTERVAL_M36_47", "4+" = "INTERVAL_GT_M48",
                    first = "first")
code_border    <- c("1" = "ORD1", "2-3" = "ORD2_3", "4+" = "ORD4PLUS")

aggregate_ref_area_code <- c(
  residence = "AGG_RESIDENCE", wealth = "AGG_WEALTH", edu = "AGG_EDU",
  motherage = "AGG_MOTHERAGE", birthinterval = "AGG_BIRTHINTERVAL",
  birthorder = "AGG_BIRTHORDER"
)

# Map x through tbl, warning on anything unmapped
recode_dim <- function(x, tbl, dim_name) {
  out <- unname(tbl[x])
  bad <- is.na(out) & !is.na(x)
  if (any(bad))
    stop(sprintf("Unmapped %s group code(s): %s", dim_name,
                 paste(unique(x[bad]), collapse = ", ")))
  out
}


# ---- 1. Load ----

cat("Loading long CME data file...\n")
dt <- fread(in_path, encoding = "UTF-8")
if (!"Short_survey_name" %in% names(dt)) {
  dt[, Short_survey_name := ifelse(
    Series.Category == "Others",
    abbrev_vec(Series.Name),
    Series.Category
  )]
  cat("  Added Short_survey_name from Series.Category/Series.Name\n")
}
cat(sprintf("  %d rows | %d labels | indicators: %s\n",
            nrow(dt), uniqueN(dt$label), paste(sort(unique(dt$Indicator)), collapse = ", ")))

# OfficialName lookup for the rows sourced from the xlsx
cmeinfo <- fread(cmeinfo_path, encoding = "UTF-8",
                 select = c("ISO3Code", "OfficialName"))
officialname_lkp <- setNames(cmeinfo$OfficialName, cmeinfo$ISO3Code)


# ---- 2. Clear the wealth U5MR aggregate flag ----

n_wealth_u5mr <- dt[label == "wealth" & Indicator == "U5MR" & in_aggregate == TRUE, .N]
dt[label == "wealth" & Indicator == "U5MR", in_aggregate := FALSE]
cat(sprintf("\nStep 1: set U5MR wealth in_aggregate -> FALSE (%d rows were TRUE)\n",
            n_wealth_u5mr))


# ---- 3. Recompute in_aggregate for total rows ----
# TRUE where any non-total dimension of the same survey and indicator is TRUE

agg_keys <- unique(dt[label != "total" & in_aggregate == TRUE,
                      .(DataCatalogID, Indicator)])
agg_keys[, .is_agg := TRUE]

n_tot_before <- dt[label == "total" & in_aggregate == TRUE, .N]

dt <- merge(dt, agg_keys, by = c("DataCatalogID", "Indicator"), all.x = TRUE)
dt[label == "total", in_aggregate := !is.na(.is_agg)]
dt[, .is_agg := NULL]

n_tot_after <- dt[label == "total" & in_aggregate == TRUE, .N]
cat(sprintf("Step 2: total rows in_aggregate==TRUE  before: %d  after: %d\n",
            n_tot_before, n_tot_after))

# Drop the pipeline's wealth U5MR rows; section 5b replaces them from the xlsx
n_drop <- dt[label == "wealth" & Indicator == "U5MR", .N]
dt <- dt[!(label == "wealth" & Indicator == "U5MR")]
cat(sprintf("Step 3: dropped %d survey wealth U5MR rows (replaced from xlsx)\n", n_drop))


# ---- 4. Pivot into SDMX dimension columns ----

cat("\nBuilding wide dimension columns...\n")

# Start every dimension at "_T", then recode only the active rows
dt[, c("RESIDENCE", "WEALTH_QUINTILE", "EDUCATION",
       "MATERNAL_AGE", "BIRTH_INTERVAL", "BIRTH_ORDER") := "_T"]

dt[label == "residence",     RESIDENCE       := recode_dim(Group, code_residence, "residence")]
dt[label == "wealth",        WEALTH_QUINTILE := recode_dim(Group, code_wealth,    "wealth")]
dt[label == "edu",           EDUCATION       := recode_dim(Group, code_edu,       "edu")]
dt[label == "motherage",     MATERNAL_AGE    := recode_dim(Group, code_mage,      "motherage")]
dt[label == "birthinterval", BIRTH_INTERVAL  := recode_dim(Group, code_binterval, "birthinterval")]
dt[label == "birthorder",    BIRTH_ORDER     := recode_dim(Group, code_border,    "birthorder")]

dt[, INDICATOR := recode_dim(Indicator, indicator_code, "indicator")]


# ---- 5. Assemble the output ----

wide <- dt[, .(
  DATAFLOW        = DATAFLOW_ID,
  REF_AREA        = Country.ISO,
  # identifiers and series columns
  OfficialName,
  Short_survey_name,
  Series.Name,
  Series.Year,
  Series.Type,
  Series.plot.symbol,
  # SDMX core
  INDICATOR,
  SEX             = "T",
  RESIDENCE,
  WEALTH_QUINTILE,
  SERIES_TYPE     = Series.Category,
  EDUCATION,
  MATERNAL_AGE,
  BIRTH_INTERVAL,
  BIRTH_ORDER,
  TIME_PERIOD     = Reference.Date,
  OBS_VALUE       = Estimates,
  OBS_SE          = `Standard.Error.of.Estimates`,
  # trailing
  in_aggregate,
  DataCatalogID,
  n_countries     = NA_real_   # populated only on World aggregate rows (5d)
)]


# ---- 5b. Wealth U5MR rows from the xlsx ----
# Display only, so in_aggregate is FALSE; same columns as wide.

cat("\nBuilding wealth U5MR rows from xlsx...\n")
wq <- as.data.table(read.xlsx(wealth_u5mr_xlsx, sheet = 1))
cat(sprintf("  xlsx: %d rows | %d surveys | %d countries\n",
            nrow(wq), uniqueN(wq$DataCatalogID), uniqueN(wq$Country.ISO)))

wq[, WEALTH_QUINTILE := recode_dim(paste0(quintile, ".quintile"), code_wealth, "wealth")]

off <- officialname_lkp[wq$Country.ISO]
if (any(is.na(off)))
  warning(sprintf("xlsx ISO(s) with no OfficialName: %s",
                  paste(unique(wq$Country.ISO[is.na(off)]), collapse = ", ")))

wealth_wide <- data.table(
  DATAFLOW                  = DATAFLOW_ID,
  REF_AREA                  = wq$Country.ISO,
  OfficialName              = unname(off),
  Short_survey_name         = ifelse(wq$Series.Category == "Others",
                                     abbrev_vec(wq$Series.Name),
                                     wq$Series.Category),
  Series.Name               = wq$Series.Name,
  Series.Year               = wq$Series.Year,
  Series.Type               = wq$Series.Type,
  Series.plot.symbol        = wq$Series.plot.symbol,
  INDICATOR                 = indicator_code[["U5MR"]],
  SEX                       = "T",
  RESIDENCE                 = "_T",
  WEALTH_QUINTILE           = wq$WEALTH_QUINTILE,
  SERIES_TYPE               = wq$Series.Category,
  EDUCATION                 = "_T",
  MATERNAL_AGE              = "_T",
  BIRTH_INTERVAL            = "_T",
  BIRTH_ORDER               = "_T",
  TIME_PERIOD               = wq$Reference.Date,
  OBS_VALUE                 = wq$Estimates,
  OBS_SE                    = wq$`Standard.Error.of.Estimates`,
  in_aggregate              = FALSE,
  DataCatalogID             = wq$DataCatalogID,
  n_countries               = NA_real_
)

wide <- rbindlist(list(wide, wealth_wide), use.names = TRUE)
cat(sprintf("  Added %d wealth U5MR rows from xlsx\n", nrow(wealth_wide)))


# ---- 5c. Drop orphan totals ----
# Totals whose survey and indicator have no dimension row left after 5b.

dim_cols <- c("RESIDENCE", "WEALTH_QUINTILE", "EDUCATION",
              "MATERNAL_AGE", "BIRTH_INTERVAL", "BIRTH_ORDER")
wide[, .is_total := rowSums(as.matrix(.SD) != "_T") == 0, .SDcols = dim_cols]

has_dim <- unique(wide[.is_total == FALSE, .(DataCatalogID, INDICATOR)])
has_dim[, .has_dim := TRUE]
wide <- merge(wide, has_dim, by = c("DataCatalogID", "INDICATOR"), all.x = TRUE)

n_orphan <- wide[.is_total == TRUE & is.na(.has_dim), .N]
wide <- wide[!(.is_total == TRUE & is.na(.has_dim))]
wide[, c(".is_total", ".has_dim") := NULL]
cat(sprintf("\nDropped %d orphan total row(s) with no underlying dimension data\n",
            n_orphan))


# ---- 5d. World aggregate rows ----
# From UNIGME2025_risk_factor_aggregate_world.csv: one active dimension per row,
# AGG_* REF_AREA, n_countries in OBS_FOOTNOTE, and no series metadata.

cat("\nBuilding WORLD aggregate rows...\n")
agg <- fread(agg_world_path, encoding = "UTF-8")
cat(sprintf("  aggregate: %d rows | labels: %s | indicators: %s\n",
            nrow(agg), paste(sort(unique(agg$Label)), collapse = ", "),
            paste(sort(unique(agg$Indicator)), collapse = ", ")))

# The aggregate uses ">=4" for birth interval; the country map uses "4+"
agg[Label == "birthinterval" & Group == ">=4", Group := "4+"]

# Start at "_T", then recode the active dimension
agg[, c("RESIDENCE", "WEALTH_QUINTILE", "EDUCATION",
        "MATERNAL_AGE", "BIRTH_INTERVAL", "BIRTH_ORDER") := "_T"]
agg[Label == "residence",     RESIDENCE       := recode_dim(Group, code_residence, "residence")]
agg[Label == "wealth",        WEALTH_QUINTILE := recode_dim(Group, code_wealth,    "wealth")]
agg[Label == "edu",           EDUCATION       := recode_dim(Group, code_edu,       "edu")]
agg[Label == "motherage",     MATERNAL_AGE    := recode_dim(Group, code_mage,      "motherage")]
agg[Label == "birthinterval", BIRTH_INTERVAL  := recode_dim(Group, code_binterval, "birthinterval")]
agg[Label == "birthorder",    BIRTH_ORDER     := recode_dim(Group, code_border,    "birthorder")]

agg_wide <- data.table(
  DATAFLOW           = DATAFLOW_ID,
  REF_AREA           = recode_dim(agg$Label, aggregate_ref_area_code, "aggregate REF_AREA"),
  OfficialName       = NA_character_,
  Short_survey_name  = NA_character_,
  Series.Name        = NA_character_,
  Series.Year        = "2024",
  Series.Type        = NA_character_,
  Series.plot.symbol = NA_character_,
  INDICATOR          = recode_dim(agg$Indicator, indicator_code, "indicator"),
  SEX                = "T",
  RESIDENCE          = agg$RESIDENCE,
  WEALTH_QUINTILE    = agg$WEALTH_QUINTILE,
  SERIES_TYPE        = NA_character_,          # -> SERIES_CATEGORY after setnames
  EDUCATION          = agg$EDUCATION,
  MATERNAL_AGE       = agg$MATERNAL_AGE,
  BIRTH_INTERVAL     = agg$BIRTH_INTERVAL,
  BIRTH_ORDER        = agg$BIRTH_ORDER,
  TIME_PERIOD        = agg$Year,
  OBS_VALUE          = agg$Median,
  OBS_SE             = NA_real_,
  in_aggregate       = NA,                     # blank on aggregate rows
  DataCatalogID      = NA_character_,
  n_countries        = agg$n_countries
)

wide <- rbindlist(list(wide, agg_wide), use.names = TRUE)
cat(sprintf("  Added %d WORLD aggregate rows\n", nrow(agg_wide)))


# ---- 6. Checks and write ----

cat("\n── Sanity checks ──\n")
cat(sprintf("  Output rows: %d (%d non-wealth-U5MR + %d wealth-U5MR xlsx + %d WORLD aggregate)\n",
            nrow(wide), nrow(dt), nrow(wealth_wide), nrow(agg_wide)))

# Every row must have exactly one active (non-_T) dimension, except totals (zero)
dim_cols <- c("RESIDENCE", "WEALTH_QUINTILE", "EDUCATION",
              "MATERNAL_AGE", "BIRTH_INTERVAL", "BIRTH_ORDER")
n_active <- rowSums(as.matrix(wide[, ..dim_cols]) != "_T")
cat("  Active dimensions per row (0 = total row):\n")
print(table(n_active))
if (any(n_active > 1))
  stop("Some rows have >1 active dimension — pivot logic error.")

cat("  Active group code by dimension column:\n")
for (cl in dim_cols) {
  vals <- sort(unique(wide[[cl]][wide[[cl]] != "_T"]))
  cat(sprintf("    %-15s %s\n", cl, paste(vals, collapse = ", ")))
}

cat("\n-- Final CME DSD recoding --\n")

convert_time_period <- function(x) {
  x_chr <- as.character(x)
  out <- x_chr
  num <- suppressWarnings(as.numeric(x_chr))
  ok <- !is.na(num)

  if (any(ok)) {
    year <- floor(num[ok])
    month <- floor((num[ok] - year) * 12) + 1L
    rollover <- month > 12L
    year[rollover] <- year[rollover] + 1L
    month[rollover] <- 1L
    out[ok] <- sprintf("%04d-%02d", year, month)
  }

  out
}

setnames(
  wide,
  old = c("SERIES_TYPE", "Series.Year", "Series.Name", "Short_survey_name",
          "Series.Type", "Series.plot.symbol", "in_aggregate",
          "EDUCATION", "MATERNAL_AGE", "OBS_SE"),
  new = c("SERIES_CATEGORY", "SERIES_YEAR", "SERIES_NAME", "SHORT_SURVEY_NAME",
          "SERIES_TYPE", "SERIES_METHOD", "IN_AGGREGATE",
          "EDUCATION_LEVEL", "MATERNAL_AGE_AT_BIRTH", "STD_ERR")
)

# Treat non-finite standard errors and the 9999 missing-value sentinel as NA.
std_err_values <- suppressWarnings(as.numeric(wide[["STD_ERR"]]))
invalid_se_rows <- which(
  !is.na(std_err_values) &
    (!is.finite(std_err_values) | std_err_values == 9999)
)
n_invalid_se <- length(invalid_se_rows)
set(wide, i = invalid_se_rows, j = "STD_ERR", value = NA_character_)
rm(std_err_values, invalid_se_rows)
cat(sprintf("  Replaced %d invalid STD_ERR value(s) with NA\n", n_invalid_se))

wide[, DATAFLOW := EXPORT_DATAFLOW_ID]
wide[, TIME_PERIOD := convert_time_period(TIME_PERIOD)]
wide[, SERIES_YEAR := gsub("-", "_", SERIES_YEAR, fixed = TRUE)]
wide[, REF_AREA := fcase(
  REF_AREA == "RKS", "XKX",
  REF_AREA == "World", "WORLD",
  default = REF_AREA
)]
wide[, SEX := fifelse(SEX == "T", "_T", SEX)]
wide[, INDICATOR := fcase(
  INDICATOR == "MRM0", "CME_MRM0",
  INDICATOR == "MRY0T4", "CME_MRY0T4",
  default = INDICATOR
)]
wide[, OBS_FOOTNOTE := fifelse(
  !is.na(n_countries),
  sprintf("WORLD refers to %d countries with data.", as.integer(n_countries)),
  NA_character_
)]

education_level_code <- c(
  HIGH = "AGG_5T8",
  NONE = "ISCED11A_01",
  NONE_PRIM = "AGG_0_1",
  PRIM = "ISCED11_1",
  SEC = "AGG_2_3",
  SEC_HIGH = "AGG_3S_H"
)
wide[
  EDUCATION_LEVEL %in% names(education_level_code),
  EDUCATION_LEVEL := unname(education_level_code[EDUCATION_LEVEL])
]

dims <- c(
  "REF_AREA", "INDICATOR", "SEX", "RESIDENCE", "WEALTH_QUINTILE",
  "EDUCATION_LEVEL", "MATERNAL_AGE_AT_BIRTH", "BIRTH_INTERVAL",
  "BIRTH_ORDER", "SERIES_YEAR", "TIME_PERIOD"
)
attribs <- c(
  "SERIES_CATEGORY", "SERIES_NAME", "SHORT_SURVEY_NAME", "SERIES_TYPE",
  "SERIES_METHOD", "IN_AGGREGATE", "OBS_FOOTNOTE"
)
data_structure_definition <- c("DATAFLOW", dims, "OBS_VALUE", "STD_ERR", attribs)

missing_cols <- setdiff(data_structure_definition, names(wide))
if (length(missing_cols) > 0)
  stop(sprintf("Missing final output column(s): %s",
               paste(missing_cols, collapse = ", ")))

# Drop the agreed exact values only, once all inputs are combined
excluded_values <- c(500, 1000, 9999)
excluded_rows <- wide[OBS_VALUE %in% excluded_values]
wide <- wide[!(OBS_VALUE %in% excluded_values)]
excluded_path <- paste0(tools::file_path_sans_ext(out_path), "-excluded-exact-values.csv")
fwrite(excluded_rows, excluded_path, bom = TRUE)
cat(sprintf("  Excluded %d final rows with OBS_VALUE exactly 500, 1000 or 9999; audit: %s\n",
            nrow(excluded_rows), excluded_path))
stopifnot(!any(wide$OBS_VALUE %in% excluded_values))

dup_dims <- duplicated(wide[, ..dims])
if (any(dup_dims))
  stop(sprintf("The dims subset contains duplicated rows (%d duplicate rows).",
               sum(dup_dims)))

setorderv(wide, dims)
wide <- wide[, ..data_structure_definition]
cat(sprintf("  Final schema: %d columns | duplicate dim rows: 0\n", ncol(wide)))

# bom = TRUE so Excel detects UTF-8; otherwise "Turkiye" renders as mojibake
fwrite(wide, out_path, bom = TRUE)
cat(sprintf("\n  Saved: %s\n         (%d rows x %d cols)\n",
            out_path, nrow(wide), ncol(wide)))

cat("\n── 05_make_cme_wide_format complete ──\n")
