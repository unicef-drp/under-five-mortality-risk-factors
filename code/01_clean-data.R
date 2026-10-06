# Clean the raw equity database into the country-specific files used downstream.
#
# Outputs, in output/country/:
#   country-specific-database-cleaned_<date>.csv              survey rates, survey wealth U5MR
#   country-specific-nbirth_<date>.csv                        birth counts, for weighting in 02
#   country-specific-database-cleaned-igme-wealth_<date>.csv  wealth U5MR from the IGME model
#   db-pre-aggregation_<date>.csv                             rates before MR1t59 and nbirth rows
#   db-all-surveys_<date>.csv                                 all qualifying surveys, before the most-recent step


# ---- 0. Libraries and paths ----

library(dplyr)
library(tidyr)
library(data.table)
library(readxl)

USERPROFILE <- Sys.getenv("USERPROFILE")

# profile.R sets dir_IGME
source(file.path(USERPROFILE, "Dropbox/UNICEF Work/profile.R"))
source("code/run-config.R")
source("code/lib/equity-common.R")
source("code/lib/cme-validation.R")
source("code/lib/cme-education.R")
cfg <- equity_run_config()

# Inputs (Dropbox and SharePoint)
db_dir      <- file.path(USERPROFILE, "Dropbox/UNICEF Work/Equity database/Databases/Databases 2025")
cmeinfo_path <- file.path(dir_IGME, "2025 Round Estimation/Code/input/country.info.CME.csv")
lb_path      <- file.path(dir_IGME, "2025 Round Estimation/Code/input/data_livebirths.csv")
wealth_path  <- file.path(USERPROFILE, "Dropbox/IGME Wealth/output/M17/country-specific")

# Outputs, relative to the project root
output_dir  <- cfg$output_dir
dir_country <- file.path(output_dir, "country")
for (d in c(output_dir, dir_country)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

RUN_DATE <- cfg$run_date

# Data vintage, from run-config.R
DB_DATE_PATTERN  <- cfg$db_date_pattern
DB_CUTOFF_DATE   <- cfg$db_cutoff_date  # surveys added after this are dropped, except edu
LB_REF_YEAR      <- cfg$lb_ref_year


# ---- 1. Load raw data ----

# Reads data_<dimension>_<date>.csv only, so diagnostic files in the same
# folder are skipped. read_equity_db is shared with 03 via lib/equity-common.R.

db_all   <- read_equity_db(db_dir, DB_DATE_PATTERN)
cmeinfo  <- fread(cmeinfo_path)
lb_wpp   <- read.csv(lb_path, as.is = TRUE) %>% filter(year == LB_REF_YEAR)

message("Raw equity database: ", nrow(db_all), " rows, ",
        uniqueN(db_all$label), " equity dimensions")

# Expect 7 equity dimensions
stopifnot(uniqueN(db_all$label) == 7)

# Strip the trailing underscore from labels such as "4+_"
db_all <- normalize_equity_groups(db_all)

# normalize_equity_groups() also recodes birth order, birth interval and
# mother's age to their canonical labels.

# "first" is not part of the <2 / 2 / 3 / 4+ set
db_all <- db_all %>% filter(!(label == "birthinterval" & Group == "first"))

message("Distinct Group values after standardisation: ", uniqueN(db_all$Group))



# ---- 3. Initial filters ----

# Drop surveys added after the cut-off. Education is exempt: it lags the other
# dimensions and is always kept current.
db_all <- db_all %>%
  mutate(date_added = as.Date(paste(added, "01", sep = "-"), format = "%Y-%m-%d")) %>%
  filter(date_added < DB_CUTOFF_DATE | label == "edu")

message("After date cut-off filter: ", nrow(db_all), " rows")

# Mortality indicators and births; nbirth rows are handled in section 14
db <- db_all[Indicator %in% c("NMR", "U5MR", "nbirth")]

# Mortality rows: drop missing estimates and non-visible points. The exceptions
# were Visible == 2 when the published figures were produced and have since been
# set lower: Lesotho 2023-2024 DHS, Peru 2022 and 2023 ENDES, and the 2023 round
# of the India Sample Registration System used for residence.
visible_exceptions <- c("TBALSO2023", "9272", "9273", "1865")

db <- db %>%
  filter(Indicator != "nbirth" & !is.na(Estimates) &
           (Visible == 2 | DataCatalogID %in% visible_exceptions))

# Exclude high-income countries (GUY, TTO, TCA, OMN, ROU at the 2025 vintage)
db <- db %>%
  left_join(cmeinfo[, .(ISO3Code, WBRegion4)], by = c("Country.Code" = "ISO3Code"))

hic_codes <- unique(db$Country.Code[!is.na(db$WBRegion4) & db$WBRegion4 == "High income"])
message("High-income countries excluded: ", paste(sort(hic_codes), collapse = ", "))
db <- filter(db, !Country.Code %in% hic_codes)

# Mid-point for year ranges ("2019-2021" becomes 2020), then surveys since 2013
db <- db %>%
  mutate(
    Average.Date.Of.Survey = {
      yr <- Series.Year
      is_range <- nchar(yr) > 4
      avg <- numeric(length(yr))
      avg[!is_range] <- as.numeric(yr[!is_range])
      avg[is_range]  <- (as.numeric(substr(yr[is_range], 1, 4)) +
                         as.numeric(substr(yr[is_range], 6, 9))) / 2
      avg
    }
  ) %>%
  filter(Average.Date.Of.Survey >= 2013)

message("After >=2013 filter: ", nrow(db), " rows") # 52062 rows

# Keep surveys in the IGME U5MR database that have a recall interval;
# indirect surveys have none.
db <- db %>%
  filter(Inclusion.U5MR == 1 & (!is.na(Interval) | Series.Type == "Indirect"))


# ---- 4. Indirect surveys ----

# Indirect surveys give one estimate per age window. Keep age of women 25-29
# and time since first birth 5-9.

db[, Age.Group.of.Women.or.Time.Since.First.Birth :=
     sub("_$", "", Age.Group.of.Women.or.Time.Since.First.Birth)]

indirect_selected <- db %>%
  filter(
    Series.Type == "Indirect" & (
      (Indirect.Classification == "AOW"  & Age.Group.of.Women.or.Time.Since.First.Birth == "25-29") |
      (Indirect.Classification == "TSFB" & Age.Group.of.Women.or.Time.Since.First.Birth == "5-9")
    )
  )

message("Indirect surveys selected: ", uniqueN(indirect_selected$IGME_Key), " unique surveys")

db <- filter(db, Series.Type != "Indirect") %>%
  bind_rows(indirect_selected)

# Age windows can carry different reference dates; use the first for the survey
indirect_unified <- db %>%
  filter(Series.Type == "Indirect") %>%
  group_by(label, DataCatalogID) %>%
  mutate(Reference.Date = first(Reference.Date)) %>%
  ungroup()

db <- filter(db, Series.Type != "Indirect") %>%
  bind_rows(indirect_unified)

stopifnot(db[is.na(Reference.Date), .N] == 0)

message("After indirect selection: ", nrow(db), " rows")


# ---- 5. Education: 2 vs 4 categories ----
# Surveys can report four categories (noeducation / primary / secondary / higher)
# and the combined pair (noedu.primary / sec.higher). Keep one representation,
# then the most recent reference date per survey and indicator.

db_edu     <- db %>% filter(label == "edu")
db_not_edu <- db %>% filter(label != "edu")

# Drop the 2-group rows where 4-group rows exist
db_edu_processed <- db_edu %>%
  group_by(DataCatalogID, Indicator, Reference.Date) %>%
  mutate(flag = if (all(c("noeducation", "primary", "secondary", "higher") %in% Group))
           Group %in% c("noedu.primary", "sec.higher")
         else
           FALSE) %>%
  filter(!flag) %>%
  select(-flag) %>%
  ungroup()

# Drop the 4-group rows where only 2-group rows exist
db_edu_processed <- db_edu_processed %>%
  group_by(DataCatalogID, Indicator, Reference.Date) %>%
  mutate(flag = if (all(c("noedu.primary", "sec.higher") %in% Group))
           Group %in% c("noeducation", "primary", "secondary", "higher")
         else
           FALSE) %>%
  filter(!flag) %>%
  select(-flag) %>%
  ungroup()

# Most recent reference date per survey and indicator
db_edu_processed <- db_edu_processed %>%
  group_by(DataCatalogID, Indicator) %>%
  filter(Reference.Date == max(Reference.Date)) %>%
  ungroup()

# Require a complete set: all four groups, both combined groups, or the hybrid
# noeducation + primary + sec.higher.
db_edu_processed <- db_edu_processed %>%
  group_by(DataCatalogID, Indicator) %>%
  filter(
    all(c("noeducation", "primary", "secondary", "higher") %in% Group) |
    all(c("noedu.primary", "sec.higher") %in% Group) |
    # Hybrid (e.g. VNM 5580): noedu.primary is derived in section 16
    (all(c("noeducation", "primary", "sec.higher") %in% Group) &
     !any(c("secondary", "higher", "noedu.primary") %in% Group))
  ) %>%
  ungroup()


# ---- 6. Other dimensions: require all groups ----
# A survey is kept only if every canonical group is present: wealth quintiles 1-5,
# residence rural / urban, bsize small.vsmall / av.large, birth order 1 / 2-3 / 4+,
# birth interval <2 / 2 / 3 / 4+, mother's age 20-29 / 30-39 / 40-49.

stopifnot(uniqueN(db_not_edu$label) == 6)

expected_groups_for <- function(lbl) {
  switch(lbl,
    wealth        = c("1.quintile", "2.quintile", "3.quintile", "4.quintile", "5.quintile"),
    residence     = c("rural", "urban"),
    bsize         = c("small.vsmall", "av.large"),
    birthorder    = c("1", "2-3", "4+"),
    birthinterval = c("<2", "2", "3", "4+"),
    motherage     = c("20-29", "30-39", "40-49"),
    NULL
  )
}

db_not_edu_processed <- db_not_edu %>%
  rowwise() %>%
  mutate(expected = list(expected_groups_for(label))) %>%
  ungroup() %>%
  # bsize and motherage are checked at survey-year level below: their groups can
  # legitimately differ in reference date or interval.
  group_by(DataCatalogID, Indicator, label, Series.Year, Reference.Date, Interval) %>%
  filter(label %in% c("bsize", "motherage") | all(expected[[1]] %in% Group)) %>%
  ungroup() %>%
  # bsize and motherage
  group_by(DataCatalogID, Indicator, label, Series.Year) %>%
  filter(!label %in% c("bsize", "motherage") | all(expected[[1]] %in% Group)) %>%
  ungroup() %>%
  select(-expected)

# bsize: the total row often uses a 10-year window while the sub-groups use a
# shorter one. Align the total to the small.vsmall interval.
db_not_edu_processed <- db_not_edu_processed %>%
  group_by(DataCatalogID, Indicator, label, Series.Year) %>%
  mutate(
    # base ifelse() gives NA, not numeric(0), when no small.vsmall row exists
    target_interval = ifelse(
      label == "bsize" & any(Group == "small.vsmall"),
      Interval[Group == "small.vsmall"][1],
      NA_real_
    ),
    n_total = sum(Group == "total")
  ) %>%
  filter(
    label != "bsize" |
    Group != "total" |
    n_total == 1 |
    Interval == target_interval
  ) %>%
  ungroup() %>%
  select(-target_interval, -n_total)


# ---- 7. Combine, drop total-only surveys, manual exclusions ----

db_all_group <- bind_rows(db_edu_processed, db_not_edu_processed)
message("After group completeness filter: ", nrow(db_all_group), " rows")

# Drop surveys with no breakdown; bsize is grouped differently
n_countries_before <- uniqueN(db_all_group$Country.Name)

db_all_group <- db_all_group %>%
  group_by(DataCatalogID, label, Series.Year, Indicator) %>%
  filter(if_else(label == "bsize", !all(Group == "total"), TRUE)) %>%
  ungroup() %>%
  group_by(DataCatalogID, label, Series.Year, Indicator, Reference.Date, Interval) %>%
  filter(if_else(label != "bsize", !all(Group == "total"), TRUE)) %>%
  ungroup()

n_countries_after <- uniqueN(db_all_group$Country.Name)
if (n_countries_before != n_countries_after)
  message("Countries lost after total-only removal: ",
          paste(setdiff(unique(db_all_group$Country.Name[seq_len(n_countries_before)]),
                        unique(db_all_group$Country.Name)), collapse = ", "))

# Use the newer direct survey instead of the indirect one
surveys_to_exclude <- c(
  "EGY-2015-Health Indicator Survey-Indirect",      # EGY: newer direct survey available
  "TGO-2017-Malaria Indicator Survey-Indirect",
  "UGA-2018-2019-Malaria Indicator Survey-Indirect",
  "CMR-2022-Malaria Indicator Survey-Indirect",
  "GHA-2019-Malaria Indicator Survey-Indirect",     # GHA: newer direct survey available
  "GIN-2021-Malaria Indicator Survey-Indirect",
  "MLI-2021-Malaria Indicator Survey-Indirect",     # MLI: keep -Direct, drop -Indirect
  "SEN-2020-2021-Malaria Indicator Survey-Indirect",# SEN: keep -Direct, drop -Indirect
  "NER-2021-Malaria Indicator Survey-Indirect"
)
db_all_group <- filter(db_all_group, !IGME_Key %in% surveys_to_exclude)

# Data-quality exclusions: too few cases, or highest/lowest ratio above 3
db_all_group <- db_all_group %>%
  filter(
    !(label == "edu" & Indicator == "U5MR" & Country.Code %in% c("BFA","KAZ","KGZ","NER","SLE","UZB")),
    !(label == "edu" & Indicator == "NMR"  & Country.Code %in% c("KGZ","NER","SLE","UZB")),
    !(label == "birthorder"               & Country.Code == "BOL")
  )

# Drop countries with fewer than 10,000 live births (WPP 2024)
lb_flag <- lb_wpp %>%
  left_join(cmeinfo[, .(ISO3Code, UNCode)], by = c("uncode" = "UNCode")) %>%
  mutate(is_small_island = lb < 10000)

small_islands <- lb_flag$ISO3Code[lb_flag$is_small_island]
message("Small island countries removed: ", paste(sort(small_islands), collapse = ", "))

db_all_group <- db_all_group %>%
  left_join(lb_flag[, c("ISO3Code", "is_small_island")], by = c("Country.Code" = "ISO3Code")) %>%
  filter(!is_small_island) %>%
  select(-is_small_island)

# Save every qualifying survey wave, before the most-recent selection.
# Used by 04 and by Table 1 of the report.
fwrite(db_all_group,
       file.path(dir_country, paste0("db-all-surveys_", RUN_DATE, ".csv")))
message("Written: output/country/db-all-surveys_", RUN_DATE, ".csv  (",
        nrow(db_all_group), " rows)")


# ---- 8. Most recent survey per country and dimension ----
# Where one survey has several recall intervals, keep the shortest.

db_all_group <- db_all_group %>%
  group_by(Country.Code, label) %>%
  slice_max(Average.Date.Of.Survey, with_ties = TRUE) %>%
  ungroup() %>%
  group_by(label, Country.Code, DataCatalogID, Series.Year, Indicator, Group) %>%
  slice_min(Interval, with_ties = TRUE) %>%
  ungroup()

message("After survey selection: ", nrow(db_all_group), " rows  |  ",
        uniqueN(db_all_group$Country.Code), " countries")


# ---- 9. One canonical total per survey and indicator ----
# Each dimension carries its own "total" row. They are the same national estimate
# but can differ slightly, so take the modal value and label it "total".

totals_raw <- db_all_group %>% filter(Group == "total")

n_differing <- totals_raw %>%
  group_by(DataCatalogID, Indicator) %>%
  summarise(n_vals = n_distinct(round(as.numeric(Estimates), 8)), .groups = "drop") %>%
  filter(n_vals > 1) %>%
  nrow()
message("DataCatalogID × Indicator combos with differing totals: ", n_differing)

canonical_totals <- totals_raw %>%
  group_by(DataCatalogID, Indicator) %>%
  mutate(
    Estimates_r = round(as.numeric(Estimates), 8),
    freq        = sapply(Estimates_r, function(v) sum(Estimates_r == v, na.rm = TRUE))
  ) %>%
  filter(freq == max(freq)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-Estimates_r, -freq) %>%
  mutate(label = "total")

message("Canonical totals: ", nrow(canonical_totals),
        " rows (one per DataCatalogID × Indicator)")

# Keep the per-dimension totals as ratio denominators for 02; the canonical rows
# are appended alongside them.
db_all_group <- bind_rows(db_all_group, canonical_totals)


# ---- 10. India DataCatalogID ----
# India surveys all take SRS id 1865, so 1865 covers the SRS for residence and
# NFHS for the other dimensions.

db_all_group <- db_all_group %>%
  mutate(DataCatalogID = if_else(Country.Code == "IND", "1865", DataCatalogID))


# ---- 11. Snapshot before aggregation ----
# Rates only, with survey wealth U5MR. Used by the maps and source tables.

pre_agg_path <- file.path(dir_country,
  paste0("db-pre-aggregation_", RUN_DATE, ".csv"))
fwrite(db_all_group, pre_agg_path)
message("Written: ", pre_agg_path)


# ---- 12. Latest reference date per survey and group ----
# Grouped by Group, so each category picks its own latest date.

db_all_group <- db_all_group %>%
  group_by(label, Country.Code, DataCatalogID, Series.Year, Indicator, Group, Interval) %>%
  mutate(
    Reference.Date  = as.numeric(Reference.Date),
    max_ref_date    = if (all(is.na(Reference.Date))) NA_real_
                      else max(Reference.Date, na.rm = TRUE),
    all_na_ref      = all(is.na(Reference.Date))
  ) %>%
  filter(all_na_ref | is.na(Reference.Date) | Reference.Date == max_ref_date) %>%
  select(-max_ref_date, -all_na_ref) %>%
  ungroup() %>%
  filter(Estimates != 0)   # remove rows with zero mortality rate


# ---- 13. MR1t59 (1-59 month mortality) ----
# 1000 * (1 - (1 - U5MR/1000) / (1 - NMR/1000)), where both rates exist.
# Metadata comes from the NMR row.

MR1t59 <- db_all_group %>%
  filter(Group != "total", Indicator %in% c("U5MR", "NMR")) %>%
  select(DataCatalogID, label, Group, Series.Year, Reference.Date, Interval, Indicator, Estimates) %>%
  pivot_wider(names_from = Indicator, values_from = Estimates) %>%
  filter(!is.na(U5MR), !is.na(NMR)) %>%
  mutate(
    Indicator = "MR1t59",
    Estimates = 1000 * (1 - (1 - U5MR / 1000) / (1 - NMR / 1000))
  ) %>%
  select(DataCatalogID, label, Group, Series.Year, Reference.Date, Interval, Indicator, Estimates) %>%
  # Join metadata from NMR rows
  left_join(
    db_all_group %>% filter(Indicator == "NMR"),
    by = c("DataCatalogID", "label", "Group", "Series.Year", "Reference.Date", "Interval")
  ) %>%
  select(-Indicator.y, -Estimates.y) %>%
  rename(Indicator = Indicator.x, Estimates = Estimates.x) %>%
  select(all_of(names(db_all_group)))

db_all_group <- bind_rows(db_all_group, MR1t59)

message("Country × dimension coverage (n countries):")
print(setDT(db_all_group)[, uniqueN(Country.Code), by = .(label, Indicator)][order(label, Indicator)])


# ---- 14. Birth counts ----
# Needed for birth-weighted aggregates, keyed to the same survey, dimension and group.

# Wealth: surveys without microdata report a total only, so split it equally
# across the quintiles.
nbirth_wealth_nonmicro <- db_all[
  Indicator == "nbirth" &
    !Source.1 %in% c("Computed from microdata", "DHS output", "MICS output") &
    label == "wealth"
] %>%
  group_by(Country.Code, Series.Name, Series.Year, Series.Type,
           Series.Category, Interval, DataCatalogID, IGME_Key) %>%
  mutate(Estimates = if_else(Group == "total", Estimates, Estimates[Group == "total"] / 5)) %>%
  ungroup()

# Replace those rows in the full raw dataset
db_all_births <- db_all[
  !(Indicator == "nbirth" &
      !Source.1 %in% c("Computed from microdata", "DHS output", "MICS output") &
      label == "wealth")
] %>%
  bind_rows(nbirth_wealth_nonmicro)

# 13b. Extract and clean nbirth rows
nbirth <- db_all_births %>%
  filter(Indicator == "nbirth") %>%
  # Sum counts within group × interval (some surveys have duplicate raw rows)
  group_by(label, DataCatalogID, Group, Interval) %>%
  mutate(Estimates = sum(Estimates)) %>%
  ungroup() %>%
  distinct_all() %>%
  # Keep only Visible == 2 rows; if none exist, keep all (some indirect surveys)
  group_by(label, DataCatalogID, Group) %>%
  mutate(has_vis2 = any(Visible == 2)) %>%
  filter(Visible == 2 | !has_vis2) %>%
  ungroup() %>%
  select(-has_vis2)

# 13c. Birth size: combine "small" and "vsmall" into "small.vsmall"
nbirth_bsize <- nbirth %>%
  filter(label == "bsize") %>%
  mutate(Group = if_else(Group %in% c("small", "vsmall"), "small.vsmall", Group)) %>%
  group_by(DataCatalogID, Country.ISO, label, Group, Interval) %>%
  mutate(Estimates = sum(Estimates)) %>%
  ungroup() %>%
  distinct_all()

nbirth <- bind_rows(
  filter(nbirth, label != "bsize"),
  nbirth_bsize
) %>%
  group_by(DataCatalogID, Country.ISO, label, Interval) %>%
  filter(!(2 %in% Visible) | Visible == 2) %>%
  ungroup()

# Interval: longest for bsize (report windows), shortest elsewhere
nbirth <- nbirth %>%
  group_by(label, DataCatalogID, Series.Year, Group) %>%
  filter(
    is.na(Interval) |
    (label == "bsize" & Interval == max(Interval, na.rm = TRUE)) |
    (label != "bsize" & Interval == min(Interval, na.rm = TRUE))
  ) %>%
  ungroup()

# 13e. Restrict nbirth to DataCatalogID × label × Group combinations that
#      exist in the rate data (inner join — drops birth data for unused surveys)
nbirth <- nbirth %>%
  inner_join(
    db_all_group %>% distinct(DataCatalogID, label, Group),
    by = c("DataCatalogID", "label", "Group")
  )

# Group completeness for nbirth is enforced in 02. A survey with partial birth
# counts stays here so its rate ratios still feed the country-level analysis.

# Check for surveys with no birth count (expected for indirect surveys)
surveys_no_nbirth <- setdiff(
  unique(db_all_group$IGME_Key),
  unique(db_all_births$IGME_Key[db_all_births$Indicator == "nbirth"])
)
message("Surveys in rate data with no nbirth (expected for indirect): ",
        length(surveys_no_nbirth))

# Drop survey and dimension combinations with no birth counts. Canonical totals
# stay for any survey that has counts for at least one dimension.
surveys_with_births <- nbirth %>% distinct(DataCatalogID, label)
dcids_with_births   <- surveys_with_births %>% distinct(DataCatalogID)

n_combos_dropped <- db_all_group %>%
  filter(label != "total") %>%
  anti_join(surveys_with_births, by = c("DataCatalogID", "label")) %>%
  distinct(DataCatalogID, label) %>%
  nrow()
if (n_combos_dropped > 0)
  message("Dropping ", n_combos_dropped,
          " DataCatalogID x label combo(s) with no birth count data")

db_all_group <- bind_rows(
  db_all_group %>%
    filter(label != "total") %>%
    semi_join(surveys_with_births, by = c("DataCatalogID", "label")),
  db_all_group %>%
    filter(label == "total") %>%
    semi_join(dcids_with_births, by = "DataCatalogID")
)

# 13f. Append nbirth rows to the main dataset
db_all_group <- bind_rows(db_all_group, nbirth)


# ---- 15. IGME model wealth U5MR ----
# Modelled U5MR by quintile for 106 low- and middle-income countries, written to
# the igme-wealth variant in section 18.

quintile_files <- c(
  "1.quintile" = "Estimated U5MR 1st wealth quintile group_country.csv",
  "2.quintile" = "Estimated U5MR 2nd wealth quintile group_country.csv",
  "3.quintile" = "Estimated U5MR 3rd wealth quintile group_country.csv",
  "4.quintile" = "Estimated U5MR 4th wealth quintile group_country.csv",
  "5.quintile" = "Estimated U5MR 5th wealth quintile group_country.csv"
)

igme_wealth_list <- lapply(names(quintile_files), function(q) {
  read.csv(file.path(wealth_path, quintile_files[q]), stringsAsFactors = FALSE) %>%
    filter(Quantile == "Median") %>%
    select(Country.Name, ISO.Code, X2024) %>%
    mutate(Group = q)
})

igme_wealth_U5MR <- bind_rows(igme_wealth_list) %>%
  left_join(cmeinfo[, .(ISO3Code, Code)], by = c("ISO.Code" = "ISO3Code")) %>%
  rename(Estimates = X2024, Country.ISO = Code,
         Country.Code = ISO.Code) %>%
  mutate(
    label       = "wealth",
    Indicator   = "U5MR",
    Series.Name = "UN IGME",
    Series.Year = "2024",
    Reference.Date = 2024
  )


# ---- 16. Education two-group values ----
# Four-group surveys use their supplied pairs; only hybrid surveys are derived.
# 02 drops the companion two-group rows to avoid double weighting.

nbirth_edu <- nbirth %>%
  left_join(lb_wpp[, c("uncode", "lb")], by = c("Country.ISO" = "uncode")) %>%
  distinct(across(-c(Estimates, added)), .keep_all = TRUE) %>%
  filter(label == "edu") %>%
  group_by(DataCatalogID, Interval) %>%
  mutate(
    prop_birth = if_else(Group == "total", 1,
                         as.numeric(Estimates) / sum(Estimates[Group != "total"])),
    wgt_birth  = prop_birth * lb
  ) %>%
  ungroup()

# Identify hybrid-style surveys FIRST so they can be excluded from the
# 4-group derivation below (VNM 5580: noeducation + primary + sec.higher).
vnm_style_ids <- db_all_group %>%
  filter(label == "edu") %>%
  group_by(DataCatalogID, Indicator) %>%
  filter(all(c("noeducation","primary","sec.higher") %in% Group) &
         !any(c("secondary","higher","noedu.primary") %in% Group)) %>%
  pull(DataCatalogID) %>%
  unique()

edu_2group <- lookup_source_education_pairs(
  db_all_group %>%
    filter(label == "edu", Indicator %in% c("NMR", "U5MR"),
           Group %in% c("noeducation", "primary", "secondary", "higher"),
           !DataCatalogID %in% vnm_style_ids),
  db_edu
)
message("Added supplied two-group education rates (plus MR1t59): ", nrow(edu_2group))

# Hybrid-style surveys: derive noedu.primary (rates) by birth-weighting
# noeducation + primary.  sec.higher is kept as-is.
edu_2group_vnm <- db_all_group %>%
  filter(DataCatalogID %in% vnm_style_ids,
         label == "edu", Indicator != "nbirth",
         Group %in% c("noeducation","primary")) %>%
  left_join(nbirth_edu[, c("label","DataCatalogID","Group","wgt_birth")],
            by = c("label","DataCatalogID","Group")) %>%
  mutate(wgt_death = Estimates * wgt_birth,
         Group     = "noedu.primary") %>%
  group_by(DataCatalogID, Indicator, Group) %>%
  mutate(Estimates                   = sum(wgt_death) / sum(wgt_birth),
         Standard.Error.of.Estimates = NA_real_) %>%
  ungroup() %>%
  select(-wgt_birth, -wgt_death) %>%
  distinct_all() %>%
  mutate(category = "edu 2 groups (derived from components)")

# Hybrid-style surveys: derive noedu.primary BIRTH COUNT by summing
# noeducation + primary nbirth rows.  Required for script 02's nbirth join.
nbirth_vnm_2group <- db_all_group %>%
  filter(DataCatalogID %in% vnm_style_ids,
         label == "edu", Indicator == "nbirth",
         Group %in% c("noeducation","primary")) %>%
  group_by(across(-c(Group, Estimates, Standard.Error.of.Estimates))) %>%
  summarise(
    Estimates                   = sum(as.numeric(Estimates), na.rm = TRUE),
    Standard.Error.of.Estimates = NA_real_,
    .groups = "drop"
  ) %>%
  mutate(Group = "noedu.primary")

# Tag education rows; category must exist before case_when's fallback
if (!"category" %in% names(db_all_group))
  db_all_group$category <- NA_character_

db_all_group <- db_all_group %>%
  # Hybrid surveys: the raw rows are replaced by the derived noedu.primary
  filter(!(DataCatalogID %in% vnm_style_ids &
           label == "edu" &
           Group %in% c("noeducation","primary"))) %>%
  mutate(category = case_when(
    label == "edu" & Group %in% c("noeducation","primary","secondary","higher") ~ "edu 4 groups",
    label == "edu" & Group %in% c("noedu.primary","sec.higher")                 ~ "edu 2 groups",
    TRUE ~ category
  )) %>%
  bind_rows(edu_2group) %>%
  bind_rows(edu_2group_vnm) %>%
  bind_rows(nbirth_vnm_2group)

# Patch the all-surveys snapshot with the same derived rows
if (length(vnm_style_ids) > 0) {
  snap_path <- file.path(dir_country, paste0("db-all-surveys_", RUN_DATE, ".csv"))
  snap <- fread(snap_path, data.table = FALSE) %>%
    mutate(DataCatalogID = as.character(DataCatalogID),
           date_added    = as.character(date_added))
  snap_cols <- names(snap)   # snapshot has no 'category' column yet
  snap <- snap %>%
    filter(!(DataCatalogID %in% vnm_style_ids &
             label == "edu" &
             Group %in% c("noeducation", "primary"))) %>%
    bind_rows(
      edu_2group_vnm %>%
        mutate(across(where(lubridate::is.Date), as.character)) %>%
        select(any_of(snap_cols))
    )
  fwrite(snap, snap_path)
  message("Patched all-surveys snapshot: replaced noeducation/primary with ",
          "noedu.primary for VNM-style surveys (", length(vnm_style_ids),
          " survey IDs)")
}


# ---- 17. Mother's age exclusions ----
# Small samples in the <20 and 40+ groups.

motherage_exclusions <- c(
  "DOM-2019-Multiple Indicator Cluster Survey-Direct",
  "NPL-2022-Demographic and Health Survey-Direct",
  "SLV-2014-Multiple Indicator Cluster Survey-Direct",
  "TKM-2019-Multiple Indicator Cluster Survey-Direct",
  "TUR-2013-Demographic and Health Survey-Direct",
  "VNM-2013-2014-Multiple Indicator Cluster Survey-Direct",
  "ZAF-2016-Demographic and Health Survey-Direct"
)

db_all_group <- db_all_group %>%
  filter(!(label == "motherage" & IGME_Key %in% motherage_exclusions)) %>%
  arrange(Country.Code, label, Indicator, Group)

message("Final dataset: ", nrow(db_all_group), " rows  |  ",
        uniqueN(db_all_group$Country.Code[db_all_group$Indicator %in% c("NMR","U5MR")]),
        " countries")


# ---- 18. Write outputs ----

# Cleaned survey database, with survey wealth U5MR
db_cleaned_path <- file.path(dir_country,
  paste0("country-specific-database-cleaned_", RUN_DATE, ".csv"))
fwrite(db_all_group, db_cleaned_path)
message("Written: ", db_cleaned_path)

# Birth counts, keyed to the file above by survey, dimension and group
nbirth_path <- file.path(dir_country,
  paste0("country-specific-nbirth_", RUN_DATE, ".csv"))

# Hybrid surveys: sum noeducation + primary so 02's nbirth join matches
if (length(vnm_style_ids) > 0) {
  nbirth_vnm_noedu <- nbirth %>%
    filter(DataCatalogID %in% vnm_style_ids,
           label == "edu",
           Group %in% c("noeducation", "primary")) %>%
    group_by(across(-c(Group, Estimates, Standard.Error.of.Estimates))) %>%
    summarise(
      Estimates                   = sum(Estimates, na.rm = TRUE),
      Standard.Error.of.Estimates = NA_real_,
      .groups = "drop"
    ) %>%
    mutate(Group = "noedu.primary")

  nbirth <- nbirth %>%
    filter(!(DataCatalogID %in% vnm_style_ids &
             label == "edu" &
             Group %in% c("noeducation", "primary"))) %>%
    bind_rows(nbirth_vnm_noedu)
}

fwrite(nbirth, nbirth_path)
message("Written: ", nbirth_path)

# Variant with IGME model wealth U5MR (maps, Figure 3, report tables)
db_with_igme_wealth <- db_all_group %>%
  filter(!(Indicator == "U5MR" & label == "wealth")) %>%
  bind_rows(igme_wealth_U5MR)
db_igme_wealth_path <- file.path(dir_country,
  paste0("country-specific-database-cleaned-igme-wealth_", RUN_DATE, ".csv"))
fwrite(db_with_igme_wealth, db_igme_wealth_path)
message("Written: ", db_igme_wealth_path)

message("\n── 01_clean-data complete ──")
