# Build the CME website files from the pipeline outputs: read the full raw survey
# history, apply the CME display rules, and flag the rows that fed the world
# aggregate.
#
# Outputs, in output/cme/:
#   riskfactors-data-CMEinfo.csv                 qualifying surveys, with in_aggregate
#   UNIGME2025_risk_factor_aggregate_world.csv   world aggregate in CME format
#
# in_aggregate is matched on country, survey name and year, indicator and group,
# since a catalogue id alone cannot separate India's SRS vintages from NFHS.
# Wealth U5MR here is survey-based; the igme-wealth variant from 01 carries the
# modelled values.

rm(list = ls())

library(dplyr)
library(data.table)

USERPROFILE <- Sys.getenv("USERPROFILE")
source(file.path(USERPROFILE, "Dropbox/UNICEF Work/profile.R"))
source("code/run-config.R")
source("code/lib/equity-common.R")
source("code/lib/cme-validation.R")
source("code/lib/cme-education.R")
cfg <- equity_run_config()


# ---- 0. Paths and parameters ----

RUN_DATE  <- cfg$run_date
IGME_YEAR <- cfg$igme_year

output_dir  <- cfg$output_dir
dir_country <- file.path(output_dir, "country")
dir_agg     <- file.path(output_dir, "aggregate")

dir_cme <- file.path(output_dir, "cme")
if (!dir.exists(dir_cme)) dir.create(dir_cme, recursive = TRUE)

# Equity database, the same files as 01
db_dir          <- file.path(USERPROFILE,
                             "Dropbox/UNICEF Work/Equity database/Databases/Databases 2025")
DB_DATE_PATTERN <- cfg$db_date_pattern

cmeinfo_path       <- file.path(dir_IGME, "2025 Round Estimation/Code/input/country.info.CME.csv")
cleaned_db_path    <- file.path(dir_country, paste0("country-specific-database-cleaned_", RUN_DATE, ".csv"))
aggregate_path     <- file.path(dir_agg,  paste0("aggregate_",                 RUN_DATE, ".csv"))
deaths_births_path <- file.path(dir_agg,  paste0("deaths-and-births-country_", RUN_DATE, ".csv"))
country_count_path <- file.path(dir_agg,  paste0("aggregate-country-count_",   RUN_DATE, ".csv"))


# Column set for the CME data file
TARGET_COLS <- c(
  "Country.Name", "Country.ISO", "Series.Name", "Series.Year", "Indicator",
  "Sex", "Series.Type", "Series.Category", "Data.Collection.Method", "Series.Label",
  "Model.Life.Table", "Age.Group.of.Women", "Interval", "Series.Quantity",
  "Reference.Date", "Group", "Estimates", "Standard.Error.of.Estimates",
  "Indirect.method.based.on.TSFB", "Method.based.on.Age.Group.of.Women",
  "Inclusion", "Visible.indirect.series", "DataCatalogID", "Series.plot.symbol",
  "legend.series.label", "concat.type.adj", "Series.ID", "Point.ID",
  "Series.Type.orig", "label", "OfficialName", "in_aggregate"
)


# ---- 1. Reference data ----

cat("=== 1. Reference data ===\n")
cmeinfo <- fread(cmeinfo_path, select = c("ISO3Code", "OfficialName"),
                 data.table = FALSE) %>%
  filter(OfficialName != "")
cat(sprintf("  cmeinfo: %d countries\n", nrow(cmeinfo)))

# Short label for "Others" series: an acronym from the survey name
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


# ---- 2. Build the CME display dataset ----
# Every survey with a complete group set, NMR and U5MR only. The report filters
# (year cut-off, high income, quality) do not apply here; CME prefers
# Visible == 2 and falls back where no such row exists.

cat("\n=== 2. Build CME display dataset from equity database ===\n")

# Canonical groups per dimension
expected_groups_for <- function(lbl) {
  switch(lbl,
    wealth        = c("1.quintile", "2.quintile", "3.quintile", "4.quintile", "5.quintile"),
    residence     = c("rural", "urban"),
    birthorder    = c("1", "2-3", "4+"),
    birthinterval = c("<2", "2", "3", "4+"),
    motherage     = c("20-29", "30-39", "40-49", "<20"),
    NULL
  )
}

# All database files matching the date pattern
all_surveys <- read_equity_db(db_dir, DB_DATE_PATTERN)

# Group standardisation, as in 01
all_surveys <- normalize_equity_groups(all_surveys)
all_surveys <- all_surveys %>%
  mutate(
    estimate_origin = "source",
    DataCatalogID  = as.character(DataCatalogID),
    Estimates      = as.numeric(Estimates),
    Reference.Date = suppressWarnings(as.numeric(Reference.Date))
  ) %>%
  filter(!(label == "birthinterval" & Group == "first")) %>%
  # Keep only canonical groups; "total" is handled in section 3
  filter(!(label == "birthinterval" &
             !Group %in% c("<2", "2", "3", "4+", "total"))) %>%
  filter(!(label == "motherage" &
             !Group %in% c("<20", "20-29", "30-39", "40-49", "total")))

# Rates only, non-missing
all_surveys <- all_surveys[Indicator %in% c("NMR", "U5MR")] %>%
  filter(!is.na(Estimates))

# Some India SRS rows have no DataCatalogID; every SRS year uses 1865
all_surveys <- all_surveys %>%
  mutate(DataCatalogID = if_else(
    Country.Code == "IND" & Series.Name == "Sample Registration System" &
      (is.na(DataCatalogID) | DataCatalogID == ""),
    "1865", DataCatalogID
  ))

# Birth size is excluded from CME website outputs
all_surveys <- all_surveys %>% filter(label != "bsize")

# Indirect surveys: select canonical AOW/TSFB window and unify Reference.Date
all_surveys[, Age.Group.of.Women.or.Time.Since.First.Birth :=
              sub("_$", "", Age.Group.of.Women.or.Time.Since.First.Birth)]
cme_indirect <- all_surveys %>%
  filter(Series.Type == "Indirect") %>%
  filter(
    (Indirect.Classification == "AOW"  &
       Age.Group.of.Women.or.Time.Since.First.Birth == "25-29") |
    (Indirect.Classification == "TSFB" &
       Age.Group.of.Women.or.Time.Since.First.Birth == "5-9")
  ) %>%
  group_by(label, DataCatalogID) %>%
  mutate(Reference.Date = first(Reference.Date)) %>%
  ungroup()
all_surveys <- all_surveys %>%
  filter(Series.Type != "Indirect") %>%
  bind_rows(cme_indirect)
rm(cme_indirect)

# Keep complete two- and four-group sets. Hybrids missing a combined group are
# supplemented in 4a.
cme_edu <- retain_complete_education_sets(filter(all_surveys, label == "edu"))

# Other dimensions: require all groups within a survey and indicator. Checking at
# survey-year level lets mother's age <20 carry an earlier reference date than
# the older bands while still requiring all four bands.
cme_nonedu <- all_surveys %>%
  filter(label != "edu") %>%
  rowwise() %>%
  mutate(expected = list(expected_groups_for(label))) %>%
  ungroup() %>%
  group_by(Country.Code, Series.Name, Series.Year, Indicator, label) %>%
  filter(all(expected[[1]] %in% Group)) %>%
  ungroup() %>%
  select(-expected)

all_surveys <- bind_rows(cme_edu, cme_nonedu)
rm(cme_edu, cme_nonedu)

# Prefer Visible == 2 per survey, dimension, group and indicator, falling back
# where none exists, then keep the latest reference date.
all_surveys <- all_surveys %>%
  group_by(DataCatalogID, label, Group, Indicator) %>%
  mutate(.has_preferred = any(Visible == 2, na.rm = TRUE)) %>%
  filter(!.has_preferred | Visible == 2) %>%
  filter(Reference.Date == max(Reference.Date, na.rm = TRUE) |
           all(is.na(Reference.Date))) %>%
  slice(1) %>%
  ungroup() %>%
  select(-.has_preferred)

# Selection must not leave a partial education representation.
all_surveys$.cme_order <- seq_len(nrow(all_surveys))
all_surveys <- bind_rows(
  filter(all_surveys, label != "edu"),
  retain_complete_education_sets(filter(all_surveys, label == "edu"))
) %>% arrange(.cme_order) %>% select(-.cme_order)

# Groups of a dimension must share one reference date, or they are not
# contemporaneous. Education and mother's age are exempt, since the <20 band is
# reported earlier and is used in the aggregate; totals are never dropped.
all_surveys <- all_surveys %>%
  group_by(Country.Code, Series.Name, Series.Year, Indicator, label) %>%
  mutate(.rd_consistent = {
    rd <- Reference.Date[Group != "total" & !is.na(Reference.Date)]
    length(unique(rd)) <= 1
  }) %>%
  ungroup() %>%
  filter(
    label %in% c("edu", "motherage") |
      Group == "total" |
      .rd_consistent
  ) %>%
  select(-.rd_consistent)

cat(sprintf("  %d rows | %d countries | %d equity dimensions\n",
            nrow(all_surveys),
            n_distinct(all_surveys$Country.Code),
            n_distinct(all_surveys$label)))
cat("  Rows by label \u00d7 Indicator:\n")
print(table(all_surveys$label, all_surveys$Indicator))


# ---- 3. One canonical total per survey and indicator ----
# The modal estimate, as in section 9 of 01.

cat("\n=== 3. Canonical totals ===\n")

totals_raw <- all_surveys %>% filter(Group == "total")

n_differing <- totals_raw %>%
  group_by(DataCatalogID, Indicator) %>%
  summarise(n_vals = n_distinct(round(as.numeric(Estimates), 8)), .groups = "drop") %>%
  filter(n_vals > 1) %>%
  nrow()
cat(sprintf("  DataCatalogID \u00d7 Indicator combos with differing totals: %d\n", n_differing))

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

cat(sprintf("  Canonical totals: %d rows (one per DataCatalogID \u00d7 Indicator)\n",
            nrow(canonical_totals)))

all_surveys <- all_surveys %>%
  filter(Group != "total") %>%
  bind_rows(canonical_totals)


# ---- 4. Aggregate keys and the in_aggregate flag ----
# Keys of the rows that entered the aggregate in 02. Indicator is part of the key,
# so a survey contributing only NMR does not flag its U5MR rows.

cat("\n=== 4. Aggregate keys + in_aggregate flag ===\n")

agg_all <- fread(deaths_births_path, data.table = FALSE) %>%
  mutate(DataCatalogID = as.character(DataCatalogID)) %>%
  filter(Group != "total")

# Catalog-id count for the run log; flags use the full survey identity.
agg_catalog_ids <- unique(agg_all$DataCatalogID)

# Keys for non-total rows; bsize is not in the CME outputs
agg_keys <- aggregate_display_keys(agg_all)

# Countries behind each dimension, indicator and group, used in section 8
ref_area_lookup <- agg_all %>%
  filter(Indicator %in% c("NMR", "U5MR")) %>%
  group_by(label, Indicator, Group) %>%
  summarise(n_countries = n_distinct(Country.Code),
            .groups = "drop")

# 02 collapses four-group countries into the combined two-group aggregate, so
# they also count towards noedu.primary and sec.higher.
edu_combined_ref <- agg_all %>%
  filter(Indicator %in% c("NMR", "U5MR"), label == "edu",
         Group %in% c("noeducation", "primary", "noedu.primary",
                      "secondary",   "higher",  "sec.higher")) %>%
  mutate(Group = case_when(
    Group %in% c("noeducation", "primary", "noedu.primary") ~ "noedu.primary",
    Group %in% c("secondary",   "higher",  "sec.higher")    ~ "sec.higher"
  )) %>%
  group_by(label, Indicator, Group) %>%
  summarise(n_countries = n_distinct(Country.Code),
            .groups = "drop")

ref_area_lookup <- ref_area_lookup %>%
  filter(!(label == "edu" & Group %in% c("noedu.primary", "sec.higher"))) %>%
  bind_rows(edu_combined_ref)

# Birth interval "4+" is ">=4" in the aggregate file
ref_area_lookup <- bind_rows(
  ref_area_lookup,
  ref_area_lookup %>%
    filter(label == "birthinterval", Group == "4+") %>%
    mutate(Group = ">=4")
)
# Step 2: MR1t59 is derived from NMR/U5MR and uses the same country set as NMR.
ref_area_lookup <- bind_rows(
  ref_area_lookup,
  ref_area_lookup %>%
    filter(Indicator == "NMR") %>%
    mutate(Indicator = "MR1t59")
)

# aggregate_display_keys already carries the combined education keys

rm(agg_all)

# ---- 4a. Supplement missing combinations ----
# Most surveys supply two-group data; the report database covers hybrids such as
# Viet Nam's noeducation + primary + sec.higher.
report_edu <- fread(cleaned_db_path, data.table = FALSE) %>%
  mutate(DataCatalogID = as.character(DataCatalogID),
         Reference.Date = as.numeric(Reference.Date), Estimates = as.numeric(Estimates),
         estimate_origin = if_else(grepl("derived|recalculated", category),
                                   "report-derived", "report-source")) %>%
  filter(label == "edu", Group %in% c("noedu.primary", "sec.higher"),
         Indicator %in% c("NMR", "U5MR"))
n_before_supplement <- nrow(all_surveys)
all_surveys <- supplement_education(all_surveys, report_edu, agg_keys)
cat(sprintf("  Added %d missing education observations from the report database\n",
            nrow(all_surveys) - n_before_supplement))
rm(report_edu, n_before_supplement)

cat(sprintf("  Deaths-and-births: %d survey/dimension/group/indicator combinations (%d catalogue IDs)\n",
            nrow(agg_keys), length(agg_catalog_ids)))

all_surveys <- flag_aggregate_membership(all_surveys, agg_keys)

cat(sprintf("  Rows flagged in_aggregate = TRUE:  %d\n", sum(all_surveys$in_aggregate)))
cat(sprintf("  Rows flagged in_aggregate = FALSE: %d\n", sum(!all_surveys$in_aggregate)))


# ---- 5. Check every aggregate survey is present ----

cat("\n=== 5. Validation ===\n")

dir_qa <- file.path(output_dir, "qa")
dir.create(dir_qa, recursive = TRUE, showWarnings = FALSE)
missing_from_data <- anti_join(agg_keys, display_keys(all_surveys), by = CME_GROUP_KEY)
fwrite(missing_from_data, file.path(dir_qa, "cme-missing-aggregate-observations.csv"))
assert_expected_keys_present(agg_keys, all_surveys)
fwrite(all_surveys %>% filter(label == "edu") %>%
         select(Country.Code, Series.Name, Series.Year, DataCatalogID, Indicator,
                Group, Reference.Date, Interval, Estimates, estimate_origin, in_aggregate),
       file.path(dir_qa, "education-selection.csv"))
cat("  Every required aggregate survey/group is represented. OK\n")


# ---- 6. Align to the CME column format ----

cat("\n=== 6. Align to CME format ===\n")

align_db <- function(df, cmeinfo_df) {
  nm <- names(df)

  # Rename column variants from the equity DB naming convention
  nm[nm == "Age.Group.of.Women.or.Time.Since.First.Birth"] <- "Age.Group.of.Women"
  nm[nm == "Inclusion.U5MR"]  <- "Inclusion"
  nm[nm == "Visible"]         <- "Visible.indirect.series"
  names(df) <- nm

  # Drop numeric Country.ISO (UN code); rename alpha Country.Code → Country.ISO
  if ("Country.ISO" %in% names(df)) df <- select(df, -Country.ISO)
  if ("Country.Code" %in% names(df))
    names(df)[names(df) == "Country.Code"] <- "Country.ISO"

  # Join OfficialName, update Country.Name
  df <- df %>%
    left_join(cmeinfo_df %>% select(ISO3Code, OfficialName),
              by = c("Country.ISO" = "ISO3Code")) %>%
    mutate(Country.Name = if_else(!is.na(OfficialName), OfficialName, Country.Name))

  # Add missing target columns first, so case_when can reference them
  for (col in setdiff(TARGET_COLS, names(df))) df[[col]] <- NA_character_

  # Impute derived metadata columns
  df <- df %>%
    mutate(
      Data.Collection.Method = case_when(
        !is.na(Data.Collection.Method)              ~ Data.Collection.Method,
        Country.ISO == "IND" & label == "residence" ~ "SVR",
        TRUE                                        ~ "Full Birth Histories"
      ),
      Series.Label        = if_else(is.na(Series.Label),
                                    paste(Series.Category, Series.Type), Series.Label),
      Series.plot.symbol  = if_else(is.na(Series.plot.symbol),
                                    Data.Collection.Method, Series.plot.symbol),
      legend.series.label = if_else(is.na(legend.series.label),
                                    paste(Series.Name, Series.Year), legend.series.label),
      concat.type.adj     = if_else(is.na(concat.type.adj),
                                    Series.Category, concat.type.adj),
      Series.Type.orig    = if_else(is.na(Series.Type.orig), Series.Type, Series.Type.orig),
      Interval            = if_else(is.na(Interval), "5", as.character(Interval)),
      Sex                 = "Total"
    ) %>%
    group_by(DataCatalogID) %>%
    mutate(Series.Quantity = if_else(
      is.na(Series.Quantity),
      as.character(n_distinct(Series.Type)),
      Series.Quantity
    )) %>%
    ungroup() %>%
    mutate(
      Series.ID = if_else(
        is.na(Series.ID),
        paste(Country.ISO, Series.Name, Series.Year,
              Data.Collection.Method, Indicator, "Total", sep = "_"),
        gsub("_NA(?=_|$)", "_Total", Series.ID, perl = TRUE)
      ),
      Point.ID = if_else(
        is.na(Point.ID),
        paste(Country.ISO, Series.Name, Series.Year,
              Data.Collection.Method, Indicator, "Total",
              round(as.numeric(Reference.Date), 1), Group, label, sep = "_"),
        gsub("_NA(?=_|$)", "_Total", Point.ID, perl = TRUE)
      )
    )

  # Select in order; coerce all columns except in_aggregate to character
  df %>%
    select(all_of(TARGET_COLS)) %>%
    mutate(across(-in_aggregate, as.character))
}

data_out <- align_db(all_surveys, cmeinfo)
cat(sprintf("  %d rows \u00d7 %d cols\n", nrow(data_out), ncol(data_out)))
cat("  Rows by label \u00d7 Indicator:\n")
print(table(data_out$label, data_out$Indicator))
cat(sprintf("  in_aggregate = TRUE:  %d rows\n", sum(data_out$in_aggregate)))
cat(sprintf("  in_aggregate = FALSE: %d rows\n", sum(!data_out$in_aggregate)))


# ---- 7. Write the CME data file ----

cat("\n=== 7. Write CME data file ===\n")

out_data_path <- file.path(dir_cme, "riskfactors-data-CMEinfo.csv")
data_out <- data_out %>%
  mutate(
    Short_survey_name = if_else(
      Series.Category == "Others",
      abbrev_vec(Series.Name),
      Series.Category
    )
  ) %>%
  arrange(Country.ISO, as.numeric(Series.Year))
write.csv(data_out, out_data_path, row.names = FALSE, na = "")
cat(sprintf("  Saved: %s\n        (%d rows \u00d7 %d cols)\n",
            out_data_path, nrow(data_out), ncol(data_out)))


# ---- 8. Aggregate in CME world format ----
# Maps the display names back to machine labels. The pipeline has no uncertainty
# bounds, so Lower and Upper are written as NA.

cat("\n=== 8. Aggregate world CSV ===\n")

equity_to_label <- c(
  "Wealth quintile"    = "wealth",
  "Residence"          = "residence",
  "Mother's education" = "edu",
  "Birth order"        = "birthorder",
  "Birth interval"     = "birthinterval",
  "Mother's age"       = "motherage"
)

agg_raw <- read.csv(aggregate_path, stringsAsFactors = FALSE)

# Wealth U5MR countries come from the IGME model, not the survey keys
n_wealth_u5mr_igme <- {
  cc <- read.csv(country_count_path, stringsAsFactors = FALSE)
  cc$n_countries[cc$label == "wealth" & cc$Indicator == "U5MR (IGME model)"]
}

agg_world <- agg_raw %>%
  filter(Group != "total") %>%          # dimension-level totals not in CME aggregate
  filter(Indicator != "MR1t59") %>%     # MR1t59 excluded from the aggregate file
  # "combined all countries" is the definitive two-group aggregate; the
  # "2 groups" rows cover only native two-group countries
  filter(!(equity == "Mother's education" & type == "2 groups")) %>%
  mutate(
    # Year comes from the aggregate file; wealth U5MR spans 2000.5-2024.5
    Label  = equity_to_label[equity],
    Median = wgt.rate,
    Lower  = NA_real_,
    Upper  = NA_real_
  ) %>%
  filter(!is.na(Label)) %>%             # safety: drop unmapped rows
  select(Year, Label, Indicator, Group, Median, Lower, Upper) %>%
  left_join(ref_area_lookup, by = c("Label" = "label", "Indicator", "Group")) %>%
  # Override wealth U5MR country count with the IGME modelled count (106)
  mutate(n_countries = ifelse(Label == "wealth" & Indicator == "U5MR",
                              n_wealth_u5mr_igme, n_countries))

cat(sprintf("  %d rows\n", nrow(agg_world)))
cat("  Rows by Label \u00d7 Indicator:\n")
print(table(agg_world$Label, agg_world$Indicator))

out_agg_path <- file.path(dir_cme, "UNIGME2025_risk_factor_aggregate_world.csv")
write.csv(agg_world, out_agg_path, row.names = FALSE, na = "")
cat(sprintf("  Saved: %s\n", out_agg_path))


cat("\n\u2500\u2500 03_compile_data_for_cme complete \u2500\u2500\n")
