# Quality checks on the pipeline outputs. Run after 01, 02 and 03.
#
#   1. Every key in deaths-and-births is flagged in_aggregate in the CME file,
#      and nothing else is.
#   2. Each survey and dimension in the all-surveys snapshot has all its groups.
#   3. The delivered CME file is complete and free of duplicates; failure exits
#      with a non-zero status.

rm(list = ls())

library(dplyr)
library(data.table)

USERPROFILE <- Sys.getenv("USERPROFILE")
source(file.path(USERPROFILE, "Dropbox/UNICEF Work/profile.R"))
source("code/run-config.R")
source("code/lib/cme-validation.R")
cfg <- equity_run_config()


# ---- 0. Paths and parameters ----

RUN_DATE <- cfg$run_date

output_dir    <- cfg$output_dir
dir_country   <- file.path(output_dir, "country")

dir_aggregate <- file.path(output_dir, "aggregate")
CME_DIR       <- file.path(output_dir, "cme")

all_surveys_path    <- file.path(dir_country,
                        paste0("db-all-surveys_", RUN_DATE, ".csv"))
cme_data_path       <- file.path(CME_DIR, "riskfactors-data-CMEinfo.csv")
deaths_births_path  <- file.path(dir_aggregate,
                        paste0("deaths-and-births-country_", RUN_DATE, ".csv"))

# Expected sub-groups per dimension, excluding "total". Birth interval uses the
# raw code "4+"; ">=4" appears only in the aggregate output. Education counts as
# complete with either the four-group or the two-group set.
EXPECTED_GROUPS <- list(
  wealth        = c("1.quintile", "2.quintile", "3.quintile", "4.quintile", "5.quintile"),
  residence     = c("rural", "urban"),
  edu4          = c("noeducation", "primary", "secondary", "higher"),
  edu2          = c("noedu.primary", "sec.higher"),
  birthinterval = c("<2", "2", "3", "4+"),
  birthorder    = c("1", "2-3", "4+"),
  motherage     = c("<20", "20-29", "30-39", "40-49"),
  bsize         = c("small.vsmall", "av.large")
)


# ---- Load data ----

cat("Loading CME data file...\n")
cme_data <- fread(cme_data_path, data.table = FALSE) %>%
  mutate(DataCatalogID = as.character(DataCatalogID))
cat(sprintf("  %d rows\n", nrow(cme_data)))

cat("Loading deaths-and-births (aggregate ground truth)...\n")
deaths_births <- fread(deaths_births_path, data.table = FALSE) %>%
  mutate(DataCatalogID = as.character(DataCatalogID))
cat(sprintf("  %d rows\n", nrow(deaths_births)))

cat("Loading all-qualifying-surveys snapshot...\n")
all_surveys <- fread(all_surveys_path, data.table = FALSE) %>%
  mutate(DataCatalogID = as.character(DataCatalogID))
cat(sprintf("  %d rows\n", nrow(all_surveys)))

# Expected keys come from the report inputs, not from the CME file itself.
# Birth size is outside the CME schema.
agg_ground_truth <- aggregate_display_keys(deaths_births)
cme_agg <- cme_data %>%
  filter(as.logical(in_aggregate), Indicator %in% c("NMR", "U5MR"), label != "total") %>%
  display_keys()

cat(sprintf("  in_aggregate rows (non-total, NMR/U5MR): %d distinct keys\n", nrow(cme_agg)))
cat(sprintf("  deaths-and-births keys (non-total, NMR/U5MR): %d distinct keys\n", nrow(agg_ground_truth)))


# ---- 1. in_aggregate rows against deaths-and-births ----

cat("\n══ 1. in_aggregate vs deaths-and-births (aggregate ground truth) ══\n")

cme_all_keys <- display_keys(filter(cme_data, label != "total"))
missing_observations <- anti_join(agg_ground_truth, cme_all_keys, by = CME_GROUP_KEY)
dir_qa <- file.path(output_dir, "qa")
dir.create(dir_qa, recursive = TRUE, showWarnings = FALSE)
fwrite(missing_observations, file.path(dir_qa, "missing-observations.csv"))
cat(sprintf("  Required aggregate observations absent from CME: %d\n", nrow(missing_observations)))

# A: aggregate keys in the CME file but not flagged (expect 0)
not_flagged <- anti_join(
  agg_ground_truth, cme_agg, by = CME_GROUP_KEY
)
cat(sprintf("  Aggregate keys NOT flagged in_aggregate in CME:   %d\n", nrow(not_flagged)))
if (nrow(not_flagged) > 0) {
  cat("  (These surveys fed the aggregate but are not marked in_aggregate.\n")
  cat("   Investigate in script 03.\n")
  print(head(as.data.frame(not_flagged), 30))
}

# B: flagged rows not in deaths-and-births (expect 0)
extra_flagged <- anti_join(
  cme_agg, agg_ground_truth,
  by = CME_GROUP_KEY
)
cat(sprintf("  CME in_aggregate rows NOT in deaths-and-births:   %d\n", nrow(extra_flagged)))
if (nrow(extra_flagged) > 0) {
  cat("  (These rows are flagged in_aggregate but did NOT feed the aggregate.\n")
  cat("   Investigate in script 03.\n")
  print(head(as.data.frame(extra_flagged), 30))
}

fwrite(not_flagged, file.path(dir_qa, "not-flagged.csv"))
fwrite(extra_flagged, file.path(dir_qa, "extra-flagged.csv"))
if (nrow(missing_observations) == 0 && nrow(not_flagged) == 0 && nrow(extra_flagged) == 0)
  cat("  PASS: in_aggregate aligns exactly with the deaths-and-births aggregate inputs.\n")


# ---- 2. Group completeness ----
# Checked on the all-surveys snapshot, so it covers surveys outside the aggregate.

cat("\n══ 2. Report-input group completeness (not website history) ══\n")

completeness_check <- all_surveys %>%
  filter(
    Group      != "total",
    Indicator  == "U5MR",
    label      %in% c("wealth", "residence", "edu", "birthinterval",
                      "birthorder", "motherage", "bsize")
  ) %>%
  group_by(DataCatalogID, Country.Code, Series.Year, label) %>%
  summarise(groups_present = list(sort(unique(Group))), .groups = "drop") %>%
  rowwise() %>%
  mutate(
    # Education: complete with all four groups or both two-group categories
    complete = if (label == "edu") {
      all(EXPECTED_GROUPS[["edu4"]] %in% groups_present) ||
      all(EXPECTED_GROUPS[["edu2"]] %in% groups_present)
    } else {
      all(EXPECTED_GROUPS[[label]] %in% groups_present)
    },
    miss_groups = if (label == "edu") {
      # Missing groups relative to the closest complete set
      m4 <- setdiff(EXPECTED_GROUPS[["edu4"]], groups_present)
      m2 <- setdiff(EXPECTED_GROUPS[["edu2"]], groups_present)
      if (length(m4) <= length(m2)) list(m4) else list(m2)
    } else {
      list(setdiff(EXPECTED_GROUPS[[label]], groups_present))
    },
    extra = if (label == "edu") {
      list(setdiff(groups_present,
                   union(EXPECTED_GROUPS[["edu4"]], EXPECTED_GROUPS[["edu2"]])))
    } else {
      list(setdiff(groups_present, EXPECTED_GROUPS[[label]]))
    }
  ) %>%
  ungroup() %>%
  mutate(n_missing = sapply(miss_groups, length))

incomplete <- completeness_check %>% filter(!complete)
cat(sprintf("  DataCatalogID \u00d7 label combos with missing groups: %d\n", nrow(incomplete)))

if (nrow(incomplete) > 0) {
  incomplete %>%
    mutate(
      missing_str        = sapply(miss_groups, paste, collapse = ", "),
      groups_present_str = sapply(groups_present, paste, collapse = ", ")
    ) %>%
    select(DataCatalogID, Country.Code, Series.Year, label, n_missing,
           missing_str, groups_present_str) %>%
    as.data.frame() %>%
    print()
} else {
  cat("  PASS: all DataCatalogID \u00d7 label combos have all expected groups.\n")
}

# Completeness rate by dimension
completeness_summary <- completeness_check %>%
  group_by(label) %>%
  summarise(
    n_complete   = sum(complete),
    n_incomplete = sum(!complete),
    n_total      = n(),
    pct_complete = round(100 * n_complete / n_total, 1),
    .groups = "drop"
  )
cat("\nGroup completeness summary by label:\n")
print(as.data.frame(completeness_summary))

# Groups outside the recognised set for the dimension
has_extra <- completeness_check %>%
  filter(sapply(extra, length) > 0)
if (nrow(has_extra) > 0) {
  cat(sprintf("\n  %d combo(s) have UNEXPECTED extra groups:\n", nrow(has_extra)))
  has_extra %>%
    mutate(extra_str = sapply(extra, paste, collapse = ", ")) %>%
    select(DataCatalogID, Country.Code, Series.Year, label, extra_str) %>%
    as.data.frame() %>%
    print()
}


cat("\n=== 3. Delivered CME completeness and uniqueness ===\n")
display_incomplete <- cme_completeness(cme_data)
fwrite(display_incomplete, file.path(dir_qa, "cme-incomplete-groups.csv"))
cat(sprintf("  Incomplete displayed representations: %d\n", nrow(display_incomplete)))
if (nrow(display_incomplete)) print(as.data.frame(display_incomplete))
display_observation_keys <- cme_data %>%
  select(Country.ISO, Series.Name, Series.Year, Indicator, label, Group, Reference.Date, Interval)
duplicates <- cme_data[duplicated(display_observation_keys) | duplicated(display_observation_keys, fromLast = TRUE), ]
fwrite(duplicates, file.path(dir_qa, "cme-duplicate-observations.csv"))
expected_flags <- flag_aggregate_membership(cme_data, agg_ground_truth)$in_aggregate
wrong_flags <- cme_data[is.na(cme_data$in_aggregate) | as.logical(cme_data$in_aggregate) != expected_flags, ]
fwrite(wrong_flags, file.path(dir_qa, "cme-wrong-flags.csv"))
if (nrow(missing_observations) + nrow(not_flagged) + nrow(extra_flagged) +
    nrow(display_incomplete) + nrow(duplicates) + nrow(wrong_flags) > 0)
  stop("CME QA failed; inspect CSVs in ", dir_qa, call. = FALSE)
cat("PASS: required observations, flags, complete displayed groups and unique observations\n")
cat("\n\u2500\u2500 04_check_output complete \u2500\u2500\n")
