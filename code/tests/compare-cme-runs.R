# Rscript code/tests/compare-cme-runs.R baseline/output candidate/output
suppressPackageStartupMessages({library(data.table); library(dplyr)})
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
baseline <- args[1]; candidate <- args[2]
qa <- file.path(candidate, "qa")
dir.create(qa, recursive = TRUE, showWarnings = FALSE)
source("code/run-config.R")
source("code/lib/cme-validation.R")
stamp <- equity_run_config()$run_date
read_csv <- function(root, relative) fread(file.path(root, relative), colClasses = "character", na.strings = "")
canonical <- function(x) {
  x <- copy(as.data.table(x))
  setcolorder(x, sort(names(x)))
  setorderv(x, names(x), na.last = FALSE)
  as.data.frame(x)
}
same_table <- function(a, b) identical(canonical(a), canonical(b))
summary_rows <- list()
record <- function(item, status, detail) {
  summary_rows[[length(summary_rows) + 1L]] <<- data.frame(item, status, detail)
  cat(status, ": ", item, " -- ", detail, "\n", sep = "")
}
unchanged <- c(
  file.path("country", paste0("db-all-surveys_", stamp, ".csv")),
  file.path("country", paste0("db-pre-aggregation_", stamp, ".csv")),
  file.path("country", paste0("country-specific-nbirth_", stamp, ".csv")),
  file.path("aggregate", paste0("aggregate_", stamp, ".csv")),
  file.path("aggregate", paste0("aggregate-country-count_", stamp, ".csv")),
  file.path("aggregate", paste0("deaths-and-births-country_", stamp, ".csv")),
  "cme/UNIGME2025_risk_factor_aggregate_world.csv"
)
for (f in unchanged) {
  a <- read_csv(baseline, f); b <- read_csv(candidate, f)
  ok <- same_table(a, b)
  record(f, if (ok) "PASS" else "FAIL", paste(nrow(a), "baseline rows;", nrow(b), "candidate rows; exact table comparison"))
}

companion <- function(d) !is.na(d$category) & d$category %in% c(
  "recalculated edu 2 groups from 4 groups", "source edu 2 groups alongside 4 groups",
  "derived edu 2 groups alongside 4 groups")
rate_key <- c("Country.Code", "Series.Name", "Series.Year", "Series.Type", "Indicator", "Group", "Reference.Date", "Interval")
for (stem in c("country-specific-database-cleaned", "country-specific-database-cleaned-igme-wealth")) {
  f <- file.path("country", paste0(stem, "_", stamp, ".csv"))
  a <- read_csv(baseline, f); b <- read_csv(candidate, f)
  ac <- a[companion(a)]; bc <- b[companion(b)]
  stopifnot(!anyDuplicated(ac[, ..rate_key]), !anyDuplicated(bc[, ..rate_key]))
  ok <- same_table(a[!companion(a)], b[!companion(b)]) && same_table(ac[, ..rate_key], bc[, ..rate_key])
  record(stem, if (ok) "PASS" else "FAIL", "Only companion two-group education values/metadata may change; keys and all other rows must match")
  changes <- merge(ac[, c(rate_key, "Estimates", "Standard.Error.of.Estimates"), with = FALSE],
                   bc[, c(rate_key, "Estimates", "Standard.Error.of.Estimates"), with = FALSE],
                   by = rate_key, suffixes = c(".baseline", ".candidate"))
  changes[, difference := as.numeric(Estimates.candidate) - as.numeric(Estimates.baseline)]
  fwrite(changes, file.path(qa, paste0(stem, "-education-changes.csv")))
  cat("  Max absolute education difference: ", max(abs(changes$difference)), "\n", sep = "")
}

long_file <- "cme/riskfactors-data-CMEinfo.csv"
a <- read_csv(baseline, long_file); b <- read_csv(candidate, long_file)
key <- c("Country.ISO", "Series.Name", "Series.Year", "Indicator", "label", "Group", "Reference.Date", "Interval")
stopifnot(!anyDuplicated(a[, ..key]), !anyDuplicated(b[, ..key]))
added <- anti_join(b, a, by = key); removed <- anti_join(a, b, by = key)
fwrite(added, file.path(qa, "cme-added-observations.csv"))
fwrite(removed, file.path(qa, "cme-removed-observations.csv"))
allowed_rows <- function(x) all(x$label == "edu" & x$Group %in% c("noedu.primary", "sec.higher", "noeducation", "primary", "secondary", "higher"))
record("CME added/removed observations", if (allowed_rows(added) && allowed_rows(removed)) "PASS" else "FAIL",
       paste(nrow(added), "added;", nrow(removed), "removed; education only"))
four <- c("noeducation", "primary", "secondary", "higher")
old_four <- a[label == "edu" & Group %in% four, .(complete4 = all(four %in% Group)),
              by = .(Country.ISO, Series.Name, Series.Year, Indicator)]
removed_four <- merge(as.data.table(removed)[Group %in% four], old_four,
                      by = c("Country.ISO", "Series.Name", "Series.Year", "Indicator"))
stopifnot(!any(removed_four$complete4))
record("Removed four-group fragments", "PASS", paste(nrow(removed_four), "rows belonged to incomplete representations, not full four-group series"))
matches <- merge(a, b, by = key, suffixes = c(".baseline", ".candidate"))
value_changed <- matches[!(is.na(Estimates.baseline) & is.na(Estimates.candidate)) &
                           (is.na(Estimates.baseline) | is.na(Estimates.candidate) | Estimates.baseline != Estimates.candidate)]
fwrite(value_changed, file.path(qa, "cme-changed-values.csv"))
record("CME changed values", if (all(value_changed$label == "edu" & value_changed$Group %in% c("noedu.primary", "sec.higher"))) "PASS" else "FAIL",
       paste(nrow(value_changed), "changed matching values; only two-group education allowed"))
flag_changes <- matches[in_aggregate.baseline != in_aggregate.candidate]
fwrite(flag_changes, file.path(qa, "cme-changed-flags.csv"))
record("CME membership corrections", "INFO", paste(nrow(flag_changes), "flag changes; validated independently by script 04"))
metadata <- setdiff(names(a), c(key, "Estimates", "in_aggregate"))
metadata_changes <- bind_rows(lapply(metadata, function(column) {
  old <- matches[[paste0(column, ".baseline")]]; new <- matches[[paste0(column, ".candidate")]]
  changed <- xor(is.na(old), is.na(new)) | (!is.na(old) & !is.na(new) & old != new)
  out <- as.data.frame(matches[which(changed), ..key])
  out$column <- rep(column, nrow(out)); out$baseline <- old[which(changed)]; out$candidate <- new[which(changed)]
  out
}))
fwrite(metadata_changes, file.path(qa, "cme-changed-metadata.csv"))
# Non-education content must be byte-equivalent after table sorting, except for
# explicitly verified aggregate-membership corrections.
stable_cols <- setdiff(names(a), "in_aggregate")
record("CME non-education values and metadata", if (same_table(a[label != "edu", ..stable_cols], b[label != "edu", ..stable_cols])) "PASS" else "FAIL",
       "All non-education columns except aggregate membership are unchanged")

# Compare delivered SDMX data too: wealth survey replacements and WORLD rows
# are generated by 05, so the long-file comparison alone is insufficient.
# Allow archived runs to retain the former filename.
sdmx_file <- function(root) {
  current <- file.path("cme", paste0("riskfactors-sdmx-upload_", stamp, ".csv"))
  if (file.exists(file.path(root, current))) current else
    file.path("cme", paste0("riskfactors-wide_", stamp, ".csv"))
}
wa <- read_csv(baseline, sdmx_file(baseline)); wb <- read_csv(candidate, sdmx_file(candidate))
wide_key <- c("REF_AREA", "INDICATOR", "SEX", "RESIDENCE", "WEALTH_QUINTILE", "EDUCATION_LEVEL",
              "MATERNAL_AGE_AT_BIRTH", "BIRTH_INTERVAL", "BIRTH_ORDER", "SERIES_YEAR", "TIME_PERIOD")
stopifnot(identical(names(wa), names(wb)), !anyDuplicated(wb[, ..wide_key]))
wcols <- setdiff(names(wa), "IN_AGGREGATE")
record("SDMX non-education observations", if (same_table(wa[EDUCATION_LEVEL == "_T", ..wcols], wb[EDUCATION_LEVEL == "_T", ..wcols])) "PASS" else "FAIL",
       "Same schema, unique keys, non-education data unchanged except validated membership")

# BGD: raw supplied pairs should appear across all eleven qualifying surveys.
audit <- fread(file.path(qa, "education-selection.csv"))
bgd <- audit[Country.Code == "BGD" & Indicator == "U5MR" & Group %in% c("noedu.primary", "sec.higher")]
stopifnot(uniqueN(bgd$Series.Year) == 11L, all(bgd$estimate_origin == "source"), nrow(bgd) == 22L)
fwrite(bgd, file.path(qa, "BGD-education-source-observations.csv"))
record("BGD U5MR source pairs", "PASS", "22 source observations across 11 surveys")
source_path <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/UNICEF Work/Equity database/Databases/Databases 2025/data_edu_2025-05-01.csv")
raw <- fread(source_path)
raw$Series.Year <- as.character(raw$Series.Year)
bgd$Series.Year <- as.character(bgd$Series.Year)
source_key <- c("Country.Code", "Series.Name", "Series.Year", "Indicator", "Group", "Reference.Date", "Interval")
joined <- merge(bgd, raw, by = source_key, suffixes = c(".output", ".source"))
stopifnot(nrow(joined) == nrow(bgd), all(joined$Estimates.output == joined$Estimates.source))
record("BGD source equality", "PASS", "All selected pairs equal the raw estimates at the same reference period")

clean <- fread(file.path(candidate, "country", paste0("country-specific-database-cleaned_", stamp, ".csv")))
pairs <- clean[category == "source edu 2 groups alongside 4 groups" & Indicator %in% c("U5MR", "NMR")]
pairs$Series.Year <- as.character(pairs$Series.Year)
pair_source <- merge(pairs, raw, by = c(source_key, "Series.Type"), suffixes = c(".output", ".source"))
stopifnot(nrow(pair_source) == nrow(pairs), all(pair_source$Estimates.output == pair_source$Estimates.source))
record("Selected report source equality", "PASS", paste(nrow(pairs), "two-group rates equal their source observations"))

# Flag duplicated source labels; they are reported here, not corrected.
source_groups <- raw[Country.Code %in% clean$Country.Code & Indicator %in% c("NMR", "U5MR") &
                     Group %in% c(four, "noedu.primary", "sec.higher")]
period <- setdiff(source_key, "Group")
source_groups <- unique(source_groups[, c(period, "Group", "Estimates"), with = FALSE])
source_wide <- dcast(source_groups, as.formula(paste(paste(period, collapse = " + "), "~ Group")),
                     value.var = "Estimates", fun.aggregate = function(x) if(length(unique(x)) == 1L) x[1] else NA_real_)
review <- source_wide[!is.na(noedu.primary) & !is.na(sec.higher) & !is.na(noeducation) & !is.na(primary) &
                      !is.na(secondary) & !is.na(higher) & noedu.primary == noeducation & sec.higher == primary &
                      (secondary != primary | higher != primary)]
fwrite(review, file.path(qa, "education-source-review.csv"))
record("Source label review", "REVIEW", paste(nrow(review), "source survey/period/indicator cases copy noeducation and primary into the combined categories; inspect education-source-review.csv"))

mb <- fread(file.path(baseline, "qa", "run-inputs-before.csv"))
ma <- fread(file.path(candidate, "qa", "run-inputs-before.csv"))
stopifnot(same_table(mb, ma))
record("Baseline/candidate inputs", "PASS", paste(nrow(ma), "identical source file hashes"))

out <- bind_rows(summary_rows)
fwrite(out, file.path(qa, "comparison-summary.csv"))
if (any(out$status == "FAIL")) stop("Unexplained baseline/candidate difference; inspect qa/comparison-summary.csv")
cat("PASS: baseline/candidate comparison completed\n")
