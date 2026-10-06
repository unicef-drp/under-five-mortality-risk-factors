# Run from the Equity brochure project root with Rscript --vanilla.
suppressPackageStartupMessages(library(dplyr))
source("code/lib/cme-education.R")
source("code/lib/cme-validation.R")
source("code/lib/equity-common.R")

expect_error <- function(expr) {
  stopifnot(inherits(tryCatch({force(expr); NULL}, error = identity), "error"))
}
four <- c("noeducation", "primary", "secondary", "higher")
two <- c("noedu.primary", "sec.higher")
mk <- function(groups, year = "2022") {
  data.frame(Country.Code = "BGD", Series.Name = "DHS", Series.Year = year,
             Indicator = "U5MR", Group = groups, DataCatalogID = "9204",
             label = "edu", Estimates = as.numeric(seq_along(groups)), Reference.Date = 2019.5,
             Interval = 5, Series.Type = "Direct", Standard.Error.of.Estimates = 0.1)
}
stopifnot(setequal(retain_complete_education_sets(mk(c(four, two)))$Group, c(four, two)))
stopifnot(setequal(retain_complete_education_sets(mk(four))$Group, four))
stopifnot(setequal(retain_complete_education_sets(mk(two))$Group, two))
stopifnot(setequal(retain_complete_education_sets(mk(c(two, "primary")))$Group, two))
stopifnot(nrow(retain_complete_education_sets(mk(c("primary", "noeducation")))) == 0L)
stopifnot(nrow(retain_complete_education_sets(mk(c("primary", "noeducation", "sec.higher")))) == 0L)
# The same catalogue id reused in another year cannot grant membership.
expected <- aggregate_display_keys(mk(four))
actual <- bind_rows(mk(c(four, two)), mk(c(four, two), "2017"))
flagged <- flag_aggregate_membership(actual, expected)
stopifnot(all(flagged$in_aggregate[flagged$Series.Year == "2022"]))
stopifnot(!any(flagged$in_aggregate[flagged$Series.Year == "2017"]))
assert_expected_keys_present(expected, flagged)
expect_error(assert_expected_keys_present(expected, flagged[flagged$Group != "higher", ]))
# Partial education sets must be diagnosed separately even if four groups exist.
stopifnot(nrow(cme_completeness(mk(c(four, "sec.higher")))) == 1L)
stopifnot(nrow(cme_completeness(mk(c(four, two)))) == 0L)
# Source values take precedence; report-derived rows only fill genuinely absent keys.
source_rows <- mk(two)
source_rows$estimate_origin <- "source"
derived_rows <- mk(two)
derived_rows$Estimates <- 900
derived_rows$estimate_origin <- "derived"
supplemented <- supplement_education(source_rows, derived_rows, expected)
stopifnot(identical(supplemented$Estimates, source_rows$Estimates))
supplemented <- supplement_education(source_rows[1, ], derived_rows, expected)
stopifnot(nrow(supplemented) == 2L,
          supplemented$Estimates[supplemented$Group == "sec.higher"] == 900)
# Selected report pairs are read verbatim, not birth-weighted again.
report_four <- bind_rows(mk(four), mutate(mk(four), Indicator = "NMR"))
input_pairs <- bind_rows(mutate(mk(two), Estimates = c(37.65636461, 29.8041888)),
                        mutate(mk(two), Indicator = "NMR", Estimates = c(20, 15)))
paired <- lookup_source_education_pairs(report_four, input_pairs)
stopifnot(nrow(paired) == 6L,
          identical(paired$Estimates[paired$Indicator == "U5MR"], c(37.65636461, 29.8041888)),
          all(paired$category[paired$Indicator != "MR1t59"] == "source edu 2 groups alongside 4 groups"))
expect_error(lookup_source_education_pairs(report_four, input_pairs[-1, ]))
expect_error(lookup_source_education_pairs(report_four, bind_rows(input_pairs, mutate(input_pairs[1, ], Estimates = 99))))
# Recoding preserves unrelated columns, order and the caller's original data.
original <- data.table::data.table(label = c("birthorder", "birthinterval", "motherage", "edu"),
                                   Group = c("6+_", "5+", "<20_orig", "noeducation"), tag = 1:4)
normalized <- normalize_equity_groups(original)
stopifnot(identical(original$Group, c("6+_", "5+", "<20_orig", "noeducation")),
          identical(normalized$Group, c("4+", "4+", "<20", "noeducation")),
          identical(normalized$tag, original$tag), data.table::is.data.table(normalized))
cat("PASS: education representations, fallback priority, survey identity, missing-key QA and normalization\n")
