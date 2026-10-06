retain_complete_education_sets <- function(x) {
  four <- c("noeducation", "primary", "secondary", "higher")
  two <- c("noedu.primary", "sec.higher")
  x |>
    dplyr::group_by(Country.Code, Series.Name, Series.Year, Indicator) |>
    dplyr::mutate(.has4 = all(four %in% Group), .has2 = all(two %in% Group)) |>
    dplyr::filter((.has4 & Group %in% four) | (.has2 & Group %in% two) |
                    ((.has4 | .has2) & Group == "total")) |>
    dplyr::ungroup() |>
    dplyr::select(-.has4, -.has2)
}

# Only missing display observations are supplemented here; source observations
# always take precedence over the narrow report-derived hybrid combinations.
supplement_education <- function(source_rows, report_rows, expected) {
  needed <- expected |>
    dplyr::filter(label == "edu", Group %in% c("noedu.primary", "sec.higher")) |>
    dplyr::anti_join(display_keys(source_rows), by = CME_GROUP_KEY)
  additions <- report_rows |>
    dplyr::semi_join(needed, by = CME_GROUP_KEY)
  if (anyDuplicated(display_keys(additions, unique = FALSE)))
    stop("Ambiguous report education observations for a missing display key", call. = FALSE)
  dplyr::bind_rows(source_rows, additions)
}

# Source pairs for the report's selected four-group surveys. Their insertion is
# deliberately AFTER birth-count preparation: overlapping categories must not
# affect the four-group weights used by 02. Match the actual observation period.
lookup_source_education_pairs <- function(four_rows, source_rows) {
  join_key <- c(CME_SURVEY_KEY, "Series.Type", "Indicator", "Group", "Reference.Date", "Interval")
  wanted <- four_rows |>
    dplyr::filter(Indicator %in% c("NMR", "U5MR")) |>
    dplyr::mutate(Group = ifelse(Group %in% c("noeducation", "primary"), "noedu.primary", "sec.higher")) |>
    dplyr::select(dplyr::all_of(join_key), .report_catalog = DataCatalogID) |>
    dplyr::distinct()
  source_rows <- source_rows |>
    dplyr::filter(Group %in% c("noedu.primary", "sec.higher"), !is.na(Estimates))
  # CSV inference can make the year numeric in one table and character in another.
  text_key <- c(CME_SURVEY_KEY, "Series.Type", "Indicator", "Group")
  wanted <- dplyr::mutate(wanted, dplyr::across(dplyr::all_of(text_key), as.character))
  source_rows <- dplyr::mutate(source_rows,
    dplyr::across(dplyr::all_of(text_key), as.character),
    Reference.Date = as.numeric(Reference.Date), Interval = as.numeric(Interval))
  wanted <- dplyr::mutate(wanted, Reference.Date = as.numeric(Reference.Date), Interval = as.numeric(Interval))
  matched <- dplyr::semi_join(source_rows, wanted, by = join_key) |>
    dplyr::distinct()
  if (anyDuplicated(dplyr::select(matched, dplyr::all_of(join_key))))
    stop("Ambiguous source two-group education observation; inspect the source duplicates", call. = FALSE)
  missing <- dplyr::anti_join(wanted, matched, by = join_key)
  if (nrow(missing)) {
    print(as.data.frame(missing))
    stop("Selected four-group survey lacks a source pair at the same period; review this exception", call. = FALSE)
  }
  result <- dplyr::inner_join(matched, wanted, by = join_key) |>
    dplyr::mutate(DataCatalogID = .report_catalog,
                  category = "source edu 2 groups alongside 4 groups") |>
    dplyr::select(-.report_catalog)
  # MR1t59 is an indicator derivation from the supplied pair, not a weighted
  # recombination of the four education categories.
  period_key <- setdiff(join_key, "Indicator")
  nmr <- dplyr::filter(result, Indicator == "NMR")
  u5 <- result |>
    dplyr::filter(Indicator == "U5MR") |>
    dplyr::select(dplyr::all_of(period_key), .u5 = Estimates)
  mr <- dplyr::inner_join(nmr, u5, by = period_key) |>
    dplyr::mutate(Estimates = 1000 * (1 - (1 - .u5 / 1000) / (1 - Estimates / 1000)),
                  Indicator = "MR1t59", Standard.Error.of.Estimates = NA_real_,
                  category = "derived edu 2 groups alongside 4 groups") |>
    dplyr::select(-.u5)
  dplyr::bind_rows(result, mr)
}
