# Catalog IDs can cover multiple survey years (and are rewritten for India in
# the report). Display membership therefore uses the actual survey identity.
CME_SURVEY_KEY <- c("Country.Code", "Series.Name", "Series.Year")
CME_GROUP_KEY <- c(CME_SURVEY_KEY, "label", "Indicator", "Group")

display_keys <- function(x, unique = TRUE) {
  x <- as.data.frame(x)
  if (!"Country.Code" %in% names(x)) x$Country.Code <- x$Country.ISO
  stopifnot(all(CME_GROUP_KEY %in% names(x)))
  out <- dplyr::mutate(x, dplyr::across(dplyr::all_of(CME_GROUP_KEY), as.character)) |>
    dplyr::select(dplyr::all_of(CME_GROUP_KEY))
  if (unique) dplyr::distinct(out) else out
}

aggregate_display_keys <- function(aggregate_rows) {
  base <- aggregate_rows |>
    dplyr::filter(Group != "total", label != "bsize", Indicator %in% c("NMR", "U5MR")) |>
    display_keys()
  collapsed <- base |>
    dplyr::filter(label == "edu",
                  Group %in% c("noeducation", "primary", "secondary", "higher", "noedu.primary", "sec.higher")) |>
    dplyr::mutate(Group = ifelse(Group %in% c("noeducation", "primary", "noedu.primary"),
                                 "noedu.primary", "sec.higher"))
  dplyr::distinct(dplyr::bind_rows(base, collapsed))
}

flag_aggregate_membership <- function(rows, expected) {
  # Preserve input row order and the established long-file total convention:
  # a national total is flagged if this survey contributed in any indicator.
  keys <- display_keys(rows, unique = FALSE)
  marked <- dplyr::left_join(keys, dplyr::mutate(expected, .contributes = TRUE), by = CME_GROUP_KEY)
  stopifnot(nrow(marked) == nrow(rows))
  totals <- dplyr::distinct(dplyr::select(expected, dplyr::all_of(CME_SURVEY_KEY)))
  totals <- dplyr::left_join(keys, dplyr::mutate(totals, .total_contributes = TRUE), by = CME_SURVEY_KEY)
  rows$in_aggregate <- ifelse(rows$label == "total", !is.na(totals$.total_contributes),
                             !is.na(marked$.contributes))
  rows
}

assert_expected_keys_present <- function(expected, actual) {
  missing <- dplyr::anti_join(expected, display_keys(actual), by = CME_GROUP_KEY)
  if (nrow(missing)) {
    print(as.data.frame(missing))
    stop("Expected observations are absent from CME", call. = FALSE)
  }
  invisible(TRUE)
}

# Return the problems found.
cme_completeness <- function(rows) {
  expected <- list(
    wealth = c("1.quintile", "2.quintile", "3.quintile", "4.quintile", "5.quintile"),
    residence = c("rural", "urban"), birthorder = c("1", "2-3", "4+"),
    birthinterval = c("<2", "2", "3", "4+"),
    motherage = c("<20", "20-29", "30-39", "40-49"),
    edu4 = c("noeducation", "primary", "secondary", "higher"),
    edu2 = c("noedu.primary", "sec.higher")
  )
  keys <- display_keys(rows) |>
    dplyr::filter(Group != "total", label != "total", Indicator %in% c("NMR", "U5MR")) |>
    dplyr::mutate(representation = dplyr::case_when(
      label == "edu" & Group %in% expected$edu4 ~ "edu4",
      label == "edu" & Group %in% expected$edu2 ~ "edu2", TRUE ~ label
    )) |>
    dplyr::group_by(Country.Code, Series.Name, Series.Year, Indicator, label, representation) |>
    dplyr::summarise(groups = list(sort(unique(Group))), .groups = "drop")
  keys$missing <- vapply(seq_len(nrow(keys)), function(i) {
    wanted <- expected[[keys$representation[i]]]
    if (is.null(wanted)) return("unrecognized representation")
    paste(setdiff(wanted, keys$groups[[i]]), collapse = "|")
  }, character(1))
  keys$extra <- vapply(seq_len(nrow(keys)), function(i) {
    paste(setdiff(keys$groups[[i]], expected[[keys$representation[i]]]), collapse = "|")
  }, character(1))
  keys$groups <- vapply(keys$groups, paste, character(1), collapse = "|")
  dplyr::filter(keys, missing != "" | extra != "")
}
