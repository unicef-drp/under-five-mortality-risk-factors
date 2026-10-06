# Source labels for Table 5's ethnicity and subnational LMIC columns.
# Keep these populations independent of the main equity selection database.
table5_additional_sources <- function(project_root) {
  ethnicity_file <- file.path(project_root, "input/ethnicity/ethnicity dataset_0522.csv")
  subnational_file <- file.path(project_root, "Subnational",
    "UN-IGME-2023-Subnational-U5MR-and-NMR-database-survey.xlsx")
  eth <- data.table::fread(ethnicity_file, data.table = FALSE)
  eth <- unique(eth[c("ISO3Code", "source", "SurveyYear")])
  source_names <- c(DHS = "Demographic and Health Survey",
    MICS = "Multiple Indicator Cluster Survey")
  eth$Source.Name <- unname(source_names[eth$source])
  eth$Source.Name[is.na(eth$Source.Name)] <- eth$source[is.na(eth$Source.Name)]
  eth$Source.Label <- paste(trimws(eth$Source.Name), trimws(eth$SurveyYear))
  eth <- eth[order(eth$ISO3Code, eth$SurveyYear, eth$Source.Name), ]

  sub <- as.data.frame(readxl::read_excel(subnational_file, sheet = "U5MR and NMR"))
  # Table 5 lists the most recent survey used, not excluded or older surveys.
  sub <- sub[!is.na(sub$Inclusion) & sub$Inclusion == 1 &
    sub$Indicator %in% c("Under-five mortality rate", "Neonatal mortality rate"), ]
  sub <- unique(sub[c("Country.ISO", "Series.Name", "Series.Year", "DataCatalogID")])
  survey_end <- function(year) {
    years <- regmatches(as.character(year), gregexpr("[0-9]{4}", as.character(year)))
    vapply(years, function(x) {
      if (!length(x)) stop("Cannot parse a subnational survey year.")
      max(as.integer(x))
    }, integer(1))
  }
  sub$Survey.End <- survey_end(sub$Series.Year)
  latest <- ave(sub$Survey.End, sub$Country.ISO, FUN = max)
  sub <- sub[sub$Survey.End == latest, ]
  sub$Source.Label <- paste(trimws(sub$Series.Name), trimws(sub$Series.Year))
  sub <- sub[order(sub$Country.ISO, sub$Source.Label), ]
  collapse_sources <- function(codes, labels) {
    if (anyNA(codes) || any(!nzchar(codes)) || anyNA(labels) || any(!nzchar(labels))) {
      stop("Missing country code or source label in Table 5 sources.")
    }
    vapply(split(labels, codes), function(x) paste(unique(x), collapse = "; "), character(1))
  }
  list(
    ethnicity = collapse_sources(eth$ISO3Code, eth$Source.Label),
    subnational = collapse_sources(sub$Country.ISO, sub$Source.Label),
    ethnicity_records = eth,
    subnational_records = sub
  )
}
