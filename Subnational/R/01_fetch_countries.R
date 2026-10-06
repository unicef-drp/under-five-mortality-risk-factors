# Country sources that are reachable as a stable URL or API. Everything else in
# input/country/ is a hand download - see input/country/SOURCES.md.
# Run-once (or whenever a publisher updates); the build reads only the cached files.

library(httr2)

IN <- file.path("input", "country")

stamp <- function(dir, ...) {
  writeLines(c(..., paste0("retrieved:  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))),
             file.path(dir, "provenance.txt"))
}

fetch <- function(url, path, accept = NULL) {
  req <- request(url)
  if (!is.null(accept)) req <- req_headers(req, Accept = accept)
  resp <- req_perform(req, path = path)
  message(basename(path), ": ", resp_status(resp), ", ", file.size(path), " bytes")
}

# ---- 1. Australia: ABS Data API (SDMX) ----
# ABS Data API (data.api.abs.gov.au): same dataflow and labelled-CSV shape as the
# 2021 export.
# key = MEASURE(Population, Deaths) . SEX(Persons) . AGE(0-4) . REGION(all) . FREQ(all)
aus <- file.path(IN, "Australia")
fetch(
  paste0("https://data.api.abs.gov.au/rest/data/",
         "ABS,DEATHS_AGESPECIFIC_OCCURENCEYEAR,1.0.0/1+4.3.0+1+2+3+4...",
         "?startPeriod=2015"),
  file.path(aus, "ABS_DEATHS_AGESPECIFIC_OCCURENCEYEAR_0to4.csv"),
  accept = "application/vnd.sdmx.data+csv;labels=both"
)
stamp(aus,
      "source:     ABS Data API, dataflow DEATHS_AGESPECIFIC_OCCURENCEYEAR 1.0.0",
      "portal:     https://dataexplorer.abs.gov.au/",
      "publication: Deaths, Australia, 2024")

# ---- 2. Malaysia: OpenDOSM ----
mys <- file.path(IN, "Malaysia")
fetch("https://storage.dosm.gov.my/demography/deaths_early_childhood_state.csv",
      file.path(mys, "deaths_early_childhood_state.csv"))
fetch("https://storage.dosm.gov.my/demography/birth_state.csv",
      file.path(mys, "birth_state.csv"))
stamp(mys,
      "source:     OpenDOSM data catalogue (deaths_early_childhood_state, births_annual_state)",
      "publication: Vital Statistics, Malaysia, 2025 (16 Oct 2025); next update 15 Oct 2026")

# ---- 3. Argentina: DEIS open data ----
# Infant mortality and births are complete here; the deaths microdata is 0-14 only, so the
# 1-4 component still needs the DEIS anuario table (hand download).
arg <- file.path(IN, "Argentina")
base <- "https://datos.salud.gob.ar/dataset"
fetch(paste0(base, "/2eff770c-1c2b-4a22-9281-c3b5e9412086/resource/",
             "c1253897-d507-41f7-a3e1-6ed756e7243b/download/",
             "tasa-mortalidad-infantil-deis-1990-2024.csv"),
      file.path(arg, "tasa-mortalidad-infantil-deis-1990-2024.csv"))
fetch(paste0(base, "/01ede118-2c3e-4f92-b943-cec5770ad83e/resource/",
             "8e46c92b-c0ab-4e99-b967-0f1a5f2dc9f6/download/",
             "serie-historica-nacimientos-ocurridos-argentina-jurisdiccion-1914-2024_1.xlsx"),
      file.path(arg, "nacimientos_jurisdiccion_1914-2024.xlsx"))
fetch(paste0(base, "/27c588e8-43d0-411a-a40c-7ecc563c2c9f/resource/",
             "28e2aef4-c536-43bc-a098-0bdfb9d7933c/download/defuncion2024.csv"),
      file.path(arg, "defuncion2024.csv"))
stamp(arg,
      "source:     DEIS via datos.salud.gob.ar",
      "gap:        deaths microdata is 0-14 only; 1-4 deaths still need the DEIS anuario")
