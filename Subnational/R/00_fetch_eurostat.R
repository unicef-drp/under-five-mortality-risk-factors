# Pulls the two Eurostat source tables into input/eurostat/ as gzipped long CSVs.
# Run-once (or whenever Eurostat updates); everything downstream reads the cached files.

library(eurostat)
library(dplyr)
library(readr)

IN <- file.path("input", "eurostat")
dir.create(IN, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Deaths and population by single year of age, NUTS 0-3 ----
tables <- c(dth = "demo_r_magec", pop = "demo_r_d2jan")

for (nm in names(tables)) {
  code <- tables[[nm]]
  d <- get_eurostat(code, time_format = "num", cache = FALSE)
  write_csv(d, file.path(IN, paste0(code, ".csv.gz")))

  # provenance: the publisher's own last-update stamp, so a rerun can be dated
  stamp <- eurostat::get_eurostat_toc() %>%
    filter(code == !!code) %>%
    slice(1) %>%
    pull(last.update.of.data)

  writeLines(
    c(paste0("dataset:      ", code),
      paste0("retrieved:    ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      paste0("last update:  ", stamp),
      paste0("rows:         ", nrow(d)),
      paste0("years:        ", min(d$TIME_PERIOD), "-", max(d$TIME_PERIOD))),
    file.path(IN, paste0(code, ".provenance.txt"))
  )

  message(code, ": ", nrow(d), " rows, ", min(d$TIME_PERIOD), "-", max(d$TIME_PERIOD))
}
