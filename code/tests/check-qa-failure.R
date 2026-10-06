# Prove that the real script 04 exits nonzero when a required observation is lost.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
source("code/run-config.R")
stamp <- equity_run_config()$run_date
source_root <- args[1]
test_root <- file.path(source_root, "qa", "missing-row-test")
for (p in c("country", "aggregate", "cme")) dir.create(file.path(test_root, p), recursive = TRUE, showWarnings = FALSE)
for (p in c(file.path("country", paste0("db-all-surveys_", stamp, ".csv")),
            file.path("aggregate", paste0("deaths-and-births-country_", stamp, ".csv")))) {
  stopifnot(file.copy(file.path(source_root, p), file.path(test_root, p), overwrite = TRUE))
}
cme <- data.table::fread(file.path(source_root, "cme", "riskfactors-data-CMEinfo.csv"))
remove <- which(cme$Country.ISO == "BGD" & cme$Series.Year == "2022" & cme$Indicator == "U5MR" &
                 cme$label == "edu" & cme$Group == "noedu.primary")
stopifnot(length(remove) == 1L)
data.table::fwrite(cme[-remove], file.path(test_root, "cme", "riskfactors-data-CMEinfo.csv"))
Sys.setenv(EQUITY_OUTPUT_ROOT = normalizePath(test_root, winslash = "/"), LC_ALL = "C")
log <- file.path(test_root, "expected-failure.log")
rc <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript.exe"),
                               c("--vanilla", shQuote("code/04_check_output.R")), stdout = log, stderr = log))
stopifnot(rc != 0L, any(grepl("CME QA failed", readLines(log, warn = FALSE), fixed = TRUE)))
cat("PASS: script 04 rejects a missing required BGD observation (expected nonzero exit)\n")
