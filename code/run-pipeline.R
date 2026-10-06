# Usage from project root:
# Rscript --vanilla code/run-pipeline.R OUTPUT_DIR [SCRIPT_DIR] [RUN_WORKDIR]
# The optional script and work directories run an alternative copy of the scripts.
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) > 3L)
  stop("Usage: run-pipeline.R OUTPUT_DIR [SCRIPT_DIR] [RUN_WORKDIR]")
project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
source("code/run-config.R")
cfg <- equity_run_config()
USERPROFILE <- Sys.getenv("USERPROFILE")
source(file.path(USERPROFILE, "Dropbox/UNICEF Work/profile.R"))
out <- args[1]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(out, winslash = "/", mustWork = TRUE)
scripts_dir <- normalizePath(if (length(args) >= 2L) args[2] else "code", winslash = "/", mustWork = TRUE)
run_dir <- normalizePath(if (length(args) >= 3L) args[3] else project_dir, winslash = "/", mustWork = TRUE)
qa <- file.path(out, "qa")
logs <- file.path(out, "logs")
dir.create(qa, recursive = TRUE, showWarnings = FALSE)
dir.create(logs, recursive = TRUE, showWarnings = FALSE)

db_dir <- file.path(USERPROFILE, "Dropbox/UNICEF Work/Equity database/Databases/Databases 2025")
wealth_dir <- file.path(USERPROFILE, "Dropbox/IGME Wealth/output/M17/country-specific")
quintiles <- c("1st", "2nd", "3rd", "4th", "5th")
inputs <- c(
  file.path(USERPROFILE, "Dropbox/UNICEF Work/profile.R"),
  list.files(db_dir, pattern = paste0("^data_.*", cfg$db_date_pattern), full.names = TRUE),
  file.path(dir_IGME, "2025 Round Estimation/Code/input", c("country.info.CME.csv", "data_livebirths.csv")),
  file.path(dir_IGME, paste0("2025 Round Estimation/Code/Aggregate results (final) ", cfg$igme_rates_date),
            "Rates & Deaths_Country Summary.csv"),
  file.path(wealth_dir, paste0("Estimated U5MR ", quintiles, " wealth quintile group_country.csv")),
  file.path(USERPROFILE, "Dropbox/IGME Wealth/aggregate", c(
    "IGME2025_U5MR_wealth_quintile_projection_country.csv", "IGME2025_U5MR_wealth_quintile_projection_world.csv")),
  file.path(USERPROFILE, "Dropbox/UNICEF Work/Data and charts for websites/Files 2025/CME/Source data/wealthquintile-data-CMEinfo.xlsx")
)
if (any(!file.exists(inputs))) stop("Missing inputs: ", paste(inputs[!file.exists(inputs)], collapse = "; "))
hash_files <- function(paths) {
  paths <- sort(unique(normalizePath(paths, winslash = "/", mustWork = TRUE)))
  data.frame(path = paths, md5 = unname(tools::md5sum(paths)),
             bytes = file.info(paths)$size, stringsAsFactors = FALSE)
}
before <- hash_files(inputs)
write.csv(before, file.path(qa, "run-inputs-before.csv"), row.names = FALSE)
script_files <- list.files(scripts_dir, pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
scripts_before <- hash_files(script_files)
write.csv(scripts_before, file.path(qa, "run-scripts.csv"), row.names = FALSE)
write.csv(data.frame(setting = names(cfg), value = vapply(cfg, as.character, character(1))),
          file.path(qa, "run-settings.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(qa, "run-session.txt"))

Sys.setenv(EQUITY_OUTPUT_ROOT = out, EQUITY_RUN_DATE = cfg$run_date, LC_ALL = "C")
Sys.unsetenv("RISKFACTORS_WIDE_OUT_PATH")
scripts <- c("01_clean-data.R", "02_produce-aggregate.R", "03_compile_data_for_cme.R", "04_check_output.R", "05_make_cme_wide_format.R")
rscript <- file.path(R.home("bin"), "Rscript.exe")
status <- data.frame(script = character(), exit_code = integer(), log = character())
setwd(run_dir)
for (script in scripts) {
  log <- file.path(logs, paste0(script, ".log"))
  rc <- system2(rscript, c("--vanilla", shQuote(file.path(scripts_dir, script))), stdout = log, stderr = log)
  status <- rbind(status, data.frame(script, exit_code = rc, log))
  write.csv(status, file.path(qa, "run-status.csv"), row.names = FALSE)
  cat(script, " exit=", rc, "\n", sep = "")
  if (rc != 0L) {
    cat(tail(readLines(log, warn = FALSE), 25L), sep = "\n")
    stop("Pipeline stopped; inspect ", log)
  }
}
after <- hash_files(inputs)
write.csv(after, file.path(qa, "run-inputs-after.csv"), row.names = FALSE)
stopifnot(identical(before, after), identical(scripts_before, hash_files(script_files)))
outputs <- unlist(lapply(c("country", "aggregate", "cme"), function(p) {
  list.files(file.path(out, p), pattern = "[.]csv$", full.names = TRUE)
}))
write.csv(hash_files(outputs), file.path(qa, "run-outputs.csv"), row.names = FALSE)
cat("PASS: all scripts completed; input and script hashes unchanged during the run\n")
