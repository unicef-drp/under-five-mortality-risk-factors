# Shared run settings. Report and website selection rules stay in their scripts.
equity_run_config <- function() {
  list(
    output_dir = Sys.getenv("EQUITY_OUTPUT_ROOT", unset = "output"),
    run_date = Sys.getenv("EQUITY_RUN_DATE", unset = "2026-06-23"),
    db_date_pattern = "2025-05-01",
    db_cutoff_date = as.Date("2026-02-01"),
    lb_ref_year = 2024.5,
    igme_year = 2024,
    igme_rates_date = "2026-01-28"
  )
}
