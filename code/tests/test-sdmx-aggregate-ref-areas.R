library(data.table)

# Exercise the actual aggregate assembly block without loading survey inputs.
expressions <- parse("code/05_make_cme_wide_format.R", encoding = "UTF-8")
assigns <- function(x, name) is.call(x) && identical(x[[1]], as.name("<-")) &&
  identical(x[[2]], as.name(name))
test_env <- new.env(parent = globalenv())
for (name in c("indicator_code", "recode_dim", "aggregate_ref_area_code")) {
  for (expr in expressions[vapply(expressions, assigns, logical(1), name = name)]) {
    eval(expr, test_env)
  }
}
test_env$DATAFLOW_ID <- "test"
test_env$agg <- data.table(
  Label = c("residence", "wealth", "edu", "edu", "motherage", "birthinterval", "birthorder"),
  Indicator = "NMR", Year = 2024.5, Median = seq_len(7), n_countries = 66L,
  RESIDENCE = c("R", rep("_T", 6)),
  WEALTH_QUINTILE = c("_T", "Q1", rep("_T", 5)),
  EDUCATION = c("_T", "_T", "NONE_PRIM", "NONE", "_T", "_T", "_T"),
  MATERNAL_AGE = c(rep("_T", 4), "Y0T19", "_T", "_T"),
  BIRTH_INTERVAL = c(rep("_T", 5), "INTERVAL_LT_M24", "_T"),
  BIRTH_ORDER = c(rep("_T", 6), "ORD1")
)
assembly <- expressions[[which(vapply(expressions, assigns, logical(1), name = "agg_wide"))]]
eval(assembly, test_env)
expected <- c("AGG_RESIDENCE", "AGG_WEALTH", "AGG_EDU", "AGG_EDU",
              "AGG_MOTHERAGE", "AGG_BIRTHINTERVAL", "AGG_BIRTHORDER")
if (!identical(test_env$agg_wide$REF_AREA, expected)) {
  stop("Aggregate REF_AREA must use the agreed dimension-specific AGG_* codes, not WORLD.")
}
stopifnot(identical(test_env$agg_wide$OBS_VALUE, test_env$agg$Median),
          identical(test_env$agg_wide$n_countries, test_env$agg$n_countries))
test_env$agg$Label[1] <- "unknown"
err <- tryCatch({ eval(assembly, test_env); NULL }, error = identity)
stopifnot(inherits(err, "error"), grepl("Unmapped aggregate", conditionMessage(err)))
cat("PASS: six aggregate area codes, both education groupings, values preserved, unknown label rejected.\n")
