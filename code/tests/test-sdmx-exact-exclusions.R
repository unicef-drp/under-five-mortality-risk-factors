library(data.table)

# Exercise the actual final-export block, without reading production inputs.
expressions <- parse("code/05_make_cme_wide_format.R", encoding = "UTF-8")
assigns <- function(x, name) is.call(x) && identical(x[[1]], as.name("<-")) &&
  identical(x[[2]], as.name(name))
start <- which(vapply(expressions, assigns, logical(1), name = "missing_cols"))
end <- which(vapply(expressions, assigns, logical(1), name = "dup_dims"))
test_env <- new.env(parent = globalenv())
test_env$wide <- data.table(
  row_id = 1:9, OBS_VALUE = c(499.9, 500, 500.1, 999.9, 1000, 1000.1, 9999, 10000, NA_real_),
  SERIES_YEAR = 1000L, STD_ERR = 500
)
original <- copy(test_env$wide)
test_env$data_structure_definition <- names(original)
test_env$out_path <- tempfile(fileext = ".csv")
for (expr in expressions[start:(end - 1L)]) eval(expr, test_env)
stopifnot(
  identical(test_env$wide$row_id, c(1L, 3L, 4L, 6L, 8L, 9L)),
  identical(test_env$wide, original[!OBS_VALUE %in% c(500, 1000, 9999)])
)
stopifnot(file.exists(test_env$excluded_path))
excluded <- fread(test_env$excluded_path)
stopifnot(identical(excluded$row_id, c(2L, 5L, 7L)))
cat("PASS: exactly 500/1000/9999 removed; adjacent values, NA and other fields retained; excluded rows recorded.\n")
