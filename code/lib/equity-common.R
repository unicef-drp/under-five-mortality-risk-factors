# Shared preparation only: no visibility, income, year or survey selection here.
read_equity_db <- function(db_dir, date_pattern) {
  files <- list.files(db_dir, pattern = paste0("^data_.*", date_pattern), full.names = TRUE)
  if (length(files) == 0)
    stop("No equity database files found matching pattern: ", date_pattern)
  message("Reading ", length(files), " equity database file(s) ...")
  data.table::rbindlist(lapply(files, data.table::fread), fill = TRUE, use.names = TRUE)
}

normalize_equity_groups <- function(x) {
  x <- data.table::copy(x)
  x$Group <- sub("_$", "", x$Group)
  x <- dplyr::mutate(x, Group = dplyr::case_when(
    Group %in% c("1", "<2") & label == "birthorder" ~ "1",
    Group %in% c("2", "3") & label == "birthorder" ~ "2-3",
    Group %in% c("4", "4-5", "6+", "4-6", "7+") & label == "birthorder" ~ "4+",
    Group %in% c("<1.5", "1.5-2") & label == "birthinterval" ~ "<2",
    Group %in% c("4", "5+") & label == "birthinterval" ~ "4+",
    Group == "<20_orig" & label == "motherage" ~ "<20",
    Group == "total_orig" & label == "motherage" ~ "total",
    TRUE ~ Group
  ))
  data.table::as.data.table(x)
}
