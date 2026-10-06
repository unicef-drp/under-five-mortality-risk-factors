# Rebuild the fertility-risk-factor inputs behind the Figure 18 box from DHS and
# MICS birth histories. Standalone: not part of the 01-07 pipeline, and rerun only
# when the survey extracts change.
#
# The microdata is not in the repo and cannot be redistributed. Download into
# input/ipums/ first:
#   idhs_00001.xml + .dat.gz   IPUMS-DHS birth records, 2013 onwards
#   <CC>BR<vv>FL.zip x 7       DHS birth recodes IPUMS does not carry: AO, CD,
#                              ML, MW, NG, TJ, ZM
#   mics_0001/data + syntax    IPUMS-MICS birth histories, 2012 onwards
#
# Writes the birth-level files input/fertility/dhs database.csv and mics
# database.csv (both gitignored), and the Cox model output to
# output/other/fertility/result.xlsx and result2.xlsx.
#
# input/fertility/result.xlsx, the workbook Figure 18 reads, was assembled by
# hand from those model outputs; this script reproduces the models, not that
# formatting.

rm(list = ls())

suppressPackageStartupMessages({
  library(ipumsr)
  library(data.table)
  library(dplyr)
  library(readr)
  library(survival)
  library(openxlsx)
})

raw_dir <- file.path("input", "ipums")          # downloaded extracts (see header)
out_dir <- file.path("input", "fertility")      # birth-level files
mod_dir <- file.path("output", "other", "fertility")
dir.create(mod_dir, recursive = TRUE, showWarnings = FALSE)


# ---- 1. DHS: IPUMS extract ----

ddi <- read_ipums_ddi(file.path(raw_dir, "idhs_00001.xml"))
dhs <- setDT(read_ipums_micro(ddi))

# age at death in months: KIDAGEDIEDIMP (B7), Child's age at death in month (including imputed)
dhs[KIDAGEDIEDIMP==999, KIDAGEDIEDIMP:=NA]
setnames(dhs, old = "KIDAGEDIEDIMP", new = "B7")

#age of child, KIDCURAGEMO (B19), Current age of child in months
dhs[KIDCURAGEMO==999, KIDCURAGEMO:=NA]
setnames(dhs, old = "KIDCURAGEMO", new = "ageofchild")

#KIDBORD, Child's birth order number
dhs[, KIDBORD := KIDBORD]
dhs[, KIDBORD := cut(KIDBORD, breaks = c(0, 1, 3, 100), right = TRUE)]
levels(dhs$KIDBORD) <- 1:3
dhs[, bhorder_grp:= as.numeric(KIDBORD)]

#calculate fertility factor variables, BIRTHINTPREMO (B11) Length of preceding birth interval, in months
dhs[, BIRTHINTPREMO := as.numeric(BIRTHINTPREMO)]
dhs[BIRTHINTPREMO==999, BIRTHINTPREMO:=NA]
dhs[, BIRTHINTPREMO := cut(BIRTHINTPREMO,
                           breaks = c(0, 24, 36, 48, 10000),
                           labels = 1:4,
                           right = FALSE)]
dhs[, bhinterval_grp:= as.numeric(BIRTHINTPREMO)]
dhs[bhorder_grp == 1, bhinterval_grp := 5]

#mother's age at birth, derived from KIDDOBCMC-DOBCMC
dhs[, B100:=KIDDOBCMC-DOBCMC]
dhs[, B100 := cut(B100, breaks = c(0, 240, 360, 480, 600), right = FALSE)]
levels(dhs$B100) <- 1:4
dhs[, bhmomage_grp:= as.numeric(B100)]

dhs<-dhs %>%
  select(CASEID, SAMPLE, YEAR, B7, ageofchild, bhorder_grp, bhinterval_grp,
         bhmomage_grp) %>%
  rename(sample = SAMPLE)

# Filter to surveys from 2013 onwards
dhs <- dhs[YEAR >= 2013]
dhs[, YEAR := NULL]


# ---- 2. DHS: manually downloaded birth recode files (.DAT + .DCT) ----

# Parse a Stata DCT dictionary file; returns data.frame of name + width
parse_dct <- function(dct_file) {
  lines <- readLines(dct_file, warn = FALSE)
  pattern <- "^\\s+\\S+\\s+(\\S+)\\s+%(\\d+)[Sf]"
  m <- regmatches(lines, regexec(pattern, lines))
  m <- m[lengths(m) > 0]
  data.frame(
    name  = sapply(m, `[`, 2),
    width = as.integer(sapply(m, `[`, 3)),
    stringsAsFactors = FALSE
  )
}

vars_needed <- c("caseid", "b7", "b19", "bord", "b3", "b11", "v011")

manual_zips <- file.path(raw_dir, c(
  "CDBR81FL.zip",
  "ZMBR81FL.zip",
  "NGBR8BFL.zip",
  "TJBR81FL.zip",
  "MLBR8AFL.zip",
  "AOBR81FL.zip",
  "MWBR81FL.zip"
))

dhs_manual_list <- lapply(manual_zips, function(zip_path) {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  utils::unzip(zip_path, exdir = tmp_dir)

  dat_file <- list.files(tmp_dir, pattern = "\\.DAT$", ignore.case = TRUE, full.names = TRUE)
  dct_file <- list.files(tmp_dir, pattern = "\\.DCT$", ignore.case = TRUE, full.names = TRUE)

  dict <- parse_dct(dct_file)
  dict$start <- cumsum(c(1L, head(dict$width, -1L)))
  dict$end   <- dict$start + dict$width - 1L

  sel <- dict[dict$name %in% vars_needed, ]

  col_positions <- fwf_positions(start = sel$start, end = sel$end, col_names = sel$name)

  df <- setDT(read_fwf(dat_file, col_positions = col_positions,
                        col_types = cols(.default = col_character()),
                        show_col_types = FALSE))

  # Convert numeric columns
  for (v in setdiff(vars_needed, "caseid")) suppressWarnings(df[, (v) := as.numeric(get(v))])

  # b7: age at death in months
  df[b7 == 999, b7 := NA]

  # b19: current age of child in months
  df[b19 == 999, b19 := NA]
  setnames(df, "b19", "ageofchild")

  # bord: birth order groups
  df[, bord := cut(bord, breaks = c(0, 1, 3, 100), right = TRUE)]
  levels(df$bord) <- 1:3
  df[, bhorder_grp := as.numeric(bord)]

  # b11: preceding birth interval in months
  df[, b11 := cut(b11, breaks = c(0, 24, 36, 48, 10000), labels = 1:4, right = FALSE)]
  df[, bhinterval_grp := as.numeric(b11)]
  df[bhorder_grp == 1, bhinterval_grp := 5]

  # mother's age at birth (months): b3 - v011
  df[, B100 := b3 - v011]
  df[, B100 := cut(B100, breaks = c(0, 240, 360, 480, 600), right = FALSE)]
  levels(df$B100) <- 1:4
  df[, bhmomage_grp := as.numeric(B100)]

  setnames(df, c("caseid", "b7"), c("CASEID", "B7"))
  df[, sample := tools::file_path_sans_ext(basename(zip_path))]
  out <- df[, .(CASEID, sample, B7, ageofchild, bhorder_grp, bhinterval_grp, bhmomage_grp)]
  # Coerce to integer to match IPUMS column types
  for (v in c("B7", "ageofchild", "bhorder_grp", "bhinterval_grp", "bhmomage_grp")) {
    out[, (v) := as.integer(get(v))]
  }
  out
})

dhs_manual <- rbindlist(dhs_manual_list, use.names = TRUE)
dhs <- rbindlist(list(dhs, dhs_manual), use.names = TRUE, ignore.attr = TRUE)

write.csv(dhs, file.path(out_dir, "dhs database.csv"), row.names=F)


# ---- 3. MICS: IPUMS extract ----

dat_dir    <- file.path(raw_dir, "mics_0001", "data")
syntax_dir <- file.path(raw_dir, "mics_0001", "syntax")

# Variables extracted from each survey
# bhmomage_grp excluded — recomputed downstream from dobbh - dobwm
WANTED_VARS <- c("serial", "cluster", "hhno", "linewm", "linenobh",
                 "doi", "interviewmo", "interviewyr",
                 "dobwm", "dobbh",
                 "bhalive", "bhagedeathunit", "bhagedeathnum",
                 "bhorder_grp", "bhinterval_grp", "weightwm", "urban")

# Parse one .do file into a data.table. "gen VARNAME = RAWCOLNAME" gives the raw
# column for each wanted variable, which is then looked up in the infix block, so
# this works whatever the suffix numbering.
read_mics_dat <- function(do_path, dat_dir) {
  lines <- readLines(do_path, warn = FALSE)

  # 1. Extract infix column positions → named list: rawname → c(start, end)
  infix_idx <- which(grepl("quietly infix", lines))[1]
  using_idx  <- which(grepl('using "data/', lines))[1]
  if (is.na(infix_idx) || is.na(using_idx)) return(NULL)

  col_pat   <- "^\\s+str\\s+(\\S+)\\s+(\\d+)-(\\d+)"
  col_lines <- lines[seq(infix_idx + 1, using_idx - 1)]
  m         <- regmatches(col_lines, regexec(col_pat, col_lines))
  m         <- m[lengths(m) == 4]
  if (length(m) == 0) return(NULL)

  raw_names  <- sapply(m, `[`, 2)
  col_starts <- as.integer(sapply(m, `[`, 3))
  col_ends   <- as.integer(sapply(m, `[`, 4))
  pos_map    <- setNames(
    Map(c, col_starts, col_ends),
    toupper(raw_names)
  )

  # 2a. Parse "gen [type] VARNAME = RAWCOLNAME" statements
  #     Skip entries where the "raw col" is "." or starts with a digit/symbol
  gen_pat  <- "^gen\\s+(?:byte|int|long|float|double|str\\w*\\s+)?(\\w+)\\s*=\\s*(\\S+)"
  gen_lines <- grep("^gen\\s", lines, value = TRUE)
  gen_m     <- regmatches(gen_lines, regexec(gen_pat, gen_lines))
  gen_m     <- gen_m[lengths(gen_m) == 3]
  gen_m     <- gen_m[grepl("^[A-Za-z][A-Za-z0-9_]+$", sapply(gen_m, `[`, 3))]
  var_to_raw <- setNames(
    toupper(sapply(gen_m, `[`, 3)),
    sapply(gen_m, `[`, 2)
  )

  # 2b. Fallback: "replace VARNAME = X if ( RAWCOL == "Y")" for derived variables
  #     (e.g. bhalive, bhagedeathunit, bhorder_grp, bhinterval_grp, urban, interviewmo/yr)
  rep_pat   <- "^\\s*replace\\s+(\\w+)\\s*=.*if\\s*\\(\\s*(\\w+)\\s*=="
  rep_lines <- grep(rep_pat, lines, value = TRUE)
  rep_m     <- regmatches(rep_lines, regexec(rep_pat, rep_lines))
  rep_m     <- rep_m[lengths(rep_m) == 3]
  if (length(rep_m) > 0) {
    rep_vars <- sapply(rep_m, `[`, 2)
    rep_cols <- toupper(sapply(rep_m, `[`, 3))
    first_rep <- !duplicated(rep_vars)
    for (i in which(first_rep)) {
      v <- rep_vars[i]
      if (!v %in% names(var_to_raw)) var_to_raw[v] <- rep_cols[i]
    }
  }

  # 3. For each wanted variable, find its raw column and position
  sel_starts <- integer(0); sel_ends <- integer(0); sel_names <- character(0)
  for (v in WANTED_VARS) {
    raw_col <- var_to_raw[v]
    if (is.na(raw_col) || is.null(pos_map[[raw_col]])) next
    sel_starts <- c(sel_starts, pos_map[[raw_col]][1])
    sel_ends   <- c(sel_ends,   pos_map[[raw_col]][2])
    sel_names  <- c(sel_names,  paste0(v, "_raw"))
  }
  if (length(sel_starts) == 0) { message("  No wanted cols found: ", basename(do_path)); return(NULL) }

  # 4. Locate and read the .dat file
  dat_fname <- regmatches(lines[using_idx],
    regexec('using "data/([^"]+)"', lines[using_idx]))[[1]][2]
  dat_file <- file.path(dat_dir, dat_fname)
  if (!file.exists(dat_file)) { message("  Not found: ", dat_fname); return(NULL) }

  dt <- tryCatch(
    setDT(readr::read_fwf(
      dat_file,
      readr::fwf_positions(sel_starts, sel_ends, col_names = sel_names),
      col_types = paste(rep("c", length(sel_names)), collapse = ""),
      show_col_types = FALSE
    )),
    error = function(e) { message("  Read error in ", basename(do_path), ": ", e$message); NULL }
  )
  if (is.null(dt)) return(NULL)

  # 5. Extract country and year from hardcoded replace statements
  country_val <- as.integer(sub(".*replace country = (\\d+).*", "\\1",
    grep("replace country = \\d+", lines, value = TRUE)[1]))
  year_val    <- as.integer(sub(".*replace year = (\\d+).*", "\\1",
    grep("replace year = \\d+",    lines, value = TRUE)[1]))
  dt[, country := country_val]
  dt[, year    := year_val]

  # 6. Helper: safe numeric conversion
  ns <- function(x) suppressWarnings(as.numeric(trimws(x)))
  has <- function(col) paste0(col, "_raw") %in% names(dt)

  # Sorting variables
  if (has("serial"))    dt[, serial    := ns(serial_raw)]
  if (has("cluster"))   dt[, cluster   := ns(cluster_raw)]
  if (has("hhno"))      dt[, hhno      := ns(hhno_raw)]
  if (has("linewm"))    dt[, linewm    := ns(linewm_raw)]
  if (has("linenobh"))  dt[, linenobh  := ns(linenobh_raw)]
  if (has("weightwm"))  dt[, weightwm  := ns(weightwm_raw)]
  if (has("urban"))     dt[, urban     := ns(urban_raw)]

  # doi: prefer recomputed from interviewyr + interviewmo (CMC), fall back to raw doi
  if (has("interviewyr") && has("interviewmo")) {
    dt[, iyr := ns(interviewyr_raw)]
    dt[, imo := ns(interviewmo_raw)]
    dt[, doi := ifelse(!is.na(iyr) & iyr < 9000 & !is.na(imo) & imo < 90,
                       (iyr - 1900) * 12 + imo,
                       if (has("doi")) ns(doi_raw) else NA_real_)]
    dt[, c("iyr", "imo") := NULL]
  } else if (has("doi")) {
    dt[, doi := ns(doi_raw)]
  }

  # dobwm / dobbh: CMC, 9999+ = NA
  if (has("dobwm")) { dt[, dobwm := ns(dobwm_raw)]; dt[dobwm >= 9990, dobwm := NA_real_] }
  if (has("dobbh")) { dt[, dobbh := ns(dobbh_raw)]; dt[dobbh >= 9990, dobbh := NA_real_] }

  # bhalive: "1"=alive, "2"=dead
  if (has("bhalive"))
    dt[, bhalive := fcase(trimws(bhalive_raw) == "1", 1L,
                          trimws(bhalive_raw) == "2", 0L,
                          default = NA_integer_)]

  # bhagedeathunit / bhagedeathnum
  if (has("bhagedeathunit"))
    dt[, bhagedeathunit := fcase(trimws(bhagedeathunit_raw) == "1", 1L,
                                 trimws(bhagedeathunit_raw) == "2", 2L,
                                 trimws(bhagedeathunit_raw) == "3", 3L,
                                 default = NA_integer_)]
  if (has("bhagedeathnum")) {
    dt[, bhagedeathnum := ns(bhagedeathnum_raw)]
    dt[bhagedeathnum >= 97, bhagedeathnum := NA_real_]
  }

  # bhorder_grp: 1=1st, 2=2-3, 3=4-6, 4=7+
  if (has("bhorder_grp")) {
    dt[, bhorder_grp := ns(bhorder_grp_raw)]
    dt[bhorder_grp >= 8, bhorder_grp := NA_real_]
  }

  # bhinterval_grp: raw "0"=first birth, "1"=<2yr, "2"=2yr, "3"=3yr, "4"=4+yr
  if (has("bhinterval_grp"))
    dt[, bhinterval_grp := fcase(trimws(bhinterval_grp_raw) == "0", 1L,
                                 trimws(bhinterval_grp_raw) == "1", 2L,
                                 trimws(bhinterval_grp_raw) == "2", 3L,
                                 trimws(bhinterval_grp_raw) == "3", 4L,
                                 trimws(bhinterval_grp_raw) == "4", 5L,
                                 default = NA_integer_)]

  # Drop all _raw columns
  drop_cols <- names(dt)[endsWith(names(dt), "_raw")]
  dt[, (drop_cols) := NULL]

  message("  ", basename(do_path), ": ", nrow(dt), " rows  (country=", country_val, ", year=", year_val, ")")
  dt
}

# Read and combine all surveys (MICS5+ only, year >= 2013)
# Filter by year extracted from filename, e.g. "af2022a_bh_mics.do" → 2022
do_files <- sort(list.files(syntax_dir, pattern = "\\.do$", full.names = TRUE))
survey_years <- as.integer(regmatches(basename(do_files),
                                      regexpr("(?<=^[a-z]{2})\\d{4}", basename(do_files), perl = TRUE)))
do_files <- do_files[!is.na(survey_years) & survey_years >= 2012]
message("Reading ", length(do_files), " survey files (year >= 2012)...")

all_dt <- lapply(do_files, function(f) {
  tryCatch(read_mics_dat(f, dat_dir),
           error = function(e) { message("  ERROR in ", basename(f), ": ", e$message); NULL })
})
all_dt <- Filter(Negate(is.null), all_dt)
mics <- rbindlist(all_dt, fill = TRUE)
message("Combined: ", format(nrow(mics), big.mark = ","), " rows, ",
        length(unique(mics$country)), " countries")

# Keep only the most recent survey year per country
mics <- mics[, .SD[year == max(year)], by = country]
message("After latest-year filter: ", format(nrow(mics), big.mark = ","), " rows")

mics <- mics %>%
  arrange(serial, cluster, hhno, linewm, linenobh) %>%
  mutate(CASEID = 1:n())

mics <- setDT(mics)

# Calculate age at death in months
mics[bhagedeathunit == 1, B7 := round(bhagedeathnum / 30.44, 2)]  # Convert days to months
mics[bhagedeathunit == 2, B7 := bhagedeathnum]                    # Already in months
mics[bhagedeathunit == 3, B7 := bhagedeathnum * 12]               # Convert years to months

# Calculate age of child
mics[dobbh == 9999, dobbh := NA]
mics$ageofchild <- mics$doi - mics$dobbh

# Mother's age at birth, derived from DOBBH-DOBWM
mics[, B100 := dobbh - dobwm]
mics[, B100 := cut(B100, breaks = c(0, 240, 360, 480, 600), right = FALSE)]
levels(mics$B100) <- 1:4
mics[, bhmomage_grp := as.numeric(B100)]

# Birthinterval: 1=<2yr, 2=2yr, 3=3yr, 4=4+yr, 5=first birth  (ref = "3 years")
mics <- mics %>%
  mutate(bhinterval_grp = as.numeric(bhinterval_grp)) %>%
  mutate(bhinterval_grp = recode(bhinterval_grp,
                                 `1` = "5", `2` = "1", `3` = "2", `4` = "3", `5` = "4")) %>%
  mutate(bhinterval_grp = relevel(as.factor(bhinterval_grp), ref = "3"))

# Birth order: 1=first birth, 2=2-3, 3=4+
mics[bhorder_grp %in% c(3, 4), bhorder_grp := 3]
mics[bhorder_grp == 8, bhorder_grp := NA]
mics$bhorder_grp <- relevel(as.factor(mics$bhorder_grp), ref = "2")

# Final select and write
mics <- mics %>%
  mutate(sample = paste0(country, "_", year)) %>%
  select(CASEID, sample, B7, ageofchild, bhorder_grp, bhinterval_grp, bhmomage_grp)
write.csv(mics, file.path(out_dir, "mics database.csv"), row.names = FALSE)
message("Done. Written: mics database.csv (", format(nrow(mics), big.mark = ","), " rows)")


# ---- 4. Survival analysis (Cox models) ----

#function for multivariate survival analysis using cox regression model
calculate_cox_results <- function(data, status_var, time_var) {
  cox_model <- coxph(
    Surv(get(time_var), get(status_var)) ~
      as.factor(bhorder_grp) +
      as.factor(bhmomage_grp) +
      I(bhinterval_grp == 1) + I(bhinterval_grp == 2) + I(bhinterval_grp == 4) +
      I(bhinterval_grp == 1 & bhorder_grp == 3) +
      I(bhinterval_grp == 2 & bhorder_grp == 3) +
      I(bhorder_grp == 1 & bhmomage_grp == 1) +
      I(bhorder_grp == 1 & bhmomage_grp == 4) +
      I(bhorder_grp == 3 & bhmomage_grp == 1) +
      I(bhorder_grp == 3 & bhmomage_grp == 4) +
      I(bhinterval_grp == 1 & bhmomage_grp == 1) +
      I(bhinterval_grp == 2 & bhmomage_grp == 1) +
      I(bhinterval_grp == 1 & bhmomage_grp == 4) +
      I(bhinterval_grp == 2 & bhmomage_grp == 4) +
      I(bhinterval_grp == 2 & bhorder_grp == 3 & bhmomage_grp == 1) +
      I(bhinterval_grp == 1 & bhorder_grp == 3 & bhmomage_grp == 4) +
      I(bhinterval_grp == 1 & bhorder_grp == 3 & bhmomage_grp == 1) +
      I(bhinterval_grp == 2 & bhorder_grp == 3 & bhmomage_grp == 4),
    data = data
  )
  summary_model <- summary(cox_model)
  # Extract coefficients and standard errors
  coefficients <- summary_model$coefficients[, "coef"]  # Log(HR)
  se_coefficients <- summary_model$coefficients[, "se(coef)"]  # Standard errors of log(HR)
  hr <- exp(coefficients)  # Hazard Ratios
  # Calculate 95% confidence intervals
  z <- 1.96  # Critical value for 95% confidence
  ci_lower <- exp(coefficients - z * se_coefficients)  # Lower bound of CI
  ci_upper <- exp(coefficients + z * se_coefficients)  # Upper bound of CI
  # Create results data frame
  results <- data.frame(
    Term = rownames(summary_model$coefficients),
    HR = as.numeric(hr),
    CI_Lower = as.numeric(ci_lower),
    CI_Upper = as.numeric(ci_upper)
  )
  return(results)
}

#read in dhs and mics data
dhs<-read.csv(file.path(out_dir, "dhs database.csv")) %>%
  mutate(ID=1:n()) %>%
  select(-CASEID)
message("DHS: ", nrow(dhs), " rows, ", length(unique(dhs$sample)), " unique surveys")
max<-max(dhs$ID)
mics<-read.csv(file.path(out_dir, "mics database.csv")) %>%
  mutate(ID=1:n(), ID=ID+max) %>%
  select(-CASEID)
message("MICS: ", nrow(mics), " rows, ", length(unique(mics$sample)), " unique surveys")
df<-setDT(bind_rows(dhs, mics)) %>%
  rename(CASEID=ID)  #unique identifier for both mics and dhs
message("Combined: ", nrow(df), " rows, ", length(unique(df$sample)), " unique surveys")

df$bhorder_grp <- relevel(as.factor(df$bhorder_grp), ref = "2") #the reference category is second or third birth
df$bhinterval_grp <- relevel(as.factor(df$bhinterval_grp), ref = "3") #the reference category is 3 years birth interval
df$bhmomage_grp <- relevel(as.factor(df$bhmomage_grp), ref = "2")  #the reference category is age 20-29

# single, double and triple risks
df$singlerisk<-ifelse(df$bhorder_grp %in% c(1, 3) | df$bhinterval_grp %in% c(1, 2) | df$bhmomage_grp %in% c(1, 4), 1, 0)
df$doublerisk <- ifelse(
  (df$bhorder_grp %in% c(1, 3)) +
    (df$bhinterval_grp %in% c(1, 2)) +
    (df$bhmomage_grp %in% c(1, 4)) >= 2,
  1, 0
)
df$threewayrisk<-ifelse(df$bhorder_grp %in% c(1, 3) & df$bhinterval_grp %in% c(1, 2) & df$bhmomage_grp %in% c(1, 4), 1, 0)

#cox survival analysis_u5, right censored
df$u5mr_status<-ifelse(!is.na(df$B7) & df$B7 < 60, 1, 0)
df$u5mr_time <- df$B7
df[is.na(B7), u5mr_time:= ageofchild]
df[u5mr_time>60, u5mr_time:=60]

results_u5 <- calculate_cox_results(
  data=df, status_var = "u5mr_status",
  time_var = "u5mr_time"
)

#1-59 month mortality, risk of dying 1-59 month, right-censored
# Define child mortality status (1 = died 1-59 month, 0 = survived until 5)
#filter only the subset of population who survived until age 1
mr1t59<-filter(df, is.na(B7) | B7>1)
mr1t59$mr1t59_time <- ifelse(
  !is.na(mr1t59$B7) & mr1t59$B7 < 60, mr1t59$B7, 60)
mr1t59$mr1t59_status <- ifelse(
  !is.na(mr1t59$B7) & mr1t59$B7 >= 1 & mr1t59$B7 < 60, 1, 0)
results_mr1t59 <- calculate_cox_results(
  data=mr1t59, status_var = "mr1t59_status",
  time_var = "mr1t59_time"
)

#cox survival analysis_nmr, right censored
df$nmr_status<-ifelse(!is.na(df$B7) & df$B7 < 1, 1, 0)
df$nmr_time <- df$B7
df[is.na(B7), nmr_time:= ageofchild]
df[nmr_time>1, nmr_time:=1]

results_nmr <- calculate_cox_results(
  status_var = "nmr_status",
  time_var = "nmr_time", data=df
)

combined_results <- cbind(results_u5, results_mr1t59, results_nmr)
write.xlsx(combined_results, file.path(mod_dir, "result.xlsx"), rowNames = F)

#how does number of risks affect mortality?
numberofrisk <- function(data, status_var, time_var) {
  cox_model <- coxph(
    Surv(get(time_var), get(status_var)) ~
      I(bhorder_grp==1)+singlerisk+doublerisk+threewayrisk,
    data = data
  )
  summary_model <- summary(cox_model)
  # Extract coefficients and standard errors
  coefficients <- summary_model$coefficients[, "coef"]  # Log(HR)
  se_coefficients <- summary_model$coefficients[, "se(coef)"]  # Standard errors of log(HR)
  hr <- exp(coefficients)  # Hazard Ratios
  # Calculate 95% confidence intervals
  z <- 1.96  # Critical value for 95% confidence
  ci_lower <- exp(coefficients - z * se_coefficients)  # Lower bound of CI
  ci_upper <- exp(coefficients + z * se_coefficients)  # Upper bound of CI
  # Create results data frame
  results <- data.frame(
    Term = rownames(summary_model$coefficients),
    HR = as.numeric(hr),
    CI_Lower = as.numeric(ci_lower),
    CI_Upper = as.numeric(ci_upper)
  )
  return(results)
}

numberofrisk_u5 <- numberofrisk(
  data=df, status_var = "u5mr_status",
  time_var = "u5mr_time"
)

numberofrisk_nmr <- numberofrisk(
  data=df, status_var = "nmr_status",
  time_var = "nmr_time"
)

numberofrisk_mr1t59 <- numberofrisk(
  data=mr1t59, status_var = "mr1t59_status",
  time_var = "mr1t59_time"
)
combined_results2 <- cbind(numberofrisk_u5, numberofrisk_mr1t59, numberofrisk_nmr)

write.xlsx(combined_results2, file.path(mod_dir, "result2.xlsx"), rowNames = F)
message("Done. Model output written to ", mod_dir)
