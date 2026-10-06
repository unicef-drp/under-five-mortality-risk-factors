# make_report_tables.R -- derives Tables 1-5 into one Excel workbook.

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
  library(dplyr)
  library(readxl)
})

USERPROFILE <- Sys.getenv("USERPROFILE")
source(file.path(USERPROFILE, "Dropbox/UNICEF Work/profile.R"))
gp <- function(...) file.path(USERPROFILE, ...)

eq_out  <- "output"
cme_dir <- gp("Dropbox/UNICEF Work/Data and charts for websites/Files 2025/CME/Estimates")
lb_file <- file.path(dir_IGME, "2025 Round Estimation/Code/input/data_livebirths.csv")
out_dir <- "tables"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_xlsx <- file.path(out_dir, "An Uneven Chance of Survival - Tables 1-5.xlsx")

Y_MAIN <- 2024.5
IGME_ROUND <- 2025   # estimation round, labels the wealth-U5MR source

latest <- function(dir, pattern) {
  fs <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!length(fs)) stop("No file matching ", pattern, " in ", dir)
  fs[which.max(file.mtime(fs))]
}

# Deaths by country and year
cd <- fread(file.path(cme_dir, "UNIGME2025_Country_Rates_&_Deaths.csv"))
cd <- cd[Sex == "Total"]
death_vec <- function(shortind, year) {
  x <- cd[Shortind == shortind & Year == year]
  setNames(as.numeric(x$Median), x$`ISO.Code`)
}
u5d <- death_vec("Under.five.deaths", Y_MAIN)
nnd <- death_vec("Neonatal.deaths",   Y_MAIN)

# Live births, keyed UN code to ISO3
lbdt   <- fread(lb_file)
lb_iso <- fread(file.path(dir_IGME, "2025 Round Estimation/Code/input/country.info.CME.csv"),
                select = c("ISO3Code", "UNCode"))
lbdt <- merge(lbdt[sex == "b"], lb_iso, by.x = "uncode", by.y = "UNCode")
setnames(lbdt, "ISO3Code", "iso3code")
lbdt <- lbdt[!is.na(iso3code) & iso3code != ""]

# Restrict to the 200 IGME countries
lb_by_year <- function(y) {
  world_country_codes <- unique(cd[
    Sex == "Total" & Year == y & Shortind == "Under.five.deaths", `ISO.Code`
  ])
  if (length(world_country_codes) != 200L) {
    stop("Expected 200 countries in the world birth denominator for ", y)
  }
  x <- lbdt[lbdt$year == y & lbdt$iso3code %in% world_country_codes]
  tapply(x$lb, x$iso3code, sum)
}
lb <- lb_by_year(Y_MAIN)

# World totals from the regional file
rg <- fread(file.path(cme_dir, "UNIGME2025_Region_Rates_&_Deaths.csv"))
rgw <- rg[Region == "World" & Sex == "Total" & Year == Y_MAIN]
W <- list(
  U5D = as.numeric(rgw[Shortind == "Under.five.deaths"]$Median),
  NND = as.numeric(rgw[Shortind == "Neonatal.deaths"]$Median),
  LB  = sum(lb, na.rm = TRUE)
)

ssum <- function(vec, set) sum(vec[intersect(set, names(vec))], na.rm = TRUE)
comma <- function(x) formatC(round(x), format = "d", big.mark = ",")
pct   <- function(x) paste0(round(100 * x), "%")

# Equity pipeline outputs
D <- fread(latest(file.path(eq_out, "country"),  "country-specific-database-cleaned-igme-wealth_.*\\.csv$"))  # wealth = IGME 106
C <- fread(latest(file.path(eq_out, "country"),  "country-specific-database-cleaned_[0-9].*\\.csv$"))          # cleaned set
P <- fread(latest(file.path(eq_out, "country"),  "db-pre-aggregation_.*\\.csv$"))                              # country-level set
S <- fread(latest(file.path(eq_out, "country"),  "db-all-surveys_[0-9].*\\.csv$"))                             # selected surveys
A <- fread(latest(file.path(eq_out, "aggregate"),"deaths-and-births-country_.*\\.csv$"))                       # aggregate set

getset <- function(dt, lab, ind) sort(unique(dt[label == lab & Indicator == ind, `Country.Code`]))

# Dedup P to match the cleaning step
P_dedup <- P %>%
  mutate(Reference.Date = as.numeric(Reference.Date)) %>%
  group_by(label, Country.Code, DataCatalogID, Series.Year, Indicator, Group, Interval) %>%
  mutate(
    max_ref_date = if (all(is.na(Reference.Date))) NA_real_ else max(Reference.Date, na.rm = TRUE),
    all_na_ref   = all(is.na(Reference.Date))
  ) %>%
  filter(all_na_ref | is.na(Reference.Date) | Reference.Date == max_ref_date) %>%
  select(-max_ref_date, -all_na_ref) %>%
  ungroup() %>%
  as.data.table()   # restore data.table class

# C rows plus countries only P has
supplement <- function(lab, ind) {
  c_sub <- C[label == lab & Indicator == ind]
  covered <- unique(c_sub$Country.Code)
  extra <- P_dedup[label == lab & Indicator == ind & !(Country.Code %in% covered)]
  bind_rows(c_sub, extra)
}

# TABLE 3 - aggregate set, one row per dimension
t3_rows <- list(
  list("Wealth quintile", getset(D,"wealth","U5MR"),        getset(D,"wealth","NMR")),
  list("Education",       getset(A,"edu","U5MR"),           getset(A,"edu","NMR")),
  list("Residence",       getset(A,"residence","U5MR"),     getset(A,"residence","NMR")),
  list("Mother's age",    getset(A,"motherage","U5MR"),     getset(A,"motherage","NMR")),
  list("Birth interval",  getset(A,"birthinterval","U5MR"), getset(A,"birthinterval","NMR")),
  list("Birth order",     getset(A,"birthorder","U5MR"),    getset(A,"birthorder","NMR")),
  list("Birth size",      getset(A,"bsize","U5MR"),         getset(A,"bsize","NMR"))
)

build_t3 <- function(r) {
  nm <- r[[1]]; u5 <- r[[2]]; nm2 <- r[[3]]
  same <- setequal(u5, nm2)
  ncount <- if (same) as.character(length(u5)) else sprintf("U5MR: %d\nNMR: %d", length(u5), length(nm2))
  U <- ssum(u5d, u5); N <- ssum(nnd, nm2)
  bU <- ssum(lb, u5); bN <- ssum(lb, nm2)
  births <- if (same) comma(bU) else sprintf("U5MR: %s\nNMR: %s", comma(bU), comma(bN))
  shb    <- if (same) pct(bU/W$LB) else sprintf("U5MR: %s\nNMR: %s", pct(bU/W$LB), pct(bN/W$LB))
  c(nm, ncount, comma(U), comma(N), births, pct(U/W$U5D), pct(N/W$NND), shb)
}
T3 <- do.call(rbind, lapply(t3_rows, build_t3))
T3_head <- c("Equity","Number of countries with survey data","Number of under-five deaths",
             "Number of neonatal deaths","Number of live births",
             "Share of global under-five deaths in 2024","Share of global neonatal deaths in 2024",
             "Share of global live births in 2024")

# TABLE 4 - ethnicity & subnational, fixed country lists
# Subnational (2) inputs - UMIC/HIC, 2025 update
sub2_dir  <- file.path("Subnational", "output")
sub2_supp <- read_excel(file.path(sub2_dir, "subnational database_supplementary.xlsx"))
sub2_eu   <- read_excel(file.path(sub2_dir, "subnational database_eurostat.xlsx"), sheet = "mr")
sub2_eu   <- sub2_eu[sub2_eu$`NUTS level` %in% c("NUTS1", "NUTS2"), ]   # NUTS0 is national
sub2_src  <- read_excel(file.path(sub2_dir, "subnational sources.xlsx"))
eth1 <- c("AFG","AGO","BEN","BFA","CAF","CIV","CMR","COD","COG","ETH","GAB","GHA","GIN","GMB","GNB",
          "GTM","GUY","HND","IND","JOR","KEN","LAO","LBR","MLI","MOZ","MRT","MWI","NER","NGA","NPL",
          "PAK","PHL","PRY","RKS","SEN","SLE","SUR","TCD","TGO","UGA","ZAF","ZMB")                 # 42, LMIC
sub1 <- c("AGO","BDI","BEN","BGD","CMR","ETH","GHA","GIN","HTI","KEN","LAO","LBR","LSO","MDG","MLI",
          "MMR","MRT","MWI","NAM","NGA","NPL","PAK","RWA","SEN","SLE","TCD","TGO","TZA","UGA","ZMB","ZWE") # 31, LMIC
sub2 <- sort(unique(c(
  sub2_eu$ISO3Code,
  sub2_src$ISO3Code[sub2_src$Country %in% unique(sub2_supp$Country)]
)))                                                                                                   # 29, UMIC/HIC

build_t4 <- function(nm, iso) {
  U <- ssum(u5d, iso); N <- ssum(nnd, iso); b <- ssum(lb, iso)
  c(nm, as.character(length(iso)), comma(U), comma(N), comma(b),
    pct(U/W$U5D), pct(N/W$NND), pct(b/W$LB))
}
T4 <- rbind(build_t4("Ethnicity",       eth1),
            build_t4("Subnational (1)", sub1),
            build_t4("Subnational (2)", sub2))
T4_head <- c("Equity","Number of countries with survey data","Number of under-five deaths",
             "Number of neonatal deaths","Number of live births",
             "Share of global under-five deaths in 2024","Share of global neonatal deaths in 2024",
             "Share of global live births in 2024")

# TABLE 2 - distribution of selected survey data
refstats <- function(dt, lab, ind, shift = 0) {
  sub <- dt[label == lab & Indicator == ind]
  rd  <- suppressWarnings(as.numeric(sub$`Reference.Date`)) + shift; rd <- rd[!is.na(rd)]
  list(dp = nrow(sub), sv = length(unique(sub$`Country.Code`)),
       rng = if (!length(rd)) "" else if (min(rd)==max(rd)) sprintf("%g", min(rd)) else sprintf("%.1f–%.1f", min(rd), max(rd)),
       avg = if (length(rd)) sprintf("%.1f", mean(rd)) else "")
}

wU <- refstats(D,"wealth","U5MR", shift = 0.5)   # IGME model year to mid-year
wN <- refstats(D,"wealth","NMR")
re <- refstats(supplement("residence","U5MR"),"residence","U5MR")
ed <- refstats(supplement("edu","U5MR"),"edu","U5MR");   edN <- refstats(supplement("edu","NMR"),"edu","NMR")
bsU<- refstats(supplement("bsize","U5MR"),"bsize","U5MR"); bsN <- refstats(supplement("bsize","NMR"),"bsize","NMR")
bo <- refstats(supplement("birthorder","U5MR"),"birthorder","U5MR")
bi <- refstats(supplement("birthinterval","U5MR"),"birthinterval","U5MR")
ma <- refstats(supplement("motherage","U5MR"),"motherage","U5MR")
DASH <- "–"

range_avg <- function(years) {
  years <- years[!is.na(years)]
  if (!length(years)) return(list(dp = 0, rng = "", avg = ""))
  list(dp = length(years),
       rng = if (min(years) == max(years)) sprintf("%g", min(years)) else sprintf("%.1f–%.1f", min(years), max(years)),
       avg = sprintf("%.1f", mean(years)))
}
mid_year <- function(y) y + 0.5   # bare survey year to mid-year date

# Ethnicity and subnational reference years
eth_dir  <- file.path("input", "ethnicity")
sub_dir  <- gp("Dropbox/UNICEF Work/Disaggregated VR data/2023 update/subnational")

eth1_stats <- range_avg(
  mid_year(fread(file.path(eth_dir, "ethnicity dataset_0522.csv"))$SurveyYear_numeric)
)

sub1_stats <- range_avg(
  read_excel(file.path(sub_dir, "UN-IGME-2023-Subnational-U5MR-and-NMR-database.xlsx"),
             sheet = "U5MR and NMR")$Reference.Date
)

sub2_stats <- range_avg(mid_year(c(
  as.numeric(sub2_supp$Year),
  as.numeric(sub2_eu$TIME_PERIOD)
)))

T2 <- rbind(
 c("Wealth quintile U5MR","poorest, 2nd, 3rd, 4th quintile, wealthiest","UN IGME",
   "Indicator of the economic status of households", wU$rng, wU$avg, wU$dp, wU$sv),
 c("Wealth quintile NMR and 1–59-months mortality rate","poorest, 2nd, 3rd, 4th quintile, wealthiest",
   "DHS, MICS and other national household surveys",
   "Indicator of the economic status of households", wN$rng, wN$avg, wN$dp, wN$sv),
 c("Education","Group 1: no education, primary, secondary, higher.  Group 2: no education/primary, secondary/higher",
   "DHS, MICS, other national household surveys","Highest level of schooling attended or completed",
   ed$rng, ed$avg, ed$dp, ed$sv),
 c("Residence","rural, urban","DHS, MICS, other national household surveys and sample vital registration",
   "Country-specific delimitations at the time of the survey", re$rng, re$avg, re$dp, re$sv),
 c("Mother's age", paste0("<20 / 20",DASH,"29 / 30",DASH,"39 / 40",DASH,"49"),"DHS, MICS, other national household surveys",
   "Mother's age at birth", ma$rng, ma$avg, ma$dp, ma$sv),
 c("Birth interval","<2 / 2 / 3 / 4+ years","DHS, MICS, other national household surveys",
   "Difference in years between birthdate of child and birthdate of preceding child", bi$rng, bi$avg, bi$dp, bi$sv),
 c("Birth order", paste0("1 / 2",DASH,"3 / 4+"),"DHS, MICS, other national household surveys","Birth order",
   bo$rng, bo$avg, bo$dp, bo$sv),
 c("Birth size","small or very small / average or large","DHS, MICS, other national household surveys",
   "Mother's estimate of baby's size at birth",
   sprintf("U5MR: %s\nNMR: %s", bsU$rng, bsN$rng), sprintf("U5MR: %s\nNMR: %s", bsU$avg, bsN$avg),
   sprintf("U5MR: %s\nNMR: %s", bsU$dp, bsN$dp), sprintf("U5MR: %s\nNMR: %s", bsU$sv, bsN$sv)),
 c("Ethnicity","self-reported ethnicity or proxy variables such as language","DHS and MICS",
   "self-reported ethnic affiliation, language, skin colour, caste or tribal group",
   eth1_stats$rng, eth1_stats$avg, as.character(eth1_stats$dp), as.character(length(eth1))),
 c("Subnational (1) low- & middle-income","Admin level 1 and Admin level 2","DHS, MICS, other national household surveys",
   "subnational", sub1_stats$rng, sub1_stats$avg, as.character(sub1_stats$dp), as.character(length(sub1))),
 c("Subnational (2) upper-middle- & high-income","Admin level 1 and Admin level 2","Vital registration",
   "subnational", sub2_stats$rng, sub2_stats$avg, as.character(sub2_stats$dp), as.character(length(sub2)))
)
T2_head <- c("Equity group","Categories","Data sources","Definition",
             "Range of reference years","Average reference year","Number of datapoints","Number of surveys")

# TABLE 1 - country counts by selection step
# Rows 1-5 recount the survey selection steps on the raw equity database; rows
# 6-7 come from the pipeline outputs.
source(file.path("code", "run-config.R"))
cfg <- equity_run_config()
eqdb_dir <- gp("Dropbox/UNICEF Work/Equity database/Databases/Databases 2025")
cmeinfo  <- fread(file.path(dir_IGME, "2025 Round Estimation/Code/input/country.info.CME.csv"))

raw <- rbindlist(lapply(list.files(eqdb_dir, pattern = cfg$db_date_pattern, full.names = TRUE), fread),
                 fill = TRUE, use.names = TRUE)
raw[, Group := sub("_$", "", Group)]
raw[, Group := fcase(
  label == "birthorder"    & Group %in% c("1", "<2"),                   "1",
  label == "birthorder"    & Group %in% c("2", "3"),                    "2-3",
  label == "birthorder"    & Group %in% c("4", "4-5", "6+", "4-6", "7+"), "4+",
  label == "birthinterval" & Group %in% c("<1.5", "1.5-2"),             "<2",
  label == "birthinterval" & Group %in% c("4", "5+"),                   "4+",
  label == "motherage"     & Group == "<20_orig",                       "<20",
  label == "motherage"     & Group == "total_orig",                     "total",
  default = Group)]

# Step 1: IGME-visible U5MR/NMR data added before the cut-off
visible_exceptions <- c("TBALSO2023", "9272", "9273", "1865")  # same exceptions as 01
s1 <- raw[Indicator %in% c("U5MR", "NMR") & !is.na(Estimates) &
            (Visible == 2 | DataCatalogID %in% visible_exceptions) &
            (as.Date(paste0(added, "-01"), format = "%Y-%m-%d") < cfg$db_cutoff_date | label == "edu")]

# Step 2: surveys since 2013 (mid-point of multi-year surveys)
s1[, Average.Date.Of.Survey := (as.numeric(substr(Series.Year, 1, 4)) + as.numeric(substr(Series.Year, nchar(Series.Year) - 3, nchar(Series.Year)))) / 2]
s2 <- s1[Average.Date.Of.Survey >= 2013]

# Step 3: included in IGME; indirect data only AOW 25-29 or TSFB 5-9
s3 <- s2[Inclusion.U5MR == 1 & (!is.na(Interval) | Series.Type == "Indirect")]
s3[, Age.Group.of.Women.or.Time.Since.First.Birth := sub("_$", "", Age.Group.of.Women.or.Time.Since.First.Birth)]
indirect <- s3 %>%
  filter(Series.Type == "Indirect" &
           ((Indirect.Classification == "AOW"  & Age.Group.of.Women.or.Time.Since.First.Birth == "25-29") |
            (Indirect.Classification == "TSFB" & Age.Group.of.Women.or.Time.Since.First.Birth == "5-9")))
s3 <- s3 %>% filter(Series.Type != "Indirect") %>% bind_rows(indirect)
indirect <- s3 %>% filter(Series.Type == "Indirect") %>%
  group_by(label, DataCatalogID) %>% mutate(Reference.Date = first(Reference.Date)) %>% ungroup()
s3 <- s3 %>% filter(Series.Type != "Indirect") %>% bind_rows(indirect)

# Step 4: enough cases - complete groups, no total-only surveys, data quality exclusions
edu_4 <- c("noeducation", "primary", "secondary", "higher")
edu_2 <- c("noedu.primary", "sec.higher")
s4_edu <- s3 %>% filter(label == "edu") %>%
  group_by(DataCatalogID, Indicator, Reference.Date) %>%
  filter(!(all(edu_4 %in% Group) & Group %in% edu_2)) %>%   # 4 groups available: drop the 2-group rows
  filter(!(all(edu_2 %in% Group) & Group %in% edu_4)) %>%
  group_by(DataCatalogID, Indicator) %>%
  filter(Reference.Date == max(Reference.Date)) %>%
  ungroup()

expected <- list(wealth = paste0(1:5, ".quintile"), residence = c("rural", "urban"), bsize = c("small.vsmall", "av.large"),
                 birthorder = c("1", "2-3", "4+"), birthinterval = c("<2", "2", "3", "4+"), motherage = c("20-29", "30-39", "40-49"))
s4_other <- s3 %>% filter(label != "edu") %>%
  group_by(DataCatalogID, Indicator, label, Series.Year, Reference.Date, Interval) %>%
  filter(label == "bsize" | all(expected[[first(label)]] %in% Group)) %>%
  group_by(DataCatalogID, Indicator, label, Series.Year) %>%
  filter(label != "bsize" | all(expected[[first(label)]] %in% Group)) %>%
  mutate(target_interval = if (first(label) == "bsize" & any(Group == "small.vsmall")) first(Interval[Group == "small.vsmall"]) else NA,
         total_count = sum(Group == "total")) %>%
  filter(label != "bsize" | Group != "total" | total_count == 1 | Interval == target_interval) %>%
  ungroup()

s4 <- bind_rows(s4_edu, s4_other) %>%
  group_by(DataCatalogID, label, Series.Year, Indicator) %>%
  filter(label != "bsize" | !all(Group == "total")) %>%
  group_by(DataCatalogID, label, Series.Year, Indicator, Reference.Date, Interval) %>%
  filter(label == "bsize" | !all(Group == "total")) %>%
  ungroup() %>%
  filter(!IGME_Key %in% c("EGY-2015-Health Indicator Survey", "TGO-2017-Malaria Indicator Survey-Indirect",
                          "UGA-2018-2019-Malaria Indicator Survey-Indirect", "CMR-2022-Malaria Indicator Survey-Indirect",
                          "GHA-2019-Malaria Indicator Survey", "GIN-2021-Malaria Indicator Survey-Indirect",
                          "MLI-2021-Malaria Indicator Survey", "SEN-2020-2021-Malaria Indicator Survey",
                          "NER-2021-Malaria Indicator Survey-Indirect"),   # most recent direct survey used instead
         !(label == "edu" & Indicator == "U5MR" & Country.Code %in% c("BFA", "KAZ", "KGZ", "NER", "SLE", "UZB")),
         !(label == "edu" & Indicator == "NMR"  & Country.Code %in% c("KGZ", "NER", "SLE", "UZB")),
         !(label == "birthorder" & Country.Code == "BOL")) %>%
  as.data.table()

# Step 5: drop countries with <10,000 live births and high-income countries
big <- lbdt[year == cfg$lb_ref_year & lb >= 10000, iso3code]
hic <- cmeinfo[WBRegion4 == "High income", ISO3Code]
s5  <- s4[Country.Code %in% big & !Country.Code %in% hic]

# Assemble the table in the report layout
cnt  <- function(dt, lab, ind) length(unique(dt[label == lab & Indicator == ind, `Country.Code`]))
fmt2 <- function(u, n) if (u == n) as.character(u) else sprintf("U5MR: %d\nNMR: %d", u, n)
dims <- c("edu", "residence", "motherage", "birthinterval", "birthorder", "bsize")
t1_row <- function(step, wealth_u5, dt, eth = "/", sub = "/") {
  c(step, wealth_u5, cnt(dt, "wealth", "NMR"),
    sapply(dims, function(l) fmt2(cnt(dt, l, "U5MR"), cnt(dt, l, "NMR"))), eth, sub)
}
n_wealth <- as.character(length(getset(D, "wealth", "U5MR")))
eth_txt  <- sprintf("%d (survey data) and 2 (vital registration data)", length(eth1))  # 2 = Brazil and Australia
T1 <- rbind(
  t1_row("UN IGME estimates or empirical data", n_wealth, s1, eth_txt,
         sprintf("%d (UN IGME estimate) and 47 (vital registration data)", length(sub1))),
  t1_row("Select most recent survey year since 2013", "/", s2),
  t1_row("Exclude surveys with low quality in national data", "/", s3),
  t1_row("Enough cases in each category to allow comparison", "/", s4),
  t1_row("Exclude small countries with less than 10,000 live births and high-income countries where applicable", "/", s5),
  t1_row("Number of countries - country-level", n_wealth, P, eth_txt,
         sprintf("%d (UN IGME estimate) and %d (vital registration data)", length(sub1), length(sub2))),
  t1_row("Number of countries - aggregate", n_wealth, A, "Not applicable", "Not applicable"))
T1_head <- c("Analysis steps", "Wealth quintile U5MR", "Wealth quintile NMR", "Mother's education", "Residence",
             "Mother's age", "Birth interval", "Birth order", "Birth size", "Ethnicity", "Subnational")

# TABLE 5 - survey sources by country
source(file.path(dirname(eq_out), "code/figures and tables/table5_additional_sources.R"))
table5_sources <- table5_additional_sources(dirname(eq_out))
stopifnot(setequal(names(table5_sources$ethnicity), eth1),
          setequal(names(table5_sources$subnational), sub1))
additional_source <- function(sources, iso) {
  if (iso %in% names(sources)) unname(sources[[iso]]) else ""
}
src_lab <- function(iso, lab) {
  sub <- P[`Country.Code` == iso & label == lab & Indicator %in% c("U5MR","NMR")]
  if (!nrow(sub)) return("")
  v <- unique(paste(trimws(sub$`Series.Name`), trimws(sub$`Series.Year`)))
  paste(v, collapse = "; ")
}
countries <- unique(rbind(D[, .(`Country.Name`, `Country.Code`)],
                          P[, .(`Country.Name`, `Country.Code`)]))
extra_codes <- setdiff(union(names(table5_sources$ethnicity), names(table5_sources$subnational)),
                       countries$Country.Code)
countries <- rbind(countries, data.table(Country.Name = extra_codes, Country.Code = extra_codes))
off <- fread(file.path(dir_IGME, "2025 Round Estimation/Code/input/country.info.CME.csv"),
             select = c("ISO3Code", "OfficialName"))
countries <- merge(countries, off, by.x = "Country.Code", by.y = "ISO3Code", all.x = TRUE)
countries[is.na(OfficialName) | OfficialName == "", OfficialName := `Country.Name`]
setorder(countries, OfficialName)
wealth_u5 <- function(iso) { s <- D[`Country.Code`==iso & label=="wealth" & Indicator=="U5MR"]; if(nrow(s)) paste(trimws(s$`Series.Name`[1]), IGME_ROUND) else "" }
wealth_nm <- function(iso) { s <- D[`Country.Code`==iso & label=="wealth" & Indicator=="NMR"]; if(nrow(s)) paste0(unique(paste(trimws(s$`Series.Name`), trimws(s$`Series.Year`))), collapse="; ") else "" }
T5 <- t(sapply(seq_len(nrow(countries)), function(i) {
  iso <- countries$`Country.Code`[i]
  c(countries$OfficialName[i], iso,
    wealth_u5(iso), wealth_nm(iso),
    src_lab(iso,"residence"), src_lab(iso,"edu"), src_lab(iso,"bsize"),
    src_lab(iso,"birthorder"), src_lab(iso,"birthinterval"), src_lab(iso,"motherage"),
    additional_source(table5_sources$ethnicity, iso),
    additional_source(table5_sources$subnational, iso))
}))
T5_head <- c("Country","Country code","Wealth quintile U5MR","Wealth quintile NMR","Residence",
             "Education","Birth size","Birth order","Birth interval","Mother's age",
             "Ethnicity","Subnational LMIC")

# Write workbook
hdrStyle  <- createStyle(fontColour="#FFFFFF", fgFill="#1F4E79", halign="center", valign="center",
                         textDecoration="bold", wrapText=TRUE, border="TopBottomLeftRight", borderColour="#BFBFBF")
bodyStyle <- createStyle(valign="top", wrapText=TRUE, border="TopBottomLeftRight", borderColour="#BFBFBF")
titleStyle<- createStyle(textDecoration="bold", fontSize=12, fontColour="#1F4E79", valign="center", wrapText=TRUE)

wb <- createWorkbook()
add_sheet <- function(name, title, header, body, w1=22, wrest=20) {
  ncol <- length(header)
  addWorksheet(wb, name)
  writeData(wb, name, title, startRow=1, startCol=1)
  mergeCells(wb, name, cols=1:ncol, rows=1); addStyle(wb, name, titleStyle, rows=1, cols=1)
  mat <- rbind(header, body)
  writeData(wb, name, mat, startRow=3, startCol=1, colNames=FALSE)
  addStyle(wb, name, hdrStyle, rows=3, cols=1:ncol, gridExpand=TRUE)
  nb <- nrow(body)
  addStyle(wb, name, bodyStyle, rows=4:(3+nb), cols=1:ncol, gridExpand=TRUE)
  setColWidths(wb, name, cols=1, widths=w1)
  setColWidths(wb, name, cols=2:ncol, widths=wrest)
  freezePane(wb, name, firstActiveRow=4)
}

add_sheet("Table 1", "Table 1. Number of countries with data, by selection criteria and equity dimension", T1_head, T1, w1=30, wrest=14)
add_sheet("Table 2", "Table 2. Distribution of selected survey data", T2_head, T2, w1=30, wrest=24)
add_sheet("Table 3", "Table 3. Countries, under-five/neonatal deaths, live births and global shares (aggregate analysis set)", T3_head, T3, w1=16, wrest=17)
add_sheet("Table 4", "Table 4. Countries, under-five/neonatal deaths, live births and global shares (ethnicity and subnational)", T4_head, T4, w1=16, wrest=17)
add_sheet("Table 5", "Table 5. Data sources by country for selected equity dimensions", T5_head, T5, w1=22, wrest=22)

saveWorkbook(wb, out_xlsx, overwrite=TRUE)
cat("Saved:", out_xlsx, "\n")
cat("World @2024.5  U5D=", comma(W$U5D), " NND=", comma(W$NND), " LB=", comma(W$LB), "\n", sep="")
cat("\nTable 3 (derived):\n"); print(T3, quote=FALSE)
cat("\nTable 4 (derived):\n"); print(T4, quote=FALSE)
