# Builds output/subnational database_eurostat.xlsx from the cached Eurostat pulls.

library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(openxlsx)

IN  <- "input"
OUT <- "output"
AGES_MR <- c("x0", "x1", "x2", "x3", "x4", "x5", "x10", "x15", "x20")

# ---- 1. NUTS classification ----
# NUTS 2024 dropped the UK; its codes come from the 2021 set
read_nuts <- function(yr) {
  read_sf(file.path(IN, "eurostat", sprintf("NUTS_RG_20M_%s_4326.shp", yr),
                    sprintf("NUTS_RG_20M_%s_4326.shp", yr))) %>%
    st_drop_geometry() %>%
    transmute(region.code = NUTS_ID, level = LEVL_CODE, cntr = CNTR_CODE,
              name = if ("NAME_ENGL" %in% names(.)) NAME_ENGL else NAME_LATN,
              iso3 = if ("ISO3_CODE" %in% names(.)) ISO3_CODE else NA_character_)
}
# Liechtenstein excluded
nuts <- bind_rows(read_nuts(2024), read_nuts(2021) %>% filter(cntr == "UK")) %>%
  filter(level <= 2, cntr != "LI") %>%
  distinct(region.code, .keep_all = TRUE) %>%
  mutate(iso3 = ifelse(cntr == "UK", "GBR", iso3))

countries <- nuts %>%
  filter(level == 0) %>%
  transmute(cntr, Country = name, ISO3Code = iso3)

# ---- 2. Deaths and population by age ----
tidy_ages <- function(f, var) {
  read_csv(file.path(IN, "eurostat", f), col_types = cols(.default = "c")) %>%
    transmute(region.code = geo, TIME_PERIOD = as.integer(TIME_PERIOD), sex,
              age = recode(sub("^Y", "", age),
                           "_LT1" = "0", "UNK" = "unknown", "_OPEN" = "open.ended"),
              value = as.numeric(values), var = var)
}
raw <- bind_rows(tidy_ages("demo_r_magec.csv.gz", "dth"),
                 tidy_ages("demo_r_d2jan.csv.gz", "pop")) %>%
  semi_join(nuts, by = "region.code")

# five-year bands, single years 0-4 kept apart
banded <- raw %>%
  mutate(single = suppressWarnings(as.numeric(age)),
         band = case_when(
           age %in% c("TOTAL", "unknown", "open.ended") ~ age,
           single < 5  ~ paste0("x", single),
           !is.na(single) ~ paste0("x", pmin(floor(single / 5) * 5, 95)),
           TRUE ~ NA_character_)) %>%
  filter(!is.na(band)) %>%
  group_by(region.code, TIME_PERIOD, sex, var, band) %>%
  summarise(value = sum(value), .groups = "drop")

pop.dth <- banded %>%
  pivot_wider(names_from = band, values_from = value) %>%
  rename(total = TOTAL) %>%
  mutate(source = "eurostat", Countrycode = substr(region.code, 1, 2),
         date = ifelse(var == "pop", paste0(TIME_PERIOD, "0101"), "")) %>%
  left_join(countries, by = c("Countrycode" = "cntr")) %>%
  select(region.code, TIME_PERIOD, sex, var, total, open.ended, unknown,
         x0:x4, x5, num_range("x", seq(10, 95, 5)), date, source, Countrycode,
         Country, ISO3Code) %>%
  arrange(region.code, TIME_PERIOD, var)

# ---- 3. Age-specific rates ----
# profile.R resolves each user's SharePoint root into dir_IGME
source(file.path(Sys.getenv("USERPROFILE"), "Dropbox/UNICEF Work/profile.R"))
a0 <- read_csv(file.path(dir_IGME, "2026 Round Estimation/Code/input/a0.csv"),
               col_types = cols()) %>%
  select(iso, a0)

# uninhabited regions (Svalbard, extra-regio) would make every rate NaN
inhabited <- pop.dth %>%
  filter(sex == "T", var == "pop", total > 0) %>%
  select(region.code, TIME_PERIOD)

rates <- pop.dth %>%
  filter(sex == "T") %>%
  semi_join(inhabited, by = c("region.code", "TIME_PERIOD")) %>%
  select(region.code, TIME_PERIOD, var, all_of(AGES_MR)) %>%
  pivot_longer(all_of(AGES_MR), names_to = "band") %>%
  pivot_wider(names_from = var, values_from = value) %>%
  mutate(m = dth / pop) %>%
  select(-dth, -pop) %>%
  pivot_wider(names_from = band, values_from = m) %>%
  transmute(region.code, TIME_PERIOD,
            x1M0 = x0, x4M1 = (x1 + x2 + x3 + x4) / 4, x5, x10, x15, x20) %>%
  mutate(across(everything(), ~ replace(., is.nan(.) | is.infinite(.), NA)))

# ---- 4. Reference year per country ----
# full NUTS-1 coverage required, else a country that stops
# reporting subnationally resolves to its national row alone
expected <- nuts %>% filter(level == 1) %>% count(cntr, name = "n1")

picked <- rates %>%
  left_join(select(nuts, region.code, level, cntr), by = "region.code") %>%
  mutate(usable = if_all(c(x1M0, x5, x10, x15, x20), ~ !is.na(.))) %>%
  group_by(cntr, TIME_PERIOD) %>%
  summarise(bad = sum(!usable), got1 = sum(level == 1 & usable), .groups = "drop") %>%
  left_join(expected, by = "cntr") %>%
  filter(bad == 0, got1 == n1) %>%
  group_by(cntr) %>%
  slice_max(TIME_PERIOD, n = 1) %>%
  ungroup() %>%
  select(cntr, TIME_PERIOD)

# ---- 5. Mortality rates ----
mr <- rates %>%
  left_join(select(nuts, region.code, level, cntr), by = "region.code") %>%
  inner_join(picked, by = c("cntr", "TIME_PERIOD")) %>%
  left_join(countries, by = "cntr") %>%
  left_join(a0, by = c("ISO3Code" = "iso")) %>%
  mutate(IMR = x1M0 / (1 + (1 - a0) * x1M0),
         CMR = 4 * x4M1 / (1 + (4 - 1.6) * x4M1),
         U5MR = (1 - (1 - IMR) * (1 - CMR)) * 1000,
         IMR = IMR * 1000, CMR = CMR * 1000,
         X5q5  = 5 * x5  * (1 + (5 - 2.5) * x5),
         X5q10 = 5 * x10 * (1 + (5 - 2.5) * x10),
         X5q15 = 5 * x15 * (1 + (5 - 2.5) * x15),
         X5q20 = 5 * x20 * (1 + (5 - 2.5) * x20),
         `NUTS level` = paste0("NUTS", level), sex = "T") %>%
  select(`NUTS level`, Country, ISO3Code, region.code, TIME_PERIOD, sex,
         IMR, CMR, U5MR, X5q5, X5q10, X5q15, X5q20) %>%
  arrange(Country, `NUTS level`, region.code)

dir.create(OUT, showWarnings = FALSE)
write.xlsx(list(pop.dth = pop.dth, mr = mr),
           file.path(OUT, "subnational database_eurostat.xlsx"))

message(nrow(distinct(mr, Country)), " countries, ", nrow(mr), " regions")

# ---- 6. Restrict outputs to countries qualifying for the report's set ----
# rule: >1 NUTS-1 region with non-zero U5MR *and* non-zero IMR
qualifying <- mr %>%
  filter(`NUTS level` == "NUTS1", U5MR != 0, IMR != 0) %>%
  count(Country, name = "nq") %>% filter(nq > 1) %>% pull(Country)

pop.dth <- filter(pop.dth, Country %in% qualifying)
mr      <- filter(mr,      Country %in% qualifying)

write.xlsx(list(pop.dth = pop.dth, mr = mr),
           file.path(OUT, "subnational database_eurostat.xlsx"))
message("qualifying: ", length(qualifying), " countries, ", nrow(mr), " regions")
