# Birth-weighted global aggregates by equity dimension, benchmarked to the UN
# IGME 2024 national estimates.
#
# Inputs: country-specific-database-cleaned_<date>.csv and
# country-specific-nbirth_<date>.csv from 01, plus WPP live births, IGME
# national rates and the IGME wealth quintile files.
#
# Outputs, in output/aggregate/:
#   deaths-and-births-country_<date>.csv  per-country weighted rates, births and deaths
#   aggregate_<date>.csv                  aggregates by dimension, indicator and group
#   aggregate-country-count_<date>.csv    countries behind each aggregate
#
# Wealth is treated differently from the other dimensions: NMR is benchmarked to
# the IGME quintile projections, and U5MR comes from the IGME world aggregate
# rather than from surveys.

rm(list = ls())


# ---- 0. Libraries and paths ----

library(dplyr)
library(tidyr)
library(data.table)

USERPROFILE <- Sys.getenv("USERPROFILE")

# profile.R sets dir_IGME
source(file.path(USERPROFILE, "Dropbox/UNICEF Work/profile.R"))
source("code/run-config.R")
cfg <- equity_run_config()
output_dir <- cfg$output_dir

# Data vintage, from run-config.R
RUN_DATE    <- cfg$run_date
LB_REF_YEAR <- cfg$lb_ref_year
IGME_YEAR   <- cfg$igme_year
IGME_RATES_DATE <- cfg$igme_rates_date

# External sources
lb_path <- file.path(dir_IGME,
  "2025 Round Estimation/Code/input/data_livebirths.csv")

rates_path <- file.path(dir_IGME,
  paste0("2025 Round Estimation/Code/Aggregate results (final) ",
         IGME_RATES_DATE, "/Rates & Deaths_Country Summary.csv"))

wealth_country_path <- file.path(USERPROFILE,
  "Dropbox/IGME Wealth/aggregate/IGME2025_U5MR_wealth_quintile_projection_country.csv")

wealth_world_path <- file.path(USERPROFILE,
  "Dropbox/IGME Wealth/aggregate/IGME2025_U5MR_wealth_quintile_projection_world.csv")

# Outputs of 01
db_path     <- file.path(output_dir, "country",
  paste0("country-specific-database-cleaned_", RUN_DATE, ".csv"))
nbirth_path <- file.path(output_dir, "country",
  paste0("country-specific-nbirth_", RUN_DATE, ".csv"))

# Output directory
dir_aggregate <- file.path(output_dir, "aggregate")
if (!dir.exists(dir_aggregate)) dir.create(dir_aggregate, recursive = TRUE)


# ---- 1. Reference data ----

cat("=== 1. Reference data ===\n")

# WPP live births: the common base each survey is weighted to
cat("Loading WPP live births...\n")
lb_wpp <- read.csv(lb_path, as.is = TRUE) %>%
  filter(year == LB_REF_YEAR) %>%
  select(uncode, lb)
cat(sprintf("  %d countries\n", nrow(lb_wpp)))

# IGME national medians: the benchmark for survey ratios
cat("Loading IGME national medians...\n")
rates_igme <- read.csv(rates_path, as.is = TRUE) %>%
  filter(X == "Median") %>%
  select(ISO3Code, U5MR.2024, NMR.2024) %>%
  pivot_longer(c(U5MR.2024, NMR.2024),
               names_to  = "Shortind",
               values_to = "Median") %>%
  mutate(Shortind = ifelse(Shortind == "U5MR.2024", "U5MR", "NMR"),
         Median   = as.numeric(Median))
cat(sprintf("  %d country × indicator rows\n", nrow(rates_igme)))

# IGME wealth quintile projections: the benchmark for wealth NMR
cat("Loading IGME wealth quintile country projections...\n")
unigme_wealth_U5MR <- read.csv(wealth_country_path, as.is = TRUE) %>%
  filter(scenario == "UN IGME estimate", Year == IGME_YEAR) %>%
  mutate(Quintile = paste0(Quintile, ".quintile")) %>%
  select(ISO3Code, Quintile, U5MR.proj)
cat(sprintf("  %d country × quintile rows\n", nrow(unigme_wealth_U5MR)))


# ---- 2. Outputs of 01 ----

cat("\n=== 2. Script-01 outputs ===\n")

# U5MR and NMR only. Companion two-group education rows from four-group surveys
# are dropped here; section 9 recombines them without double counting.
cat("Loading cleaned country-specific database...\n")
db_all_group <- fread(db_path, data.table = FALSE) %>%
  filter(
    Indicator %in% c("NMR", "U5MR"),
    is.na(category) | !category %in% c("recalculated edu 2 groups from 4 groups",
                                      "source edu 2 groups alongside 4 groups",
                                      "derived edu 2 groups alongside 4 groups")
  ) %>%
  mutate(
    DataCatalogID = as.character(DataCatalogID),
    Estimates     = as.numeric(Estimates)
  )
cat(sprintf("  %d rows after filter\n", nrow(db_all_group)))
cat("  Rows by label × Indicator:\n")
print(table(db_all_group$label, db_all_group$Indicator))

# Births per survey and group, used as weights
cat("\nLoading nbirth...\n")
nbirth <- fread(nbirth_path, data.table = FALSE) %>%
  mutate(DataCatalogID = as.character(DataCatalogID),
         Estimates     = as.numeric(Estimates))
cat(sprintf("  %d rows\n", nrow(nbirth)))

# A survey joins a dimension's birth-weighted aggregate only if its birth counts
# cover the whole group set. prop.birth divides by the births that are present,
# so a missing group renormalises the weights and distorts them. El Salvador ENS
# 2021 birth interval and Yemen MICS 2022-2023 birth order are dropped here;
# their rates stay in the cleaned file for the country-level ratios.
nbirth_expected <- list(
  birthinterval = c("<2", "2", "3", "4+"),
  birthorder    = c("1", "2-3", "4+"),
  motherage     = c("<20", "20-29", "30-39", "40-49"),
  residence     = c("rural", "urban"),
  wealth        = c("1.quintile", "2.quintile", "3.quintile", "4.quintile", "5.quintile")
)
nbirth_present <- nbirth %>% filter(Group != "total") %>% distinct(DataCatalogID, label, Group)
incomplete_std <- nbirth_present %>%
  filter(label %in% names(nbirth_expected)) %>%
  group_by(DataCatalogID, label) %>%
  summarise(ok = all(nbirth_expected[[label[1]]] %in% Group), .groups = "drop") %>%
  filter(!ok) %>% select(DataCatalogID, label)
incomplete_edu <- nbirth_present %>%
  filter(label == "edu") %>%
  group_by(DataCatalogID, label) %>%
  summarise(ok = all(c("noeducation","primary","secondary","higher") %in% Group) |
                 all(c("noedu.primary","sec.higher") %in% Group) |
                 all(c("noeducation","primary","sec.higher") %in% Group),
            .groups = "drop") %>%
  filter(!ok) %>% select(DataCatalogID, label)
nbirth_incomplete <- bind_rows(incomplete_std, incomplete_edu)
if (nrow(nbirth_incomplete) > 0) {
  cat(sprintf("  Excluding %d DataCatalogID x label combo(s) with incomplete nbirth from the aggregate:\n",
              nrow(nbirth_incomplete)))
  print(as.data.frame(nbirth_incomplete %>%
          left_join(distinct(nbirth, DataCatalogID, Country.ISO), by = "DataCatalogID")))
  nbirth <- nbirth %>% anti_join(nbirth_incomplete, by = c("DataCatalogID", "label"))
}


# ---- 3. Birth proportions and weighted births ----

cat("\n=== 3. Birth proportions ===\n")

# prop.birth = group births / non-total group births within survey and dimension
# wgt.birth  = prop.birth * national WPP live births

nbirth_all_group <- nbirth %>%
  left_join(lb_wpp, by = c("Country.ISO" = "uncode")) %>%
  distinct(across(-c(Estimates, date_added)), .keep_all = TRUE) %>%
  group_by(label, DataCatalogID) %>%
  mutate(
    prop.birth = if_else(
      Group == "total", 1,
      Estimates / sum(Estimates[Group != "total"])
    ),
    wgt.birth = prop.birth * lb
  ) %>%
  ungroup()

cat(sprintf("  nbirth_all_group: %d rows\n", nrow(nbirth_all_group)))

# 3b. Education two-group birth weights.
# Some surveys report education rates in two groups but birth counts in four
# (e.g. Jordan DHS 2017 NMR). Sum the four-group proportions into pairs, and only
# where no native two-group row exists, so nothing is double counted.
edu_already_2group <- nbirth_all_group %>%
  filter(label == "edu", Group %in% c("noedu.primary", "sec.higher")) %>%
  distinct(DataCatalogID)
edu_2group_bw <- nbirth_all_group %>%
  filter(label == "edu", Group %in% c("noeducation", "primary", "secondary", "higher")) %>%
  anti_join(edu_already_2group, by = "DataCatalogID") %>%
  mutate(Group = if_else(Group %in% c("noeducation", "primary"), "noedu.primary", "sec.higher")) %>%
  group_by(DataCatalogID, label, Group) %>%
  summarise(prop.birth = sum(prop.birth), wgt.birth = sum(wgt.birth), .groups = "drop")
nbirth_all_group <- bind_rows(nbirth_all_group, edu_2group_bw)
cat(sprintf("  edu 2-group birth weights derived for %d survey(s)\n",
            dplyr::n_distinct(edu_2group_bw$DataCatalogID)))


# ---- 4. Wealth NMR, benchmarked to the IGME quintile projections ----

cat("\n=== 4. Wealth NMR benchmark ===\n")

# wealth NMR = (survey NMR / survey U5MR) * IGME quintile U5MR; for the total
# group, that ratio times the IGME national median. Kosovo has no quintile
# estimates and keeps its survey rate. Wealth's own total row is excluded
# because the canonical label = "total" row covers it.
rks_wealth_nmr <- db_all_group %>%
  filter(Country.Code == "RKS", label %in% c("wealth", "total"), Indicator == "NMR",
         !(label == "wealth" & Group == "total")) %>%
  mutate(wgt.rate = Estimates, label = "wealth")

U5MR_national <- rates_igme %>% filter(Shortind == "U5MR")

wealth_NMR <- db_all_group %>%
  filter(Indicator %in% c("NMR", "U5MR"), label %in% c("wealth", "total"),
         !(label == "wealth" & Group == "total")) %>%
  select(Country.Code, DataCatalogID, Group, Interval,
         Series.Name, Series.Year, Series.Type, Series.Category,
         Average.Date.Of.Survey, Reference.Date, Indicator, Estimates) %>%
  pivot_wider(
    names_from   = Indicator,
    values_from  = Estimates,
    values_fn    = first   # guard against any duplicate rows
  ) %>%
  filter(!is.na(NMR), !is.na(U5MR)) %>%
  mutate(ratio = NMR / U5MR) %>%
  left_join(unigme_wealth_U5MR,
            by = c("Country.Code" = "ISO3Code", "Group" = "Quintile")) %>%
  left_join(U5MR_national %>% select(ISO3Code, Median),
            by = c("Country.Code" = "ISO3Code")) %>%
  mutate(
    wgt.rate  = if_else(Group == "total", ratio * Median, ratio * U5MR.proj),
    Indicator = "NMR",
    label     = "wealth"
  ) %>%
  # Kosovo re-added with its unadjusted survey rate
  filter(Country.Code != "RKS") %>%
  bind_rows(rks_wealth_nmr)

cat(sprintf("  wealth_NMR: %d rows, %d countries\n",
            nrow(wealth_NMR),
            n_distinct(wealth_NMR$Country.Code)))


# ---- 5. Other dimensions, benchmarked to the IGME national estimates ----

cat("\n=== 5. All other equity/indicator benchmark ===\n")

# wgt.rate = (group rate / dimension total rate) * IGME national median, so each
# dimension uses its own total as the denominator.
canonical_totals_for_ratio <- db_all_group %>%
  filter(Group == "total", label != "total") %>%
  select(DataCatalogID, label, Indicator, total_est = Estimates)

dt <- db_all_group %>%
  filter(
    Group != "total",                               # exclude dimension-specific total rows
    label != "total",                               # exclude canonical total rows
    !(label == "wealth" & Indicator == "NMR")        # wealth NMR handled in section 4
  ) %>%
  left_join(
    rates_igme %>% select(ISO3Code, Shortind, Median),
    by = c("Country.Code" = "ISO3Code", "Indicator" = "Shortind")
  ) %>%
  left_join(canonical_totals_for_ratio,
            by = c("DataCatalogID", "label", "Indicator")) %>%
  mutate(
    ratio    = Estimates / total_est,
    wgt.rate = ratio * Median
  ) %>%
  bind_rows(wealth_NMR)   # append wealth NMR (benchmarked differently above)

cat(sprintf("  dt after benchmark: %d rows\n", nrow(dt)))
cat("  Rows by label × Indicator:\n")
print(table(dt$label, dt$Indicator))


# ---- 6. Join births, compute weighted deaths ----

cat("\n=== 6. Join nbirth and compute weighted deaths ===\n")

# Join on survey, dimension and group, taking only the two weight columns to
# avoid column clashes.

dt <- dt %>%
  left_join(
    nbirth_all_group %>% select(DataCatalogID, label, Group, prop.birth, wgt.birth),
    by = c("DataCatalogID", "label", "Group")
  ) %>%
  mutate(wgt.death = wgt.birth * wgt.rate / 1000) %>%
  filter(!is.na(wgt.birth))

cat(sprintf("  dt after nbirth join: %d rows, %d countries\n",
            nrow(dt), n_distinct(dt$Country.Code)))
cat("  Rows by label × Indicator × Group (sample):\n")
print(table(dt$label, dt$Indicator))

write.csv(dt,
          file.path(dir_aggregate, paste0("deaths-and-births-country_", RUN_DATE, ".csv")),
          row.names = FALSE)
cat(sprintf("  Saved: aggregate/deaths-and-births-country_%s.csv\n", RUN_DATE))


# ---- 7. Country counts ----

cat("\n=== 7. Country counts ===\n")

# Countries contributing at least one non-total group to the aggregate

country_count <- dt %>%
  filter(
    !is.na(wgt.rate),
    !is.na(wgt.death),
    Group != "total"
  ) %>%
  group_by(label, Indicator) %>%
  summarise(n_countries = n_distinct(Country.Code), .groups = "drop") %>%
  # Wealth U5MR comes from the IGME model (section 12), not from surveys
  bind_rows(data.frame(
    label       = "wealth",
    Indicator   = "U5MR (IGME model)",
    n_countries = n_distinct(unigme_wealth_U5MR$ISO3Code)
  )) %>%
  arrange(label, Indicator)

cat("Countries per label × Indicator:\n")
print(country_count, n = 30)


# ---- 8. Aggregate by dimension, indicator and group ----

cat("\n=== 8. Aggregate ===\n")

# Birth-weighted aggregate: sum(deaths) / sum(births) × 1000
agg <- dt %>%
  filter(!is.na(wgt.rate), !is.na(wgt.death), Group != "total") %>%
  group_by(label, Indicator, Group) %>%
  summarise(
    wgt.rate  = sum(wgt.death) / sum(wgt.birth) * 1000,
    wgt.birth = sum(wgt.birth),
    wgt.death = sum(wgt.death),
    .groups   = "drop"
  )

cat(sprintf("  agg: %d rows\n", nrow(agg)))


# ---- 9. Combined education groups ----

cat("\n=== 9. Combined education groups ===\n")

# Collapse the four groups into two across all countries at once: noeducation +
# primary and secondary + higher, tagged "combined all countries".

agg_edu_combined <- agg %>%
  filter(label == "edu") %>%
  mutate(
    Group = case_when(
      Group %in% c("noeducation", "primary")   ~ "noedu.primary",
      Group %in% c("secondary",   "higher")    ~ "sec.higher",
      TRUE                                      ~ Group
    )
  ) %>%
  group_by(Group, Indicator) %>%
  summarise(
    wgt.birth = sum(wgt.birth),
    wgt.death = sum(wgt.death),
    wgt.rate  = sum(wgt.death) / sum(wgt.birth) * 1000,
    .groups   = "drop"
  ) %>%
  mutate(type  = "combined all countries",
         label = "edu")


# ---- 10. Totals across groups ----

agg_total <- agg %>%
  group_by(label, Indicator) %>%
  summarise(
    wgt.birth = sum(wgt.birth),
    wgt.death = sum(wgt.death),
    wgt.rate  = sum(wgt.death) / sum(wgt.birth) * 1000,
    .groups   = "drop"
  ) %>%
  mutate(Group = "total")

agg <- bind_rows(agg, agg_edu_combined, agg_total)


# ---- 11. MR1t59 from the U5MR and NMR aggregates ----

cat("\n=== 11. MR1t59 ===\n")

# MR1t59 = 1000 * (1 - (1 - U5MR/1000) / (1 - NMR/1000)).
# For birth size the U5MR aggregate covers 7 countries and the NMR aggregate 40,
# so chaining them mixes two populations. For this chain only, NMR is recomputed
# on the same 7 countries, matching the Figure 27 note. The standalone NMR rate
# is unchanged.

bsize_u5mr_countries <- dt %>%
  filter(label == "bsize", Indicator == "U5MR") %>%
  pull(Country.Code) %>% unique()

bsize_nmr_dt <- dt %>%
  filter(label == "bsize", Indicator == "NMR", Country.Code %in% bsize_u5mr_countries)

bsize_nmr_matched <- bind_rows(
  bsize_nmr_dt %>% group_by(label, Group) %>%
    summarise(wgt.rate_matched = sum(wgt.death) / sum(wgt.birth) * 1000, .groups = "drop"),
  bsize_nmr_dt %>% summarise(wgt.rate_matched = sum(wgt.death) / sum(wgt.birth) * 1000) %>%
    mutate(label = "bsize", Group = "total")
)

cat(sprintf("  Birth size MR1t59 chain: NMR restricted to the %d U5MR countries (was 40)\n",
            length(bsize_u5mr_countries)))
print(bsize_nmr_matched)

agg_for_mr1t59 <- agg %>%
  left_join(bsize_nmr_matched, by = c("label", "Group")) %>%
  mutate(wgt.rate = if_else(label == "bsize" & Indicator == "NMR" & !is.na(wgt.rate_matched),
                             wgt.rate_matched, wgt.rate)) %>%
  select(-wgt.rate_matched)

mr1t59 <- agg_for_mr1t59 %>%
  group_by(label, Group, type) %>%
  summarise(
    wgt.rate  = 1000 * (1 - (1 - wgt.rate[Indicator == "U5MR"] / 1000) /
                              (1 - wgt.rate[Indicator == "NMR"]  / 1000)),
    wgt.death = wgt.death[Indicator == "U5MR"] - wgt.death[Indicator == "NMR"],
    .groups   = "drop"
  ) %>%
  mutate(Indicator = "MR1t59")

cat(sprintf("  MR1t59: %d rows\n", nrow(mr1t59)))


# ---- 12. Wealth U5MR from the IGME world aggregate ----

cat("\n=== 12. Wealth U5MR from IGME world aggregate ===\n")

# The modelled world aggregate replaces survey-based wealth U5MR entirely

# Keep the whole 2000-2024 series for the analysis app's year slider. Year is
# stamped mid-year, as for every other row.
wealth_U5MR_world <- read.csv(wealth_world_path, as.is = TRUE) %>%
  filter(scenario == "UN IGME estimate", Year >= 2000, Year <= IGME_YEAR) %>%
  mutate(
    label     = "wealth",
    Indicator = "U5MR",
    Quintile  = paste0(Quintile, ".quintile"),
    Year      = Year + 0.5
  ) %>%
  rename(Group = Quintile, wgt.rate = value, wgt.death = dths, wgt.birth = lbs) %>%
  select(label, Indicator, Group, Year, wgt.rate, wgt.birth, wgt.death)

cat(sprintf("  Wealth U5MR world aggregate: %d rows (%d years)\n",
            nrow(wealth_U5MR_world), n_distinct(wealth_U5MR_world$Year)))

# Other rows take Year = IGME_YEAR + 0.5; wealth U5MR keeps its own year
agg <- agg %>%
  filter(!(label == "wealth" & Indicator == "U5MR")) %>%
  bind_rows(wealth_U5MR_world, mr1t59) %>%
  mutate(Year = ifelse(is.na(Year), IGME_YEAR + 0.5, Year))


# ---- 13. Labels and outputs ----

cat("\n=== 13. Labels and output ===\n")

# Display names for dimensions
equity_labels <- c(
  wealth        = "Wealth quintile",
  residence     = "Residence",
  edu           = "Mother's education",
  bsize         = "Birth size",
  birthorder    = "Birth order",
  birthinterval = "Birth interval",
  motherage     = "Mother's age"
)

# Display labels for groups
group_labels <- data.frame(
  Group = c(
    "1.quintile",  "2.quintile", "3.quintile", "4.quintile", "5.quintile",
    "rural",       "urban",
    "noeducation", "primary",    "secondary",  "higher",
    "noedu.primary", "sec.higher",
    "small.vsmall", "av.large",
    "1",  "2-3",  "4+",
    "<2", "2",    "3",   ">=4",
    "<20", "20-29", "30-39", "40-49",
    "total"
  ),
  label = c(
    "Poorest",        "2nd quintile",  "3rd quintile",    "4th quintile",  "Wealthiest",
    "Rural",          "Urban",
    "No education",   "Primary",       "Secondary",       "Higher",
    "No education or primary",        "Secondary or higher",
    "Small or very small",            "Average or large",
    "First", "Second or third", "Fourth or higher",
    "Less than 2 years", "2 years", "3 years", "4 years or more",
    "<20", "20-29", "30-39", "40-49",
    "Total"
  ),
  stringsAsFactors = FALSE
)

# Birth interval "4+" is ">=4" in the label table
aggregate_final <- agg %>%
  rename(equity = label) %>%
  mutate(
    Group  = ifelse(Group == "4+" & equity == "birthinterval", ">=4", Group),
    equity = recode(equity, !!!equity_labels)
  ) %>%
  left_join(group_labels, by = "Group") %>%
  # Tag education rows as 4 groups or 2 groups; blank elsewhere
  mutate(
    type = case_when(
      equity == "Mother's education" &
        Group %in% c("noeducation", "primary", "secondary", "higher") ~ "4 groups",
      equity == "Mother's education" &
        Group %in% c("noedu.primary", "sec.higher") & is.na(type)     ~ "2 groups",
      is.na(type)                                                       ~ "",
      TRUE                                                              ~ type
    )
  ) %>%
  arrange(equity, type, Indicator, Group)

cat(sprintf("  aggregate_final: %d rows\n", nrow(aggregate_final)))
print(table(aggregate_final$equity, aggregate_final$Indicator))

write.csv(aggregate_final,
          file.path(dir_aggregate, paste0("aggregate_", RUN_DATE, ".csv")),
          row.names = FALSE)
cat(sprintf("  Saved: aggregate/aggregate_%s.csv\n", RUN_DATE))

# Country count output
write.csv(country_count,
          file.path(dir_aggregate, paste0("aggregate-country-count_", RUN_DATE, ".csv")),
          row.names = FALSE)
cat(sprintf("  Saved: aggregate/aggregate-country-count_%s.csv\n", RUN_DATE))

cat("\n=== Done ===\n")
