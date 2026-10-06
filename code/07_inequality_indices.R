# Country inequality indices (concentration, slope, Theil, between-group variance,
# disparity ratio, absolute difference) for Figures 31-32 and the report text.
# Run from the project root after 01 and 02.

library(dplyr)

source(file.path(Sys.getenv("USERPROFILE"), "Dropbox/UNICEF Work/profile.R"))  # dir_IGME
source("code/run-config.R")
cfg <- equity_run_config()

# ---- 1. Inputs ----
country_dir <- file.path(cfg$output_dir, "country")
db_survey <- read.csv(file.path(country_dir, paste0("country-specific-database-cleaned_", cfg$run_date, ".csv")))
db_raw    <- read.csv(file.path(country_dir, paste0("country-specific-database-cleaned-igme-wealth_", cfg$run_date, ".csv")))
births    <- read.csv(file.path(cfg$output_dir, "aggregate", paste0("deaths-and-births-country_", cfg$run_date, ".csv")))
national  <- read.csv(file.path(dir_IGME, paste0("2025 Round Estimation/Code/Aggregate results (median) ", cfg$igme_rates_date),
                                "Rates & Deaths_Country Summary.csv"))
out_file  <- file.path(cfg$output_dir, "other", "inequality_indices.csv")

indicators <- c("U5MR", "NMR", "MR1t59")
# groups in rank order for the concentration index; pair = groups compared in the disparity ratio
dims <- list(
  list(equity = "wealth",        comparison = NA,         pair = c(1, 5), groups = c("1.quintile", "2.quintile", "3.quintile", "4.quintile", "5.quintile")),
  list(equity = "edu",           comparison = "4 groups", pair = c(1, 4), groups = c("noeducation", "primary", "secondary", "higher")),
  list(equity = "edu",           comparison = "2 groups", pair = c(1, 2), groups = c("noedu.primary", "sec.higher")),
  list(equity = "residence",     comparison = NA,         pair = c(1, 2), groups = c("rural", "urban")),
  list(equity = "birthorder",    comparison = NA,         pair = c(1, 2), groups = c("1", "2-3", "4+")),
  list(equity = "birthinterval", comparison = NA,         pair = c(1, 3), groups = c("<2", "2", "3", "4+")),
  list(equity = "motherage",     comparison = NA,         pair = c(1, 2), groups = c("<20", "20-29", "30-39", "40-49"))
)

# ---- 2. Birth shares ----
# NMR birth shares weight every indicator; IGME wealth U5MR quintiles are equal fifths
nmr_shares <- births %>%
  filter(Indicator == "NMR") %>%
  select(Country.Code, Group, label, prop.birth)
igme_wealth <- db_raw$label == "wealth" & db_raw$Indicator == "U5MR"
db <- bind_rows(
  db_raw[!igme_wealth, ] %>% left_join(nmr_shares, by = c("Country.Code", "Group", "label")),
  db_raw[igme_wealth, ] %>% mutate(prop.birth = 0.2)
)

# ---- 3. Concentration and slope index (Kakwani, Wagstaff and van Doorslaer 1997) ----
concentration <- function(d, ind) {
  db %>%
    filter(label == d$equity, Indicator == ind, Group %in% d$groups) %>%
    arrange(Country.Code, match(Group, d$groups)) %>%  # rank groups in listed order, not file order
    group_by(Country.Code) %>%
    mutate(prop.birth = prop.birth / sum(prop.birth),  # shares sum to 1 within each country
           rank_pos = prop.birth / 2 + lag(cumsum(prop.birth), default = 0)) %>%
    summarise(rate = sum(prop.birth * Estimates),
              ci   = 2 / rate * sum(prop.birth * Estimates * rank_pos) - 1,
              rii  = ci / (2 * sum(prop.birth * (rank_pos - sum(prop.birth * rank_pos))^2)),  # birth-weighted rank variance
              .groups = "drop") %>%
    filter(!is.na(ci), !is.na(rii)) %>%
    transmute(Country.Code, Indicator = ind,
              `slope index` = rii * rate, `concentration index` = ci * 100,
              `concentration index rank` = rank(`concentration index`),
              `slope index rank` = rank(`slope index`))
}

# ---- 4. Theil index and between-group variance ----
theil <- function(d, ind) {
  if (d$equity == "wealth" && ind == "U5MR") {
    # IGME quintiles against the IGME national rate
    x <- db %>%
      filter(label == d$equity, Indicator == ind, Group %in% d$groups) %>%
      left_join(select(national, Country.Code = ISO3Code, mean = U5MR.2024), by = "Country.Code") %>%
      mutate(share = 0.2)
    ties <- "average"
  } else {
    if (ind == "MR1t59") {
      # 1-59 month mean from the survey U5MR and NMR totals
      tot <- function(i) db_survey %>% filter(label == d$equity, Indicator == i, Group == "total") %>% select(Country.Code, Estimates)
      means <- full_join(rename(tot("U5MR"), u5 = Estimates), rename(tot("NMR"), nmr = Estimates), by = "Country.Code") %>%
        transmute(Country.Code, mean = 1000 * (1 - (1 - u5 / 1000) / (1 - nmr / 1000))) %>%
        filter(!is.na(mean))
    } else {
      means <- db %>% filter(label == d$equity, Indicator == ind, Group == "total") %>% select(Country.Code, mean = Estimates)
    }
    x <- db %>%
      filter(label == d$equity, Indicator == ind, Group %in% d$groups) %>%
      right_join(means, by = "Country.Code") %>%
      mutate(share = prop.birth)
    ties <- "min"
  }
  x %>%
    group_by(Country.Code) %>%
    mutate(share = share / sum(share)) %>%
    summarise(`theils index` = sum(share * Estimates / mean * log(Estimates / mean)) * 1000,
              `between group variance` = sum(share * (Estimates - mean)^2), .groups = "drop") %>%
    filter(ties == "average" | (!is.na(`theils index`) & !is.na(`between group variance`))) %>%
    mutate(Indicator = ind,
           `theils index rank` = rank(`theils index`, ties.method = ties),
           `between group variance rank` = rank(`between group variance`, ties.method = ties))
}

# ---- 5. Disparity ratio and absolute difference ----
gap <- function(d) {
  hi <- d$groups[d$pair[1]]
  lo <- d$groups[d$pair[2]]
  db_raw %>%
    filter(label == d$equity, Group %in% d$groups, Indicator %in% indicators) %>%
    group_by(Country.Code, Indicator) %>%
    filter(all(d$groups %in% Group)) %>%
    summarise(`disparity ratio` = Estimates[Group == hi] / Estimates[Group == lo],
              `absolute difference` = Estimates[Group == hi] - Estimates[Group == lo], .groups = "drop") %>%
    filter(!is.na(`disparity ratio`), !is.na(`absolute difference`)) %>%
    group_by(Indicator) %>%
    mutate(`disparity ratio rank` = rank(`disparity ratio`),
           `absolute difference rank` = rank(`absolute difference`)) %>%
    ungroup() %>%
    distinct()
}

# ---- 6. Combine and write ----
indices <- bind_rows(lapply(dims, function(d) {
  bind_rows(lapply(indicators, function(ind) concentration(d, ind))) %>%
    left_join(bind_rows(lapply(indicators, function(ind) theil(d, ind))), by = c("Country.Code", "Indicator")) %>%
    left_join(gap(d), by = c("Country.Code", "Indicator")) %>%
    mutate(equity = d$equity, comparison = d$comparison)
})) %>%
  filter(`disparity ratio` != 0) %>%
  select(Country.Code, equity, Indicator, comparison,
         `absolute difference`, `slope index`, `between group variance`,
         `disparity ratio`, `concentration index`, `theils index`,
         `absolute difference rank`, `slope index rank`, `between group variance rank`,
         `disparity ratio rank`, `concentration index rank`, `theils index rank`)

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
write.csv(indices, out_file, row.names = FALSE, na = "")
message("Written: ", out_file, " (", nrow(indices), " rows)")
