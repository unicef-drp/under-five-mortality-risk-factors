# Builds output/subnational database_supplementary.xlsx from the non-Eurostat sources.
# One block per country; each returns Country / Region / Year / IMR / U5MR.

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(pdftools)
library(purrr)
library(stringi)
library(openxlsx)

IN  <- file.path("input", "country")
OUT <- "output"

# profile.R resolves each user's SharePoint root into dir_IGME
source(file.path(Sys.getenv("USERPROFILE"), "Dropbox/UNICEF Work/profile.R"))
a0 <- read_csv(file.path(dir_IGME, "2026 Round Estimation/Code/input/a0.csv"),
               col_types = cols())
sep <- function(iso) a0$a0[a0$iso == iso]

# deaths and population by age -> IMR/CMR/U5MR, the Eurostat convention
from_pop <- function(d0, d14, p0, p14, iso) {
  x1M0 <- d0 / p0
  x4M1 <- d14 / p14
  imr <- x1M0 / (1 + (1 - sep(iso)) * x1M0)
  cmr <- 4 * x4M1 / (1 + (4 - 1.6) * x4M1)
  tibble(IMR = imr * 1000, U5MR = (1 - (1 - imr) * (1 - cmr)) * 1000)
}
num <- function(x) suppressWarnings(as.numeric(gsub("[ ,]", "", as.character(x))))
num_ar <- function(x) suppressWarnings(as.numeric(gsub("[. ]", "", x)))   # AR uses . for thousands

# ---- 1. Argentina ----
# three sources, three spellings of the same 24 jurisdictions
canon <- function(x) {
  stri_trans_general(x, "Latin-ASCII") %>% tolower() %>% str_replace_all("[^a-z]", "") %>%
    str_replace("^(ciudaut|capitalfederal).*", "caba") %>%
    str_replace("^tierradelfuego.*", "tierradelfuego") %>%
    recode("santiengodelestero" = "santiagodelestero", "medoza" = "mendoza")   # DEIS typos
}

arg_imr <- read_csv(file.path(IN, "Argentina/tasa-mortalidad-infantil-deis-1990-2024.csv"),
                    col_types = cols()) %>%
  slice_tail(n = 1) %>%
  pivot_longer(-indice_tiempo, names_to = "key", values_to = "IMR") %>%
  filter(key != "mortalidad_infantil_argentina") %>%
  transmute(key = canon(sub("^mortalidad_infantil_", "", key)), IMR)

arg_lb <- read_excel(file.path(IN, "Argentina/nacimientos_jurisdiccion_1914-2024.xlsx")) %>%
  slice_tail(n = 1) %>%
  pivot_longer(-anio, names_to = "key", values_to = "lb") %>%
  filter(key != "total_argentina") %>%
  transmute(key = canon(key), lb = num(lb))

# cuadro 21 columns: total, <1 detail x4, subtotal <1, ages 1-4, subtotal 1-4
arg_pdf <- pdf_text(file.path(IN, "Argentina/serie_5_nro_68_anuario_vitales_2024_v2.pdf"))
arg_dth <- str_split(arg_pdf[grep("CUADRO 21", arg_pdf)][1], "\n")[[1]] %>%
  str_squish() %>%
  str_match("^([A-Za-zÀ-ÿ. ]{4,}?) +([0-9. ]+)$") %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(c("raw", "jur", "nums")) %>%
  filter(!is.na(jur)) %>%
  mutate(v = str_split(nums, " +")) %>%
  filter(lengths(v) >= 11) %>%
  transmute(Region = str_squish(jur), key = canon(jur),
            dth0 = map_dbl(v, ~ num_ar(.x[6])), dth14 = map_dbl(v, ~ num_ar(.x[11])))

argentina <- arg_dth %>%
  inner_join(arg_imr, by = "key") %>%
  inner_join(arg_lb, by = "key") %>%
  transmute(Country = "Argentina", Region, Year = 2024, IMR,
            U5MR = (dth0 + dth14) / lb * 1000,
            imr_check = dth0 / lb * 1000)

# ---- 2. Australia ----
aus <- read_csv(file.path(IN, "Australia/ABS_DEATHS_AGESPECIFIC_OCCURENCEYEAR_0to4.csv"),
                col_types = cols(.default = "c")) %>%   # "4: 4" parses as a time otherwise
  transmute(measure = sub(".*: ", "", `MEASURE: Measure`),
            age = sub(".*: ", "", `AGE: Age`),
            Region = sub("^[^:]*: ", "", `REGION: Region`),
            Year = as.integer(`TIME_PERIOD: Time Period`),
            value = as.numeric(OBS_VALUE)) %>%
  filter(Year == 2024, Region != "Australia") %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  group_by(Region, Year) %>%
  summarise(d0 = Deaths[age == "0"], p0 = Population[age == "0"],
            d14 = sum(Deaths[age != "0"]), p14 = sum(Population[age != "0"]),
            .groups = "drop") %>%
  bind_cols(., from_pop(.$d0, .$d14, .$p0, .$p14, "AUS")) %>%
  transmute(Country = "Australia", Region, Year, IMR, U5MR)

# ---- 3. Brazil ----
# preamble length varies by tabulation, so match the UF rows directly
br <- function(f, col) {
  read_lines(Sys.glob(file.path(IN, "Brazil", f)),
             locale = locale(encoding = "ISO-8859-1")) %>%
    str_match('^"(\\d\\d [^"]+)";"?([\\d.]+)"?$') %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("raw", "uf", "v")) %>%
    filter(!is.na(uf)) %>%
    transmute(Region = str_sub(uf, 4), !!col := num(v))
}
brazil <- br("sim_cnv_inf10uf*.csv", "inf") %>%
  inner_join(br("sim_cnv_obt10uf*.csv", "u5"), by = "Region") %>%
  inner_join(br("sinasc_cnv_nvuf*.csv", "lb"), by = "Region") %>%
  transmute(Country = "Brazil", Region, Year = 2024,
            IMR = inf / lb * 1000, U5MR = u5 / lb * 1000)

# ---- 4. Canada ----
ca <- function(f, ...) read_csv(Sys.glob(file.path(IN, "Canada", f)), col_types = cols(.default = "c"))
ca_geo <- function(x) str_squish(sub(", place of residence$", "", x))

ca_d14 <- ca("1310070901_*.csv") %>%
  filter(REF_DATE == "2024", `Age at time of death` == "Age at time of death, 0 to 4 years",
         Sex == "Both sexes") %>%
  transmute(Region = ca_geo(GEO), d04 = num(VALUE))
ca_d0 <- ca("1310071301_*.csv") %>%
  filter(REF_DATE == "2024", Characteristics == "Number of deaths",
         `Age at time of death` == "Total infant deaths, age at time of death, under 1 year") %>%
  transmute(Region = ca_geo(GEO), d0 = num(VALUE))
ca_pop <- ca("1710000501_*.csv") %>%
  filter(REF_DATE == "2024", `Gender` == "Total - gender",
         `Age group` %in% c("0 years", "1 year", "2 years", "3 years", "4 years")) %>%
  transmute(Region = ca_geo(GEO), age = `Age group`, pop = num(VALUE)) %>%
  group_by(Region) %>%
  summarise(p0 = pop[age == "0 years"], p14 = sum(pop[age != "0 years"]), .groups = "drop")

canada <- ca_d14 %>%
  inner_join(ca_d0, by = "Region") %>%
  inner_join(ca_pop, by = "Region") %>%
  filter(!Region %in% c("Canada", "Northwest Territories including Nunavut"),
         !str_detect(Region, "^Unknown")) %>%
  bind_cols(., from_pop(.$d0, .$d04 - .$d0, .$p0, .$p14, "CAN")) %>%
  transmute(Country = "Canada", Region, Year = 2024, IMR, U5MR)

# ---- 5. China ----
china <- read_excel(file.path(IN, "China/A0601.xls"), sheet = "dth") %>%
  filter(region != "total") %>%
  transmute(Country = "China", Region = region, Year = 2020,
            IMR = x0 / lb * 1000, U5MR = (x0 + x4) / lb * 1000)

# ---- 6. Colombia ----
# Cuadro4 age groups run in threes: Hombres, Mujeres, Indeterminado
co_d <- read_excel(file.path(IN, "Colombia/anex-EEVV-DefuncionesNoFetalesCuadroUnificado-2024.xlsx"),
                   sheet = "Cuadro4", col_names = FALSE)
co_grp <- as.character(unlist(co_d[7, ]))
co_cols <- function(labels) {
  i <- which(co_grp %in% labels)
  sort(c(i, i + 1, i + 2))
}
INF <- c("Menor 1 hora", "De 1 a 23 horas", "De 1 a 6 días", "De 7 a 27 días",
         "De 28 a 29 días", "De 1 a 5 meses", "De 6 a 11 meses")
colombia_d <- co_d %>%
  filter(as.character(.[[4]]) == "Total Dpto", as.character(.[[5]]) == "TOTAL") %>%
  transmute(Region = str_squish(sub("^\\d+ ", "", as.character(.[[3]]))),
            inf = rowSums(across(all_of(co_cols(INF)), num), na.rm = TRUE),
            d14 = rowSums(across(all_of(co_cols(c("De 1 año", "De 2 a 4 años"))), num), na.rm = TRUE))

co_b <- read_excel(file.path(IN, "Colombia/anex-EEVV-NacimientosCuadroAcumulado-2024.xlsx"),
                   sheet = "Cuadro3", col_names = FALSE)
colombia_b <- co_b %>%
  filter(as.character(.[[4]]) == "Total") %>%
  transmute(Region = str_squish(sub("^\\d+ ", "", as.character(.[[3]]))), lb = num(.[[5]]))

colombia <- inner_join(colombia_d, colombia_b, by = "Region") %>%
  filter(Region != "Sin Información") %>%
  transmute(Country = "Colombia", Region, Year = 2024,
            IMR = inf / lb * 1000, U5MR = (inf + d14) / lb * 1000)

# ---- 7. Japan ----
jp <- function(f, skip) read_csv(file.path(IN, "Japan", f), skip = skip,
                                 locale = locale(encoding = "SHIFT-JIS"),
                                 col_types = cols(.default = "c"))
jp_pref <- function(x) str_squish(sub("^\\d\\d ", "", x))

jp_imr <- jp("emd110000.csv", 4) %>%
  filter(str_detect(.[[1]], "^\\d\\d ")) %>%
  transmute(Region = jp_pref(.[[1]]), IMR = num(.data[["2024"]]))
jp_lb <- jp("emb030000.csv", 5) %>%
  filter(str_detect(.[[1]], "^\\d\\d ")) %>%
  transmute(Region = jp_pref(.[[1]]), lb = num(.data[["2024"]]))
jp_d04 <- read_csv(file.path(IN, "Japan/ec030000.csv"), skip = 4, col_names = FALSE,
                   locale = locale(encoding = "SHIFT-JIS"), col_types = cols(.default = "c")) %>%
  filter(str_detect(.[[1]], "^\\d\\d ")) %>%
  transmute(Region = jp_pref(.[[1]]), d04 = num(.[[5]]))

japan <- jp_imr %>%
  inner_join(jp_lb, by = "Region") %>%
  inner_join(jp_d04, by = "Region") %>%
  transmute(Country = "Japan", Region, Year = 2024, IMR, U5MR = d04 / lb * 1000)

# ---- 8. Republic of Korea ----
kr_d <- read_excel(Sys.glob(file.path(IN, "Korea/Deaths_and_Death_Rates*.xlsx")),
                   sheet = "Data", col_names = FALSE) %>%
  slice(-(1:2)) %>%
  fill(1, 2) %>%
  transmute(sex = as.character(.[[1]]), age = as.character(.[[2]]),
            Region = as.character(.[[3]]), d = num(.[[4]])) %>%
  filter(sex == "Total", age %in% c("0 year old", "1 - 4 years old")) %>%
  select(-sex) %>%
  pivot_wider(names_from = age, values_from = d) %>%
  rename(d0 = `0 year old`, d14 = `1 - 4 years old`)

kr_b <- read_excel(Sys.glob(file.path(IN, "Korea/Live_Births*.xlsx")),
                   sheet = "Data", col_names = FALSE) %>%
  slice(-(1:2)) %>%
  fill(1, 2) %>%
  filter(as.character(.[[2]]) == "Total", as.character(.[[3]]) == "Total") %>%
  transmute(Region = as.character(.[[1]]), lb = num(.[[4]]))

korea <- kr_d %>%
  mutate(Region = recode(Region, "Sejong" = "sejong-si", "Jeju-do" = "Jeju")) %>%
  inner_join(kr_b, by = "Region") %>%
  filter(Region != "Whole country") %>%
  transmute(Country = "Republic of Korea", Region, Year = 2024,
            IMR = d0 / lb * 1000, U5MR = (d0 + d14) / lb * 1000)

# ---- 9. Malaysia ----
my_d <- read_csv(file.path(IN, "Malaysia/deaths_early_childhood_state.csv"), col_types = cols()) %>%
  filter(format(date, "%Y") == "2024", type %in% c("total", "infant")) %>%
  select(state, type, abs) %>%
  pivot_wider(names_from = type, values_from = abs)
my_b <- read_csv(file.path(IN, "Malaysia/birth_state.csv"), col_types = cols()) %>%
  filter(format(date, "%Y") == "2024") %>%
  select(state, lb = abs)

malaysia <- inner_join(my_d, my_b, by = "state") %>%
  transmute(Country = "Malaysia", Region = state, Year = 2024,
            IMR = infant / lb * 1000, U5MR = total / lb * 1000)

# ---- 10. Mexico ----
# the OLAP export is xlsx despite its .xls extension
mx_d <- read_xlsx(Sys.glob(file.path(IN, "Mexico/INEGI_exporta_*.xls")), col_names = FALSE) %>%
  transmute(Region = as.character(.[[2]]), inf = num(.[[4]]), d14 = num(.[[5]])) %>%
  filter(!is.na(Region), !is.na(inf), !Region %in% c("Total", "No especificado", "Extranjero"))
mx_b <- read_excel(file.path(IN, "Mexico/Natalidad_01.xlsx"), col_names = FALSE) %>%
  transmute(Region = as.character(.[[1]]), lb = num(.[[2]])) %>%
  filter(!is.na(Region), !is.na(lb), Region != "Total")

mexico <- inner_join(mx_d, mx_b, by = "Region") %>%
  transmute(Country = "Mexico", Region, Year = 2024,
            IMR = inf / lb * 1000, U5MR = (inf + d14) / lb * 1000)

# ---- 11. New Zealand ----
# the 2023 extract has one 00-04 band, so no IMR
nz_d <- read_excel(file.path(IN, "New Zealand/deaths 2023.xlsx"), skip = 1) %>%
  transmute(Region = District, d04 = replace_na(num(`00-04`), 0)) %>%   # blank means zero
  filter(!Region %in% c("All New Zealand", "Unknown"))
nz_b <- read_csv(file.path(IN, "New Zealand/lb_dhb_2001-2025.csv"), col_types = cols()) %>%
  filter(year == 2023) %>%
  pivot_longer(-year, names_to = "Region", values_to = "lb") %>%
  mutate(Region = recode(Region, "Otago" = "Southern", "Southland" = "Southern",
                         "Hutt" = "Hutt Valley", "Midcentral" = "MidCentral",
                         "Capital and Coast" = "Capital & Coast")) %>%
  group_by(Region) %>%
  summarise(lb = sum(lb), .groups = "drop")

newzealand <- inner_join(nz_d, nz_b, by = "Region") %>%
  transmute(Country = "New Zealand", Region, Year = 2023,
            IMR = NA_real_, U5MR = d04 / lb * 1000)

# ---- 12. United States of America ----
# Ten-Year Age Groups, not single years: WONDER omits suppressed rows
# and single-year 1-4 counts fall below the threshold in small states
us <- read_tsv(Sys.glob(file.path(IN, "US/Underlying Cause of Death*.xls")),
               col_types = cols(.default = "c")) %>%
  filter(!is.na(State)) %>%
  transmute(Region = State, age = `Ten-Year Age Groups`,
            d = num(Deaths), p = num(Population)) %>%
  pivot_wider(names_from = age, values_from = c(d, p)) %>%
  rename(d0 = `d_< 1 year`, p0 = `p_< 1 year`,
         d14 = `d_1-4 years`, p14 = `p_1-4 years`)
usa <- us %>%
  bind_cols(from_pop(us$d0, us$d14, us$p0, us$p14, "USA")) %>%
  transmute(Country = "United States of America", Region, Year = 2023, IMR, U5MR)

# ---- 14. Combine ----
output <- bind_rows(aus, brazil, canada, china, colombia, japan, korea,
                    malaysia, mexico, newzealand, usa,
                    select(argentina, Country, Region, Year, IMR, U5MR)) %>%
  arrange(Country, Region)

dir.create(OUT, showWarnings = FALSE)
write.xlsx(output, file.path(OUT, "subnational database_supplementary.xlsx"))

output %>% count(Country, Year, name = "regions") %>% as.data.frame() %>% print(row.names = FALSE)
