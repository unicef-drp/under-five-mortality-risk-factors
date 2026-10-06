# Map 10: subnational U5MR (UMIC + HIC).
# Map 11: countries with subnational data (UMIC/HIC + LMIC survey set).
rm(list = ls())
library(sf)
library(sp)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(readxl)
library(stringi)
library(scales)
library(RColorBrewer)

USERPROFILE <- Sys.getenv("USERPROFILE")
sub_dir    <- "Subnational"
gadm.dir   <- file.path(sub_dir, "input/gadm")
map.sp.dir <- file.path(USERPROFILE, "Dropbox/UNICEF Work/unmap/un_world_map/rds")
map.sp     <- file.path(map.sp.dir, "sp.world.robin.rds")

fig.dir  <- file.path("figures", "Map 10-11 subnational")
if (!dir.exists(fig.dir)) dir.create(fig.dir, recursive = TRUE)

MAP_WIDTH_IN  <- 10
MAP_HEIGHT_IN <- 4.5

save_on_page <- function(plot, path, width, height) {
  ggsave(path, plot, width = width, height = height, device = cairo_pdf)
}

# ---- Eurostat NUTS-1 data ----
eurostat <- read_excel(file.path(sub_dir, "output/subnational database_eurostat.xlsx"), sheet = 2) %>%
  filter(U5MR != 0 & IMR != 0)
eucountry <- eurostat %>%
  filter(`NUTS level` == "NUTS1") %>%
  group_by(Country) %>% mutate(n = n()) %>% filter(n > 1)
eurostat <- eurostat %>%
  filter(Country %in% eucountry$Country) %>%   # keep countries with >1 subnational region
  mutate(Country.Code = substr(region.code, 1, 2),
         Country = recode(Country, "Czechia" = "Czech Republic", "Türkiye" = "Turkey"))

# HIC / UMIC country sets
cnames_hic <- c("Canada", "United States of America", "New Zealand", "Australia", "South Korea", "Japan")
eurostatnames_hic <- c("FR","HR","HU","AT","BE","CH","CY","CZ","DE","EE","EL","IE","ES","FI","IS",
                       "PT","SE","SI","SK","UK","NO","IT","LI","LU","LV","MT","NL","PL")
eurostatnames_hic <- eurostatnames_hic[eurostatnames_hic %in% eurostat$Country.Code]

cnames_umic <- c("Argentina", "Brazil", "China", "Colombia", "Malaysia", "Mexico")  # Peru uses survey data -> skipped
eurostatnames_umic <- c("AL","BG","ME","MK","RO","RS","TR")
eurostatnames_umic <- eurostatnames_umic[eurostatnames_umic %in% eurostat$Country.Code]

cnames        <- c(cnames_hic, cnames_umic)
eurostatnames <- c(eurostatnames_hic, eurostatnames_umic)

# Eurostat ISO2 -> ISO3
eurostat_iso2_to_iso3 <- c(
  FR="FRA", HR="HRV", HU="HUN", AT="AUT", BE="BEL", CH="CHE", CY="CYP", CZ="CZE", DE="DEU",
  EE="EST", EL="GRC", IE="IRL", ES="ESP", FI="FIN", IS="ISL", PT="PRT", SE="SWE", SI="SVN",
  SK="SVK", UK="GBR", NO="NOR", IT="ITA", LI="LIE", LU="LUX", LV="LVA", MT="MLT", NL="NLD",
  PL="POL", AL="ALB", BG="BGR", ME="MNE", MK="MKD", RO="ROU", RS="SRB", TR="TUR"
)

# ---- GADM admin-1 boundaries ----
name2iso <- c("Canada"="CAN", "United States of America"="USA", "New Zealand"="NZL",
              "Australia"="AUS", "South Korea"="KOR", "Japan"="JPN",
              "Argentina"="ARG", "Brazil"="BRA", "China"="CHN", "Colombia"="COL",
              "Malaysia"="MYS", "Mexico"="MEX")
dt_geos_sf <- do.call(rbind, lapply(name2iso[cnames], function(cc) {
  s <- st_as_sf(readRDS(file.path(gadm.dir, paste0("gadm36_", cc, "_1_sp.rds"))))
  st_set_crs(s, 4326)   # GADM is lon/lat WGS84
}))

# world basemap and national borders (Robinson projection)
shp_world_robin <- readRDS(map.sp)
if (inherits(shp_world_robin, "Spatial")) shp_world_robin <- st_as_sf(shp_world_robin)
crs_robin <- st_crs(shp_world_robin)
dt_geos_sf <- st_transform(dt_geos_sf, crs_robin)

all_iso3 <- unname(c(name2iso[cnames], eurostat_iso2_to_iso3[eurostatnames]))
national_borders <- shp_world_robin %>% filter(ISO3_CODE %in% all_iso3)

# NUTS 2024 has no UK; fall back to 2021 for UK
nuts_read <- function(year) read_sf(file.path(sub_dir, sprintf(
  "input/eurostat/NUTS_RG_20M_%s_4326.shp/NUTS_RG_20M_%s_4326.shp", year, year))) %>%
  select(NUTS_ID, LEVL_CODE, CNTR_CODE)
dt_geos_eurostat <- rbind(
  nuts_read(2024) %>% filter(CNTR_CODE %in% eurostatnames, CNTR_CODE != "UK", LEVL_CODE == 1),
  nuts_read(2021) %>% filter(CNTR_CODE == "UK", LEVL_CODE == 1))
dt_geos_eurostat <- st_transform(dt_geos_eurostat, crs_robin)

# ---- Merge non-Europe rates onto GADM regions ----
# join key: canon() (accent-stripped lowercase) + per-country recodes on both sides
canon <- function(x) tolower(iconv(x, to = "ASCII//TRANSLIT"))
recode_region <- function(country, name) {
  dplyr::case_when(
    country == "Argentina" & name == "ciud. aut. de buenos aires"          ~ "ciudad de buenos aires",
    country == "Colombia"  & name == "archipielago de san andres y providencia" ~ "san andres y providencia",
    country == "Colombia"  & name == "bogota d.c."                          ~ "bogota",
    country == "Malaysia"  & name == "w.p. kuala lumpur"                   ~ "kuala lumpur",
    country == "Malaysia"  & name == "w.p. labuan"                         ~ "labuan",
    country == "Malaysia"  & name == "w.p. putrajaya"                      ~ "putrajaya",
    country == "Mexico"    & name == "coahuila de zaragoza"                ~ "coahuila",
    country == "Mexico"    & name == "michoacan de ocampo"                 ~ "michoacan",
    country == "Mexico"    & name == "veracruz de ignacio de la llave"     ~ "veracruz",
    country == "Mexico"    & name == "ciudad de mexico"                    ~ "distrito federal",
    country == "Japan"     & name == "naoasaki"                            ~ "nagasaki",             # GADM typo
    country == "Malaysia"  & name == "trengganu"                            ~ "terengganu",           # GADM alt spelling
    country == "New Zealand" & name == "capital & coast"                    ~ "capital and coast",
    country == "South Korea" & name == "sejong-si"                         ~ "sejong",
    TRUE ~ name)
}

other <- read_excel(file.path(sub_dir, "output/subnational database_supplementary.xlsx")) %>%
  filter(Country %in% c(cnames, "Republic of Korea")) %>%
  mutate(Country = ifelse(Country == "United States of America", "United States",
                   ifelse(Country == "Republic of Korea", "South Korea", Country)),
         Region_key = recode_region(Country, canon(Region)))

dt_geos_other <- dt_geos_sf %>%
  mutate(Region_key = recode_region(NAME_0, canon(NAME_1))) %>%
  left_join(other %>% select(Country, Region_key, IMR, U5MR, Year),
            by = c("NAME_0" = "Country", "Region_key"))
dt_geos_eurostat <- dt_geos_eurostat %>%
  left_join(eurostat, by = c("NUTS_ID" = "region.code"))

message("U5MR match rate -- GADM regions: ", sum(!is.na(dt_geos_other$U5MR)), "/", nrow(dt_geos_other),
        "; Eurostat regions: ", sum(!is.na(dt_geos_eurostat$U5MR)), "/", nrow(dt_geos_eurostat))

# plot extent, with 1.2x padding
bb <- st_bbox(dt_geos_sf)
x_min <- as.numeric(bb["xmin"]) * 1.2; x_max <- as.numeric(bb["xmax"]) * 1.2
y_min <- as.numeric(bb["ymin"]) * 1.2; y_max <- as.numeric(bb["ymax"]) * 1.2

map_theme <- ggthemes::theme_map() +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.justification = "center")

# ---- Map 10 ----
u5mr_breaks <- c(-Inf, 5, 10, 25, 50, 75, 100, Inf)
u5mr_labels <- c("<=5", "5 to 10", "10 to 25", "25 to 50", "50 to 75", "75 to 100", ">100")
u5mr_colors <- setNames(c(RColorBrewer::brewer.pal(9, "Blues")[2:8], "grey70"),
                         c(u5mr_labels, "no data"))

categorize_u5mr <- function(x) {
  cat <- cut(x, breaks = u5mr_breaks, labels = u5mr_labels, include.lowest = TRUE)
  cat <- as.character(cat)
  cat[is.na(cat)] <- "no data"
  factor(cat, levels = c(u5mr_labels, "no data"))
}
dt_geos_other$U5MR_category    <- categorize_u5mr(dt_geos_other$U5MR)
dt_geos_eurostat$U5MR_category <- categorize_u5mr(dt_geos_eurostat$U5MR)

map10 <- ggplot() +
  geom_sf(data = shp_world_robin, fill = "lightgray", color = "white", linewidth = 0.05) +
  # admin-1 / NUTS-1 fill, with a thin grey border between neighbouring regions
  # (countries with no data keep the white border set above)
  geom_sf(data = dt_geos_other, aes(fill = U5MR_category), color = "grey40", linewidth = 0.1) +
  geom_sf(data = dt_geos_eurostat, aes(fill = U5MR_category), color = "grey40", linewidth = 0.1) +
  # national outline for every included country, taken from the same basemap Maps 1-8 use
  geom_sf(data = national_borders, color = "black", fill = NA, linewidth = 0.3) +
  scale_fill_manual(
    values = u5mr_colors, drop = FALSE,
    name = "Under-five mortality rate (deaths per 1,000 live births)"
  ) +
  coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
  map_theme +
  guides(fill = guide_legend(title.position = "top", nrow = 1))
ggsave(file.path(fig.dir, "Map_10_U5MR_subnational.png"), map10, width = 10, height = 4.5, dpi = 300, units = "in", bg = "white")
save_on_page(map10, file.path(fig.dir, "Map_10_U5MR_subnational.pdf"), width = 10, height = 4.5)
message("Saved: Map_10_U5MR_subnational")

# ---- Map 11 ----
# base-R render to match the style of Maps 1-8
bnd  <- readRDS(file.path(map.sp.dir, "sp.bnd.rds"))
cst  <- readRDS(file.path(map.sp.dir, "sp.cst.rds"))
lks  <- readRDS(file.path(map.sp.dir, "sp.lks.rds"))
rks  <- readRDS(file.path(map.sp.dir, "sp.RKS.boarder.rds"))   # Kosovo boundary overlay (outline only)
world.robin.base <- readRDS(map.sp)   # raw sp object (NOT the sf-converted shp_world_robin used above)

# ISO3 -> M49 lookup so the country merge lets disputed slivers inherit parent colour
iso3_to_m49 <- unique(world.robin.base@data[
  !is.na(world.robin.base@data$ISO3_CODE) & world.robin.base@data$ISO3_CODE != "",
  c("ISO3_CODE", "M49COLOR")])

sp_to_df <- function(sp_obj) {
  do.call(rbind, lapply(seq_along(sp_obj@polygons), function(i) {
    poly <- sp_obj@polygons[[i]]
    do.call(rbind, lapply(seq_along(poly@Polygons), function(j) {
      p <- poly@Polygons[[j]]@coords
      data.frame(long = p[, 1], lat = p[, 2], group = paste(i, j, sep = "."))
    }))
  }))
}
lks.df <- sp_to_df(lks)

bnd.line <- bnd[bnd$CARTOGRAPH == "International boundary line", ]
bnd.dash <- bnd[bnd$CARTOGRAPH %in% c("Dashed boundary line", "Undetermined international dashed boundary line"), ]
bnd.dot  <- bnd[bnd$CARTOGRAPH %in% c("Dotted boundary line", "Dotted boundary line (Abyei)"), ]
bnd.ssd  <- bnd[bnd$BDY_CNT01 == "SDN" & bnd$BDY_CNT02 == "SSD", ]

NoDataColor      <- "darkgray"
boundary.color   <- "white"
background.color <- "white"
coastline.color  <- "darkgray"
availability.colors <- "blue"
legend.labels    <- c("With subnational data", "No data")
legend.title     <- ""

unpd.map <- function(world.robin) {
  world.robin$colorcode <- NoDataColor
  world.robin$colorcode[!is.na(world.robin$vartomap)] <- availability.colors

  # Greenland inherits Denmark's colour (matches Maps 1-8 / make.world.map.R)
  world.robin$colorcode[world.robin$TERR_NAME == "Greenland"] <-
    world.robin$colorcode[world.robin$TERR_NAME == "Denmark"]

  # Hong Kong inherits China's colour; its own M49 code (344) differs from China's
  # (156), so the merge needs this patch. Taiwan is already 156, Macau is left as is.
  world.robin$colorcode[world.robin$TERR_NAME == "Hong Kong"] <-
    world.robin$colorcode[world.robin$TERR_NAME == "China"]

  # Aksai Chin: stripe with China's colour per UN Cartography
  ac  <- world.robin[world.robin$TERR_NAME == "Aksai Chin", ]
  acf <- sp_to_df(ac)
  acf$colorcode <- world.robin$colorcode[world.robin$TERR_NAME == "China"]

  plot(world.robin, border = NA, col = world.robin$colorcode, bg = background.color)
  polygon(acf$long, acf$lat, col = acf$colorcode[1], border = NA,
          density = 130, angle = 45, lwd = 0.4)
  lines(bnd.line, col = boundary.color, lwd = 0.2, lty = 1)
  lines(bnd.dash, col = boundary.color, lwd = 0.2, lty = 2)
  lines(bnd.dot,  col = boundary.color, lwd = 0.2, lty = 3)
  lines(bnd.ssd,  col = boundary.color, lwd = 0.2, lty = 2)
  # Kosovo: outline only (no separate fill), matching Maps 1-8 / make.world.map.R
  plot(rks, add = TRUE, border = boundary.color, lwd = 0.2, lty = 2)

  for (gp in unique(lks.df$group)) {
    lk <- lks.df[lks.df$group == gp, ]
    polygon(lk$long, lk$lat, col = background.color, border = boundary.color, lty = 2, lwd = 0.2)
  }
  lines(cst, col = coastline.color, lwd = 0.2, lty = 1)

  legend(-16820000, -1000000,
         col = c(availability.colors, NoDataColor), pt.bg = c(availability.colors, NoDataColor),
         pch = 15, pt.cex = 2, cex = 0.7,
         legend = legend.labels, title = legend.title,
         box.lty = 0, box.col = "white", bty = "o", bg = "white")
}

# add the LMIC survey-based countries (Table 4 "Subnational (1)") to the UMIC/HIC set
lmic_subnat <- read_excel(
  file.path(sub_dir, "UN-IGME-2023-Subnational-U5MR-and-NMR-database-survey.xlsx"),
  sheet = "U5MR and NMR")
lmic_iso3 <- unique(lmic_subnat$Country.ISO)
message("LMIC/LIC subnational countries: ", length(lmic_iso3),
        "; UMIC/HIC subnational countries: ", length(all_iso3))

all_iso3_map11 <- union(all_iso3, lmic_iso3)
map11_m49 <- unique(iso3_to_m49$M49COLOR[iso3_to_m49$ISO3_CODE %in% all_iso3_map11])
indata <- data.frame(M49COLOR = map11_m49, vartomap = 1L)
world.robin <- merge(world.robin.base, indata, by = "M49COLOR", all.x = TRUE, all.y = FALSE)

png(file.path(fig.dir, "Map_11_data_availability_subnational.png"),
    width = MAP_WIDTH_IN, height = MAP_HEIGHT_IN, units = "in", res = 300)
par(mfrow = c(1,1), omi = c(0,0,0,0), mar = c(0,0,0,0), mgp = c(2,0.5,0), las = 0, mex = 1, cex = 1)
unpd.map(world.robin)
dev.off()

cairo_pdf(file.path(fig.dir, "Map_11_data_availability_subnational.pdf"),
          width = MAP_WIDTH_IN, height = MAP_HEIGHT_IN)
par(mfrow = c(1,1), omi = c(0,0,0,0), mar = c(0,0,0,0), mgp = c(2,0.5,0), las = 0, mex = 1, cex = 1)
unpd.map(world.robin)
dev.off()

message("Saved: Map_11_data_availability_subnational")
