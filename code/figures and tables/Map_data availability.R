rm(list = ls())
library(dplyr)
library(sp)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(classInt)
library(mapplots)

USERPROFILE <- Sys.getenv("USERPROFILE")

# World-map output at about 2.2:1, with readable legend text.
MAP_WIDTH_IN   <- 10
MAP_HEIGHT_IN  <- 4.5

input.dir  <- file.path("output", "country")
output.dir <- file.path("figures", "Map data availability")
if (!dir.exists(output.dir)) dir.create(output.dir, recursive = TRUE)

map.sp.dir <- file.path(USERPROFILE, "Dropbox/UNICEF Work/unmap/un_world_map/rds")

# Auto-detect the latest country CSVs. The map uses db-pre-aggregation rather than
# the final cleaned file: both are fully quality-filtered, but the final file also
# requires matching birth counts, which drops countries whose data is real but not
# birth-weight eligible, such as China and India residence.
country_file <- tail(sort(list.files(input.dir, pattern = "^country-specific-database-cleaned-igme-wealth_.*\\.csv$", full.names = TRUE)), 1)
disparity <- read.csv(country_file) %>% rename(equity = label)

pre_agg_file <- tail(sort(list.files(input.dir, pattern = "^db-pre-aggregation_.*\\.csv$", full.names = TRUE)), 1)
disparity_pre_agg <- read.csv(pre_agg_file) %>% rename(equity = label)

# Equity dimensions to map: name -> filter expression. `source` picks the file:
# "cleaned" (default) or "pre_agg". Wealth stays on "cleaned" because the IGME
# model rows are joined only after the pre-aggregation snapshot is written. Wealth
# filters on neither survey nor model, so it covers both the 106 U5MR model
# countries and the NMR survey countries (107 with Kosovo).
equity_map_config <- list(
  wealth        = list(label = "wealth",        survey_only = FALSE, igme_only = FALSE, source = "cleaned", map_name = "Map_1_data_availability_wealth"),
  edu           = list(label = "edu",            survey_only = TRUE,  igme_only = FALSE, source = "pre_agg", map_name = "Map_2_data_availability_surveydata_edu"),
  residence     = list(label = "residence",      survey_only = TRUE,  igme_only = FALSE, source = "pre_agg", map_name = "Map_3_data_availability_surveydata_residence"),
  bsize         = list(label = "bsize",          survey_only = TRUE,  igme_only = FALSE, source = "pre_agg", map_name = "Map_8_data_availability_surveydata_bsize"),
  birthorder    = list(label = "birthorder",     survey_only = TRUE,  igme_only = FALSE, source = "pre_agg", map_name = "Map_7_data_availability_birthorder"),
  birthinterval = list(label = "birthinterval",  survey_only = TRUE,  igme_only = FALSE, source = "pre_agg", map_name = "Map_6_data_availability_birthinterval"),
  motherage     = list(label = "motherage",      survey_only = TRUE,  igme_only = FALSE, source = "pre_agg", map_name = "Map_5_data_availability_motherage"),
  # Ethnicity is not in the disparity country file, so the countries are listed
  # here: Figure 13's 42-country survey set plus Australia and Brazil from the 2024
  # file. The six countries taken from the older VR database are not mapped.
  ethnicity     = list(map_name = "Map_4_data_availability_ethnicity",
                       iso3 = c("AFG","AGO","BEN","BFA","CAF","CIV","CMR","COD","COG","ETH",
                                "GAB","GHA","GIN","GMB","GNB","GTM","GUY","HND","IND","JOR",
                                "KEN","LAO","LBR","MLI","MOZ","MRT","MWI","NER","NGA","NPL",
                                "PAK","PHL","PRY","SEN","SLE","SUR","TCD","TGO","UGA","XKX",
                                "ZAF","ZMB",
                                "AUS","BRA"))
)

# Map aesthetics
NoDataColor    <- "darkgray"
boundary.color <- "white"
background.color <- "white"
coastline.color  <- "darkgray"
colors           <- "blue"
legend.labels    <- c("With data", "No data")
legend.title     <- ""
disclaimer.text  <- paste0(
  "The boundaries and names shown and the designations used on this map do not imply official\n",
  "endorsement or acceptance by the United Nations. Dotted line represents approximately the\n",
  "Line of Control in Jammu and Kashmir agreed upon by India and Pakistan. The final status of\n",
  "Jammu and Kashmir has not yet been agreed upon by the parties. Final boundary between the\n",
  "Republic of Sudan and the Republic of South Sudan has not yet been determined."
)

# sp -> data.frame helper (broom::tidy defunct for sp >= 1.0.4)
sp_to_df <- function(sp_obj) {
  do.call(rbind, lapply(seq_along(sp_obj@polygons), function(i) {
    poly <- sp_obj@polygons[[i]]
    do.call(rbind, lapply(seq_along(poly@Polygons), function(j) {
      p <- poly@Polygons[[j]]@coords
      data.frame(long = p[, 1], lat = p[, 2], group = paste(i, j, sep = "."))
    }))
  }))
}

# Load shapefiles once
world.robin.base <- readRDS(file.path(map.sp.dir, "sp.world.robin.rds"))

# ISO3 -> M49 lookup from the shapefile's own attributes. The reference IGME map
# script joins on M49COLOR, which lets territories the UN records under a parent
# country inherit its colour (Taiwan, the disputed border slivers), so these maps
# merge the same way.
iso3_to_m49 <- unique(world.robin.base@data[
  !is.na(world.robin.base@data$ISO3_CODE) & world.robin.base@data$ISO3_CODE != "",
  c("ISO3_CODE", "M49COLOR")])

bnd  <- readRDS(file.path(map.sp.dir, "sp.bnd.rds"))
cst  <- readRDS(file.path(map.sp.dir, "sp.cst.rds"))
lks  <- readRDS(file.path(map.sp.dir, "sp.lks.rds"))
lks.df <- sp_to_df(lks)

# Kosovo boundary overlay (outline only, no separate fill) -- matches make.world.map.R
rks <- readRDS(file.path(map.sp.dir, "sp.RKS.boarder.rds"))

bnd.line <- bnd[bnd$CARTOGRAPH == "International boundary line", ]
bnd.dash <- bnd[bnd$CARTOGRAPH %in% c("Dashed boundary line", "Undetermined international dashed boundary line"), ]
bnd.dot  <- bnd[bnd$CARTOGRAPH %in% c("Dotted boundary line", "Dotted boundary line (Abyei)"), ]
bnd.ssd  <- bnd[bnd$BDY_CNT01 == "SDN" & bnd$BDY_CNT02 == "SSD", ]

unpd.map <- function(world.robin) {
  # Colour countries
  world.robin$colorcode <- NoDataColor
  world.robin$colorcode[!is.na(world.robin$vartomap)] <- colors

  # Greenland inherits Denmark's colour (matches make.world.map.R)
  world.robin$colorcode[world.robin$TERR_NAME == "Greenland"] <-
    world.robin$colorcode[world.robin$TERR_NAME == "Denmark"]

  # Hong Kong inherits China's colour. Its own M49 code (344) differs from China's
  # (156), so the merge needs this patch; Taiwan is already 156 and Macau is left
  # as is, matching the reference map.
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
  # Kosovo: outline only, so it takes the colour of the polygon underneath
  plot(rks, add = TRUE, border = boundary.color, lwd = 0.2, lty = 2)

  # Lakes -- dashed border (lty = 2) matches the reference IGME map script's lake styling
  for (gp in unique(lks.df$group)) {
    lk <- lks.df[lks.df$group == gp, ]
    polygon(lk$long, lk$lat, col = background.color, border = boundary.color, lty = 2, lwd = 0.2)
  }
  lines(cst, col = coastline.color, lwd = 0.2, lty = 1)

  legend(-16820000, -1000000,
         col = c(colors, NoDataColor), pt.bg = c(colors, NoDataColor),
         pch = 15, pt.cex = 2, cex = 0.7,
         legend = legend.labels, title = legend.title,
         box.lty = 0, box.col = "white", bty = "o", bg = "white")
}

# Loop over equity dimensions and produce one map each
for (cfg in equity_map_config) {
  # Get countries with data for this equity dimension
  if (!is.null(cfg$iso3)) {
    # explicit ISO3 list (e.g. ethnicity, which is not in the disparity country file)
    map_countries <- cfg$iso3
  } else {
    src <- if (identical(cfg$source, "pre_agg")) disparity_pre_agg else disparity
    data_subset <- if (cfg$igme_only) {
      src %>% filter(Series.Name == "UN IGME" & equity == cfg$label)
    } else if (cfg$survey_only) {
      src %>% filter(Series.Name != "UN IGME" & equity == cfg$label)
    } else {
      src %>% filter(equity == cfg$label)
    }
    map_countries <- unique(data_subset$Country.Code)
  }

  # Merge on M49COLOR rather than ISO3 so territories filed under a parent
  # country's code inherit its colour, as in the reference maps
  map_m49 <- unique(iso3_to_m49$M49COLOR[iso3_to_m49$ISO3_CODE %in% map_countries])
  indata <- data.frame(M49COLOR = map_m49, vartomap = 1L)

  # Attach data to shapefile on M49COLOR
  world.robin <- merge(world.robin.base, indata, by = "M49COLOR", all.x = TRUE, all.y = FALSE)

  map_name <- cfg$map_name

  png(file.path(output.dir, paste0(map_name, ".png")),
      width = MAP_WIDTH_IN, height = MAP_HEIGHT_IN, units = "in", res = 300)
  par(mfrow = c(1,1), omi = c(0,0,0,0), mar = c(0,0,0,0),
      mgp = c(2,0.5,0), las = 0, mex = 1, cex = 1)
  unpd.map(world.robin)
  dev.off()

  # PDF at the map's natural size (no A3 whitespace)
  cairo_pdf(file.path(output.dir, paste0(map_name, ".pdf")),
            width = MAP_WIDTH_IN, height = MAP_HEIGHT_IN)
  par(mfrow = c(1,1), omi = c(0,0,0,0), mar = c(0,0,0,0),
      mgp = c(2,0.5,0), las = 0, mex = 1, cex = 1)
  unpd.map(world.robin)
  dev.off()

  message("Saved: ", map_name)
}
