# Recreate "Map 9": Under-five mortality rate at admin level 1 & 2, 2021
suppressMessages({
  library(sf); library(geojsonsf); library(data.table)
  library(dplyr); library(ggplot2); library(rnaturalearth); library(patchwork)
})
sf_use_s2(FALSE)

USERPROFILE <- Sys.getenv("USERPROFILE")
root <- file.path(USERPROFILE, "UNICEF/Child Mortality - Documents/UN IGME Subnational/Earlier Round/2023 Round Estimation")
code <- file.path(root, "_code_CC_subnational_2023")
shp  <- file.path(root, "shape_files")
sp1  <- file.path(shp, "admin1 only")
spc  <- file.path(shp, "special")
outdir <- file.path("figures", "Map 9 subnational U5MR admin1 admin2")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# ---- data ----
key <- as.data.table(readRDS(file.path(code, "data/dt_geo_key.rds")))
est <- fread(file.path(code, "output/Results_of_estimates_subnational.csv"))
e21 <- est[Shortind == "U5MR" & Year == 2021,
           .(ISO.Internal, Country.Code, Country.Name, Median)]
d <- merge(key[, .(ISO.Internal, Admin.Level, GADM.Region, GID_1, GID_2)],
           e21, by = "ISO.Internal")
d[, GADM.Region := as.character(GADM.Region)]
d[, Country.Code := as.character(Country.Code)]
d <- d[!(Country.Code == "TCD" & Admin.Level == "Admin2")]   # Chad admin2 excluded (as in original)
A1 <- d[Admin.Level == "Admin1"]; A2 <- d[Admin.Level == "Admin2"]

norm <- function(x) tolower(trimws(iconv(x, to = "ASCII//TRANSLIT")))
read_json <- function(f) st_set_crs(geojson_sf(f), 4326)
read_shp  <- function(f) st_transform(st_zm(st_read(f, quiet = TRUE)), 4326)

attach_val <- function(geo, dat, geo_key, dat_key = geo_key, by_norm = FALSE, cc = NA) {
  geo$.k <- if (by_norm) norm(geo[[geo_key]]) else geo[[geo_key]]
  dd <- copy(dat); dd$.k <- if (by_norm) norm(dd[[dat_key]]) else dd[[dat_key]]
  geo <- merge(geo, as.data.frame(dd)[, c(".k","Median")], by = ".k", all.x = TRUE)
  geo$cc <- cc
  geo[, c("Median","cc")]
}

std_both <- c("AGO","BEN","BGD","CMR","ETH","GIN","HTI","LBR","MDG","MLI",
              "MMR","MRT","NAM","PAK","RWA","SEN","TZA","ZMB","ZWE")
a1_only  <- c("KEN","LAO","LSO","NGA","TGO","TCD")

a1 <- list(); a2 <- list()
for (cc in std_both) {
  a1[[cc]] <- attach_val(read_json(file.path(shp,  sprintf("gadm41_%s_1.json",cc))), A1[Country.Code==cc], "GID_1", cc=cc)
  a2[[cc]] <- attach_val(read_json(file.path(shp,  sprintf("gadm41_%s_2.json",cc))), A2[Country.Code==cc], "GID_2", cc=cc)
}
for (cc in a1_only)
  a1[[cc]] <- attach_val(read_json(file.path(sp1, sprintf("gadm41_%s_1.json",cc))), A1[Country.Code==cc], "GID_1", cc=cc)

# specials (name joins)
a1[["GHA"]] <- attach_val(read_shp(file.path(spc,"Ghana/gadm40_GHA_shp/gadm40_GHA_1.shp")), A1[Country.Code=="GHA"], "NAME_1","GADM.Region",TRUE,"GHA")
a1[["BDI"]] <- attach_val(read_shp(file.path(spc,"Burundi/shapeFiles/bdi_admbnda_adm1_igebu_ocha_20171103.shp")), A1[Country.Code=="BDI"], "admin1Name","GADM.Region",TRUE,"BDI")
a1[["MWI"]] <- attach_val(read_shp(file.path(spc,"Malawi/shapeFiles_gadm/sdr_subnational_boundaries.shp")), A1[Country.Code=="MWI"], "DHSREGEN","GADM.Region",TRUE,"MWI")
{ g <- read_shp(file.path(spc,"Nepal/shapeFiles/npl_admbnda_adm1_nd_20180806.shp"))
  prov <- c("1"="Koshi Province","2"="Madhesh Province","3"="Bagmati Province","4"="Gandaki Province",
            "5"="Lumbini Province","6"="Karnali Province","7"="Sudurpashchim Province")
  g$provname <- prov[as.character(g$ADM1_EN)]
  a1[["NPL"]] <- attach_val(g, A1[Country.Code=="NPL"], "provname","GADM.Region",TRUE,"NPL") }
a1[["SLE"]] <- attach_val(read_shp(file.path(spc,"Sierra_Leone/shapeFiles/sle_adm1_province_mic.shp")), A1[Country.Code=="SLE"], "province","GADM.Region",TRUE,"SLE")
a1[["UGA"]] <- attach_val(read_shp(file.path(spc,"Uganda_new2023/Region/UDHS Regions 2019.shp")), A1[Country.Code=="UGA"], "Name","GADM.Region",TRUE,"UGA")

{ g <- read_shp(file.path(spc,"Burundi/shapeFiles/bdi_admbnda_adm2_igebu_ocha_20171103.shp"))
  g$admin2Name[g$admin2Name=="Kanyosha1"]<-"Kanyosha"; g$admin2Name[g$admin2Name=="Butaganzwa1"]<-"Butaganzwa"
  a2[["BDI"]] <- attach_val(g, A2[Country.Code=="BDI"], "admin2Name","GADM.Region",TRUE,"BDI") }
a2[["MWI"]] <- attach_val(read_shp(file.path(spc,"Malawi/shapeFiles_gadm/gadm36_MWI_1.shp")), A2[Country.Code=="MWI"], "NAME_1","GADM.Region",TRUE,"MWI")
a2[["NPL"]] <- attach_val(read_shp(file.path(spc,"Nepal/shapeFiles/npl_admbnda_districts_nd_20180806.shp")), A2[Country.Code=="NPL"], "DIST_EN","GADM.Region",TRUE,"NPL")
a2[["SLE"]] <- attach_val(read_shp(file.path(spc,"Sierra_Leone/shapeFiles/sle_adm2_districts_mic_cleaned.shp")), A2[Country.Code=="SLE"], "new_distri","GADM.Region",TRUE,"SLE")
{ g <- read_shp(file.path(spc,"Uganda_new2023/districts/uganda_districts.shp"))
  g$District <- sub("(.)","\\U\\1", tolower(g$District), perl=TRUE)
  a2[["UGA"]] <- attach_val(g, A2[Country.Code=="UGA"], "District","GADM.Region",TRUE,"UGA") }

admin1_sf <- do.call(rbind, a1); admin2_sf <- do.call(rbind, a2)

# published division / country counts (distinct estimate regions)
n1_div <- length(unique(A1$ISO.Internal)); n1_ctry <- length(unique(A1$Country.Code))
n2_div <- length(unique(A2$ISO.Internal)); n2_ctry <- length(unique(A2$Country.Code))
cat(sprintf("admin1: %d divisions / %d countries ; admin2: %d divisions / %d countries\n",
            n1_div, n1_ctry, n2_div, n2_ctry))
saveRDS(list(a1=admin1_sf, a2=admin2_sf,
             counts=c(n1_div=n1_div,n1_ctry=n1_ctry,n2_div=n2_div,n2_ctry=n2_ctry)),
        file.path(tempdir(),"map9_geo.rds"))

# ---- STYLING ----
brk <- c(-Inf,25,50,75,100,150,200,300,Inf)
lab <- c("0–25","25–50","50–75","75–100","100–150","150–200","200–300",">300")
col <- c("#4DAF4A","#CFF4FF","#FEEC9F","#FDC57E","#F98E1E","#D8620B","#A83E06","#5E2606")
names(col) <- lab
admin1_sf$cat <- cut(admin1_sf$Median, breaks=brk, labels=lab)
admin2_sf$cat <- cut(admin2_sf$Median, breaks=brk, labels=lab)

world <- ne_countries(scale="medium", returnclass="sf")
blue <- "#1CABE2"          # UNICEF cyan for captions
GREY <- "grey85"           # single grey used for ALL landmass + no-data regions
xlim <- c(-19,102); ylim <- c(-35,38)
hti  <- list(x=c(-75.3,-71.0), y=c(17.2,20.7))   # Haiti extent for the inset

base_panel <- function(reg, xl, yl, box=c("none","map","inset")) {
  box <- match.arg(box)
  g <- ggplot() +
    geom_sf(data=world, fill=GREY, color="white", linewidth=0.12) +
    geom_sf(data=reg, aes(fill=cat), color=NA) +                 # no subnational border lines
    scale_fill_manual(values=col, drop=FALSE, na.value=GREY, guide="none") +
    coord_sf(xlim=xl, ylim=yl, expand=FALSE) + theme_void()
  if (box=="inset") g <- g + theme(panel.border=element_rect(color="grey75", fill=NA, linewidth=0.4),
                                   plot.background=element_rect(fill="white", color=NA))
  if (box=="map")   g <- g + theme(panel.border=element_rect(color="grey80", fill=NA, linewidth=0.5))
  g
}
inset <- function(reg) base_panel(reg[reg$cc=="HTI",], hti$x, hti$y, box="inset")

boxed_map <- function(reg) base_panel(reg, xlim, ylim, box="map") +
  inset_element(inset(reg), left=0.008, bottom=0.01, right=0.20, top=0.28, align_to="panel")

cap_panel <- function(head, sub) ggplot() + theme_void() +
  coord_cartesian(xlim=c(0,1), ylim=c(0,1), clip="off") +
  annotate("text", x=0, y=0.95, hjust=0, vjust=1, label=head, color=blue,    size=6) +
  annotate("text", x=0, y=0.52, hjust=0, vjust=1, label=sub,  color="grey15", size=4.7)

p1 <- boxed_map(admin1_sf); p2 <- boxed_map(admin2_sf)
cap1 <- cap_panel("Administrative level 1", sprintf("%s divisions across\n%d countries", format(n1_div,big.mark=","), n1_ctry))
cap2 <- cap_panel("Administrative level 2", sprintf("%s divisions across\n%d countries", format(n2_div,big.mark=","), n2_ctry))

# horizontal stepped legend bar (smaller, with breathing room above/below)
tick <- c(0,25,50,75,100,150,200,300,600)
legdf <- data.frame(xmin=0:7, xmax=1:8, fill=factor(lab, levels=lab))
legend <- ggplot(legdf) +
  geom_rect(aes(xmin=xmin,xmax=xmax,ymin=0,ymax=1,fill=fill), color=NA) +
  scale_fill_manual(values=col, guide="none") +
  annotate("text", x=0:8, y=-0.55, label=tick, size=3.5, color="grey25") +
  annotate("text", x=4, y=-2.0, label="Deaths per 1,000 live births", size=3.9, color="grey25") +
  coord_cartesian(xlim=c(-0.4,8.4), ylim=c(-2.6,1.3), clip="off") +
  theme_void() +
  theme(plot.margin=margin(t=14, r=55, b=14, l=55))   # narrower bar + space above (from captions) & below

maps <- (p1   | plot_spacer() | p2)   + plot_layout(widths=c(1, 0.025, 1))  # smaller gap between the two boxes
caps <- (cap1 | plot_spacer() | cap2) + plot_layout(widths=c(1, 0.025, 1))
final <- maps / caps / legend + plot_layout(heights=c(1, 0.30, 0.26)) +
  plot_annotation(theme=theme(plot.margin=margin(3,4,3,4)))

ggsave(file.path(outdir,"Map_9.png"), final, width=8.5, height=5.0, dpi=300, bg="white")
ggsave(file.path(outdir,"Map_9.pdf"), final, width=8.5, height=5.0, device=cairo_pdf, bg="white")
cat("saved png + pdf to: ", outdir, "\n")
