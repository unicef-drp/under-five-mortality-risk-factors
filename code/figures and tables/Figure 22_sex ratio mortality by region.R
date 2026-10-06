# plot of 5-yr mortality rates by sex and region 
rm(list=ls())
library(dplyr)
calc.part.rate <- function(whole.rate, part.rate){ # rates should be provided as per 1,000 scale 
  part.rate <- 1-(1-whole.rate/1000)/(1-part.rate/1000)
  part.rate <- part.rate*1000
  return(part.rate)
}
USERPROFILE <- Sys.getenv("USERPROFILE")

# brochure figure output
fig.dir <- file.path("figures", "Figure 22 sex ratio")
if (!dir.exists(fig.dir)) dir.create(fig.dir, recursive = TRUE)

# Wide 2x9 panel grid, saved at about 16 x 5.33in.
PDF_WIDTH_IN  <- 16
PDF_HEIGHT_IN <- 16 / (21 / 7)   # keep the 3:1 aspect -> 5.33in
# ---- Region-level median rates from the combined IGME 2025 region file ----
# Only median rates are needed, so the combined file can stand in for the
# sync-restricted sex-specific folders. It is long format and already has CMR.
library(tidyr)
region_file <- file.path(USERPROFILE,
  "Dropbox/UNICEF Work/Data and charts for websites/Files 2025/CME/Estimates/UNIGME2025_Region_Rates_&_Deaths.csv")
rr <- read.csv(region_file, stringsAsFactors = FALSE, check.names = FALSE)

# reshape one (grouping, sex) into the wide layout the plotting code expects:
# columns Region, Year (integer), U5MR.median, IMR.median, CMR.median, ordered by year.
# "World" lives under its own Regional_Grouping, so it is pulled in alongside each grouping.
build_region_wide <- function(grouping, sex) {
  rr %>%
    filter(Shortind %in% c("U5MR", "IMR", "CMR"), Sex == sex,
           Regional_Grouping %in% c(grouping, "World")) %>%
    mutate(Year = floor(as.numeric(Year))) %>%   # 1990.5 -> 1990 to match integer plot years
    select(Region, Year, Shortind, Median) %>%
    pivot_wider(names_from = Shortind, values_from = Median) %>%
    rename(U5MR.median = U5MR, IMR.median = IMR, CMR.median = CMR) %>%
    arrange(Region, Year) %>%
    as.data.frame()
}
res.u5.sdg.t          <- build_region_wide("SDG", "Total")
res.u5.sdg.m          <- build_region_wide("SDG", "Male")
res.u5.sdg.f          <- build_region_wide("SDG", "Female")
res.u5.unicefreport.t <- build_region_wide("UNICEF", "Total")
res.u5.unicefreport.m <- build_region_wide("UNICEF", "Male")
res.u5.unicefreport.f <- build_region_wide("UNICEF", "Female")

regs.u5 <- c("Sub-Saharan Africa", "Oceania (exc. Australia and New Zealand)",
             "Central and Southern Asia", "Northern Africa and Western Asia",
             "Latin America and the Caribbean", "Eastern and South-Eastern Asia",
             "Europe and Northern America", "Australia and New Zealand", "World")

year.order.sr <- 1990
order.sr <- data.frame("regs"=res.u5.sdg.m[res.u5.sdg.m$Year%in%year.order.sr&res.u5.sdg.m$Region%in%regs.u5,"Region"], 
                       "sr"=res.u5.sdg.m[res.u5.sdg.m$Year%in%year.order.sr&res.u5.sdg.m$Region%in%regs.u5,"U5MR.median"]/res.u5.sdg.f[res.u5.sdg.f$Year%in%year.order.sr&res.u5.sdg.f$Region%in%regs.u5,"U5MR.median"])
order.sr <- rbind(order.sr[order(order.sr[1:nrow(order.sr)-1,"sr"]),],order.sr[nrow(order.sr),])

regs.order <- order.sr[,"regs"]


# SDG region print labels (looked up from regs.order so sorting never mismatches labels)
regs.print.map.sdg <- c(
  "Sub-Saharan Africa"                          = "Sub-Saharan Africa",
  "Oceania (exc. Australia and New Zealand)"    = "Oceania (excl.\nAustralia and New Zealand)",
  "Central and Southern Asia"                   = "Central and\nSouthern Asia",
  "Northern Africa and Western Asia"            = "Northern Africa\nand Western Asia",
  "Latin America and the Caribbean"             = "Latin America and\nthe Caribbean",
  "Eastern and South-Eastern Asia"              = "Eastern and\nSouth-Eastern Asia",
  "Europe and Northern America"                 = "Europe and\nNorthern America",
  "Australia and New Zealand"                   = "Australia and\nNew Zealand",
  "World"                                       = "World"
)
regs.order.print <- unname(regs.print.map.sdg[regs.order])

cbind(regs.order, regs.order.print)

# SDG region -- Figure 22, saved at natural size (16 x 5.33in, no whitespace padding)
pdf(file.path(fig.dir, "Figure_22_SexRatio_Rates_1990-2024_SDGregion.pdf"), width=PDF_WIDTH_IN, height=PDF_HEIGHT_IN)
plot.cols <- c("red", "#204878", "green4")
plot.years <- c(1990:2024)
par(mar=c(4,5,3,2.5), cex=1.15, mfrow=c(2,length(regs.order)))
for(i in 1:length(regs.order)){
  regs.order.i <- regs.order[i]
  plot(plot.years, res.u5.sdg.m[res.u5.sdg.m$Year%in%plot.years&res.u5.sdg.m$Region==regs.order.i,"U5MR.median"]/res.u5.sdg.f[res.u5.sdg.f$Year%in%plot.years&res.u5.sdg.f$Region==regs.order.i,"U5MR.median"], 
       type='l', ylim=c(0.7, 1.3), xlab="Year", ylab="Sex ratio (m/f)", col=plot.cols[1], bty='n', las=2, main=regs.order.print[i], xaxt="n")
  axis(1, at=c(seq(1990,2015,5),2024), c(seq(1990,2015,5),2024))
 points(plot.years, res.u5.sdg.m[res.u5.sdg.m$Year%in%plot.years&res.u5.sdg.m$Region==regs.order.i,"IMR.median"]/res.u5.sdg.f[res.u5.sdg.f$Year%in%plot.years&res.u5.sdg.f$Region==regs.order.i,"IMR.median"], type='l', col=plot.cols[2])
 points(plot.years, res.u5.sdg.m[res.u5.sdg.m$Year%in%plot.years&res.u5.sdg.m$Region==regs.order.i,"CMR.median"]/res.u5.sdg.f[res.u5.sdg.f$Year%in%plot.years&res.u5.sdg.f$Region==regs.order.i,"CMR.median"], type='l', col=plot.cols[3])
  if(i==9){
    legend(x='bottomright', legend=c("U5MR", "IMR", "CMR"), bty='n', ncol=1, cex=1.25, col=c(plot.cols), pch=c(16,16,16))
  }
}
for(i in 1:length(regs.order)){
  regs.order.i <- regs.order[i]
  plot(plot.years, res.u5.sdg.m[res.u5.sdg.m$Year%in%plot.years&res.u5.sdg.m$Region==regs.order.i,"U5MR.median"], 
       type='l', ylim=c(0, 200), xlab="Year", ylab="mortality rate (deaths per 1,000)", col=plot.cols[1], bty='n', las=2, xaxt="n")
  axis(1, at=c(seq(1990,2015,5),2024), c(seq(1990,2015,5),2024))
  points(plot.years, res.u5.sdg.f[res.u5.sdg.f$Year%in%plot.years&res.u5.sdg.f$Region==regs.order.i,"U5MR.median"], type = "l", lty=2, col=plot.cols[1])
  points(plot.years, res.u5.sdg.m[res.u5.sdg.m$Year%in%plot.years&res.u5.sdg.m$Region==regs.order.i,"IMR.median"], type='l', col=plot.cols[2])
  points(plot.years, res.u5.sdg.f[res.u5.sdg.f$Year%in%plot.years&res.u5.sdg.f$Region==regs.order.i,"IMR.median"], type='l', lty=2, col=plot.cols[2])
  points(plot.years, res.u5.sdg.m[res.u5.sdg.m$Year%in%plot.years&res.u5.sdg.m$Region==regs.order.i,"CMR.median"], type='l', col=plot.cols[3])
  points(plot.years, res.u5.sdg.f[res.u5.sdg.f$Year%in%plot.years&res.u5.sdg.f$Region==regs.order.i,"CMR.median"], type='l', lty=2, col=plot.cols[3])
  if(i==9){
    legend(x='topright', legend=c("Female", "Male"), bty='n', ncol=1, cex=1.25, col=c(1,1), lty=c(2,1))
  }
}
dev.off()

png(file.path(fig.dir, "Figure_22_SexRatio_Rates_1990-2024_SDGregion.png"), width=21*100, height=7*100, res=100)
par(mar=c(4,5,3,2.5), cex=1.15, mfrow=c(2,length(regs.order)))
for(i in 1:length(regs.order)){
  regs.order.i <- regs.order[i]
  plot(plot.years, res.u5.sdg.m[res.u5.sdg.m$Year%in%plot.years&res.u5.sdg.m$Region==regs.order.i,"U5MR.median"]/res.u5.sdg.f[res.u5.sdg.f$Year%in%plot.years&res.u5.sdg.f$Region==regs.order.i,"U5MR.median"], 
       type='l', ylim=c(0.7, 1.3), xlab="Year", ylab="Sex ratio (m/f)", col=plot.cols[1], bty='n', las=2, main=regs.order.print[i], xaxt="n")
  axis(1, at=c(seq(1990,2015,5),2024), c(seq(1990,2015,5),2024))
 points(plot.years, res.u5.sdg.m[res.u5.sdg.m$Year%in%plot.years&res.u5.sdg.m$Region==regs.order.i,"IMR.median"]/res.u5.sdg.f[res.u5.sdg.f$Year%in%plot.years&res.u5.sdg.f$Region==regs.order.i,"IMR.median"], type='l', col=plot.cols[2])
 points(plot.years, res.u5.sdg.m[res.u5.sdg.m$Year%in%plot.years&res.u5.sdg.m$Region==regs.order.i,"CMR.median"]/res.u5.sdg.f[res.u5.sdg.f$Year%in%plot.years&res.u5.sdg.f$Region==regs.order.i,"CMR.median"], type='l', col=plot.cols[3])
  if(i==9){
    legend(x='bottomright', legend=c("U5MR", "IMR", "CMR"), bty='n', ncol=1, cex=1.25, col=c(plot.cols), pch=c(16,16,16))
  }
}
for(i in 1:length(regs.order)){
  regs.order.i <- regs.order[i]
  plot(plot.years, res.u5.sdg.m[res.u5.sdg.m$Year%in%plot.years&res.u5.sdg.m$Region==regs.order.i,"U5MR.median"], 
       type='l', ylim=c(0, 200), xlab="Year", ylab="mortality rate (deaths per 1,000)", col=plot.cols[1], bty='n', las=2, xaxt="n")
  axis(1, at=c(seq(1990,2015,5),2024), c(seq(1990,2015,5),2024))
  points(plot.years, res.u5.sdg.f[res.u5.sdg.f$Year%in%plot.years&res.u5.sdg.f$Region==regs.order.i,"U5MR.median"], type = "l", lty=2, col=plot.cols[1])
  points(plot.years, res.u5.sdg.m[res.u5.sdg.m$Year%in%plot.years&res.u5.sdg.m$Region==regs.order.i,"IMR.median"], type='l', col=plot.cols[2])
  points(plot.years, res.u5.sdg.f[res.u5.sdg.f$Year%in%plot.years&res.u5.sdg.f$Region==regs.order.i,"IMR.median"], type='l', lty=2, col=plot.cols[2])
  points(plot.years, res.u5.sdg.m[res.u5.sdg.m$Year%in%plot.years&res.u5.sdg.m$Region==regs.order.i,"CMR.median"], type='l', col=plot.cols[3])
  points(plot.years, res.u5.sdg.f[res.u5.sdg.f$Year%in%plot.years&res.u5.sdg.f$Region==regs.order.i,"CMR.median"], type='l', lty=2, col=plot.cols[3])
  if(i==9){
    legend(x='topright', legend=c("Female", "Male"), bty='n', ncol=1, cex=1.25, col=c(1,1), lty=c(2,1))
  }
}
dev.off()

# UNICEF report region ordering (recompute regs.order for UNICEF regions)
regs.u5.unicef <- c("East Asia and Pacific", "Eastern and Southern Africa",
                    "Eastern Europe and Central Asia", "Europe and Central Asia",
                    "Latin America and Caribbean", "Middle East and North Africa",
                    "North America", "South Asia", "Sub-Saharan Africa",
                    "West and Central Africa", "Western Europe", "World")
order.sr.unicef <- data.frame(
  "regs" = res.u5.unicefreport.m[res.u5.unicefreport.m$Year %in% year.order.sr & res.u5.unicefreport.m$Region %in% regs.u5.unicef, "Region"],
  "sr"   = res.u5.unicefreport.m[res.u5.unicefreport.m$Year %in% year.order.sr & res.u5.unicefreport.m$Region %in% regs.u5.unicef, "U5MR.median"] /
           res.u5.unicefreport.f[res.u5.unicefreport.f$Year %in% year.order.sr & res.u5.unicefreport.f$Region %in% regs.u5.unicef, "U5MR.median"]
)
order.sr.unicef <- rbind(order.sr.unicef[order(order.sr.unicef[order.sr.unicef$regs != "World", "sr"]), ],
                         order.sr.unicef[order.sr.unicef$regs == "World", ])
regs.order <- order.sr.unicef[, "regs"]
regs.print.map.unicef <- c(
  "East Asia and Pacific"           = "East Asia and\nPacific",
  "Eastern and Southern Africa"     = "Eastern and\nSouthern Africa",
  "Eastern Europe and Central Asia" = "Eastern Europe\nand Central Asia",
  "Europe and Central Asia"         = "Europe and\nCentral Asia",
  "Latin America and Caribbean"     = "Latin America\nand Caribbean",
  "Middle East and North Africa"    = "Middle East and\nNorth Africa",
  "North America"                   = "North America",
  "South Asia"                      = "South Asia",
  "Sub-Saharan Africa"              = "Sub-Saharan Africa",
  "West and Central Africa"         = "West and\nCentral Africa",
  "Western Europe"                  = "Western Europe",
  "World"                           = "World"
)
regs.order.print <- unname(regs.print.map.unicef[regs.order])

# UNICEF report regions; not a numbered figure
pdf(file.path(fig.dir, "SexRatio_Rates_1990-2024_UNICEFReportregion.pdf"), width=PDF_WIDTH_IN, height=PDF_HEIGHT_IN)
plot.cols <- c("red", "#204878", "green4")
plot.years <- c(1990:2024)
par(mar=c(4,5,3,2.5), cex=1.15, mfrow=c(2,length(regs.order)))
for(i in 1:length(regs.order)){
  regs.order.i <- regs.order[i]
  plot(plot.years, res.u5.unicefreport.m[res.u5.unicefreport.m$Year%in%plot.years&res.u5.unicefreport.m$Region==regs.order.i,"U5MR.median"]/res.u5.unicefreport.f[res.u5.unicefreport.f$Year%in%plot.years&res.u5.unicefreport.f$Region==regs.order.i,"U5MR.median"], 
       type='l', ylim=c(0.7, 1.3), xlab="Year", ylab="Sex ratio (m/f)", col=plot.cols[1], bty='n', las=2, main=regs.order.print[i], xaxt="n")
  axis(1, at=c(seq(1990,2015,5),2024), c(seq(1990,2015,5),2024))
  points(plot.years, res.u5.unicefreport.m[res.u5.unicefreport.m$Year%in%plot.years&res.u5.unicefreport.m$Region==regs.order.i,"IMR.median"]/res.u5.unicefreport.f[res.u5.unicefreport.f$Year%in%plot.years&res.u5.unicefreport.f$Region==regs.order.i,"IMR.median"], type='l', col=plot.cols[2])
  points(plot.years, res.u5.unicefreport.m[res.u5.unicefreport.m$Year%in%plot.years&res.u5.unicefreport.m$Region==regs.order.i,"CMR.median"]/res.u5.unicefreport.f[res.u5.unicefreport.f$Year%in%plot.years&res.u5.unicefreport.f$Region==regs.order.i,"CMR.median"], type='l', col=plot.cols[3])
  if(i==length(regs.order)){
    legend(x='bottomright', legend=c("U5MR", "IMR", "CMR"), bty='n', ncol=1, cex=1.25, col=c(plot.cols), pch=c(16,16,16))
  }
}
for(i in 1:length(regs.order)){
  regs.order.i <- regs.order[i]
  plot(plot.years, res.u5.unicefreport.m[res.u5.unicefreport.m$Year%in%plot.years&res.u5.unicefreport.m$Region==regs.order.i,"U5MR.median"], 
       type='l', ylim=c(0, 200), xlab="Year", ylab="mortality rate (deaths per 1,000)", col=plot.cols[1], bty='n', las=2, xaxt="n")
  axis(1, at=c(seq(1990,2015,5),2024), c(seq(1990,2015,5),2024))
  points(plot.years, res.u5.unicefreport.f[res.u5.unicefreport.f$Year%in%plot.years&res.u5.unicefreport.f$Region==regs.order.i,"U5MR.median"], type = "l", lty=2, col=plot.cols[1])
  points(plot.years, res.u5.unicefreport.m[res.u5.unicefreport.m$Year%in%plot.years&res.u5.unicefreport.m$Region==regs.order.i,"IMR.median"], type='l', col=plot.cols[2])
  points(plot.years, res.u5.unicefreport.f[res.u5.unicefreport.f$Year%in%plot.years&res.u5.unicefreport.f$Region==regs.order.i,"IMR.median"], type='l', lty=2, col=plot.cols[2])
  points(plot.years, res.u5.unicefreport.m[res.u5.unicefreport.m$Year%in%plot.years&res.u5.unicefreport.m$Region==regs.order.i,"CMR.median"], type='l', col=plot.cols[3])
  points(plot.years, res.u5.unicefreport.f[res.u5.unicefreport.f$Year%in%plot.years&res.u5.unicefreport.f$Region==regs.order.i,"CMR.median"], type='l', lty=2, col=plot.cols[3])
  if(i==length(regs.order)){
    legend(x='topright', legend=c("Female", "Male"), bty='n', ncol=1, cex=1.25, col=c(1,1), lty=c(2,1))
  }
}
dev.off()

png(file.path(fig.dir, "SexRatio_Rates_1990-2024_UNICEFReportregion.png"), width=21*100, height=7*100, res=100)
par(mar=c(4,5,3,2.5), cex=1.15, mfrow=c(2,length(regs.order)))
for(i in 1:length(regs.order)){
  regs.order.i <- regs.order[i]
  plot(plot.years, res.u5.unicefreport.m[res.u5.unicefreport.m$Year%in%plot.years&res.u5.unicefreport.m$Region==regs.order.i,"U5MR.median"]/res.u5.unicefreport.f[res.u5.unicefreport.f$Year%in%plot.years&res.u5.unicefreport.f$Region==regs.order.i,"U5MR.median"], 
       type='l', ylim=c(0.7, 1.3), xlab="Year", ylab="Sex ratio (m/f)", col=plot.cols[1], bty='n', las=2, main=regs.order.print[i], xaxt="n")
  axis(1, at=c(seq(1990,2015,5),2024), c(seq(1990,2015,5),2024))
  points(plot.years, res.u5.unicefreport.m[res.u5.unicefreport.m$Year%in%plot.years&res.u5.unicefreport.m$Region==regs.order.i,"IMR.median"]/res.u5.unicefreport.f[res.u5.unicefreport.f$Year%in%plot.years&res.u5.unicefreport.f$Region==regs.order.i,"IMR.median"], type='l', col=plot.cols[2])
  points(plot.years, res.u5.unicefreport.m[res.u5.unicefreport.m$Year%in%plot.years&res.u5.unicefreport.m$Region==regs.order.i,"CMR.median"]/res.u5.unicefreport.f[res.u5.unicefreport.f$Year%in%plot.years&res.u5.unicefreport.f$Region==regs.order.i,"CMR.median"], type='l', col=plot.cols[3])
  if(i==length(regs.order)){
    legend(x='bottomright', legend=c("U5MR", "IMR", "CMR"), bty='n', ncol=1, cex=1.25, col=c(plot.cols), pch=c(16,16,16))
  }
}
for(i in 1:length(regs.order)){
  regs.order.i <- regs.order[i]
  plot(plot.years, res.u5.unicefreport.m[res.u5.unicefreport.m$Year%in%plot.years&res.u5.unicefreport.m$Region==regs.order.i,"U5MR.median"], 
       type='l', ylim=c(0, 200), xlab="Year", ylab="mortality rate (deaths per 1,000)", col=plot.cols[1], bty='n', las=2, xaxt="n")
  axis(1, at=c(seq(1990,2015,5),2024), c(seq(1990,2015,5),2024))
  points(plot.years, res.u5.unicefreport.f[res.u5.unicefreport.f$Year%in%plot.years&res.u5.unicefreport.f$Region==regs.order.i,"U5MR.median"], type = "l", lty=2, col=plot.cols[1])
  points(plot.years, res.u5.unicefreport.m[res.u5.unicefreport.m$Year%in%plot.years&res.u5.unicefreport.m$Region==regs.order.i,"IMR.median"], type='l', col=plot.cols[2])
  points(plot.years, res.u5.unicefreport.f[res.u5.unicefreport.f$Year%in%plot.years&res.u5.unicefreport.f$Region==regs.order.i,"IMR.median"], type='l', lty=2, col=plot.cols[2])
  points(plot.years, res.u5.unicefreport.m[res.u5.unicefreport.m$Year%in%plot.years&res.u5.unicefreport.m$Region==regs.order.i,"CMR.median"], type='l', col=plot.cols[3])
  points(plot.years, res.u5.unicefreport.f[res.u5.unicefreport.f$Year%in%plot.years&res.u5.unicefreport.f$Region==regs.order.i,"CMR.median"], type='l', lty=2, col=plot.cols[3])
  if(i==length(regs.order)){
    legend(x='topright', legend=c("Female", "Male"), bty='n', ncol=1, cex=1.25, col=c(1,1), lty=c(2,1))
  }
}
dev.off()

# World-level U5MR sex ratio (male/female), 2000 and 2023
res.u5.sdg.m$U5MR.median[res.u5.sdg.m$Region=="World" & res.u5.sdg.m$Year==2000]/res.u5.sdg.f$U5MR.median[res.u5.sdg.f$Region=="World" & res.u5.sdg.f$Year==2000]
res.u5.sdg.m$U5MR.median[res.u5.sdg.m$Region=="World" & res.u5.sdg.m$Year==2023]/res.u5.sdg.f$U5MR.median[res.u5.sdg.f$Region=="World" & res.u5.sdg.f$Year==2023]

