# Trend analysis: project each country's equity estimates to 2000 and 2024,
# benchmark them to the UN IGME 2025-round national estimates, and aggregate to a
# world rate per dimension and group.
#
# Outputs, in output/aggregate/:
#   aggregates_2000_2024_from_extrapolation_benchmarked.xlsx    world, long and wide
#   country_projection_2000_2024_from_extrapolation_benchmarked.xlsx
#   trend_summary_nc_2000_2024_PD.csv / .xlsx                   countries, rates, percent decline
#   two diagnostic PDFs, written only when MAKE_DIAGNOSTIC_PLOTS is TRUE
#
# The wide world sheet carries the 2000 and 2024 rates by group with percent
# decline and ARR, which the report's trend claims draw on.
#
# Steps
# 1. Take the most recent point from each survey.
# 2. Keep countries with at least three points for a dimension (47 to 56 of them).
# 3. Fit a linear model per country and dimension on log rates, and extrapolate
#    to 2000 and 2024.
# 4. Benchmark to the IGME national deaths, assuming live births are spread
#    evenly across the groups of a dimension.
# 5. Sum adjusted deaths and births across countries for the world rate.


library("dplyr")
library("data.table")
library("ggplot2")
library("gridExtra")
library("grid")

USERPROFILE <- Sys.getenv("USERPROFILE")
MAKE_DIAGNOSTIC_PLOTS <- FALSE   # set TRUE to also render the country/label diagnostic PDFs

# profile.R sets dir_IGME
source(file.path(USERPROFILE, "Dropbox/UNICEF Work/profile.R"))

# Percent decline and annual rate of reduction between two year columns
calculate.pd <- function(dt, year1, year2) {
  dt[, pd := (get(as.character(year2)) / get(as.character(year1)) - 1) * -100]
  setnames(dt, "pd", paste0("PD", year1, "-", year2))
  dt
}
calculate.arr <- function(dt, year1, year2) {
  drop_half <- function(y) as.numeric(sub("\\.5$", "", as.character(y)))
  dt[, arr := log(get(as.character(year2)) / get(as.character(year1))) /
       (drop_half(year2) - drop_half(year1)) * -100]
  setnames(dt, "arr", paste0(year1, "-", year2))
  dt
}

# Data vintage, kept in sync with 01
DB_DATE_PATTERN <- "2025-05-01"            # pattern to match raw equity DB files
DB_CUTOFF_DATE  <- as.Date("2026-02-01")   # exclude surveys added after this (edu exempt)

eq_root     <- "."                      # scripts run from the project root
dir_output  <- file.path(eq_root, "output/aggregate")

# Newest file matching a pattern, so date stamps can change
latest_file <- function(dir, pattern) {
  fs <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!length(fs)) stop("No file matching ", pattern, " in ", dir)
  fs[which.max(file.mtime(fs))]
}

# Selected-survey list from the current pipeline output
dt <- as.data.frame(fread(latest_file(file.path(eq_root, "output/country"), "^country-specific-database-cleaned_[0-9].*\\.csv$")))

# Raw multi-survey history: the trend fit needs every time point, unlike the
# cleaned output, which keeps one survey per country and dimension.
dt.dir <- file.path(USERPROFILE, "Dropbox/UNICEF Work/Equity database/Databases/Databases 2025/")
file_names <- list.files(dt.dir)
file_names <- grep(paste0("^data_.*", DB_DATE_PATTERN), file_names, value = TRUE)

eq <- c("bi","bo","bs","medu","mage","res","wealth")
order_eq<-c("Wealth quintile","Mother's education","Residence","Mother's age","Birth size","Birth interval","Birth order")

order_gr<-c("1.quintile","2.quintile","3.quintile","4.quintile","5.quintile","noedu.primary", "sec.higher", "noeducation","primary","secondary","higher", 
            "rural","urban", "<20","20-29","30-39","40-49","small.vsmall","av.large", "<2","2","3","4+" ,"1_","2-3_","4+_")

labels_gr <- c("1.quintile","2.quintile","3.quintile","4.quintile","5.quintile","no education/primary", "secondary/higher", "no education","primary",
             "secondary","higher",
             "rural","urban", "<20","20-29","30-39","40-49","small/vsmall","average/large", "<2","2","3",">=4" ,"1_","2-3_","4+_")

color_gr <- c("skyblue","cyan","blue","royalblue","midnightblue","red","orange","darkred","red","darkorange","yellow",
            "darkgreen","darkkhaki",
            "purple","orchid","pink","salmon","orangered","gold","#999900","#DDD000","#FFFF00","#CC9933" ,"#666600","#CCFF33","#99FF00")

color_group <- color_gr
names(color_group) <- labels_gr

alldta<-c()
for (i in (1:length(file_names))){
dta<-read.csv(file.path(dt.dir, file_names[i]),stringsAsFactors = F)
alldta<-rbind(alldta,dta)
}
cols_wanted <- copy(colnames(alldta))

# Prefer Visible == 2
alldta <- alldta[alldta$Visible==2 & alldta$Inclusion.U5MR==1,] 

# Surveys added after DB_CUTOFF_DATE are inspected but deliberately kept: the
# trend fit benefits from every available time point.
alldta <- alldta %>% mutate(date.of.data.added = as.Date(paste(added, "01", sep = "-"), format = "%Y-%m-%d"))
alldta[alldta$date.of.data.added > DB_CUTOFF_DATE,]
nrow(alldta) # 2024.09:264886 # 2025.01:286156 # 2025.06: 294166 # 2026.04: 302028


# Merge datasets
dt$equity <- dt$label
smry.list <- unique(dt[c(
  "Country.Name",
  "Country.Code",
  "Series.Year",
  "Series.Category",
  "Series.Type",
  "label"
)]) 
nrow(smry.list) # 443
dtid <- with(smry.list,
             paste0(Country.Code, Series.Category, Series.Type, Series.Year, label))
alldta$id <- with(alldta,
                  paste0(Country.Code, Series.Category, Series.Type, Series.Year, label))
alldta$selected <- with(alldta, ifelse(id %in% dtid, 1, 0))
dts <- alldta[alldta$selected == 1, ]

smry.list.s<-unique(dts[c("Country.Name","Country.Code", "Series.Name", "Series.Year","Series.Category","Series.Type","label")]) #454
allsmry<-as.data.frame(table(smry.list.s$Country.Code,smry.list.s$label))
selsmry<-as.data.frame(table(smry.list$Country.Code,smry.list$label))

missdt<-merge(selsmry,allsmry,by=c("Var1","Var2"),all=T)
missdt$diff<-missdt$Freq.x-missdt$Freq.y
missdt<-filter(missdt,diff!=0) # empty 

# Trends in selected countries 
alldta$idt<-with(alldta,paste0(Country.Code,label))
dt$idt<-with(dt,paste0(Country.Code,label))

# Drop totals on a different reference period, and indirect, census, other
# non-standard and VR series
dtt <- filter(
  alldta,
  alldta$idt %in% dt$idt,
  Indicator %in% c("U5MR", "NMR", "nbirth"),
  Group != "period2", !Series.Type %in% c("Indirect", "Direct (Single year)", "VR")
)

dtt_lb <- dtt[dtt$Indicator == "nbirth",]

# ---- 1. Most recent point per series ----
dtt <- dtt %>%
  mutate(
    S.Year = as.numeric(ifelse(nchar(Series.Year) > 4, substr(Series.Year, 6, 9), Series.Year)),
    ind = ifelse(Indicator != "nbirth", "MR", Indicator)
  )


# Most recent data point per series
dtt <- group_by(dtt, label, Country.Code, Series.Name, S.Year, Series.Type, ind) %>%
  mutate(max_y=ifelse(ind=="MR",max(Reference.Date),max(S.Year)),
         yr=ifelse(ind=="MR",Reference.Date,S.Year))%>%
  filter(yr==max_y)%>%
  mutate(Interval2=ifelse(ind=="MR",(S.Year-Reference.Date)*2,NA))


setDT(dtt)
dtt[,.N] # 2025-07: 31835

# Both education labels need a total
dtt[label == "edu", table(label, Group)]
dtt[label == "edu" & !Group %in% c("noedu.primary", "sec.higher"), label:= "edu_group_1"] 
dtt[label == "edu" & Group %in% c("noedu.primary", "sec.higher"), label:= "edu_group_2"]
dtt_total <- dtt[label == "edu_group_1" & Group == "total"][, label := "edu_group_2"]
dtt <- rbind(dtt, dtt_total)
isos <- dtt[order(Country.Name), unique(Country.Code)]

# ---- 2. Countries with enough data points ----
dtt[, table(Indicator)]
dt1 <- dtt[Indicator %in% c("U5MR", "NMR")]
labs <- unique(dt1$label)

# Define prediction years
pred_years <- data.table(Reference.Date = c(2000, 2024))

dt1[, Log_Est := log(Estimates)]

# Drop data before 2000
dt1[Reference.Date<2000 ,.N] # 4360 

# Keep countries with 2 or 3 points
dt1[, uniqueN(Country.Code)] # 73
dt1[, n_data := .N, by = .(Country.Code, Indicator, label, Group)]
dt1[, table(n_data)]

# With more than 3 points, data before 1990 can go

c1 <- dt1[, .(all = uniqueN(Country.Code)), by = .(label)]
c2 <- dt1[n_data >= 2, .(c2 = uniqueN(Country.Code)), by = .(label)]
c3 <- dt1[n_data >= 3, .(c3 = uniqueN(Country.Code)), by = .(label)]
dt_n_countries <- Reduce(merge, list(c1, c2, c3))
dt_n_countries[, deduct := c3/all] # about 70% of the countries have 3 data points, so are used for the projection

# limit to at least three data points 
dt1[,.N] # 14368
dt1[Group!= "total",.N] # 14368

c1 <- dt1[, .(all = uniqueN(Country.Code)), by = .(label)]
c2 <- dt1[n_data >= 2, .(c2 = uniqueN(Country.Code)), by = .(label)]
c3 <- dt1[n_data >= 3, .(c3 = uniqueN(Country.Code)), by = .(label)]
dt_n_countries <- Reduce(merge, list(c1, c2, c3))
dt_n_countries[, deduct := c3/all] # about 70% of the countries have 3 data points, so are used for the projection

dt1[n_data >= 3 & Reference.Date<=1990,.N] #  --- exclude
dt1 <- dt1[!(n_data >= 3 & Reference.Date<=1990)]
dt1[, n_data := .N, by = .(Country.Code, Indicator, label, Group)]
dt1 <- dt1[n_data>=3]
dt1[Group!= "total",.N] # 11647
dt1[, uniqueN(Country.Code)] # 59


# Benchmarking needs every group, including the total
dt1[, ngroup:= uniqueN(Group), by = .(label)]
dt1[, ngroupc := uniqueN(Group), by = .(Country.Code, Indicator, label, Reference.Date)]
dt1[ngroup!=ngroupc] # miss groups 
dt1 <- dt1[ngroup == ngroupc] # keep only those with all groups

dt1[Group!= "total",.N] # 11191
dt1[, n_data := .N, by = .(Country.Code, Indicator, label, Group)]
c1 <- dt1[, .(all = uniqueN(Country.Code)), by = .(label)]
c2 <- dt1[n_data >= 2, .(c2 = uniqueN(Country.Code)), by = .(label)] # if require 2 dps
c3 <- dt1[n_data >= 3, .(c3 = uniqueN(Country.Code)), by = .(label)] # if require at least 3 dps
dt_n_countries2 <- Reduce(merge, list(c1, c2, c3))
dt_n_countries2[, deduct := c3/all] # about 70% of the countries have 3 data points, so are used for the projection
dt_n_countries2



dt1 <- dt1[n_data>=3]
dt1[,.N] #  11091
dt1[, uniqueN(Country.Code)] # 55

uniqueN(dt1$Group) # 24 
unique(dt1$Group) 

# extract the total
group_order <- c(
  "total",
  "<2", "2", "3", "4+", 
  "1_", "2-3_", "4+_", 
  "noeducation", "primary", "secondary", "higher",
  "noedu.primary", "sec.higher",
  "<20", "20-29", "30-39", "40-49", 
  "rural","urban", 
  "1.quintile", "2.quintile", "3.quintile", "4.quintile", "5.quintile"
)

dt1[, Group := factor(as.factor(Group), levels = group_order)]

# label order
label_order <- c("edu_group_1", "edu_group_2", "residence", "wealth", "motherage", "birthinterval", "birthorder")

# Live births from the WPP reference file under dir_IGME, keyed by UN code and
# mapped to ISO3 through country.info.CME.csv.
lb_file <- file.path(dir_IGME, "2025 Round Estimation/Code/input/data_livebirths.csv")
lb_iso  <- fread(file.path(dir_IGME, "2025 Round Estimation/Code/input/country.info.CME.csv"),
                 select = c("ISO3Code", "UNCode"))
dtlb <- merge(fread(lb_file)[sex == "b"], lb_iso, by.x = "uncode", by.y = "UNCode")
dtlb <- dtlb[!is.na(ISO3Code) & ISO3Code != ""]
dtlb[, Year:= floor(year)]
dtlb <- dtlb[Year %in% pred_years$Reference.Date, .(lb = sum(lb)), by = .(ISO3Code, Year)]

# lb from high to low for plotting 
isos_order <- dtlb[order(lb, decreasing = TRUE)][Year == 2024, ISO3Code]
setnames(dtlb, c("ISO3Code", "Year", "lb"), c("Country.Code", "Reference.Date", "lb.wpp"), skip_absent = TRUE) # <- wpp lb

# ---- 3. Linear projection per country ----
# Fitted by label on log estimates, then converted back.
pred.by.label <- function(lab0){
  print(lab0)
  dt1_sub <- dt1[label == lab0]
  setorder(dt1_sub, Group)
  suppressWarnings({
  dt1_pred <- dt1_sub[, {
    if (.N > 1) {
      model <- lm(Log_Est ~ Reference.Date, data = .SD)
      preds <- predict(model, newdata = pred_years)
      .(Country.Code, Reference.Date, Log_Est, Indicator, label, Group, log_predicted_2000 = preds[1], log_predicted_2024 = preds[2])
    } else {
      .(Country.Code, Reference.Date, Log_Est, Indicator, label, Group, log_predicted_2000 = NA_real_, log_predicted_2024 = NA_real_)
    }
  }, by = .(Country.Code, Indicator, label, Group)]
  })
  dt1_pred[, `:=`(predicted_2000 = exp(log_predicted_2000), predicted_2024 = exp(log_predicted_2024))]
  dt1_pred <- unique(dt1_pred[,.(Country.Code, Indicator, label, Group, predicted_2000, predicted_2024)])
  return(dt1_pred)
}
dt1_pred <- rbindlist(lapply(labs, pred.by.label))
dt1_pred[predicted_2024<0] # none



# ---- 4. Benchmark to the IGME national rates ----
# Group estimate / total in the equity database, times the IGME median, taken
# from the 2025-round country rates and deaths file.
cme_country_file <- file.path(USERPROFILE,
  "Dropbox/UNICEF Work/Data and charts for websites/Files 2025/CME/Estimates/UNIGME2025_Country_Rates_&_Deaths.csv")
dtu5c <- fread(cme_country_file)[Sex == "Total" & Shortind %in% c("NMR", "U5MR")]
dtu5c[, Year:= floor(Year)]
dtu5c <- dtu5c[Year %in% pred_years$Reference.Date, .(`ISO.Code`, Year, Shortind, Median)]
setnames(dtu5c, c("ISO.Code", "Shortind", "Year", "Median"),
         c("Country.Code", "Indicator", "Reference.Date", "rate.IGME")) # keep these column names so we can bind back later
setkey(dtu5c, Country.Code, Indicator, Reference.Date)

# merge with dt1_pred_l
# dt1_pre in long format 
dt1_pred_l <- melt(dt1_pred, id.vars = c("Country.Code", "Indicator", "label", "Group"), variable.name = "Reference.Date", value.name = "Estimates")
dt1_pred_l[, Reference.Date:= as.numeric(gsub("predicted_", "", Reference.Date))]

setkey(dt1_pred_l, Country.Code, Indicator, Reference.Date)
dt1_pred_l <- dtu5c[dt1_pred_l]

setkey(dt1_pred_l, Country.Code, Reference.Date)
setkey(dtlb, Country.Code, Reference.Date)
dt1_pred_l <- dtlb[dt1_pred_l]
# for wealth, assume equal distribution of live births across groups
setkey(dt1_pred_l, Country.Code, label, Group)


# ---- lb from data ----
dt_nbirth <- dtt[Indicator == "nbirth"]
uniqueN(dt_nbirth$Group) # 36 
unique(dt_nbirth$Group) 
dt_nbirth <- dt_nbirth[Group %in% group_order]
dt_nbirth[, Group := factor(as.factor(Group), levels = group_order)]

dt_nbirth[, ngroup:= uniqueN(Group), by = .(label)]
dt_nbirth[, ngroupc:= uniqueN(Group), by = .(Country.Code, DataCatalogID, label, Interval)]
dt_nbirth[, uniqueN(Country.Code)]
dt_nbirth[ngroup == ngroupc, uniqueN(Country.Code)]
dt_nbirth[ngroup == ngroupc & !is.na(Interval), uniqueN(Country.Code)]
dt_nbirth <- dt_nbirth[ngroup == ngroupc] # keep only those with all groups (for all dimensions)

dt_nbirth[is.na(Interval) & label != "wealth", table(IGME_Key, label)]
dt_nbirth[is.na(Interval), Interval := 5]

dt_nbirth[, Int_ava := paste(sort(unique(Interval)), collapse = "_"), by = .(Country.Code, label, Group)]
dt_nbirth[, table(Int_ava)]
dt_nbirth <- dt_nbirth[Interval == 5]

# Most recent year per country and label
dt_nbirth[, max_series_year := max(Series.Year), by = .(Country.Code, label)]
dt_nbirth <- dt_nbirth[max_series_year == Series.Year]

dt_nbirth[, N := .N, by = .(Country.Code, label, Group)]
dt_nbirth[N>1] # inspect any duplicates (same Series.Year, different surveys)
dt_nbirth_sub <- dt_nbirth[, .(Estimates = mean(Estimates)), by = .(Country.Code, label, Group)]
setnames(dt_nbirth_sub, "Estimates", "lb_data")
setkey(dt_nbirth_sub, Country.Code, label, Group)

dt1_pred_lb <- dt_nbirth_sub[dt1_pred_l]
dt1_pred_lb[is.na(lb_data) & label != "wealth"] # a few countries, drop 
dt1_pred_lb <- dt1_pred_lb[!(is.na(lb_data) & label != "wealth")]

# extract the total 
dt1_pred_total <- dt1_pred_lb[Group == "total",.(Country.Code, Indicator, label, Reference.Date, lb_data_total = lb_data, Estimates_total = Estimates)]
dt1_pred_total[, table(label)] 
dt1_pred_lb <- dt1_pred_lb[Group != "total"]

setkey(dt1_pred_lb, Country.Code, Indicator, label, Reference.Date)
setkey(dt1_pred_total, Country.Code, Indicator, label, Reference.Date)
dt1_pred_lb <- dt1_pred_total[dt1_pred_lb]

# calculate live births by group
dt1_pred_lb[, lb_pcnt_in_c := lb_data / sum(lb_data), by = .(Country.Code, Indicator, label, Reference.Date)]
dt1_pred_lb[, lb_from_data := lb.wpp * lb_pcnt_in_c]

dt1_pred_lb[, ngroup:= uniqueN(Group), by = .(label)]
dt1_pred_lb[, lb_equal_dist := lb.wpp / ngroup] # for wealth assume equal distribution of live births across groups
dt1_pred_lb[, wgt.birth := ifelse(label == "wealth", lb_equal_dist, lb_from_data)]

# adjusted rates 
dt1_pred_lb[, wgt.rate  := Estimates / Estimates_total * rate.IGME]
            
# adjusted death
dt1_pred_lb[, wgt.death := wgt.birth*wgt.rate/1000]
            
dt1_pred_lb[is.na(wgt.death)]
dt1_pred_lb <- dt1_pred_lb[!is.na(wgt.death)]

# ---- 5. Aggregate to the world level ----
dt1_pred_lb[, value := sum(wgt.death) / sum(wgt.birth) * 1000, by = .(Indicator, label, Group, Reference.Date)]

dt1_pred_copy <- copy(dt1_pred_lb)
dt1_pred_plot <- rbindlist(list(dt1[, type := ""], dt1_pred_copy[, type := "predicted"]), fill = TRUE, use.names = TRUE)

# number of countries with data 
dt1_pred_lb[, nc:= uniqueN(Country.Code), by = .(label)]

dt1_agg_world <- unique(dt1_pred_lb[,.(Indicator, label, Group, Reference.Date, value, nc)]) 
setorder(dt1_agg_world, -Indicator, -Reference.Date, Group)
dt1_agg_world[Reference.Date==2024 & Indicator == "U5MR" & label == "wealth"]


dt1_agg_worldw <- dcast(dt1_agg_world, Indicator + label + Group  ~ Reference.Date, value.var = "value")
dt1_agg_worldw[, `2030`:= ifelse(Indicator == "U5MR", 25, 12)]

dt1_agg_worldw <- calculate.pd(dt1_agg_worldw, 2000, 2024)
dt1_agg_worldw <- calculate.arr(dt1_agg_worldw, 2000, 2024)
dt1_agg_worldw <- calculate.arr(dt1_agg_worldw, 2024, 2030)

writexl::write_xlsx(list(dt1_agg_world, dt1_agg_worldw), file.path(dir_output, "aggregates_2000_2024_from_extrapolation_benchmarked.xlsx"))



setorder(dt1_pred_lb, -Indicator, -Reference.Date, Group)
writexl::write_xlsx(dt1_pred_lb, file.path(dir_output, "country_projection_2000_2024_from_extrapolation_benchmarked.xlsx"))


# ---- 5.1 Trend summary ----
# One row per dimension, group and indicator: countries, 2000 and 2024, decline.
label_names <- c(edu_group_1 = "Mother's education (4 groups)", edu_group_2 = "Mother's education (2 groups)",
                  residence = "Residence", wealth = "Wealth quintile", motherage = "Mother's age",
                  birthinterval = "Birth interval", birthorder = "Birth order")

trend_summary <- copy(dt1_agg_worldw)
trend_summary[, `:=`(label_full = label_names[label])]
setnames(trend_summary, c("2000", "2024"), c("rate_2000", "rate_2024"))
trend_summary <- trend_summary[Group != "total"]
trend_summary <- trend_summary[, .(label, label_full, Indicator, Group, rate_2000, rate_2024,
                                    `PD2000-2024`, `2000-2024`, `2024-2030`)]
setnames(trend_summary, c("PD2000-2024", "2000-2024", "2024-2030"),
         c("PD_2000_2024_pct", "ARR_2000_2024", "ARR_2024_2030"))
trend_summary <- merge(trend_summary,
                        unique(dt1_agg_world[, .(label, Indicator, nc)]),
                        by = c("label", "Indicator"), all.x = TRUE)
setcolorder(trend_summary, c("label", "label_full", "Indicator", "Group", "nc",
                              "rate_2000", "rate_2024", "PD_2000_2024_pct", "ARR_2000_2024", "ARR_2024_2030"))
setorder(trend_summary, label, Indicator, Group)

cat("\n=== Trend summary: number of countries, 2000 & 2024 rates, PD ===\n")
print(trend_summary)

fwrite(trend_summary, file.path(dir_output, "trend_summary_nc_2000_2024_PD.csv"))
writexl::write_xlsx(trend_summary, file.path(dir_output, "trend_summary_nc_2000_2024_PD.xlsx"))


if (!MAKE_DIAGNOSTIC_PLOTS) {
  message("MAKE_DIAGNOSTIC_PLOTS is FALSE -- skipping diagnostic PDF plotting (set TRUE to render).")
} else {

# ---- 6.1 Country-level diagnostics ----
# Raw points and fitted values per country and label.
plot.by.iso <- function(iso0, ind0 = "U5MR"){
  db1 <- dt1_pred_plot[Country.Code == iso0 & Indicator == ind0 & Group != "total"]
  max_y <- max(db1$Estimates, na.rm = TRUE)
  min_x <- min(db1$Reference.Date, na.rm = TRUE)
  groups <- unique(db1$label)
  groups <- groups[order(match(groups, label_order))]
  plot.group <- function(group0){
    db1g1 <- db1[label == group0]
    ggplot(db1g1[type == ""], aes(x = Reference.Date, y = Estimates, color = Group)) + 
      scale_x_continuous(limits = c(min_x, 2024), breaks = seq(min_x, 2024, 5)) +
      scale_y_continuous(limits = c(0, max_y)) +
      geom_line() + 
      geom_point() + 
      geom_line(data = db1g1[type == "predicted"], aes(x = Reference.Date, y = Estimates), linetype = "dashed") +
      labs(title = db1g1$label[1], x = "Year", y = "Estimates") + 
      theme_minimal() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  }
  ggs <- lapply(groups, plot.group)
  suppressMessages(suppressWarnings(
    gridExtra::marrangeGrob(grobs = ggs, layout_matrix = matrix(1:9, byrow = TRUE, nrow = 3),
                            top = paste(db1$Country.Name[1]))))
  
}
plot.by.iso(iso0 = "AFG")

plot.by.label <- function(label0, ind0 = "U5MR"){
  stopifnot(label0 %in% label_order)
  db1 <- dt1_pred_plot[label == label0 & Indicator == ind0 & Group != "total"]
  max_y <- max(db1$Estimates, na.rm = TRUE)
  min_x <- min(db1$Reference.Date, na.rm = TRUE)
  isos <- unique(db1$Country.Code)
  isos <- isos[order(match(isos, isos_order))]
  plot.iso <- function(iso0){
    db1g1 <- db1[Country.Code == iso0]
    ggplot(db1g1[type == ""], aes(x = Reference.Date, y = Estimates, color = Group)) + 
      scale_x_continuous(limits = c(min_x, 2024), breaks = seq(min_x, 2024, 5)) +
      scale_y_continuous(limits = c(0, NA)) +
      geom_line() + 
      geom_point() + 
      geom_line(data = db1g1[type == "predicted"], aes(x = Reference.Date, y = Estimates), linetype = "dashed") +
      labs(title = db1g1$Country.Name[1], x = "Year", y = "Estimates") + 
      theme_minimal() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  }
  ggs <- lapply(isos, plot.iso)
  suppressMessages(suppressWarnings(
    gridExtra::marrangeGrob(grobs = ggs, layout_matrix = matrix(1:20, byrow = TRUE, nrow = 5),
                            top = paste(db1$label[1]))))
  
}
grid_list2 <- lapply(label_order, plot.by.label)
pdf(file.path(dir_output, "data_pred_to_2000_2024_countries_by_label_3_datapoints.pdf"), width = 20, height = 20)  # Specify the file name and dimensions
for (grid in grid_list2) {
  grid.draw(grid)
}
dev.off()


# ---- 6.2 Aggregate diagnostics ----
# Benchmarked values, the "value" column.

dt1_agg_l <- dt1_agg_world
dt1_agg_l[, label := factor(label, levels = label_order)]
y_max <- max(dt1_agg_l$value) * 1.1

plot.by.label <- function(label0, fixed_y = TRUE){
  db1_agg0 <- dt1_agg_l[label == label0]
  g <- ggplot(db1_agg0, aes(x = Reference.Date, y = value, color = Group, shape = Group)) + 
    geom_line(linewidth = 0.8) + 
    geom_point() + 
    ggrepel::geom_text_repel(aes(label = round(value, 1)), show.legend = FALSE) + 
    labs(title = paste0(db1_agg0$label[1], ", nc = ", db1_agg0$nc[1]), x = "Year", y = "Estimates") + 
    facet_wrap(~ Indicator, scales = ifelse(fixed_y, "fixed", "free"))+ 
    scale_y_continuous(limits = c(0, NA)) + 
    theme_minimal() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  if(fixed_y) g <- g + scale_y_continuous(limits = c(0, y_max))
  g
}
plot.by.label(label0 = "edu_group_2", F)


grid_list <- lapply(label_order, plot.by.label, fixed_y = TRUE)
ggsave(file.path(dir_output, "aggregates_2000_2024_from_extrapolation_by_label_fix_y.pdf"), 
       gridExtra::marrangeGrob(grobs = grid_list, layout_matrix = matrix(1:9, byrow = TRUE, nrow = 3)),
       width = 20, height = 15)


grid_list <- lapply(label_order, plot.by.label, fixed_y = FALSE)
ggsave(file.path(dir_output, "aggregates_2000_2024_from_extrapolation_by_label.pdf"),
       gridExtra::marrangeGrob(grobs = grid_list, layout_matrix = matrix(1:9, byrow = TRUE, nrow = 3)),
       width = 20, height = 15)

} # end MAKE_DIAGNOSTIC_PLOTS block

