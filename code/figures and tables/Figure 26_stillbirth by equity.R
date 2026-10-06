library(tidyr)
library(openxlsx)
library(stringr)
# Shared world-map helpers
USERPROFILE <- Sys.getenv("USERPROFILE")
package.dir <- file.path(USERPROFILE, "/Dropbox/UNICEF Work/unmap/un_world_map")
source(file.path(package.dir, "R/make.world.map.R"))
work_dir <- file.path("input", "stillbirth")
side_dir <- file.path("output", "other", "stillbirth")  # figure data written at the end
lb_dir<-paste0(USERPROFILE, "/Dropbox/UN IGME data/2023 Round Estimation/Code/input")

library("rdhs")
library("sf")
library("ggplot2")
library("dplyr")
library("data.table")
library("countrycode")
library("RColorBrewer")

# Save at natural size.
save_on_page <- function(plot, path, width, height) {
  ggsave(path, plot, width = width, height = height, device = cairo_pdf)
}
# roundoff() is not defined in the sourced map script; same version as the other scripts
roundoff <- function(x, digits = 0) {
  z <- trunc(abs(x) * 10^digits + 0.5)
  sign(x) * z / 10^digits
}

#load stillbirth data
#CM_PNMR_C_NSB stillbirths, CM_PNMR_C_UNN 	Number of live births (unweighted)
#https://api.dhsprogram.com/rest/dhs/indicators?returnFields=IndicatorId,Label,Definition&f=html
dt_est <- fread(paste0(work_dir, "/DHS_API_download_2025-02-18.csv")) %>%
  filter(SDRID %in% c("CMPNMRCSBR", "CMPNMRCUNN") & 
           CharacteristicCategory %in% c("Mother's age at birth", "Residence", "Education", 
                                         "Education (2 groups)", "Wealth quintile", "Previous pregnancy interval in months"))
unique(dt_est$SurveyId) #101 surveys
unique(dt_est$ISO3Code)  #54 countries
cmeinfo <- fread(file.path(USERPROFILE, "Dropbox/UN IGME data/2023 Round Estimation/Code/input/country.info.CME.csv"))
lb<-read.csv(paste0(lb_dir, "/data_livebirths.csv")) %>%
   left_join(cmeinfo[,c("ISO3Code", "UNCode")], by=c("uncode"="UNCode")) %>%
   filter(year==2023.5)
db<-dt_est %>%
  group_by(ISO3Code) %>%
  filter(SurveyYear==max(SurveyYear)) %>%
  ungroup() 
summary(db$SurveyYear)
table(db$CharacteristicCategory, db$ISO3Code)
unique(db$ISO3Code)  #54 countries
#check 1: inclusion in the stillbirth database
inclusion<-read.xlsx(paste0(USERPROFILE, "/Dropbox/UNICEF Stillbirth/Plots/input/data_for_plotting_2025-03-10.xlsx")) %>%
  filter(source %in% c("survey", "HMIS")) %>%
  select(iso, int.year, inclusion, source) %>%
  distinct_all()
check1<-db %>%
  mutate(year=as.numeric(str_extract(SurveyId, "\\d+"))) %>%
  select(ISO3Code, year) %>% 
  distinct_all() %>%
  left_join(inclusion, by=c("ISO3Code"="iso", "year"="int.year"))

#check 2: check sbr for no education and higher
check<- db %>%
  filter(Indicator == "Stillbirth rate" & CharacteristicCategory == "Education" & 
           CharacteristicLabel %in% c("No education", "Higher")) %>%
  dplyr::select(ISO3Code, CharacteristicLabel, Value) %>%
  pivot_wider(names_from = CharacteristicLabel, values_from = Value) %>%
  mutate(check=`No education`>=`Higher`)

#check 3: range (max, min and median) of the ratios at the country level for each equity dimension
db<-db %>%
  select(ISO3Code, SurveyId, Indicator, CharacteristicCategory, CharacteristicLabel, Value) %>%
  pivot_wider(
    names_from = Indicator,   # Convert unique values in Indicator to column names
    values_from = Value       # Fill new columns with Value
  ) %>%
  left_join(lb, by="ISO3Code") %>%
  group_by(ISO3Code) %>%
  mutate(lb=`Number of live births (unweighted)`/sum(`Number of live births (unweighted)`)*lb) %>%
  ungroup() %>%
  mutate(SB=lb*`Stillbirth rate`/1000*(1-`Stillbirth rate`/1000)) %>%
  filter(!is.na(SB))
unique(db$ISO3Code)
table(check$inclusion)
db_wide <- db %>%
  select(ISO3Code, CharacteristicCategory, CharacteristicLabel, `Stillbirth rate`) %>%
  filter(CharacteristicCategory %in% c("Mother's age at birth", "Residence", 
                                       "Education", "Wealth quintile", 
                                       "Previous pregnancy interval in months")) %>%
  pivot_wider(names_from = CharacteristicLabel, values_from = `Stillbirth rate`)
ratios <- db_wide %>%
  mutate(
    Ratio = case_when(
      CharacteristicCategory == "Mother's age at birth" ~ `<20` / `20-29`,
      CharacteristicCategory == "Residence" ~ Rural / Urban,
      CharacteristicCategory == "Education" ~ `No education` / Higher,
      CharacteristicCategory == "Wealth quintile" ~ Lowest / Highest,
      CharacteristicCategory == "Previous pregnancy interval in months" ~ `<15` / `27-38`,
      TRUE ~ NA_real_
    )
  ) %>%
  select(ISO3Code, CharacteristicCategory, Ratio)
summary_stats <- ratios %>%
  filter(!is.na(Ratio) & Ratio>0 & is.finite(Ratio)) %>%
  group_by(CharacteristicCategory) %>%
  summarise(
    Min_Ratio = roundoff(min(Ratio, na.rm = TRUE), digits = 1),
    Max_Ratio = roundoff(max(Ratio, na.rm = TRUE), digits=1),
    Median_Ratio = roundoff(median(Ratio, na.rm = TRUE), digits=1)
  ) %>%
  ungroup()

sb<-read.csv(paste0(USERPROFILE, "/Dropbox/UNICEF Stillbirth/Aggregate results 2025-03-12/UNIGME_SBR_CountryResults.csv")) %>%
  filter(Year==2023)
sum(sb$Median[sb$ISO3Code %in% db$ISO3Code & sb$Shortind=="SB"])/sum(sb$Median[sb$Shortind=="SB"])
db<-db %>%
  group_by(CharacteristicCategory, CharacteristicLabel) %>%
  summarise(SBR=sum(SB)/(sum(SB)+sum(lb))*1000,
            SB=sum(SB), lb=sum(lb)) %>%
  ungroup() %>%
  mutate(CharacteristicCategory=ifelse(CharacteristicCategory=="Education (2 groups)", "Education", CharacteristicCategory))

db$CharacteristicLabel <- factor(db$CharacteristicLabel, 
                                 levels = c(
                                   "Lowest", "Second", "Middle", 
                                   "Fourth", "Highest",            # Wealth groups
                                   "No education or primary", "Secondary or higher", "No education", "Primary", 
                                   "Secondary", "Higher",       # Education levels
                                   "Rural", "Urban",                   # Residence"
                                   "<20", "20-29", "30-39", "40-49",   # Age groups
                                   "First pregnancy", "<15", "15-26", "27-38", "39+"               # Birth intervals
                                 ),
                                 labels = c(
                                   "Poorest", "2nd quintile", "3rd quintile", 
                                   "4th quintile", "Wealthiest", 
                                   "No education or primary", "Secondary or higher", "No education", "Primary", 
                                   "Secondary", "Higher",
                                   "Rural", "Urban", 
                                   "<20", "20-29", "30-39", "40-49", 
                                   "First pregnancy", "Less than 15 months", "15-26 months", "27-38 months", "39 months or more"
                                 ))
db$CharacteristicCategory <- factor(db$CharacteristicCategory,
                                    levels = c("Wealth quintile", "Education", "Residence",
                                                "Mother's age at birth", "Previous pregnancy interval in months"
                                    ),
                                    labels = c(
                                      "Household wealth", "Mother's education", "Residence", "Mother's age", "Birth interval"
                                    ))

db<-arrange(db, CharacteristicCategory, CharacteristicLabel)

# One sequential ramp per dimension, light to dark in category order: wealth
# orange, education blue, residence green, mother's age purple, birth interval
# yellow.
wealth_colors        <- setNames(RColorBrewer::brewer.pal(5, "Oranges"),
                                 c("Poorest", "2nd quintile", "3rd quintile", "4th quintile", "Wealthiest"))
education_colors     <- setNames(RColorBrewer::brewer.pal(4, "Blues"),
                                 c("No education", "Primary", "Secondary", "Higher"))
residence_colors     <- setNames(RColorBrewer::brewer.pal(3, "Greens")[2:3], c("Rural", "Urban"))
motherage_colors     <- setNames(RColorBrewer::brewer.pal(4, "Purples"), c("<20", "20-29", "30-39", "40-49"))
# RColorBrewer has no plain yellow ramp (YlOrBr skews to orange), so build one
birthinterval_colors <- setNames(colorRampPalette(c("#FFFDE0", "#F2C200"))(4),
                                 c("Less than 15 months", "15-26 months", "27-38 months", "39 months or more"))
all_colors <- c(wealth_colors, education_colors, residence_colors, motherage_colors, birthinterval_colors)

db <- db %>%
  mutate(Color = unname(all_colors[as.character(CharacteristicLabel)]))



# ---- make plot ----

plot <- ggplot(subset(db, !CharacteristicLabel %in% c("First pregnancy",
                                                      "No education or primary", "Secondary or higher")),
               aes(x = CharacteristicLabel, y = SBR, fill = Color)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.95) +
  scale_x_discrete(labels = scales::wrap_format(10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(db$SBR)*1.1)) +
  scale_fill_identity() +
  facet_grid(~CharacteristicCategory, scales = "free", space = "free") +
  labs(
    x = "",
    y = "Stillbirth rate (Stillbirths per 1,000 total births)"
  ) +
  theme_minimal() +
  theme(
    panel.spacing = unit(0.4, "lines"),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "none",
    panel.grid = element_blank()
  )

fig.dir <- file.path("figures", "Figure 26 stillbirth by equity")
if (!dir.exists(fig.dir)) dir.create(fig.dir, recursive = TRUE)
ggsave(paste0(fig.dir, "/Figure_26_stillbirth by equity.png"), plot = plot, width = 12, height = 6, dpi = 300, bg = "white")
save_on_page(plot, paste0(fig.dir, "/Figure_26_stillbirth by equity.pdf"), width = 12, height = 6)

dir.create(side_dir, recursive = TRUE, showWarnings = FALSE)
write.xlsx(db, file.path(side_dir, "stillbirth rate by equity.xlsx"))


