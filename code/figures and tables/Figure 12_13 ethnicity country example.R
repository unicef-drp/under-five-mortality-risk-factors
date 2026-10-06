library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(openxlsx)
pdf(NULL)  # arrangeGrob needs a device to measure text; stops a stray Rplots.pdf in batch runs

USERPROFILE <- Sys.getenv("USERPROFILE")
output.dir <- file.path("figures", "Figure 12-14 ethnicity country examples")
if (!dir.exists(output.dir)) dir.create(output.dir, recursive = TRUE)

# Save each chart at its natural size.
save_grob_on_page <- function(grob, path, width, height) {
  ggsave(path, grob, width = width, height = height, device = cairo_pdf)
}
cmeinfo<-read.csv(paste0(USERPROFILE, "/UNICEF/Child Mortality - Documents/UN IGME data/2026 Round Estimation/Code/input/country.info.CME.csv"))
input<-read.csv(file.path("input", "ethnicity", "ethnicity dataset_0522.csv")) %>%
  left_join(cmeinfo[, c("ISO3Code", "OfficialName")], by="ISO3Code") %>%
  mutate(survey=ifelse(source=="DHS", "Demographic and Health Survey", 
                       ifelse(source=="MICS", "Multiple Indicator Cluster Surveys", source)))
plot_u5mr_by_ethnicity <- function(data, iso3code, theil_index, width, xmax = NA) {
  # Filter data
  country_data <- data %>%
    filter(ISO3Code == iso3code)

  # Extract metadata (assumes they are the same within one country)
  country_name <- unique(country_data$OfficialName)
  survey_name <- unique(country_data$survey)
  survey_year <- unique(country_data$SurveyYear)

  # Small groups can have a symmetric lower confidence bound below zero. The x
  # scale censors out-of-range values to NA, which would drop the whole error bar,
  # so those rows are drawn as a line clipped at 0 with no left cap.
  ci_pos <- country_data %>% filter(u5mr_ll >= 0)
  ci_neg <- country_data %>% filter(u5mr_ll < 0)

  # Create the plot
  p <- ggplot(country_data, aes(x = u5mr_r, y = ethnicity)) +
    geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7, width=width)
  if (nrow(ci_pos) > 0) {
    p <- p + geom_errorbarh(data = ci_pos, aes(xmin = u5mr_ll, xmax = u5mr_ul), height = 0.3)
  }
  if (nrow(ci_neg) > 0) {
    p <- p +
      geom_segment(data = ci_neg, aes(x = 0, xend = u5mr_ul, y = ethnicity, yend = ethnicity)) +
      geom_errorbarh(data = ci_neg, aes(xmin = u5mr_ul, xmax = u5mr_ul), height = 0.3)
  }
  p <- p +
    theme_minimal() +
    labs(
      title = paste0(country_name, ", ", survey_name, " ", survey_year),
      x = NULL, y = NULL
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.y = element_text(size = 9),
      axis.title.x = element_text(size = 9),
      axis.title.y = element_text(size = 9),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      text = element_text(size = 9),
      legend.title = element_text(size = 9),
      legend.text  = element_text(size = 9)
    )
  # put all country panels on the same x-axis scale when a shared xmax is supplied
  if (!is.na(xmax)) p <- p + scale_x_continuous(limits = c(0, xmax))
  return(p)
}
# shared x-axis maximum across the three LIC panels (covers the widest upper CI)
lic_max <- max(input$u5mr_ul[input$ISO3Code %in% c("ETH", "NPL", "MRT")], na.rm = TRUE)
eth<-plot_u5mr_by_ethnicity(input, "ETH", 115, 0.8, xmax = lic_max)
nepal<-plot_u5mr_by_ethnicity(input, "NPL", 42, 0.8, xmax = lic_max)
mrt<-plot_u5mr_by_ethnicity(input, "MRT", 4, 0.4, xmax = lic_max)
plot<-arrangeGrob(eth, nepal, mrt, ncol = 3,
                   bottom = textGrob("Under-five mortality rate (deaths per 1,000 live births)", gp = gpar(fontsize = 15)))
ggsave(paste0(output.dir, "/Figure_12_country example by ethnicity_lic.png"), plot = plot, width = 12, height = 4, dpi = 300)
save_grob_on_page(plot, paste0(output.dir, "/Figure_12_country example by ethnicity_lic.pdf"), width = 12, height = 4)

# Brazil and Australia U5MR come from the 2024 update file (Sheet2), which fixed an
# Indigenous / non-Indigenous label swap for Australia.
input<-read.xlsx(file.path("input", "ethnicity", "ethnicity_data_brazil_australia_2024.xlsx"), sheet="Sheet2") %>%
  rename(country = Country, ethnicity = Ethnicity, year = Year) %>%
  mutate(ISO3Code = case_when(country == "Australia" ~ "AUS", country == "Brazil" ~ "BRA", TRUE ~ NA_character_))
plot_u5mr_by_ethnicity <- function(data, iso3code, width, xmax = NA) {
  country_data <- data %>%
    filter(ISO3Code == iso3code) %>%
    filter(year==max(year))

  country_name <- unique(country_data$country)
  survey_year <- unique(country_data$year)

  # Create the plot
  p <- ggplot(country_data, aes(x = U5MR, y = ethnicity)) +
    geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7, width=width) +
    theme_minimal() +
    labs(
      title = paste0(unique(country_data$country), ", ", unique(country_data$year)),
      x = NULL, y = NULL
    )+
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.y = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  # put both country panels on the same x-axis scale when a shared xmax is supplied
  if (!is.na(xmax)) p <- p + scale_x_continuous(limits = c(0, xmax))
  return(p)
}
# shared x-axis maximum across the two UMIC panels (latest survey year per country)
umic_max <- input %>%
  filter(ISO3Code %in% c("BRA", "AUS")) %>%
  group_by(ISO3Code) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  summarise(m = max(U5MR, na.rm = TRUE)) %>%
  pull(m)
bra<-plot_u5mr_by_ethnicity(input, "BRA", 0.8, xmax = umic_max)
aus<-plot_u5mr_by_ethnicity(input, "AUS", 0.3, xmax = umic_max)
plot<-arrangeGrob(aus, bra, ncol = 3,
                   bottom = textGrob("Under-five mortality rate (deaths per 1,000 live births)", gp = gpar(fontsize = 15)))
ggsave(paste0(output.dir, "/Figure_14_country example by ethnicity_umic.png"), plot = plot, width = 9, height = 4, dpi = 300)
save_grob_on_page(plot, paste0(output.dir, "/Figure_14_country example by ethnicity_umic.pdf"), width = 9, height = 4)
