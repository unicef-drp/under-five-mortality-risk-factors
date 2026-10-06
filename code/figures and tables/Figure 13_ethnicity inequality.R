# Figure 13: ethnicity inequality dot plot.
# Uses the same n>200, latest-survey-year dataset as every other ethnicity
# statistic in the brochure, so very small groups (such as Ethiopia's Mezengrigna,
# n=121) do not appear as extreme points here while being excluded elsewhere.
library(tidyverse)

USERPROFILE <- Sys.getenv("USERPROFILE")
source(file.path(USERPROFILE, "Dropbox/UNICEF Work/profile.R"))   # sets dir_IGME

# brochure figure output
fig.dir <- file.path("figures", "Figure 13 ethnicity inequality")
if (!dir.exists(fig.dir)) dir.create(fig.dir, recursive = TRUE)

# Tall dot plot of 42 countries, saved at 10 x 10.5in.
save_on_page <- function(plot, path, width, height) {
  ggsave(path, plot, width = width, height = height, device = cairo_pdf)
}

# Disparity-analysis dataset, filtered to groups with n>200, as used for the
# country examples and the significance count
data <- read_csv(file.path("input", "ethnicity", "ethnicity dataset_0522.csv")) %>%
  mutate(is_reference = `u5mr ratio` == 1,
         ISO3Code = if_else(ISO3Code == "RKS", "XKX", ISO3Code))  # 2026 country info codes Kosovo as XKX

#country name -> official name lookup (CME info, 2026 round), joined directly on ISO3Code
cmeinfo <- read.csv(file.path(dir_IGME, "2026 Round Estimation/Code/input/country.info.CME.csv"),
                    encoding = "UTF-8") %>%
  select(ISO3Code, OfficialName) %>%
  distinct_all()
data <- data %>%
  left_join(cmeinfo, by = "ISO3Code")
#report any countries that failed to match an OfficialName
unmatched <- data %>% filter(is.na(OfficialName)) %>% distinct(ISO3Code)
if (nrow(unmatched) > 0) { cat("No OfficialName match for:\n"); print(unmatched) }

# Order by adjusted disparity ratio; ratio_ag is computed per country in the
# source file as max/min adjusted U5MR among the n>200 groups
country_order <- data %>%
  distinct(ISO3Code, OfficialName, ratio_ag) %>%
  arrange(ratio_ag) %>%
  pull(OfficialName)
data <- data %>% mutate(OfficialName = factor(OfficialName, levels = country_order))

U5MR_summary <- data %>%
  group_by(OfficialName) %>%
  summarize(adj_u5mr_min = min(`Adjusted U5MR`),
            adj_u5mr_max = max(`Adjusted U5MR`))
# Dot plot
p <- ggplot(data, aes(x = `Adjusted U5MR`, y = OfficialName)) +
  geom_point(aes(color = ifelse(is_reference, "black", "grey")), size = 3) +
  scale_color_identity(guide = "none") +
  theme_classic() +
  labs(x = "Adjusted Under-five mortality rate (deaths per 1,000 live births)",
       y = NULL) +
  theme(axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 10)) +
  geom_segment(data = U5MR_summary, aes(x = adj_u5mr_min, xend = adj_u5mr_max, y = OfficialName, yend = OfficialName),
               size = 1.2, lineend = "round", alpha = 0.8, color = "gray50")
ggsave(file.path(fig.dir, "Figure_13_ethnicity_inequality.png"), plot = p, width = 10, height = 10.5, units = "in")
save_on_page(p, file.path(fig.dir, "Figure_13_ethnicity_inequality.pdf"), width = 10, height = 10.5)
