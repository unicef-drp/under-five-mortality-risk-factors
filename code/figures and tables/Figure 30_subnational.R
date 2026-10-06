# Figure 30_subnational
# Per cent decline in the neonatal mortality rate (NMR), 2000-2021, by subnational region.
# One row per country, ordered by national decline (high to low, top to bottom).
#   - faded green filled circles = Admin 2 regions
#   - open orange circles        = Admin 1 regions
#   - filled light grey circle   = national value
# x-axis: per cent decline in NMR (positive = decline), -100 to 100.

library(openxlsx)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)

# ---- paths ----
USERPROFILE <- Sys.getenv("USERPROFILE")
est.dir  <- file.path(USERPROFILE, "Dropbox/UNICEF Work/Data and charts for websites/Files 2022/CME/Estimates")
sub.file <- file.path(est.dir, "Subnational_estimates_2023-08-15.xlsx")
nat.file <- file.path(est.dir, "UNIGME2022_Country_Rates.csv")
out.dir  <- "figures/"

year0 <- 2000.5
year1 <- 2021.5
pct_decline <- function(v0, v1) (v0 - v1) / v0 * 100

# ---- subnational decline (Admin 1 and Admin 2) ----
sub <- read.xlsx(sub.file) %>%
  filter(Indicator == "Neonatal mortality rate", Year %in% c(year0, year1))

sub.decl <- sub %>%
  mutate(period = ifelse(Year == year0, "start", "end")) %>%
  select(Country.Name, Admin.Level, ISO.Internal, period, Median) %>%
  pivot_wider(names_from = period, values_from = Median) %>%
  filter(!is.na(start), !is.na(end)) %>%
  mutate(decline = pct_decline(start, end),
         type = ifelse(Admin.Level == "Admin1", "Admin 1", "Admin 2")) %>%
  select(Country.Name, type, decline)

# ---- national decline (grey dot) ----
nat.decl <- read.csv(nat.file, check.names = FALSE) %>%
  filter(Indicator == "Neonatal mortality rate", Sex == "Total", Year %in% c(year0, year1)) %>%
  mutate(period = ifelse(Year == year0, "start", "end")) %>%
  select(Country.Name, period, Median) %>%
  pivot_wider(names_from = period, values_from = Median) %>%
  mutate(decline = pct_decline(start, end)) %>%
  select(Country.Name, decline) %>%
  mutate(type = "National")

# keep only the countries that have subnational NMR estimates (29 of the 31;
# Uganda and Zambia have subnational U5MR but no subnational NMR in this file)
keep <- intersect(unique(sub.decl$Country.Name), unique(nat.decl$Country.Name))

# country order: highest national decline at the top
country.order <- nat.decl %>%
  filter(Country.Name %in% keep) %>%
  arrange(decline) %>%          # ascending -> highest ends up as top row
  pull(Country.Name)

# shorten the two long country names to match the report layout
name.recode <- c("Lao People's Democratic Republic" = "Lao People's Democratic Rep.",
                 "United Republic of Tanzania"       = "United Rep. of Tanzania")

plot.df <- bind_rows(sub.decl, filter(nat.decl, Country.Name %in% keep)) %>%
  filter(Country.Name %in% keep) %>%
  mutate(Country.Name = recode(Country.Name, !!!name.recode),
         type = factor(type, levels = c("National", "Admin 1", "Admin 2"))) %>%   # legend order
  mutate(Country.Name = factor(Country.Name, levels = recode(country.order, !!!name.recode))) %>%
  arrange(match(type, c("Admin 2", "Admin 1", "National")))   # draw National last, on top

# ---- plot ----
cols   <- c("Admin 2" = "#6aa84f", "Admin 1" = "#e8842b", "National" = "#4d4d4d")  # National = dark border (shape 21)
fills  <- c("Admin 2" = "#6aa84f", "Admin 1" = NA,        "National" = "#7f7f7f")
shapes <- c("Admin 2" = 16,        "Admin 1" = 1,         "National" = 21)
alphas <- c("Admin 2" = 0.75,      "Admin 1" = 0.90,      "National" = 1)
sizes  <- c("Admin 2" = 2.6,       "Admin 1" = 2.8,       "National" = 4.2)

p <- ggplot(plot.df, aes(x = decline, y = Country.Name)) +
  geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.4) +
  geom_point(aes(shape = type, colour = type, fill = type, alpha = type, size = type),
             stroke = 0.9) +
  scale_shape_manual(values = shapes) +
  scale_colour_manual(values = cols, na.value = NA) +
  scale_fill_manual(values = fills, na.value = NA) +
  scale_alpha_manual(values = alphas, guide = "none") +
  scale_size_manual(values = sizes, guide = "none") +
  scale_x_continuous(limits = c(-100, 100), breaks = seq(-100, 100, 50),
                     expand = expansion(mult = 0.01)) +
  guides(colour = guide_legend(override.aes = list(size = 4, alpha = 1)),
         shape = "none", fill = "none") +
  labs(x = "Per cent decline in NMR", y = NULL, colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "grey40"),
    axis.ticks = element_line(colour = "grey40"),
    axis.ticks.length = unit(3, "pt"),
    axis.text.y = element_text(size = 8.5),
    axis.title.x = element_text(size = 11, margin = margin(t = 8)),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    plot.margin = margin(6, 10, 6, 6)
  )

# National in the legend needs its dark outline (shape 21) restored explicitly
p <- p + guides(colour = guide_legend(
  override.aes = list(
    shape  = shapes[c("National", "Admin 1", "Admin 2")],
    fill   = fills[c("National", "Admin 1", "Admin 2")],
    colour = c(National = "#4d4d4d", "Admin 1" = "#e8842b", "Admin 2" = "#6aa84f"),
    size   = 4, alpha = 1, stroke = 0.9)))

# ---- save (PNG + PDF) ----
if (!dir.exists(out.dir)) dir.create(out.dir, recursive = TRUE)
ggsave(paste0(out.dir, "Figure 30_subnational.png"), p, width = 6.5, height = 7.2, dpi = 300, bg = "white")
ggsave(paste0(out.dir, "Figure 30_subnational.pdf"), p, width = 6.5, height = 7.2, device = cairo_pdf)
