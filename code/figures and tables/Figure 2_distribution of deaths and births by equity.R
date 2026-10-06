rm(list = ls())
library(dplyr)
library(ggplot2)
library(ggpattern)
library(scales)
library(glue)
library(tidyr)
library(gridExtra)
library(data.table)
library(openxlsx)

roundoff <- function(x, digits = 0) {
  z <- trunc(abs(x) * 10^digits + 0.5)
  sign(x) * z / 10^digits
}

USERPROFILE <- Sys.getenv("USERPROFILE")
input.dir  <- file.path("output", "aggregate")
output.dir <- file.path("figures", "Figure 2 distribution of deaths and births by equity")
if (!dir.exists(output.dir)) dir.create(output.dir, recursive = TRUE)

agg_file <- tail(sort(list.files(input.dir, pattern = "^aggregate_.*\\.csv$", full.names = TRUE)), 1)
aggregate <- read.csv(agg_file) %>% filter(Year == 2024.5)
# age-range labels use an en dash (20-29 -> 20–29, etc.) rather than a hyphen
aggregate$label <- ifelse(aggregate$label %in% c("20-29", "30-39", "40-49"),
                          gsub("-", "–", aggregate$label), aggregate$label)

# ---- Deaths distribution ----
total_death <- aggregate %>%
  filter(Group != "total") %>%
  group_by(equity, Indicator, type) %>%
  summarise(wgt.death = sum(wgt.death), .groups = "drop") %>%
  select(Indicator, equity, wgt.death, type)

death <- aggregate %>%
  filter(Group != "total") %>%
  group_by(equity, Group, label, Indicator, type) %>%
  summarize(wgt.death = sum(wgt.death), .groups = "drop") %>%
  left_join(total_death, by = c("equity", "Indicator", "type")) %>%
  mutate(proportion = wgt.death.x / wgt.death.y) %>%
  select(equity, Group, label, Indicator, proportion, type)

group_levels <- c("1.quintile", "2.quintile", "3.quintile", "4.quintile", "5.quintile",
                  "noedu.primary", "sec.higher", "noeducation", "primary", "secondary", "higher",
                  "rural", "urban", "small.vsmall", "av.large",
                  "1", "2-3", "4+", "<2", "2", "3", ">=4",
                  "<20", "20-29", "30-39", "40-49")
equity_levels <- c("Wealth quintile", "Mother's education", "Mother's education detailed",
                   "Residence", "Birth size", "Birth order", "Birth interval", "Mother's age")

death$Group  <- factor(death$Group,  levels = group_levels)
death$equity <- factor(death$equity, levels = equity_levels)
setorder(death, Group)
death <- death %>%
  group_by(Indicator, equity, type) %>%
  mutate(label_pos  = cumsum(proportion * 100) - (0.5 * proportion * 100),
         label_text = percent(roundoff(proportion, digits = 2))) %>%
  ungroup()

# ---- Births distribution ----
total_birth <- aggregate %>%
  filter(Group != "total") %>%
  group_by(equity, Indicator, type) %>%
  summarise(wgt.birth = sum(wgt.birth), .groups = "drop") %>%
  select(Indicator, equity, wgt.birth, type)

birth <- aggregate %>%
  filter(Group != "total") %>%
  group_by(equity, Group, label, Indicator, type) %>%
  summarize(wgt.birth = sum(wgt.birth), .groups = "drop") %>%
  left_join(total_birth, by = c("equity", "Indicator", "type")) %>%
  mutate(proportion = wgt.birth.x / wgt.birth.y) %>%
  select(equity, Group, label, Indicator, proportion, type)

birth$Group  <- factor(birth$Group,  levels = group_levels)
birth$equity <- factor(birth$equity, levels = equity_levels)
setorder(birth, Group)
birth <- birth %>%
  group_by(Indicator, equity, type) %>%
  mutate(label_pos  = cumsum(proportion * 100) - (0.5 * proportion * 100),
         label_text = sprintf("%.0f%%", round(proportion * 100, digits = 0))) %>%
  ungroup()

# ---- Combine and label Type ----
combined_data <- bind_rows(death, birth, .id = "Type") %>%
  filter(!(Type == 2 & Indicator == "MR1t59"))

combined_data$Type <- ifelse(
  combined_data$Type == 1 & combined_data$Indicator == "U5MR",  "Under-five deaths",
  ifelse(combined_data$Type == 1 & combined_data$Indicator == "NMR",  "Neonatal deaths",
         ifelse(combined_data$Type == 2 & combined_data$Indicator == "NMR",  "Live births NMR", "Live births U5MR")))

combined_data <- combined_data %>%
  mutate(Type = ifelse(Indicator == "MR1t59", "1–59-months deaths",
                       ifelse(equity != "Wealth quintile" & equity != "Birth size" &
                                Type == "Live births U5MR", "Live births", Type)))

combined_data <- combined_data %>%
  filter(!(Type == "Live births NMR"  & equity != "Wealth quintile" & equity != "Birth size") &
           !(Type == "Live births U5MR" & equity == "Birth size")) %>%
  mutate(Type = ifelse(equity == "Birth size" & Type == "Live births NMR", "Live births", Type))

combined_data <- combined_data %>%
  mutate(
    equity = as.character(equity),
    equity = ifelse(Type %in% c("Live births U5MR", "Under-five deaths") & equity == "Wealth quintile",
                    "Wealth quintile estimates",
                    ifelse(Type %in% c("Live births NMR", "Neonatal deaths", "", "1–59-months deaths") &
                             equity == "Wealth quintile", "Wealth quintile survey", equity)),
    Type = ifelse(Type %in% c("Live births U5MR", "Live births NMR") &
                    equity %in% c("Wealth quintile estimates", "Wealth quintile survey"), "Live births", Type)
  )

combined_data$equity <- factor(
  combined_data$equity,
  levels = c("Wealth quintile estimates", "Wealth quintile survey", "Mother's education",
             "Mother's education detailed", "Residence", "Birth size",
             "Birth order", "Birth interval", "Mother's age")
)
combined_data <- combined_data %>% arrange(equity, desc(Type))
# Factor levels are in bottom-to-top plotting order (coord_flip reverses the usual
# left-to-right order), so this renders top-to-bottom as:
# Live births, Under-five deaths, Neonatal deaths, 1-59-months deaths
combined_data$Type <- factor(
  combined_data$Type,
  levels = c("1–59-months deaths", "Neonatal deaths", "Live births NMR",
             "Under-five deaths", "Live births U5MR", "Live births")
)
combined_data <- combined_data %>%
  mutate(equity = ifelse(equity == "Mother's education" & is.na(type) &
                           Group %in% c("noeducation", "primary", "secondary", "higher"),
                         "Mother's education detailed", as.character(equity)))

# ---- Plot function ----
create_plots_with_colors <- function(data, equity_subset, colors, title) {
  data_subset <- subset(data, equity == equity_subset)
  present_groups <- levels(droplevels(data_subset$Group))
  legend_labels <- sapply(present_groups, function(g) as.character(unique(data_subset$label[data_subset$Group == g])[1]))
  ggplot(data_subset, aes(x = Type, y = proportion * 100, fill = Group)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.8) +
    scale_fill_manual(values = colors, labels = legend_labels, name = NULL) +
    theme_minimal() +
    theme(strip.text.y       = element_text(size = 10, face = "bold", angle = 0),
          strip.background   = element_rect(fill = "lightblue"),
          strip.placement    = "outside",
          strip.switch.pad.grid = unit(1, "lines"),
          legend.position    = "bottom",
          panel.grid.major   = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text.x        = element_blank(),
          axis.ticks.x       = element_blank()) +
    geom_text(aes(label = label_text, y = label_pos), color = "black", size = 4.5, vjust = 0.5) +
    coord_flip() +
    labs(y = NULL, x = NULL)
}

# Save at a print-friendly size; wide, short stacked bars
save_fig <- function(plot, name, width = 10, height = 3.5) {
  ggsave(file.path(output.dir, paste0(name, ".png")), plot = plot, width = width, height = height)
  ggsave(file.path(output.dir, paste0(name, ".pdf")), plot = plot, width = width, height = height, device = cairo_pdf)
}

colors_wealth              <- c("#BCB900", "#85E200", "#34F62C", "#76BD7F", "#7367CA")
colors_education           <- c("#FFA500", "#4A90E2")
colors_education_detailed  <- c("#B37400", "gold", "#6A8A3D", "#545892")
colors_residence           <- c("#FF0000", "#ADD8E6")
colors_bsize               <- c("gold", "#7D9AA0")
colors_birthorder          <- c("#34F62C", "skyblue", "#A52A2A")
colors_birthinterval       <- c("#D35E60", "#E08700", "#68D465", "#7B82B4")
colors_motherage           <- c("#FFD700", "#3CF334", "skyblue", "#DEB887")

save_fig(create_plots_with_colors(combined_data, "Wealth quintile estimates", colors_wealth,             "Wealth quintile"), "Figure_2_wealth_estimate")
save_fig(create_plots_with_colors(combined_data, "Wealth quintile survey",    colors_wealth,             "Wealth quintile"), "wealth_survey")

education_combined <- combined_data[!is.na(combined_data$type) &
                                      combined_data$type == "combined all countries" &
                                      combined_data$equity == "Mother's education", ]
save_fig(create_plots_with_colors(education_combined,  "Mother's education", colors_education,          "Mother's education"), "Figure_7_education")

education_detailed <- combined_data[combined_data$Group %in% c("noeducation", "primary", "secondary", "higher"), ]
save_fig(create_plots_with_colors(education_detailed,  "Mother's education", colors_education_detailed, "Mother's education"), "edu_detailed")

save_fig(create_plots_with_colors(combined_data, "Residence",    colors_residence,    "Residence"),    "Figure_10_residence")

bsize_data <- filter(combined_data, Type != "1–59-months deaths" & Type != "Under-five deaths")
save_fig(create_plots_with_colors(bsize_data, "Birth size",  colors_bsize,  "Birth size"),  "Figure_28_bsize",  height = 2.5)

save_fig(create_plots_with_colors(combined_data, "Birth order",    colors_birthorder,    "Birth order"),    "Figure_24_birthorder")
save_fig(create_plots_with_colors(combined_data, "Birth interval", colors_birthinterval, "Birth interval"), "Figure_20_birthinterval")
save_fig(create_plots_with_colors(combined_data, "Mother's age",   colors_motherage,     "Mother's age"),   "Figure_16_motherage")

write.xlsx(combined_data, file.path(input.dir, "distribution of deaths and births.xlsx"))  # saved alongside aggregate CSV
