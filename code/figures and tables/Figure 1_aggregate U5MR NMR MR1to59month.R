rm(list = ls())
library(grid)
library(data.table)
library(gridExtra)
library(ggplot2)
library(dplyr)

USERPROFILE <- Sys.getenv("USERPROFILE")
input.dir  <- file.path("output", "aggregate")
output.dir <- file.path("figures", "Figure 1 U5MR, NMR and MR1t59 by equity")
if (!dir.exists(output.dir)) dir.create(output.dir, recursive = TRUE)

# Auto-detect the latest aggregate CSV
agg_file <- tail(sort(list.files(input.dir, pattern = "^aggregate_.*\\.csv$", full.names = TRUE)), 1)
aggregate <- read.csv(agg_file)

roundoff <- function(x, digits = 0) {
  z <- trunc(abs(x) * 10^digits + 0.5)
  sign(x) * z / 10^digits
}

aggregate$Group <- factor(
  aggregate$Group,
  levels = c("1.quintile", "2.quintile", "3.quintile", "4.quintile", "5.quintile",
             "noedu.primary", "sec.higher", "noeducation", "primary", "secondary", "higher",
             "rural", "urban", "small.vsmall", "av.large",
             "1", "2-3", "4+", "<2", "2", "3", ">=4",
             "<20", "20-29", "30-39", "40-49")
)
# age-range labels use an en dash (20-29 -> 20–29, etc.) rather than a hyphen
aggregate$label <- ifelse(aggregate$label %in% c("20-29", "30-39", "40-49"),
                          gsub("-", "–", aggregate$label), aggregate$label)
aggregate$label <- factor(
  aggregate$label,
  levels = c("Poorest", "2nd quintile", "3rd quintile", "4th quintile", "Wealthiest",
             "No education or primary", "Secondary or higher",
             "No education", "Primary", "Secondary", "Higher",
             "Rural", "Urban", "Small or very small", "Average or large",
             "First", "Second or third", "Fourth or higher",
             "Less than 2 years", "2 years", "3 years", "4 years or more",
             "<20", "20–29", "30–39", "40–49")
)
aggregate$Indicator <- factor(aggregate$Indicator, levels = c("U5MR", "NMR", "MR1t59"))

colors_wealth       <- c("#BCB900", "#85E200", "#34F62C", "#76BD7F", "#7367CA")
colors_education    <- c("#FFA500", "#4A90E2", "#B37400", "gold", "#6A8A3D", "#545892")
colors_residence    <- c("#FF0000", "#ADD8E6")
colors_bsize        <- c("gold", "#7D9AA0")
colors_birthorder   <- c("#34F62C", "skyblue", "#A52A2A")
colors_birthinterval <- c("#D35E60", "#E08700", "#68D465", "#7B82B4")
colors_motherage    <- c("#FFD700", "#3CF334", "skyblue", "#DEB887")

create_plot <- function(data, indicator, plot_title, y_label, colors, bar_width = 0.9) {
  ggplot(subset(data, Indicator == indicator), aes(x = label, y = wgt.rate, fill = label)) +
    geom_bar(stat = "identity", width = bar_width) +
    geom_text(aes(label = round(wgt.rate, digits = 0), y = wgt.rate),
              vjust = 3, size = 6, position = position_dodge(width = bar_width), color = "black") +
    labs(y = y_label, title = plot_title) +
    scale_fill_manual(name = "", values = colors) +
    theme_bw(base_size = 16) +
    scale_y_continuous(limits = c(0, ymax + 5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid = element_blank(),
          strip.text = element_blank()) +
    guides(fill = "none")
}

# Save at 15 x 8in landscape.
save_combined <- function(p1, p2, p3, filename_base, width = 15, height = 8) {
  empty_plot <- rectGrob(gp = gpar(col = "white"))
  combined_grob <- arrangeGrob(
    p1, empty_plot, p2, empty_plot, p3,
    ncol = 5, widths = c(1, 0.2, 1, 0.2, 1)
  )
  ggsave(file.path(output.dir, paste0(filename_base, ".png")), combined_grob, width = width, height = height, units = "in")
  ggsave(file.path(output.dir, paste0(filename_base, ".pdf")), combined_grob, width = width, height = height, units = "in", device = cairo_pdf)
}

# Wealth quintile
wealth <- aggregate %>% filter(equity == "Wealth quintile" & Group != "total" & Year == 2024.5) %>% arrange(wgt.rate)
ymax <- max(wealth$wgt.rate, na.rm = TRUE)
save_combined(
  create_plot(wealth, "U5MR",   "Under-five mortality rate", "Deaths per 1,000 live births", colors_wealth),
  create_plot(wealth, "NMR",    "Neonatal mortality rate", "Deaths per 1,000 live births", colors_wealth),
  create_plot(wealth, "MR1t59", "1–59 months mortality rate", "Deaths per 1,000 children aged 28 days", colors_wealth),
  "Figure_1_wealth_aggregate"
)

# Mother's education
edu <- aggregate %>%
  filter(equity == "Mother's education" & Group != "total" & type != "2 groups") %>%
  arrange(wgt.rate)
ymax <- max(edu$wgt.rate, na.rm = TRUE)
save_combined(
  create_plot(edu, "U5MR",   "Under-five mortality rate", "Deaths per 1,000 live births", colors_education) +
    geom_vline(xintercept = 2.5, linetype = "dashed", color = "black"),
  create_plot(edu, "NMR",    "Neonatal mortality rate", "Deaths per 1,000 live births", colors_education) +
    geom_vline(xintercept = 2.5, linetype = "dashed", color = "black"),
  create_plot(edu, "MR1t59", "1–59 months mortality rate", "Deaths per 1,000 children aged 28 days", colors_education) +
    geom_vline(xintercept = 2.5, linetype = "dashed", color = "black"),
  "Figure_6_edu_aggregate"
)

# Residence
residence <- aggregate %>% filter(equity == "Residence" & Group != "total") %>% arrange(wgt.rate)
ymax <- max(residence$wgt.rate, na.rm = TRUE)
save_combined(
  create_plot(residence, "U5MR",   "Under-five mortality rate", "Deaths per 1,000 live births", colors_residence, bar_width = 0.5),
  create_plot(residence, "NMR",    "Neonatal mortality rate", "Deaths per 1,000 live births", colors_residence, bar_width = 0.5),
  create_plot(residence, "MR1t59", "1–59 months mortality rate", "Deaths per 1,000 children aged 28 days", colors_residence, bar_width = 0.5),
  "Figure_9_residence_aggregate"
)

# Birth size
bsize <- aggregate %>% filter(equity == "Birth size" & Group != "total") %>% arrange(wgt.rate)
ymax <- max(bsize$wgt.rate, na.rm = TRUE)
save_combined(
  create_plot(bsize, "U5MR",   "Under-five mortality rate", "Deaths per 1,000 live births", colors_bsize, bar_width = 0.5),
  create_plot(bsize, "NMR",    "Neonatal mortality rate", "Deaths per 1,000 live births", colors_bsize, bar_width = 0.5),
  create_plot(bsize, "MR1t59", "1–59 months mortality rate", "Deaths per 1,000 children aged 28 days", colors_bsize, bar_width = 0.5),
  "Figure_27_bsize_aggregate"
)

# Birth order
birthorder <- aggregate %>% filter(equity == "Birth order" & Group != "total") %>% arrange(wgt.rate)
ymax <- max(birthorder$wgt.rate, na.rm = TRUE)
save_combined(
  create_plot(birthorder, "U5MR",   "Under-five mortality rate", "Deaths per 1,000 live births", colors_birthorder, bar_width = 0.7),
  create_plot(birthorder, "NMR",    "Neonatal mortality rate", "Deaths per 1,000 live births", colors_birthorder, bar_width = 0.7),
  create_plot(birthorder, "MR1t59", "1–59 months mortality rate", "Deaths per 1,000 children aged 28 days", colors_birthorder, bar_width = 0.7),
  "Figure_23_birthorder_aggregate"
)

# Birth interval
birthinterval <- aggregate %>% filter(equity == "Birth interval" & Group != "total") %>% arrange(wgt.rate)
ymax <- max(birthinterval$wgt.rate, na.rm = TRUE)
save_combined(
  create_plot(birthinterval, "U5MR",   "Under-five mortality rate", "Deaths per 1,000 live births", colors_birthinterval),
  create_plot(birthinterval, "NMR",    "Neonatal mortality rate", "Deaths per 1,000 live births", colors_birthinterval),
  create_plot(birthinterval, "MR1t59", "1–59 months mortality rate", "Deaths per 1,000 children aged 28 days", colors_birthinterval),
  "Figure_19_birthinterval_aggregate"
)

# Mother's age
motherage <- aggregate %>% filter(equity == "Mother's age" & Group != "total") %>% arrange(wgt.rate)
ymax <- max(motherage$wgt.rate, na.rm = TRUE)
save_combined(
  create_plot(motherage, "U5MR",   "Under-five mortality rate", "Deaths per 1,000 live births", colors_motherage),
  create_plot(motherage, "NMR",    "Neonatal mortality rate", "Deaths per 1,000 live births", colors_motherage),
  create_plot(motherage, "MR1t59", "1–59 months mortality rate", "Deaths per 1,000 children aged 28 days", colors_motherage),
  "Figure_15_motherage_aggregate"
)
