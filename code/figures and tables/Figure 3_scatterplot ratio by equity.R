rm(list = ls())
library(tidyr)
library(dplyr)
library(ggplot2)

USERPROFILE <- Sys.getenv("USERPROFILE")
agg_input_dir     <- file.path("output", "aggregate")
country_input_dir <- file.path("output", "country")
output.dir        <- file.path("figures", "Figure x_U5MR scatter plot ratio")
if (!dir.exists(output.dir)) dir.create(output.dir, recursive = TRUE)

# Auto-detect latest date-stamped CSVs
agg_file     <- tail(sort(list.files(agg_input_dir,     pattern = "^aggregate_.*\\.csv$",                                      full.names = TRUE)), 1)
country_file <- tail(sort(list.files(country_input_dir, pattern = "^country-specific-database-cleaned-igme-wealth_.*\\.csv$",   full.names = TRUE)), 1)

plot_equity_facet_by_indicator <- function() {

  aggregate <- read.csv(agg_file) %>%
    # birth interval: compare "Less than 2 years" vs "3 years"
    # (matches the plotted axes)
    filter(label %in% c("Less than 2 years", "3 years",
                        "Second or third", "Fourth or higher",
                        "<20", "20-29",
                        "No education or primary", "Secondary or higher",
                        "Rural", "Urban",
                        "Poorest", "Wealthiest",
                        "Small or very small", "Average or large")) %>%
    # As in Figure 1: current year only, and no "2 groups" education rows, so the
    # ratio is not inflated by mixing years or aggregation methods
    filter(Year == 2024.5, type != "2 groups") %>%
    group_by(equity, Indicator) %>%
    summarise(ratio = max(wgt.rate) / min(wgt.rate), .groups = "drop") %>%
    mutate(equity = recode(equity,
                           "Birth interval"    = "birth interval",
                           "Birth order"       = "birth order",
                           "Birth size"        = "birth size",
                           "Mother's age"      = "mother's age",
                           "Mother's education" = "maternal education",
                           "Residence"         = "residence",
                           "Wealth quintile"   = "wealth quintile"))

  all_data <- read.csv(country_file) %>%
    filter(Group %in% c("noedu.primary", "sec.higher",
                        "1.quintile", "5.quintile",
                        "rural", "urban",
                        "<2", "3", "4+", "2-3",
                        "<20", "20-29",
                        "small.vsmall", "av.large") &
             !(label == "birthinterval" & Group == "4+") &
             Indicator != "nbirth") %>%
    select(Country.Code, label, Group, Estimates, Indicator) %>%
    pivot_wider(names_from = Group, values_from = Estimates) %>%
    mutate(equity = recode(label,
                           "wealth"        = "wealth quintile",
                           "edu"           = "maternal education",
                           "birthinterval" = "birth interval",
                           "birthorder"    = "birth order",
                           "bsize"         = "birth size",
                           "motherage"     = "mother's age")) %>%
    mutate(
      x_group = case_when(
        equity == "wealth quintile"   ~ "children by in the poorest households",
        equity == "residence"         ~ "children by in rural areas",
        equity == "maternal education" ~ "children born by mothers with no education or primary education",
        equity == "birth interval"    ~ "children born with a birth interval of less than 2 years",
        equity == "birth order"       ~ "fourth or higher order births",
        equity == "birth size"        ~ "small or very small birth size",
        equity == "mother's age"      ~ "children born by mothers aged <20",
        TRUE ~ "disadvantaged group"
      ),
      y_group = case_when(
        equity == "wealth quintile"   ~ "children born in the wealthiest households",
        equity == "residence"         ~ "children in urban areas",
        equity == "maternal education" ~ "children born by mothers with secondary or higher education",
        equity == "birth interval"    ~ "children born with a birth interval of three years",
        equity == "birth order"       ~ "second or third births",
        equity == "birth size"        ~ "average or large birth size",
        equity == "mother's age"      ~ "children born by mothers aged 20–29",
        TRUE ~ "advantaged group"
      ),
      x_axis = case_when(
        equity == "maternal education" ~ noedu.primary,
        equity == "wealth quintile"   ~ `1.quintile`,
        equity == "residence"         ~ rural,
        equity == "birth interval"    ~ `<2`,
        equity == "birth order"       ~ `4+`,
        equity == "birth size"        ~ small.vsmall,
        equity == "mother's age"      ~ `<20`
      ),
      y_axis = case_when(
        equity == "maternal education" ~ sec.higher,
        equity == "wealth quintile"   ~ `5.quintile`,
        equity == "residence"         ~ urban,
        equity == "birth interval"    ~ `3`,
        equity == "birth order"       ~ `2-3`,
        equity == "birth size"        ~ av.large,
        equity == "mother's age"      ~ `20-29`
      ),
      favouring_x = case_when(
        equity == "maternal education" ~ "Favouring more educated",
        equity == "wealth quintile"   ~ "Favouring rich",
        equity == "residence"         ~ "Favouring urban",
        equity == "birth interval"    ~ "Favouring longer birth spacing",
        equity == "birth order"       ~ "Favouring second or third birth",
        equity == "birth size"        ~ "Favouring average or large birth size",
        equity == "mother's age"      ~ "Favouring mother aged 20–29"
      ),
      favouring_y = case_when(
        equity == "maternal education" ~ "Favouring less educated",
        equity == "wealth quintile"   ~ "Favouring poor",
        equity == "residence"         ~ "Favouring rural",
        equity == "birth interval"    ~ "Favouring shorter birth spacing",
        equity == "birth order"       ~ "Favouring fourth or higher order birth",
        equity == "birth size"        ~ "Favouring small or very small birth size",
        equity == "mother's age"      ~ "Favouring mother aged <20"
      )
    ) %>%
    left_join(aggregate, by = c("equity", "Indicator"))

  all_data$equity <- factor(all_data$equity,
                            levels = c("wealth quintile", "maternal education", "residence",
                                       "mother's age", "birth interval", "birth order", "birth size"))

  plots <- list()

  for (eq in levels(all_data$equity)) {
    subset_data <- if (eq == "birth size") {
      filter(all_data, equity == eq & Indicator == "NMR")
    } else {
      filter(all_data, equity == eq & Indicator != "nbirth")
    }
    subset_data$Indicator_facet <- factor(
      subset_data$Indicator,
      levels = c("U5MR", "NMR", "MR1t59"),
      labels = c("Under-five mortality rate", "Neonatal mortality rate", "1–59−months mortality rate")
    )

    p <- ggplot(subset_data, aes(x = x_axis, y = y_axis)) +
      geom_point(size = 0.8, aes(color = Indicator_facet)) +
      geom_abline(intercept = 0, slope = 1,   linetype = "dotted", color = "black") +
      geom_abline(intercept = 0, slope = 0.5, linetype = "dotted", color = "lightgrey") +
      geom_abline(intercept = 0, slope = 2,   linetype = "dotted", color = "darkgrey") +
      geom_text(aes(x = 180, y = 10,  label = favouring_x), size = 3.5, hjust = 1) +
      geom_text(aes(x = 10,  y = 180, label = favouring_y), size = 3.5, hjust = 0) +
      geom_text(data = data.frame(x = 100, y = 100, label = "Equity"),
                aes(x = x, y = y, label = label), size = 3.5, inherit.aes = FALSE) +
      annotate("text", x = 100, y = 50,  label = "Ratio = 2",  size = 3.5, color = "darkgrey",  fontface = "italic") +
      annotate("text", x = 50,  y = 100, label = "Ratio = 0.5", size = 3.5, color = "lightgrey", fontface = "italic") +
      labs(x = paste0("Mortality rate for the ", unique(subset_data$x_group), "\n(deaths per 1,000)"),
           y = paste0("Mortality rate for the ", sub("^children", "children\n", unique(subset_data$y_group)), "\n(deaths per 1,000)")) +
      facet_wrap(~Indicator_facet, nrow = 1, strip.position = "top") +
      scale_x_continuous(limits = c(0, 200), breaks = c(0, 50, 100, 150, 200), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 200), breaks = c(0, 50, 100, 150, 200), expand = c(0, 0)) +
      theme_minimal(base_size = 12) +
      theme(plot.title   = element_text(size = 8, face = "bold", hjust = 0.5),
            axis.title   = element_text(size = 8),
            axis.title.y = element_text(size = 8),
            axis.text    = element_text(size = 6),
            strip.text   = element_text(size = 9, face = "bold"),
            legend.position = "none",
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
            panel.grid   = element_blank(),
            panel.spacing.x = unit(0.6, "cm"),
            plot.margin  = margin(10, 10, 10, 20))

    plots[[eq]] <- p
  }

  return(plots)
}

equity_plots <- plot_equity_facet_by_indicator()

# 3-panel scatters at about 9 x 3.5in keep the facets square, so the 45-degree
# equity lines read correctly; the birth-size scatter stays square.
save_scatter <- function(plot, name, width = 9, height = 3.5) {
  ggsave(file.path(output.dir, paste0(name, ".png")), plot = plot, width = width, height = height)
  ggsave(file.path(output.dir, paste0(name, ".pdf")), plot = plot, width = width, height = height, device = cairo_pdf)
}

save_scatter(equity_plots[["maternal education"]], "Figure_8_scatterplot_edu_combined")
save_scatter(equity_plots[["wealth quintile"]],    "Figure_3_scatterplot_wealth")
save_scatter(equity_plots[["residence"]],          "Figure_11_scatterplot_residence")
save_scatter(equity_plots[["mother's age"]],       "Figure_17_scatterplot_motherage")
save_scatter(equity_plots[["birth interval"]],     "Figure_21_scatterplot_interval")
save_scatter(equity_plots[["birth order"]],        "Figure_25_scatterplot_order")
save_scatter(equity_plots[["birth size"]],         "Figure_29_scatterplot_bsize", width = 5, height = 5)
