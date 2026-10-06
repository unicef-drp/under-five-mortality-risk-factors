# Figures 4 and 5: U5MR trend by wealth quintile

library("data.table")
library("ggplot2")
library("dplyr")

USERPROFILE <- Sys.getenv("USERPROFILE")

# final figure outputs go to the shared brochure figures folder
output.dir <- file.path("figures", "Figure 4-5 U5MR trend by wealth quintile")
if (!dir.exists(output.dir)) dir.create(output.dir, recursive = TRUE)

# Save each chart at its natural size.
save_on_page <- function(plot, path, width, height) {
  ggsave(path, plot, width = width, height = height, device = cairo_pdf)
}

input.dir<-file.path(USERPROFILE, "/Dropbox/IGME Wealth/projection")
dtprojw <- read.csv(paste0(USERPROFILE, "/Dropbox/IGME Wealth/aggregate/IGME2025_U5MR_wealth_quintile_aggregate_world_UI.csv"))

# ---- Figure 4 current estimates ----

color_wealth <- c(
  "Poorest" = "#BCB900",  # Poorest
  "2nd quintile" = "#85E200",  # 2nd quintile
  "3rd quintile" = "#34F62C",  # 3rd quintile
  "4th quintile" =  "#76BD7F",  # 4th quintile
  "Wealthiest" = "#7367CA"   # Wealthiest
)
dtIGME.worldl <- dtprojw
setDT(dtIGME.worldl)
dtIGME.worldl[, Indicator:= as.factor(quintile)]
levels(dtIGME.worldl$Indicator) <- names(color_wealth)
max_sbr_Upper_World <- max(dtIGME.worldl$value)
dtprow_world<-dtIGME.worldl %>%
  filter(
           Year<=2024)
max_sbr_Upper_World <- max(dtprow_world$U5MR.upper, na.rm = TRUE)
gwIGME <- ggplot(data = dtprow_world, aes(x = Year, y = U5MR, group = factor(Indicator))) +
  geom_ribbon(aes(ymin = U5MR.lower, ymax = U5MR.upper, fill = factor(Indicator)), alpha = 0.3, color = NA) +
  geom_line(aes(color = factor(Indicator)), linewidth = 1) +
  scale_color_manual(
    name = "Wealth quintile",
    values = color_wealth,
    labels = c("Poorest", "2nd quintile", "3rd quintile", "4th quintile", "Wealthiest")
  ) +
  scale_fill_manual(
    name = "Wealth quintile",
    values = color_wealth,
    labels = c("Poorest", "2nd quintile", "3rd quintile", "4th quintile", "Wealthiest"),
    guide = "none"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max_sbr_Upper_World * 1.05)) +
  scale_x_continuous(
    breaks = sort(unique(c(seq(1990, 2020, by = 5), 2024))),  # force include 2024
    limits = c(1990, 2024)
  ) +
  labs(
    x = "Year",
    y = "Under-five mortality rate (deaths per 1,000 live births)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )
gwIGME
ggsave(filename = file.path(output.dir, "Figure_4_projection_v1.png"),
       plot = gwIGME, width = 9, height = 7)
save_on_page(gwIGME, file.path(output.dir, "Figure_4_projection_v1.pdf"), width = 9, height = 7)

# ---- Figure 5 lines by scenarios ----
# color scheme

dtprojw <- fread(file.path(USERPROFILE, "/Dropbox/IGME Wealth/aggregate/IGME2025_U5MR_wealth_quintile_projection_world.csv"))
dtprojw_dup <- dtprojw[Year==2024] # connect the lines
dtprojw.plot <- rbindlist(list(
  dtprojw,
  copy(dtprojw_dup)[, scenario:= "Continue current trends"],
  copy(dtprojw_dup)[, scenario:= "Achieve SDG target by 2030"]
))

# only show two quintiles
dtprojw.plot <- dtprojw.plot[Quintile %in% c(1,5)]
dtprojw.plot[, ind:=factor(as.factor(Quintile), levels = 1:5)]
levels(dtprojw.plot$ind) <- names(color_wealth)

# projection
select.scen.u5mr <- c("UN IGME estimate",
                      "Continue current trends",
                      "Achieve SDG target by 2030")
select.scen.u5mr.rename <- c("UN IGME estimate 1990–2024",
                             "Continue current trends",
                             "Achieve SDG target by 2030")
dtprojw.plot[, scenario.r := factor(scenario, levels = select.scen.u5mr)]
levels(dtprojw.plot$scenario.r) <- select.scen.u5mr.rename

# add label
dtprojw.plot[Year %in% c(1990, 2024, 2030) & Quintile %in% c(1,5), label:= format(roundoff(value, 1), nsmall = 1)]
# remove dup label at year 2024
dtprojw.plot[Year%in%c(2024) & scenario.r != "UN IGME estimate 1990–2024", label:= NA_character_]
dtprojw.plot[, ind_group:= as.factor(paste0(scenario.r, " for the ", tolower(ind)))] # use `ind_group ` as label
levels(dtprojw.plot$ind_group)

# level setting
grid <- data.table::setDT(expand.grid(select.scen.u5mr.rename, "for the", c( "poorest", "wealthiest")))
grid[, level:= paste(Var1, Var2, tolower(Var3))]
dtprojw.plot[, ind_group:=factor(as.factor(ind_group), levels = grid$level)]

# color setting part
dtprojw.plot[, color := dplyr::recode(ind, !!!color_wealth)] # colors for UN IGME estimate
dtprojw.plot[scenario=="Continue current trends" & Quintile==1, color := "#961A49"]
dtprojw.plot[scenario=="Continue current trends" & Quintile==5, color := "#002800"]
dtprojw.plot[scenario=="Achieve SDG target by 2030" & Quintile==1, color := "#0058AB"]
dtprojw.plot[scenario=="Achieve SDG target by 2030" & Quintile==5, color := "#1CABE2"]
color_values <- as.character(dtprojw.plot$color)
names(color_values) <- as.character(dtprojw.plot$ind_group)

note.u5mr <- "SDG target at country level of \nunder-five mortality rate ≤25" # <=25
g.u5mr <- ggplot(data = dtprojw.plot[Year>=1990],
                 aes(x = Year, y = value,
                     color = ind_group, linetype = scenario.r)) +
  geom_line(linewidth = 1.5) +
  labs(y = "Under-five mortality rate (deaths per 1,000 live births)", x = "Year") +
  scale_x_continuous(limits = c(1990, 2030),
                     breaks = c(1990, 2000, 2010, 2024, 2030)) +
  scale_y_continuous(limits=c(0, dtprojw.plot[Year>=1990, max(value)]*1.1), expand = c(0, 0))+
  scale_color_manual("", values = color_values) +
  scale_linetype_manual("", values = c("solid", "solid", "dashed"), guide = "none") +
  ggrepel::geom_text_repel(data = dtprojw.plot, aes(x = Year, y = value, label = label),
                           size = 3, show.legend = FALSE) +
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  geom_hline(yintercept = 25, linetype = "dashed", color = "black", linewidth = 0.8) + # Add horizontal line
  annotate("text", x = 2024, y = 25, label = "SDG target", hjust = 2.1, vjust = -0.5, size = 4, color = "black") # Add text for SDG target
g.u5mr
ggsave(filename = file.path(output.dir, "Figure_5_projection_v2.png"),
       plot = g.u5mr, width = 9, height = 7)
save_on_page(g.u5mr, file.path(output.dir, "Figure_5_projection_v2.pdf"), width = 9, height = 7)
