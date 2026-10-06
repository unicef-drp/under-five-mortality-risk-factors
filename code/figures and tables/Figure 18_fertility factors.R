rm(list=ls())
library(openxlsx)# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
USERNAME <- Sys.getenv("USERNAME")
USERPROFILE <- Sys.getenv("USERPROFILE")
work_dir <- file.path("input", "fertility")
# brochure figure output
fig.dir <- file.path("figures", "Figure 18 fertility factors")
if (!dir.exists(fig.dir)) dir.create(fig.dir, recursive = TRUE)

# Panels are about 16 x 6in landscape.
save_on_page <- function(plot, path, width, height) {
  ggsave(path, plot, width = width, height = height, device = cairo_pdf)
}
data<-read.xlsx(paste0(work_dir, "/result.xlsx"), sheet=2) %>%
  rename(`Fertility factor`=Fertility.factor)

data <- data %>%
  pivot_longer(cols = starts_with("U5MR") | starts_with("MR1t59") | starts_with("NMR"), 
               names_to = "Measure", 
               values_to = "Value") %>%
  separate(Measure, into = c("Measure", "Type"), sep = "_") %>%
  spread(key = Type, value = Value)

data$Term <- factor(data$Term, levels = c("first birth",
                                          "fourth or higher order birth",
                                          "mother less than 20",
                                          "mother 30-39",
                                          "mother 40-49",
                                          "previous birth less than 2 years ago",
                                          "previous birth 2 years ago",
                                          "previous birth more than 4 years ago"),
                    labels = c("First birth",
                               "Fourth or higher order birth",
                               "Mother less than 20",
                               "Mother 30–39",
                               "Mother 40–49",
                               "Previous birth less than 2 years ago",
                               "Previous birth 2 years ago",
                               "Previous birth more than 4 years ago"))
data$`Fertility factor` <- factor(data$`Fertility factor`, levels = c("birth order", "mother's age", "birth interval"),
                                  labels = c("Birth order", "Mother's age", "Birth interval"))

data$`Measure` <- factor(data$`Measure`, levels = c("U5MR", "NMR", "MR1t59"),
                         labels = c("Under-five mortality rate \n(deaths per 1,000 live births)",
                                    "Neonatal mortality rate \n(deaths per 1,000 live births)",
                                    "1–59 months mortality rate \n(deaths per 1,000 children aged 28 days)"))

relativerisk<-ggplot(data, aes(x = Term, y = Median, fill = `Fertility factor`)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 2, color = "purple") +
  facet_wrap(~Measure, scales = "fixed") +  # Ensure all panels have the same y-axis scale
  labs(x = "Term",
       y = "Relative risk (95% CI)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase font size for x-axis labels
    axis.text.y = element_text(size = 14),  # Increase font size for y-axis labels
    axis.title.x = element_blank(),  # Skip the x-axis label
    axis.title.y = element_text(size = 16),  # Increase font size for y-axis title
    strip.text = element_text(size = 16),  # Increase font size for facet labels
    plot.title = element_text(size = 18, face = "bold"),  # Increase font size for the plot title
    panel.grid = element_blank(),  # Remove grid lines
    legend.position = "bottom",  # Move the legend to the bottom
    legend.text = element_text(size = 14),  # Increase font size for legend text
    legend.title = element_blank()  # Increase font size for legend title
  )
ggsave(paste0(fig.dir, "/Figure_18_A_fertility factors_singlerisk.png"), plot = relativerisk, width = 16, height = 6, dpi = 300)
save_on_page(relativerisk, paste0(fig.dir, "/Figure_18_A_fertility factors_singlerisk.pdf"), width = 16, height = 6)

double<-read.xlsx(paste0(work_dir, "/result.xlsx"), sheet=3) %>%
  rename(`Risk Type`=Fertility.factor) %>%
  pivot_longer(cols = starts_with("U5MR") | starts_with("MR1t59") | starts_with("NMR"), 
               names_to = "Measure", 
               values_to = "Value") %>%
  separate(Measure, into = c("Measure", "Type"), sep = "_") %>%
  spread(key = Type, value = Value)
double$`Risk Type` <- factor(double$`Risk Type`, levels = c("double risk", "triple risk"),
                              labels = c("Double risk", "Triple risk"))
double$`Measure` <- factor(double$`Measure`, levels = c("U5MR", "NMR", "MR1t59"),
                         labels = c("Under-five mortality rate \n(deaths per 1,000 live births)",
                                    "Neonatal mortality rate \n(deaths per 1,000 live births)",
                                    "1–59 months mortality rate \n(deaths per 1,000 children aged 28 days)"))
double$Term<-factor(double$Term, levels=c("previous birth less than 2 years ago & fourth or higher order birth",
                                          "previous birth 2 years ago & fourth or higher order birth",
                                          "previous birth less than 2 years ago & fourth or higher order birth & mother 40-49",
                                          "previous birth 2 years ago & fourth or higher order birth & mother 40-49"),
                    labels=c("Previous birth less than 2 years ago & fourth or higher order birth",
                             "Previous birth 2 years ago & fourth or higher order birth",
                             "Previous birth less than 2 years ago & fourth or higher order birth & mother 40–49",
                             "Previous birth 2 years ago & fourth or higher order birth & mother 40–49"))
relativerisk2 <- ggplot(double, aes(x = Term, y = Median, fill = `Risk Type`)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 2, color = "purple") +
  facet_wrap(~Measure, scales = "fixed") +  
  labs(x = "Term",
       y = "Relative risk (95% CI)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, vjust = 1, size = 8),  
    axis.text.y = element_text(size = 12),  
    axis.title.x = element_blank(),  
    axis.title.y = element_text(size = 12),  
    strip.text = element_text(size = 14),  
    plot.title = element_text(size = 14, face = "bold"),  
    panel.grid = element_blank(),  
    legend.position = "bottom",  
    legend.text = element_text(size = 12),  
    legend.title = element_text(size = 12)  
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))  # Break x-axis labels into 3 lines
ggsave(paste0(fig.dir, "/Figure_18_B_fertility factors_doubletriplerisk.png"), plot = relativerisk2, width = 16, height = 6, dpi = 300)
save_on_page(relativerisk2, paste0(fig.dir, "/Figure_18_B_fertility factors_doubletriplerisk.pdf"), width = 16, height = 6)

#number of risks
db<-read.xlsx(paste0(work_dir, "/result.xlsx"), sheet=4) %>%
  pivot_longer(cols = starts_with("U5MR") | starts_with("MR1t59") | starts_with("NMR"), 
             names_to = "Measure", 
             values_to = "Value") %>%
  separate(Measure, into = c("Measure", "Type"), sep = "_") %>%
  spread(key = Type, value = Value)
db$Term <- factor(db$Term, levels = c("Unavoidable first birth risk", "singlerisk", "doublerisk", "threewayrisk"),
                  labels=c("Unavoidable first birth risk", "Single risk", "Double risk", "Three-way risk"))
db$Measure <- factor(db$Measure, levels = c("U5MR", "NMR", "MR1t59"),
                           labels = c("Under-five mortality rate \n(deaths per 1,000 live births)", 
                                      "Neonatal mortality rate \n(deaths per 1,000 live births)", 
                                      "1—59 months mortality rate \n(deaths per 1,000 children aged 28 days)"))
nrisks<- ggplot(db, aes(x = Term, y = Median, fill = Term)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 2, color = "purple") +
  facet_wrap(~Measure, scales = "fixed") +  
  labs(x = "Term",
       y = "Relative risk (95% CI)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, vjust = 1, size = 12),  
    axis.text.y = element_text(size = 14),  
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size = 14),  
    strip.text = element_text(size = 14),  
    plot.title = element_text(size = 14, face = "bold"),  
    panel.grid = element_blank(),  
    legend.position = "bottom",  
    legend.text = element_text(size = 12),  
    legend.title = element_text(size = 12)  
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))  # Break x-axis labels into 3 lines
# extra panel, not a numbered figure
ggsave(paste0(fig.dir, "/fertility factors_numberofrisks.png"), plot = nrisks, width = 16, height = 6, dpi = 300)
save_on_page(nrisks, paste0(fig.dir, "/fertility factors_numberofrisks.pdf"), width = 16, height = 6)
