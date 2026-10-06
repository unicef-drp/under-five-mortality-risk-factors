rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)

USERPROFILE <- Sys.getenv("USERPROFILE")
# brochure figure output
fig.dir <- file.path("figures", "Figure 31-32 inequality heatmaps")
if (!dir.exists(fig.dir)) dir.create(fig.dir, recursive = TRUE)

# Heatmaps saved at about 10 x 8in.
save_on_page <- function(plot, path, width, height) {
  ggsave(path, plot, width = width, height = height, device = cairo_pdf)
}

# ---- Read indices ----
indices <- read.csv(file.path("output", "other", "inequality_indices.csv"), check.names = FALSE)  # from code/07_inequality_indices.R
indices$comparison[is.na(indices$comparison)] <- ""

# ---- Measures (raw and rank) ----
abs_measures      <- c("absolute difference", "slope index", "between group variance")
rel_measures      <- c("disparity ratio", "concentration index", "theils index")
all_measures      <- c(abs_measures, rel_measures)

abs_rank_measures <- c("absolute difference rank", "slope index rank", "between group variance rank")
rel_rank_measures <- c("disparity ratio rank", "concentration index rank", "theils index rank")
all_rank_measures <- c(abs_rank_measures, rel_rank_measures)

# Display names shared by raw and rank versions (same label, different column)
full_names <- c(
  "absolute difference"          = "Absolute difference",
  "slope index"                  = "Slope index",
  "between group variance"       = "Between-group variance",
  "disparity ratio"              = "Disparity ratio",
  "concentration index"          = "Concentration index",
  "theils index"                 = "Theil index",
  "absolute difference rank"     = "Absolute difference",
  "slope index rank"             = "Slope index",
  "between group variance rank"  = "Between-group variance",
  "disparity ratio rank"         = "Disparity ratio",
  "concentration index rank"     = "Concentration index",
  "theils index rank"            = "Theil index"
)

# ---- Build pair_info for a given set of abs/rel measures ----
make_pair_info <- function(abs_m, rel_m) {
  all_m     <- c(abs_m, rel_m)
  pairs_mat <- combn(all_m, 2)
  pi <- data.frame(
    m1        = pairs_mat[1, ],
    m2        = pairs_mat[2, ],
    pair_type = apply(pairs_mat, 2, function(p) {
      if      (all(p %in% abs_m)) "Absolute vs Absolute"
      else if (all(p %in% rel_m)) "Relative vs Relative"
      else                        "Absolute vs Relative"
    }),
    stringsAsFactors = FALSE
  )
  pi$pair_label <- paste(full_names[pi$m1], "\u2013", full_names[pi$m2])
  pi$pair_type  <- factor(pi$pair_type,
    levels = c("Absolute vs Absolute", "Relative vs Relative", "Absolute vs Relative"))
  pi <- pi[order(pi$pair_type), ]
  pi$pair_label <- factor(pi$pair_label, levels = rev(unique(pi$pair_label)))
  pi
}

pair_info      <- make_pair_info(abs_measures,      rel_measures)
pair_info_rank <- make_pair_info(abs_rank_measures, rel_rank_measures)

# ---- Three equity × U5MR combos ----
combos <- list(
  list(equity = "wealth",    indicator = "U5MR", comparison = "",         label = "Wealth"),
  list(equity = "edu",       indicator = "U5MR", comparison = "2 groups", label = "Education\n(2 groups)"),
  list(equity = "edu",       indicator = "U5MR", comparison = "4 groups", label = "Education\n(4 groups)"),
  list(equity = "residence", indicator = "U5MR", comparison = "",         label = "Residence")
)
combo_labels <- sapply(combos, `[[`, "label")

# ---- Compute correlations for a given pair_info, measure columns, and method ----
# Raw chart:  pair_info + all_measures  + method="pearson"  (Pearson on raw values)
# Rank chart: pair_info + all_measures  + method="spearman" (Spearman = Pearson on ranks)
# Both use the same raw columns; the method drives the difference.
compute_cor_rows <- function(pi, measure_cols, cor_method) {
  lapply(combos, function(combo) {
    fd <- indices[
      indices$equity     == combo$equity    &
      indices$Indicator  == combo$indicator &
      indices$comparison == combo$comparison,
      measure_cols, drop = FALSE]
    fd <- na.omit(fd)
    if (nrow(fd) < 5) return(NULL)
    lapply(seq_len(nrow(pi)), function(i) {
      data.frame(
        equity_label = combo$label,
        pair_label   = as.character(pi$pair_label[i]),
        pair_type    = as.character(pi$pair_type[i]),
        cor          = cor(fd[[pi$m1[i]]], fd[[pi$m2[i]]],
                           use = "complete.obs", method = cor_method),
        stringsAsFactors = FALSE
      )
    }) %>% bind_rows()
  }) %>% bind_rows()
}

finalize_cor_rows <- function(cr, pi) {
  cr$equity_label <- factor(cr$equity_label, levels = combo_labels)
  cr$pair_label   <- factor(cr$pair_label,   levels = levels(pi$pair_label))
  cr$pair_type    <- factor(cr$pair_type,
    levels = c("Absolute vs Absolute", "Relative vs Relative", "Absolute vs Relative"))
  cr$abs_cor <- abs(cr$cor)
  cr
}

cor_rows      <- finalize_cor_rows(compute_cor_rows(pair_info, all_measures, "pearson"),  pair_info)
cor_rows_rank <- finalize_cor_rows(compute_cor_rows(pair_info, all_measures, "spearman"), pair_info)

# ---- HEATMAP function ----
# facet_grid(pair_type ~ ., space="free_y") → equal tile sizes across all panels
#   (panel height proportional to row count; avoids stretched tiles in 3-row panels)
# scale_x_discrete(position = "top") → Wealth/Education/Residence labels at top
# Fill = |cor|, blue/purple gradient; text = signed value, white bold

make_heatmap <- function(cr) {
  ggplot(cr, aes(x = equity_label, y = pair_label, fill = abs_cor)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = sprintf("%.2f", cor)), size = 3.8, color = "white", fontface = "bold") +
    facet_grid(pair_type ~ ., scales = "free_y", space = "free_y") +
    scale_fill_gradient(
      low    = "#c8d9f0",
      high   = "#1a237e",
      limits = c(0, 1),
      name   = "Absolute\ncorrelation"
    ) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x       = element_text(size = 11, face = "bold"),
      axis.text.y       = element_text(size = 10.5, hjust = 1),
      strip.text.y      = element_text(size = 8, face = "bold"),
      strip.background  = element_rect(fill = "grey88", color = NA),
      panel.grid        = element_blank(),
      panel.spacing     = unit(0.8, "lines"),
      legend.position   = "right",
      plot.margin       = margin(10, 10, 10, 15)
    )
}

p      <- make_heatmap(cor_rows)
p_rank <- make_heatmap(cor_rows_rank)

# brochure outputs, matching the design figure names
ggsave(file.path(fig.dir, "Figure_31_inequality_heatmap.png"),      p,      width = 9, height = 8, dpi = 300)
save_on_page(p,      file.path(fig.dir, "Figure_31_inequality_heatmap.pdf"),      width = 9, height = 8)
ggsave(file.path(fig.dir, "Figure_32_inequality_heatmap_rank.png"), p_rank, width = 9, height = 8, dpi = 300)
save_on_page(p_rank, file.path(fig.dir, "Figure_32_inequality_heatmap_rank.pdf"), width = 9, height = 8)
message("Saved: Figure_31_inequality_heatmap")
message("Saved: Figure_32_inequality_heatmap_rank")
