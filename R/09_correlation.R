# =========================================================================
# 09_correlation.R
# Purpose:  Pearson correlation analysis between MT, TP, and Error Rate
# Input:    long_data (from 06_boxplots.R), datos_transformados (from 01)
# Output:   cor_matrix, Figures B.10 and B.11
# =========================================================================

library(tidyverse)
library(corrplot)

# -------------------------------------------------------------------------
# 1. DATA PREPARATION
# Pivot to wide format for column-wise correlation
# -------------------------------------------------------------------------

correlation_data <- long_data %>%
  pivot_wider(names_from = Metric, values_from = Value) %>%
  select(`Movement Time (ms)`, `Throughput (bps)`, Error_pct) %>%
  drop_na()

cat("Correlation dataset:", nrow(correlation_data), "observations\n")

# -------------------------------------------------------------------------
# 2. PEARSON CORRELATION MATRIX
# -------------------------------------------------------------------------

cor_matrix <- cor(correlation_data, method = "pearson")

cat("\nCorrelation matrix:\n")
print(round(cor_matrix, 3))

# -------------------------------------------------------------------------
# 3. FIGURE B.10 — Correlation Matrix: MT, TP, and Error Rate
# -------------------------------------------------------------------------

corrplot(cor_matrix,
         method      = "circle",
         type        = "upper",
         addCoef.col = "black",
         tl.col      = "black",
         tl.srt      = 45,
         title       = "\nCorrelation: MT, TP, and Error Rate",
         diag        = FALSE,
         col         = COL2("RdBu", 10))

# -------------------------------------------------------------------------
# 4. FIGURE B.11 — Effect of Fisher's Z-transformation on Correlation (r)
# -------------------------------------------------------------------------

comparison_r_data <- datos_transformados %>%
  select(r_2d, z_Fisher_r) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "Transformation",
    values_to = "Value"
  ) %>%
  mutate(
    Transformation = recode(Transformation,
                            "r_2d"       = "Original Correlation (r)",
                            "z_Fisher_r" = "Fisher's Z-transform"),
    Transformation = factor(Transformation,
                            levels = c("Original Correlation (r)",
                                       "Fisher's Z-transform"))
  )

ggplot(comparison_r_data, aes(x = Value, fill = Transformation)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 25, alpha = 0.5, color = "white") +
  geom_density(linewidth = 1) +
  facet_wrap(~ Transformation, scales = "free") +
  labs(
    title    = "Effect of Fisher's Z-transformation on Correlation Coefficients",
    subtitle = "Normalizes the skewed distribution of r for modeling",
    x        = "Value",
    y        = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle   = element_text(size = 10, hjust = 0.5, color = "grey40"),
    legend.position = "none",
    strip.text      = element_text(face = "bold", size = 11)
  )
