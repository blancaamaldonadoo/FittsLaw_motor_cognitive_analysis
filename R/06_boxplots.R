# =========================================================================
# 06_boxplots.R
# Purpose:  Outlier detection, skewness visualization, and log transformation
#           of MT. Distribution plots for TP, ER, and r.
# Input:    long_data (from 02_descriptives.R)
# Output:   Figures B.3, B.4, B.5, Figure 7 (TP distribution)
# Note:     long_data is overwritten at the end with log10(MT)
# =========================================================================

library(tidyverse)

# -------------------------------------------------------------------------
# 1. SHARED THEME
# -------------------------------------------------------------------------

boxplot_theme <- theme_minimal() +
  theme(
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
    plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle    = element_text(size = 10, hjust = 0.5, color = "grey40"),
    axis.title       = element_text(size = 10),
    legend.position  = "none"
  )

# -------------------------------------------------------------------------
# 2. FIGURE B.3 — Skewness and Outlier Detection in MT (original scale)
# -------------------------------------------------------------------------

ggplot(long_data %>% filter(Metric == "Movement Time (ms)"),
       aes(x = as.factor(Dia), y = Value, fill = Condicion)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  facet_wrap(~ Condicion, scales = "free_y") +
  labs(
    title    = "Skewness and Outlier Detection in MT\u2090\u1D65\u1D4D",
    subtitle = "Red dots represent statistical outliers",
    x        = "Session (Day)",
    y        = "Movement Time (ms)"
  ) +
  boxplot_theme

# -------------------------------------------------------------------------
# 3. FIGURE 10 — Skewness and Outlier Detection in TP
# -------------------------------------------------------------------------

ggplot(long_data %>% filter(Metric == "Throughput (bps)"),
       aes(x = as.factor(Dia), y = Value, fill = Condicion)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  facet_wrap(~ Condicion, scales = "free_y") +
  labs(
    title    = "Skewness and Outlier Detection in Throughput (TP)",
    subtitle = "Red dots represent statistical outliers",
    x        = "Session (Day)",
    y        = "Throughput (bps)"
  ) +
  boxplot_theme

# -------------------------------------------------------------------------
# 4. FIGURE B.4 — Effect of Log Transformation on MT (distribution comparison)
# -------------------------------------------------------------------------

comparison_orig <- long_data %>%
  filter(Metric == "Movement Time (ms)") %>%
  mutate(Transformation = "Original (ms)")

comparison_log <- comparison_orig %>%
  mutate(Value          = log10(Value),
         Transformation = "Log-Transformed (log10)")

bind_rows(comparison_orig, comparison_log) %>%
  mutate(Transformation = factor(Transformation,
                                 levels = c("Original (ms)", "Log-Transformed (log10)"))) %>%
  ggplot(aes(x = Value, fill = Transformation)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, alpha = 0.5, color = "white") +
  geom_density(linewidth = 1) +
  facet_wrap(~ Transformation, scales = "free") +
  labs(
    title    = "Effect of Logarithmic Transformation on MT\u2090\u1D65\u1D4D",
    subtitle = "Log scale reduces positive skewness and helps normalize residuals",
    x        = "Value",
    y        = "Density"
  ) +
  boxplot_theme

# -------------------------------------------------------------------------
# 5. FIGURE B.5 — MT Boxplots in Logarithmic Scale
# -------------------------------------------------------------------------

ggplot(long_data %>% filter(Metric == "Movement Time (ms)"),
       aes(x = as.factor(Dia), y = Value, fill = Condicion)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  scale_y_log10() +
  facet_wrap(~ Condicion, scales = "free_y") +
  labs(
    title    = "Boxplots in Logarithmic Scale (Log10)",
    subtitle = "Transformation applied across all conditions to improve comparability",
    x        = "Session (Day)",
    y        = "log10(MT\u2090\u1D65\u1D4D)"
  ) +
  boxplot_theme

# -------------------------------------------------------------------------
# 6. OVERWRITE long_data: apply log10 to MT for downstream LMM modeling
# -------------------------------------------------------------------------

long_data <- long_data %>%
  mutate(Value = if_else(Metric == "Movement Time (ms)", log10(Value), Value))

cat("long_data updated: MT values are now log10-transformed.\n")

# -------------------------------------------------------------------------
# 7. FIGURE 7 — Distribution of TP (justification for no transformation)
# -------------------------------------------------------------------------

ggplot(long_data %>% filter(Metric == "Throughput (bps)"),
       aes(x = Value)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 20, fill = "#404080", alpha = 0.5, color = "white") +
  geom_density(linewidth = 1, color = "#404080") +
  labs(
    title = "Throughput (TP) Distribution",
    x     = "Throughput (bps)",
    y     = "Density"
  ) +
  boxplot_theme
