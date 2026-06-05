# =========================================================================
# 07_learning_metrics.R
# Purpose:  Compute and visualize learning acquisition indices (Day 1 to Day 3)
# Input:    long_data (from 06_boxplots.R — MT already log10-transformed)
# Output:   learning_indices, Figures B.6 and B.7
# Note:     Day 4 (retention) is excluded here — analyzed in LMM section
# =========================================================================

library(tidyverse)

# -------------------------------------------------------------------------
# 1. SHARED THEME
# -------------------------------------------------------------------------

learning_theme <- theme_minimal() +
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
# 2. LEARNING ACQUISITION INDICES
# Acquisition phase: Day 1 to Day 3 only
# MT:  improvement = D1 - D3 (lower MT = better)
# TP:  improvement = D3 - D1 (higher TP = better)
# -------------------------------------------------------------------------

learning_indices <- long_data %>%
  filter(Dia %in% c(1, 3)) %>%
  group_by(Sujeto, Condicion, Metric) %>%
  summarise(
    Acquisition_Improvement = if_else(
      first(Metric) == "Movement Time (ms)",
      Value[Dia == 1] - Value[Dia == 3],   # MT: reduction is improvement
      Value[Dia == 3] - Value[Dia == 1]    # TP: increase is improvement
    ),
    Percentage_Change = 100 * (Acquisition_Improvement / Value[Dia == 1]),
    .groups = "drop"
  )

cat("Learning indices computed for", n_distinct(learning_indices$Sujeto),
    "subjects across", n_distinct(learning_indices$Condicion), "conditions.\n")

# -------------------------------------------------------------------------
# 3. FIGURE B.6 — Learning Acquisition: Throughput Efficiency
# -------------------------------------------------------------------------

ggplot(learning_indices %>% filter(Metric == "Throughput (bps)"),
       aes(x = Condicion, y = Acquisition_Improvement, fill = Condicion)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title    = "Learning Acquisition: Throughput Efficiency",
    subtitle = "Net gain in bits/s from Day 1 to Day 3",
    x        = "Condition",
    y        = "Delta TP (bps) [D3 - D1]"
  ) +
  learning_theme

# -------------------------------------------------------------------------
# 4. FIGURE B.7 — Learning Acquisition: Movement Time
# -------------------------------------------------------------------------

ggplot(learning_indices %>% filter(Metric == "Movement Time (ms)"),
       aes(x = Condicion, y = Acquisition_Improvement, fill = Condicion)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title    = "Learning Acquisition: Movement Time",
    subtitle = "Improvement measured from Day 1 to Day 3 (Practice Phase)",
    x        = "Condition",
    y        = "Time Gained (ms) [D1 - D3]"
  ) +
  learning_theme

# -------------------------------------------------------------------------
# 5. DESCRIPTIVE SUMMARY (Days 1-3 only)
# -------------------------------------------------------------------------

final_summary_table <- long_data %>%
  filter(Dia <= 3) %>%
  group_by(Condicion, Dia, Metric) %>%
  summarise(
    Mean = round(mean(Value, na.rm = TRUE), 3),
    SD   = round(sd(Value, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(Metric, Condicion, Dia)

print(final_summary_table)
