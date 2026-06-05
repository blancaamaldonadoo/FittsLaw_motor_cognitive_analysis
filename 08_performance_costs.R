# =========================================================================
# 08_performance_costs.R
# Purpose:  Compute and visualize cognitive and dominance performance costs
# Input:    long_data (from 06_boxplots.R — MT already log10-transformed)
# Output:   performance_costs_all, costs_long, Figures B.8 and B.9
# Note:     Cognitive cost = dual-task minus single-task (same hand)
#           Dominance cost = non-dominant minus dominant (no load)
#           TP: negative = worse | MT: positive = worse (slower)
# =========================================================================

library(tidyverse)

# -------------------------------------------------------------------------
# 1. SHARED THEME
# -------------------------------------------------------------------------

costs_theme <- theme_minimal() +
  theme(
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
    plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle    = element_text(size = 10, hjust = 0.5, color = "grey40"),
    axis.title       = element_text(size = 10),
    legend.position  = "bottom"
  )

cost_colors <- c(
  "cog_cost_D"  = "#fc8d62",   # Cognitive Cost (Dominant)
  "cog_cost_ND" = "#66c2a5",   # Cognitive Cost (Non-Dominant)
  "dom_cost"    = "#8da0cb"    # Dominance Cost
)

cost_labels <- c(
  "cog_cost_D"  = "Cognitive Cost (D)",
  "cog_cost_ND" = "Cognitive Cost (ND)",
  "dom_cost"    = "Dominance Cost"
)

# -------------------------------------------------------------------------
# 2. COMPUTE PERFORMANCE COSTS
# -------------------------------------------------------------------------

performance_costs_all <- long_data %>%
  pivot_wider(
    id_cols    = c(Sujeto, Dia, Metric),
    names_from = Condicion,
    values_from = Value
  ) %>%
  rename(
    Load_D     = `Dominant + Load`,
    Load_ND    = `Non-Dominant + Load`,
    Control_D  = Dominant,
    Control_ND = `Non-Dominant`
  ) %>%
  mutate(
    cog_cost_D  = Load_D     - Control_D,    # Cognitive cost: dominant hand
    cog_cost_ND = Load_ND    - Control_ND,   # Cognitive cost: non-dominant hand
    dom_cost    = Control_ND - Control_D     # Dominance cost: ND vs D (no load)
  )

# -------------------------------------------------------------------------
# 3. RESHAPE TO LONG FORMAT FOR PLOTTING
# -------------------------------------------------------------------------

costs_long <- performance_costs_all %>%
  select(Sujeto, Dia, Metric, cog_cost_D, cog_cost_ND, dom_cost) %>%
  pivot_longer(
    cols      = c(cog_cost_D, cog_cost_ND, dom_cost),
    names_to  = "Cost_Type",
    values_to = "Delta_Value"
  ) %>%
  drop_na(Delta_Value)

cat("Performance costs computed:",
    n_distinct(costs_long$Sujeto), "subjects |",
    n_distinct(costs_long$Cost_Type), "cost types\n")

# -------------------------------------------------------------------------
# 4. FIGURE B.8 — Performance Costs: Throughput (TP)
# Negative values = performance degradation relative to control
# -------------------------------------------------------------------------

ggplot(costs_long %>% filter(Metric == "Throughput (bps)"),
       aes(x = as.factor(Dia), y = Delta_Value, fill = Cost_Type)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = cost_colors, labels = cost_labels) +
  labs(
    title    = "Performance Costs Analysis (Throughput)",
    subtitle = "Negative values indicate performance degradation compared to control",
    x        = "Session (Day)",
    y        = "Delta TP (bps)",
    fill     = "Cost Type"
  ) +
  costs_theme

# -------------------------------------------------------------------------
# 5. FIGURE B.9 — Performance Costs: Movement Time (MT)
# Positive values = slower performance (cost)
# -------------------------------------------------------------------------

ggplot(costs_long %>% filter(Metric == "Movement Time (ms)"),
       aes(x = as.factor(Dia), y = Delta_Value, fill = Cost_Type)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = cost_colors, labels = cost_labels) +
  labs(
    title    = "Performance Costs: Movement Time (Rapidity)",
    subtitle = "Positive values = Slower performance (Cost)",
    x        = "Session (Day)",
    y        = "Delta MT (ms)",
    fill     = "Cost Type"
  ) +
  costs_theme
