# =========================================================================
# 05_individual_trajectories.R
# Purpose:  Spaghetti plots of individual MT and TP trajectories per condition
# Input:    long_data (from 02_descriptives.R)
# Output:   Figures B.1 and B.2 (Appendix B)
# =========================================================================

library(tidyverse)

# -------------------------------------------------------------------------
# 1. SHARED THEME
# -------------------------------------------------------------------------

trajectory_theme <- theme_minimal() +
  theme(
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
    plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle    = element_text(size = 10, hjust = 0.5, color = "grey40"),
    axis.title       = element_text(size = 10)
  )

# -------------------------------------------------------------------------
# 2. FIGURE B.1 — Individual TP Trajectories by Condition
# -------------------------------------------------------------------------

ggplot(long_data %>% filter(Metric == "Throughput (bps)"),
       aes(x = as.factor(Dia), y = Value, group = Sujeto)) +
  geom_line(alpha = 0.3, linewidth = 0.5, color = "gray40") +
  geom_point(alpha = 0.3, size = 1) +
  stat_summary(fun = "mean", geom = "line",
               aes(group = 1), color = "red", linewidth = 1.2) +
  stat_summary(fun = "mean", geom = "point",
               color = "red", size = 2) +
  facet_wrap(~ Condicion, scales = "free_y") +
  labs(
    title    = "Individual Throughput Trajectories by Condition",
    subtitle = "Grey lines represent individual subjects; the red line represents the group mean",
    x        = "Session (Day)",
    y        = "Throughput (bps)"
  ) +
  trajectory_theme

# -------------------------------------------------------------------------
# 3. FIGURE B.2 — Individual MT Trajectories by Condition
# -------------------------------------------------------------------------

ggplot(long_data %>% filter(Metric == "Movement Time (ms)"),
       aes(x = as.factor(Dia), y = Value, group = Sujeto)) +
  geom_line(alpha = 0.3, linewidth = 0.5, color = "gray40") +
  geom_point(alpha = 0.3, size = 1) +
  stat_summary(fun = "mean", geom = "line",
               aes(group = 1), color = "red", linewidth = 1.2) +
  stat_summary(fun = "mean", geom = "point",
               color = "red", size = 2) +
  facet_wrap(~ Condicion, scales = "free_y") +
  labs(
    title    = "Individual MT\u2090\u1D65\u1D4D Trajectories by Condition",
    subtitle = "Grey lines: individual subjects | Red line: group mean",
    x        = "Session (Day)",
    y        = "Movement Time (ms)"
  ) +
  trajectory_theme
