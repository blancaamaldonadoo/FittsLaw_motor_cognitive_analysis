library(readxl)
library(tidyverse)

# --- Movement Time (MT) Evolution Plot ---
# Visualizing the learning curve for MTavg across sessions
ggplot(descriptives %>% filter(Metric == "Movement Time (ms)"), 
       aes(x = as.factor(Dia), y = mean, color = Condicion, group = Condicion)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  # Add error bars (95% Confidence Interval)
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "Movement Time (MT) Evolution",
       subtitle = "Error bars represent the 95% Confidence Interval (CI)",
       x = "Session (Day)",
       y = "Mean MT (ms)",
       color = "Condition") +
  theme_minimal() +
  theme(legend.position = "bottom")

# --- Throughput (TP) Evolution Plot ---
# Visualizing the efficiency/performance (TP_avg_2d) across sessions
ggplot(descriptives %>% filter(Metric == "Throughput (bps)"), 
       aes(x = as.factor(Dia), y = mean, color = Condicion, group = Condicion)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  # Add error bars (95% Confidence Interval)
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "Throughput (TP) Evolution",
       subtitle = "Error bars represent the 95% Confidence Interval (CI)",
       x = "Session (Day)",
       y = "Mean Throughput (bps)",
       color = "Condition") +
  theme_minimal() +
  theme(legend.position = "bottom")













library(readxl)
library(tidyverse)

# Shared theme for both plots
custom_theme <- theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
    axis.title = element_text(size = 10),
    legend.position = "none"   # No legend needed — condition is in facet label
  )

# --- Movement Time (MT) Evolution Plot ---
ggplot(descriptives %>% filter(Metric == "Movement Time (ms)"),
       aes(x = as.numeric(as.character(Dia)), y = mean, group = Condicion)) +
  geom_line(color = "#E69F00", linewidth = 1) +
  geom_point(color = "#E69F00", size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                color = "#E69F00", width = 0.2) +
  facet_wrap(~ Condicion, labeller = labeller(Condicion = function(x) paste("Condition:", x))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4)) +
  labs(
    title = "Motor Learning Analysis: Movement Time (MT) Evolution",
    subtitle = "Error bars represent the 95% Confidence Interval (CI)",
    x = "Session (Days)",
    y = "Mean MT (ms)"
  ) +
  custom_theme

# --- Throughput (TP) Evolution Plot ---
ggplot(descriptives %>% filter(Metric == "Throughput (bps)"),
       aes(x = as.numeric(as.character(Dia)), y = mean, group = Condicion)) +
  geom_line(color = "#E69F00", linewidth = 1) +
  geom_point(color = "#E69F00", size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                color = "#E69F00", width = 0.2) +
  facet_wrap(~ Condicion, labeller = labeller(Condicion = function(x) paste("Condition:", x))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4)) +
  labs(
    title = "Motor Learning Analysis: Throughput (TP) Evolution",
    subtitle = "Error bars represent the 95% Confidence Interval (CI)",
    x = "Session (Days)",
    y = "Throughput (bps)"
  ) +
  custom_theme








