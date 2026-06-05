library(tidyverse)

# --- Calculating Learning Indices (Adquisition Phase: Day 1 to Day 3) ---
# We focus strictly on the change between the start and the end of the practice period.
learning_indices <- long_data %>%
  filter(Dia %in% c(1, 3)) %>% # We ignore Day 4 as per requirements
  group_by(Sujeto, Condicion, Metric) %>%
  summarise(
    # Acquisition Index:
    # For MT, lower is better (D1 - D3). For TP, higher is better (D3 - D1).
    Acquisition_Improvement = if_else(first(Metric) == "Movement Time (ms)", 
                                      Value[Dia == 1] - Value[Dia == 3], 
                                      Value[Dia == 3] - Value[Dia == 1]),
    
    # Percentage change relative to the first day
    Percentage_Change = 100 * (Acquisition_Improvement / Value[Dia == 1]),
    .groups = "drop"
  )

# --- Visualization: Total Improvement in Movement Time (D1 to D3) ---
ggplot(learning_indices %>% filter(Metric == "Movement Time (ms)"), 
       aes(x = Condicion, y = Acquisition_Improvement, fill = Condicion)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Learning Acquisition: Movement Time",
       subtitle = "Improvement measured from Day 1 to Day 3 (Practice Phase)",
       x = "Condition",
       y = "Time Gained (ms) [D1 - D3]") +
  theme_minimal()

# --- Visualization: Total Improvement in Throughput (D1 to D3) ---
ggplot(learning_indices %>% filter(Metric == "Throughput (bps)"), 
       aes(x = Condicion, y = Acquisition_Improvement, fill = Condicion)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Learning Acquisition: Throughput Efficiency",
       subtitle = "Net gain in bits/s from Day 1 to Day 3",
       x = "Condition",
       y = "Delta TP (bps) [D3 - D1]") +
  theme_minimal()

# --- Updated Descriptive Summary (Excluding Day 4) ---
final_summary_table <- long_data %>%
  filter(Dia <= 3) %>% # Filtering out Day 4
  group_by(Condicion, Dia, Metric) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE), 
    SD = sd(Value, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  arrange(Metric, Condicion, Dia)

print(final_summary_table)

