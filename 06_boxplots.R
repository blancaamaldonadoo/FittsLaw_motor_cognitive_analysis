library(tidyverse)

# --- Boxplots: Identifying Skewness and Outliers (Normal Scale) ---
ggplot(long_data %>% filter(Metric == "Movement Time (ms)"), 
       aes(x = as.factor(Dia), y = Value, fill = Condicion)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  # Facet_wrap with free scales to better visualize each Condition
  facet_wrap(~Condicion, scales = "free_y") +
  labs(title = "Skewness and Outlier Detection in MTavg",
       subtitle = "Red dots represent statistical outliers",
       x = "Session (Day)",
       y = "Movement Time (ms)",
       fill = "Condition") +
  theme_minimal()

# --- Boxplots: Identifying Skewness and Outliers for Throughput (TP) ---
ggplot(long_data %>% filter(Metric == "Throughput (bps)"), 
       aes(x = as.factor(Dia), y = Value, fill = Condicion)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  # Facet_wrap para ver cada condición por separado con su propia escala
  facet_wrap(~Condicion, scales = "free_y") +
  labs(title = "Skewness and Outlier Detection in Throughput (TP)",
       subtitle = "Red dots represent statistical outliers",
       x = "Session (Day)",
       y = "Throughput (bps)",
       fill = "Condition") +
  theme_minimal()


# --- Boxplots: Logarithmic Scale (Log10) ---
# Note: Log transformation is often used to normalize positively skewed reaction time data.
ggplot(long_data %>% filter(Metric == "Movement Time (ms)"), 
       aes(x = as.factor(Dia), y = Value, fill = Condicion)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  # Applying log10 scale to the Y-axis
  scale_y_log10() + 
  facet_wrap(~Condicion, scales = "free_y") +
  labs(title = "Boxplots in Logarithmic Scale (Log10)",
       subtitle = "Transformation applied across all conditions to improve comparability",
       x = "Session (Day)",
       y = "log10(MTavg)",
       fill = "Condition") +
  theme_minimal()

# --- Distribution Comparison: Original vs. Log-Transformed ---
# 1. Create a temporary dataset for comparison
comparison_orig <- long_data %>%
  filter(Metric == "Movement Time (ms)") %>%
  mutate(Transformation = "Original (ms)")

comparison_log <- comparison_orig %>%
  mutate(Value = log10(Value),
         Transformation = "Log-Transformed (log10)")

# 2. Merge, set factor levels for ordering, and Plot
bind_rows(comparison_orig, comparison_log) %>%
  # Forzamos el orden: primero Original, luego Log
  mutate(Transformation = factor(Transformation, 
                                 levels = c("Original (ms)", "Log-Transformed (log10)"))) %>%
  ggplot(aes(x = Value, fill = Transformation)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.5, color = "white") +
  geom_density(linewidth = 1) +
  facet_wrap(~Transformation, scales = "free") +
  labs(title = "Effect of Logarithmic Transformation on MTavg",
       subtitle = "Log scale reduces positive skewness and helps normalize residuals",
       x = "Value", 
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")

# Sobrescribimos la columna original con su versión logarítmica
long_data <- long_data %>%
  mutate(Value = if_else(Metric == "Movement Time (ms)", log10(Value), Value))



library(tidyverse)

# --- 1. Gráfica para Throughput (TP) ---
p_tp <- long_data %>%
  filter(Metric == "Throughput (bps)") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "#404080", alpha = 0.5, color = "white") +
  geom_density(linewidth = 1, color = "#404080") +
  labs(title = "Throughput (TP) Distribution",
       x = "Throughput (bits/s)", y = "Density") +
  theme_minimal()

# --- 2. Gráfica para Error Percentage (%) ---
p_error <- long_data %>%
  filter(Metric == "Error_pct") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "#f4a261", alpha = 0.5, color = "white") +
  geom_density(linewidth = 1, color = "#f4a261") +
  labs(title = "Distribución de la Tasa de Error",
       subtitle = "Asimetría positiva: la mayoría de los ensayos tienen pocos errores",
       x = "Porcentaje de Error (%)", y = "Densidad") +
  theme_minimal()

# --- 3. Gráfica para el Coeficiente de Ajuste (r) ---
p_r <- long_data %>%
  filter(Metric == "r_2d") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "#69b3a2", alpha = 0.5, color = "white") +
  geom_density(linewidth = 1, color = "#69b3a2") +
  labs(title = "Distribución del Coeficiente de Ajuste (r)",
       subtitle = "Asimetría negativa: indica un excelente ajuste a la Ley de Fitts (> 0.9)",
       x = "Valor de r", y = "Densidad") +
  theme_minimal()


# Para verlas, simplemente escribe el nombre de la que quieras en la consola:
p_tp
p_error
p_r
