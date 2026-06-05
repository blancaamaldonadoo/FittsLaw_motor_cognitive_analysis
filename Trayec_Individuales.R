# --- Individual Trajectories (Spaghetti Plots) ---

# 1. Plot for Movement Time (MTavg)
# We filter by metric to keep the visualization clean and readable
# Ejemplo corregido para el primer gráfico
ggplot(long_data %>% filter(Metric == "Movement Time (ms)"), 
       aes(x = as.factor(Dia), y = Value, group = Sujeto)) +
  
  geom_line(alpha = 0.3, linewidth = 0.5, color = "gray40") +
  geom_point(alpha = 0.3, size = 1) +
  
  # CAMBIO AQUÍ: Usamos "mean" entre comillas para evitar el error
  stat_summary(fun = "mean", geom = "line", aes(group = 1), color = "red", linewidth = 1.2) +
  stat_summary(fun = "mean", geom = "point", color = "red", size = 2) +
  
  facet_wrap(~Condicion, scales = "free_y") +
  labs(title = "Individual MTavg Trajectories by Condition",
       subtitle = "Grey lines: individual subjects | Red line: group mean",
       x = "Session (Day)",
       y = "Movement Time (ms)") +
  theme_minimal()

# 2. Plot for Throughput (TP_avg_2d)
ggplot(long_data %>% filter(Metric == "Throughput (bps)"), 
       aes(x = as.factor(Dia), y = Value, group = Sujeto)) +
  
  # 1. Individual lines
  geom_line(alpha = 0.3, linewidth = 0.5, color = "gray40") +
  
  # 2. Individual points
  geom_point(alpha = 0.3, size = 1) +
  
  # 3. Group MEAN trend
  stat_summary(fun = "mean", geom = "line", aes(group = 1), color = "red", linewidth = 1.2) +
  stat_summary(fun = "mean", geom = "point", color = "red", size = 2) +
  
  # 4. Facet by Condition
  facet_wrap(~Condicion, scales = "free_y") +
  
  labs(title = "Individual Throughput Trajectories by Condition",
       subtitle = "Grey lines represent individual subjects; the red line represents the group mean",
       x = "Session (Day)",
       y = "Throughput (bps)") +
  theme_minimal()

