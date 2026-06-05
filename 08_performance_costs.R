library(tidyverse)


# 2. CÁLCULO DE COSTES PARA AMBAS MÉTRICAS
performance_costs_all <- long_data %>%
  pivot_wider(id_cols = c(Sujeto, Dia, Metric), 
              names_from = Condicion, 
              values_from = Value) %>%
  rename(Load_D = `Dominant + Load`, 
         Load_ND = `Non-Dominant + Load`, 
         Control_D = Dominant, 
         Control_ND = `Non-Dominant`) %>%
  mutate(
    # Coste Cognitivo: Impacto de la carga mental en cada mano
    cog_cost_D = if_else(Metric == "Throughput (bps)", Load_D - Control_D, Load_D - Control_D),
    cog_cost_ND = if_else(Metric == "Throughput (bps)", Load_ND - Control_ND, Load_ND - Control_ND),
    # Coste de Dominancia: Diferencia entre manos (sin carga)
    dom_cost = if_else(Metric == "Throughput (bps)", Control_ND - Control_D, Control_ND - Control_D)
  )

# 3. PREPARACIÓN PARA GRÁFICOS
costs_long <- performance_costs_all %>%
  select(Sujeto, Dia, Metric, cog_cost_D, cog_cost_ND, dom_cost) %>%
  pivot_longer(cols = c(cog_cost_D, cog_cost_ND, dom_cost), 
               names_to = "Cost_Type", 
               values_to = "Delta_Value") %>%
  drop_na(Delta_Value)

# 4. VISUALIZACIÓN 1: THROUGHPUT (BPS)
# Recordatorio: Negativo = Peor rendimiento
ggplot(costs_long %>% filter(Metric == "Throughput (bps)"), 
       aes(x = as.factor(Dia), y = Delta_Value, fill = Cost_Type)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Performance Costs: Throughput Efficiency",
       subtitle = "Negative values = Performance degradation",
       x = "Session (Day)", y = "Delta TP (bps)", fill = "Cost Type") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"),
                    labels = c("Cognitive Cost (D)", "Cognitive Cost (ND)", "Dominance Cost")) +
  theme_minimal()

# 5. VISUALIZACIÓN 2: MOVEMENT TIME (MS)
# Recordatorio: Positivo = Más tiempo empleado (Peor rendimiento)
ggplot(costs_long %>% filter(Metric == "Movement Time (ms)"), 
       aes(x = as.factor(Dia), y = Delta_Value, fill = Cost_Type)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Performance Costs: Movement Time (Rapidity)",
       subtitle = "Positive values = Slower performance (Cost)",
       x = "Session (Day)", y = "Delta MT (ms)", fill = "Cost Type") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"),
                    labels = c("Cognitive Cost (D)", "Cognitive Cost (ND)", "Dominance Cost")) +
  theme_minimal()

