# --- Normality Check (Shapiro-Wilk Test) ---

normality_test <- long_data %>%
  # Grouping by Metric, Condition, and Day to check distribution per subgroup
  group_by(Metric, Condicion, Dia) %>% 
  summarise(
    shapiro_p = shapiro.test(Value)$p.value,
    .groups = 'drop'
  )

# Display results
print(normality_test)

# Technical summary for your thesis/report
non_normal_count <- sum(normality_test$shapiro_p < 0.05)
total_groups <- nrow(normality_test)

cat("\nTechnical Summary for your Thesis:\n")
cat("A total of", total_groups, "operational groups were analyzed.\n")
cat("Out of these,", non_normal_count, "groups show evidence of NON-normality (p < 0.05).\n")
