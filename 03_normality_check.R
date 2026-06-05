# =========================================================================
# 03_normality_check.R
# Purpose:  Assess normality of MT and TP distributions (Shapiro-Wilk test)
# Input:    long_data (from 02_descriptives.R)
# Output:   normality_test (console summary)
# =========================================================================

# -------------------------------------------------------------------------
# 1. SHAPIRO-WILK TEST
# Applied per subgroup: Metric × Condition × Day
# H0: data follows a normal distribution (rejected if p < 0.05)
# -------------------------------------------------------------------------

normality_test <- long_data %>%
  group_by(Metric, Condicion, Dia) %>%
  summarise(
    shapiro_p = shapiro.test(Value)$p.value,
    normal    = ifelse(shapiro.test(Value)$p.value >= 0.05, "YES", "NO"),
    .groups   = "drop"
  )

print(normality_test)

# -------------------------------------------------------------------------
# 2. SUMMARY
# -------------------------------------------------------------------------

non_normal_count <- sum(normality_test$shapiro_p < 0.05)
total_groups     <- nrow(normality_test)

cat("\nNormality Check Summary (Shapiro-Wilk):\n")
cat("Total subgroups tested:", total_groups, "\n")
cat("Non-normal (p < 0.05):", non_normal_count, "\n")
cat("Normal     (p >= 0.05):", total_groups - non_normal_count, "\n")
cat("\nNote: mean and SD are retained for comparability with Fitts' Law literature,\n")
cat("but SD should be interpreted as dispersion, not as a strict probability interval.\n")
