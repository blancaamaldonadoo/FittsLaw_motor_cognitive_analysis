# =========================================================================
# 00_quality_check.R
# Purpose:  Load raw data and verify dataset integrity
# Input:    DATOS_TFG.xlsx (sheet: "TODO")
# Output:   datos, balance_control, missing_values_summary
# =========================================================================

library(readxl)
library(tidyverse)

# -------------------------------------------------------------------------
# 1. LOAD DATA
# -------------------------------------------------------------------------

datos <- read_excel("DATOS_TFG.xlsx", sheet = "TODO")

cat("Dataset loaded:", nrow(datos), "rows |", 
    n_distinct(datos$Sujeto), "subjects |",
    n_distinct(datos$Condicion), "conditions |",
    n_distinct(datos$Dia), "days\n")

# -------------------------------------------------------------------------
# 2. BALANCE CHECK
# Verifies every subject has one entry per day and condition
# -------------------------------------------------------------------------

balance_control <- datos %>%
  count(Sujeto, Dia, Condicion, name = "row_count") %>%
  complete(Sujeto, Dia, Condicion, fill = list(row_count = 0))

# Flag any missing combinations
missing_combinations <- balance_control %>% filter(row_count == 0)

if (nrow(missing_combinations) == 0) {
  cat("Balance check PASSED: no missing subject/day/condition combinations.\n")
} else {
  cat("WARNING: missing combinations detected:\n")
  print(missing_combinations)
}

# -------------------------------------------------------------------------
# 3. MISSING VALUES CHECK
# -------------------------------------------------------------------------

missing_values_summary <- datos %>%
  summarise(across(everything(), ~sum(is.na(.x))))

cat("\nMissing values per variable:\n")
print(missing_values_summary)
