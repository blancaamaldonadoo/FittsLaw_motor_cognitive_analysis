# =========================================================================
# 02_descriptives.R
# Purpose:  Compute descriptive statistics for MT and TP
# Input:    datos (from 00_quality_check.R)
# Output:   long_data, descriptives, final_tfg_table
#           Descriptive_Statistics_Table.docx
# =========================================================================

library(readxl)
library(tidyverse)
library(flextable)
library(officer)

# -------------------------------------------------------------------------
# 1. HELPER FUNCTION
# Computes mean, SD, SE, and 95% confidence intervals
# -------------------------------------------------------------------------

summary_mean_ci <- function(data, variable) {
  data %>%
    summarise(
      n        = n(),
      mean     = mean({{variable}}, na.rm = TRUE),
      sd       = sd({{variable}}, na.rm = TRUE),
      se       = sd / sqrt(n),
      ci_lower = mean - (1.96 * se),
      ci_upper = mean + (1.96 * se)
    ) %>%
    ungroup()
}

# -------------------------------------------------------------------------
# 2. RESHAPE TO LONG FORMAT
# Pivots MT and TP into a single Value column for grouped analysis
# -------------------------------------------------------------------------

long_data <- datos %>%
  pivot_longer(
    cols      = c(MTavg, TP_avg_2d),
    names_to  = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    # Numeric correction (handles commas as decimal separators if present)
    Value = as.numeric(gsub(",", ".", as.character(Value))),

    # Rename metrics to formal Fitts' Law terminology
    Metric = recode(Metric,
                    "MTavg"     = "Movement Time (ms)",
                    "TP_avg_2d" = "Throughput (bps)"),

    # Rename conditions to English labels
    Condicion = recode(Condicion,
                       "D"        = "Dominant",
                       "ND"       = "Non-Dominant",
                       "Carga D"  = "Dominant + Load",
                       "Carga ND" = "Non-Dominant + Load")
  )

# -------------------------------------------------------------------------
# 3. DESCRIPTIVE STATISTICS
# Grouped by Metric, Condition, and Session Day
# -------------------------------------------------------------------------

descriptives <- long_data %>%
  group_by(Metric, Condicion, Dia) %>%
  summary_mean_ci(Value)

# -------------------------------------------------------------------------
# 4. PUBLICATION-READY TABLE
# Formats results as "Mean ± SD" for thesis reporting
# -------------------------------------------------------------------------

final_tfg_table <- descriptives %>%
  mutate(Report = paste0(round(mean, 2), " \u00b1 ", round(sd, 2))) %>%
  select(
    Metric,
    Condition = Condicion,
    Day       = Dia,
    n,
    `Mean ± SD` = Report
  )

print(final_tfg_table)

# -------------------------------------------------------------------------
# 5. EXPORT TO WORD (.docx)
# -------------------------------------------------------------------------

std_border <- fp_border(color = "black", width = 1)

ft <- flextable(final_tfg_table) %>%
  theme_box() %>%
  border_outer(border = std_border, part = "all") %>%
  border_inner(border = std_border, part = "all") %>%
  bg(bg = "#EFEFEF", part = "header") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_table_properties(layout = "autofit")

save_as_docx(ft, path = "Descriptive_Statistics_Table.docx")
cat("Table exported: Descriptive_Statistics_Table.docx\n")
