library(readxl)
library(tidyverse)
library(flextable)
library(officer)

# 1. Define Summary Function (Internal names in English)
# Calculates Mean, SD, SE, and 95% Confidence Intervals
summary_mean_ci <- function(data, variable) {
  data %>%
    summarise(
      n = n(),
      mean = mean({{variable}}, na.rm = TRUE),
      sd = sd({{variable}}, na.rm = TRUE),
      se = sd / sqrt(n),
      ci_lower = mean - (1.96 * se),
      ci_upper = mean + (1.96 * se)
    ) %>%
    ungroup()
}

# 2. Transform and TRANSLATE Content
# Convert to long format and translate factor levels for international standards
long_data <- datos %>%
  pivot_longer(
    cols = c(MTavg, TP_avg_2d), 
    names_to = "Metric", 
    values_to = "Value"
  ) %>%
  mutate(
    # Numerical format correction (handling commas as decimals if present)
    Value = as.numeric(gsub(",", ".", as.character(Value))),
    
    # Translate Metrics to formal Fitts' Law terminology
    Metric = recode(Metric, 
                    "MTavg" = "Movement Time (ms)", 
                    "TP_avg_2d" = "Throughput (bps)"),
    
    # Translate Experimental Conditions
    Condicion = recode(Condicion,
                       "D" = "Dominant",
                       "ND" = "Non-Dominant",
                       "Carga D" = "Dominant + Load",
                       "Carga ND" = "Non-Dominant + Load")
  )

# 3. Calculate Descriptive Statistics
# Grouped by Metric, Condition, and Day
descriptives <- long_data %>%
  group_by(Metric, Condicion, Dia) %>%
  summary_mean_ci(Value)

# 4. Format Final Table with English Headers
# Creates a publication-ready column "Mean ± SD"
final_tfg_table <- descriptives %>%
  mutate(Report = paste0(round(mean, 2), " ± ", round(sd, 2))) %>%
  select(
    Metric, 
    Condition = Condicion, 
    Day = Dia, 
    n, 
    `Mean ± SD` = Report
  )

# Display results in console
print(final_tfg_table)

# --- Table Export for Word (APA-style formatting) ---

# Define border style (black, 1pt thickness)
std_border = fp_border(color="black", width = 1)

# Create the flextable with a boxed layout
ft <- flextable(final_tfg_table) %>%
  # 1. Apply boxed theme (standard borders)
  theme_box() %>%
  # 2. Reinforce borders with custom style
  border_outer(border = std_border, part = "all") %>%
  border_inner(border = std_border, part = "all") %>%
  # 3. Grey background for headers
  bg(bg = "#EFEFEF", part = "header") %>%
  # 4. Bold header text
  bold(part = "header") %>%
  # 5. Center-align all content
  align(align = "center", part = "all") %>%
  # 6. Autofit to Word page width
  set_table_properties(layout = "autofit")

# Save as a .docx file
save_as_docx(ft, path = "Descriptive_Statistics_Table.docx")

