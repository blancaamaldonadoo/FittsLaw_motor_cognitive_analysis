# -------------------------------------------------------------------------
# CORRELATION ANALYSIS BETWEEN METRICS
# -------------------------------------------------------------------------
library(tidyverse)
library(corrplot) # Install if needed: install.packages("corrplot")

# 1. Data Preparation (selecting primary Fitts' Law metrics)
# We pivot the data back to wide format to correlate variables column-wise
correlation_data <- long_data %>%
  pivot_wider(names_from = Metric, values_from = Value) %>%
  # Using the standardized English names from previous steps
  select(`Movement Time (ms)`, `Throughput (bps)`, Error_pct) %>%
  drop_na()

# 2. Calculate Pearson Correlation Matrix
cor_matrix <- cor(correlation_data)

# 3. Visual Representation
# A circular correlation plot is highly effective for thesis presentations
corrplot(cor_matrix, 
         method = "circle", 
         type = "upper", 
         addCoef.col = "black", # Displays correlation coefficients
         tl.col = "black",      # Text label color
         tl.srt = 45,           # Rotates labels for better readability
         title = "\n\n Correlation: MT, TP, and Error Rate",
         diag = FALSE)

