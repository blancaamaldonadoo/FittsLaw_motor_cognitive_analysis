# 1. Load libraries (this enables the read_excel function and tidyverse suite)
library(readxl)
library(tidyverse)

# Load data from the specified path and sheet
# Path: "Tablas_TFG_con_Fallos.xlsx"
# Sheet: "TODO" (renamed to 'raw_data' or kept as 'datos' for logic)
datos <- read_excel('/Users/blancamaldonado/Library/Mobile Documents/com~apple~CloudDocs/Blanca/Uni/4º Carrera/TFG/DATOS_TFG.xlsx', sheet="TODO", skip = 0)

# Check balance and experimental design completeness
balance_control <- datos %>%
  # Group values by Subject, Day, and Condition, creating a new 'row_count' column
  count(Sujeto, Dia, Condicion, name = "row_count") %>%
  
  # Ensure every Subject has an entry for every Day and Condition; 
  # if a combination is missing, assign 0 to row_count
  complete(Sujeto, Dia, Condicion, fill = list(row_count = 0))

# Quantify missing values per variable
# This summarizes how many empty (NA) values exist in each column of the dataset.
missing_values_summary <- datos %>%
  # Reduce the dataset to a single summary row.
  # across(everything()) applies the operation to all columns in the dataframe.
  summarise(across(everything(), ~sum(is.na(.x))))

