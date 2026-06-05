# =========================================================================
# 01_transformations.R
# Generates: datos_transformados
# Depends on: datos (from 00_load_data.R)
# =========================================================================

library(tidyverse)

datos_transformados <- datos %>%
  mutate(
    # Factorize grouping variables
    Mano  = factor(ifelse(grepl("ND", Condicion), "ND", "D"), levels = c("D", "ND")),
    Carga = factor(ifelse(grepl("Carga", Condicion), "C", "NC"), levels = c("NC", "C")),
    Dia   = relevel(as.factor(Dia), ref = "1"),
    
    # Numeric correction (in case decimals are stored as commas)
    MTavg      = as.numeric(gsub(",", ".", as.character(MTavg))),
    TP_avg_2d  = as.numeric(gsub(",", ".", as.character(TP_avg_2d))),
    Error_pct  = as.numeric(gsub(",", ".", as.character(Error_pct))),
    r_2d       = as.numeric(gsub(",", ".", as.character(r_2d))),
    
    # Transformations
    trans_MT    = log10(MTavg),
    trans_Error = asin(sqrt(Error_pct)),
    z_Fisher_r  = atanh(r_2d)
  )

# Cognitive errors subset (dual-task only)
datos_cognitivos <- datos_transformados %>%
  filter(Carga == "C")

cat("datos_transformados creado con", nrow(datos_transformados), "filas y",
    n_distinct(datos_transformados$Sujeto), "sujetos.\n")

