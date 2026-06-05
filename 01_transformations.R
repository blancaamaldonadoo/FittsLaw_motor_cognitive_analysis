# =========================================================================
# 01_transformations.R
# Purpose:  Apply variable transformations required for LMM modeling
# Input:    datos (from 00_quality_check.R)
# Output:   datos_transformados, datos_cognitivos
# =========================================================================

library(tidyverse)

# -------------------------------------------------------------------------
# 1. FACTORIZATION AND NUMERIC CORRECTION
# -------------------------------------------------------------------------

datos_transformados <- datos %>%
  mutate(
    # Grouping variables as factors with reference levels
    Mano  = factor(ifelse(grepl("ND", Condicion), "ND", "D"), levels = c("D", "ND")),
    Carga = factor(ifelse(grepl("Carga", Condicion), "C", "NC"), levels = c("NC", "C")),
    Dia   = relevel(as.factor(Dia), ref = "1"),

    # Numeric correction (handles commas as decimal separators if present)
    MTavg     = as.numeric(gsub(",", ".", as.character(MTavg))),
    TP_avg_2d = as.numeric(gsub(",", ".", as.character(TP_avg_2d))),
    Error_pct = as.numeric(gsub(",", ".", as.character(Error_pct))),
    r_2d      = as.numeric(gsub(",", ".", as.character(r_2d))),

# -------------------------------------------------------------------------
# 2. VARIABLE TRANSFORMATIONS
# MT:        log10 — reduces positive skewness for LMM normality assumption
# Error_pct: arcsine-square-root — stabilizes variance of proportions
# r_2d:      Fisher's Z (atanh) — normalizes correlation coefficients
# -------------------------------------------------------------------------

    trans_MT    = log10(MTavg),
    trans_Error = asin(sqrt(Error_pct)),
    z_Fisher_r  = atanh(r_2d)
  )

# -------------------------------------------------------------------------
# 3. COGNITIVE ERRORS SUBSET
# Filters dual-task conditions only (Errors variable is only recorded here)
# -------------------------------------------------------------------------

datos_cognitivos <- datos_transformados %>%
  filter(Carga == "C")

# -------------------------------------------------------------------------
# 4. VERIFICATION
# -------------------------------------------------------------------------

cat("datos_transformados:", nrow(datos_transformados), "rows |",
    n_distinct(datos_transformados$Sujeto), "subjects\n")
cat("datos_cognitivos (dual-task only):", nrow(datos_cognitivos), "rows\n")
cat("New variables created: trans_MT, trans_Error, z_Fisher_r\n")
