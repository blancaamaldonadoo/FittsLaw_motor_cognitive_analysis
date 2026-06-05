# =========================================================================
# 11_linear_mixed_models.R
# Purpose:  Fit linear mixed-effects models for TP, ER, and cognitive Errors
# Input:    long_data         (from 06_boxplots.R)
#           datos_transformados (from 01_transformations.R)
#           datos_cognitivos    (from 01_transformations.R)
# Output:   Tables 3-11 (model results, ANOVA, post-hoc comparisons)
# Packages: lme4, lmerTest, emmeans, performance
# =========================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(see)

# =========================================================================
# 1. DATA PREPARATION
# =========================================================================

# --- TP: filter from long_data and factorize ---
data_tp <- long_data %>%
  filter(Metric == "Throughput (bps)") %>%
  mutate(
    Mano  = factor(ifelse(grepl("Non-Dominant", Condicion), "ND", "D"),
                   levels = c("D", "ND")),
    Carga = factor(ifelse(grepl("Load", Condicion), "C", "NC"),
                   levels = c("NC", "C")),
    Dia   = relevel(as.factor(Dia), ref = "1")
  )

# --- ER: use arcsine-transformed variable from datos_transformados ---
data_error <- datos_transformados %>%
  mutate(
    Mano  = factor(Mano, levels = c("D", "ND")),
    Carga = factor(Carga, levels = c("NC", "C")),
    Dia   = relevel(as.factor(Dia), ref = "1")
  )

# --- Cognitive Errors: dual-task only ---
datos_cognitivos <- datos_transformados %>%
  filter(Carga == "C") %>%
  mutate(
    Mano = factor(Mano, levels = c("D", "ND")),
    Dia  = relevel(as.factor(Dia), ref = "1")
  )

cat("Data prepared — TP:", nrow(data_tp), "rows |",
    "ER:", nrow(data_error), "rows |",
    "Errors:", nrow(datos_cognitivos), "rows\n")

# =========================================================================
# 2. THROUGHPUT MODEL (TP)
# =========================================================================

# 2.1 Null model and ICC
mod_null_tp <- lmer(Value ~ 1 + (1 | Sujeto), data = data_tp)
icc_tp      <- performance::icc(mod_null_tp)
cat("\nICC TP:", round(icc_tp$ICC_unadjusted, 4), "\n")

# 2.2 Interaction model
mod_tp_interaccion <- lmer(Value ~ Mano * Carga * Dia + (1 | Sujeto),
                           data = data_tp)
cat("\n--- ANOVA Type III: TP Interaction Model ---\n")
print(anova(mod_tp_interaccion, type = 3))

# 2.3 Post-hoc: Hand × Load (Table 5)
cat("\n--- Post-hoc: Hand:Load interaction (TP) ---\n")
emm_tp_mano_carga <- emmeans(mod_tp_interaccion, pairwise ~ Carga | Mano)
print(emm_tp_mano_carga$contrasts)

# 2.4 Post-hoc: Hand × Day (Table 6)
cat("\n--- Post-hoc: Hand:Day interaction (TP) ---\n")
emm_tp_mano_dia <- emmeans(mod_tp_interaccion, pairwise ~ Dia | Mano)
print(emm_tp_mano_dia$contrasts)

# 2.5 Model comparison (Table 3)
cat("\n--- Model Comparison: TP (AIC, Chi-square) ---\n")
comp_tp <- anova(mod_null_tp, mod_tp_interaccion)
print(comp_tp)

# =========================================================================
# 3. ERROR RATE MODEL (ER)
# =========================================================================

# 3.1 Null model and ICC
mod_null_error <- lmer(trans_Error ~ 1 + (1 | Sujeto), data = data_error)
icc_error      <- performance::icc(mod_null_error)
cat("\nICC ER:", round(icc_error$ICC_unadjusted, 4), "\n")

# 3.2 Interaction model
mod_error_interaccion <- lmer(trans_Error ~ Mano * Carga * Dia + (1 | Sujeto),
                              data = data_error)
cat("\n--- ANOVA Type III: ER Interaction Model ---\n")
print(anova(mod_error_interaccion, type = 3))

# 3.3 Post-hoc: Hand × Load (Table 8)
cat("\n--- Post-hoc: Hand:Load interaction (ER) ---\n")
emm_error_mano_carga <- emmeans(mod_error_interaccion, pairwise ~ Carga | Mano)
print(emm_error_mano_carga$contrasts)

# 3.4 Model comparison (Table 3)
cat("\n--- Model Comparison: ER (AIC, Chi-square) ---\n")
comp_error <- anova(mod_null_error, mod_error_interaccion)
print(comp_error)

# =========================================================================
# 4. COGNITIVE ERRORS MODEL (Errors)
# =========================================================================

# 4.1 Null model and ICC
mod_cog_nulo <- lmer(ERRORES ~ 1 + (1 | Sujeto), data = datos_cognitivos)
icc_cog      <- performance::icc(mod_cog_nulo)
cat("\nICC Cognitive Errors:", round(icc_cog$ICC_unadjusted, 4), "\n")

# 4.2 Interaction model (Load excluded — dual-task only)
mod_cog_interaccion <- lmer(ERRORES ~ Mano * Dia + (1 | Sujeto),
                            data = datos_cognitivos)
cat("\n--- ANOVA Type III: Errors Interaction Model ---\n")
print(anova(mod_cog_interaccion, type = 3))

# 4.3 Post-hoc: Day effect (Table 10)
cat("\n--- Post-hoc: Day effect (Errors) ---\n")
emm_cog_dias <- emmeans(mod_cog_interaccion, pairwise ~ Dia)
print(emm_cog_dias$contrasts)

# 4.4 Post-hoc: Hand effect (Table 11)
cat("\n--- Post-hoc: Hand effect (Errors) ---\n")
emm_cog_mano <- emmeans(mod_cog_interaccion, pairwise ~ Mano)
print(emm_cog_mano$contrasts)

# 4.5 Model comparison (Table 3)
cat("\n--- Model Comparison: Errors (AIC, Chi-square) ---\n")
comp_cog <- anova(mod_cog_nulo, mod_cog_interaccion)
print(comp_cog)

# =========================================================================
# 5. MODEL DIAGNOSTICS (performance::check_model)
# =========================================================================

cat("\nRunning model diagnostics — this may take a moment...\n")
plot(performance::check_model(mod_tp_interaccion))
plot(performance::check_model(mod_error_interaccion))
plot(performance::check_model(mod_cog_interaccion))
