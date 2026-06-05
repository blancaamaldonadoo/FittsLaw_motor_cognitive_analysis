# =========================================================================
# 1. PREPARACIÓN DE DATOS
# =========================================================================

# --- Preparación para TP ---
data_tp <- long_data %>%
  filter(Metric == "Throughput (bps)") %>%
  mutate(
    Mano = factor(ifelse(grepl("Non-Dominant", Condicion), "ND", "D"), levels = c("D", "ND")),
    Carga = factor(ifelse(grepl("Load", Condicion), "C", "NC"), levels = c("NC", "C")),
    Dia = relevel(as.factor(Dia), ref = "1")
  )

# --- Preparación para Error (Usando la transformada Arcoseno) ---
# Aseguramos que las columnas de agrupación existan y estén factorizadas igual
data_error <- datos_transformados %>%
  mutate(
    Mano = factor(Mano, levels = c("D", "ND")),
    Carga = factor(Carga, levels = c("NC", "C")),
    Dia = relevel(as.factor(Dia), ref = "1")
  )

# Asegurar factores en datos_cognitivos antes de los modelos
datos_cognitivos <- datos_cognitivos %>%
  mutate(
    Mano = factor(Mano, levels = c("D", "ND")),
    Dia = relevel(as.factor(Dia), ref = "1")
  )
datos_cognitivos <- datos_transformados %>% filter(Carga == "C")


# =========================================================================
# 2. MODELOS PARA THROUGHPUT (TP)
# =========================================================================

# 2.1. Modelo Nulo e ICC (TP)
mod_null_tp <- lmer(Value ~ 1 + (1 | Sujeto), data = data_tp)
icc_tp <- performance::icc(mod_null_tp)
print(paste("ICC TP:", icc_tp$ICC_unadjusted))

# 2.2. Modelo de Interacción (TP)
mod_tp_interaccion <- lmer(Value ~ Mano * Carga * Dia + (1 | Sujeto), data = data_tp)
anova_tp <- anova(mod_tp_interaccion, type = 3)
print(anova_tp)

# 2.3. Post-hocs (TP)
# Comparación de Carga dentro de cada Mano
emm_tp_mano_carga <- emmeans(mod_tp_interaccion, pairwise ~ Carga | Mano)
emm_tp_mano_carga$contrasts

# Comparación de Días dentro de cada Mano (Aprendizaje)
emm_tp_mano_dia <- emmeans(mod_tp_interaccion, pairwise ~ Dia | Mano)
emm_tp_mano_dia$contrasts

#Comparación, ajuste de modelos (AIC, Chi square)

comp_tp <- anova(mod_null_tp, mod_tp_interaccion)
print("Comparación de Modelos - Throughput:")
print(comp_tp)


# =========================================================================
# 3. MODELOS PARA ERROR (trans_Error)
# =========================================================================

# 3.1. Modelo Nulo e ICC (Error)
mod_null_error <- lmer(trans_Error ~ 1 + (1 | Sujeto), data = data_error)
icc_error <- performance::icc(mod_null_error)
print(paste("ICC Error:", icc_error$ICC_unadjusted))

# 3.2. Modelo de Interacción (Error)
mod_error_interaccion <- lmer(trans_Error ~ Mano * Carga * Dia + (1 | Sujeto), data = data_error)
anova_error <- anova(mod_error_interaccion, type = 3)
print(anova_error)

# 3.3. Post-hocs (Error)
# Comparación de Carga dentro de cada Mano
emm_error_mano_carga <- emmeans(mod_error_interaccion, pairwise ~ Carga | Mano)
emm_error_mano_carga$contrasts

# Comparación de Días dentro de cada Mano (Aprendizaje)
emm_error_mano_dia <- emmeans(mod_error_interaccion, pairwise ~ Dia | Mano)
emm_error_mano_dia$contrasts


# --- Comparación y ajuste de modelos (AIC) 
comp_error <- anova(mod_null_error, mod_error_interaccion)
print("Comparación de Modelos - Error:")
print(comp_error)


# =========================================================================
# 4. MODELOS PARA ERRORES EN CARGA COGNITIVA (Errores)
# =========================================================================

# 3.1. Modelo Nulo e ICC (Error)
mod_cog_nulo <- lmer (ERRORES ~ 1 + (1|Sujeto), data = datos_cognitivos)
icc_cog <- performance::icc(mod_cog_nulo)
print(paste("ICC errores cognitivos: ", icc_cog$ICC_unadjusted))

# 3.2. Modelo de Interacción (Error)
mod_cog_interaccion <- lmer(ERRORES ~ Mano * Dia + (1 | Sujeto), data = datos_cognitivos)
anova_cog <- anova(mod_cog_interaccion, type = 3)
print(anova_cog)

# 3.3. Post-hocs (Error)
# Comparación de Carga dentro de cada Mano
emm_cog_dias <- emmeans(mod_cog_interaccion, pairwise ~ Dia)
emm_cog_dias$contrasts

# Comparación de Días dentro de cada Mano (Aprendizaje)
emm_cog_mano <- emmeans(mod_cog_interaccion, pairwise ~ Mano)
emm_cog_mano$contrasts

# --- Comparación y ajuste de modelos (AIC) 
comp_cog <- anova(mod_cog_nulo, mod_cog_interaccion)
print("Comparación de Modelos - Errores cognitivos:")
print(comp_cog)


performance::check_model(mod_tp_interaccion)
performance::check_model(mod_error_interaccion)
performance::check_model(mod_cog_interaccion)


