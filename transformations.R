# -------------------------------------------------------------------------
# VISUALIZING FISHER'S Z-TRANSFORMATION (r to z)
# -------------------------------------------------------------------------
library(tidyverse)

# 1. Prepare data for comparison
comparison_r_data <- datos_transformados %>%
  select(r_2d, z_Fisher_r) %>%
  pivot_longer(cols = everything(), 
               names_to = "Transformation", 
               values_to = "Value") %>%
  mutate(
    Transformation = recode(Transformation, 
                            "r_2d" = "Original Correlation (r)", 
                            "z_Fisher_r" = "Fisher's z-transform"),
    # Convert to factor to force order: Original left, Transformed right
    Transformation = factor(Transformation, levels = c("Original Correlation (r)", "Fisher's z-transform"))
  )

# 2. Plotting
ggplot(comparison_r_data, aes(x = Value, fill = Transformation)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25, alpha = 0.5, color = "white") +
  geom_density(linewidth = 1) +
  facet_wrap(~Transformation, scales = "free") +
  labs(title = "Effect of Fisher's z-transformation on Correlation Coefficients",
       x = "Value", 
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 11))


# -------------------------------------------------------------------------
# VISUALIZING ARCSINE-SQUARE-ROOT TRANSFORMATION (Error Rate)
# -------------------------------------------------------------------------

# 1. Prepare data for comparison
comparison_error_data <- datos_transformados %>%
  select(Error_pct, trans_Error) %>%
  pivot_longer(cols = everything(), 
               names_to = "Transformation", 
               values_to = "Value") %>%
  mutate(
    Transformation = recode(Transformation, 
                            "Error_pct" = "Original Error (%)", 
                            "trans_Error" = "Arcsine-Square-Root Transform"),
    # Convert to factor to force order: Original left, Transformed right
    Transformation = factor(Transformation, levels = c("Original Error (%)", "Arcsine-Square-Root Transform"))
  )

# 2. Plotting
ggplot(comparison_error_data, aes(x = Value, fill = Transformation)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, alpha = 0.5, color = "white") +
  geom_density(linewidth = 1) +
  facet_wrap(~Transformation, scales = "free") +
  labs(title = "Effect of Arcsine Transformation on Error Rates",
       x = "Value", 
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 11))

