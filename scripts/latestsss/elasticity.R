# ---------------------------------------------------------------------------- #
#   Script:       13_calculate_elasticity.R
#   Description:  Calculates Elasticity at the Mean to compare coefficients
#                  across different units (GDP vs Capita).
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, broom, gt, glue, here)

DIR_DATA <- here::here("data", "_processed")
MASTER_PANEL <- file.path(DIR_DATA, "master_panel.rds")
master_panel <- readRDS(MASTER_PANEL)

# 1. PREPARE DATA
df_cross <- master_panel %>%
  filter(group == "treatment") %>%
  filter(year %in% c(2014, 2021, 2024)) %>%
  select(iso3c, year, dist_min, dist_min_log, milex_cap, milex_gdp)

df_delta <- df_cross %>%
  pivot_wider(names_from = year, values_from = c(milex_cap, milex_gdp)) %>%
  mutate(
    # Calculate Changes (Outcomes)
    change_cap_total = milex_cap_2024 - milex_cap_2014,
    change_cap_crimea = milex_cap_2021 - milex_cap_2014,
    change_cap_war = milex_cap_2024 - milex_cap_2021,
    change_gdp_total = milex_gdp_2024 - milex_gdp_2014,
    change_gdp_crimea = milex_gdp_2021 - milex_gdp_2014,
    change_gdp_war = milex_gdp_2024 - milex_gdp_2021
  )

# 2. CALCULATION FUNCTION
calculate_elasticity <- function(data, y_col, label) {
  # 1. Run Log-Linear Model
  model <- lm(as.formula(glue("{y_col} ~ dist_min_log")), data = data)
  slope_beta <- coef(model)[2] # Extract Beta

  # 2. Calculate Mean of Outcome (Y-bar)
  mean_y <- mean(data[[y_col]], na.rm = TRUE)

  # 3. Calculate Elasticity at the Mean (Beta / Mean_Y)
  elasticity <- slope_beta / mean_y

  return(tibble(
    Period_Variable = label,
    Slope_Beta = slope_beta,
    Mean_Outcome = mean_y,
    Elasticity = elasticity
  ))
}

# 3. RUN COMPARISON
results <- bind_rows(
  # --- Total Shift ---
  calculate_elasticity(df_delta, "change_gdp_total", "GDP: Total (2014-24)"),
  calculate_elasticity(df_delta, "change_cap_total", "Cap: Total (2014-24)"),

  # --- Crimea Legacy ---
  calculate_elasticity(df_delta, "change_gdp_crimea", "GDP: Crimea (2014-21)"),
  calculate_elasticity(df_delta, "change_cap_crimea", "Cap: Crimea (2014-21)"),

  # --- War Shock ---
  calculate_elasticity(df_delta, "change_gdp_war", "GDP: War Shock (21-24)"),
  calculate_elasticity(df_delta, "change_cap_war", "Cap: War Shock (21-24)")
)

# 4. PRINT RESULTS
results %>%
  mutate(
    Slope_Beta = round(Slope_Beta, 3),
    Mean_Outcome = round(Mean_Outcome, 3),
    Elasticity = round(Elasticity, 3),
    Interpretation = glue("1% distance increase = {abs(Elasticity)}% drop in spending boost")
  ) %>%
  print()
