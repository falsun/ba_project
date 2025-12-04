# ---------------------------------------------------------------------------- #
#   Script: 13_z_test_comparison.R
#   Description: Paternoster-Clogg Z-test for equality of regression coefficients
#                Comparing Model 6 (Pre) vs Model 6 (Post)
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(broom)
library(sandwich)
library(lmtest)
library(here)
library(gt)

DIR_SCRIPTS <- here::here("scripts")

# Source Theme Function (if exists)
if (file.exists(here::here("scripts", "00_functions.R"))) {
  source(here::here("scripts", "00_functions.R"))
} else {
  # Fallback if function script missing
  theme_gt_bachelor_project <- function(data) { data }
}

# 0. Load Data
# Ensure data is loaded so the script is standalone
ols_data <- readRDS(here("data", "_processed", "ols_data.rds"))

# 1. Define the models (Model 6: Full Specification)
# We use the full model to ensure we control for all competing factors
model_pre  <- lm(milex_usd_pre ~ dist_enemy_log + border_rus + nato_gap_2014 + debt_gdp_2014_log, data = ols_data)
model_post <- lm(milex_usd_post ~ dist_enemy_log + border_rus + nato_gap_2021 + debt_gdp_2021_log, data = ols_data)

# 2. Extract Coefficients and SEs (HC3)
# Helper function to pull stats for a specific variable from a specific model
get_stats <- function(model, var_name) {
  # Calculate Robust SEs
  res <- coeftest(model, vcov = vcovHC(model, type = "HC3"))

  # Return list with Beta and Standard Error
  list(
    beta = res[var_name, "Estimate"],
    se   = res[var_name, "Std. Error"]
  )
}

# 3. Calculate Z-Score
# Updated to accept different names for Pre and Post variables
compare_coeffs <- function(label, var_pre, var_post) {

  stats_pre  <- get_stats(model_pre, var_pre)
  stats_post <- get_stats(model_post, var_post)

  # Paternoster Clogg Z-test Formula
  # Z = (b_post - b_pre) / sqrt(SE_pre^2 + SE_post^2)
  # Tests if the DIFFERENCE is significantly different from zero
  z_score <- (stats_post$beta - stats_pre$beta) / sqrt(stats_pre$se^2 + stats_post$se^2)

  # Calculate P-value (Two-tailed)
  p_value <- 2 * (1 - pnorm(abs(z_score)))

  return(tibble(
    Variable  = label,
    Beta_Pre  = stats_pre$beta,
    Beta_Post = stats_post$beta,
    Diff      = stats_post$beta - stats_pre$beta,
    Z_Score   = z_score,
    P_Value   = p_value,
    Signif    = ifelse(p_value < 0.05, "*", "ns") # Quick visual flag
  ))
}

# 4. Run Tests
# We map the 2014 variables to their 2021 counterparts
z_results <- bind_rows(

  # H2: Threat (Distance) - Same name
  compare_coeffs("Log afstand til fjende (km)", "dist_enemy_log", "dist_enemy_log"),

  # H2: Threat (Border) - Same name
  compare_coeffs("Delt grænse med Rusland (dummy)", "border_rus", "border_rus"),

  # H3: Institutions (NATO Gap) - Different names
  compare_coeffs("Afstand til 2% mål", "nato_gap_2014", "nato_gap_2021"),

  # H4: Economics (Debt) - Different names
  compare_coeffs("Log offentlig gæld (% BNP)", "debt_gdp_2014_log", "debt_gdp_2021_log")
)

# 5. Output
print(z_results)

# Optional: Save to file if needed
# write_csv(z_results, here("_output", "_tables", "z_test_results.csv"))


# ---------------------------------------------------------------------------- #
# 5. FORMAT AND SAVE TABLE
# ---------------------------------------------------------------------------- #

# Function to format p-values (Reuse from Script 11)
format_p_val <- function(x) {
  sapply(x, function(val) {
    if (is.na(val)) return(NA)
    if (val < 0.001) return("<0,001")
    formatC(val, digits = 3, format = "f", decimal.mark = ",")
  })
}

z_table <- z_results %>%
  # Select only the columns we need for the final table
  select(Variable, Beta_Pre, Beta_Post, Diff, Z_Score, P_Value) %>%
  gt() %>%

  # Title and Subtitle
  tab_header(
    title = "Sammenligning af Effekter (Z-Test) - Forsvarsbudget (US$)",
    subtitle = "Test for signifikant forskel i koefficienter mellem præ- og post-invasion"
  ) %>%

  # Column Labels (Danish)
  cols_label(
    Variable  = "Variabel",
    Beta_Pre  = "Koeff. (2014-21)",
    Beta_Post = "Koeff. (2021-25)",
    Diff      = "Ændring",
    Z_Score   = "Z-Score",
    P_Value   = "P-værdi"
  ) %>%
  theme_gt_bachelor_project() %>%
  # P-value Formatting (<0,001)
  fmt(
    columns = c(P_Value),
    fns = format_p_val
  ) %>%

  # Add Footnote explaining the test
  tab_source_note(
    source_note = "Note: Z-test baseret på Paternoster et al. (1998) formel med HC3 robuste standardfejl. Tester nulhypotesen om at forskellen mellem koefficienterne er nul."
  )

# Save
gtsave(z_table, here("_output", "_tables", "z_test_summary_usd.html"))

message("Z-Test Table saved to _output/_tables/z_test_summary_usd.html")
