# ---------------------------------------------------------------------------- #
#
#   Script:       11_ols_main_analysis.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Description:  Main OLS Analysis for Part 2 (Heterogeneity).
#                 Runs the "Horse Race" between Realism (Threat) and
#                 Liberalism (Institutional Pressure), with controls.
#                 Generates four final regression tables.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION ===========================================================
message("--- Section 0: Loading Configuration ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, broom, modelsummary, here, glue, gt)

DIR_DATA_PROC <- here::here("data", "_processed")
DIR_TAB       <- here::here("_output", "_tables", "_reg_ols")
OLS_DATA      <- file.path(DIR_DATA_PROC, "ols_data.rds")

if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!file.exists(OLS_DATA)) stop("Run 09_ols_data_trans.R first.")

# 1. LOAD DATA ===============================================================
message("--- Section 1: Loading Data ---")

# We assume all transformations (logs, sqrts) are already done in Script 09.
df <- readRDS(OLS_DATA)

# 2. MODELING FUNCTION =======================================================
# This function runs the "Build-Up" strategy: Baseline -> Combined -> Robustness
run_horse_race <- function(data, y_var, x_realist_1, x_realist_2, x_liberal, title, filename) {

  message(glue("Estimating models for: {title}"))

  # 1. Realism Baseline (Does Distance matter alone?)
  m1 <- lm(as.formula(glue("{y_var} ~ {x_realist_1}")), data = data)

  # 2. Liberalism Baseline (Does the 2% Gap matter alone?)
  m2 <- lm(as.formula(glue("{y_var} ~ {x_realist_2}")), data = data)

  # 3. The Horse Race (Combined) - The Critical Test
  m3 <- lm(as.formula(glue("{y_var} ~ {x_liberal}")), data = data)

  # 4. + Economic Capacity (Wealth) - Testing Wagner's Law
  m4 <- lm(as.formula(glue("{y_var} ~ {x_realist_1} + {x_realist_2} + {x_liberal}")), data = data)

  # Generate Table
  modelsummary(
    list(
      "(1)"   = m1,
      "(2)"   = m2,
      "(3)"  = m3,
      "(4)"   = m4
    ),
    stars = TRUE,
    # Standard Error Robustness: Using HC1 (Standard for Small N Heteroskedasticity)
    vcov = "HC1",
    gof_map = c("nobs", "adj.r.squared", "f"),
    coef_map = c(
      "dist_conf_log"       = "Afstand til konfliktzone (Log)",
      "border_rus"         = "Delt grænse med Rusland (dummy)",
      "debt_gdp_2021"      = "Offentlig gæld 2021 (% af BNP)",
      "debt_gdp_2014"      = "Offentlig gæld 2014 (% af BNP)",
      "(Intercept)"        = "Constant"
    ),
    title = title,
    output = "gt",
    stars = c('*' = .05, '**' = .01, '***' = .001),
  ) %>%
    # Add footnote about SEs
    tab_footnote(footnote = "Standard Errors are Heteroskedasticity-Consistent (HC1).") %>%
    theme_gt_bachelor_project() %>%
    gtsave(filename = file.path(DIR_TAB, filename))

  message(glue("Saved table to: {filename}"))
}


# 3. EXECUTE ANALYSIS LOOP ===================================================
message("--- Section 3: Executing Models ---")

# Note: We use the _log suffix for variables that required transformation (GDP, Trade, Distance)
# and raw names for those that didn't (Debt, Democracy, Gap).

# --- A. War Shock (2021-2024) - Milex GDP ---
# Best Fit: Log-Linear
run_horse_race(
  data = df,
  y_var = "milex_gdp_post",
  x_realist_1 = "dist_conf_log",
  x_realist_2 = "border_rus",
  x_liberal = "debt_gdp_2021",
  title = "Table 1: Tværsnits OLS 2021-25 - Forsvarsbudget (% af BNP)",
  filename = "ols_post.html"
)

# --- C. Crimea Legacy (2014-2021) - Milex GDP ---
# Best Fit: Log-Linear
run_horse_race(
  data = df,
  y_var = "milex_gdp_pre",
  x_realist_1 = "dist_conf_log",
  x_realist_2 = "border_rus",
  x_liberal = "debt_gdp_2014",
  title = "Table 1: Tværsnits OLS 2014-21 - Forsvarsbudget (% af BNP)",
  filename = "ols_pre.html"
)

message("\n--- Script 11_ols_main_analysis.R finished ---")
message("All regression tables saved to _output/_tables/_reg_ols/")
