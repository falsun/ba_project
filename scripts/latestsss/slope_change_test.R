# ---------------------------------------------------------------------------- #
#
#   Script:       12_ols_structural_change_test.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Description:  Tests for Structural Break in the Distance Coefficient.
#                 Uses a Pooled Interaction Model to see if the slope of
#                 distance changed significantly between Pre- and Post-Invasion.
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
if (!file.exists(OLS_DATA)) stop("ols_data.rds not found. Run Data Prep first.")


# 1. PREPARE STACKED DATA ====================================================
message("--- Section 1: Stacking Data for Interaction Test ---")

df_wide <- readRDS(OLS_DATA)

# We need to "pivot longer" so we have one row per Country-Period
# This creates a dataset with N = 44 (22 countries * 2 periods)

df_stacked <- df_wide %>%
  select(
    iso3c,
    dist_enemy_log,
    # Outcome Variables
    change_gdp_pre = milex_usd_pre,
    change_gdp_post = milex_usd_post
  ) %>%
  pivot_longer(
    cols = starts_with("change_"),
    names_to = c("outcome_type", "period"),
    names_pattern = "change_(.*)_(.*)",
    values_to = "change_value"
  ) %>%
  pivot_wider(
    names_from = outcome_type,
    values_from = change_value
  ) %>%
  mutate(
    # Create Dummy: 1 if Post-Invasion, 0 if Pre-Invasion
    is_post_invasion = if_else(period == "post", 1, 0)
  )

# Check structure
print(head(df_stacked))


# 2. RUN INTERACTION MODELS ==================================================
message("--- Section 2: Estimating Structural Break ---")

# --- Model A: GDP Share ---
# We test if the slope of dist_enemy_log is different when is_post_invasion == 1
m_gdp_break <- lm(gdp ~ dist_enemy_log * is_post_invasion, data = df_stacked)


# 3. OUTPUT RESULTS ==========================================================
message("--- Section 3: Generating Test Tables ---")

modelsummary(
  list(
    "GDP Share (Structural Break)" = m_gdp_break
  ),
  stars = TRUE,
  vcov = "HC1", # Robust SEs are crucial here because variance likely changed too
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  coef_map = c(
    "dist_enemy_log" = "Distance Slope (Pre-Invasion)",
    "is_post_invasion" = "Intercept Shift (Post-Invasion)",
    "dist_enemy_log:is_post_invasion" = "CHANGE in Slope (Interaction)",
    "(Intercept)" = "Constant"
  ),
  title = "Test of Structural Change: Did the Distance Effect Change Significantly?",
  output = "gt"
) %>%
  tab_style(
    style = list(cell_fill(color = "#e6f7ff"), cell_text(weight = "bold")),
    locations = cells_body(rows = 3) # Highlight the Interaction Row
  ) %>%
  gtsave(filename = file.path(DIR_TAB, "table_structural_break_test.html"))

message(glue("Structural Break Table saved to: {DIR_TAB}/table_structural_break_test.html"))
