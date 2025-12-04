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

library(conflicted)
library(tidyverse)
library(here)
library(modelsummary)
library(gt)
library(glue)
library(broom)

DIR_DATA      <- file.path("data", "_processed", "ols_data.rds")
DIR_SCRIPTS   <- here::here("scripts")
DIR_TAB       <- here::here("_output", "_tables", "_ols_models")

if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!file.exists(DIR_DATA)) stop("Run 09_ols_data_trans.R first.")

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

source(here::here("scripts", "00_functions.R"))

options(OutDec = ",")


# 1. LOAD DATA ===============================================================
message("--- Section 1: Loading Data ---")

# Cross-Section Data (for OLS)
ols_data <- readRDS(DIR_DATA)


# 2. RUN OLS MODELS ============================================================
# 2014-2021 Modeller
models_pre <- list(
  "(1)" = lm(milex_gdp_pre ~ dist_enemy_log, data = ols_data),
  "(2)" = lm(milex_gdp_pre ~ border_rus, data = ols_data),
  "(3)" = lm(milex_gdp_pre ~ debt_gdp_2014, data = ols_data),
  "(4)" = lm(milex_gdp_pre ~ dist_enemy_log + border_rus + debt_gdp_2014, data = ols_data)
)

# 2021-2025 Modeller
models_post <- list(
  "(1)" = lm(milex_gdp_post ~ dist_enemy_log, data = ols_data),
  "(2)" = lm(milex_gdp_post ~ border_rus, data = ols_data),
  "(3)" = lm(milex_gdp_post ~ debt_gdp_2021, data = ols_data),
  "(4)" = lm(milex_gdp_post ~ dist_enemy_log + border_rus + debt_gdp_2021, data = ols_data)
)

# 2014-2021 Tabel
modelsummary(
  models_pre,
  title = "Tværsnits OLS 2014-21 - Forsvarsudgifter (% af BNP)",
  output = "gt",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  vcov = "HC1",
  coef_map = c(
    "dist_enemy_log" = "Log afstand til konfliktzone (km)",
    "border_rus"    = "Delt grænse med Rusland (dummy)",
    "debt_gdp_2014" = "Offentlig gæld 2014 (% af BNP)",
    "(Intercept)"   = "Konstant"
  ),
  gof_map = c("nobs", "adj.r.squared")
) %>%
  theme_gt_bachelor_project() %>%
  gtsave(filename = file.path(DIR_TAB, "ols_pre.html"))

# 2021-2025 Tabel
modelsummary(
  models_post,
  title = "Tværsnits OLS 2021-25 - Forsvarsudgifter (% af BNP)",
  output = "gt",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  vcov = "HC1",
  coef_map = c(
    "dist_enemy_log" = "Log afstand til konfliktzone (km)",
    "border_rus"    = "Delt grænse med Rusland (dummy)",
    "debt_gdp_2021" = "Offentlig gæld 2021 (% af BNP)",
    "(Intercept)"   = "Konstant"
  ),
  gof_map = c("nobs", "adj.r.squared")
) %>%
  theme_gt_bachelor_project() %>%
  gtsave(filename = file.path(DIR_TAB, "ols_post.html"))

message("\n--- Script 11_ols_main_analysis.R finished ---")
message("All regression tables saved to: ", DIR_TAB)
