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
# 2014-2021 BNP modeller
models_pre_gdp <- list(
  "(1) Trussel gradient" = lm(milex_gdp_pre ~ dist_enemy_log, data = ols_data),
  "(2) Trussel binær" = lm(milex_gdp_pre ~ border_rus, data = ols_data),
  "(3) NATO pres" = lm(milex_gdp_pre ~ nato_gap_2014, data = ols_data),
  "(4) Økonomi" = lm(milex_gdp_pre ~ debt_gdp_2014_log, data = ols_data),
  "(5) Trussel vs. NATO pres" = lm(milex_gdp_pre ~ dist_enemy_log + border_rus + nato_gap_2014, data = ols_data),
  "(6) Trussel vs. NATO pres vs. økonomi" = lm(milex_gdp_pre ~ dist_enemy_log + border_rus + nato_gap_2014 + debt_gdp_2014_log, data = ols_data)
)

# 2021-2025 BNP modeller
models_post_gdp <- list(
  "(1) Trussel gradient" = lm(milex_gdp_post ~ dist_enemy_log, data = ols_data),
  "(2) Trussel binær" = lm(milex_gdp_post ~ border_rus, data = ols_data),
  "(3) NATO pres" = lm(milex_gdp_post ~ nato_gap_2021, data = ols_data),
  "(4) Økonomi" = lm(milex_gdp_post ~ debt_gdp_2021_log, data = ols_data),
  "(5) Trussel vs. NATO pres" = lm(milex_gdp_post ~ dist_enemy_log + border_rus + nato_gap_2021, data = ols_data),
  "(6) Trussel vs. NATO pres vs. økonomi" = lm(milex_gdp_post ~ dist_enemy_log + border_rus + nato_gap_2021 + debt_gdp_2021_log, data = ols_data)
)

# 2014-2021 USD modeller
models_pre_usd <- list(
  "(1) Trussel gradient" = lm(milex_usd_pre ~ dist_enemy_log, data = ols_data),
  "(2) Trussel binær" = lm(milex_usd_pre ~ border_rus, data = ols_data),
  "(3) NATO pres" = lm(milex_usd_pre ~ nato_gap_2014, data = ols_data),
  "(4) Økonomi" = lm(milex_usd_pre ~ debt_gdp_2014_log, data = ols_data),
  "(5) Trussel vs. NATO pres" = lm(milex_usd_pre ~ dist_enemy_log + border_rus + nato_gap_2014, data = ols_data),
  "(6) Trussel vs. NATO pres vs. økonomi" = lm(milex_usd_pre ~ dist_enemy_log + border_rus + nato_gap_2014 + debt_gdp_2014_log, data = ols_data)
)

# 2021-2024 USD modeller
models_post_usd <- list(
  "(1) Trussel gradient" = lm(milex_usd_post ~ dist_enemy_log, data = ols_data),
  "(2) Trussel binær" = lm(milex_usd_post ~ border_rus, data = ols_data),
  "(3) NATO pres" = lm(milex_usd_post ~ nato_gap_2021, data = ols_data),
  "(4) Økonomi" = lm(milex_usd_post ~ debt_gdp_2021_log, data = ols_data),
  "(5) Trussel vs. NATO pres" = lm(milex_usd_post ~ dist_enemy_log + border_rus + nato_gap_2021, data = ols_data),
  "(6) Trussel vs. NATO pres vs. økonomi" = lm(milex_usd_post ~ dist_enemy_log + border_rus + nato_gap_2021 + debt_gdp_2021_log, data = ols_data)
)

# 2014-2021 GDP Tabel
modelsummary(
  models_pre_gdp,
  title = "Tværsnits OLS 2014-21 - Forsvarsudgifter (% af BNP)",
  output = "gt",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  vcov = "HC3",
  coef_map = c(
    "dist_enemy_log" = "Log afstand til fjende (km)",
    "border_rus"    = "Delt grænse med Rusland (dummy)",
    "nato_gap_2014" = "Afstand til 2% mål 2014 (procentpoint)",
    "debt_gdp_2014_log" = "Log offentlig gæld 2014 (% af BNP)"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "Obs.", "fmt" = 0),
    list("raw" = "rmse", "clean" = "RMSE", "fmt" = 3),
    list("raw" = "adj.r.squared", "clean" = "Justeret R²", "fmt" = 3)
  ),
  notes = "Standardfejl er heteroskedasticitets-robuste (HC3). Konstantled er inkluderet i modellerne, men udeladt fra tabellen."
) %>%
  theme_gt_bachelor_project() %>%
  gtsave(filename = file.path(DIR_TAB, "ols_pre_gdp.html"))

# 2021-2025 BNP Tabel
modelsummary(
  models_post_gdp,
  title = "Tværsnits OLS 2021-25 - Forsvarsudgifter (% af BNP)",
  output = "gt",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  vcov = "HC3",
  coef_map = c(
    "dist_enemy_log" = "Log afstand til fjende (km)",
    "border_rus"    = "Delt grænse med Rusland (dummy)",
    "nato_gap_2021" = "Afstand til 2% mål 2021 (procentpoint)",
    "debt_gdp_2021_log" = "Log offentlig gæld 2021 (% af BNP)"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "Obs.", "fmt" = 0),
    list("raw" = "rmse", "clean" = "RMSE", "fmt" = 3),
    list("raw" = "adj.r.squared", "clean" = "Justeret R²", "fmt" = 3)
    ),
  notes = "Standardfejl er heteroskedasticitets-robuste (HC3). Konstantled er inkluderet i modellerne, men udeladt fra tabellen."
  ) %>%
  theme_gt_bachelor_project() %>%
  gtsave(filename = file.path(DIR_TAB, "ols_post_gdp.html"))

# 2014-2021 USD Tabel
modelsummary(
  models_pre_usd,
  title = "Tværsnits OLS 2014-21 - Forsvarsudgifter (2023 US$)",
  output = "gt",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  vcov = "HC3",
  coef_map = c(
    "dist_enemy_log" = "Log afstand til fjende (km)",
    "border_rus"    = "Delt grænse med Rusland (dummy)",
    "nato_gap_2014" = "Afstand til 2% mål 2014 (procentpoint)",
    "debt_gdp_2014_log" = "Log offentlig gæld 2014 (% af BNP)"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "Obs.", "fmt" = 0),
    list("raw" = "rmse", "clean" = "RMSE", "fmt" = 3),
    list("raw" = "adj.r.squared", "clean" = "Justeret R²", "fmt" = 3)
  ),
  notes = "Standardfejl er heteroskedasticitets-robuste (HC3). Konstantled er inkluderet i modellerne, men udeladt fra tabellen."
) %>%
  theme_gt_bachelor_project() %>%
  gtsave(filename = file.path(DIR_TAB, "ols_pre_usd.html"))

# 2021-2024 USD Tabel
modelsummary(
  models_post_usd,
  title = "Tværsnits OLS 2021-24 - Forsvarsudgifter (2023 US$)",
  output = "gt",
  stars = c('*' = .05, '**' = .01, '***' = .001),
  vcov = "HC3",
  coef_map = c(
    "dist_enemy_log" = "Log afstand til fjende (km)",
    "border_rus"    = "Delt grænse med Rusland (dummy)",
    "nato_gap_2021" = "Afstand til 2% mål 2021 (procentpoint)",
    "debt_gdp_2021_log" = "Log offentlig gæld 2021 (% af BNP)"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "Obs.", "fmt" = 0),
    list("raw" = "rmse", "clean" = "RMSE", "fmt" = 3),
    list("raw" = "adj.r.squared", "clean" = "Justeret R²", "fmt" = 3)
  ),
  notes = "Standardfejl er heteroskedasticitets-robuste (HC3). Konstantled er inkluderet i modellerne, men udeladt fra tabellen."
) %>%
  theme_gt_bachelor_project() %>%
  gtsave(filename = file.path(DIR_TAB, "ols_post_usd.html"))

message("\n--- Script 11_ols_main_analysis.R finished ---")
message("All regression tables saved to: ", DIR_TAB)
