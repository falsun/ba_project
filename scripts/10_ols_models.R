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

# Load packages
library(conflicted)
library(tidyverse)
library(here)
library(modelsummary)
library(gt)
library(glue)
library(broom)
library(scales)

# Conflict Resolution
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Directories
DIR_DATA    <- here::here("data", "_processed", "ols_data.rds")
DIR_SCRIPTS <- here::here("scripts")
DIR_TAB     <- here::here("_output", "_tables", "_ols_models")
DIR_FIG     <- here::here("_output", "_figures", "_ols_plots")

if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

# Load Data Check
if (!file.exists(DIR_DATA)) stop("Data not found. Run 09_ols_data_trans.R first.")

# Source Theme Function (if exists)
if (file.exists(here::here("scripts", "00_functions.R"))) {
  source(here::here("scripts", "00_functions.R"))
} else {
  # Fallback if function script missing
  theme_gt_bachelor_project <- function(data) { data }
}

options(OutDec = ",")

# --- TABLE CONFIGURATION ---
# Define these once to ensure identical formatting across all 4 tables
TABLE_CONFIG <- list(
  stars = c('*' = .05, '**' = .01, '***' = .001),
  vcov = "HC3",
  coef_map = c(
    "border_rus"        = "Delt grænse med Rusland (dummy)",
    "dist_enemy_log"    = "Log afstand til fjende (km)",
    "nato_gap_2014"     = "Afstand til 2% mål 2014 (procentpoint)",
    "nato_gap_2021"     = "Afstand til 2% mål 2021 (procentpoint)",
    "gdp_2014_log"      = "Log BNP 2014 (milliarder)",
    "gdp_2021_log"      = "Log BNP 2021 (milliarder)"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "Obs.", "fmt" = 0),
    list("raw" = "rmse", "clean" = "RMSE", "fmt" = 3),
    list("raw" = "adj.r.squared", "clean" = "Justeret R²", "fmt" = 3)
  ),
  notes = "Standardfejl er heteroskedasticitets-robuste (HC3). Konstantled er inkluderet i modellerne, men udeladt fra tabellen."
)


# 1. LOAD DATA ===============================================================
message("--- Section 1: Loading Data ---")

ols_data <- readRDS(DIR_DATA)


# 2. HELPER FUNCTIONS ========================================================

#' Fit the 6 standard OLS models for a given period
#' @param dv String: Name of Dependent Variable (e.g., "milex_gdp_pre")
#' @param gap_var String: Name of the NATO Gap variable
#' @param gdp_var String: Name of the Debt variable
fit_ols_models <- function(data, dv, gap_var, gdp_var) {

  # Dynamic Formulas
  f1 <- as.formula(glue("{dv} ~ border_rus"))
  f2 <- as.formula(glue("{dv} ~ dist_enemy_log"))
  f3 <- as.formula(glue("{dv} ~ {gap_var}"))
  f4 <- as.formula(glue("{dv} ~ {gdp_var}"))
  f5 <- as.formula(glue("{dv} ~ border_rus + {gap_var}"))
  f6 <- as.formula(glue("{dv} ~ dist_enemy_log + {gap_var}"))
  f7 <- as.formula(glue("{dv} ~ border_rus + dist_enemy_log + {gap_var} + {gdp_var}"))

  list(
    "(1) Trussel (binær)"            = lm(f1, data = data),
    "(2) Trussel (gradient)"         = lm(f2, data = data),
    "(3) NATO pres"                  = lm(f3, data = data),
    "(4) Økonomi"                    = lm(f4, data = data),
    "(5) Trussel (binær) og NATO"    = lm(f5, data = data),
    "(6) Trussel (gradient) og NATO" = lm(f6, data = data),
    "(7) NATO, trussel og økonomi"   = lm(f7, data = data)
  )
}

#' Generate and Save GT Table with Auto-Naming
#' @param models List of lm objects
#' @param title String: Title of the table
#' @param dv String: The dependent variable name (used for filename generation)
save_ols_table <- function(models, title, dv) {

  # Generate Table
  tab <- modelsummary(
    models,
    title = title,
    output = "gt",
    stars = TABLE_CONFIG$stars,
    vcov = TABLE_CONFIG$vcov,
    coef_map = TABLE_CONFIG$coef_map,
    gof_map = TABLE_CONFIG$gof_map,
    notes = TABLE_CONFIG$notes
  ) %>%
    theme_gt_bachelor_project()

  # Auto-generate filename based on DV
  # Example: milex_gdp_pre -> ols_milex_gdp_pre.html
  file_name <- glue("ols_{dv}.html")

  # Save
  gtsave(tab, filename = file.path(DIR_TAB, file_name))
  message(glue("Saved table: {file_name}"))
}


# 3. RUN ANALYSIS ============================================================
message("--- Section 2: Running Models & Saving Tables ---")

# --- A. 2014-2021 GDP ---
# DV: milex_gdp_pre -> saves as: ols_milex_gdp_pre.html
models_pre_gdp <- fit_ols_models(ols_data, "milex_gdp_pre", "nato_gap_2014", "gdp_2014_log")
save_ols_table(models_pre_gdp,
               "Tværsnits OLS 2014-21 - Forsvarsudgifter (% af BNP)",
               "milex_gdp_pre")

# --- B. 2021-2025 GDP ---
# DV: milex_gdp_post -> saves as: ols_milex_gdp_post.html
models_post_gdp <- fit_ols_models(ols_data, "milex_gdp_post", "nato_gap_2021", "gdp_2021_log")
save_ols_table(models_post_gdp,
               "Tværsnits OLS 2021-25 - Forsvarsudgifter (% af BNP)",
               "milex_gdp_post")

# --- C. 2014-2021 USD ---
# DV: milex_usd_pre -> saves as: ols_milex_usd_pre.html
models_pre_usd <- fit_ols_models(ols_data, "milex_usd_pre", "nato_gap_2014", "gdp_2014_log")
save_ols_table(models_pre_usd,
               "Tværsnits OLS 2014-21 - Forsvarsudgifter (2023 US$)",
               "milex_usd_pre")

# --- D. 2021-2024 USD ---
# DV: milex_usd_post -> saves as: ols_milex_usd_post.html
models_post_usd <- fit_ols_models(ols_data, "milex_usd_post", "nato_gap_2021", "gdp_2021_log")
save_ols_table(models_post_usd,
               "Tværsnits OLS 2021-24 - Forsvarsudgifter (2023 US$)",
               "milex_usd_post")

message("\n--- Script 11_ols_main_analysis.R finished ---")


# 4. VIZ

# PRE-PLOT GDP
milex_gdp_pre_plot <- ggplot(ols_data, aes(x = dist_enemy, y = milex_gdp_pre)) +
  # Loess for reference (Blue)
  #geom_smooth(method = "loess", color = "#f48c06", se = FALSE, linetype = "dashed", linewidth = 0.8) +

  # Linear Fit (Red) - adjusted to y ~ log(x) to match the regression model
  geom_smooth(method = "lm", formula = y ~ log(x), color = "#0077b6", fill = "#0077b6", alpha = 0.1, se = TRUE, fullrange = TRUE) +

  geom_text(
    aes(label = iso3c, fontface = ifelse(border_rus == 1, "bold", "plain")),
    family = "IBM Plex Serif",
    size = 3.5
  ) +
  scale_x_dk() +
  scale_y_dk(accuracy = 0.01) +
  labs(
    x = "Afstand til Rusland (km)",
    y = "Ændring i forsvarsudgifter (% BNP) 2014-2021",
    caption = "Stater markeret med fed deler grænse med Rusland."
  ) +
  theme_bachelor_project()

ggsave(file.path(DIR_FIG, glue("ols_plot_milex_gdp_pre.png")), milex_gdp_pre_plot, width = 8, height = 7, bg = "white")


# POST-PLOT GDP
milex_gdp_post_plot <- ggplot(ols_data, aes(x = dist_enemy, y = milex_gdp_post)) +
  # Loess for reference (Blue)
  #geom_smooth(method = "loess", color = "#f48c06", se = FALSE, linetype = "dashed", linewidth = 0.8) +

  # Linear Fit (Red) - adjusted to y ~ log(x) to match the regression model
  geom_smooth(method = "lm", formula = y ~ log(x), color = "#0077b6", fill = "#0077b6", alpha = 0.1, se = TRUE, fullrange = TRUE) +

  geom_text(
    aes(label = iso3c, fontface = ifelse(border_rus == 1, "bold", "plain")),
    family = "IBM Plex Serif",
    size = 3.5
  ) +
  scale_x_dk() +
  scale_y_dk(accuracy = 0.01) +
  labs(
    x = "Afstand til Rusland (km)",
    y = "Ændring i forsvarsudgifter (% BNP) 2021-2025",
    caption = "Stater markeret med fed deler grænse med Rusland."
  ) +
  theme_bachelor_project()

ggsave(file.path(DIR_FIG, glue("ols_plot_milex_gdp_post.png")), milex_gdp_post_plot, width = 8, height = 7, bg = "white")


# PRE-PLOT USD
milex_usd_pre_plot <- ggplot(ols_data, aes(x = dist_enemy, y = milex_usd_pre)) +
  # Loess for reference (Blue)
  #geom_smooth(method = "loess", color = "#f48c06", se = FALSE, linetype = "dashed", linewidth = 0.8) +

  # Linear Fit (Red) - adjusted to y ~ log(x) to match the regression model
  geom_smooth(method = "lm", formula = y ~ log(x), color = "#0077b6", fill = "#0077b6", alpha = 0.1, se = TRUE, fullrange = TRUE) +

  geom_text(
    aes(label = iso3c, fontface = ifelse(border_rus == 1, "bold", "plain")),
    family = "IBM Plex Serif",
    size = 3.5
  ) +
  scale_x_dk() +
  scale_y_dk(accuracy = 0.01) +
  labs(
    x = "Afstand til Rusland (km)",
    y = "Ændring i forsvarsudgifter (% BNP) 2014-2021",
    caption = "Stater markeret med fed skrift deler grænse med Rusland."
  ) +
  theme_bachelor_project()

ggsave(file.path(DIR_FIG, glue("ols_plot_milex_usd_pre.png")), milex_usd_pre_plot, width = 8, height = 7, bg = "white")


# POST-PLOT USD
milex_usd_post_plot <- ggplot(ols_data, aes(x = dist_enemy, y = milex_usd_post)) +
  # Loess for reference (Blue)
  #geom_smooth(method = "loess", color = "#f48c06", se = FALSE, linetype = "dashed", linewidth = 0.8) +

  # Linear Fit (Red) - adjusted to y ~ log(x) to match the regression model
  geom_smooth(method = "lm", formula = y ~ log(x), color = "#0077b6", fill = "#0077b6", alpha = 0.1, se = TRUE, fullrange = TRUE) +
  geom_text(
    aes(label = iso3c, fontface = ifelse(border_rus == 1, "bold", "plain")),
    family = "IBM Plex Serif",
    size = 3.5
  ) +
  scale_x_dk() +
  scale_y_dk(accuracy = 0.01) +
  labs(
    x = "Afstand til Rusland (km)",
    y = "Ændring i forsvarsudgifter (% BNP) 2021-2025",
    caption = "Stater markeret med fed skrift deler grænse med Rusland."
  ) +
  theme_bachelor_project()

ggsave(file.path(DIR_FIG, glue("ols_plot_milex_usd_post.png")), milex_usd_post_plot, width = 8, height = 7, bg = "white")
