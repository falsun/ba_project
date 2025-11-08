# ---------------------------------------------------------------------------- #
#
#   Project:     NATO Defence Spending Bachelor's Thesis
#   Script:      04_viz.R
#   Author:      Frederik Bender Bøeck-Nielsen
#   Date:        2025-10-17
#   Description: Generates key time-series visualizations for the analysis,
#                such as parallel trends plots.
#
# ---------------------------------------------------------------------------- #


# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

TREATMENT_YEAR <- 2022

DIR_DATA         <- here::here("data", "_processed")
DIR_SCRIPTS      <- here::here("scripts")
DIR_FIG          <- here::here("_output", "_figures")

if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

MASTER_PANEL_LOG <- file.path(DIR_DATA, "master_panel_log.rds")

VARS_FOR_PLOTS <- c(
  "milex_gdp"     = "Military Expenditure (% of GDP)",
  "log_pop"       = "Log(Population)",
  "log_gdp_cap"   = "Log(GDP per Capita)",
  "log_trade_gdp" = "Log(Trade Openness)",
  "lib_dem"       = "Liberal Democracy Index (0-1)"
)


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, conflicted)
conflict_prefer("filter", "dplyr")

# Load all custom functions from the central functions script.
source(file.path(DIR_SCRIPTS, "00_functions.R"))


# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

master_panel <- readRDS(MASTER_PANEL_LOG)

# Prepare data for plotting, including the log transformations.
# Importantly, we are now including data up to 2024.
plot_df <- master_panel %>%
  filter(group %in% c("control", "treatment")) %>%
  mutate(group = factor(group, levels = c("treatment", "control"))) %>%
  select(group, iso3c, year, all_of(names(VARS_FOR_PLOTS)))


# 3. RIDGELINE PLOT ==========================================================
message("--- Section 4: Generating Ridgeline Plot ---")

create_ridgeline_plot(
  data = plot_df,
  var_name = milex_gdp,
  var_label = "Military Expenditure (% of GDP)",
  group_filter = "treatment",
  output_dir = DIR_FIG
)

# 5. DUMBBELL PLOT ===========================================================
message("--- Section 5: Generating Dumbbell Plot ---")
create_dumbbell_plot(
  data = plot_df,
  var_name = milex_gdp,
  var_label = "Military Expenditure (% of GDP)",
  start_year = 2021,
  end_year = 2024,
  output_dir = DIR_FIG
)


# 4. SCRIPT COMPLETION =======================================================
message("\n--- Script 04_viz.R finished successfully ---")
message(paste("All figures saved to:", DIR_FIG))
