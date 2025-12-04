# ---------------------------------------------------------------------------- #
#
#   Script:       09_ols_data_trans.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Description:  Transforms the Master Panel (Long) into the Cross-Sectional
#                 Analytical Dataset (Wide) for the OLS Heterogeneity Analysis.
#                 Calculates Long Differences and Initial Conditions.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION ===========================================================
message("--- Section 0: Loading Configuration ---")

library(conflicted)
library(tidyverse)
library(here)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

DIR_DATA_PROC <- here("data", "_processed")
MASTER_PANEL  <- file.path(DIR_DATA_PROC, "master_panel.rds")
OUTPUT_FILE   <- file.path(DIR_DATA_PROC, "ols_data.rds")


# 1. LOAD & FILTER ===========================================================
message("--- Section 1: Loading and Filtering Data ---")

master_panel <- readRDS(MASTER_PANEL)

# We only need the Treatment Group (European NATO) and specific years
df_filtered <- master_panel %>%
  filter(group == "Behandlet") %>%
  filter(year %in% c(2014, 2021, 2024, 2025)) %>%
  # Select only the variables needed for OLS
  select(
    iso3c, country_dan, year, post_com,
    # Outcomes
    milex_gdp_nato, milex_usd_log,
    # Explanatory (Time-Invariant)
    border_rus, dist_enemy,
    # Controls (Time-Variant)
    gdp, gdp_local, gdp_cap, debt_gdp, us_troops
  )

# 2. PIVOT TO WIDE FORMAT ====================================================
message("--- Section 2: Pivoting to Cross-Sectional Format ---")

# Pivot time-variant variables to wide (e.g., milex_cap_2014, gdp_cap_2021)
df_wide <- df_filtered %>%
  pivot_wider(
    id_cols = c(iso3c, country_dan, post_com, border_rus, dist_enemy), # ID variables (Time-Invariant)
    names_from = year,
    values_from = c(milex_gdp_nato, milex_usd_log, gdp, gdp_local, gdp_cap,
                    debt_gdp, us_troops)
  )

# 3. FEATURE ENGINEERING =====================================================
message("--- Section 3: Calculating Changes and Controls ---")

ols_data <- df_wide %>%
  mutate(
    # DEPENDENT VARIABLES (TOTAL CHANGE FOR EACH PERIOD)
    milex_gdp_pre      = milex_gdp_nato_2021 - milex_gdp_nato_2014,
    milex_gdp_post     = milex_gdp_nato_2025 - milex_gdp_nato_2021,
    milex_usd_pre      = milex_usd_log_2021 - milex_usd_log_2014,
    milex_usd_post     = milex_usd_log_2024 - milex_usd_log_2021,
    dist_enemy_inv_log     = 1 / log(dist_enemy),
    dist_enemy_log     = log(dist_enemy),
    dist_enemy_inv_sqrt = 1 / sqrt(dist_enemy),
    # Institutional Pressure (The 2% Gap)
    # Formula: Max(0, 2% - Spending). Only those below 2% feel pressure.
    nato_gap_2014      = pmax(0, 2.0 - milex_gdp_nato_2014),
    nato_gap_2021      = pmax(0, 2.0 - milex_gdp_nato_2021),
    gdp_2021_log       = log(gdp_2021),
    gdp_2014_log       = log(gdp_2014),
    gdp_growth_post     = log(gdp_local_2025) - log(gdp_local_2021),
    gdp_cap_2021_log   = log(gdp_cap_2021),
    gdp_cap_2014_log   = log(gdp_cap_2014),
    debt_gdp_2014_log  = log(debt_gdp_2014),
    debt_gdp_2021_log  = log(debt_gdp_2021),
    us_troops_2014_log = log(us_troops_2014),
    us_troops_2021_log = log(us_troops_2021)
    ) %>%
  select(
    iso3c, country_dan,
    # OUTCOMES
    milex_gdp_pre, milex_gdp_post, milex_usd_pre, milex_usd_post,
    # REALISM
    border_rus, dist_enemy, dist_enemy_log, dist_enemy_inv_log, dist_enemy_inv_sqrt,
    # LIBERALISM
    nato_gap_2014, nato_gap_2021, milex_gdp_nato_2014, milex_gdp_nato_2021,
    # FREE-RIDING
    gdp_2014_log, gdp_2021_log, us_troops_2014_log, us_troops_2021_log,
    # ECONOMICS
    gdp_growth_post, gdp_cap_2014_log, gdp_cap_2021_log, debt_gdp_2014_log,
    debt_gdp_2021_log,
    # HISTORY
    post_com
  )

# 4. SAVE ====================================================================
message("--- Section 4: Saving Analytical Dataset ---")

saveRDS(ols_data, file = OUTPUT_FILE)

# Preview
glimpse(ols_data)

message(paste0(
  "\nSuccess! Cross-sectional dataset created with N=", nrow(ols_data), " countries.",
  "\nSaved to: ", OUTPUT_FILE
))
