library(conflicted)
library(tidyverse)
library(here)
library(glue)
library(gt)
library(gtsummary)
library(ggcorrplot)
library(psych)
library(patchwork)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("alpha", "ggplot2")

DIR_DATA    <- here("data", "_processed")
DIR_SCRIPTS <- here("scripts")
DIR_TAB     <- here("_output", "_tables", "_ols_eda")
DIR_FIG     <- here("_output", "_figures", "_ols_eda")

if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

OLS_DATA <- file.path(DIR_DATA, "ols_data.rds")

source(file.path(DIR_SCRIPTS, "00_functions.R"))


# --- OLS VARIABLES SETUP ---
# Define the variables we want to plot
VARS_FOR_OLS <- c(
  # Model 1 Variables (Pre-Period)
  "milex_gdp_pre"       = "Ændring i forsvarsudgifter (% BNP) 2014-21",
  "milex_usd_pre"       = "Ændring i log forsvarsudgifter (US$) 2014-21",
  "dist_enemy_log"      = "Log afstand til fjende (km)",
  "nato_gap_2014"       = "Afstand til 2% mål (2014)",
  "gdp_2014_log"        = "Log BNP (2014)",

  # Model 2 & 3 Variables (Post-Period)
  "milex_gdp_post"      = "Ændring i forsvarsudgifter (% BNP) 2021-25",
  "milex_usd_post"      = "Ændring i log forsvarsudgifter (US$) 2021-24",
  "nato_gap_2021"       = "Afstand til 2% mål i 2021 (procentpoint)",
  "gdp_2021_log"        = "Log BNP (milliarder)",
  "debt_gdp_2021_log"   = "Log Offentlig gæld (% BNP)(2021)",
  "gdp_cap_2021_log"    = "Log BNP per indbygger (2021)",
  "gdp_growth_post"      = "BNP vækst (2021-2025)",
  "us_troops_2021_log"  = "Log US Troops (2021)"
)

# Variables to SKIP (Dummies / binaries where histograms don't make sense)
VARS_TO_SKIP <- c("border_rus", "post_com")

# Manual Breaks (Customize as needed)
MANUAL_BREAKS_OLS <- list(
  # Milex: Start at 0 to capture low values
  "milex_gdp_pre"      = seq(0, 1.4, by = 0.2),
  "milex_gdp_post"     = seq(0, 2.5, by = 0.5), # Covers up to 2.29
  "milex_usd_pre"      = seq(0, 1.2, by = 0.2),
  "milex_usd_post"     = seq(0, 0.8, by = 0.1),
  # Distance: 5 to 8+
  "dist_enemy_log"     = seq(5, 8.5, by = 0.5),
  # NATO Gap: 0 to 1.6
  "nato_gap_2014"      = seq(0, 1.8, by = 0.2),
  "nato_gap_2021"      = seq(0, 1.8, by = 0.2),
  # Log GDP: 2.5 to 8.4
  "gdp_2014_log"       = seq(2, 9, by = 1),
  "gdp_2021_log"       = seq(2, 9, by = 1),
  # Log GDP Cap: 9.5 to 11.8
  "gdp_cap_2014_log"   = seq(9.5, 12, by = 0.5),
  "gdp_cap_2021_log"   = seq(9.5, 12, by = 0.5),
  # Growth: -0.04 to 0.17
  "gdp_growth_post"    = seq(-0.05, 0.20, by = 0.05),
  # Debt: 2.5 to 5.0
  "debt_gdp_2014_log"  = seq(2, 5.5, by = 0.5),
  "debt_gdp_2021_log"  = seq(2, 5.5, by = 0.5),
  # Troops: 1.8 to 10.6
  "us_troops_2014_log" = seq(1, 11, by = 1),
  "us_troops_2021_log" = seq(1, 11, by = 1)
)

# Manual Binwidths
MANUAL_BINWIDTHS_OLS <- list(
  # Military Spending (Pre/Post)
  "milex_gdp_pre"      = 0.20,  # FD was 0.201
  "milex_gdp_post"     = 0.25,  # FD was 0.7 (Too chunky). 0.25 gives better detail.
  "milex_usd_pre"      = 0.20,  # FD was 0.198
  "milex_usd_post"     = 0.10,  # FD was 0.25, but range is small (0.74). 0.1 is safer.
  # Controls (Main)
  "dist_enemy_log"     = 0.50,  # FD was 0.365. 0.5 is cleaner.
  "nato_gap_2014"      = 0.20,  # FD was 0.37. 0.2 gives more definition.
  "nato_gap_2021"      = 0.20,  # FD was 0.45.
  "gdp_2014_log"       = 1.00,  # FD was 1.8. 1.0 is standard for log GDP.
  "gdp_2021_log"       = 1.00,
  # Robustness Variables
  "gdp_cap_2014_log"   = 0.25,  # FD was 0.31.
  "gdp_cap_2021_log"   = 0.25,
  "gdp_growth_post"    = 0.025,  # FD was 0.018.
  "debt_gdp_2014_log"  = 0.50,  # FD was 0.57.
  "debt_gdp_2021_log"  = 0.50,
  "us_troops_2014_log" = 1.00,  # FD was 2.8. 1.0 gives better resolution.
  "us_troops_2021_log" = 1.00
)

# Manual Captions
MANUAL_CAPTIONS_OLS <- list()

MANUAL_NUDGES_OLS <- list(
  "gdp_growth_post" = c("LUX" = 0.05, "DEU" = -0.05, "ALB" = 0.1)
)

ols_data <- readRDS(OLS_DATA) %>%
  select(iso3c, all_of(names(VARS_FOR_OLS)))

# 5. GENERATE OLS DISTRIBUTION PLOTS =========================================
message("--- Section 5: Generating OLS Distribution Plots ---")

for (var in names(VARS_FOR_OLS)) {

  # Skip dummy variables
  if (var %in% VARS_TO_SKIP) {
    message(glue::glue(">> Skipping plot for binary variable: {var}"))
    next
  }

  # Lookup manual settings (defaults to NULL if not found)
  curr_breaks <- if (var %in% names(MANUAL_BREAKS_OLS)) MANUAL_BREAKS_OLS[[var]] else NULL
  curr_bw     <- if (var %in% names(MANUAL_BINWIDTHS_OLS)) MANUAL_BINWIDTHS_OLS[[var]] else NULL
  curr_cap    <- if (var %in% names(MANUAL_CAPTIONS_OLS)) MANUAL_CAPTIONS_OLS[[var]] else NULL
  curr_nudges <- if (var %in% names(MANUAL_NUDGES_OLS)) MANUAL_NUDGES_OLS[[var]] else NULL

  create_ols_distribution_plot(
    data = ols_data, # Or whatever your OLS dataframe is called
    var_name = !!sym(var),
    var_label = VARS_FOR_OLS[var],
    manual_breaks = curr_breaks,
    manual_binwidth = curr_bw,
    manual_nudges = curr_nudges,
    manual_caption = curr_cap,
    output_dir = DIR_FIG
  )
}
