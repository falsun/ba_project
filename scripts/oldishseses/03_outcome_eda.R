# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       02_eda.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-11-09
#   Description:  This script generates descriptive statistics tables and EDA
#                 visualizations.
#
# ---------------------------------------------------------------------------- #


# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

DIR_DATA <- here::here("data", "_processed")
DIR_SCRIPTS <- here::here("scripts")
DIR_TAB <- here::here("_output", "_tables", "_eda")
DIR_FIG <- here::here("_output", "_figures", "_eda")

if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

MASTER_PANEL <- file.path(DIR_DATA, "master_panel.rds")

TREATMENT_YEAR <- 2022

VARS_FOR_EDA <- c(
  "milex_usd_log" = "Log(Mil. Exp. Const. 2023 US$)",
  "milex_cap"     = "Mil. Exp. per Capita",
  "milex_cap_log" = "Log(Mil. Exp. per Capita)",
  "milex_gdp"     = "Mil. Exp. % of GDP",
  "milex_gdp_log" = "Log(Mil. Exp. % of GDP)",
  "milex_gov"     = "Mil. Exp. % of Govt. Spending",
  "milex_gov_log" = "Log(Mil. Exp. % of Govt. Spending)",
  "pop"           = "Population",
  "pop_log"       = "Log(Population)",
  "gdp_cap"       = "GDP per Capita (PPP, constant 2017 intl. $)",
  "gdp_cap_log"   = "Log(GDP per Capita)",
  "trade_gdp"     = "Trade % of GDP",
  "lib_dem"       = "Liberal Democracy Score (0-1 Index)"
)

MANUAL_BREAKS <- list()

MANUAL_BINWIDTHS <- list()


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, gtsummary, gt, psych, ggcorrplot, conflicted, stringr)
conflict_prefer("filter", "dplyr")
conflict_prefer("alpha", "ggplot2")

source(file.path(DIR_SCRIPTS, "00_functions.R"))

# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

master_panel <- readRDS(MASTER_PANEL)

# 1. Create the full time-series df for plots
ts_df <- master_panel %>%
  filter(group %in% c("control", "treatment")) %>%
  mutate(group = factor(group, levels = c("treatment", "control")))

# 2. Create the pre-treatment df for tables and dist plots
pre_df <- ts_df %>%
  filter(post_treat == 0) %>%
  select(group, iso3c, year, all_of(names(VARS_FOR_EDA)))


# 3. CHECK CORRELATION =======================================================
message("--- Section 3: Checking Correlation ---")

create_corr_plot(
  data       = pre_df,
  vars       = names(VARS_FOR_EDA),
  title      = "Pre-Treatment Outcome Correlation Matrix (2014-2021)",
  output_dir = DIR_FIG,
  file_name  = "outcome_correlation_matrix.png"
)

# 4. TABLE 1: PRE-TREATMENT BALANCE ==========================================
message("--- Section 4: Generating Table 1 (Balance) ---")

country_avg_df <- pre_df %>%
  group_by(group, iso3c) %>%
  summarise(across(all_of(names(VARS_FOR_EDA)), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

table_1_bal <- country_avg_df %>%
  select(-iso3c) %>%
  tbl_summary(
    by = group,
    label = as.list(VARS_FOR_EDA),
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no",
    digits = all_continuous() ~ 2
  ) %>%
  add_difference(test = all_continuous() ~ "smd") %>%
  modify_column_merge(
    pattern = "{conf.low}–{conf.high}"
  ) %>%
  modify_footnote(
    estimate ~ "Standardized Mean Difference (Hedges' g)",
    conf.low ~ NA
  ) %>%
  modify_header(
    estimate ~ "**SMD**",
    conf.low ~ "**95% CI**"
  ) %>%
  modify_spanning_header(everything() ~ NA) %>%
  modify_caption("**Table 1: Pre-Treatment Outcome Balance (2014-2021)**")

print(table_1_bal)
gtsave(as_gt(table_1_bal), file = file.path(DIR_TAB, "table_1_outcome_balance.html"))


# 5. TABLE 2: DETAILED DESCRIPTIVES ==========================================
message("--- Section 5: Generating Table 2 (Detailed Descriptives) ---")

kurtosis <- function(x, na.rm = TRUE) {
  psych::kurtosi(x, na.rm = na.rm)
}

table_2_det_bal <- pre_df %>%
  select(group, all_of(names(VARS_FOR_EDA))) %>%
  tbl_summary(
    by = group,
    label = as.list(VARS_FOR_EDA),
    missing = "no",
    statistic = all_continuous() ~ "{min}–{max} ({skew}, {kurtosis})",
    digits = all_continuous() ~ 2
  ) %>%
  modify_footnote(
    all_stat_cols() ~ "min-max (skew, excess kurtosis)"
  ) %>%
  modify_caption("**Table 2: Pre-Treatment Outcome Descriptive Statistics (2014-2021)**")

print(table_2_det_bal)
gtsave(as_gt(table_2_det_bal), file = file.path(DIR_TAB, "table_2_outcome_det_balance.html"))


# 7. GENERATE OUTLIER REPORTS ================================================
message("--- Section 8: Generating Outlier Reports ---")

for (var in names(VARS_FOR_EDA)) {
  create_outlier_table(
    data = pre_df,
    var_name = !!sym(var),
    var_label = VARS_FOR_EDA[var],
    output_dir = DIR_TAB,
    title = glue::glue("Outlier Report for {VARS_FOR_EDA[var]} (2014-2021)")
  )
}


# 6. GENERATE DISTRIBUTION PLOTS =============================================
message("--- Section 6: Generating Distribution Plots ---")

for (var in names(VARS_FOR_EDA)) {
  create_distribution_plot(
    data = pre_df,
    var_name = !!sym(var),
    var_label = VARS_FOR_EDA[var],
    manual_breaks = MANUAL_BREAKS[[var]],
    manual_binwidth = MANUAL_BINWIDTHS[[var]],
    output_dir = DIR_FIG,
    title = glue::glue("Distribution of {VARS_FOR_EDA[var]} (2014-2021)")
  )
}


# 8. GENERATE AGGREGATED TIME-SERIES PLOTS ===================================
message("--- Section 7: Generating Aggregated Time-Series Plots ---")

for (var in names(VARS_FOR_EDA)) {
  create_aggregated_ts_plot(
    data = ts_df,
    var_name = !!sym(var),
    var_label = VARS_FOR_EDA[var],
    treatment_year = TREATMENT_YEAR,
    output_dir = DIR_FIG,
    title = glue::glue("Aggregated Trends for {VARS_FOR_EDA[var]}")
  )
}

message(paste(
  "\n--- Script 02_eda.R finished ---",
  "\nAll output (tables and figures) saved to:", here::here("_output")
))
