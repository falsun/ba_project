# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       02_eda.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-10-21
#   Description:  This script generates descriptive statistics tables and EDA
#                 visualizations for the pre-treatment period.
#
# ---------------------------------------------------------------------------- #


# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

DIR_DATA <- here::here("data", "_processed")
DIR_SCRIPTS <- here::here("scripts")
DIR_TAB <- here::here("_output", "_tables")
DIR_FIG <- here::here("_output", "_figures")

if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

CLEAN_PANEL <- file.path(DIR_DATA, "clean_panel.rds")
LOG_PANEL <- file.path(DIR_DATA, "log_panel.rds")

VARS_FOR_EDA <- c(
  "milex_gdp" = "Military Spending (% of GDP)",
  "milex_usd" = "Military Spending (Constant 2023 US$)(Millions)",
  "pop"       = "Population (Millions)",
  "gdp_cap"   = "GDP per Capita ($1,000s)",
  "trade_gdp" = "Trade Openness (% of GDP)",
  "lib_dem"   = "Liberal Democracy Index (0-1)"
)

MANUAL_BREAKS <- list(
  milex_gdp = seq(0, 4, by = 1),
  milex_usd = seq(0, 70000, by = 17500),
  pop       = seq(0, 140, by = 35),
  gdp_cap   = seq(0, 140, by = 35),
  trade_gdp = seq(0, 400, by = 100),
  lib_dem   = seq(0, 1, by = 0.25)
)

MANUAL_BINWIDTHS <- list()


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, gtsummary, gt, psych, ggcorrplot, conflicted, stringr)
conflict_prefer("filter", "dplyr")
conflict_prefer("alpha", "ggplot2")

source(file.path(DIR_SCRIPTS, "00_functions.R"))

options(scipen = 999)

# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

clean_panel <- readRDS(CLEAN_PANEL)

pre_treat_df <- clean_panel %>%
  filter(post_treat == 0) %>%
  filter(group %in% c("control", "treatment")) %>%
  mutate(
    group   = factor(group, levels = c("treatment", "control")),
    gdp_cap = gdp_cap / 1000,
    pop     = pop / 1000000
  ) %>%
  select(group, iso3c, year, all_of(names(VARS_FOR_EDA)))


# 3. CHECK CORRELATION =======================================================
message("--- Section 3: Checking Correlation ---")

create_corr_plot(
  data       = pre_treat_df,
  vars       = names(VARS_FOR_EDA),
  title      = "Pre-Treatment Correlation Matrix (2014-2021)",
  output_dir = DIR_FIG,
  file_name  = "correlation_matrix.png"
)

# 4. TABLE 1: PRE-TREATMENT BALANCE ==========================================
message("--- Section 4: Generating Table 1 (Balance) ---")

country_avg_df <- pre_treat_df %>%
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
  modify_caption("**Table 1: Pre-Treatment Balance (2014-2021)**")

print(table_1_bal)
gtsave(as_gt(table_1_bal), file = file.path(DIR_TAB, "table_1_balance.png"), vwidth = 1000)
saveRDS(table_1_bal, file = file.path(DIR_TAB, "table_1_balance.rds"))


# 5. TABLE 2: DETAILED DESCRIPTIVES ==========================================
message("--- Section 5: Generating Table 2 (Detailed Descriptives) ---")

kurtosis <- function(x, na.rm = TRUE) {
  psych::kurtosi(x, na.rm = na.rm)
}

table_2_det_bal <- pre_treat_df %>%
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
  modify_caption("**Table 2: Pre-Treatment Descriptive Statistics (2014-2021)**")

print(table_2_det_bal)
gtsave(as_gt(table_2_det_bal), file = file.path(DIR_TAB, "table_2_det_balance.png"), vwidth = 1000)
saveRDS(table_2_det_bal, file = file.path(DIR_TAB, "table_2_det_balance.rds"))


# 6. GENERATE DISTRIBUTION PLOTS =============================================
message("--- Section 6: Generating Distribution Plots ---")

for (var in names(VARS_FOR_EDA)) {
  create_distribution_plot(
    data = pre_treat_df,
    var_name = !!sym(var),
    var_label = VARS_FOR_EDA[var],
    manual_breaks = MANUAL_BREAKS[[var]],
    manual_binwidth = MANUAL_BINWIDTHS[[var]],
    output_dir = DIR_FIG,
    title = glue::glue("Distribution of {VARS_FOR_EDA[var]} (2014-2021)")
  )
}


# 7. TRANSFORM & SAVE DATA ===================================================
message("--- Section 7: Logging Covariates and Saving Data ---")

log_panel <- clean_panel %>%
  mutate(
    log_milex_gdp = log(milex_gdp),
    log_milex_usd = log(milex_usd),
    log_pop       = log(pop),
    log_gdp_cap   = log(gdp_cap),
    log_trade_gdp = log(trade_gdp)
  ) %>%
  relocate(log_milex_gdp, .after = milex_gdp) %>%
  relocate(log_milex_usd, .after = milex_usd) %>%
  relocate(log_pop, .after = pop) %>%
  relocate(log_gdp_cap, .after = gdp_cap) %>%
  relocate(log_trade_gdp, .after = trade_gdp)

saveRDS(log_panel, file = LOG_PANEL)

message(paste(
  "\n--- Script 02_eda.R finished ---",
  "\nAll output (tables and figures) saved to:", here::here("_output"),
  "\nLogged master panel data frame saved to: ", LOG_PANEL
))
