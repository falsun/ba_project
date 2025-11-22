# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       02_eda.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-11-07
#   Description:  This script generates descriptive statistics tables and EDA
#                 visualizations for the pre-treatment period (logged variables).
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

LOG_PANEL <- file.path(DIR_DATA, "log_panel.rds")
FD_PANEL <- file.path(DIR_DATA, "fd_panel.rds")

VARS_FOR_EDA <- c(
  "log_milex_gdp" = "Military Spending (% of GDP)(Log)",
  "log_milex_usd" = "Military Spending (US$)(Log)",
  "log_pop" = "Population (Log)",
  "log_gdp_cap" = "GDP per Capita (Log)",
  "log_trade_gdp" = "Trade Openness (Log)",
  "lib_dem" = "Liberal Democracy Index (0-1)"
)

MANUAL_BREAKS <- list()

MANUAL_BINWIDTHS <- list()


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, gtsummary, gt, psych, ggcorrplot, conflicted, stringr)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("alpha", "ggplot2")

source(file.path(DIR_SCRIPTS, "00_functions.R"))

options(scipen = 999)

# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

log_panel <- readRDS(LOG_PANEL)

pre_treat_df <- log_panel %>%
  filter(post_treat == 0) %>%
  filter(group %in% c("control", "treatment")) %>%
  mutate(
    group = factor(group, levels = c("treatment", "control"))
  ) %>%
  select(group, iso3c, year, all_of(names(VARS_FOR_EDA)))


# 3. CHECK CORRELATION =======================================================
message("--- Section 3: Checking Correlation ---")

create_corr_plot(
  data       = pre_treat_df,
  vars       = names(VARS_FOR_EDA),
  title      = "Pre-Treatment Correlation Matrix (2014-2021)",
  output_dir = DIR_FIG,
  file_name  = "log_correlation_matrix.png"
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
gtsave(as_gt(table_1_bal), file = file.path(DIR_TAB, "table_1_log_balance.png"), vwidth = 1000)
saveRDS(table_1_bal, file = file.path(DIR_TAB, "table_1_log_balance.rds"))


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
gtsave(as_gt(table_2_det_bal), file = file.path(DIR_TAB, "table_2_log_det_balance.png"), vwidth = 1000)
saveRDS(table_2_det_bal, file = file.path(DIR_TAB, "table_2_log_det_balance.rds"))


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


# 8. FIRST-DIFFERENCE & SAVE DATA ============================================
message("--- Section 8: First-Differencing Variables and Saving Data ---")

# We must arrange by country and year to ensure lag() is correct
fd_panel <- log_panel %>%
  arrange(iso3c, year) %>%
  # Group by country so lag() doesn't pull from the previous country
  group_by(iso3c) %>%
  # Create first-differenced (year-to-year change) variables
  mutate(
    fd_log_milex_gdp = log_milex_gdp - lag(log_milex_gdp),
    fd_log_milex_usd = log_milex_usd - lag(log_milex_usd)
  ) %>%
  # Ungroup after calculations
  ungroup() %>%
  # (Optional) Relocate new variables for clarity
  relocate(fd_log_milex_gdp, .after = log_milex_gdp) %>%
  relocate(fd_log_milex_usd, .after = log_milex_usd)

# Note: The first year (2014) for each country will be NA
# This is correct and expected.

saveRDS(fd_panel, file = FD_PANEL)


# 9. SCRIPT COMPLETION =======================================================
message(paste(
  "\n--- Script 03_eda_log.R finished ---",
  "\nAll output (tables and figures) saved to:", here::here("_output"),
  "\nFirst-differenced panel data frame saved to: ", FD_PANEL
))
