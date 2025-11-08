# ---------------------------------------------------------------------------- #
#
#   Project:     NATO Defence Spending Bachelor's Thesis
#   Script:      03_prelims.R
#   Author:      Frederik Bender Bøeck-Nielsen
#   Date:        2025-10-21 (Refactored)
#   Description: Visualizes parallel trends, and conducts stationarity and
#                cross-sectional dependence tests.
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

VARS_FOR_PRELIM <- c(
  "log_milex_usd" = "Military Spending (Constant US$)"
)


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, plm, tseries, gt, broom, scales, gtsummary, conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("between", "dplyr")

source(file.path(DIR_SCRIPTS, "00_functions.R"))

options(scipen = 999)


# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

log_panel <- readRDS(LOG_PANEL)

TREATMENT_YEAR <- log_panel %>%
  filter(group == "treatment", post_treat == 1) %>%
  pull(year) %>%
  min()

analysis_df <- log_panel %>%
  select(group, iso3c, year, post_treat, all_of(names(VARS_FOR_PRELIM))) %>%
  filter(group %in% c("control", "treatment"))

pre_treat_df <- analysis_df %>% filter(post_treat == 0)


# 3. GENERATE TIME-SERIES PLOTS ==============================================
message("--- Section 3: Generating Time-Series Plots ---")

for (var in names(VARS_FOR_PRELIM)) {
  create_aggregated_ts_plot(
    data = analysis_df,
    var_name = !!sym(var),
    var_label = VARS_FOR_PRELIM[var],
    treatment_year = TREATMENT_YEAR,
    output_dir = DIR_FIG,
    title = glue::glue("Aggregated Trends for {VARS_FOR_PRELIM[var]}")
  )

  create_spaghetti_ts_plot(
    data = analysis_df,
    var_name = !!sym(var),
    var_label = VARS_FOR_PRELIM[var],
    treatment_year = TREATMENT_YEAR,
    output_dir = DIR_FIG,
    title = glue::glue("Individual Trends for {VARS_FOR_PRELIM[var]}")
  )
}


# 4. TEST FOR STATIONARITY (ADF TEST) ========================================
message("--- Section 4: Testing for Stationarity (ADF) ---")

safe_adf_test <- function(data_vector) {
  tryCatch(
    {
      test <- tseries::adf.test(data_vector, alternative = "stationary")
      tibble::tibble(statistic = test$statistic, p.value = test$p.value)
    },
    error = function(e) {
      tibble::tibble(statistic = NA, p.value = NA)
    }
  )
}

adf_results <- pre_treat_df %>%
  group_by(iso3c, group) %>%
  arrange(year) %>%
  summarise(adf_test_result = list(safe_adf_test(log_milex_usd)), .groups = "drop") %>%
  tidyr::unnest(adf_test_result)

non_stationary_series <- adf_results %>%
  filter(p.value > 0.05 | is.na(p.value)) %>%
  arrange(desc(p.value), desc(statistic))

message("\nUnits with potentially non-stationary military expenditure (p > 0.05):")
print(non_stationary_series, n = Inf)

if (nrow(non_stationary_series) > 0) {
  tbl_adf_results <- non_stationary_series %>%
    gt() %>%
    cols_label(
      iso3c = "Country",
      group = "Group",
      statistic = "Test Statistic",
      p.value = "P-Value"
    ) %>%
    fmt_number(columns = c(statistic, p.value), decimals = 3) %>%
    tab_header(
      title = "Table 3: Augmented Dickey-Fuller Test Results",
      subtitle = "Units with Potentially Non-Stationary Military Expenditure (p > 0.05)"
    ) %>%
    tab_source_note("H0: Series is non-stationary (p > 0.05).")

  gtsave(tbl_adf_results, file = file.path(DIR_TAB, "table_3_adf_test.png"))
}


# 5. TEST FOR CROSS-SECTIONAL DEPENDENCE (PESARAN'S CD TEST) ================
message("--- Section 5: Testing for Cross-Sectional Dependence ---")

p_pre_treat_df <- pdata.frame(pre_treat_df, index = c("iso3c", "year"))
cd_test_result <- pcdtest(log_milex_usd ~ 1, data = p_pre_treat_df)

print(cd_test_result)

tbl_cd_test <- broom::tidy(cd_test_result) %>%
  select(statistic, p.value) %>%
  gt() %>%
  cols_label(
    statistic = "Test Statistic",
    p.value = "P-Value"
  ) %>%
  fmt(
    columns = p.value,
    fns = function(x) gtsummary::style_pvalue(x, digits = 3)
  ) %>%
  tab_header(title = "Table 4: Pesaran's CD Test for Cross-Sectional Dependence") %>%
  tab_source_note("H0: No cross-sectional dependence (p > 0.05).")

gtsave(tbl_cd_test, file = file.path(DIR_TAB, "table_4_cd_test.png"))


# 6. SCRIPT COMPLETION =======================================================
message("\n--- Script 03_prelims.R finished successfully ---")
