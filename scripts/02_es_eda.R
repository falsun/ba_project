# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       02_eda.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-11-17 (Final)
#   Description:  Generates descriptive statistics tables, correlation matrices,
#                 distribution plots, and Z-score outlier reports.
#
# ---------------------------------------------------------------------------- #


# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

DIR_DATA <- here::here("data", "_processed")
DIR_SCRIPTS <- here::here("scripts")
DIR_TAB <- here::here("_output", "_tables", "_es_eda")
DIR_FIG <- here::here("_output", "_figures", "_es_eda")

if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

ES_PANEL <- file.path(DIR_DATA, "es_panel.rds")

TREAT_YEAR <- 2022

options(OutDec = ",")

# Ensure these match your master_panel columns exactly
VARS_FOR_EDA <- c(
  "milex_gdp"     = "Forsvarsudgifter (% BNP)",
  "milex_usd_log" = "Log forsvarsudgifter (US$)"
  )

# Optional: Define manual scales if needed (can leave empty)
MANUAL_BREAKS <- list("milex_gdp"     = seq(0, 3, by = 0.5),
                      "milex_usd_log" = seq(18, 26, by = 2)
                      )

MANUAL_BINWIDTHS <- list(
  "milex_gdp"     = 0.25,  # Fits twice into 0.5 breaks
  "milex_usd_log" = 1      # Fits perfectly into integer breaks
)

MANUAL_CAPTIONS <- list("milex_gdp" = "Den eneste outlier er Sydkorea (KOR) 2020, med en Z-score på 2,19.")


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

library(conflicted)
library(here)
library(tidyverse)
library(gtsummary)
library(gt)
library(glue)
library(psych)

conflict_prefer("filter", "dplyr")
conflict_prefer("alpha", "ggplot2")

source(file.path(DIR_SCRIPTS, "00_functions.R"))


# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

es_panel <- readRDS(ES_PANEL) %>%
  mutate(group = factor(group, levels = c("Behandlet", "Kontrol")))

# 2. Create the pre-treatment df for tables and dist plots
# This inherits the filter from ts_df, so it is also safe
pre_panel <- es_panel %>%
  filter(year < TREAT_YEAR) %>%
  select(group, iso3c, year, all_of(names(VARS_FOR_EDA)))

# create country avg dataframe for SMD calculation for Summary Table
country_avg_df <- pre_panel %>%
  group_by(group, iso3c) %>%
  summarise(across(all_of(names(VARS_FOR_EDA)), \(x) mean(x, na.rm = TRUE)), .groups = "drop")


# 3. TABLE 1: PRE-TREATMENT BALANCE ==========================================
message("--- Section 4: Generating Table 1 (Balance) ---")

# Helper for skewness
skew <- function(x) as.numeric(psych::skew(x, na.rm = TRUE))

# 1. Calculate SMD on Country Averages (N=30)
smd_lookup <- country_avg_df %>%
  select(group, all_of(names(VARS_FOR_EDA))) %>%
  tbl_summary(by = group, missing = "no") %>%
  add_difference(test = everything() ~ "smd") %>%
  getElement("table_body") %>%
  transmute(
    variable,
    smd_formatted = formatC(estimate, digits = 2, format = "f", decimal.mark = ",")
  )

# 2. Generate Main Table on Full Panel (N=240)
sum_stat_table <- pre_panel %>%
  select(group, all_of(names(VARS_FOR_EDA))) %>%
  tbl_summary(
    by = group,
    label = as.list(VARS_FOR_EDA),
    missing = "no",
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean} ({sd})", "{min}–{max} ({skew})"),
    digits = all_continuous() ~ 2
  ) %>%

  # Inject SMD and Translate Labels
  modify_table_body(~ .x %>%
                      left_join(smd_lookup, by = "variable") %>%
                      mutate(
                        label = case_when(
                          label == "Mean (SD)" ~ "Gennemsnit (standardafvigelse)",
                          str_detect(label, "Min") ~ "Min.–maks. (skævhed)",
                          TRUE ~ label
                        ),
                        smd_formatted = ifelse(label == "Gennemsnit (standardafvigelse)", smd_formatted, "")
                      )
  ) %>%

  # Set Headers
  modify_header(
    label = "",
    stat_1 = "Behandlet (N = {n})",
    stat_2 = "Kontrol (N = {n})",
    smd_formatted = "SMD"
  ) %>%

  # GT Styling
  as_gt() %>%
  fmt_markdown(columns = c(stat_1, stat_2)) %>%
  cols_align(align = "right", columns = c(stat_1, stat_2, smd_formatted)) %>%

  # Footnotes & Sources
  tab_footnote(
    footnote = "Standardiseret gennemsnitsforskel (Hedges' g). Beregnet på landegennemsnit.",
    locations = cells_column_labels(columns = smd_formatted)
  ) %>%
  tab_source_note("N repræsenterer antal lande-år observationer.") %>%

  # Styling
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = label, rows = row_type == "label")
  ) %>%
  theme_gt_bachelor_project()

# Save
gtsave(sum_stat_table, file = file.path(DIR_TAB, "summary_statistics.html"))


# 4. GENERATE OUTLIER REPORTS (Z-SCORE) ======================================
message("--- Section 6: Generating Outlier Reports (IQR + Z-Score) ---")

for (var in names(VARS_FOR_EDA)) {
  print_outlier_report(
    data = pre_panel,
    var_name = !!sym(var),
    var_label = VARS_FOR_EDA[var]
  )
}


# 5. GENERATE DISTRIBUTION PLOTS =============================================
message("--- Section 7: Generating Distribution Plots ---")

for (var in names(VARS_FOR_EDA)) {
  create_distribution_plot(
    data = pre_panel,
    var_name = !!sym(var),
    var_label = VARS_FOR_EDA[var],
    manual_breaks = MANUAL_BREAKS[[var]],
    manual_binwidth = MANUAL_BINWIDTHS[[var]],
    manual_caption = MANUAL_CAPTIONS[[var]],
    output_dir = DIR_FIG
  )
}


# 6. GENERATE AGGREGATED TIME-SERIES PLOTS ===================================
message("--- Section 8: Generating Aggregated Time-Series Plots ---")

# 1. Aggregate and Translate Groups
agg_data <- es_panel %>%
  group_by(group, year) %>%
  summarise(
    milex_gdp = mean(milex_gdp, na.rm = TRUE),
    milex_usd_log = mean(milex_usd_log, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Define Helper Function
save_trend_plot <- function(y_var, y_axis_label) {

  ts_plot <- ggplot(agg_data, aes(x = year, y = .data[[y_var]], color = group, group = group)) +
    geom_line() +
    geom_vline(xintercept = TREAT_YEAR - 1, linetype = "dashed", color = "grey40") +
    scale_x_continuous(breaks = seq(2014, 2024, by = 2)) +
    scale_color_project_qual(name = NULL) +
    theme_bachelor_project() +
    labs(x = NULL, y = y_axis_label)

  ggsave(here(DIR_FIG, glue("agg_ts_{y_var}.png")), ts_plot, width = 8, height = 6)
}

# 3. Execute with Custom Danish Labels

# Plot 1: % of GDP
save_trend_plot(
  y_var = "milex_gdp",
  y_axis_label = "Forsvarsudgifter (% BNP)"
)

# Plot 2: Log USD
save_trend_plot(
  y_var = "milex_usd_log",
  y_axis_label = "Log forsvarsudgifter (US$)"
)

message(paste(
  "\n--- Script 02_eda.R finished ---",
  "\nAll output (tables and figures) saved to:", here::here("_output")
))
