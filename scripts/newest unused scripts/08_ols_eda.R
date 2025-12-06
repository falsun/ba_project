# ---------------------------------------------------------------------------- #
#
#   Script:       10_ols_eda.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Description:  Exploratory Data Analysis for Cross-Sectional OLS Data.
#                 - Descriptive Statistics (Skew/Kurtosis)
#                 - Split Correlation Matrices (Pre vs. Post Invasion)
#                 - Outlier Reports (Z-Scores)
#                 - Distribution Plots
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

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

# --- DEFINE VARIABLES FOR DESCRIPTIVES (ALL) ---
VARS_FOR_EDA <- c(
  # Outcomes
  "milex_gdp_pre"  = "Milex Change in GDP Share (2014-21)",
  "milex_gdp_post" = "Milex Change in GDP Share (2021-25)",
  "milex_usd_pre"  = "Milex Change in 2023 US$ (2014-21)",
  "milex_usd_post" = "Milex Change in 2023 US$ (2021-24)",

  # Explanatory
  "dist_enemy_log" = "Log distance to enemy (km)",
  "border_rus"     = "Shared border Russia (dummy)",
  "nato_gap_2014"  = "NATO 2% Gap 2014",
  "nato_gap_2021"  = "NATO 2% Gap 2021",
  "gdp_2014_log"   = "Log GDP (2014)",
  "gdp_2021_log"   = "Log GDP (2021)",

  # Controls
  "gdp_cap_2014_log"   = "Log GDP per capita (2014)",
  "gdp_cap_2021_log"   = "Log GDP per capita (2021)",
  "gdp_growth_post"     = "GDP growth (2021-25)",
  "debt_gdp_2014_log"  = "Log debt % GDP (2014)",
  "debt_gdp_2021_log"  = "Log debt % GDP (2021)",
  "us_troops_2014_log" = "Log US troops (2014)",
  "us_troops_2021_log" = "Log US troops (2021)",
  "post_com"           = "Post Communist (dummy)"
)

# --- DEFINE GROUPS FOR CORRELATION MATRICES ---
# 1. War Shock Model (2021-2025)
VARS_CORR_WAR <- c(
  "milex_gdp_post",
  "milex_usd_post",
  "post_com",
  "border_rus",
  "dist_enemy_log",
  "nato_gap_2021",
  "gdp_2021_log",
  "gdp_growth_post",
  "gdp_cap_2021_log",
  "debt_gdp_2021_log",
  "us_troops_2021_log"
)

# 2. Crimea Legacy Model (2014-2021)
VARS_CORR_CRIMEA <- c(
  "milex_gdp_pre",
  "milex_usd_pre",
  "post_com",
  "border_rus",
  "dist_enemy_log",
  "nato_gap_2014",
  "gdp_2014_log",
  "gdp_cap_2014_log",
  "debt_gdp_2014_log",
  "us_troops_2014_log"
)


# --- LOCAL HELPER FUNCTIONS (No Year/Group Dependency) ---

create_outlier_table_ols <- function(data, var_name, var_label, output_dir, title = NULL) {
  var_name_enquo <- enquo(var_name)
  current_var_name <- quo_name(var_name_enquo)

  outlier_data <- data %>%
    mutate(
      iqr = IQR({{ var_name_enquo }}, na.rm = TRUE),
      upper_bound = quantile({{ var_name_enquo }}, 0.75, na.rm = TRUE) + 1.5 * iqr,
      lower_bound = quantile({{ var_name_enquo }}, 0.25, na.rm = TRUE) - 1.5 * iqr,
      mean_val = mean({{ var_name_enquo }}, na.rm = TRUE),
      sd_val = sd({{ var_name_enquo }}, na.rm = TRUE),
      z_score = ({{ var_name_enquo }} - mean_val) / sd_val,
      outlier_type = case_when(
        {{ var_name_enquo }} > upper_bound ~ "High",
        {{ var_name_enquo }} < lower_bound ~ "Low",
        TRUE ~ NA_character_
      ),
      is_outlier = !is.na(outlier_type)
    ) %>%
    filter(is_outlier) %>%
    select(iso3c, outlier_type, value = {{ var_name_enquo }}, z_score) %>%
    arrange(desc(value))

  if (nrow(outlier_data) == 0) {
    message(paste("  No outliers found for", current_var_name))
    return(invisible(NULL))
  }

  message(paste("  Saving outlier table for", current_var_name))

  outlier_table <- gt(outlier_data) %>%
    tab_header(title = gt::md(glue::glue("**{title}**")), subtitle = "Outliers defined by 1.5*IQR rule") %>%
    cols_label(iso3c = "Country", outlier_type = "Type", value = "Value", z_score = "Z-Score") %>%
    fmt_number(columns = c(value, z_score), decimals = 3) %>%
    theme_gt_bachelor_project()

  file_name <- glue::glue("outliers_{current_var_name}.html")
  gtsave(outlier_table, file = file.path(output_dir, file_name))
}

create_distribution_plot_ols <- function(data, var_name, var_label, output_dir, title) {
  var_name_enquo <- enquo(var_name)

  p1 <- ggplot(data, aes(x = {{ var_name_enquo }})) +
    geom_histogram(aes(y = ..density..), bins = 15, fill = "#2c3e50", color = "white", alpha = 0.7) +
    geom_density(color = "#c0392b", size = 1) +
    labs(x = var_label, y = "Density") + theme_bachelor_project()

  p2 <- ggplot(data, aes(x = {{ var_name_enquo }}, y = "")) +
    geom_boxplot(fill = "#2c3e50", color = "black", alpha = 0.5, outlier.colour = "red") +
    labs(x = var_label, y = NULL) +
    theme_bachelor_project() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

  p_final <- p1 / p2 + plot_layout(heights = c(3, 1)) + plot_annotation(title = title)

  file_name <- glue::glue("dist_{quo_name(var_name_enquo)}.png")
  ggsave(file.path(output_dir, file_name), p_final, width = 6, height = 5, bg = "white")
}


# 1. LOAD DATA ===============================================================
message("--- Section 1: Loading Data ---")

ols_data <- readRDS(OLS_DATA) %>%
  select(iso3c, all_of(names(VARS_FOR_EDA)))


# 2. DESCRIPTIVE STATISTICS TABLE ============================================
message("--- Section 2: Generating Descriptive Statistics ---")

skew_fun <- function(x, na.rm = TRUE) { psych::skew(x, na.rm = na.rm) }

table_descriptives <- ols_data %>%
  select(all_of(names(VARS_FOR_EDA))) %>%
  tbl_summary(
    label = as.list(VARS_FOR_EDA),
    missing = "no",
    statistic = all_continuous() ~ "{mean} ({sd}) | {min}–{max} | Skew: {skew_fun}",
    digits = all_continuous() ~ 2
  ) %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("**Table 1: Descriptive Statistics for Cross-Sectional Analysis**")

gtsave(as_gt(table_descriptives), file = file.path(DIR_TAB, "table_1_ols_descriptives.html"))


# 3. CORRELATION MATRICES (SPLIT) ============================================
message("--- Section 3: Checking Correlations (Split by Model) ---")

# Helper to save correlation plot
save_corr_matrix <- function(data, var_list, title, filename) {
  cor_matrix <- cor(data %>% select(all_of(var_list)), use = "complete.obs")

  p_corr <- ggcorrplot(cor_matrix, lab = TRUE,
                       title = title,
                       colors = c("#6D9EC1", "white", "#E46726"),
                       type = "lower") + # Lower triangle is cleaner
    theme_bachelor_project()

  ggsave(file.path(DIR_FIG, filename), p_corr, width = 8, height = 8, bg = "white")
  message(paste("Saved:", filename))
}

# A. War Shock Model (2021-2025)
save_corr_matrix(
  data = ols_data,
  var_list = VARS_CORR_WAR,
  title = "Correlation: War Shock Variables (2021-2025)",
  filename = "ols_corr_war_shock.png"
)

# B. Crimea Legacy Model (2014-2021)
save_corr_matrix(
  data = ols_data,
  var_list = VARS_CORR_CRIMEA,
  title = "Correlation: Crimea Legacy Variables (2014-2021)",
  filename = "ols_corr_crimea_legacy.png"
)


# 4. OUTLIER REPORTS =========================================================
message("--- Section 4: Generating Outlier Reports ---")

for (var in names(VARS_FOR_EDA)) {
  create_outlier_table_ols(
    data       = ols_data,
    var_name   = !!sym(var),
    var_label  = VARS_FOR_EDA[var],
    output_dir = DIR_TAB,
    title      = glue::glue("Outlier Report: {VARS_FOR_EDA[var]}")
  )
}


# 5. DISTRIBUTION PLOTS ======================================================
message("--- Section 5: Generating Distribution Plots ---")

for (var in names(VARS_FOR_EDA)) {
  create_distribution_plot_ols(
    data       = ols_data,
    var_name   = !!sym(var),
    var_label  = VARS_FOR_EDA[var],
    output_dir = DIR_FIG,
    title      = glue::glue("Distribution: {VARS_FOR_EDA[var]}")
  )
}

message("\n--- Script 10_ols_eda.R finished ---")
