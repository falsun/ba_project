# ---------------------------------------------------------------------------- #
#
#   Script:       22_ols_model_selection.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Description:  Functional Form "Horse Race" for OLS Analysis.
#                 - Optimizes Spline Thresholds via BIC.
#                 - Compares subset of Functional Forms (Log/Inverse/Spline).
#                 - OUTPUT: One table per Control Scenario (Pre + Post included).
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION ===========================================================
message("--- Section 0: Loading Configuration ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, broom, glue, here, gt)

DIR_DATA_PROC <- here::here("data", "_processed")
DIR_TAB       <- here::here("_output", "_tables", "_model_selection")

if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)

OLS_DATA <- file.path(DIR_DATA_PROC, "ols_data.rds")

if (!file.exists(OLS_DATA)) stop("ols_data.rds not found. Run Data Prep first.")

# --- DEFINE PERIODS AND VARIABLES ---
PERIODS <- list(
  "crimea" = list(
    title = "Crimea Legacy (2014-2021)",
    col_gdp = "milex_gdp_pre",
    var_gap = "nato_gap_2014",
    var_debt = "debt_gdp_2014_log"
  ),
  "war" = list(
    title = "War Shock (2021-2025)",
    col_gdp = "milex_gdp_post",
    var_gap = "nato_gap_2021",
    var_debt = "debt_gdp_2021_log"
  )
)

VARS_FOR_ANALYSIS <- c("milex_gdp")


# 1. HELPER FUNCTIONS ========================================================
message("--- Section 1: Defining Optimization Functions ---")

# A. Find BIC-Optimal Spline Threshold (Bivariate Basis)
find_optimal_spline_threshold <- function(data, y_col, threshold_range = seq(300, 3000, by = 10)) {

  data_clean <- data %>% filter(!is.na(.data[[y_col]]), !is.na(dist_enemy))
  if (nrow(data_clean) < 5) return(NA)

  bic_results <- list()

  for (t in threshold_range) {
    data_spline <- data_clean %>%
      mutate(
        dist_near_T = pmin(dist_enemy, t),
        dist_far_T  = pmax(dist_enemy - t, 0)
      )

    model_fit <- tryCatch({
      lm(as.formula(paste(y_col, "~ dist_near_T + dist_far_T")), data = data_spline)
    }, error = function(e) NULL)

    if (!is.null(model_fit)) {
      bic_results[[as.character(t)]] <- tibble(Threshold = t, BIC_Value = BIC(model_fit))
    }
  }

  if (length(bic_results) == 0) return(NA)

  bind_rows(bic_results) %>% arrange(BIC_Value) %>% slice(1) %>% pull(Threshold)
}

# B. Run Functional Forms (Scenario Version)
run_functional_forms_scenarios <- function(data, y_col, threshold_T, var_gap, var_debt) {

  data <- data %>% filter(!is.na(.data[[y_col]]), !is.na(dist_enemy))

  data <- data %>%
    mutate(
      dist_near_T = pmin(dist_enemy, threshold_T),
      dist_far_T  = pmax(dist_enemy - threshold_T, 0)
    )

  # 1. Define the Base Functional Forms (Reduced List)
  base_forms <- list(
    "Log-Linear"   = "~ dist_enemy_log",
    "Inverse"      = "~ dist_enemy_inv",
    "Inv Sqrt"     = "~ dist_enemy_inv_sqrt",
    "Inv Log"      = "~ dist_enemy_inv_log",
    "Spline"       = "~ dist_near_T + dist_far_T"
  )

  # 2. Define the Control Scenarios (Formula Suffixes)
  scenarios <- list(
    "1_Bivariate"    = "",
    "2_Control_Gap"  = paste("+", var_gap),
    "3_Control_Debt" = paste("+", var_debt),
    "4_Control_Full" = paste("+", var_gap, "+", var_debt)
  )

  results_list <- list()

  # 3. Nested Loop: Scenarios -> Forms
  for (scen_name in names(scenarios)) {
    suffix <- scenarios[[scen_name]]

    for (form_name in names(base_forms)) {
      base_part <- base_forms[[form_name]]

      # Construct Full Formula
      full_formula <- as.formula(paste(y_col, base_part, suffix))

      # Run Model
      mod <- lm(full_formula, data = data)
      stats <- glance(mod)
      tidy_stats <- tidy(mod)

      # Extract P-value for Distance Term
      # We look for the term containing "dist".
      # For Spline, this picks the first match (Near Slope), which is the primary threat signal.
      dist_row <- tidy_stats %>% filter(str_detect(term, "dist")) %>% slice(1)
      dist_p_val <- if(nrow(dist_row) > 0) dist_row$p.value else NA

      results_list[[paste(scen_name, form_name)]] <- tibble(
        Scenario = scen_name,
        Form     = form_name,
        Adj_R2   = stats$adj.r.squared,
        AIC      = AIC(mod),
        BIC      = BIC(mod),
        Dist_P   = dist_p_val # Add P-value column
      )
    }
  }

  bind_rows(results_list) %>% arrange(Scenario, BIC)
}


# 2. EXECUTE ANALYSIS ========================================================
message("--- Section 2: Loading Data and Running Loops ---")

df <- readRDS(OLS_DATA) %>%
  mutate(
    dist_enemy_inv = 1 / dist_enemy,
    dist_enemy_inv_sq = 1 / (dist_enemy^2),
    dist_enemy_inv_log = 1 / log(dist_enemy),
    dist_enemy_inv_sqrt = 1 / sqrt(dist_enemy),
    dist_enemy_sqrt = sqrt(dist_enemy),
    dist_enemy_log = log(dist_enemy)
  )

all_results <- list()

for (VAR in VARS_FOR_ANALYSIS) {
  message(glue("Processing: {VAR}"))

  for (p_name in names(PERIODS)) {
    p_cfg <- PERIODS[[p_name]]

    y_col <- p_cfg$col_gdp
    var_gap <- p_cfg$var_gap
    var_debt <- p_cfg$var_debt

    if (!y_col %in% names(df)) stop(glue("Column '{y_col}' not found."))

    message(glue("  - Period: {p_cfg$title} | Outcome: {y_col}"))

    # 1. Optimize Spline (Bivariate)
    opt_t <- find_optimal_spline_threshold(df, y_col)

    # 2. Run All Scenarios
    res <- run_functional_forms_scenarios(df, y_col, opt_t, var_gap, var_debt) %>%
      mutate(
        Outcome_Variable = VAR,
        Period = p_cfg$title,
        Optimal_Spline_T = opt_t
      )

    all_results[[paste(VAR, p_name)]] <- res
  }
}

final_table <- bind_rows(all_results)


# 3. SAVE SCENARIO OUTPUTS ===================================================
message("--- Section 3: Formatting and Saving Scenario Tables ---")

save_scenario_table <- function(data, var_name, scenario_name) {

  # Filter Data (Keep both periods in the same table)
  tbl_data <- data %>%
    filter(Outcome_Variable == var_name, Scenario == scenario_name) %>%
    group_by(Period) %>%
    arrange(BIC) %>%
    ungroup() %>%
    select(Period, Form, Dist_P, Adj_R2, AIC, BIC) # Added Dist_P

  # Create Table
  gt_obj <- tbl_data %>%
    gt(groupname_col = "Period") %>%
    tab_header(
      title = md(glue("**Functional Form Selection: {var_name}**")),
      subtitle = glue("Scenario: {scenario_name} (Sorted by BIC)")
    ) %>%
    cols_label(
      Dist_P = "Dist P-Val"
    ) %>%
    fmt_number(columns = c(Dist_P), decimals = 4) %>% # Format P-value
    fmt_number(columns = c(Adj_R2, AIC, BIC), decimals = 2) %>%
    tab_style(
      style = cell_fill(color = "#e6f7ff"),
      locations = cells_body(rows = BIC == min(BIC)) # Logic is approximate for grouped tables
    )

  # Save
  filename <- glue("selection_{scenario_name}_{var_name}.html")
  gtsave(gt_obj, file.path(DIR_TAB, filename))
  message(glue("Saved: {filename}"))
}

# Loop through scenarios to create the 4 tables
unique_scenarios <- unique(final_table$Scenario)

for (s in unique_scenarios) {
  save_scenario_table(final_table, "milex_gdp", s)
}

message("\n--- Script 22_ols_model_selection.R finished ---")
