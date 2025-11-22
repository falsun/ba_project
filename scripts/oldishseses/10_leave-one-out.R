# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       06_robustness_leave_one_out.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-11-16
#   Description:  Runs a leave-one-out (LOO) robustness test.
#                 It runs one baseline model (N=30) and 30 LOO models (N=29),
#                 then compiles key stats into a single summary table.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

# --- Set Variables to Test ---
VARS_TO_TEST <- c("milex_cap", "milex_cap_log", "milex_gdp", "milex_gdp_log")

# --- Define Directories ---
DIR_DATA <- here::here("data", "_processed")
DIR_SCRIPTS <- here::here("scripts")
DIR_TAB <- here::here("_output", "_tables", "_robustness")
DIR_FIG <- here::here("_output", "_figures", "_robustness")

if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)

MASTER_PANEL <- file.path(DIR_DATA, "master_panel.rds")

# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, # For dplyr, ggplot2, purrr, stringr
  fixest, # For feols, wald
  broom, # For tidy()
  glue, # For glue()
  here, # For here::here()
  conflicted, # For conflict_prefer()
  gt, # For creating tables
  gtsummary # For p-value formatting
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# Source your custom functions (for theme_gt_bachelor_project)
source(file.path(DIR_SCRIPTS, "00_functions.R"))

options(scipen = 999)

# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

# Load the main panel data
master_panel <- readRDS(MASTER_PANEL)

# Create the base analysis dataframe
analysis_df <- master_panel %>%
  filter(group %in% c("control", "treatment")) %>%
  mutate(event_time = year - 2022)

# Get the list of the 30 units we will loop over
all_units_loo <- unique(analysis_df$iso3c)
message(paste("--- Running LOO analysis on", length(all_units_loo), "units ---"))


# 3. HELPER FUNCTION TO EXTRACT METRICS ======================================
# This function runs a model and extracts the 7 key stats we want
extract_loo_metrics <- function(data, var) {
  # 1. Define and run the model
  formula <- as.formula(glue(
    "{var} ~ i(event_time, treat_dummy, ref = c(-1, -8)) | iso3c + year + treat_dummy[year]"
  ))

  # We MUST cluster here. This is our main model specification.
  model <- feols(formula, data = data, cluster = ~iso3c)

  # 2. Extract F-Tests
  all_coef_names <- names(coef(model))

  # Pre-Trend F-Test
  pre_terms <- all_coef_names[grepl("event_time::", all_coef_names) & grepl("-", all_coef_names)]
  p_pre <- NA
  if (length(pre_terms) > 0) {
    f_test_pre <- try(wald(model, pre_terms), silent = TRUE)
    if (!inherits(f_test_pre, "try-error")) {
      p_pre <- f_test_pre$p[1]
    }
  }

  # 3. Extract individual ATTs and p-values
  tidy_model <- tidy(model)

  get_att <- function(term_name) {
    att_row <- tidy_model %>% filter(term == term_name)
    if (nrow(att_row) == 1) {
      return(list(att = att_row$estimate, pval = att_row$p.value))
    }
    return(list(att = NA, pval = NA))
  }

  att_2022 <- get_att("event_time::0:treat_dummy")
  att_2023 <- get_att("event_time::1:treat_dummy")
  att_2024 <- get_att("event_time::2:treat_dummy")

  # 4. Return all stats as a 1-row data frame
  return(data.frame(
    pre_trend_p = p_pre,
    att_2022 = att_2022$att, p_2022 = att_2022$pval,
    att_2023 = att_2023$att, p_2023 = att_2023$pval,
    att_2024 = att_2024$att, p_2024 = att_2024$pval
  ))
}

# 4. START MAIN LOOP FOR EACH VARIABLE ========================================
message(paste("--- Starting LOO loop for", length(VARS_TO_TEST), "variables ---"))

for (VAR_TO_TEST in VARS_TO_TEST) {
  message(paste("\n--- Processing:", VAR_TO_TEST, "---"))

  # --- 4.1. Run BASELINE Model (N=30) ---
  message("  ... 4.1: Running Baseline (N=30) Model")

  baseline_results <- extract_loo_metrics(analysis_df, VAR_TO_TEST) %>%
    mutate(dropped_unit = "Baseline", .before = 1)

  # --- 4.2. Run LOO Loop (30 iterations) ---
  message("  ... 4.2: Running 30 LOO iterations")

  loo_results_df <- map_dfr(all_units_loo, function(unit_to_drop) {
    # Create the N=29 dataset for this loop
    loo_data <- analysis_df %>%
      filter(iso3c != unit_to_drop)

    # Run the function to get all stats
    metrics <- extract_loo_metrics(loo_data, VAR_TO_TEST)

    # Add the "dropped_unit" label and return
    metrics %>%
      mutate(dropped_unit = unit_to_drop, .before = 1)
  })

  # --- 4.3. Combine and Format the Final Table ---
  message("  ... 4.3: Generating summary table")

  # Get unit groups (Treated/Control) to add to the table
  unit_groups <- analysis_df %>%
    distinct(iso3c, group) %>%
    rename(dropped_unit = iso3c) %>%
    add_row(dropped_unit = "Baseline", group = "All 30")

  # Combine baseline and LOO results
  final_summary_data <- bind_rows(baseline_results, loo_results_df) %>%
    left_join(unit_groups, by = "dropped_unit") %>%
    # Reorder columns
    select(
      dropped_unit, pre_trend_p, att_2022, p_2022, att_2023, p_2023, att_2024,
      p_2024)

  # --- 4.4. Build and Save the gt Table ---

  # Create the hybrid columns as you suggested
  final_table_formatted <- final_summary_data %>%
    mutate(
      # Format F-Test p-values
      p_pre_fmt = gtsummary::style_pvalue(pre_trend_p, digits = 3),

      # Format ATT (p-value) columns
      att_2022_fmt = glue("{round(att_2022, 1)} ({gtsummary::style_pvalue(p_2022, digits = 2)})"),
      att_2023_fmt = glue("{round(att_2023, 1)} ({gtsummary::style_pvalue(p_2023, digits = 2)})"),
      att_2024_fmt = glue("{round(att_2024, 1)} ({gtsummary::style_pvalue(p_2024, digits = 2)})")
    ) %>%
    # Keep only the columns we want to display
    select(dropped_unit, p_pre_fmt, att_2022_fmt, att_2023_fmt, att_2024_fmt)

  # Build the gt object
  table_loo_summary_gt <- gt(final_table_formatted) %>%
    tab_header(
      title = "Leave-One-Out (LOO) Robustness Check",
      subtitle = glue("Outcome: {VAR_TO_TEST}")
    ) %>%
    cols_label(
      dropped_unit = "Dropped Unit",
      p_pre_fmt = "Pre-Trend F-Test (p)",
      att_2022_fmt = "ATT 2022",
      att_2023_fmt = "ATT 2023",
      att_2024_fmt = "ATT 2024"
    ) %>%
    # Right-align all summary columns
    cols_align(
      align = "right",
      columns = c(p_pre_fmt, att_2022_fmt, att_2023_fmt, att_2024_fmt)
    ) %>%
    # Bold the baseline row
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = dropped_unit == "Baseline")
    ) %>%
    theme_gt_bachelor_project() %>%
    tab_source_note(
      source_note = md("ATT columns show: `Estimate (p-value)`. Baseline row (N=30) is compared against 30 models (N=29) where one unit is dropped.")
    )

  # --- 4.5. Save the Table ---
  table_html_filename <- file.path(DIR_TAB, glue("loo_summary_{VAR_TO_TEST}.html"))
  gtsave(table_loo_summary_gt, file = table_html_filename)

  message(paste("--- Finished processing:", VAR_TO_TEST, "---"))
} # End of FOR loop for variables

# 5. SCRIPT COMPLETION =======================================================
message(paste(
  "\n--- Script 06_robustness_leave_one_out.R finished ---",
  "\nAll LOO summary tables saved to:", DIR_TAB
))
