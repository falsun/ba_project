# ---------------------------------------------------------------------------- #
#
#   Project:     NATO Defence Spending Bachelor's Thesis
#   Script:      05_sensitivity_CALC_smoothness.R
#   Author:      Frederik Bender Bøeck-Nielsen
#   Date:        2025-11-12
#   Description: This script runs the HEAVY sensitivity calculations
#                for the SMOOTHNESS RESTRICTION test (Rambachan & Roth, 2022b)
#                and saves the results to a file for fast plotting.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

DIR_DATA <- here::here("data", "_processed")
DIR_OUTPUT <- here::here("_output", "_tables", "_robustness")

if (!dir.exists(DIR_OUTPUT)) dir.create(DIR_OUTPUT, recursive = TRUE)

MASTER_PANEL <- file.path(DIR_DATA, "master_panel.rds")
RESULTS_FILE <- file.path(DIR_OUTPUT, "sensitivity_results_smoothness.rds") # <-- New output
VAR_NAME <- "milex_cap"

# Define the grid for the smoothness parameter M
# This is NOT M-bar (relative magnitude), but the bound on the
# second difference of the counterfactual trend. Adjust as needed.
SENSITIVITY_M_GRID <- seq(0, 16, by = 4) # <-- NEW AND SCALED


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, fixest, broom, honestDiD, conflicted)
conflict_prefer("filter", "dplyr")

# 2. PREPARE DATA & RUN BASE MODEL ===========================================
message("--- Section 2: Loading Data and Running Base Model ---")

master_panel <- readRDS(MASTER_PANEL)

analysis_df <- master_panel %>%
  filter(group %in% c("control", "treatment")) %>%
  mutate(event_time = year - 2022)

formula_es <- as.formula(glue::glue(
  "{VAR_NAME} ~ i(event_time, treat_dummy, ref = -1) | iso3c + year"
))
model_es <- feols(
  formula_es,
  data = analysis_df,
  cluster = ~iso3c
)

# 3. EXTRACT MODEL COMPONENTS ================================================
message("--- Section 3: Extracting Model Components ---")

betahat <- summary(model_es)$coefficients
sigma <- summary(model_es)$cov.scaled
pre_treatment_indices <- which(grepl("event_time::-", names(betahat)))
post_treatment_indices <- which(grepl("event_time::[0-9]", names(betahat)))
num_pre_periods <- length(pre_treatment_indices)
num_post_periods <- length(post_treatment_indices)
POST_TREATMENT_LABELS <- c("2022", "2023", "2024") # Assuming 3 post-periods

# 4. RUN SENSITIVITY ANALYSIS (THE SLOW PART) ================================
message(paste("--- Section 4: Running sensitivity analysis (Smoothness) for each post-period ---"))

all_results_list <- list()

for (i in 1:num_post_periods) {
  l_vec_dynamic <- HonestDiD::basisVector(index = i, size = num_post_periods)
  year_label <- POST_TREATMENT_LABELS[i]

  message(paste("  Calculating for year:", year_label, "..."))

  # --- Use the Smoothness Restriction function ---
  # Note: The function is 'createSensitivityResults'
  # The parameter is 'M' (not 'Mbarvec')

  sensitivity_results_df <- HonestDiD::createSensitivityResults(
    betahat = betahat,
    sigma = sigma,
    numPrePeriods = num_pre_periods,
    numPostPeriods = num_post_periods,
    M = SENSITIVITY_M_GRID, # <-- Using the new M grid
    l_vec = l_vec_dynamic
  ) %>%
    mutate(Year = year_label) # Add the year label

  all_results_list[[i]] <- sensitivity_results_df
}

# 5. COMBINE AND SAVE RESULTS ================================================
message("--- Section 5: Combining and Saving Results ---")

combined_sensitivity_data <- bind_rows(all_results_list)

# The resulting file will have columns:
# lb, ub, delta, M, Year
# 'M' will be used for the x-axis in the new plot

saveRDS(combined_sensitivity_data, file = RESULTS_FILE)

# 6. SCRIPT COMPLETION =======================================================
message(paste(
  "\n--- Script 05_sensitivity_CALC_smoothness.R finished ---",
  "\nAll calculation results saved to:", RESULTS_FILE
))
