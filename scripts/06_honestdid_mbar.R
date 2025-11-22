# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       05_sensitivity_CALC.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-11-12
#   Description:  This script runs the HEAVY sensitivity calculations
#                 and saves the results to a file for fast plotting.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

DIR_DATA <- here::here("data", "_processed")
DIR_OUTPUT <- here::here("_output", "_tables", "_robustness")

if (!dir.exists(DIR_OUTPUT)) dir.create(DIR_OUTPUT, recursive = TRUE)

MASTER_PANEL <- file.path(DIR_DATA, "master_panel.rds")
RESULTS_FILE <- file.path(DIR_OUTPUT, "sensitivity_results.rds") # <-- New output

VAR_NAME <- "milex_cap"
SENSITIVITY_M_VEC <- seq(0.5, 1.5, by = 0.5)

# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, fixest, broom, HonestDiD, conflicted)
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
message(paste("--- Section 4: Running sensitivity analysis for each post-period ---"))

all_results_list <- list()

for (i in 1:num_post_periods) {
  l_vec_dynamic <- HonestDiD::basisVector(index = i, size = num_post_periods)
  year_label <- POST_TREATMENT_LABELS[i]

  message(paste("  Calculating for year:", year_label, "..."))

  sensitivity_results_df <- HonestDiD::createSensitivityResults_relativeMagnitudes(
    betahat = betahat,
    sigma = sigma,
    numPrePeriods = num_pre_periods,
    numPostPeriods = num_post_periods,
    Mbarvec = SENSITIVITY_M_VEC,
    l_vec = l_vec_dynamic
  ) %>%
    mutate(Year = year_label)

  all_results_list[[i]] <- sensitivity_results_df
}

# 5. COMBINE AND SAVE RESULTS ================================================
message("--- Section 5: Combining and Saving Results ---")

combined_sensitivity_data <- bind_rows(all_results_list)

# We are *not* joining the 'estimate' from tidy(), as it is inconsistent
# with the robust CIs. The plot will only show the robust CI range.

saveRDS(combined_sensitivity_data, file = RESULTS_FILE)

# 6. SCRIPT COMPLETION =======================================================
message(paste(
  "\n--- Script 05_sensitivity_CALC.R finished ---",
  "\nAll calculation results saved to:", RESULTS_FILE
))
