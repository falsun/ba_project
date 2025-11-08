# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       04b_run_placebo_2020.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-10-26
#   Description:  This script runs ONLY the stochastic calculations
#                 for the 2020 IN-TIME PLACEBO model.
#                 It runs separately due to bootstrap instability.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

# Try a new seed to see if it's stable
set.seed(1)

DIR_SCRIPTS <- here::here("scripts")
DIR_OUT     <- here::here("_output", "_raw_objects")

# Ensure the output directory exists
if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)

SE_METHOD   <- "bootstrap"


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  here,
  synthdid
)

# Need to source the functions file to get 'get_raw_sdid_plot'
source(file.path(DIR_SCRIPTS, "00_functions.R"))


# 2. LOAD MAIN ESTIMATE ======================================================
message("--- Section 2: Loading Main SDID Estimate ---")

# This script DEPENDS on '04_run_calculations.R' having been run successfully.
sdid_estimate_path <- file.path(DIR_OUT, "sdid_estimate.rds")

if (!file.exists(sdid_estimate_path)) {
  stop("ERROR: 'sdid_estimate.rds' not found. Please run '04_run_calculations.R' first.")
}

sdid_estimate <- readRDS(sdid_estimate_path)
message("Successfully loaded sdid_estimate.rds")


# 3. ESTIMATE & RUN 2020 PLACEBO BOOTSTRAPS ==================================
message("--- Section 3: Estimating and Running 2020 Placebo ---")

# Estimate the 2020 placebo model (fraction = 2/8 = 0.25)
message("Estimating 2020 placebo model...")
sdid_placebo_2020_estimate <- synthdid_placebo(sdid_estimate, treated.fraction = 0.25)

# --- Placebo 2020 Model Bootstrap Calls ---
message("Calculating 2020 placebo model statistics (Bootstrap Call 1)...")
placebo_2020_se <- sqrt(vcov(sdid_placebo_2020_estimate, method = SE_METHOD)) # <<< CALL 1

message("Generating raw 2020 placebo plot (Bootstrap Call 2)...")
placebo_2020_plot_raw <- get_raw_sdid_plot(sdid_placebo_2020_estimate) # <<< CALL 2

message("Generating raw 2020 placebo overlay plot (Bootstrap Call 3)...")
placebo_2020_overlay_plot_raw <- get_raw_sdid_plot(sdid_placebo_2020_estimate, overlay = TRUE) # <<< CALL 3

message("Generating raw 2020 placebo units dot plot (Bootstrap Call 4)...")
placebo_2020_units_dot_plot_raw <- synthdid_units_plot(sdid_placebo_2020_estimate, se.method = SE_METHOD) # <<< CALL 4

message("--- All 2020 Placebo Calculations Finished ---")


# 4. SAVE RAW 2020 OBJECTS ===================================================
message(paste("--- Section 4: Saving all 2020 raw objects to:", DIR_OUT, "---"))

# --- Core Model Object ---
saveRDS(sdid_placebo_2020_estimate, file = file.path(DIR_OUT, "sdid_placebo_2020_estimate.rds"))

# --- Bootstrap Results (Stochastic) ---
saveRDS(placebo_2020_se, file = file.path(DIR_OUT, "placebo_2020_se.rds"))

# --- Raw Plot Objects (Stochastic) ---
saveRDS(placebo_2020_plot_raw, file = file.path(DIR_OUT, "placebo_2020_plot_raw.rds"))
saveRDS(placebo_2020_overlay_plot_raw, file = file.path(DIR_OUT, "placebo_2020_overlay_plot_raw.rds"))
saveRDS(placebo_2020_units_dot_plot_raw, file = file.path(DIR_OUT, "placebo_2020_units_dot_plot_raw.rds"))


# 5. SCRIPT COMPLETION =======================================================
message(paste("\n--- Script 04b_run_placebo_2020.R finished ---",
  "\nAll 2020 raw objects saved to:", DIR_OUT))
