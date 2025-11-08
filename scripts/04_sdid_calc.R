# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       04_run_calculations.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-10-26
#   Description:  This script runs ONLY the stochastic (random) calculations
#                 and saves the core model objects and bootstrap results.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

set.seed(1)

DIR_DATA     <- here::here("data", "_processed")
DIR_SCRIPTS  <- here::here("scripts")
DIR_OUT      <- here::here("_output", "_raw_objects")

if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)

MASTER_PANEL <- file.path(DIR_DATA, "master_panel_log.rds")

SE_METHOD    <- "bootstrap"


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  here,
  synthdid
)

source(file.path(DIR_SCRIPTS, "00_functions.R"))


# 2. PREPARE DATA & ESTIMATE MODELS ==========================================
message("--- Section 2: Loading Data and Estimating Models ---")

master_panel <- readRDS(MASTER_PANEL)
sdid_data_formatted <- master_panel %>%
  filter(group %in% c("control", "treatment")) %>%
  mutate(treated = (group == "treatment" & post_treat == 1)) %>%
  rename(unit = iso3c, time = year, outcome = milex_gdp) %>%
  select(unit, time, outcome, treated) %>%
  as.data.frame()

setup <- panel.matrices(sdid_data_formatted)

sdid_estimate <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
sdid_placebo_estimate <- synthdid_placebo(sdid_estimate)


# 3. RUN ALL STOCHASTIC (BOOTSTRAP) CALCULATIONS =============================
message("--- Section 3: Running All 7 Bootstrap Calculations ---")

# --- Main Model Bootstrap Calls ---
message("Running main model statistics (Bootstrap Call 1)...")
se <- sqrt(vcov(sdid_estimate, method = SE_METHOD)) # <<< CALL 1

message("Generating raw main SDID plot (Bootstrap Call 2)...")
sdid_plot_raw <- get_raw_sdid_plot(sdid_estimate) # <<< CALL 2

message("Generating raw overlay plot (Bootstrap Call 3)...")
overlay_plot_raw <- get_raw_sdid_plot(sdid_estimate, overlay = TRUE) # <<< CALL 3

message("Generating raw units dot plot (Bootstrap Call 4)...")
units_dot_plot_raw <- synthdid_units_plot(sdid_estimate, se.method = SE_METHOD) # <<< CALL 4

# --- Placebo Model Bootstrap Calls ---
message("Calculating placebo model statistics (Bootstrap Call 5)...")
placebo_se <- sqrt(vcov(sdid_placebo_estimate, method = SE_METHOD)) # <<< CALL 5

message("Generating raw placebo plot (Bootstrap Call 6)...")
placebo_plot_raw <- get_raw_sdid_plot(sdid_placebo_estimate) # <<< CALL 6

message("Generating raw placebo overlay plot (Bootstrap Call 7)...")
placebo_overlay_plot_raw <- get_raw_sdid_plot(sdid_placebo_estimate, overlay = TRUE) # <<< CALL 7

message("Generating raw placebo units dot plot (Bootstrap Call 8)...")
placebo_units_dot_plot_raw <- synthdid_units_plot(sdid_placebo_estimate, se.method = SE_METHOD) # <<< CALL 8


# 4. SAVE RAW OBJECTS ========================================================
message(paste("--- Section 4: Saving all raw objects to:", DIR_OUT, "---"))

# --- Core Model Objects (for deterministic extraction in Script 2) ---
saveRDS(sdid_estimate, file = file.path(DIR_OUT, "sdid_estimate.rds"))
saveRDS(sdid_placebo_estimate, file = file.path(DIR_OUT, "sdid_placebo_estimate.rds"))
saveRDS(sdid_data_formatted, file = file.path(DIR_OUT, "sdid_data_formatted.rds"))
saveRDS(setup, file = file.path(DIR_OUT, "setup.rds"))

# --- Bootstrap Results (Stochastic) ---
saveRDS(se, file = file.path(DIR_OUT, "se.rds"))
saveRDS(placebo_se, file = file.path(DIR_OUT, "placebo_se.rds"))

# --- Raw Plot Objects (Stochastic) ---
saveRDS(sdid_plot_raw, file = file.path(DIR_OUT, "sdid_plot_raw.rds"))
saveRDS(overlay_plot_raw, file = file.path(DIR_OUT, "overlay_plot_raw.rds"))
saveRDS(units_dot_plot_raw, file = file.path(DIR_OUT, "units_dot_plot_raw.rds"))
saveRDS(placebo_plot_raw, file = file.path(DIR_OUT, "placebo_plot_raw.rds"))
saveRDS(placebo_overlay_plot_raw, file = file.path(DIR_OUT, "placebo_overlay_plot_raw.rds"))
saveRDS(placebo_units_dot_plot_raw, file = file.path(DIR_OUT, "placebo_units_dot_plot_raw.rds"))

# 5. SCRIPT COMPLETION =======================================================
message(paste("\n--- Script 04_run_calculations.R finished ---",
              "\nAll raw objects and bootstrap results saved to:", DIR_OUT))
