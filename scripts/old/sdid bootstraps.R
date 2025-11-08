# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       04_run_calculations.R (MODIFIED)
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-10-26
#   Description:  This script runs all time-consuming and random calculations
#                 and saves the raw results to disk.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

set.seed(1) # <<< CRITICAL: The "golden seed" lives here

DIR_DATA        <- here::here("data", "_processed")
DIR_SCRIPTS     <- here::here("scripts")
DIR_OUTPUT_RAW  <- here::here("_output", "_raw_objects") # <-- New save location
DIR_FIG         <- here::here("_output", "_figures")
DIR_TAB         <- here::here("_output", "_tables")

# Create all directories
if (!dir.exists(DIR_OUTPUT_RAW)) dir.create(DIR_OUTPUT_RAW, recursive = TRUE)
if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

MASTER_PANEL_LOG <- file.path(DIR_DATA, "master_panel_log.rds")

TREATMENT_YEAR    <- 2022
PLACEBO_YEAR      <- 2019
SE_METHOD         <- "bootstrap"


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # Data manipulation
  here,         # File path management
  synthdid,     # SDID model
  gt,           # Nice tables
  gtsummary,    # Table helpers
  patchwork     # Combine plots
)

source(file.path(DIR_SCRIPTS, "00_functions.R"))

options(scipen = 999)


# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

master_panel_log <- readRDS(MASTER_PANEL_LOG)

sdid_data_formatted <- master_panel_log %>%
  filter(group %in% c("control", "treatment")) %>%
  mutate(treated = (group == "treatment" & post_treat == 1)) %>%
  rename(
    unit = iso3c,
    time = year,
    outcome = milex_gdp
  ) %>%
  select(unit, time, outcome, treated) %>%
  as.data.frame()

setup <- panel.matrices(sdid_data_formatted)

# 3. MAIN MODEL: ESTIMATION & STATISTICS =====================================
message("--- Section 3: Estimating Main SDID Model ---")

sdid_estimate <- synthdid_estimate(setup$Y, setup$N0, setup$T0)

message("Calculating main model statistics (Bootstrap Call 1)...")
se <- sqrt(vcov(sdid_estimate, method = SE_METHOD)) # <<< CALL 1

# --- All other stats are deterministic based on Call 1 ---
estimate <- as.numeric(sdid_estimate)
z_stat <- estimate / se
p_value <- 2 * pnorm(-abs(z_stat))
critical_value <- qnorm(0.975)
ci_lower <- estimate - critical_value * se
ci_upper <- estimate + critical_value * se
sdid_rmse_plot_raw <- synthdid_rmse_plot(sdid_estimate) # Raw RMSE plot
rmse_data <- sdid_rmse_plot_raw$data
rmse <- rmse_data$rmse[which.max(rmse_data$iteration)]


# 4. MAIN MODEL: RAW VISUALIZATIONS ==========================================
message("--- Section 4: Calculating Raw Visualizations ---")

# --- 4.1 Helper Function for RAW Plotting ---
# This function ONLY runs the plot() command to get the bootstrap CIs.
# All styling is removed.
get_raw_sdid_plot <- function(estimate_obj, overlay = FALSE) {

  p_raw <- plot(estimate_obj,
                overlay = as.numeric(overlay),
                se.method = SE_METHOD) # <<< BOOTSTRAP CALL HAPPENS HERE

  return(p_raw)
}

# --- 4.2 Main SDID Plot ---
message("Generating raw main SDID plot (Bootstrap Call 2)...")
sdid_plot_raw <- get_raw_sdid_plot(sdid_estimate) # <<< CALL 2

# --- 4.3 Overlay Plot ---
message("Generating raw overlay plot (Bootstrap Call 3)...")
overlay_plot_raw <- get_raw_sdid_plot(sdid_estimate, overlay = TRUE) # <<< CALL 3

# --- 4.4 Units Dot Plot ---
message("Generating raw units dot plot (Bootstrap Call 4)...")
units_dot_plot_raw <- synthdid_units_plot(sdid_estimate, se.method = SE_METHOD) # <<< CALL 4

# --- 4.5 Unit & Time Weights Plots (Deterministic) ---
message("Generating raw weights plots...")
unit_weights_plot_raw <- synthdid_controls(sdid_estimate, mass = 1, weight.type = "omega") %>%
  as_tibble(rownames = "unit") %>%
  rename(weight = "estimate 1") %>%
  ggplot(aes(x = unit, y = weight)) +
  geom_col(fill = "steelblue", alpha = 1)

time_weights_plot_raw <- synthdid_controls(sdid_estimate, mass = 1.1, weight.type = "lambda") %>%
  as_tibble(rownames = "time") %>%
  rename(weight = "estimate 1") %>%
  ggplot(aes(x = time, y = weight)) +
  geom_col(fill = "darkgreen", alpha = 1)


# 5. ROBUSTNESS CHECK: IN-TIME PLACEBO =======================================
message("--- Section 5: Estimating In-Time Placebo Model ---")

sdid_placebo_estimate <- synthdid_placebo(sdid_estimate)

message("Calculating placebo model statistics (Bootstrap Call 5)...")
placebo_att <- as.numeric(sdid_placebo_estimate)
placebo_se <- sqrt(vcov(sdid_placebo_estimate, method = SE_METHOD)) # <<< CALL 5

if (placebo_se == 0) {
  placebo_z_stat <- Inf
  placebo_p_value <- 0
} else {
  placebo_z_stat <- placebo_att / placebo_se
  placebo_p_value <- 2 * pnorm(-abs(placebo_z_stat))
}
placebo_ci_lower <- placebo_att - critical_value * placebo_se
placebo_ci_upper <- placebo_att + critical_value * placebo_se
sdid_placebo_rmse_plot_raw <- synthdid_rmse_plot(sdid_placebo_estimate) # Raw RMSE plot
rmse_placebo_data <- sdid_placebo_rmse_plot_raw$data
rmse_placebo <- rmse_placebo_data$rmse[which.max(rmse_placebo_data$iteration)]

# --- 5.1. Placebo SDID Plot ---
message("Generating raw placebo plot (Bootstrap Call 6)...")
placebo_plot_raw <- get_raw_sdid_plot(sdid_placebo_estimate) # <<< CALL 6

# --- 5.2. Placebo Overlay Plot ---
message("Generating raw placebo overlay plot (Bootstrap Call 7)...")
placebo_overlay_plot_raw <- get_raw_sdid_plot(sdid_placebo_estimate, overlay = TRUE) # <<< CALL 7


# 6. DATA EXPORT PREPARATION =================================================
message("--- Section 6: Preparing Data for Tables ---")

combined_att_data <- tibble(
  Model = c("Main (2022-2024)", "Placebo In-Time (2019-2021)"),
  Estimate = c(estimate, placebo_att),
  Std.Error = c(se, placebo_se),
  ci.lower = c(ci_lower, placebo_ci_lower),
  ci.upper = c(ci_upper, placebo_ci_upper),
  p.value = c(p_value, placebo_p_value),
  RMSE = c(rmse, rmse_placebo)
)

yearly_att_estimates <- synthdid_effect_curve(sdid_estimate)
all_years <- sort(unique(sdid_data_formatted$time))
post_treatment_years <- all_years[(setup$T0 + 1):length(all_years)]
yearly_att_table_data <- tibble(
  Term = as.character(post_treatment_years),
  Estimate = as.numeric(yearly_att_estimates)
)


# 7. SAVE ALL RAW OBJECTS ====================================================
message(paste("--- Section 7: Saving all raw objects to:", DIR_OUTPUT_RAW, "---"))

# --- Data Objects ---
saveRDS(combined_att_data, file = file.path(DIR_OUTPUT_RAW, "combined_att_data.rds"))
saveRDS(yearly_att_table_data, file = file.path(DIR_OUTPUT_RAW, "yearly_att_table_data.rds"))
saveRDS(sdid_data_formatted, file = file.path(DIR_OUTPUT_RAW, "sdid_data_formatted.rds"))

# --- Raw Plot Objects ---
saveRDS(sdid_plot_raw, file = file.path(DIR_OUTPUT_RAW, "sdid_plot_raw.rds"))
saveRDS(overlay_plot_raw, file = file.path(DIR_OUTPUT_RAW, "overlay_plot_raw.rds"))
saveRDS(units_dot_plot_raw, file = file.path(DIR_OUTPUT_RAW, "units_dot_plot_raw.rds"))
saveRDS(unit_weights_plot_raw, file = file.path(DIR_OUTPUT_RAW, "unit_weights_plot_raw.rds"))
saveRDS(time_weights_plot_raw, file = file.path(DIR_OUTPUT_RAW, "time_weights_plot_raw.rds"))
saveRDS(sdid_rmse_plot_raw, file = file.path(DIR_OUTPUT_RAW, "sdid_rmse_plot_raw.rds"))
saveRDS(placebo_plot_raw, file = file.path(DIR_OUTPUT_RAW, "placebo_plot_raw.rds"))
saveRDS(placebo_overlay_plot_raw, file = file.path(DIR_OUTPUT_RAW, "placebo_overlay_plot_raw.rds"))
saveRDS(sdid_placebo_rmse_plot_raw, file = file.path(DIR_OUTPUT_RAW, "sdid_placebo_rmse_plot_raw.rds"))

# 8. SCRIPT COMPLETION =======================================================
message(paste("\n--- Script 04_run_calculations.R finished ---",
              "\nAll raw objects saved."))
