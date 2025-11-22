# ---------------------------------------------------------------------------- #
#
#   Project:     NATO Defence Spending Bachelor's Thesis
#   Script:      05_sensitivity_PLOT.R
#   Author:      Frederik Bender Bøeck-Nielsen
#   Date:        2025-11-12
#   Description: This script loads all pre-calculated sensitivity
#                results (Relative Magnitudes & Smoothness)
#                and creates the final plots.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

DIR_SCRIPTS <- here::here("scripts")
DIR_OUTPUT <- here::here("_output", "_tables", "_robustness")
DIR_FIG <- here::here("_output", "_figures", "_robustness")

# --- Input Files ---
RESULTS_FILE_RELMAG <- file.path(DIR_OUTPUT, "sensitivity_results_rm.rds")
RESULTS_FILE_SMOOTH <- file.path(DIR_OUTPUT, "sensitivity_results_smooth.rds")

# --- Output Files ---
PLOT_FILENAME_RELMAG <- file.path(DIR_FIG, "es_sensitivity_plot_milex_cap_relmag.png")
PLOT_FILENAME_SMOOTH <- file.path(DIR_FIG, "es_sensitivity_plot_milex_cap_smooth.png")

VAR_LABEL <- "Milex per Capita"

# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, glue, conflicted)
conflict_prefer("filter", "dplyr")

# Check if custom functions exist before sourcing
if (file.exists(file.path(DIR_SCRIPTS, "00_functions.R"))) {
  source(file.path(DIR_SCRIPTS, "00_functions.R"))
} else {
  message("NOTE: 00_functions.R not found. Using default ggplot theme.")
  # Define placeholders if functions are missing, to avoid errors
  theme_bachelor_project <- function() theme_bw()
  project_colors <- list(
    low = "#0072B2", # Blue
    control = "#E69F00", # Orange
    high = "#D55E00" # Vermillion
  )
}

# 2. LOAD PRE-CALCULATED DATA ================================================
message("--- Section 2: Loading Pre-Calculated Data ---")

# --- Load Relative Magnitude Data ---
if (!file.exists(RESULTS_FILE_RELMAG)) {
  stop(paste(
    "ERROR: Results file not found at", RESULTS_FILE_RELMAG,
    "\nPlease run 05_sensitivity_CALC.R first."
  ))
}
plot_data_relmag <- readRDS(RESULTS_FILE_RELMAG)

# --- Load Smoothness Restriction Data ---
if (!file.exists(RESULTS_FILE_SMOOTH)) {
  stop(paste(
    "ERROR: Results file not found at", RESULTS_FILE_SMOOTH,
    "\nPlease run 05_sensitivity_CALC_smoothness.R first."
  ))
}
plot_data_smooth <- readRDS(RESULTS_FILE_SMOOTH)


# 3. PLOT 1: RELATIVE MAGNITUDES ==============================================
message("--- Section 3: Generating Plot (Relative Magnitudes) ---")

plot_relmag <- ggplot(
  plot_data_relmag,
  aes(
    x = Mbar, # Sensitivity parameter (M-bar)
    ymin = lb,
    ymax = ub,
    color = Year
  )
) +
  geom_errorbar(
    width = 0.1,
    linewidth = 1,
    position = position_dodge(width = 0.1)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bachelor_project() +
  scale_color_manual(values = c(
    "2022" = project_colors$low,
    "2023" = project_colors$control,
    "2024" = project_colors$high
  )) +
  labs(
    title = glue("HonestDiD Sensitivity Analysis: {VAR_LABEL}"),
    subtitle = "Relative Magnitudes Test (Rambachan & Roth, 2022b)",
    x = "Sensitivity Parameter (M-bar)",
    y = "ATT (95% Robust CI)"
  ) +
  scale_x_continuous(breaks = unique(plot_data_relmag$Mbar)) +
  theme(legend.position = "bottom")

print(plot_relmag)

# --- Save the plot ---
ggsave(PLOT_FILENAME_RELMAG, plot_relmag, width = 8, height = 6, bg = "white")
message(paste("Relative Magnitudes plot saved to:", PLOT_FILENAME_RELMAG))


# 4. PLOT 2: SMOOTHNESS RESTRICTIONS ==========================================
message("--- Section 4: Generating Plot (Smoothness Restrictions) ---")

plot_smooth <- ggplot(
  plot_data_smooth,
  aes(
    x = M, # Sensitivity parameter (M)
    ymin = lb,
    ymax = ub,
    color = Year
  )
) +
  geom_errorbar(
    width = 3,
    linewidth = 1,
    position = position_dodge(width = 5)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bachelor_project() +
  scale_color_manual(values = c(
    "2022" = project_colors$low,
    "2023" = project_colors$control,
    "2024" = project_colors$high
  )) +
  labs(
    title = glue("HonestDiD Sensitivity Analysis: {VAR_LABEL}"),
    subtitle = "Smoothness Restrictions Test (Rambachan & Roth, 2022b)",
    x = "Sensitivity Parameter (M)",
    y = "ATT (95% Robust CI)"
  ) +
  scale_x_continuous(breaks = unique(plot_data_smooth$M)) +
  theme(legend.position = "bottom")

print(plot_smooth)

# --- Save the plot ---
ggsave(PLOT_FILENAME_SMOOTH, plot_smooth, width = 8, height = 6, bg = "white")
message(paste("Smoothness Restrictions plot saved to:", PLOT_FILENAME_SMOOTH))


# 5. SCRIPT COMPLETION =======================================================
message(paste(
  "\n--- Script 05_sensitivity_PLOT.R finished ---",
  "\nAll sensitivity plots have been saved."
))
