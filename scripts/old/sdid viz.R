# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       05_make_visuals.R (NEW)
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-10-26
#   Description:  This script loads raw results and generates all final
#                 styled plots and tables for the project.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

# <<< NO set.seed(1) HERE! >>>

DIR_SCRIPTS     <- here::here("scripts")
DIR_OUTPUT_RAW  <- here::here("_output", "_raw_objects") # <-- Load from here
DIR_FIG         <- here::here("_output", "_figures")
DIR_TAB         <- here::here("_output", "_tables")

TREATMENT_YEAR    <- 2022
PLACEBO_YEAR      <- 2019
SE_METHOD         <- "bootstrap" # Still useful for table notes


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


# 2. LOAD RAW OBJECTS ========================================================
message("--- Section 2: Loading All Raw Objects ---")

# --- Data Objects ---
combined_att_data <- readRDS(file.path(DIR_OUTPUT_RAW, "combined_att_data.rds"))
yearly_att_table_data <- readRDS(file.path(DIR_OUTPUT_RAW, "yearly_att_table_data.rds"))

# --- Raw Plot Objects ---
sdid_plot_raw <- readRDS(file.path(DIR_OUTPUT_RAW, "sdid_plot_raw.rds"))
overlay_plot_raw <- readRDS(file.path(DIR_OUTPUT_RAW, "overlay_plot_raw.rds"))
units_dot_plot_raw <- readRDS(file.path(DIR_OUTPUT_RAW, "units_dot_plot_raw.rds"))
unit_weights_plot_raw <- readRDS(file.path(DIR_OUTPUT_RAW, "unit_weights_plot_raw.rds"))
time_weights_plot_raw <- readRDS(file.path(DIR_OUTPUT_RAW, "time_weights_plot_raw.rds"))
sdid_rmse_plot_raw <- readRDS(file.path(DIR_OUTPUT_RAW, "sdid_rmse_plot_raw.rds"))
placebo_plot_raw <- readRDS(file.path(DIR_OUTPUT_RAW, "placebo_plot_raw.rds"))
placebo_overlay_plot_raw <- readRDS(file.path(DIR_OUTPUT_RAW, "placebo_overlay_plot_raw.rds"))
sdid_placebo_rmse_plot_raw <- readRDS(file.path(DIR_OUTPUT_RAW, "sdid_placebo_rmse_plot_raw.rds"))


# 3. MAIN MODEL: STYLING & SAVING ============================================
message("--- Section 3: Styling and Saving Main Model Plots ---")

# --- 3.1 Helper Function for STYLING ---
# This function applies all your bachelor project themes
style_sdid_plot <- function(raw_plot, vline_year, plot_title) {

  p_styled <- raw_plot +
    geom_vline(xintercept = vline_year - 1, linetype = "dashed", color = "grey40") +
    scale_x_continuous(breaks = seq(2014, 2024, by = 2), limits = c(2014, 2024)) +
    scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.01)) +
    scale_color_project_qual(name = NULL) +
    theme_bachelor_project() +
    labs(x = NULL, y = NULL, title = plot_title)

  # Remove the original plot() legend (layer 7)
  p_styled$layers[[7]] <- NULL

  return(p_styled)
}

# --- 3.2 Main SDID Plot ---
message("Styling main SDID plot...")
sdid_plot <- style_sdid_plot(
  sdid_plot_raw,
  vline_year = TREATMENT_YEAR,
  plot_title = "SDID: Military Spending (% of GDP)"
)
ggsave(file.path(DIR_FIG, "sdid_main_plot.png"), sdid_plot, width = 8, height = 6)

# --- 3.3 Overlay Plot ---
message("Styling overlay plot...")
overlay_plot <- style_sdid_plot(
  overlay_plot_raw,
  vline_year = TREATMENT_YEAR,
  plot_title = "SDID: Military Spending (% of GDP)"
)
ggsave(file.path(DIR_FIG, "sdid_overlay_plot.png"), overlay_plot, width = 8, height = 6)

# --- 3.4 Units Dot Plot ---
message("Styling units dot plot...")
units_dot_plot <- units_dot_plot_raw +
  theme_bachelor_project() +
  labs(
    title = "SDID: Units Dot Plot",
    caption = "Dot size proportional to unit weight"
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank()
  )
ggsave(file.path(DIR_FIG, "sdid_units_dot_plot.png"), units_dot_plot, width = 8, height = 6)

# --- 3.5 Unit & Time Weights Plots ---
message("Styling weights plots...")
unit_weights_plot <- unit_weights_plot_raw +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_bachelor_project() +
  labs(x = NULL, y = NULL)

time_weights_plot <- time_weights_plot_raw +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_bachelor_project() +
  labs(x = NULL, y = NULL)

combined_weights_plot <- unit_weights_plot / time_weights_plot
ggsave(
  file.path(DIR_FIG, "sdid_combined_weights_plot.png"),
  combined_weights_plot,
  width = 7,
  height = 9
)

# --- 3.6 RMSE Plot ---
ggsave(
  file.path(DIR_FIG, "sdid_rmse_plot.png"),
  sdid_rmse_plot_raw + theme_bachelor_project(),
  width = 8, height = 6
)


# 4. PLACEBO: STYLING & SAVING ===============================================
message("--- Section 4: Styling and Saving Placebo Plots ---")

# --- 4.1. Placebo SDID Plot ---
message("Styling placebo plots...")
placebo_plot <- style_sdid_plot(
  placebo_plot_raw,
  vline_year = PLACEBO_YEAR,
  plot_title = "SDID: In-Time Placebo Test (2019-2021)"
)
ggsave(file.path(DIR_FIG, "sdid_placebo_plot.png"), placebo_plot, width = 8, height = 6)

# --- 4.2. Placebo Overlay Plot ---
placebo_overlay_plot <- style_sdid_plot(
  placebo_overlay_plot_raw,
  vline_year = PLACEBO_YEAR,
  plot_title = "SDID: In-Time Placebo Test (2019-2021)"
)
ggsave(file.path(DIR_FIG, "sdid_placebo_overlay_plot.png"), placebo_overlay_plot, width = 8, height = 6)

# --- 4.3. Placebo RMSE Plot ---
ggsave(
  file.path(DIR_FIG, "sdid_placebo_rmse_plot.png"),
  sdid_placebo_rmse_plot_raw + theme_bachelor_project(),
  width = 8, height = 6
)


# 5. EXPORT RESULTS TABLE ====================================================
message("--- Section 5: Exporting Results Table ---")

message("Generating SDID ATT table...")
tbl_combined_results <- combined_att_data %>%
  gt() %>%
  cols_label(
    Model = "Model",
    Estimate = "ATT",
    Std.Error = "Std. Error",
    ci.lower = "95% CI",
    p.value = "P-Value",
    RMSE = "RMSE"
  ) %>%
  fmt_number(columns = c(Estimate, Std.Error, ci.lower, ci.upper, RMSE), decimals = 3) %>%
  cols_merge(
    columns = c(ci.lower, ci.upper),
    pattern = "[{1}–{2}]"
  ) %>%
  fmt(
    columns = p.value,
    fns = function(x) gtsummary::style_pvalue(x, digits = 3)
  ) %>%
  tab_header(
    title = "Table 5: SDID Model Results"
  ) %>%
  tab_source_note(paste("Note: Standard errors and p-values computed via robust", SE_METHOD, "method."))

gtsave(
  tbl_combined_results,
  file = file.path(DIR_TAB, "table_5_sdid_model.png")
)


# 6. YEARLY ATT TABLE ========================================================
message("--- Section 6: Generating Yearly ATT Table ---")

tbl_yearly_att <- yearly_att_table_data %>%
  gt() %>%
  cols_label(Term = "Year", Estimate = "ATT") %>%
  fmt_number(columns = Estimate, decimals = 3) %>%
  tab_header(
    title = "Table 6: Yearly Average Treatment Effect (ATT) Estimates"
  ) %>%
  tab_source_note("Note: These are point estimates.")

gtsave(
  tbl_yearly_att,
  file = file.path(DIR_TAB, "table_6_sdid_model_yearly_att.png")
)


# 7. SCRIPT COMPLETION =======================================================
message(paste("\n--- Script 05_make_visuals.R finished ---",
              "\nAll output (tables and figures) saved to:", here::here("_output")))
