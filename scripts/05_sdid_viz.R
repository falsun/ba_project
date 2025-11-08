# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       05_make_visuals.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-10-26
#   Description:  This script loads raw model objects and generates all final
#                 styled plots and tables for the project.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

DIR_DATA     <- here::here("_output", "_raw_objects")
DIR_SCRIPTS  <- here::here("scripts")
DIR_FIG      <- here::here("_output", "_figures")
DIR_TAB      <- here::here("_output", "_tables")

if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)
if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)

TREAT_YEAR   <- 2022
PLACEBO_YEAR <- 2019


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  here,
  synthdid,
  gt,
  gtsummary,
  patchwork
)

source(file.path(DIR_SCRIPTS, "00_functions.R"))

options(scipen = 999)


# 2. LOAD RAW OBJECTS ========================================================
message("--- Section 2: Loading All Raw Objects ---")

# --- Core Model Objects ---
sdid_estimate <- readRDS(file.path(DIR_DATA, "sdid_estimate.rds"))
sdid_placebo_estimate <- readRDS(file.path(DIR_DATA, "sdid_placebo_estimate.rds"))
sdid_data_formatted <- readRDS(file.path(DIR_DATA, "sdid_data_formatted.rds"))
setup <- readRDS(file.path(DIR_DATA, "setup.rds"))

# --- Bootstrap Results (Stochastic) ---
se <- readRDS(file.path(DIR_DATA, "se.rds"))
placebo_se <- readRDS(file.path(DIR_DATA, "placebo_se.rds"))

# --- Raw Plot Objects (Stochastic) ---
sdid_plot_raw <- readRDS(file.path(DIR_DATA, "sdid_plot_raw.rds"))
overlay_plot_raw <- readRDS(file.path(DIR_DATA, "overlay_plot_raw.rds"))
units_dot_plot_raw <- readRDS(file.path(DIR_DATA, "units_dot_plot_raw.rds"))
placebo_plot_raw <- readRDS(file.path(DIR_DATA, "placebo_plot_raw.rds"))
placebo_overlay_plot_raw <- readRDS(file.path(DIR_DATA, "placebo_overlay_plot_raw.rds"))
placebo_units_dot_plot_raw <- readRDS(file.path(DIR_DATA, "placebo_units_dot_plot_raw.rds"))


# 3. DETERMINISTIC EXTRACTION & CALCULATION ==================================
message("--- Section 3: Deterministic Data Extraction & Calculation ---")

# --- Main Model Stats ---
critical_value <- qnorm(0.975)
att <- as.numeric(sdid_estimate)
z_stat <- att / se
p_value <- 2 * pnorm(-abs(z_stat))
ci_lower <- att - critical_value * se
ci_upper <- att + critical_value * se

# --- Placebo Model Stats ---
placebo_att <- as.numeric(sdid_placebo_estimate)
placebo_z_stat <- placebo_att / placebo_se
placebo_p_value <- 2 * pnorm(-abs(placebo_z_stat))
placebo_ci_lower <- placebo_att - critical_value * placebo_se
placebo_ci_upper <- placebo_att + critical_value * placebo_se

# --- RMSE Data ---
sdid_rmse_plot <- synthdid_rmse_plot(sdid_estimate)
rmse_data <- sdid_rmse_plot$data
rmse <- rmse_data$rmse[which.max(rmse_data$iteration)]

# --- Placebo RMSE Data ---
sdid_placebo_rmse_plot <- synthdid_rmse_plot(sdid_placebo_estimate)
rmse_placebo_data <- sdid_placebo_rmse_plot$data
rmse_placebo <- rmse_placebo_data$rmse[which.max(rmse_placebo_data$iteration)]

# --- Weights Data ---
unit_weights_df <- synthdid_controls(sdid_estimate, mass = 1, weight.type = "omega") %>%
  as_tibble(rownames = "unit") %>%
  rename(weight = "estimate 1")

time_weights_df <- synthdid_controls(sdid_estimate, mass = 1.1, weight.type = "lambda") %>%
  as_tibble(rownames = "time") %>%
  rename(weight = "estimate 1")

# --- Placebo Weights Data ---
placebo_unit_weights_df <- synthdid_controls(sdid_placebo_estimate, mass = 1, weight.type = "omega") %>%
  as_tibble(rownames = "unit") %>%
  rename(weight = "estimate 1")

placebo_time_weights_df <- synthdid_controls(sdid_placebo_estimate, mass = 1.1, weight.type = "lambda") %>%
  as_tibble(rownames = "time") %>%
  rename(weight = "estimate 1")

# --- Yearly ATT Data ---
yearly_att <- synthdid_effect_curve(sdid_estimate)
all_years <- sort(unique(sdid_data_formatted$time))
post_treat_years <- all_years[(setup$T0 + 1):length(all_years)]
yearly_att_table_data <- tibble(
  Term = as.character(post_treat_years),
  Estimate = as.numeric(yearly_att)
)


# 4. MAIN MODEL: STYLING & SAVING ============================================
message("--- Section 4: Styling and Saving Main Model Plots ---")

# --- SDID Plot ---
message("Styling main SDID plot...")
diagram_plot <- style_sdid_plot(
  sdid_plot_raw,
  vline_year = TREAT_YEAR,
  plot_title = "SDID: Military Spending (% of GDP)"
)
ggsave(file.path(DIR_FIG, "sdid1_diagram.png"), diagram_plot, width = 8, height = 6)

# --- Overlay Plot ---
message("Styling overlay plot...")
sdid1_overlay_plot <- style_sdid_plot(
  overlay_plot_raw,
  vline_year = TREAT_YEAR,
  plot_title = "SDID: Military Spending (% of GDP) Overlay"
)
ggsave(file.path(DIR_FIG, "sdid1_overlay.png"), sdid1_overlay_plot, width = 8, height = 6)

# --- Units Dot Plot ---
message("Styling units dot plot...")
dot_plot <- units_dot_plot_raw +
  theme_bachelor_project() +
  labs(
    title = "SDID: Units Dot Plot",
    caption = "Dot size proportional to unit weight"
  ) +
  theme(strip.background = element_blank(), strip.text = element_blank())
ggsave(file.path(DIR_FIG, "sdid1_dot.png"), dot_plot, width = 8, height = 6)

# --- Unit & Time Weights Plots ---
message("Styling weights plots...")
unit_weights_plot <- ggplot(unit_weights_df, aes(x = unit, y = weight)) +
  geom_col(fill = "steelblue", alpha = 1) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_bachelor_project() +
  labs(x = NULL, y = NULL)

time_weights_plot <- ggplot(time_weights_df, aes(x = time, y = weight)) +
  geom_col(fill = "darkgreen", alpha = 1) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_bachelor_project() +
  labs(x = NULL, y = NULL)

weights_plot <- unit_weights_plot / time_weights_plot +
  plot_annotation(
    title = "SDID: Country & Time Weights",
    theme = theme_bachelor_project()
    )
ggsave(
  file.path(DIR_FIG, "sdid1_weights.png"),
  weights_plot,
  width = 7,
  height = 9
)

# --- RMSE Plot ---
ggsave(
  file.path(DIR_FIG, "sdid1_rmse.png"),
  sdid_rmse_plot + theme_bachelor_project() +
    theme(legend.position = "none"),
  width = 8, height = 6
)


# 5. PLACEBO: STYLING & SAVING ===============================================
message("--- Section 5: Styling and Saving Placebo Plots ---")

# --- Placebo SDID Plot ---
message("Styling placebo plots...")
placebo_diagram_plot <- style_sdid_plot(
  placebo_plot_raw,
  vline_year = PLACEBO_YEAR,
  plot_title = "SDID: In-Time Placebo Test (2019-2021)"
)
ggsave(file.path(DIR_FIG, "sdid1_pit_diagram.png"), placebo_diagram_plot, width = 8, height = 6)

# --- Placebo Overlay Plot ---
placebo_overlay_plot <- style_sdid_plot(
  placebo_overlay_plot_raw,
  vline_year = PLACEBO_YEAR,
  plot_title = "SDID: In-Time Placebo Test (2019-2021) Overlay"
)
ggsave(file.path(DIR_FIG, "sdid1_pit_overlay.png"), placebo_overlay_plot, width = 8, height = 6)

# --- Placebo Units Dot Plot ---
message("Styling units dot plot...")
placebo_dot_plot <- placebo_units_dot_plot_raw +
  theme_bachelor_project() +
  labs(
    title = "SDID: In-Time Placebo Units Dot Plot",
    caption = "Dot size proportional to unit weight"
  ) +
  theme(strip.background = element_blank(), strip.text = element_blank())
ggsave(file.path(DIR_FIG, "sdid1_pit_dot.png"), placebo_dot_plot, width = 8, height = 6)

# --- Unit & Time Weights Plots ---
message("Styling weights plots...")
placebo_unit_weights_plot <- ggplot(placebo_unit_weights_df, aes(x = unit, y = weight)) +
  geom_col(fill = "steelblue", alpha = 1) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_bachelor_project() +
  labs(x = NULL, y = NULL)

placebo_time_weights_plot <- ggplot(placebo_time_weights_df, aes(x = time, y = weight)) +
  geom_col(fill = "darkgreen", alpha = 1) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_bachelor_project() +
  labs(x = NULL, y = NULL)

placebo_weights_plot <- placebo_unit_weights_plot / placebo_time_weights_plot +
  plot_annotation(
    title = "SDID: In-Time Placebo Country & Time Weights",
    theme = theme_bachelor_project()
  )
ggsave(
  file.path(DIR_FIG, "sdid1_pit_weights.png"),
  placebo_weights_plot,
  width = 7,
  height = 9
)

# --- Placebo RMSE Plot ---
ggsave(
  file.path(DIR_FIG, "sdid1_pit_rmse.png"),
  sdid_placebo_rmse_plot + theme_bachelor_project() +
    theme(legend.position = "none"),
  width = 8, height = 6
)


# 6. EXPORT RESULTS TABLE ====================================================
message("--- Section 6: Exporting Results Table ---")

message("Generating SDID ATT table...")
combined_att_data <- tibble(
  Model = c("Main (2022-2024)", "Placebo In-Time (2019-2021)"),
  Estimate = c(att, placebo_att),
  Std.Error = c(se, placebo_se),
  ci.lower = c(ci_lower, placebo_ci_lower),
  ci.upper = c(ci_upper, placebo_ci_upper),
  p.value = c(p_value, placebo_p_value),
  RMSE = c(rmse, rmse_placebo)
)

sdid1_results <- combined_att_data %>%
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
  cols_merge(columns = c(ci.lower, ci.upper), pattern = "[{1}–{2}]") %>%
  fmt(
    columns = p.value,
    fns = function(x) gtsummary::style_pvalue(x, digits = 3)
  ) %>%
  tab_header(title = "Table 5: Synthetic Difference-in-Differences Model Results") %>%
  tab_footnote(
    footnote = "Average treatment effect on the treated",
    locations = cells_column_labels(columns = Estimate)
  ) %>%
  tab_footnote(
    footnote = "Root mean squared error",
    locations = cells_column_labels(columns = RMSE)
  ) %>%
  tab_source_note("Note: Standard errors, 95% confidence intervals, and p-values computed via robust bootstrap method.") %>%
  theme_gt_bachelor_project()

gtsave(
  sdid1_results,
  file = file.path(DIR_TAB, "sdid1_results.png")
)


# 7. YEARLY ATT TABLE ========================================================
message("--- Section 7: Generating Yearly ATT Table ---")

sdid1_yearly_att <- yearly_att_table_data %>%
  gt() %>%
  cols_label(Term = "Year", Estimate = "ATT") %>%
  fmt_number(columns = Estimate, decimals = 3) %>%
  tab_header(title = "Table 6: Yearly Average Treatment Effect (ATT) Estimates") %>%
  tab_source_note("Note: These are point estimates.") %>%
  theme_gt_bachelor_project()

gtsave(
  sdid1_yearly_att,
  file = file.path(DIR_TAB, "sdid1_yearly_att.png")
)


# 8. SCRIPT COMPLETION =======================================================
message(paste("\n--- Script 05_make_visuals.R finished ---",
              "\nAll output (tables and figures) saved to:", here::here("_output")))
