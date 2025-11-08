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
PLACEBO_YEAR_2018 <- 2018 # <<< NEW
PLACEBO_YEAR_2020 <- 2020 # <<< NEW


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

# <<< NEW: Load Placebo 2018 Objects ---
sdid_placebo_2018_estimate <- readRDS(file.path(DIR_DATA, "sdid_placebo_2018_estimate.rds"))
placebo_2018_se <- readRDS(file.path(DIR_DATA, "placebo_2018_se.rds"))
placebo_2018_plot_raw <- readRDS(file.path(DIR_DATA, "placebo_2018_plot_raw.rds"))
placebo_2018_overlay_plot_raw <- readRDS(file.path(DIR_DATA, "placebo_2018_overlay_plot_raw.rds"))
placebo_2018_units_dot_plot_raw <- readRDS(file.path(DIR_DATA, "placebo_2018_units_dot_plot_raw.rds"))

# <<< NEW: Load Placebo 2020 Objects ---
sdid_placebo_2020_estimate <- readRDS(file.path(DIR_DATA, "sdid_placebo_2020_estimate.rds"))
placebo_2020_se <- readRDS(file.path(DIR_DATA, "placebo_2020_se.rds"))
placebo_2020_plot_raw <- readRDS(file.path(DIR_DATA, "placebo_2020_plot_raw.rds"))
placebo_2020_overlay_plot_raw <- readRDS(file.path(DIR_DATA, "placebo_2020_overlay_plot_raw.rds"))
placebo_2020_units_dot_plot_raw <- readRDS(file.path(DIR_DATA, "placebo_2020_units_dot_plot_raw.rds"))


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

# <<< NEW: Placebo 2018 Model Stats ---
placebo_2018_att <- as.numeric(sdid_placebo_2018_estimate)
placebo_2018_z_stat <- placebo_2018_att / placebo_2018_se
placebo_2018_p_value <- 2 * pnorm(-abs(placebo_2018_z_stat))
placebo_2018_ci_lower <- placebo_2018_att - critical_value * placebo_2018_se
placebo_2018_ci_upper <- placebo_2018_att + critical_value * placebo_2018_se

# <<< NEW: Placebo 2020 Model Stats ---
placebo_2020_att <- as.numeric(sdid_placebo_2020_estimate)
placebo_2020_z_stat <- placebo_2020_att / placebo_2020_se
placebo_2020_p_value <- 2 * pnorm(-abs(placebo_2020_z_stat))
placebo_2020_ci_lower <- placebo_2020_att - critical_value * placebo_2020_se
placebo_2020_ci_upper <- placebo_2020_att + critical_value * placebo_2020_se

# --- RMSE Data ---
sdid_rmse_plot <- synthdid_rmse_plot(sdid_estimate)
rmse_data <- sdid_rmse_plot$data
rmse <- rmse_data$rmse[which.max(rmse_data$iteration)]

# --- Placebo RMSE Data ---
sdid_placebo_rmse_plot <- synthdid_rmse_plot(sdid_placebo_estimate)
rmse_placebo_data <- sdid_placebo_rmse_plot$data
rmse_placebo <- rmse_placebo_data$rmse[which.max(rmse_placebo_data$iteration)]

# <<< NEW: Placebo 2018 RMSE Data ---
sdid_placebo_2018_rmse_plot <- synthdid_rmse_plot(sdid_placebo_2018_estimate)
rmse_placebo_2018_data <- sdid_placebo_2018_rmse_plot$data
rmse_placebo_2018 <- rmse_placebo_2018_data$rmse[which.max(rmse_placebo_2018_data$iteration)]

# <<< NEW: Placebo 2020 RMSE Data ---
sdid_placebo_2020_rmse_plot <- synthdid_rmse_plot(sdid_placebo_2020_estimate)
rmse_placebo_2020_data <- sdid_placebo_2020_rmse_plot$data
rmse_placebo_2020 <- rmse_placebo_2020_data$rmse[which.max(rmse_placebo_2020_data$iteration)]

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

# <<< NEW: Placebo 2018 Weights Data ---
placebo_2018_unit_weights_df <- synthdid_controls(sdid_placebo_2018_estimate, mass = 1, weight.type = "omega") %>%
  as_tibble(rownames = "unit") %>%
  rename(weight = "estimate 1")

placebo_2018_time_weights_df <- synthdid_controls(sdid_placebo_2018_estimate, mass = 1.1, weight.type = "lambda") %>%
  as_tibble(rownames = "time") %>%
  rename(weight = "estimate 1")

# <<< NEW: Placebo 2020 Weights Data ---
placebo_2020_unit_weights_df <- synthdid_controls(sdid_placebo_2020_estimate, mass = 1, weight.type = "omega") %>%
  as_tibble(rownames = "unit") %>%
  rename(weight = "estimate 1")

placebo_2020_time_weights_df <- synthdid_controls(sdid_placebo_2020_estimate, mass = 1.1, weight.type = "lambda") %>%
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


# <<< NEW: SECTION 6: PLACEBO (2018) STYLING & SAVING ========================
message("--- Section 6: Styling and Saving Placebo (2018) Plots ---")

# --- Placebo 2018 SDID Plot ---
message("Styling placebo 2018 plots...")
placebo_2018_diagram_plot <- style_sdid_plot(
  placebo_2018_plot_raw,
  vline_year = PLACEBO_YEAR_2018,
  plot_title = "SDID: In-Time Placebo Test (2018-2021)"
)
ggsave(file.path(DIR_FIG, "sdid1_pit_2018_diagram.png"), placebo_2018_diagram_plot, width = 8, height = 6)

# --- Placebo 2018 Overlay Plot ---
placebo_2018_overlay_plot <- style_sdid_plot(
  placebo_2018_overlay_plot_raw,
  vline_year = PLACEBO_YEAR_2018,
  plot_title = "SDID: In-Time Placebo Test (2018-2021) Overlay"
)
ggsave(file.path(DIR_FIG, "sdid1_pit_2018_overlay.png"), placebo_2018_overlay_plot, width = 8, height = 6)

# --- Placebo 2018 Units Dot Plot ---
message("Styling 2018 units dot plot...")
placebo_2018_dot_plot <- placebo_2018_units_dot_plot_raw +
  theme_bachelor_project() +
  labs(
    title = "SDID: In-Time Placebo (2018) Units Dot Plot",
    caption = "Dot size proportional to unit weight"
  ) +
  theme(strip.background = element_blank(), strip.text = element_blank())
ggsave(file.path(DIR_FIG, "sdid1_pit_2018_dot.png"), placebo_2018_dot_plot, width = 8, height = 6)

# --- Unit & Time Weights Plots ---
message("Styling 2018 weights plots...")
placebo_2018_unit_weights_plot <- ggplot(placebo_2018_unit_weights_df, aes(x = unit, y = weight)) +
  geom_col(fill = "steelblue", alpha = 1) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_bachelor_project() +
  labs(x = NULL, y = NULL)

placebo_2018_time_weights_plot <- ggplot(placebo_2018_time_weights_df, aes(x = time, y = weight)) +
  geom_col(fill = "darkgreen", alpha = 1) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_bachelor_project() +
  labs(x = NULL, y = NULL)

placebo_2018_weights_plot <- placebo_2018_unit_weights_plot / placebo_2018_time_weights_plot +
  plot_annotation(
    title = "SDID: In-Time Placebo (2018) Country & Time Weights",
    theme = theme_bachelor_project()
  )
ggsave(
  file.path(DIR_FIG, "sdid1_pit_2018_weights.png"),
  placebo_2018_weights_plot,
  width = 7,
  height = 9
)

# --- Placebo 2018 RMSE Plot ---
ggsave(
  file.path(DIR_FIG, "sdid1_pit_2018_rmse.png"),
  sdid_placebo_2018_rmse_plot + theme_bachelor_project() +
    theme(legend.position = "none"),
  width = 8, height = 6
)


# <<< NEW: SECTION 7: PLACEBO (2020) STYLING & SAVING ========================
message("--- Section 7: Styling and Saving Placebo (2020) Plots ---")

# --- Placebo 2020 SDID Plot ---
message("Styling placebo 2020 plots...")
placebo_2020_diagram_plot <- style_sdid_plot(
  placebo_2020_plot_raw,
  vline_year = PLACEBO_YEAR_2020,
  plot_title = "SDID: In-Time Placebo Test (2020-2021)"
)
ggsave(file.path(DIR_FIG, "sdid1_pit_2020_diagram.png"), placebo_2020_diagram_plot, width = 8, height = 6)

# --- Placebo 2020 Overlay Plot ---
placebo_2020_overlay_plot <- style_sdid_plot(
  placebo_2020_overlay_plot_raw,
  vline_year = PLACEBO_YEAR_2020,
  plot_title = "SDID: In-Time Placebo Test (2020-2021) Overlay"
)
ggsave(file.path(DIR_FIG, "sdid1_pit_2020_overlay.png"), placebo_2020_overlay_plot, width = 8, height = 6)

# --- Placebo 2020 Units Dot Plot ---
message("Styling 2020 units dot plot...")
placebo_2020_dot_plot <- placebo_2020_units_dot_plot_raw +
  theme_bachelor_project() +
  labs(
    title = "SDID: In-Time Placebo (2020) Units Dot Plot",
    caption = "Dot size proportional to unit weight"
  ) +
  theme(strip.background = element_blank(), strip.text = element_blank())
ggsave(file.path(DIR_FIG, "sdid1_pit_2020_dot.png"), placebo_2020_dot_plot, width = 8, height = 6)

# --- Unit & Time Weights Plots ---
message("Styling 2020 weights plots...")
placebo_2020_unit_weights_plot <- ggplot(placebo_2020_unit_weights_df, aes(x = unit, y = weight)) +
  geom_col(fill = "steelblue", alpha = 1) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_bachelor_project() +
  labs(x = NULL, y = NULL)

placebo_2020_time_weights_plot <- ggplot(placebo_2020_time_weights_df, aes(x = time, y = weight)) +
  geom_col(fill = "darkgreen", alpha = 1) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_bachelor_project() +
  labs(x = NULL, y = NULL)

placebo_2020_weights_plot <- placebo_2020_unit_weights_plot / placebo_2020_time_weights_plot +
  plot_annotation(
    title = "SDID: In-Time Placebo (2020) Country & Time Weights",
    theme = theme_bachelor_project()
  )
ggsave(
  file.path(DIR_FIG, "sdid1_pit_2020_weights.png"),
  placebo_2020_weights_plot,
  width = 7,
  height = 9
)

# --- Placebo 2020 RMSE Plot ---
ggsave(
  file.path(DIR_FIG, "sdid1_pit_2020_rmse.png"),
  sdid_placebo_2020_rmse_plot + theme_bachelor_project() +
    theme(legend.position = "none"),
  width = 8, height = 6
)


# 6. EXPORT RESULTS TABLE ====================================================
message("--- Section 6: Exporting Results Table ---")

message("Generating SDID ATT table...")
combined_att_data <- tibble(
  Model = c("Main (2022-2024)",
            "Placebo In-Time (2018-2021)",
            "Placebo In-Time (2019-2021)",
            "Placebo In-Time (2020-2021)"),
  Estimate = c(att, placebo_2018_att, placebo_att, placebo_2020_att),
  Std.Error = c(se, placebo_2018_se, placebo_se, placebo_2020_se),
  ci.lower = c(ci_lower, placebo_2018_ci_lower, placebo_ci_lower, placebo_2020_ci_lower),
  ci.upper = c(ci_upper, placebo_2018_ci_upper, placebo_ci_upper, placebo_2020_ci_upper),
  p.value = c(p_value, placebo_2018_p_value, placebo_p_value, placebo_2020_p_value),
  RMSE = c(rmse, rmse_placebo_2018, rmse_placebo, rmse_placebo_2020)
)

sdid1_results <- combined_att_data %>%
  # <<< UPDATED: Added sorting by Model to keep placebos in order
  arrange(desc(Model)) %>%
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
