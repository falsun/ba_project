# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       04_sdid_model.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-10-24
#   Description:  This script runs the Synthetic Difference-in-Differences (SDID)
#                 analysis, generates all plots, and produces summary tables.
#
# ---------------------------------------------------------------------------- #


# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

set.seed(1)

DIR_DATA         <- here::here("data", "_processed")
DIR_SCRIPTS      <- here::here("scripts")
DIR_FIG          <- here::here("_output", "_figures")
DIR_TAB          <- here::here("_output", "_tables")

if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

MASTER_PANEL_LOG <- file.path(DIR_DATA, "master_panel_log.rds")

TREATMENT_YEAR   <- 2022
PLACEBO_YEAR     <- 2019
SE_METHOD        <- "bootstrap"


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,   # Data manipulation
  here,        # File path management
  synthdid,    # SDID model
  gt,          # Nice tables
  gtsummary,   # Table helpers
  patchwork    # Combine plots
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

message("Calculating main model statistics...")

estimate <- as.numeric(sdid_estimate)
se <- sqrt(vcov(sdid_estimate, method = SE_METHOD))
z_stat <- estimate / se
p_value <- 2 * pnorm(-abs(z_stat))

critical_value <- qnorm(0.975)
ci_lower <- estimate - critical_value * se
ci_upper <- estimate + critical_value * se

sdid_rmse_plot <- synthdid_rmse_plot(sdid_estimate)
rmse_data <- sdid_rmse_plot$data
rmse <- rmse_data$rmse[which.max(rmse_data$iteration)]

message(paste("Average Treatment Effect (ATT):", round(estimate, 3)))
message(paste0("Robust Standard Error (", SE_METHOD, "): ", round(se, 3)))
message(paste0("95% CI: [", round(ci_lower, 3), "–", round(ci_upper, 3), "]"))
message(paste("P-Value (manual, from z-stat):", p_value))
message(paste("RMSE:", round(rmse, 3)))


# 4. MAIN MODEL: VISUALIZATIONS ==============================================
message("--- Section 4: Visualizing Main Model Results ---")

# --- 4.1 Helper Function for Plotting ---
create_sdid_plot <- function(estimate_obj, vline_year, plot_title, overlay = FALSE) {

  p <- plot(estimate_obj,
            overlay = as.numeric(overlay),
            treated.name = "treatment",
            control.name = "control",
            line.width = 1.2,
            point.size = 0,
            trajectory.linetype = 1,
            trajectory.alpha = 1,
            effect.alpha = 1,
            diagram.alpha = 0.6,
            se.method = SE_METHOD)

  p <- p +
    geom_vline(xintercept = vline_year - 1, linetype = "dashed", color = "grey40") +
    scale_x_continuous(breaks = seq(2014, 2024, by = 2), limits = c(2014, 2024)) +
    scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.01)) +
    scale_color_project_qual(name = NULL) +
    theme_bachelor_project() +
    labs(x = NULL, y = NULL, title = plot_title)

  p$layers[[7]] <- NULL

  return(p)
}

# --- 4.2 Main SDID Plot ---
message("Generating main SDID plot...")
sdid_plot <- create_sdid_plot(
  sdid_estimate,
  vline_year = TREATMENT_YEAR,
  plot_title = "SDID: Military Spending (% of GDP)"
)
ggsave(file.path(DIR_FIG, "sdid_main_plot.png"), sdid_plot, width = 8, height = 6)

# --- 4.3 Overlay Plot ---
message("Generating overlay plot...")
overlay_plot <- create_sdid_plot(
  sdid_estimate,
  vline_year = TREATMENT_YEAR,
  plot_title = "SDID: Military Spending (% of GDP)",
  overlay = TRUE
)
ggsave(file.path(DIR_FIG, "sdid_overlay_plot.png"), overlay_plot, width = 8, height = 6)

# --- 4.4 Units Dot Plot ---
message("Generating units dot plot...")
units_dot_plot <- synthdid_units_plot(sdid_estimate, se.method = SE_METHOD) +
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

# --- 4.5 Unit & Time Weights Plots ---
message("Generating weights plots...")

unit_weights_df <- synthdid_controls(sdid_estimate, mass = 1, weight.type = "omega") %>%
  as_tibble(rownames = "unit") %>%
  rename(weight = "estimate 1")

unit_weights_plot <- ggplot(unit_weights_df, aes(x = unit, y = weight)) +
  geom_col(fill = "steelblue", alpha = 1) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_bachelor_project() +
  labs(x = NULL, y = NULL)

time_weights_df <- synthdid_controls(sdid_estimate, mass = 1.1, weight.type = "lambda") %>%
  as_tibble(rownames = "time") %>%
  rename(weight = "estimate 1")

time_weights_plot <- ggplot(time_weights_df, aes(x = time, y = weight)) +
  geom_col(fill = "darkgreen", alpha = 1) +
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

# --- 4.6 RMSE Plot ---
ggsave(
  file.path(DIR_FIG, "sdid_rmse_plot.png"),
  sdid_rmse_plot + theme_bachelor_project(),
  width = 8, height = 6
)


# 5. ROBUSTNESS CHECK: IN-TIME PLACEBO =======================================
message("--- Section 5: Estimating In-Time Placebo Model ---")

sdid_placebo_estimate <- synthdid_placebo(sdid_estimate)

message("Calculating placebo model statistics...")
placebo_att <- as.numeric(sdid_placebo_estimate)
placebo_se <- sqrt(vcov(sdid_placebo_estimate, method = SE_METHOD))

if (placebo_se == 0) {
  placebo_z_stat <- Inf
  placebo_p_value <- 0
} else {
  placebo_z_stat <- placebo_att / placebo_se
  placebo_p_value <- 2 * pnorm(-abs(placebo_z_stat))
}

placebo_ci_lower <- placebo_att - critical_value * placebo_se
placebo_ci_upper <- placebo_att + critical_value * placebo_se

sdid_placebo_rmse_plot <- synthdid_rmse_plot(sdid_placebo_estimate)
rmse_placebo_data <- sdid_placebo_rmse_plot$data
rmse_placebo <- rmse_placebo_data$rmse[which.max(rmse_placebo_data$iteration)]

message(paste("Placebo Average Treatment Effect (ATT):", round(placebo_att, 3)))
message(paste0("Placebo Robust Standard Error (", SE_METHOD, "): ", round(placebo_se, 3)))
message(paste0("Placebo 95% CI: [", round(placebo_ci_lower, 3), "–", round(placebo_ci_upper, 3), "]"))
message(paste("Placebo P-Value (manual, from z-stat):", placebo_p_value))
message(paste("Placebo RMSE:", round(rmse_placebo, 3)))

# --- 5.1. Placebo SDID Plot ---
message("Generating placebo plots...")
placebo_plot <- create_sdid_plot(
  sdid_placebo_estimate,
  vline_year = PLACEBO_YEAR,
  plot_title = "SDID: In-Time Placebo Test (2019-2021)"
)
ggsave(file.path(DIR_FIG, "sdid_placebo_plot.png"), placebo_plot, width = 8, height = 6)

# --- 5.2. Placebo Overlay Plot ---
placebo_overlay_plot <- create_sdid_plot(
  sdid_placebo_estimate,
  vline_year = PLACEBO_YEAR,
  plot_title = "SDID: In-Time Placebo Test (2019-2021)",
  overlay = TRUE
)
ggsave(file.path(DIR_FIG, "sdid_placebo_overlay_plot.png"), placebo_overlay_plot, width = 8, height = 6)

# --- 5.3. Placebo RMSE Plot ---
ggsave(
  file.path(DIR_FIG, "sdid_placebo_rmse_plot.png"),
  sdid_placebo_rmse_plot + theme_bachelor_project(),
  width = 8, height = 6
)


# 6. EXPORT RESULTS TABLE ====================================================
message("--- Section 6: Exporting Results Table ---")

message("Generating SDID ATT table...")
combined_att_data <- tibble(
  Model = c("Main (2022-2024)", "Placebo In-Time (2019-2021)"),
  Estimate = c(estimate, placebo_att),
  Std.Error = c(se, placebo_se),
  ci.lower = c(ci_lower, placebo_ci_lower),
  ci.upper = c(ci_upper, placebo_ci_upper),
  p.value = c(p_value, placebo_p_value),
  RMSE = c(rmse, rmse_placebo)
)

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


# 7. YEARLY ATT TABLE ========================================================
message("Generating Yearly ATT Table...")

yearly_att_estimates <- synthdid_effect_curve(sdid_estimate)

all_years <- sort(unique(sdid_data_formatted$time))
post_treatment_years <- all_years[(setup$T0 + 1):length(all_years)]

if (length(yearly_att_estimates) != length(post_treatment_years)) {
  stop("Yearly estimate vector and post-treatment year vector have different lengths.")
}

yearly_att_table_data <- tibble(
  Term = as.character(post_treatment_years),
  Estimate = as.numeric(yearly_att_estimates)
)

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


# 8. SCRIPT COMPLETION =======================================================
message(paste("\n--- Script 04_sdid_model.R finished ---",
              "\nAll output (tables and figures) saved to:", here::here("_output")))
