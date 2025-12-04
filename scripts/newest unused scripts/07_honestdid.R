# ---------------------------------------------------------------------------- #
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       08_robustness_honest_did_plots_only.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-11-21
#   Description:  Streamlined HonestDiD Workflow.
#                 1. Runs Fixest Model for specified variables.
#                 2. Calculates HonestDiD metrics (RM & Smoothness).
#                 3. Generates and saves plots immediately (No intermediate .rds).
# ---------------------------------------------------------------------------- #

# ============================================================================ #
# SECTION 0: CONFIGURATION & SETUP
# ============================================================================ #
message("--- Section 0: Loading Configuration ---")

END_YEAR <- 2024
TREATMENT_YEAR <- 2022

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, fixest, broom, HonestDiD, glue, here, conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# --- Directories ---
DIR_SCRIPTS <- here::here("scripts")
DIR_DATA    <- here::here("data", "_processed")
DIR_FIG     <- here::here("_output", "_figures", "_robustness")

if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

# --- Load Custom Theme ---
if (file.exists(file.path(DIR_SCRIPTS, "00_functions.R"))) {
  source(file.path(DIR_SCRIPTS, "00_functions.R"))
} else {
  warning("00_functions.R not found. Plots may lack custom styling.")
}

# --- Inputs ---
MASTER_PANEL <- file.path(DIR_DATA, "master_panel.rds")

# --- Parameters ---
VARS_TO_TEST <- c("milex_usd_log", "milex_gdp")
POST_LABELS  <- c("2022", "2023", "2024")

# 1. Config for Relative Magnitude (RM)
SENSITIVITY_M_VEC_RM <- seq(0, 1.5, by = 0.5)

# 2. Config for Smoothness (Variable-specific grids)
SENSITIVITY_GRIDS_SMOOTH <- list(
  "milex_usd_log" = seq(0, 12, by = 3),
  "milex_gdp" = seq(0, 0.012, by = 0.003)
)


# ============================================================================ #
# SECTION 1: MAIN PROCESSING LOOP
# ============================================================================ #
message("--- Section 1: Loading Data ---")

master_panel <- readRDS(MASTER_PANEL)
analysis_df <- master_panel %>%
  filter(group %in% c("control", "treatment")) %>%
  filter(year <= END_YEAR) %>%
  mutate(event_time = year - TREATMENT_YEAR)

message("--- Section 2: Starting Calculation & Plotting Loop ---")

for (VAR_NAME in VARS_TO_TEST) {
  message(glue("\n>>> Processing Variable: {VAR_NAME}"))

  # -------------------------------------------------------
  # A. Run Main Model & Extract Coefs
  # -------------------------------------------------------
  fml <- as.formula(glue("{VAR_NAME} ~ i(event_time, treat_dummy, ref = c(-1, -8)) | iso3c + year + treat_dummy[year]"))
  model_es <- feols(fml, data = analysis_df, cluster = ~iso3c)

  all_coefs <- coef(model_es)
  target_indices <- grep("event_time::.*:treat_dummy", names(all_coefs))

  coef_map <- data.frame(
    raw_name = names(all_coefs)[target_indices],
    raw_index = target_indices
  ) %>%
    mutate(time = as.numeric(str_extract(raw_name, "-?\\d+"))) %>%
    arrange(time)

  sorted_indices <- coef_map$raw_index
  betahat <- unname(all_coefs[sorted_indices])
  sigma   <- unname(as.matrix(vcov(model_es, cluster = ~iso3c)[sorted_indices, sorted_indices]))

  num_pre_periods  <- sum(coef_map$time < 0)
  num_post_periods <- sum(coef_map$time >= 0)

  # -------------------------------------------------------
  # B. Calculate HonestDiD Metrics
  # -------------------------------------------------------
  # Temporary lists for this variable only
  temp_rm_list     <- list()
  temp_smooth_list <- list()

  # Get grid for smoothness
  current_M_grid_smooth <- SENSITIVITY_GRIDS_SMOOTH[[VAR_NAME]]
  if (is.null(current_M_grid_smooth)) current_M_grid_smooth <- seq(0, 1, by = 0.1)

  for (i in 1:num_post_periods) {
    l_vec <- HonestDiD::basisVector(index = i, size = num_post_periods)

    # RM Calculation
    temp_rm_list[[i]] <- HonestDiD::createSensitivityResults_relativeMagnitudes(
      betahat = betahat, sigma = sigma,
      numPrePeriods = num_pre_periods, numPostPeriods = num_post_periods,
      Mbarvec = SENSITIVITY_M_VEC_RM, l_vec = l_vec
    ) %>% mutate(Year = POST_LABELS[i], variable = VAR_NAME)

    # Smoothness Calculation
    temp_smooth_list[[i]] <- HonestDiD::createSensitivityResults(
      betahat = betahat, sigma = sigma,
      numPrePeriods = num_pre_periods, numPostPeriods = num_post_periods,
      l_vec = l_vec, Mvec = current_M_grid_smooth, method = "FLCI"
    ) %>% mutate(Year = POST_LABELS[i], variable = VAR_NAME)
  }

  # Bind into dataframes for plotting
  df_rm     <- bind_rows(temp_rm_list)
  df_smooth <- bind_rows(temp_smooth_list)

  # -------------------------------------------------------
  # C. Generate & Save Relative Magnitude Plot
  # -------------------------------------------------------
  p_rm <- ggplot(df_rm, aes(x = Mbar, ymin = lb, ymax = ub, color = Year)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_errorbar(width = 0.075, linewidth = 1, position = position_dodge(width = 0.075)) +
    scale_color_manual(values = c("2022"="#ce6a85", "2023"="#985277", "2024"="#5c374c")) +
    labs(
      title = glue("Relative Magnitude Test (Main Model): {VAR_NAME}"),
      subtitle = "Robustness to violations of parallel trends",
      x = "Max. Allowable Post-Trend Deviation (M)",
      y = "95% CI for ATT",
      caption = "M = 1 represents a post-trend violation equal to the worst pre-trend deviation."
    ) +
    scale_x_continuous(breaks = unique(df_rm$Mbar)) +
    theme_bachelor_project()

  path_rm <- file.path(DIR_FIG, glue("relmag_plot_{VAR_NAME}.png"))
  ggsave(path_rm, p_rm, width = 8, height = 6, bg = "white")
  message(paste("   -> Saved RM Plot:", basename(path_rm)))

  # -------------------------------------------------------
  # D. Generate & Save Smoothness Plot
  # -------------------------------------------------------
  # Dynamic Labels & Widths
  x_label_unit <- case_when(
    VAR_NAME == "milex_usd_log" ~ "Max. Allowable Post-Trend Deviation from Linear Trend (US$)",
    VAR_NAME == "milex_gdp" ~ "Max. Allowable Post-Trend Deviation from Linear Trend (% of GDP)"
  )

  bar_width <- case_when(
    VAR_NAME == "milex_usd_log" ~ 0.6,
    VAR_NAME == "milex_gdp" ~ 0.0006
  )

  p_sm <- ggplot(df_smooth, aes(x = M, ymin = lb, ymax = ub, color = Year)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_errorbar(width = bar_width, linewidth = 1, position = position_dodge(width = bar_width)) +
    scale_color_manual(values = c("2022"="#ce6a85", "2023"="#985277", "2024"="#5c374c")) +
    labs(
      title = glue("Smoothness Restriction Test (Main Model): {VAR_NAME}"),
      subtitle = "Robustness to deviations from a linear trend",
      x = x_label_unit,
      y = "95% CI for ATT",
      caption = "Bounds the curvature of the unobserved counterfactual trend. x = 0 assumes linearity; higher values allow the trend's slope to change."
    ) +
    scale_x_continuous(breaks = unique(df_smooth$M)) +
    theme_bachelor_project()

  path_sm <- file.path(DIR_FIG, glue("smooth_plot_{VAR_NAME}.png"))
  ggsave(path_sm, p_sm, width = 8, height = 6, bg = "white")
  message(paste("   -> Saved Smoothness Plot:", basename(path_sm)))
}

message("\n--- Script Finished Successfully ---")
