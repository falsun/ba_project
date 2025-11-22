# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       05_robustness_placebo_checks.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-11-20
#   Description:  Runs internal trend checks for both control and treatment groups.
#                 Outputs minimalist faceted plots with centered p-values
#                 placed OUTSIDE the plot area.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

# --- Set Variables to Test ---
VARS_TO_TEST <- c(
  "milex_cap", "milex_cap_log", "milex_gdp", "milex_gdp_log",
  "milex_usd_log"
)

# --- Define Directories ---
DIR_DATA    <- here::here("data", "_processed")
DIR_SCRIPTS <- here::here("scripts")
DIR_FIG     <- here::here("_output", "_figures", "_prelims")

if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

MASTER_PANEL <- file.path(DIR_DATA, "master_panel.rds")


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Data manipulation
  fixest,     # feols, wald
  broom,      # tidy()
  glue,       # String interpolation
  here,       # File paths
  gtsummary,  # P-value formatting
  scales      # Plot scales
)

source(file.path(DIR_SCRIPTS, "00_functions.R")) # Custom themes

# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

master_panel <- readRDS(MASTER_PANEL)

# Base Analysis Dataframe
analysis_df <- master_panel %>%
  filter(group %in% c("control", "treatment")) %>%
  mutate(
    treat_dummy = ifelse(group == "treatment", 1, 0),
    event_time = year - 2022
  )

# Define Groups
control_ids <- analysis_df %>%
  filter(group == "control") %>%
  distinct(iso3c) %>% pull(iso3c)
control_data <- analysis_df %>% filter(iso3c %in% control_ids)

treated_ids <- analysis_df %>%
  filter(group == "treatment") %>%
  distinct(iso3c) %>% pull(iso3c)
treated_data <- analysis_df %>% filter(iso3c %in% treated_ids)


# 3. START MAIN LOOP =========================================================
message(paste("--- Starting loop for", length(VARS_TO_TEST), "variables ---"))

for (VAR_TO_TEST in VARS_TO_TEST) {
  message(paste("\n--- Processing:", VAR_TO_TEST, "---"))

  # ========================================================================== #
  # 3.1. PLACEBO-IN-SPACE (CONTROL GROUP)
  # ========================================================================== #
  message("  ... 3.1: Control Group Loop")

  plot_data_control <- map_dfr(control_ids, function(placebo_country) {

    # 1. Assign Placebo Treatment
    data_loop <- control_data %>%
      mutate(treat_dummy_placebo = ifelse(iso3c == placebo_country, 1, 0))

    # 2. Run Model (Standard TWFE + IID Errors)
    f_placebo <- as.formula(glue("{VAR_TO_TEST} ~ i(event_time, treat_dummy_placebo, ref = -1) | iso3c + year"))
    mod_placebo <- feols(f_placebo, data = data_loop, vcov = "iid")

    # 3. Pre-Trend F-Test
    coef_names <- names(coef(mod_placebo))
    pre_terms <- coef_names[grepl("event_time::", coef_names) & grepl("-", coef_names)]

    p_str <- "NA"
    if (length(pre_terms) > 0) {
      ft <- try(wald(mod_placebo, pre_terms), silent = TRUE)
      if (!inherits(ft, "try-error")) {
        p_str <- gtsummary::style_pvalue(ft$p[1], digits = 3)
      }
    }

    # 4. Prepare Plot Data
    tidy(mod_placebo, conf.int = TRUE) %>%
      filter(grepl("event_time::", term)) %>%
      mutate(
        country = placebo_country,
        event_time_num = as.numeric(str_extract(term, "-?\\d+")),
        p_label_text = glue("Pre F-Test (p): {p_str}")
      )
  })

  # Generate Plot
  p_control <- ggplot(plot_data_control, aes(x = event_time_num, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +

    # Main Points and Error Bars
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, color = "#5c374c") +
    geom_point(color = "#5c374c") +

    # The P-Value Label (Placed OUTSIDE the plot area)
    geom_text(
      data = distinct(plot_data_control, country, p_label_text),
      aes(x = -3, y = -Inf, label = p_label_text),
      vjust = 1,  # Positive value pushes it DOWN below the axis line
      size = 3,
      color = "black",
      inherit.aes = FALSE
    ) +

    facet_wrap(~country, scales = "free_y") +

    # CRITICAL: Allows text to be drawn outside the plot panel
    coord_cartesian(clip = "off") +

    labs(
      title = "Placebo-in-Space Check: Control Group",
      subtitle = glue("Outcome: {VAR_TO_TEST}"),
      x = NULL,
      y = "Placebo ATT"
    ) +

    theme_bachelor_project() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = margin(b = 10)
    )

  ggsave(file.path(DIR_FIG, glue("placebo_control_plot_{VAR_TO_TEST}.png")),
         p_control, width = 10, height = 7, bg = "white")


  # ========================================================================== #
  # 3.2. INTERNAL TREND CHECK (TREATMENT GROUP)
  # ========================================================================== #
  message("  ... 3.2: Treatment Group Loop")

  plot_data_treated <- map_dfr(treated_ids, function(placebo_country) {

    data_loop <- treated_data %>%
      mutate(treat_dummy_placebo = ifelse(iso3c == placebo_country, 1, 0))

    f_placebo <- as.formula(glue("{VAR_TO_TEST} ~ i(event_time, treat_dummy_placebo, ref = -1) | iso3c + year"))
    mod_placebo <- feols(f_placebo, data = data_loop, vcov = "iid")

    coef_names <- names(coef(mod_placebo))
    pre_terms <- coef_names[grepl("event_time::", coef_names) & grepl("-", coef_names)]

    p_str <- "NA"
    if (length(pre_terms) > 0) {
      ft <- try(wald(mod_placebo, pre_terms), silent = TRUE)
      if (!inherits(ft, "try-error")) {
        p_str <- gtsummary::style_pvalue(ft$p[1], digits = 3)
      }
    }

    tidy(mod_placebo, conf.int = TRUE) %>%
      filter(grepl("event_time::", term)) %>%
      mutate(
        country = placebo_country,
        event_time_num = as.numeric(str_extract(term, "-?\\d+")),
        p_label_text = glue("Pre F-Test (p): {p_str}")
      )
  })

  # Generate Plot
  p_treated <- ggplot(plot_data_treated, aes(x = event_time_num, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +

    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, color = "#5c374c") +
    geom_point(color = "#5c374c") +

    geom_text(
      data = distinct(plot_data_treated, country, p_label_text),
      aes(x = -3, y = -Inf, label = p_label_text),
      vjust = 1,
      size = 3,
      color = "black",
      inherit.aes = FALSE
    ) +

    facet_wrap(~country, scales = "free_y") +

    coord_cartesian(clip = "off") +

    labs(
      title = "Internal Trend Check: Treatment Group",
      subtitle = glue("Outcome: {VAR_TO_TEST}"),
      x = NULL,
      y = "Relative ATT"
    ) +

    theme_bachelor_project() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = margin(b = 10)
    )

  ggsave(file.path(DIR_FIG, glue("placebo_treated_plot_{VAR_TO_TEST}.png")),
         p_treated, width = 12, height = 10, bg = "white")

}

# 4. SCRIPT COMPLETION =======================================================
message(paste("\n--- Script 05_robustness_placebo_checks.R finished ---"))
