# ---------------------------------------------------------------------------- #
#
#   Project:     NATO Defence Spending Bachelor's Thesis
#   Script:      04_outcome_event_study.R
#   Author:      Frederik Bender Bøeck-Nielsen
#   Date:        2025-11-09
#   Description: This script iterates through all outcome variables and
#                runs a full TWFE event study for each one.
#
# ---------------------------------------------------------------------------- #


# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

DIR_DATA <- here::here("data", "_processed")
DIR_SCRIPTS <- here::here("scripts")
DIR_TAB <- here::here("_output", "_tables", "_es_model")
DIR_FIG <- here::here("_output", "_figures", "_es_model")

if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

MASTER_PANEL <- file.path(DIR_DATA, "master_panel.rds")

VARS_FOR_ANALYSIS <- c(
  # Group 1: Constant 2023 US$
  "milex_usd_log",

  # Group 2: % of GDP
  "milex_gdp",
  "milex_gdp_log",

  # Group 4: Constant 2023 US$ per Capita
  "milex_cap",
  "milex_cap_log"
)

COEF_LABELS <- c(
  "event_time::-8:treat_dummy" = "2014",
  "event_time::-7:treat_dummy" = "2015",
  "event_time::-6:treat_dummy" = "2016",
  "event_time::-5:treat_dummy" = "2017",
  "event_time::-4:treat_dummy" = "2018",
  "event_time::-3:treat_dummy" = "2019",
  "event_time::-2:treat_dummy" = "2020",
  "event_time::0:treat_dummy"  = "2022",
  "event_time::1:treat_dummy"  = "2023",
  "event_time::2:treat_dummy"  = "2024"
)

# --- Parameters for Uniform CIs ---
N_SIMULATIONS <- 5000
CONF_LEVEL <- 0.95


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
# Added MASS for mvrnorm()
pacman::p_load(tidyverse, fixest, modelsummary, gt, broom, glue, scales, MASS, conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

source(file.path(DIR_SCRIPTS, "00_functions.R"))

options(scipen = 999)

# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

master_panel <- readRDS(MASTER_PANEL)

analysis_df <- master_panel %>%
  filter(group %in% c("control", "treatment")) %>%
  mutate(
    treat_dummy = ifelse(group == "treatment", 1, 0),
    event_time = year - 2022
  )


# 3. RUN EVENT STUDY LOOP ====================================================
message(paste(
  "--- Section 3: Starting Event Study Loop for",
  length(VARS_FOR_ANALYSIS), "variables ---"
))

labels_df <- enframe(COEF_LABELS, name = "term", value = "label")

for (var_name in VARS_FOR_ANALYSIS) {
  message(paste("\n--- Processing:", var_name, "---"))

  tryCatch(
    {
      # --- 3.1. RUN EVENT STUDY MODEL ---
      formula_es <- as.formula(glue(
        "{var_name} ~ i(event_time, treat_dummy, ref = c(-1, -8)) | iso3c + year + treat_dummy[year]"
      ))

      model_es <- feols(
        formula_es,
        data = analysis_df,
        cluster = ~iso3c
      )

      # --- 3.2. RUN JOINT F-TEST FOR PRE-TRENDS ---
      all_coef_names <- names(coef(model_es))

      pre_treat_terms <- all_coef_names[
        grepl("event_time::", all_coef_names) & grepl("-", all_coef_names)
      ]

      if (length(pre_treat_terms) > 0) {
        f_test <- wald(model_es, pre_treat_terms)
        f_test_p_val <- f_test$p[1]
      } else {
        f_test_p_val <- NA
      }

      # --- 3.2b. NEW: RUN F-TEST FOR POST-TREATMENT (SUGGESTION 4) ---

      post_treat_terms <- all_coef_names[
        grepl("event_time::", all_coef_names) & !grepl("-", all_coef_names)
      ]

      if (length(post_treat_terms) >= 0) {
        f_test_lo <- wald(model_es, post_treat_terms)
        f_test_lo_p_val <- f_test_lo$p[1]
      } else {
        f_test_lo_p_val <- NA
      }

      # --- 3.3. TIDY RESULTS ---
      tidy_results <- tidy(model_es, conf.int = TRUE)
      glance_results <- glance(model_es)

      table_data <- tidy_results %>%
        left_join(labels_df, by = "term") %>%
        filter(!is.na(label)) %>%
        mutate(
          event_time_num = as.numeric(str_replace_all(term, "event_time::|:treat_dummy", "")),
          label = factor(label, levels = unname(COEF_LABELS))
        ) %>%
        arrange(event_time_num)

      # --- 3.4. NEW: SIMULATE UNIFORM CIs (SUGGESTION 3) ---

      # 1. Get coefficients, SEs, and VCOV matrix for event study terms
      es_terms <- table_data$term
      b_es <- coef(model_es)[es_terms]
      se_es <- se(model_es)[es_terms]
      v_es <- vcov(model_es)[es_terms, es_terms]

      # 2. Simulate draws from the multivariate normal distribution
      set.seed(1234) # for reproducibility
      sim_draws <- MASS::mvrnorm(n = N_SIMULATIONS, mu = rep(0, length(b_es)), Sigma = v_es)

      # 3. Calculate t-statistics for each simulation
      sim_t_stats <- abs(sim_draws) / matrix(se_es, nrow = N_SIMULATIONS, ncol = length(b_es), byrow = TRUE)

      # 4. Find the maximum (supreme) t-stat for each simulation
      sup_t <- apply(sim_t_stats, 1, max)

      # 5. Find the critical value (the 95th percentile of the sup-t stats)
      critical_value_uniform <- quantile(sup_t, CONF_LEVEL)

      # 6. Add uniform CIs to the table data
      table_data <- table_data %>%
        mutate(
          uniform.low = estimate - critical_value_uniform * std.error,
          uniform.high = estimate + critical_value_uniform * std.error
        )

      # --- START: CODE FOR 2021 LABEL ---
      plot_breaks_x <- table_data$event_time_num
      plot_labels_x <- table_data$label
      plot_breaks_x <- c(plot_breaks_x, -1)
      plot_labels_x <- c(as.character(plot_labels_x), "2021")
      plot_labels_x <- plot_labels_x[order(plot_breaks_x)]
      plot_breaks_x <- sort(plot_breaks_x)

      # --- START: CODE FOR SUGGESTION 2 ---
      ref_mean_val <- mean(
        analysis_df[[var_name]][analysis_df$event_time == -1 & analysis_df$group == "treatment"],
        na.rm = TRUE
      )
      ref_mean_label <- paste0("0 (", round(ref_mean_val, 2), ")")

      # --- 3.5. GENERATE AND SAVE EVENT STUDY PLOT ---
      plot_es <- ggplot(
        table_data,
        aes(
          x = event_time_num, y = estimate
        )
      ) +
        geom_vline(xintercept = -1, linetype = "dashed", color = "grey40") +
        geom_hline(yintercept = 0, linetype = "solid", color = "grey40") +
        # --- NEW: Uniform Ribbon (lightest) ---
        geom_ribbon(
          aes(ymin = uniform.low, ymax = uniform.high),
          alpha = 0.25, fill = "#ce6a85"
        ) +

        # --- NEW: Pointwise Ribbon (light) ---
        geom_ribbon(
          aes(ymin = conf.low, ymax = conf.high),
          alpha = 0.40, fill = "#985277"
        ) +

        # --- Uniform Confidence Interval (line, no caps) ---
        geom_errorbar(
          aes(ymin = uniform.low, ymax = uniform.high),
          width = 0, # No end-cap
          color = "#5c374c",
          alpha = 1
        ) +

        # --- Pointwise Confidence Interval (with caps) ---
        geom_errorbar(
          aes(ymin = conf.low, ymax = conf.high),
          width = 0.1, # Standard end-cap
          color = "#5c374c"
        ) +
        geom_point(color = "#5c374c") + # Made point slightly larger
        scale_x_continuous(breaks = plot_breaks_x, labels = plot_labels_x) +
        scale_y_continuous(
          labels = function(breaks) {
            breaks[breaks == 0] <- ref_mean_label
            return(breaks)
          }
        ) +
        labs(
          title = glue("Event Study (TWFE): {var_name}"),
          subtitle = "Pointwise (caps/darker ribbon) and Uniform (lines/lighter ribbon) 95% CIs",
          x = NULL,
          y = "ATT"
        ) +
        theme_bachelor_project()

      plot_filename <- file.path(DIR_FIG, glue("es_main_plot_{var_name}.png"))
      ggsave(plot_filename, plot_es, width = 8, height = 5, bg = "white")

      # --- 3.6. GENERATE AND SAVE RESULTS TABLE ---

      table_data_for_gt <- table_data %>%
        select(label, estimate, std.error, conf.low, conf.high, p.value)

      table_es_gt <- gt(table_data_for_gt) %>%
        tab_header(
          title = glue("Main Event Study Model (Group-Level Trends): {var_name}"),
          subtitle = "cluster = ~iso3c"
        ) %>%
        cols_label(
          label = "Year",
          estimate = "ATT",
          std.error = "Std. Error",
          conf.low = "95% CI",
          p.value = "p-value"
        ) %>%
        cols_merge(
          columns = c(conf.low, conf.high),
          pattern = "[{1}, {2}]"
        ) %>%
        theme_gt_bachelor_project() %>%
        tab_source_note(
          source_note = md(paste(
            "**Obs.:**", glance_results$nobs,
            "&nbsp;&nbsp;&nbsp;&nbsp; **Adj. R2:**", round(glance_results$adj.r.squared, 3),
            "&nbsp;&nbsp;&nbsp;&nbsp; **Within R2:**", round(r2(model_es, "wr2"), 3),
            "&nbsp;&nbsp;&nbsp;&nbsp; **RMSE:**", round(glance_results$sigma, 3),
            "<br>",
            "**Pre F-Test (p):**",
            gtsummary::style_pvalue(f_test_p_val, digits = 3),
            "&nbsp;&nbsp;&nbsp;&nbsp; **Post F-Test (p):**",
            gtsummary::style_pvalue(f_test_lo_p_val, digits = 3)
          ))
        )

      table_html_filename <- file.path(DIR_TAB, glue("es_main_table_{var_name}.html"))
      gtsave(table_es_gt, file = table_html_filename)
    },
    error = function(e) {
      message(paste("    ERROR for", var_name, ":", e$message))
    }
  )
}


# 4. SCRIPT COMPLETION =======================================================
message(paste(
  "\n--- Script 04_outcome_event_study.R finished ---",
  "\nAll output (tables and plots) saved to:", DIR_FIG, "and", DIR_TAB
))
