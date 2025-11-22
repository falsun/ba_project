# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       04b_robustness_region_cluster.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-11-17
#   Description:  Runs the main event-study model with alternative clustering
#                 (Region + Year) as a robustness check for inference.
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

# Same variables as main analysis
VARS_FOR_ANALYSIS <- c(
  "milex_usd_log",
  "milex_gdp",
  "milex_gdp_log",
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
pacman::p_load(
  tidyverse,
  conflicted,
  fixest,
  modelsummary,
  gt,
  broom,
  glue,
  scales,
  MASS,
  gtsummary
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

source(file.path(DIR_SCRIPTS, "00_functions.R"))

options(scipen = 999)


# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

master_panel <- readRDS(MASTER_PANEL)

# Create analysis DF (Ensure 'region' column exists!)
analysis_df <- master_panel %>%
  filter(group %in% c("control", "treatment")) %>%
  mutate(
    treat_dummy = ifelse(group == "treatment", 1, 0),
    event_time = year - 2022
  )


# 3. RUN EVENT STUDY LOOP ====================================================
message(paste("--- Section 3: Starting Robust Clustering Loop ---"))

labels_df <- enframe(COEF_LABELS, name = "term", value = "label")

for (var_name in VARS_FOR_ANALYSIS) {
  message(paste("\n--- Processing:", var_name, "---"))

  tryCatch(
    {
      # --- 3.1. RUN MODEL WITH SUBREGION+YEAR CLUSTERING ---
      formula_es <- as.formula(glue(
        "{var_name} ~ i(event_time, treat_dummy, ref = c(-1, -8)) | iso3c + year + treat_dummy[year]"
      ))

      # Estimate model
      model_es <- feols(
        formula_es,
        data = analysis_df,
        cluster = ~ iso3c + year^subregion
      )

      # --- 3.2. F-TESTS (ROBUST) ---
      # Explicitly get the Vcov matrix for the F-tests
      vcov_robust <- vcov(model_es, cluster = ~ iso3c + year^subregion)

      all_coef_names <- names(coef(model_es))

      # Pre-Trends
      pre_treat_terms <- all_coef_names[grepl("event_time::", all_coef_names) & grepl("-", all_coef_names)]
      f_test_p_val <- NA
      if (length(pre_treat_terms) > 0) {
        f_test <- try(wald(model_es, pre_treat_terms, vcov = vcov_robust), silent = TRUE)
        if (!inherits(f_test, "try-error")) f_test_p_val <- f_test$p[1]
      }

      # Post-Trends
      post_treat_terms <- all_coef_names[grepl("event_time::", all_coef_names) & !grepl("-", all_coef_names)]
      f_test_lo_p_val <- NA
      if (length(post_treat_terms) > 0) {
        f_test_lo <- try(wald(model_es, post_treat_terms, vcov = vcov_robust), silent = TRUE)
        if (!inherits(f_test_lo, "try-error")) f_test_lo_p_val <- f_test_lo$p[1]
      }

      # --- 3.3. TIDY RESULTS ---
      tidy_results <- tidy(model_es, vcov = vcov_robust, conf.int = TRUE)
      glance_results <- glance(model_es)

      table_data <- tidy_results %>%
        left_join(labels_df, by = "term") %>%
        filter(!is.na(label)) %>%
        mutate(
          event_time_num = as.numeric(str_replace_all(term, "event_time::|:treat_dummy", "")),
          label = factor(label, levels = unname(COEF_LABELS))
        ) %>%
        arrange(event_time_num)

      # --- 3.4. SIMULATE UNIFORM CIs ---
      es_terms <- table_data$term
      b_es <- coef(model_es)[es_terms]
      # Use SEs from the robust tidy() which uses the robust vcov
      se_es <- table_data$std.error
      # Subset robust matrix for simulation
      v_es <- vcov_robust[es_terms, es_terms]

      set.seed(1234)
      sim_draws <- MASS::mvrnorm(n = N_SIMULATIONS, mu = rep(0, length(b_es)), Sigma = v_es)
      sim_t_stats <- abs(sim_draws) / matrix(se_es, nrow = N_SIMULATIONS, ncol = length(b_es), byrow = TRUE)
      sup_t <- apply(sim_t_stats, 1, max)
      critical_value_uniform <- quantile(sup_t, CONF_LEVEL)

      table_data <- table_data %>%
        mutate(
          uniform.low = estimate - critical_value_uniform * std.error,
          uniform.high = estimate + critical_value_uniform * std.error
        )

      # --- 3.5. PLOT ---
      plot_breaks_x <- sort(c(table_data$event_time_num, -1))
      label_map <- setNames(c(as.character(table_data$label), "2021"), c(table_data$event_time_num, -1))
      plot_labels_x <- label_map[as.character(plot_breaks_x)]

      ref_mean_val <- mean(analysis_df[[var_name]][analysis_df$event_time == -1 & analysis_df$group == "treatment"], na.rm = TRUE)
      ref_mean_label <- paste0("0 (", round(ref_mean_val, 2), ")")

      plot_es <- ggplot(table_data, aes(x = event_time_num, y = estimate)) +
        geom_vline(xintercept = -1, linetype = "dashed", color = "grey40") +
        geom_hline(yintercept = 0, linetype = "solid", color = "grey40") +
        geom_ribbon(aes(ymin = uniform.low, ymax = uniform.high), alpha = 0.25, fill = "#ce6a85") +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.40, fill = "#985277") +
        geom_errorbar(aes(ymin = uniform.low, ymax = uniform.high), width = 0, color = "#5c374c", alpha = 1) +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, color = "#5c374c") +
        geom_point(color = "#5c374c") +
        scale_x_continuous(breaks = plot_breaks_x, labels = plot_labels_x) +
        scale_y_continuous(labels = function(breaks) {
          breaks[breaks == 0] <- ref_mean_label
          return(breaks)
        }) +
        labs(
          title = glue("Event Study (Robust SEs): {var_name}"),
          subtitle = "Clustered by Subregion + Year",
          x = NULL,
          y = "ATT"
        ) +
        theme_bachelor_project()

      ggsave(file.path(DIR_FIG, glue("es_extra_plot_{var_name}.png")), plot_es, width = 8, height = 5, bg = "white")

      # --- 3.6. TABLE ---
      table_es_gt <- table_data %>%
        select(label, estimate, std.error, conf.low, conf.high, p.value) %>%
        gt() %>%
        tab_header(
          title = glue("Extra Event Study Model (Group-Level Trends): {var_name}"),
          subtitle = "cluster = ~ iso3c + year^subregion"
        ) %>%
        cols_label(
          label = "Year", estimate = "ATT", std.error = "Std. Error",
          conf.low = "95% CI", p.value = "p-value"
        ) %>%
        cols_merge(columns = c(conf.low, conf.high), pattern = "[{1}, {2}]") %>%
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

      gtsave(table_es_gt, file = file.path(DIR_TAB, glue("es_extra_table_{var_name}.html")))
    },
    error = function(e) {
      message(paste("    ERROR for", var_name, ":", e$message))
    }
  )
}

# 4. SCRIPT COMPLETION =======================================================
message(paste("\n--- Script 04b_robustness_region_cluster.R finished ---"))
