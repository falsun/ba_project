# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       09_heterogeneity_triple_did.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-11-20
#   Description:  Heterogeneity Analysis (Distance Moderator).
#                 Compares two specifications of the Triple DiD Model.
#                 FIXED: Formula now uses 'log_dist_centered' (Mean-Centered).
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

set.seed(1234)

DIR_DATA    <- here::here("data", "_processed")
DIR_SCRIPTS <- here::here("scripts")
DIR_TAB     <- here::here("_output", "_tables", "_heterogeneity")
DIR_FIG     <- here::here("_output", "_figures", "_heterogeneity")

if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

MASTER_PANEL <- file.path(DIR_DATA, "master_panel.rds")

# --- Variables to Analyze ---
VARS_FOR_ANALYSIS <- c(
  "milex_gdp",
  "milex_cap"
)

# --- Coefficient Labels ---
COEF_LABELS <- c(
  "-8" = "2014", "-7" = "2015", "-6" = "2016", "-5" = "2017",
  "-4" = "2018", "-3" = "2019", "-2" = "2020",
  "0" = "2022", "1" = "2023", "2" = "2024"
)

# --- Parameters for Uniform CIs ---
N_SIMULATIONS <- 5000
CONF_LEVEL    <- 0.95

# --- MODEL SPECIFICATIONS ---
model_specs <- list(

  # 1. BASE HETEROGENEITY
  "het_base" = list(
    suffix      = "het_base",
    title       = "Base Heterogeneity (Standard TWFE)",

    # FORMULA FIX: Pointing to 'log_dist_centered' instead of 'dist_min_log'
    formula_str = "{var_name} ~ i(event_time, treat_dummy, ref = -1) +
                                i(event_time, treat_dummy, ref = -1):log_dist_centered |
                                iso3c + year",

    cluster_fml = "~iso3c",
    subtext     = "Spec: TWFE + Interaction (Ref 2021)",
    ref_points  = -1,
    # Regex updated to match centered variable name
    term_pattern = ":treat_dummy:log_dist_centered"
  ),

  # 2. MAIN HETEROGENEITY (Trend-Corrected)
  "het_main" = list(
    suffix      = "het_main",
    title       = "Main Heterogeneity (Trend-Corrected)",

    # FORMULA FIX: Pointing to 'log_dist_centered'
    formula_str = "{var_name} ~ i(event_time, treat_dummy, ref = c(-1, -8)) +
                                i(event_time, treat_dummy, ref = c(-1, -8)):log_dist_centered +
                                i(treat_dummy, year, ref = 0) +
                                i(treat_dummy, year, ref = 0):log_dist_centered |
                                iso3c + year",

    cluster_fml = "~iso3c",
    subtext     = "Spec: Full Trend Correction",
    ref_points  = c(-1, -8),
    term_pattern = ":treat_dummy:log_dist_centered"
  )
)


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, fixest, modelsummary, gt, broom, glue, scales, MASS, gtsummary, conflicted
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

source(file.path(DIR_SCRIPTS, "00_functions.R"))
options(scipen = 999)


# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

master_panel <- readRDS(MASTER_PANEL)

analysis_df <- master_panel %>%
  filter(group %in% c("control", "treatment")) %>%
  mutate(event_time = year - 2022)

# Center Moderator on Treated Min
min_dist_treated <- min(analysis_df$dist_min_log[analysis_df$group == "treatment"], na.rm = TRUE)
analysis_df <- analysis_df %>% mutate(log_dist_centered = dist_min_log - min_dist_treated)

message("Centering Check (Mean should be approx 0 for treated):")
summary(analysis_df$log_dist_centered[analysis_df$group == "treatment"])


# 3. RUN CONSOLIDATED EVENT STUDY LOOP =======================================
message(paste("--- Section 3: Starting Heterogeneity Loop ---"))

labels_df <- enframe(COEF_LABELS, name = "term", value = "label")

for (var_name in VARS_FOR_ANALYSIS) {
  message(paste("\n=== Processing Variable:", var_name, "==="))

  for (spec_name in names(model_specs)) {

    spec <- model_specs[[spec_name]]
    message(paste("   Running:", spec$title))

    tryCatch({

      # --- 3.1. ESTIMATE MODEL ---
      f_es <- as.formula(glue(spec$formula_str))
      c_es <- as.formula(spec$cluster_fml)

      model_es <- feols(f_es, data = analysis_df, cluster = c_es)


      # --- 3.2. WALD TEST (PRE-TREND ON INTERACTION) ---
      all_coefs <- names(coef(model_es))

      relevant_terms <- all_coefs[grepl(spec$term_pattern, all_coefs)]
      # Ensure we only test event_time terms, not linear trend terms
      pre_terms <- relevant_terms[grepl("event_time::-", relevant_terms)]

      p_pre <- NA
      if(length(pre_terms) > 0) {
        ft <- try(wald(model_es, pre_terms), silent=TRUE)
        if(!inherits(ft, "try-error")) p_pre <- ft$p[1]
      }


      # --- 3.3. TIDY RESULTS ---
      tidy_results <- tidy(model_es, cluster = c_es, conf.int = TRUE)
      glance_results <- glance(model_es)

      table_data <- tidy_results %>%
        filter(grepl("event_time", term)) %>% # Filter out linear trend terms for plotting
        mutate(
          # Tag Type based on variable name in term
          type = case_when(
            grepl("log_dist_centered", term) ~ "interaction",
            TRUE ~ "base"
          ),
          event_time_num = as.numeric(str_extract(term, "-?\\d+")),
          label = COEF_LABELS[as.character(event_time_num)]
        ) %>%
        filter(!is.na(label)) %>%
        arrange(event_time_num)


      # --- 3.4. GENERATE TABLE ---
      if(nrow(table_data) > 0) {

        table_wide <- table_data %>%
          mutate(
            est_fmt = glue("{round(estimate, 2)} ({gtsummary::style_pvalue(p.value, digits=2)})")
          ) %>%
          select(label, event_time_num, type, est_fmt) %>%
          pivot_wider(names_from = type, values_from = est_fmt) %>%
          arrange(event_time_num)

        gt_tab <- gt(table_wide) %>%
          cols_hide(columns = event_time_num) %>%
          tab_header(
            title = glue("{spec$title}: {var_name}"),
            subtitle = glue("{spec$subtext}")
          ) %>%
          cols_label(
            label = "Year",
            base = "Average NATO Effect", # Updated Label for clarity
            interaction = "Distance Decay"
          ) %>%
          cols_align(align = "center", columns = everything()) %>%
          theme_gt_bachelor_project() %>%
          tab_source_note(md(paste(
            "**Obs.:**", glance_results$nobs,
            "&nbsp;&nbsp;&nbsp;&nbsp; **Interaction Pre-Trend (p):**",
            gtsummary::style_pvalue(p_pre, digits = 3)
          ))) %>%
          tab_footnote(
            footnote = "Estimated increase for a NATO member at Mean Distance (~1280km).",
            locations = cells_column_labels(columns = base)
          ) %>%
          tab_footnote(
            footnote = "Additional effect per unit of Log Distance (Negative = Decay).",
            locations = cells_column_labels(columns = interaction)
          )

        gtsave(gt_tab, file = file.path(DIR_TAB, glue("het_{spec$suffix}_table_{var_name}.html")))


        # --- 3.5. PLOT (Interaction Only) ---
        plot_data <- table_data %>% filter(type == "interaction")

        if(nrow(plot_data) > 0) {

          # Calculate Uniform CIs for Plotting (Interaction Only)
          vcov_mat <- vcov(model_es, cluster = c_es)
          plot_terms <- plot_data$term
          b_es <- coef(model_es)[plot_terms]
          se_es <- plot_data$std.error
          v_es <- vcov_mat[plot_terms, plot_terms]

          sim_draws <- MASS::mvrnorm(n = N_SIMULATIONS, mu = rep(0, length(b_es)), Sigma = v_es)
          sim_t_stats <- abs(sim_draws) / matrix(se_es, nrow = N_SIMULATIONS, ncol = length(b_es), byrow = TRUE)
          sup_t <- apply(sim_t_stats, 1, max)
          crit_val_unif <- quantile(sup_t, CONF_LEVEL)

          plot_data <- plot_data %>%
            mutate(
              uniform.low = estimate - crit_val_unif * std.error,
              uniform.high = estimate + crit_val_unif * std.error
            )

          plot_breaks_x <- sort(unique(c(plot_data$event_time_num, spec$ref_points)))
          plot_labels_x <- as.character(plot_breaks_x + 2022)

          plot_es <- ggplot(plot_data, aes(x = event_time_num, y = estimate)) +
            geom_vline(xintercept = -1, linetype = "dashed", color = "grey40") +
            geom_hline(yintercept = 0, linetype = "solid", color = "grey40") +

            geom_ribbon(aes(ymin = uniform.low, ymax = uniform.high), alpha = 0.25, fill = "#ce6a85") +
            geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.40, fill = "#985277") +

            geom_errorbar(aes(ymin = uniform.low, ymax = uniform.high), width = 0, color = "#5c374c", alpha = 1) +
            geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, color = "#5c374c") +
            geom_point(color = "#5c374c") +

            scale_x_continuous(breaks = plot_breaks_x, labels = plot_labels_x) +
            labs(
              title = glue("Heterogeneity: {var_name}"),
              subtitle = glue("{spec$title} - Interaction Term"),
              x = NULL,
              y = "Interaction Coefficient (Distance)",
              caption = "Negative values indicate the treatment effect DECREASES as Distance INCREASES."
            ) +
            theme_bachelor_project()

          ggsave(file.path(DIR_FIG, glue("het_{spec$suffix}_plot_{var_name}.png")),
                 plot_es, width = 8, height = 5, bg = "white")
        }
      }

    }, error = function(e) {
      message(paste("    ERROR for", var_name, ":", e$message))
    })
  }
}

# 4. SCRIPT COMPLETION =======================================================
message(paste("\n--- Script 09_heterogeneity_triple_did.R finished ---"))
