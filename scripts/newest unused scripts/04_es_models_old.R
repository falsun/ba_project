# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       04_outcome_event_study.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-11-20
#   Description:  Consolidated Event Study Analysis.
#                 Iterates through variables and runs three model specifications:
#                 1. Base (Standard TWFE, Cluster iso3c)
#                 2. Main (Group Trends, Cluster iso3c)
#                 3. Extra (Group Trends, Cluster Region^Year)
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

set.seed(1234)

DIR_DATA    <- here::here("data", "_processed")
DIR_SCRIPTS <- here::here("scripts")
DIR_TAB     <- here::here("_output", "_tables", "_es_models")
DIR_FIG     <- here::here("_output", "_figures", "_es_models")

if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

ES_PANEL <- file.path(DIR_DATA, "es_panel.rds")

TREAT_YEAR <- 2022

options(OutDec = ",")

# --- Variables to Analyze ---
VARS_FOR_ANALYSIS <- c(
  "milex_gdp",
  "milex_usd_log"
)

# --- Variable Labels (For Output) ---
VAR_LABELS <- c(
  "milex_gdp"     = "Forsvarsudgifter (% BNP)",
  "milex_usd_log" = "Log forsvarsudgifter (US$)"
)

# --- Coefficient Labels ---
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
CONF_LEVEL    <- 0.95

# --- MODEL SPECIFICATIONS (The Logic Engine) ---
model_specs <- list(
  "base" = list(
    suffix      = "twfe",
    formula_str = "{var_name} ~ i(event_time, treat_dummy, ref = -1) | iso3c + year",
    cluster_fml = "~iso3c",
    ref_points  = -1
  ),
  "main" = list(
    suffix      = "group_trends",
    formula_str = "{var_name} ~ i(event_time, treat_dummy, ref = c(-1, -8)) + i(treat_dummy, year, ref=0) | iso3c + year",
    cluster_fml = "~iso3c",
    ref_points  = c(-1, -8)
  )
)


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  fixest,
  modelsummary,
  gt,
  broom,
  glue,
  scales,
  MASS,
  gtsummary,
  conflicted
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

source(file.path(DIR_SCRIPTS, "00_functions.R"))

options(scipen = 999)


# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

es_panel <- readRDS(ES_PANEL) %>%
  mutate(event_time = year - TREAT_YEAR)


# 3. RUN CONSOLIDATED EVENT STUDY LOOP =======================================
message(paste("--- Section 3: Starting Event Study Loop ---"))

labels_df <- enframe(COEF_LABELS, name = "term", value = "label")

# Outer Loop: Variables
for (var_name in VARS_FOR_ANALYSIS) {
  message(paste("\n=== Processing Variable:", var_name, "==="))

  # Inner Loop: Model Specifications
  for (spec_name in names(model_specs)) {

    spec <- model_specs[[spec_name]]
    message(paste("   Running:", spec$suffix))

    tryCatch({

      # --- 3.1. ESTIMATE MODEL ---
      # Dynamic formula construction
      f_es <- as.formula(glue(spec$formula_str))
      c_es <- as.formula(spec$cluster_fml)

      model_es <- feols(
        f_es,
        data = es_panel,
        cluster = c_es
      )

      # --- 3.2. WALD TEST (PRE) ---
      all_coefs <- names(coef(model_es))

      pre_terms <- all_coefs[grepl("event_time::", all_coefs) & grepl("-", all_coefs)]
      p_pre <- NA
      if(length(pre_terms) > 0) {
        ft <- try(wald(model_es, pre_terms), silent=TRUE)
        if(!inherits(ft, "try-error")) p_pre <- ft$p[1]
      }

      # --- 3.3. TIDY RESULTS ---
      # Explicitly passing cluster to tidy ensures robust SEs are used
      tidy_results <- tidy(model_es, cluster = c_es, conf.int = TRUE)
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
      # Get Robust Vcov Matrix
      vcov_mat <- vcov(model_es, cluster = c_es)

      es_terms <- table_data$term
      b_es <- coef(model_es)[es_terms]
      se_es <- table_data$std.error # Uses the robust SEs from tidy()
      v_es <- vcov_mat[es_terms, es_terms]

      sim_draws <- MASS::mvrnorm(n = N_SIMULATIONS, mu = rep(0, length(b_es)), Sigma = v_es)
      sim_t_stats <- abs(sim_draws) / matrix(se_es, nrow = N_SIMULATIONS, ncol = length(b_es), byrow = TRUE)
      sup_t <- apply(sim_t_stats, 1, max)
      crit_val_unif <- quantile(sup_t, CONF_LEVEL)

      table_data <- table_data %>%
        mutate(
          uniform.low = estimate - crit_val_unif * std.error,
          uniform.high = estimate + crit_val_unif * std.error
        )

      # --- 3.5. PLOTTING ---
      # Handle Reference Points for X-axis breaks
      # We combine the estimated time points with the reference points defined in the spec
      plot_breaks_x <- sort(unique(c(table_data$event_time_num, spec$ref_points)))

      # Map labels (Estimated years + Reference years)
      # Note: Mapping assumes standard event time (0=2022).
      # -1 is 2021. -8 is 2014.
      label_map <- setNames(
        c(as.character(table_data$label), "2021", "2014"),
        c(table_data$event_time_num, -1, -8)
      )
      plot_labels_x <- label_map[as.character(plot_breaks_x)]

      # Y-Axis "0" Label (Mean of reference period -1)
      ref_mean_val <- mean(
        es_panel[[var_name]][es_panel$event_time == -1 & es_panel$group == "Behandlet"],
        na.rm = TRUE
      )
      ref_mean_label <- paste0("0 (", round(ref_mean_val, 2), ")")

      plot_es <- ggplot(table_data, aes(x = event_time_num, y = estimate)) +
        geom_vline(xintercept = -1, linetype = "dashed", color = "grey40") +
        geom_hline(yintercept = 0, linetype = "solid", color = "grey40") +

        # Uniform Ribbons
        geom_ribbon(aes(ymin = uniform.low, ymax = uniform.high), alpha = 0.15, fill = "grey40") +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.30, fill = "grey40") +

        # Error Bars & Points
        geom_errorbar(aes(ymin = uniform.low, ymax = uniform.high), width = 0, color = "black", alpha = 1) +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, color = "black") +
        geom_point(color = "black") +

        # Scales & Labels
        scale_x_continuous(breaks = plot_breaks_x, labels = plot_labels_x) +
        scale_y_continuous(labels = function(breaks) {
          breaks[breaks == 0] <- ref_mean_label
          return(breaks)
        }) +
        labs(
          x = NULL,
          y = "ATT"
        ) +
        theme_bachelor_project()

      # Save Plot
      ggsave(file.path(DIR_FIG, glue("es_{spec$suffix}_plot_{var_name}.png")),
             plot_es, width = 8, height = 5, bg = "white")


      # --- 3.6. TABLE GENERATION ---
      table_es_gt <- table_data %>%
        select(label, estimate, std.error, conf.low, conf.high, p.value) %>%
        gt() %>%
        cols_label(
          label = "År", estimate = "ATT", std.error = "Std. fejl",
          conf.low = "95% KI", p.value = "p-værdi"
        ) %>%
        fmt_number(
          columns = c(estimate, std.error, conf.low, conf.high),
          decimals = 3,
          dec_mark = ","
        ) %>%
        cols_merge(columns = c(conf.low, conf.high), pattern = "[{1}; {2}]") %>%
        tab_source_note(
          source_note = md(paste(
            "**Pre-Trend F-Test (p):**",
            gtsummary::style_pvalue(p_pre, digits = 3),
            "<br>",
            "**Obs.:**", glance_results$nobs
          ))
        ) %>%
        fmt(
          columns = starts_with("p.val"),
          fns = function(x) gtsummary::style_pvalue(x, digits = 3)
        ) %>%
        theme_gt_bachelor_project()

      # Save Table
      gtsave(table_es_gt, file = file.path(DIR_TAB, glue("es_{spec$suffix}_table_{var_name}.html")))

    }, error = function(e) {
      message(paste("    ERROR for", var_name, "(", spec$suffix, "):", e$message))
    })
  } # End Spec Loop
} # End Variable Loop


# 4. RUN TREND CORRECTION DIAGNOSTICS ========================================
message("--- Section 4: Running Trend Correction Diagnostics ---")
# This section runs specifically for the "main" specification logic

for (var_name in VARS_FOR_ANALYSIS) {

  var_label <- VAR_LABELS[var_name]
  message(glue::glue("   Diagnosticizing Trend for: {var_label}"))

  # 1. Re-Estimate Main Model to ensure we have the exact object
  # (Spec matches 'main' from above)
  f_main <- as.formula(glue::glue("{var_name} ~ i(event_time, treat_dummy, ref = c(-1, -8)) + i(treat_dummy, year, ref=0) | iso3c + year"))
  mod_main <- feols(f_main, data = es_panel, cluster = ~iso3c)

  # --- A. COEFFICIENT RECONSTRUCTION PLOT ---

  # Extract Coefficients
  coef_data <- tidy(mod_main) %>%
    filter(grepl("event_time::", term)) %>%
    mutate(
      event_time = as.numeric(str_extract(term, "-?\\d+")),
      att = estimate
    ) %>%
    select(event_time, att)

  # Add reference years (0)
  ref_years <- tibble(event_time = c(-1, -8), att = 0)
  coef_data <- bind_rows(coef_data, ref_years) %>% arrange(event_time)

  # Construct Plot Data
  control_baseline <- es_panel %>%
    filter(group == "Kontrol") %>%
    group_by(year, event_time) %>%
    summarize(control_val = mean(!!sym(var_name), na.rm = TRUE), .groups = "drop")

  plot_data_perfect <- control_baseline %>%
    left_join(coef_data, by = "event_time") %>%
    mutate(
      att = replace_na(att, 0),
      Kontrol = control_val,
      Behandlet = control_val + att
    ) %>%
    pivot_longer(cols = c(Kontrol, Behandlet), names_to = "group", values_to = "mean_val")

  # Generate Plot
  p_reconstruction <- ggplot(plot_data_perfect, aes(x = year, y = mean_val, color = group)) +
    geom_vline(xintercept = TREAT_YEAR - 1, linetype = "dashed", color = "grey40") +
    geom_line() +
    scale_x_continuous(breaks = seq(2014, 2024, by = 2)) +
    scale_color_project_qual(name = NULL) +
    theme_bachelor_project() +
    labs(
      y = var_label,
      x = NULL,
      color = NULL,
      caption = "Behandlet linje er konstrueret som Kontrolgruppen + Event-study koefficienterne."
    )

  ggsave(file.path(DIR_FIG, glue("trend_correction_plot_{var_name}.png")),
         p_reconstruction, width = 8, height = 6, bg = "white")


  # --- B. TREND CORRECTION TABLE ---

  # Extract Slope
  trend_slope <- coef(mod_main)[["treat_dummy::1:year"]]

  # Build Data
  trend_table_data <- tibble(year = 2014:2024) %>%
    mutate(
      years_from_start = year - 2014,
      correction = trend_slope * years_from_start
    ) %>%
    select(year, correction)

  # Generate Table
  tbl_trend_simple <- trend_table_data %>%
    gt() %>%
    cols_label(
      year = "År",
      correction = "Trend-korrektion"
    ) %>%
    fmt_number(
      columns = correction,
      decimals = 3,
      dec_mark = ","
    ) %>%
    tab_source_note(
      source_note = ("Isoleret effekt af gruppespecifik tidstendens.")
    ) %>%
    theme_gt_bachelor_project()

  gtsave(tbl_trend_simple, file = file.path(DIR_TAB, glue("trend_correction_tbl_{var_name}.html")))
}

# 5. SCRIPT COMPLETION =======================================================
message(paste(
  "\n--- Script 04_outcome_event_study.R finished ---",
  "\nAll output saved to:", DIR_FIG, "and", DIR_TAB
))
