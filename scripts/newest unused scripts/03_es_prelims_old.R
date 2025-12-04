# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       03_prelims_diagnostics.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-11-20
#   Description:  Comprehensive Diagnostic Suite.
#                 1. Placebo-in-Space (Validating the Control Group)
#                 2. Cross-Sectional Dependence (Base vs. Main Model)
#                 3. Stationarity (Intercept vs. Trend)
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

TREAT_YEAR <- 2022

options(OutDec = ",")

# --- Variables to Test ---
VARS_TO_TEST <- c(
  "milex_gdp"     = "Forsvarsudgifter (% BNP)",
  "milex_usd_log" = "Log forsvarsudgifter (US$)"
)

# --- Directories ---
DIR_DATA    <- here::here("data", "_processed")
DIR_SCRIPTS <- here::here("scripts")
DIR_TAB     <- here::here("_output", "_tables", "_es_prelims")
DIR_FIG     <- here::here("_output", "_figures", "_es_prelims")

# Create output directories if they don't exist
if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

ES_PANEL <- file.path(DIR_DATA, "es_panel.rds")


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Data manipulation
  fixest,     # Fast fixed effects (feols)
  plm,        # Panel data tests (purtest, pcdtest)
  broom,      # Tidy model output
  gt,         # Tables
  gtsummary,  # P-value formatting
  glue,       # String interpolation
  here,       # File paths
  scales,     # Plot scales
  conflicted
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("lag", "dplyr")

source(file.path(DIR_SCRIPTS, "00_functions.R")) # For custom themes


# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

es_panel <- readRDS(ES_PANEL) %>%
  mutate(event_time = year - TREAT_YEAR)

# B. Create Pre-Treatment Subset (2014-2021 for CSD/LLC Tests)
pre_treat_df <- es_panel %>%
  filter(year < TREAT_YEAR)

# Define Groups for Loop
control_ids <- es_panel %>%
  filter(group == "Kontrol") %>%
  distinct(iso3c) %>% pull(iso3c)
control_data <- es_panel %>% filter(iso3c %in% control_ids)


# 3. TEST 1: PLACEBO-IN-SPACE ==================
message("--- Section 3: Running Placebo-in-Space Checks ---")
# Logic: Run first to screen for "bad controls" before testing properties.

for (VAR_TO_TEST in VARS_TO_TEST) {
  message(paste("  ... Processing Variable:", VAR_TO_TEST))

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
        p_label_text = glue("Pre-Trend F-Test (p): {p_str}")
      )
  })

  # Generate Plot
  p_control <- ggplot(plot_data_control, aes(x = event_time_num, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, color = "#5c374c") +
    geom_point(color = "#5c374c") +
    geom_text(
      data = distinct(plot_data_control, country, p_label_text),
      aes(x = -3, y = -Inf, label = p_label_text),
      vjust = 1,
      size = 3,
      color = "black",
      inherit.aes = FALSE
    ) +
    facet_wrap(~country, scales = "free_y") +
    coord_cartesian(clip = "off") +
    labs(
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
}


# 4. TEST 2: CROSS-SECTIONAL DEPENDENCE (CSD) ================================
message("--- Section 4: Running Pesaran CD Tests (Combined) ---")

csd_comparison_results <- map_dfr(VARS_TO_TEST, function(var_name) {

  # -- 1. Prepare Data --
  data_clean <- pre_treat_df %>%
    select(iso3c, year, treat_dummy, all_of(var_name)) %>%
    na.omit()

  # -- 2. Test Base Model --
  form_base <- as.formula(glue("{var_name} ~ 1 | iso3c + year"))
  mod_base  <- try(feols(form_base, data = data_clean), silent = TRUE)

  p_base <- NA
  if (!inherits(mod_base, "try-error")) {
    resid_base <- residuals(mod_base)
    pd_base <- pdata.frame(data_clean %>% mutate(r = resid_base), index = c("iso3c", "year"))
    cd_base <- try(pcdtest(r ~ 1, data = pd_base), silent = TRUE)
    if (!inherits(cd_base, "try-error")) p_base <- cd_base$p.value
  }

  # -- 3. Test Main Model --
  form_main <- as.formula(glue("{var_name} ~ i(treat_dummy, year, ref=0) | iso3c + year"))
  mod_main  <- try(feols(form_main, data = data_clean), silent = TRUE)

  p_main <- NA
  if (!inherits(mod_main, "try-error")) {
    resid_main <- residuals(mod_main)
    pd_main <- pdata.frame(data_clean %>% mutate(r = resid_main), index = c("iso3c", "year"))
    cd_main <- try(pcdtest(r ~ 1, data = pd_main), silent = TRUE)
    if (!inherits(cd_main, "try-error")) p_main <- cd_main$p.value
  }

  # -- 4. Return Comparison Row --
  tibble(
    variable = var_name,
    p_val_base = as.numeric(p_base),
    p_val_main = as.numeric(p_main)
  )
})

# -- Generate Comparison Table --
tbl_csd_combined <- gt(csd_comparison_results) %>%
  cols_label(
    variable = "Variable",
    p_val_base = "Base Model (p)",
    p_val_main = "Main Model (p)"
  ) %>%
  tab_header(title = "Cross-Sectional Dependence Test (Pesaran CD)") %>%
  theme_gt_bachelor_project() %>%
  fmt(
    columns = starts_with("p_val"),
    fns = function(x) gtsummary::style_pvalue(x, digits = 3)
  ) %>%
  cols_align(align = "right", columns = starts_with("p_val")) %>%
  tab_footnote(
    locations = cells_column_labels(columns = p_val_base),
    footnote = "TWFE: iso3c + year."
  ) %>%
  tab_footnote(
    locations = cells_column_labels(columns = p_val_main),
    footnote = "TWFE and group trends: iso3c + year + treat_dummy[year]."
  ) %>%
  tab_source_note("H0: Cross-sectional independence (no correlation between units).")

gtsave(tbl_csd_combined, file = file.path(DIR_TAB, "diag_csd_comparison.html"))


# 5. TEST 3: ROBUST STATIONARITY (LLC SENSITIVITY) ===========================
message("--- Section 5: LLC Sensitivity Check ---")

pur_test_results <- map_dfr(VARS_TO_TEST, function(var_name) {

  data_clean <- pre_treat_df %>%
    select(iso3c, year, all_of(var_name)) %>%
    na.omit()

  p_data <- pdata.frame(data_clean, index = c("iso3c", "year"))
  p_data_balanced <- try(plm::make.pbalanced(p_data, balance.type = "shared.individuals"), silent = TRUE)

  if (inherits(p_data_balanced, "try-error") || nrow(p_data_balanced) == 0) {
    return(tibble(variable = var_name, spec = "Error", p.value = NA))
  }

  # Only running Intercept vs Trend
  specs <- c("intercept", "trend")

  map_dfr(specs, function(spec_type) {
    llc_test <- try(
      purtest(object = p_data_balanced[[var_name]], test = "levinlin", exo = spec_type, lags = 0),
      silent = TRUE
    )

    if (inherits(llc_test, "try-error")) {
      return(tibble(variable = var_name, spec = spec_type, p.value = NA))
    }

    tibble(
      variable = var_name,
      spec = spec_type,
      p.value = as.numeric(llc_test$statistic$p.value)
    )
  })
})

# -- Generate Table --
tbl_pur_clean <- pur_test_results %>%
  pivot_wider(names_from = spec, values_from = p.value, names_prefix = "p_") %>%
  gt() %>%
  cols_label(
    variable = "Variable",
    p_intercept = "Constant (p)",
    p_trend = "Time Trend (p)"
  ) %>%
  tab_header(title = "Panel Unit Root Test (Levin-Lin-Chu)") %>%
  theme_gt_bachelor_project() %>%
  fmt(
    columns = starts_with("p_"),
    fns = function(x) gtsummary::style_pvalue(x, digits = 3)
  ) %>%
  cols_align(align = "right", columns = starts_with("p_")) %>%
  tab_footnote(
    locations = cells_column_labels(columns = p_intercept),
    footnote = "Assumes data fluctuates around a fixed non-zero mean."
  ) %>%
  tab_footnote(
    locations = cells_column_labels(columns = p_trend),
    footnote = "Assumes data fluctuates around a deterministic linear growth path."
  ) %>%
  tab_source_note("H0: Unit root (non-stationary).")

gtsave(tbl_pur_clean, file = file.path(DIR_TAB, "diag_stationarity_results.html"))

message("\n--- Script 03_prelims_diagnostics.R Finished Successfully ---")
