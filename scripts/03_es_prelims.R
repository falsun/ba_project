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
# Format: "Variable_Name_In_Data" = "Nice Label For Output"
VARS_TO_TEST <- c(
  "milex_gdp"     = "Forsvarsudgifter (% BNP)",
  "milex_usd_log" = "Log forsvarsudgifter (US$)"
)

COUNTRY_NAMES <- c(
  "AUS" = "Australien",
  "AUT" = "Østrig",
  "CAN" = "Canada",
  "CHE" = "Schweiz",
  "IRL" = "Irland",
  "JPN" = "Japan",
  "KOR" = "Sydkorea",
  "NZL" = "New Zealand"
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

library(conflicted)
library(tidyverse)
library(here)
library(fixest)
library(plm)
library(broom)
library(gt)
library(gtsummary)
library(glue)
library(scales)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("lag", "dplyr")

source(file.path(DIR_SCRIPTS, "00_functions.R")) # For custom themes


# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

es_panel <- readRDS(ES_PANEL)

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

# Loop over names(VARS_TO_TEST) instead of the vector itself
for (var_code in names(VARS_TO_TEST)) {

  # Retrieve the nice label
  var_label <- VARS_TO_TEST[[var_code]]
  message(paste("  ... Processing Variable:", var_label))

  plot_data_control <- map_dfr(control_ids, function(placebo_country) {

    # 1. Assign Placebo Treatment
    data_loop <- control_data %>%
      mutate(treat_dummy_placebo = ifelse(iso3c == placebo_country, 1, 0))

    # 2. Run Model (Standard TWFE + IID Errors)
    f_placebo <- as.formula(glue("{var_code} ~ i(event_time, treat_dummy_placebo, ref = -1) | iso3c + year"))
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
    # --- UPDATED LOGIC: PULL NAME FROM DATA ---
    # We filter the control_data to find the Danish name for this ISO code
    # We take unique() because the data has multiple rows per country
    danish_country_name <- control_data %>%
      filter(iso3c == placebo_country) %>%
      pull(country_dan) %>%
      unique()

    tidy(mod_placebo, conf.int = TRUE) %>%
      filter(grepl("event_time::", term)) %>%
      mutate(
        country = glue(danish_country_name, " ({p_str})"), # <--- Use the extracted name here
        event_time_num = as.numeric(str_extract(term, "-?\\d+"))
      )
  })

  # Generate Plot
  p_control <- ggplot(plot_data_control, aes(x = event_time_num, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "grey40") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, color = "black") +
    geom_point(color = "black") +
    facet_wrap(~country, scales = "free_y", strip.position = "bottom") +
    coord_cartesian(clip = "off") +
    labs(
      x = NULL,
      y = "Placebo ATT",
      caption = "tal i parentes er pre-trend F-test p-værdier."
    ) +
    theme_bachelor_project() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 9, color = "black"),
      strip.text = element_text(hjust = 0.5),
      plot.margin           = margin(t = 5)
    )

  # Save using 'var_code' for filename to avoid spaces/symbols
  ggsave(file.path(DIR_FIG, glue("placebo_control_plot_{var_code}.png")),
         p_control, width = 10, height = 7)
}


# 4. TEST 2: CROSS-SECTIONAL DEPENDENCE (CSD) ================================
message("--- Section 4: Running Pesaran CD Tests (Combined) ---")

# FIX: Iterate over names(VARS_TO_TEST)
csd_comparison_results <- map_dfr(names(VARS_TO_TEST), function(var_code) {

  # -- 1. Prepare Data --
  data_clean <- pre_treat_df %>%
    select(iso3c, year, treat_dummy, all_of(var_code)) %>%
    na.omit()

  # -- 2. Test Base Model --
  form_base <- as.formula(glue("{var_code} ~ 1 | iso3c + year"))
  mod_base  <- try(feols(form_base, data = data_clean), silent = TRUE)

  p_base <- NA
  if (!inherits(mod_base, "try-error")) {
    resid_base <- residuals(mod_base)
    pd_base <- pdata.frame(data_clean %>% mutate(r = resid_base), index = c("iso3c", "year"))
    cd_base <- try(pcdtest(r ~ 1, data = pd_base), silent = TRUE)
    if (!inherits(cd_base, "try-error")) p_base <- cd_base$p.value
  }

  # -- 3. Test Main Model --
  form_main <- as.formula(glue("{var_code} ~ i(treat_dummy, year, ref=0) | iso3c + year"))
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
    variable = var_code, # Keeps 'milex_gdp' temporarily
    p_val_base = as.numeric(p_base),
    p_val_main = as.numeric(p_main)
  )
})

# -- Generate Comparison Table --
tbl_csd_combined <- csd_comparison_results %>%
  # FIX: Recode the variable code to the nice label for the table
  mutate(variable = VARS_TO_TEST[variable]) %>%
  gt() %>%
  cols_label(
    variable = "",
    p_val_base = "TWFE (p)",
    p_val_main = "TWFE + gruppetendenser (p)"
  ) %>%
  theme_gt_bachelor_project() %>%
  fmt(
    columns = starts_with("p_val"),
    fns = function(x) gtsummary::style_pvalue(x, digits = 3)
  ) %>%
  cols_align(align = "right", columns = starts_with("p_val")) %>%
  tab_footnote(
    locations = cells_column_labels(columns = p_val_main),
    footnote = "Inkluderer en gruppespecifik lineær tidstendens for behandlingsgruppen."
  )  %>%
  tab_source_note(
    source_note = "H₀: Tværsnitsuafhængighed. En p-værdi < 0,05 indikerer tværsnitsafhængighed."
  )

gtsave(tbl_csd_combined, file = file.path(DIR_TAB, "diag_csd_comparison.html"))


# 5. TEST 3: ROBUST STATIONARITY (LLC SENSITIVITY) ===========================
message("--- Section 5: LLC Sensitivity Check ---")

# FIX: Iterate over names(VARS_TO_TEST)
pur_test_results <- map_dfr(names(VARS_TO_TEST), function(var_code) {

  data_clean <- pre_treat_df %>%
    select(iso3c, year, all_of(var_code)) %>%
    na.omit()

  p_data <- pdata.frame(data_clean, index = c("iso3c", "year"))
  p_data_balanced <- try(plm::make.pbalanced(p_data, balance.type = "shared.individuals"), silent = TRUE)

  if (inherits(p_data_balanced, "try-error") || nrow(p_data_balanced) == 0) {
    return(tibble(variable = var_code, spec = "Error", p.value = NA))
  }

  # Only running Intercept vs Trend
  specs <- c("intercept", "trend")

  map_dfr(specs, function(spec_type) {
    llc_test <- try(
      purtest(object = p_data_balanced[[var_code]], test = "levinlin", exo = spec_type, lags = 0),
      silent = TRUE
    )

    if (inherits(llc_test, "try-error")) {
      return(tibble(variable = var_code, spec = spec_type, p.value = NA))
    }

    tibble(
      variable = var_code,
      spec = spec_type,
      p.value = as.numeric(llc_test$statistic$p.value)
    )
  })
})

# -- Generate Table --
tbl_pur_clean <- pur_test_results %>%
  pivot_wider(names_from = spec, values_from = p.value, names_prefix = "p_") %>%
  # FIX: Recode variable code to label for the table
  mutate(variable = VARS_TO_TEST[variable]) %>%
  gt() %>%
  cols_label(
    variable = "",
    p_intercept = "Konstantled (p)",
    p_trend = "Konstantled + tidstendens (p)"
  ) %>%
  theme_gt_bachelor_project() %>%
  fmt(
    columns = starts_with("p_"),
    fns = function(x) gtsummary::style_pvalue(x, digits = 3)
  ) %>%
  cols_align(align = "right", columns = starts_with("p_")) %>%
  tab_source_note(
    source_note = "H₀: Serierne er ikke-stationære (har enhedsrødder). En p-værdi < 0,05 indikerer stationaritet."
  )

gtsave(tbl_pur_clean, file = file.path(DIR_TAB, "diag_stationarity_results.html"))

message("\n--- Script 03_prelims_diagnostics.R Finished Successfully ---")
