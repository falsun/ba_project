# ---------------------------------------------------------------------------- #
#
#   Script:       11_ols_prelims.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Description:  Diagnostic Tests for OLS Models (Gauss-Markov Assumptions).
#                 - Generates ONE summary table for all 4 full models.
#                 - Saves diagnostic plots (Residuals, CR-Plots) separately.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION ===========================================================
message("--- Section 0: Loading Configuration ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, broom, here, glue, gt,
  car,        # For VIF and crPlots
  lmtest,     # For Breusch-Pagan and RESET
  ggfortify,  # For Standard Diagnostic Plots
  patchwork
)

options(OutDec = ",")

# Directories
DIR_DATA    <- here::here("data", "_processed", "ols_data.rds")
DIR_SCRIPTS <- here::here("scripts")
DIR_TAB     <- here::here("_output", "_tables", "_prelims_ols")
DIR_FIG     <- here::here("_output", "_figures", "_prelims_ols")

if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

if (!file.exists(DIR_DATA)) stop("Data not found. Run 09_ols_data_trans.R.")

# Source custom theme
if (file.exists(here::here("scripts", "00_functions.R"))) {
  source(here::here("scripts", "00_functions.R"))
} else {
  theme_gt_bachelor_project <- function(data) { data }
}

# Define the "Full" Models (Model 6 equivalent)
MODELS_TO_TEST <- list(
  "Forsvarsbudget (% BNP) 2014-21" = list(
    outcome    = "milex_gdp_pre",
    predictors = c("dist_enemy_inv_sqrt", "border_rus", "nato_gap_2014", "gdp_2014_log")
  ),
  "Forsvarsbudget (% BNP) 2021-25" = list(
    outcome    = "milex_gdp_post",
    predictors = c("dist_enemy_inv_sqrt", "border_rus", "nato_gap_2021", "gdp_2021_log")
  ),
  "Forsvarsbudget (US$) 2014-21" = list(
    outcome    = "milex_usd_pre",
    predictors = c("dist_enemy_inv_sqrt", "border_rus", "nato_gap_2014", "gdp_2014_log")
  ),
  "Forsvarsbudget (US$) 2021-24" = list(
    outcome    = "milex_usd_post",
    predictors = c("dist_enemy_inv_sqrt", "border_rus", "nato_gap_2021", "gdp_2021_log")
  )
)


# 1. LOAD DATA ===============================================================
message("--- Section 1: Loading Data ---")

ols_data <- readRDS(DIR_DATA) %>%
  filter(iso3c != "") %>%
  column_to_rownames("iso3c")

message(glue("Data loaded. N = {nrow(ols_data)}"))


# 2. HELPER FUNCTIONS ========================================================
message("--- Section 2: Defining Diagnostic Functions ---")

#' Custom P-value formatter
#' Formats numbers < 0.001 as "<0,001", otherwise 3 decimals with comma
format_p_val <- function(x) {
  sapply(x, function(val) {
    if (is.na(val)) return(NA)
    if (val < 0.001) return("<0,001")
    formatC(val, digits = 3, format = "f")
  })
}


#' Run Diagnostics and Plot
run_model_diagnostics <- function(data, model_label, config) {

  message(glue("Processing: {model_label}"))

  # A. Fit Model
  f <- as.formula(paste(config$outcome, "~", paste(config$predictors, collapse = " + ")))
  model <- lm(f, data = data)

  # B. Calculate Stats
  shapiro    <- shapiro.test(residuals(model))
  bp_test    <- lmtest::bptest(model)
  reset_test <- lmtest::resettest(model, power = 2:3, type = "fitted")
  vif_val    <- max(car::vif(model))

  # Cook's D
  cooks <- cooks.distance(model)
  threshold <- 4 / nrow(data)
  outliers <- names(cooks[cooks > threshold])
  outlier_str <- if(length(outliers) > 0) paste(outliers, collapse = ", ") else "-"

  # ... inside run_model_diagnostics ...

  # C. Generate Plots (Side Effect)
  file_slug <- config$outcome

  # Plot 1: Standard Diagnostics (Already existing)
  p_diag <- autoplot(model, which = 1:4, label.size = 3, data = data, colour = "black",
                     smooth.colour = "black", label.vjust = -0.5) +
    theme_bachelor_project() +
    theme(panel.grid.major = element_line()) +
    plot_annotation(title = glue("Diagnostics: {model_label}"))

  ggsave(file.path(DIR_FIG, glue("diag_std_{file_slug}.png")), p_diag, width = 10, height = 8, bg = "white")

  # D. Return Data Row
  tibble(
    Model_Name  = model_label,
    Shapiro_P   = shapiro$p.value,
    BP_P        = bp_test$p.value,
    RESET_P     = reset_test$p.value,
    Max_VIF     = vif_val,
    Outliers    = outlier_str
  )
}


# 3. EXECUTE & AGGREGATE =====================================================
message("--- Section 3: Executing Diagnostics ---")

# Run function for all models and bind rows
diag_results <- map_dfr(names(MODELS_TO_TEST), function(lbl) {
  run_model_diagnostics(ols_data, lbl, MODELS_TO_TEST[[lbl]])
})


# 4. GENERATE SUMMARY TABLE ==================================================
message("--- Section 4: Saving Summary Table ---")

diag_table <- diag_results %>%
  gt() %>%
  tab_header(
    title = "OLS Diagnostik",
    subtitle = "Tester for normalitet, homoskedasticitet, specifikation og outliers"
  ) %>%
  cols_label(
    Model_Name = "Model (6)",
    Shapiro_P  = "Shapiro-Wilk (p)",
    BP_P       = "Breusch-Pagan (p)",
    RESET_P    = "Ramsey RESET (p)",
    Max_VIF    = "Maks. VIF",
    Outliers   = "Indflydelsesrige Obs. (Cook's D)"
  ) %>%
  #theme_gt_bachelor_project() %>%
  # Apply Custom P-value formatting (<0,001)
  fmt(
    columns = c(Shapiro_P, BP_P, RESET_P),
    fns = format_p_val
  )

# Save
gtsave(diag_table, file.path(DIR_TAB, "ols_diagnostics_summary.html"))

message(glue("Table saved to {DIR_TAB}/ols_diagnostics_summary.html"))
message("\n--- Script 11_ols_prelims.R finished ---")
