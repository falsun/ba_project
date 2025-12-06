# ---------------------------------------------------------------------------- #
#
#   Projekt:      BACHELOR PROJEKT
#   Script:       08_ols_prelims.R
#   Forfatter:    Frederik Bender Bøeck-Nielsen
#   Dato:         06-12-2025
#   Beskrivelse:  Udfører følgende forudsætningstests for OLS modellerne, og
#                 gemmer resultaterne i en samlet tabel (gemmer også standard
#                 diagnostik plot):
#                 1. Shapiro-Wilk test (normalitet)
#                 2. Breusch-Pagan test (homooskedasticitet)
#                 3. Ramsey RESET (funktionel form)
#                 4. VIF (multikolinearitet)
#                 5. Cook's D (indflydelsesrige observationer)
#
# ---------------------------------------------------------------------------- #


# 1. OPSÆTNING AF ARBEJDSMILJØ =================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

# Indlæser pakker
library(conflicted) # konflikthåndtering
library(tidyverse) # data manipulation og plots
library(here) # robust filstier
library(broom)
library(glue)
library(gt)
library(car) # VIF test
library(lmtest) # Breusch-Pagan og Ramsey RESET tests
library(ggfortify) # Diagnostic plots
library(patchwork) # kombinerede plots

# håndterer konflikter
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Input filstier
DIR_SCRIPTS <- here::here("scripts")
DIR_DATA <- here::here("data", "_processed", "ols_data.rds")

# Output filstier
DIR_TAB <- here::here("_output", "_tables", "_ols_prelims")
DIR_FIG <- here::here("_output", "_figures", "_ols_prelims")
if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

# Indlæser funktioner og brugerdefinerede temaer
source(file.path(DIR_SCRIPTS, "00_functions.R"))

# Sætter komma som decimal-tegn
options(OutDec = ",")


## 1.1. PARAMETRE --------------------------------------------------------------

# Modeller
MODELS_TO_TEST <- list(
  "Forsvarsudgifter (% BNP) 2014-21" = list(
    outcome = "milex_gdp_pre",
    predictors = c(
      "dist_enemy_log", "border_rus", "nato_gap_2014", "gdp_2014_log"
    )
  ),
  "Forsvarsudgifter (% BNP) 2021-25" = list(
    outcome = "milex_gdp_post",
    predictors = c(
      "dist_enemy_log", "border_rus", "nato_gap_2021", "gdp_2021_log"
    )
  ),
  "Forsvarsudgifter (USD) 2014-21" = list(
    outcome = "milex_usd_pre",
    predictors = c(
      "dist_enemy_log", "border_rus", "nato_gap_2014", "gdp_2014_log"
    )
  ),
  "Forsvarsudgifter (USD) 2021-24" = list(
    outcome = "milex_usd_post",
    predictors = c(
      "dist_enemy_log", "border_rus", "nato_gap_2021", "gdp_2021_log"
    )
  )
)

# 2. DATAFORBEREDELSE ==========================================================
message("--- Sektion 2: Indlæser og forbereder data ---")

# Indlæser tværsnitsdata fra 06_ols_data_trans.R
ols_data <- readRDS(DIR_DATA) %>%
  filter(iso3c != "") %>%
  column_to_rownames("iso3c")

# 3. FORUDSÆTNINGSTESTS ========================================================
# Kalder funktion fra 00_functions.R til at køre forudsætningstests
message("--- Sektion 3: Udfører forudsætningstests ---")

# 3.2. HJÆLPEFUNKTION ----------------------------------------------------------
run_model_diagnostics <- function(data, model_label, config) {
  message(glue("Behandler model: {model_label}"))

  # A. Fit Model
  f <- as.formula(paste(config$outcome, "~", paste(config$predictors, collapse = " + ")))
  model <- lm(f, data = data)

  # B. Calculate Stats
  shapiro <- shapiro.test(residuals(model))
  bp_test <- lmtest::bptest(model)
  reset_test <- lmtest::resettest(model, power = 2:3, type = "fitted")
  vif_val <- max(car::vif(model))

  # Cook's D
  cooks <- cooks.distance(model)
  threshold <- 4 / nrow(data)
  outliers <- names(cooks[cooks > threshold])
  outlier_str <- if (length(outliers) > 0) paste(outliers, collapse = ", ") else "-"

  # C. Generate Plots (Side Effect)
  file_slug <- config$outcome

  # Plot 1: Standard Diagnostics
  p_diag <- autoplot(model,
    which = 1:4, label.size = 3, data = data, colour = "black",
    smooth.colour = "black", label.vjust = -0.5
  ) +
    ba_theme() +
    theme(panel.grid.major = element_line()) +
    plot_annotation(title = glue("Diagnostics: {model_label}"))

  ggsave(file.path(DIR_FIG, glue("diag_std_{file_slug}.png")), p_diag, width = 10, height = 8)

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

## 3.2. KØR TEST ---------------------------------------------------------------
diag_results <- map_dfr(names(MODELS_TO_TEST), function(lbl) {
  run_model_diagnostics(ols_data, lbl, MODELS_TO_TEST[[lbl]])
})

# Gemmer resultaterne fra testene i en samlet tabel
diag_table <- diag_results %>%
  gt() %>%
  cols_label(
    Model_Name = "Model (7)",
    Shapiro_P  = "Shapiro-Wilk (p)",
    BP_P       = "Breusch-Pagan (p)",
    RESET_P    = "Ramsey RESET (p)",
    Max_VIF    = "Maks. VIF",
    Outliers   = "Indflydelsesrige Obs. (Cook's D)"
  ) %>%
  fmt(columns = c(Shapiro_P, BP_P, RESET_P), fns = format_p_val) %>%
  ba_theme_gt() %>%
  gtsave(file.path(DIR_TAB, "ols_diag_summary.html"))


# 4. SCRIPT FÆRDIG =============================================================

message(paste(
  "\n--- Script 08_ols_prelims.R færdigt ---",
  "\nAlle tabeller er gemt i:", DIR_TAB,
  "\nAlle figurer er gemt i:", DIR_FIG
))
