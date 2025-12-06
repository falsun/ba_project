# ---------------------------------------------------------------------------- #
#
#   Projekt:      BACHELOR PROJEKT
#   Script:       09_ols_models.R
#   Forfatter:    Frederik Bender Bøeck-Nielsen
#   Dato:         06-12-2025
#   Beskrivelse:  Estimerer OLS modellerne:
#                 1. Estimerer OLS modeller for hver afhængig variabel *
#                    tidsperiode kombination.
#                 2. Kører alternative versioner af forsvarsudgifter (% BNP)
#                    2021-25 modellen.
#                 3. Gemmer alle regressionsmodeller som tabeller.
#                 4. Plotter og gemmer bivariate sammenhæng mellem afstand til
#                    strategisk rival (log km) og forsvarsudgifter.
#
# ---------------------------------------------------------------------------- #


# 1. OPSÆTNING AF ARBEJDSMILJØ =================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

# Indlæser pakker
library(conflicted) # håndterer konfilkter
library(here) # robuste filstier
library(tidyverse) # data manipulation og figurer
library(modelsummary) # regressionstabeller
library(fixest) # Robust SEs
library(gt)
library(gtsummary)
library(glue)
library(broom)
library(scales)

# håndterer konflikter
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Input filstier
DIR_SCRIPTS <- here("scripts")
DIR_DATA <- here("data", "_processed", "ols_data.rds")

# Output filstier
DIR_TAB <- here("_output", "_tables", "_ols_models")
DIR_FIG <- here("_output", "_figures", "_ols_plots")
if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

# Indlæser funktioner og brugerdefinerede temaer
source(file.path(DIR_SCRIPTS, "00_functions.R"))

# Sætter komma som decimal-tegn
options(OutDec = ",")

# 1.1. PARAMETRE ---------------------------------------------------------------

# MODELSUMMARY TABEL KONFIGURATION
TABLE_CONFIG <- list(
  stars = c("*" = .05, "**" = .01, "***" = .001),
  coef_map = c(
    "border_rus"     = "Delt grænse med Rusland",
    "dist_enemy_log" = "Afstand til strategisk rival (log km)",
    "nato_gap_2014"  = "Afstand til NATO's 2%-mål (% BNP), 2014",
    "nato_gap_2021"  = "Afstand til NATO's 2%-mål (% BNP), 2021",
    "gdp_2014_log"   = "BNP (log USD), 2014",
    "gdp_2021_log"   = "BNP (log USD), 2021"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "Obs.", "fmt" = 0),
    list("raw" = "adj.r.squared", "clean" = "Justeret R²", "fmt" = 3)
  ),
  notes = "Standardfejl er heteroskedasticitets-robuste (HC3). Konstantled er inkluderet i modellerne, men udeladt fra tabellen."
)


# 2. DATAFORBEREDELSE ==========================================================
message("--- Sektion 2: Indlæser og forbereder data ---")

# Indlæser tværsnitsdata fra 06_ols_data_trans.R
ols_data <- readRDS(DIR_DATA)

# 3. OLS MODELLER ==============================================================
# Estimerer 7 modeller for hver kombination af afhængig variabel og tidsperiode
message("--- Sektion 3: Estimerer OLS modeller ---")

## 3.1. HJÆLPEFUNKTION ---------------------------------------------------------
#' @param dv String: Navn på afhængig variabel
#' @param gap_var String: Navn på NATO mål variabel
#' @param gdp_var String: Navn på økonomi variabel
fit_ols_models <- function(data, dv, gap_var, gdp_var) {
  f1 <- as.formula(glue("{dv} ~ border_rus"))
  f2 <- as.formula(glue("{dv} ~ dist_enemy_log"))
  f3 <- as.formula(glue("{dv} ~ {gap_var}"))
  f4 <- as.formula(glue("{dv} ~ {gdp_var}"))
  f5 <- as.formula(glue("{dv} ~ border_rus + {gap_var}"))
  f6 <- as.formula(glue("{dv} ~ dist_enemy_log + {gap_var}"))
  f7 <- as.formula(glue("{dv} ~ border_rus + dist_enemy_log + {gap_var} + {gdp_var}"))

  list(
    "(1) Trussel (binær)"            = feols(f1, data = data, vcov = "HC3"),
    "(2) Trussel (gradient)"         = feols(f2, data = data, vcov = "HC3"),
    "(3) NATO"                       = feols(f3, data = data, vcov = "HC3"),
    "(4) Økonomi"                    = feols(f4, data = data, vcov = "HC3"),
    "(5) Trussel (binær) og NATO"    = feols(f5, data = data, vcov = "HC3"),
    "(6) Trussel (gradient) og NATO" = feols(f6, data = data, vcov = "HC3"),
    "(7) Trussel, NATO og økonomi"   = feols(f7, data = data, vcov = "HC3")
  )
}

# Generer regressionstabeller
#' @param models
#' @param dv
save_ols_table <- function(models, dv) {
  tab <- modelsummary(
    models,
    output = "gt",
    stars = TABLE_CONFIG$stars,
    coef_map = TABLE_CONFIG$coef_map,
    gof_map = TABLE_CONFIG$gof_map,
    notes = TABLE_CONFIG$notes
  ) %>%
    ba_theme_gt()

  # Gem tabel
  file_name <- glue("ols_{dv}.html")
  gtsave(tab, filename = file.path(DIR_TAB, file_name))
  message(glue("Saved table: {file_name}"))
}


## 3.2. ESTIMER MODELLER -------------------------------------------------------
# Ændring i forsvarsudgifter (% BNP), 2014-21
models_pre_gdp <- fit_ols_models(
  ols_data,
  "milex_gdp_pre",
  "nato_gap_2014",
  "gdp_2014_log"
)
save_ols_table(models_pre_gdp, "milex_gdp_pre")

# Ændring i forsvarsudgifter (% BNP), 2021-25
models_post_gdp <- fit_ols_models(
  ols_data,
  "milex_gdp_post",
  "nato_gap_2021",
  "gdp_2021_log"
)
save_ols_table(models_post_gdp, "milex_gdp_post")

# Ændring i forsvarsudgifter (USD), 2014-21
models_pre_usd <- fit_ols_models(
  ols_data,
  "milex_usd_pre",
  "nato_gap_2014",
  "gdp_2014_log"
)
save_ols_table(models_pre_usd, "milex_usd_pre")

# Ændring i forsvarsudgifter (USD), 2021-25
models_post_usd <- fit_ols_models(
  ols_data,
  "milex_usd_post",
  "nato_gap_2021",
  "gdp_2021_log"
)
save_ols_table(models_post_usd, "milex_usd_post")


# 4. Z-TEST SAMMENLIGNING (PATERNOSTER) ========================================
# Kører Z-test for at sammenligne koefficienterne fra 2014-21 og 2021-25
# forsvarsudgifter (% BNP) modellerne, for at se om ændringen er statistisk
# signifikant.
message("--- Sektion 5: Udfører Z-test for koefficient sammenligning ---")

# Henter de specifikke modeller
mod_pre <- models_pre_gdp[["(7) Trussel, NATO og økonomi"]]
mod_post <- models_post_gdp[["(7) Trussel, NATO og økonomi"]]

## 4.1. Z-TEST HJÆLPEFUNKTION 1 ------------------------------------------------
# BETA og standardfejl fra fixest objekter
get_coef_stats <- function(model, var_name) {
  ct <- coeftable(model)
  list(beta = ct[var_name, "Estimate"], se = ct[var_name, "Std. Error"])
}

## 4.2. Z-TEST HJÆLPEFUNKTION 2 ------------------------------------------------
# Beregn Z-score
calc_z_test <- function(label, var_pre, var_post) {
  s1 <- get_coef_stats(mod_pre, var_pre)
  s2 <- get_coef_stats(mod_post, var_post)
  # Paternoster Clogg formular
  z <- (s2$beta - s1$beta) / sqrt(s1$se^2 + s2$se^2)
  p <- 2 * (1 - pnorm(abs(z)))
  # samler resultaterne
  tibble(
    Variable  = label,
    Beta_Pre  = s1$beta,
    Beta_Post = s2$beta,
    Diff      = s2$beta - s1$beta,
    Z_Score   = z,
    P_Value   = p
  )
}

## 4.3. KØRER Z-TEST -----------------------------------------------------------
z_results <- bind_rows(
  calc_z_test("Delt grænse med Rusland", "border_rus", "border_rus"),
  calc_z_test("Afstand til strategisk rival (log km)", "dist_enemy_log", "dist_enemy_log"),
  calc_z_test("Afstand til NATO's 2%-mål (% BNP)", "nato_gap_2014", "nato_gap_2021"),
  calc_z_test("BNP (log USD)", "gdp_2014_log", "gdp_2021_log")
)

# genererer tabel
tbl_z <- z_results %>%
  gt() %>%
  cols_label(
    Variable  = "",
    Beta_Pre  = "Koeff. (2014–21)",
    Beta_Post = "Koeff. (2021–25)",
    Diff      = "Forskel",
    Z_Score   = "Z-Score",
    P_Value   = "P-værdi"
  ) %>%
  fmt_number(
    columns = c(Beta_Pre, Beta_Post, Diff, Z_Score),
    dec_mark = ",",
    decimals = 3
  ) %>%
  fmt(columns = P_Value, fns = function(x) style_pvalue(x, digits = 3)) %>%
  ba_theme_gt() %>%
  gtsave(file.path(DIR_TAB, "z_test.html"))


# 5. BIVARIATE VISUALISERINGER =================================================
message("--- Sektion 4: Generer visualiseringe af bivariate sammenhænge ---")

## 5.1. HJÆLPEFUNKTION ---------------------------------------------------------
plot_bivariate <- function(data, y_var, y_label, file_name) {
  p <- ggplot(data, aes(x = dist_enemy, y = {{ y_var }})) +
    geom_smooth(
      method = "lm",
      formula = y ~ log(x),
      color = "grey40",
      fill = "grey40",
      alpha = 0.1,
      se = TRUE,
      fullrange = TRUE
    ) +
    geom_text(
      aes(label = iso3c, fontface = ifelse(border_rus == 1, "bold", "plain")),
      family = "IBM Plex Serif",
      size = 3.5
    ) +
    scale_x_continuous(labels = number_format(big.mark = ".")) +
    scale_y_continuous(
      labels = number_format(accuracy = 0.01, decimal.mark = ",")
    ) +
    labs(
      x = "Afstand til strategisk rival (log km)",
      y = y_label,
      caption = "Stater markeret med fed deler grænse med Rusland."
    ) +
    ba_theme()

  ggsave(file.path(DIR_FIG, file_name), p, width = 8, height = 7)
}

## 5.2. GENERER PLOTS ----------------------------------------------------------
# Ændring i forsvarsudgifter (% BNP), 2014-21
plot_bivariate(
  ols_data,
  milex_gdp_pre,
  "Ændring i forsvarsudgifter (% BNP), 2014–21",
  "ols_plot_milex_gdp_pre.png"
)

# Ændring i forsvarsudgifter (% BNP), 2021-25
plot_bivariate(
  ols_data,
  milex_gdp_post,
  "Ændring i forsvarsudgifter (% BNP), 2021–25",
  "ols_plot_milex_gdp_post.png"
)


# 6. SCRIPT FÆRDIG =============================================================
message(paste(
  "\n--- Script 08_ols_prelims.R færdigt ---",
  "\nAlle tabeller er gemt i:", DIR_TAB,
  "\nAlle figurer er gemt i:", DIR_FIG
))
