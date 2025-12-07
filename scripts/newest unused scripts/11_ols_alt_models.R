# ---------------------------------------------------------------------------- #
#
#   Projekt:      BACHELOR PROJEKT
#   Script:       11_ols_alt_models.R
#   Forfatter:    Frederik Bender Bøeck-Nielsen
#   Dato:         07-12-2025
#   Beskrivelse:  Estimerer alternative OLS modeller (robusthed):
#                 1. Tester forskellige afstandsmål (afstand til Rusland og
#                    afstand til konfliktzone).
#                 2. Tester alternative forklaringer (kontrolvariabler).
#
# ---------------------------------------------------------------------------- #

# 1. OPSÆTNING AF ARBEJDSMILJØ =================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

# Indlæser pakker
library(conflicted) # håndtering af konflikter
library(here) # robuste filstier
library(tidyverse) # data manipulation
library(modelsummary) # regressionstabeller
library(gt) # tabelformatering
library(fixest) # robuste standardfejl (feols)

# Håndterer konflikter
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Input filstier
DIR_SCRIPTS <- here("scripts")
DIR_DATA <- here("data", "_processed", "ols_data.rds")

# Output filstier
DIR_TAB <- here("_output", "_tables", "_ols_robust")
if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)

# Indlæser funktioner og brugerdefinerede temaer
source(file.path(DIR_SCRIPTS, "00_functions.R"))

# Sætter komma som decimal-tegn
options(OutDec = ",")

# Indlæser data
ols_data <- readRDS(DIR_DATA)


# 2. ALTERNATIVE AFSTANDSMÅL ===================================================
message("--- Sektion 2: Estimerer modeller med alternative afstandsmål ---")

# Basis model
base_A <- "milex_gdp_post ~ nato_gap_2021 + border_rus + gdp_2021_log"

# Afstand til Rusland
m_rus <- feols(
  as.formula(paste(base_A, "+ dist_rus_log")),
  data = ols_data, vcov = "HC3"
)

# Afstand til strategisk rival
m_enemy <- feols(
  as.formula(paste(base_A, "+ dist_enemy_log")),
  data = ols_data, vcov = "HC3"
)

# Afstand til konfliktzone
m_conf <- feols(
  as.formula(paste(base_A, "+ dist_conf_log")),
  data = ols_data, vcov = "HC3"
)

# Regressionstabel
tbl_dist <- modelsummary(
  list(
    "1" = m_enemy,
    "2" = m_conf,
    "3" = m_rus
  ),
  output = "gt",
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  coef_map = c(
    "border_rus"     = "Delt grænse med Rusland",
    "nato_gap_2021"  = "Afstand til NATO's 2%-mål (% BNP), 2021",
    "gdp_2021_log"   = "BNP (log USD), 2021",
    "dist_enemy_log" = "Afstand til strategisk rival (log km)",
    "dist_conf_log"  = "Afstand til konfliktzone (log km)",
    "dist_rus_log"   = "Afstand til Rusland (log km)"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "Obs.", "fmt" = 0),
    list("raw" = "adj.r.squared", "clean" = "Justeret R²", "fmt" = 3),
    list("raw" = "bic", "clean" = "BIC", "fmt" = 1)
  )
) %>%
  tab_source_note("Standardfejl er heteroskedasticitets-robuste (HC3). Konstantled er inkluderet i modellerne, men udeladt fra tabellen.") %>%
  ba_theme_gt() %>%
  gtsave(file.path(DIR_TAB, "ols_alt_distance.html"))


# 3. ALTERNATIVE FORKLARINGER ==================================================
message("--- Sektion 3: Estimerer modeller med alternative kontrolvariabler ---")

# Basis model
base_B <- "milex_gdp_post ~ border_rus + dist_enemy_log + nato_gap_2021"

# BNP
m_gdp <- feols(
  as.formula(paste(base_B, "+ gdp_2021_log")),
  data = ols_data, vcov = "HC3"
)

# BNP pr. indbygger
m_cap <- feols(
  as.formula(paste(base_B, "+ gdp_cap_2021_log")),
  data = ols_data, vcov = "HC3"
)

# Offentlig Gæld
m_debt <- feols(
  as.formula(paste(base_B, "+ debt_gdp_2021_log")),
  data = ols_data, vcov = "HC3"
)

# BNP vækst (nævner-effekt)
m_growth <- feols(
  as.formula(paste(base_B, "+ gdp_growth_post")),
  data = ols_data, vcov = "HC3"
)

# Antal amerikanske tropper
m_troops <- feols(
  as.formula(paste(base_B, "+ us_troops_2021_log")),
  data = ols_data, vcov = "HC3"
)

# Post-Kommunist stat (dummy)
m_post <- feols(
  as.formula(paste(base_B, "+ post_com")),
  data = ols_data, vcov = "HC3"
)

# Regressionstabel
tbl_alt <- modelsummary(
  list(
    "1" = m_gdp,
    "2" = m_cap,
    "3" = m_growth,
    "4" = m_debt,
    "5" = m_troops,
    "6" = m_post
  ),
  output = "gt",
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  coef_map = c(
    "border_rus"         = "Delt grænse med Rusland",
    "dist_enemy_log"     = "Afstand til strategisk rival (log km)",
    "nato_gap_2021"      = "Afstand til NATO's 2%-mål (% BNP), 2021",
    "gdp_2021_log"       = "BNP (log USD), 2021",
    "gdp_cap_2021_log"   = "BNP pr. indbygger (log PPP), 2021",
    "gdp_growth_post"    = "BNP-vækst (%), 2021-25",
    "debt_gdp_2021_log"  = "Offentlig gæld (log % BNP), 2021",
    "us_troops_2021_log" = "Antal Amerikanske Tropper (log), 2021",
    "post_com"           = "Post-kommunistisk stat"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "Obs.", "fmt" = 0),
    list("raw" = "adj.r.squared", "clean" = "Justeret R²", "fmt" = 3)
  )
) %>%
  tab_source_note("Standardfejl er heteroskedasticitets-robuste (HC3). Konstantled er inkluderet i modellerne, men udeladt fra tabellen.") %>%
  ba_theme_gt() %>%
  gtsave(file.path(DIR_TAB, "ols_alt_controls.html"))


# 4. SCRIPT FÆRDIG =============================================================
message(paste(
  "\n--- Script 11_ols_alt_models.R færdigt ---",
  "\nAlle tabeller er gemt i:", DIR_TAB
))
