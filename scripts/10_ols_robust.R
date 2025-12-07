# ---------------------------------------------------------------------------- #
#   Projekt:      BACHELOR PROJEKT
#   Script:       10_ols_robust.R
#   Forfatter:    Frederik Bender Bøeck-Nielsen
#   Dato:         07-12-2025
#   Beskrivelse:  Kører robusthedstests for OLS analysen:
#                 1. Leave-One-Out (Jackknife) test af hovedmodellen.
#                 2. Alternative afstandsmål (Sensitivitetsanalyse).
#                 3. Alternative forklaringer (Kontrolvariabler).
# ---------------------------------------------------------------------------- #


# 1. OPSÆTNING =================================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

# Pakker
library(conflicted) # pakke konflikter
library(here) # robuste filstier
library(tidyverse) # data manipulation og visualisering
library(fixest) # Regressionsmodeller & robuste standardfejl
library(broom) # Tidy model output
library(modelsummary) # Regressionstabeller
library(gt) # Tabelformatering
library(scales) # Formatering af plot akser

# Håndterer konflikter
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Input filstier
DIR_SCRIPTS <- here("scripts")
DIR_DATA <- here("data", "_processed", "ols_data.rds")

# Output filstier
DIR_FIG <- here("_output", "_figures", "_ols_robust")
DIR_TAB <- here("_output", "_tables", "_ols_robust")
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)
if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)

# Indlæser funktioner og temaer
source(file.path(DIR_SCRIPTS, "00_functions.R"))

# Sætter komma som decimal-tegn
options(OutDec = ",")

# Indlæser data
ols_data <- readRDS(DIR_DATA)


# 2. LEAVE-ONE-OUT TEST (JACKKNIFE) ============================================
message("--- Sektion 2: Kører Leave-One-Out Loop ---")

# 2.1. PARAMETRE ---------------------------------------------------------------
# Model der testes (Model 7 Post)
TARGET_FORMULA <- milex_gdp_post ~ dist_enemy_log + border_rus + nato_gap_2021 + gdp_2021_log

# Labels til plot
VARIABLE_LABELS <- c(
  "dist_enemy_log" = "Afstand til strategisk rival (log km)",
  "border_rus"     = "Delt grænse med Rusland",
  "nato_gap_2021"  = "Afstand til NATO's 2%-mål (% BNP), 2021",
  "gdp_2021_log"   = "BNP (log USD), 2021"
)

# Rækkefølge i plot
PLOT_ORDER <- c(
  "Delt grænse med Rusland",
  "Afstand til strategisk rival (log km)",
  "Afstand til NATO's 2%-mål (% BNP), 2021",
  "BNP (log USD), 2021"
)

# 2.2. KØR LOOP ----------------------------------------------------------------
# Beregn baseline koefficienter
true_model <- feols(TARGET_FORMULA, data = ols_data, vcov = "HC3")
true_coeffs <- tidy(true_model) %>%
  filter(term %in% names(VARIABLE_LABELS)) %>%
  transmute(term, true_estimate = estimate, label = VARIABLE_LABELS[term])

# Kør Jackknife
countries <- unique(ols_data$iso3c)
loo_results <- map_dfr(countries, function(dropped_iso) {
  subset_data <- filter(ols_data, iso3c != dropped_iso)
  mod <- feols(TARGET_FORMULA, data = subset_data, vcov = "HC3")

  tidy(mod) %>%
    filter(term %in% names(VARIABLE_LABELS)) %>%
    mutate(dropped_unit = dropped_iso, label = VARIABLE_LABELS[term]) %>%
    select(dropped_unit, label, estimate, p.value)
})

# Klargør data til plot
plot_df <- loo_results %>%
  left_join(true_coeffs, by = "label") %>%
  mutate(
    significance_text = ifelse(
      p.value < 0.05, "Signifikant (p < 0,05)", "Insignifikant (p > 0,05)"
    ),
    significance = factor(significance_text, levels = c(
      "Signifikant (p < 0,05)",
      "Insignifikant (p > 0,05)"
    )),
    label = factor(label, levels = PLOT_ORDER)
  )

# 2.3. GENERER PLOT ------------------------------------------------------------
message("--- Genererer LOO Plot ---")

p_loo <- ggplot(plot_df, aes(x = dropped_unit, y = estimate)) +
  geom_hline(
    aes(yintercept = true_estimate),
    linetype = "dashed", color = "grey40"
  ) +
  geom_point(aes(color = significance), size = 2) +
  facet_wrap(~label, scales = "free_y", ncol = 1) +
  scale_color_manual(
    values = c(
      "Signifikant (p < 0,05)" = "black",
      "Insignifikant (p > 0,05)" = "#c0392b"
    )
  ) +
  scale_y_continuous(
    breaks = function(x) seq(from = x[1], to = x[2], length.out = 3),
    labels = number_format(accuracy = 0.01, decimal.mark = ",")
  ) +
  labs(
    x = "Ekskluderet land",
    y = "Koefficient",
    caption = "Stiplet linje indikerer reel koefficient.",
    color = NULL
  ) +
  ba_theme() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.major.x = element_line()
  )

ggsave(file.path(DIR_FIG, "ols_loo_plot_milex_gdp_post.png"), p_loo, width = 8, height = 9)


# 3. ALTERNATIVE AFSTANDSMÅL ===================================================
message("--- Sektion 3: Estimerer modeller med alternative afstandsmål ---")

# Basis model (uden afstandsvariabel)
base_A <- "milex_gdp_post ~ nato_gap_2021 + border_rus + gdp_2021_log"

# Model 1: Afstand til Rusland
m_rus <- feols(as.formula(paste(base_A, "+ dist_rus_log")), data = ols_data, vcov = "HC3")

# Model 2: Afstand til strategisk rival (Hovedmodel)
m_enemy <- feols(as.formula(paste(base_A, "+ dist_enemy_log")), data = ols_data, vcov = "HC3")

# Model 3: Afstand til konfliktzone
m_conf <- feols(as.formula(paste(base_A, "+ dist_conf_log")), data = ols_data, vcov = "HC3")

# Generer Tabel
tbl_dist <- modelsummary(
  list(
    "1" = m_enemy, # Hovedmodel først
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


# 4. ALTERNATIVE FORKLARINGER ==================================================
message("--- Sektion 4: Estimerer modeller med alternative kontrolvariabler ---")

# Basis model (uden kapacitets-variabel)
base_B <- "milex_gdp_post ~ border_rus + dist_enemy_log + nato_gap_2021"

# 1. BNP (Hovedmodel)
m_gdp <- feols(as.formula(paste(base_B, "+ gdp_2021_log")), data = ols_data, vcov = "HC3")

# 2. BNP pr. indbygger
m_cap <- feols(as.formula(paste(base_B, "+ gdp_cap_2021_log")), data = ols_data, vcov = "HC3")

# 3. BNP vækst
m_growth <- feols(as.formula(paste(base_B, "+ gdp_growth_post")), data = ols_data, vcov = "HC3")

# 4. Offentlig Gæld
m_debt <- feols(as.formula(paste(base_B, "+ debt_gdp_2021_log")), data = ols_data, vcov = "HC3")

# 5. Antal amerikanske tropper
m_troops <- feols(as.formula(paste(base_B, "+ us_troops_2021_log")), data = ols_data, vcov = "HC3")

# 6. Post-Kommunist stat
m_post <- feols(as.formula(paste(base_B, "+ post_com")), data = ols_data, vcov = "HC3")

# Generer Tabel
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


# 5. SCRIPT FÆRDIG =============================================================
message(paste(
  "\n--- Script 10_ols_robust.R færdigt ---",
  "\nAlle tabeller er gemt i:", DIR_TAB,
  "\nAlle figurer er gemt i:", DIR_FIG
))
