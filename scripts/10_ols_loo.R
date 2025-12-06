# ---------------------------------------------------------------------------- #
#   Projekt:      BACHELOR PROJEKT
#   Script:       10_ols_loo.R
#   Forfatter:    Frederik Bender Bøeck-Nielsen
#   Dato:         06-12-2025
#   Beskrivelse:  Kører Leave-One-Out test (Jackknife) på den fulde
#                 "forsvarsudgifter (% BNP) 2021-25" OLS og gemmer resultater i
#                 et faceted plot.
# ---------------------------------------------------------------------------- #


# 1. OPSÆTNING =================================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

# pakker
library(here) # robuste filstier
library(tidyverse) # data manipulation og plots
library(fixest) # regressionsmodeller
library(broom)
library(scales) # plot skalaer

# Funktioner og brugerdefinerede temaer
source(here("scripts", "00_functions.R"))

# Filstier
DIR_DATA <- here("data", "_processed", "ols_data.rds")
DIR_FIG <- here("_output", "_figures", "_ols_plots")
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

# Data
ols_data <- readRDS(DIR_DATA)


# 1.1. PARAMETRE ---------------------------------------------------------------

# Model der testes
TARGET_FORMULA <- milex_gdp_post ~ dist_enemy_log + border_rus + nato_gap_2021 + gdp_2021_log

# Variabler
VARIABLE_LABELS <- c(
  "dist_enemy_log" = "Afstand til strategisk rival (log km)",
  "border_rus"     = "Fælles grænse med strategisk rival",
  "nato_gap_2021"  = "Afstand til NATO's 2%-mål (% BNP), 2021",
  "gdp_2021_log"   = "BNP (log USD), 2021"
)

# Rækkefølge i plot
PLOT_ORDER <- c(
  "Fælles grænse med strategisk rival",
  "Afstand til strategisk rival (log km)",
  "Afstand til NATO's 2%-mål (% BNP), 2021",
  "BNP (log USD), 2021"
)


# 2. LEAVE-ONE-OUT TEST ========================================================
message("--- Sektion 2: Kører Leave-One-Out Loop ---")

# Beregn baseline koefficienter
true_model <- feols(TARGET_FORMULA, data = ols_data, vcov = "HC3")
true_coeffs <- tidy(true_model) %>%
  filter(term %in% names(VARIABLE_LABELS)) %>%
  transmute(term, true_estimate = estimate, label = VARIABLE_LABELS[term])

# Kør Loop
countries <- unique(ols_data$iso3c)
loo_results <- map_dfr(countries, function(dropped_iso) {
  subset_data <- filter(ols_data, iso3c != dropped_iso)
  mod <- feols(TARGET_FORMULA, data = subset_data, vcov = "HC3")
  # Udtræk resultater
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


# 3. LOO PLOT ==================================================================
message("--- Sektion 4: Genererer LOO Plot ---")

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

# Gem
ggsave(file.path(DIR_FIG, "ols_loo_plot.png"), p_loo, width = 8, height = 9)

# 4. SCRIPT FÆRDIG =============================================================
message(paste(
  "\n--- Script 10_ols_loo.R færdigt ---",
  "\n ols_loo_stability.png gemt i:", DIR_FIG
))
