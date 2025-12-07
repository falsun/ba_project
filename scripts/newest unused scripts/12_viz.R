# ---------------------------------------------------------------------------- #
#   Projekt:      BACHELOR PROJEKT
#   Script:       10_ols_coef_plot.R
#   Beskrivelse:  Genererer Coefficient Plot (Forest Plot)
#                 Sammenligner effekter mellem 2014-21 og 2021-25
# ---------------------------------------------------------------------------- #

# 1. OPSÆTNING =================================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

library(here)
library(tidyverse)
library(fixest) # For model estimation
library(broom) # For extracting coefficients
library(glue)

# Source Theme Function (fallback if missing)
if (file.exists(here("scripts", "00_functions.R"))) {
  source(here("scripts", "00_functions.R"))
} else {
  ba_theme <- function() theme_minimal() # Fallback
}

# Load Data
DIR_DATA <- here("data", "_processed", "ols_data.rds")
DIR_FIG <- here("_output", "_figures", "_extra_viz")
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

ols_data <- readRDS(DIR_DATA)


# 2. ESTIMER MODELLER (MODEL 7) ================================================
message("--- Sektion 2: Estimerer modeller ---")

# Vi estimerer kun Model 7 (den fulde specifikation) for begge perioder

# A. Pre-Invasion (2014-2021)
# Outcome: milex_gdp_pre
# Controls: nato_gap_2014, gdp_2014_log
mod_pre <- feols(milex_gdp_pre ~ border_rus + dist_enemy_log + nato_gap_2014 + gdp_2014_log,
  data = ols_data, vcov = "HC3"
)

# B. Post-Invasion (2021-2025)
# Outcome: milex_gdp_post
# Controls: nato_gap_2021, gdp_2021_log
mod_post <- feols(milex_gdp_post ~ border_rus + dist_enemy_log + nato_gap_2021 + gdp_2021_log,
  data = ols_data, vcov = "HC3"
)


# 3. DATA WRANGLING TIL PLOT ===================================================
message("--- Sektion 3: Klargør data til plot ---")

get_tidy_data <- function(model, period_label) {
  tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
    mutate(period = period_label) %>%
    filter(term != "(Intercept)")
}

# 1. Extract data
df_pre <- get_tidy_data(mod_pre, "Præ-invasion (2014-21)")
df_post <- get_tidy_data(mod_post, "Post-invasion (2021-25)")

# 2. Combine and Clean
plot_data <- bind_rows(df_pre, df_post) %>%
  mutate(
    # Create Common Terms
    term_common = case_when(
      term == "dist_enemy_log" ~ "dist_enemy",
      term == "border_rus" ~ "border",
      str_detect(term, "nato_gap") ~ "nato",
      str_detect(term, "gdp_") ~ "gdp",
      TRUE ~ term
    ),

    # Create Nice Labels
    term_label = case_when(
      term_common == "border" ~ "Delt grænse med Rusland",
      term_common == "dist_enemy" ~ "Afstand til strategisk rival (log km)",
      term_common == "nato" ~ "Afstand til NATO's 2%-mål (% BNP)",
      term_common == "gdp" ~ "BNP (log USD)"
    ),

    # 1. Variable Order (Top to Bottom visual = Bottom to Top factor)
    term_label = factor(term_label, levels = c(
      "BNP (log USD)",
      "Afstand til NATO's 2%-mål (% BNP)",
      "Afstand til strategisk rival (log km)",
      "Delt grænse med Rusland"
    )),

    # 2. Period Order (Dodging Logic)
    # ggplot places the 2nd Factor Level "On Top" (Higher Y offset).
    # We want 2014 on top, so it must be the SECOND level.
    period = factor(period, levels = c("Post-invasion (2021-25)", "Præ-invasion (2014-21)"))
  )


# 4. GENERER PLOT ==============================================================
message("--- Sektion 4: Generer Coefficient Plot ---")

p_forest <- ggplot(plot_data, aes(x = estimate, y = term_label, color = period)) +

  # A. Reference Line
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +

  # B. Forest Points
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.2),
    size = 0.5,
    fatten = 1.5
  ) +

  # C. Colors & Legend Fix
  scale_color_manual(
    values = c("Præ-invasion (2014-21)" = "grey30", "Post-invasion (2021-25)" = "black")
  ) +
  guides(color = guide_legend(reverse = TRUE, nrow = 1)) +


  # D. Scales
  scale_x_continuous(
    breaks = c(-0.5, 0, 0.5, 1, 1.5),
    labels = scales::number_format(decimal.mark = ",")
  ) +
  labs(
    x = "Koefficient",
    y = NULL,
    color = NULL
  ) +
  ba_theme() +
  theme(
    legend.position = c(-0.8, -0.08),
    plot.margin = margin(b = 16)
  )
ggsave(file.path(DIR_FIG, "ols_coef_comparison_pre_post.png"),
  p_forest,
  width = 6, height = 4, bg = "white"
)

message("Plot saved to: _output/_figures/_ols_plots/ols_coef_comparison_pre_post.png")
