# ---------------------------------------------------------------------------- #
#   Projekt:      BACHELOR PROJEKT
#   Script:       11_data_validation.R
#   Beskrivelse:  Validering af NATO data mod SIPRI data
#                 1. Beregner korrelation (2014-2024)
#                 2. Plotter aggregerede trends for Behandlet gruppe (2014-2025)
# ---------------------------------------------------------------------------- #

# 1. OPSÆTNING =================================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

library(here)
library(tidyverse)
library(glue)
library(scales)

# Fallback theme
if (file.exists(here("scripts", "00_functions.R"))) {
  source(here("scripts", "00_functions.R"))
} else {
  ba_theme <- function() theme_minimal()
}

# Load MASTER PANEL (Not OLS data)
DIR_DATA <- here("data", "_processed", "master_panel.rds")
DIR_FIG <- here("_output", "_figures", "_extra_viz")
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

master_panel <- readRDS(DIR_DATA)


# 2. DATAFORBEREDELSE ==========================================================
message("--- Sektion 2: Forbereder data til sammenligning ---")

# Filtrer til Behandlet gruppe og relevant periode
# Vi skal bruge 2014-2025 for plottet, men 2014-2024 for korrelation
df_val <- master_panel %>%
  filter(
    group == "Behandlet",
    year >= 2014 & year <= 2025
  ) %>%
  select(iso3c, year, sipri = milex_gdp, nato = milex_gdp_nato)

# 3. BEREGN KORRELATION (2014-2024) ============================================
message("--- Sektion 3: Beregner Korrelation ---")

# Vi ekskluderer 2025 eksplicit for korrelationstesten
cor_data <- df_val %>%
  filter(year < 2025) %>%
  drop_na(sipri, nato)

# Beregn Pearson R
r_val <- cor(cor_data$sipri, cor_data$nato, method = "pearson")
r_text <- number(r_val, accuracy = 0.01, decimal.mark = ",")

message(glue("Pearson Korrelation (2014-2024): {r_text}"))


# 4. AGGREGER TRENDS TIL PLOT ==================================================
message("--- Sektion 4: Generer Sammenligningsplot ---")

# Beregn gennemsnit pr. år for begge kilder
df_plot <- df_val %>%
  group_by(year) %>%
  summarize(
    SIPRI = mean(sipri, na.rm = TRUE),
    NATO  = mean(nato, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(SIPRI, NATO), names_to = "Kilde", values_to = "pct_bnp") %>%
  filter(!is.na(pct_bnp))

# Plot
p_val <- ggplot(df_plot, aes(x = year, y = pct_bnp, color = Kilde, linetype = Kilde)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("SIPRI" = "grey40", "NATO" = "black")) +
  scale_linetype_manual(values = c("SIPRI" = "solid", "NATO" = "solid")) +
  scale_x_continuous(breaks = 2014:2025) +
  scale_y_continuous(labels = number_format(accuracy = 0.1, decimal.mark = ",")) +
  annotate(
    "text",
    x = 2016, y = max(df_plot$pct_bnp),
    label = glue("2014-24 korrelation (Pearson r) = {r_text}"),
    hjust = 0, vjust = 11, family = "IBM Plex Serif", size = 11 / .pt, color = "black"
  ) +
  labs(
    y = "Forsvarsudgifter (% BNP)",
    x = NULL,
    color = NULL,
    linetype = NULL,
    caption = glue("Sammenligning af behandlingsgruppens årlige gennemsnit på tværs af datakilder.")
  ) +
  ba_theme()

# Gem
ggsave(file.path(DIR_FIG, "validation_sipri_vs_nato.png"),
  p_val,
  width = 6, height = 5
)

message("Plot gemt til: _output/_figures/_validation/validation_sipri_vs_nato.png")
