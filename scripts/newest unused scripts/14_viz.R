# ---------------------------------------------------------------------------- #
#
#   Project:      BACHELOR PROJEKT
#   Script:       12_viz_ridgeline.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         07-12-2025
#   Description:  Generates Ridgeline Plot showing the evolution of military
#                 spending distributions over time (using NATO data).
#
# ---------------------------------------------------------------------------- #


# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

# Setup paths
DIR_DATA <- here::here("data", "_processed", "master_panel.rds")
DIR_FIG <- here::here("_output", "_figures", "_extra_viz")
DIR_SCRIPTS <- here::here("scripts")

if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

library(here)
library(tidyverse)
library(ggridges) # Special Ridgeline package
library(glue)
library(scales)

# Fallback theme
if (file.exists(file.path(DIR_SCRIPTS, "00_functions.R"))) {
  source(file.path(DIR_SCRIPTS, "00_functions.R"))
} else {
  ba_theme <- function() theme_minimal()
}

# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

master_panel <- readRDS(DIR_DATA)

# Filter for Treatment Group and NATO data
plot_data_ridgeline <- master_panel %>%
  filter(
    group == "Behandlet", # Focus on the treated group (NATO Europe)
    year >= 2014 & year <= 2025 # Include 2025
  ) %>%
  # We use milex_gdp_nato instead of SIPRI milex_gdp
  mutate(
    # Create Factor for Y-axis (Reverse order puts recent years at top)
    year_f = factor(year, levels = rev(sort(unique(year))))
  ) %>%
  # Remove any missing data (just in case)
  drop_na(milex_gdp_nato)


# 3. RIDGELINE PLOT ==========================================================
message("--- Section 3: Generating Ridgeline Plot ---")

ridgeline_plot <- ggplot(
  plot_data_ridgeline,
  aes(x = milex_gdp_nato, y = year_f, fill = after_stat(x))
) +
  ggridges::geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  geom_vline(xintercept = 2.0, linetype = "dashed", color = "black") +
  scale_fill_gradientn(
    colors = c("#ffba08", "#f48c06", "#dc2f02", "#9d0208", "#370617", "black"),
    values = scales::rescale(c(0, 1, 2, 3, 4, 5), from = c(0, 5)),
    limits = c(0, 5),
  ) +
  scale_x_continuous(
    breaks = seq(0, 5, by = 1),
    limits = c(0, 5)
  ) +
  coord_cartesian(clip = "off") +
  ba_theme() +
  theme(legend.position = "none", plot.margin = margin(t = 1)) +
  labs(x = "Forsvarsudgifter (% BNP)", y = NULL)

# Save
ggsave(file.path(DIR_FIG, "milex_gdp_nato_ridgeline.png"),
  ridgeline_plot,
  width = 8, height = 6
)


# 4. SCRIPT COMPLETION =======================================================
message(paste(
  "\n--- Script 12_viz_ridgeline.R finished ---",
  "\nFigure saved to:", DIR_FIG
))
