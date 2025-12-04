# ---------------------------------------------------------------------------- #
#
#   Project:     NATO Defence Spending Bachelor's Thesis
#   Script:      04_viz.R
#   Author:      Frederik Bender Bøeck-Nielsen
#   Date:        2025-10-17
#   Description: Generates key time-series visualizations for the analysis,
#                such as parallel trends plots.
#
# ---------------------------------------------------------------------------- #


# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

TREATMENT_YEAR <- 2022

DIR_DATA       <- here::here("data", "_processed")
DIR_SCRIPTS    <- here::here("scripts")
DIR_FIG        <- here::here("_output", "_figures")

if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

MASTER_PANEL <- file.path(DIR_DATA, "master_panel.rds")


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, conflicted, ggridges, glue, scales)
conflict_prefer("filter", "dplyr")

source(file.path(DIR_SCRIPTS, "00_functions.R"))


# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

master_panel <- readRDS(MASTER_PANEL)


# 3. RIDGELINE PLOT ==========================================================
message("--- Section 3: Generating Ridgeline Plot ---")

plot_data_ridgeline <- master_panel %>%
  filter(group == "treatment") %>%
  mutate(year_f = factor(year, levels = rev(sort(unique(year)))))

ridgeline_plot <- ggplot(plot_data_ridgeline, aes(x = milex_gdp, y = year_f, fill = after_stat(x))) +
  ggridges::geom_density_ridges_gradient(
    scale = 3,
    rel_min_height = 0.01
  ) +
  geom_vline(xintercept = 2, linetype = "dashed", color = "black") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks = seq(0, 5, by = 1), limits = c(0, 5), labels = scales::label_percent(scale = 1)) +
  scale_y_discrete(
    breaks = c("2024", "2022", "2020", "2018", "2016", "2014")
  ) +
  scale_fill_gradientn(
    colors = c("#ffba08", "#f48c06", "#dc2f02", "#9d0208", "#370617", "black"),
    values = scales::rescale(c(0, 1, 2, 3, 4, 5), from = c(0, 5)),
    limits = c(0, 5),
  ) +
  theme_bachelor_project() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank()) +
  labs(
    title = "Distribution of Military Expenditure (% of GDP) Over Time.",
    x = NULL,
    y = NULL
  )

ggsave(file.path(DIR_FIG, "milex_gdp_ridgeline.png"), ridgeline_plot, width = 8, height = 6, bg = "white")

# 4. SCRIPT COMPLETION =======================================================
message(paste("\n--- Script 04_viz.R finished ---",
              "\nAll figures saved to:", DIR_FIG))

