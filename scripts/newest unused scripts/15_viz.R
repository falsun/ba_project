# ---------------------------------------------------------------------------- #
#
#   Project:      BACHELOR PROJEKT
#   Script:       13_viz_map_europe.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         07-12-2025
#   Description:  Generates a choropleth map of Europe showing the change in
#                 military spending (milex_gdp_post, 2021-2025).
#
# ---------------------------------------------------------------------------- #

# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

library(here)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(scales)
library(glue)

# Define paths
DIR_DATA <- here("data", "_processed", "ols_data.rds")
DIR_FIG <- here("_output", "_figures", "_extra_viz")
DIR_SCRIPTS <- here("scripts")

if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

# Fallback theme
if (file.exists(file.path(DIR_SCRIPTS, "00_functions.R"))) {
  source(file.path(DIR_SCRIPTS, "00_functions.R"))
} else {
  ba_theme <- function() theme_minimal()
}

# 2. LOAD & PREPARE DATA =====================================================
message("--- Section 2: Loading Data & Map Shapes ---")

# A. Load OLS Data
ols_data <- readRDS(DIR_DATA)

# B. Get Base Map Data
world_map_sf <- ne_countries(scale = "medium", returnclass = "sf")

# C. Join Data (THE FIX)
# We join on 'adm0_a3' instead of 'iso_a3'.
# 'iso_a3' often has -99 for France/Norway in this dataset.
map_data_joined <- world_map_sf %>%
  left_join(ols_data, by = c("adm0_a3" = "iso3c"))


# 3. GENERATE DIVERGING MAP ==================================================
message("--- Section 3: Generating European Map ---")

europe_crop <- coord_sf(
  xlim = c(-25, 45),
  ylim = c(35, 72),
  expand = FALSE,
  datum = NA
)

map_plot <- ggplot(data = map_data_joined) +
  geom_sf(aes(fill = milex_gdp_post), color = "grey60", size = 0.2) +
  scale_fill_gradient(
    low = "white",
    high = "#9d0208",
    na.value = "#cccccc",
    limits = c(0, NA),
    labels = scales::number_format(accuracy = 0.1, decimal.mark = ",")
  ) +
  europe_crop +
  ba_theme() +
  theme(legend.position = c(0, 0.7), legend.direction = "vertical")

ggsave(file.path(DIR_FIG, "map_europe_milex_change_2021_25.png"),
  map_plot,
  width = 6, height = 5.4
)

message(paste(
  "\n--- Script 13_viz_map_europe.R finished ---",
  "\nMap saved to:", DIR_FIG
))
