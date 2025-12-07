# ---------------------------------------------------------------------------- #
#
#   Project:      BACHELOR PROJEKT
#   Script:       14_viz_map_residuals.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         07-12-2025
#   Description:  Generates a Residual Map for the Main OLS Model (Model 7).
#                 Shows which countries over/under-performed relative to the model.
#
# ---------------------------------------------------------------------------- #

# 1. OPSÆTNING =================================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

library(here) # robust filstier
library(tidyverse) # data manipulation og visualiseringer
library(sf) # kort plots
library(rnaturalearth) # kort plots
library(rnaturalearth) # kort data
library(fixest) # To run the model
library(broom) # To extract residuals
library(scales) # plot skalaer
library(gt) # tabelformatering

# Input filstier
DIR_SCRIPTS <- here("scripts")
DIR_DATA <- here("data", "_processed", "ols_data.rds")

# Output filstier
DIR_FIG <- here("_output", "_figures", "_extra_viz")
DIR_TAB <- here("_output", "_tables", "_extra_tables")
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)
if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)

# Indlæser funktioner og brugerdefinerede temaer
source(file.path(DIR_SCRIPTS, "00_functions.R"))

# Sætter komma som decimal-tegn
options(OutDec = ",")


# 2. MODEL ESTIMERING & RESIDUAL BEREGNING =====================================
message("--- Sektion 2: Estimerer Model og Beregner Residualer ---")

ols_data <- readRDS(DIR_DATA)

# A. Run Main Model (Model 7: Post-Invasion)
# We re-run it here to ensure we have the exact object to extract from
main_model <- feols(milex_gdp_post ~ border_rus + dist_enemy_log + nato_gap_2021 + gdp_2021_log,
  data = ols_data, vcov = "HC3"
)

# B. Extract Residuals
# augment() adds .fitted and .resid columns to the original data
data_with_resid <- augment(main_model, data = ols_data) %>%
  select(iso3c, .resid) %>%
  mutate(
    # Create a clean label for the tooltip/legend if needed
    resid_label = round(.resid, 2)
  )


# 3. KLARGØR KORTDATA ==========================================================
message("--- Sektion 3: Klargør Kortdata ---")

# Get Base Map
world_map_sf <- ne_countries(scale = "medium", returnclass = "sf")

# Join Residuals to Map (using adm0_a3 as established in script 13)
map_data <- world_map_sf %>%
  left_join(data_with_resid, by = c("adm0_a3" = "iso3c"))


# ... (Sections 1, 2, and 3 remain the same) ...

# 4. GENERER RESIDUAL KORT =====================================================
message("--- Sektion 4: Generer Residual Kort ---")

# A. Dynamic Scale Calculation
# We find the largest deviation (positive or negative) to make the scale symmetric.
# This ensures that a +0.5 residual has the same color intensity as a -0.5 residual.
max_dev <- max(abs(map_data$.resid), na.rm = TRUE)
limit_range <- c(-max_dev, max_dev)

# Define Europe Crop
europe_crop <- coord_sf(
  xlim = c(-25, 45),
  ylim = c(35, 72),
  expand = FALSE,
  datum = NA
)

p_resid <- ggplot(data = map_data) +
  geom_sf(
    aes(fill = .resid),
    color = "grey60",
    size = 0.2
  ) +
  scale_fill_gradient2(
    low = "#0077b6",
    mid = "white",
    high = "#9d0208",
    midpoint = 0,
    na.value = "#cccccc",
    limits = limit_range,
    labels = number_format(accuracy = 0.1, decimal.mark = ",")
  ) +
  europe_crop +
  ba_theme() +
  theme(legend.position = c(0, 0.7), legend.direction = "vertical")

# Save
ggsave(
  file.path(DIR_FIG, "map_europe_residuals_model7.png"), p_resid,
  width = 6, height = 5.4
)


# 5. GENERER SIMPEL RESIDUAL TABEL =============================================
message("\n--- Sektion 5: Generer Simpel Residual Tabel ---")

# 1. Hent danske landenavne
master_panel <- readRDS(here("data", "_processed", "master_panel.rds"))

danish_names <- master_panel %>%
  select(iso3c, country_dan) %>%
  distinct()

resid_table_data <- data_with_resid %>%
  left_join(danish_names, by = "iso3c") %>%
  select(country_dan, .resid) %>%
  arrange(desc(.resid))

simple_table <- resid_table_data %>%
  gt() %>%
  cols_label(
    country_dan = "",
    .resid = "Residual"
  ) %>%
  fmt_number(
    columns = .resid,
    decimals = 3,
    dec_mark = ","
  ) %>%
  ba_theme_gt() %>%
  gtsave(file.path(DIR_TAB, "simple_residual_list.html"))

message(paste(
  "\n--- Script 14_viz_map_residuals.R finished ---",
  "\nMap saved to:", DIR_FIG,
  "\nTable saved to:", DIR_TAB
))
