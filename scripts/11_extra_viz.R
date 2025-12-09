# ---------------------------------------------------------------------------- #
#
#   Projekt:      BACHELOR PROJEKT
#   Script:       11_extra_viz.R
#   Forfatter:    Frederik Bender Bøeck-Nielsen
#   Dato:         08-12-2025
#   Beskrivelse:  Genererer yderligere visualiseringer for OLS-analysen:
#                 1. Coefficient Plot (Forest Plot) - Sammenligning 2014/2021
#                 2. Residual Map - Geografisk fordeling af modelafvigelser
#                 3. Residual Tabel - Liste over lande rangeret efter residual
# ---------------------------------------------------------------------------- #


# 1. OPSÆTNING =================================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

# Indlæser pakker
library(here) # Robuste filstier
library(tidyverse) # Data manipulation og visualisering
library(fixest) # Estimering af modeller med robuste standardfejl
library(broom) # Udtræk af koefficienter og residualer i tidy format
library(glue) # Formatér beskeder og tekststrenge
library(sf) # Håndtering af geografiske data (Simple Features)
library(rnaturalearth) # Kortdata (grænser og lande)
library(rnaturalearthdata) # Nødvendigt datasæt til rnaturalearth
library(scales) # Formatering af akser og labels (procenter, kommaer)
library(gt) # Tabelformatering

# Input filstier
DIR_SCRIPTS <- here("scripts")
DIR_DATA <- here("data", "_processed", "ols_data.rds")
DIR_MASTER <- here("data", "_processed", "master_panel.rds")

# Output filstier
DIR_FIG <- here("_output", "_figures", "_ols_models")
DIR_TAB <- here("_output", "_tables", "_ols_models")
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)
if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)

# Indlæser funktioner og brugerdefinerede temaer
source(file.path(DIR_SCRIPTS, "00_functions.R"))

# Sætter komma som decimal-tegn
options(OutDec = ",")


# 2. ESTIMER MODELLER (MODEL 7) ================================================
message("--- Sektion 2: Estimerer modeller ---")

# Indlæs OLS data
ols_data <- readRDS(DIR_DATA)

# A. Pre-Invasion (2014-2021)
mod_pre <- feols(milex_gdp_pre ~ border_rus + dist_enemy_log + nato_gap_2014 + gdp_2014_log,
  data = ols_data, vcov = "HC3"
)

# B. Post-Invasion (2021-2025)
mod_post <- feols(milex_gdp_post ~ border_rus + dist_enemy_log + nato_gap_2021 + gdp_2021_log,
  data = ols_data, vcov = "HC3"
)


# 3. FOREST PLOT KOEFFICIENT SAMMENLIGNING =====================================
message("--- Sektion 3: Genererer Forest Plot ---")

## 3.1. HJÆLPEFUNKTION ----------------------------------------------------------
get_tidy_data <- function(model, period_label) {
  tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
    mutate(period = period_label) %>%
    filter(term != "(Intercept)")
}

# Udtræk data
df_pre <- get_tidy_data(mod_pre, "Præ-invasion (2014-21)")
df_post <- get_tidy_data(mod_post, "Post-invasion (2021-25)")

# Saml og rens
plot_data <- bind_rows(df_pre, df_post) %>%
  mutate(
    term_common = case_when(
      term == "dist_enemy_log" ~ "dist_enemy",
      term == "border_rus" ~ "border",
      str_detect(term, "nato_gap") ~ "nato",
      str_detect(term, "gdp_") ~ "gdp",
      TRUE ~ term
    ),
    term_label = case_when(
      term_common == "border" ~ "Delt grænse med Rusland",
      term_common == "dist_enemy" ~ "Afstand til strategisk rival (log km)",
      term_common == "nato" ~ "Afstand til NATO's 2%-mål (% BNP)",
      term_common == "gdp" ~ "BNP (log USD)"
    ),
    term_label = factor(term_label, levels = c(
      "BNP (log USD)",
      "Afstand til NATO's 2%-mål (% BNP)",
      "Afstand til strategisk rival (log km)",
      "Delt grænse med Rusland"
    )),
    period = factor(period, levels = c(
      "Post-invasion (2021-25)",
      "Præ-invasion (2014-21)"
    ))
  )

# Generér plot
p_forest <- ggplot(plot_data, aes(x = estimate, y = term_label, color = period)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.2),
    size = 0.5,
    fatten = 1.5
  ) +
  scale_color_manual(
    values = c(
      "Præ-invasion (2014-21)" = "grey30",
      "Post-invasion (2021-25)" = "black"
    )
  ) +
  guides(color = guide_legend(reverse = TRUE, nrow = 1)) +
  scale_x_continuous(
    breaks = c(-0.5, 0, 0.5, 1, 1.5),
    labels = number_format(decimal.mark = ",")
  ) +
  labs(x = "Koefficient", y = NULL, color = NULL) +
  ba_theme() +
  theme(legend.position = c(-0.8, -0.08), plot.margin = margin(b = 16))

ggsave(
  file.path(DIR_FIG, "ols_coef_comparison_pre_post.png"), p_forest,
  width = 6, height = 4.5
)

# 4. RESIDUAL KORT =============================================================
message("--- Sektion 4: Genererer residual kort ---")

# Udtræk residualer fra Forsvarsudgifter (% BNP), 2021-25 model
data_with_resid <- augment(mod_post, data = ols_data) %>%
  select(iso3c, .resid) %>%
  mutate(resid_label = round(.resid, 2))

# Forbered kort data
world_map_sf <- ne_countries(scale = "medium", returnclass = "sf")
map_data <- world_map_sf %>%
  left_join(data_with_resid, by = c("adm0_a3" = "iso3c"))

# Beregn limits
max_dev <- max(abs(map_data$.resid), na.rm = TRUE)
limit_range <- c(-max_dev, max_dev)

# Definér Europa crop
europe_crop <- coord_sf(
  xlim = c(-25, 45),
  ylim = c(35, 72),
  expand = FALSE,
  datum = NA
)

# Generér kort
p_resid <- ggplot(data = map_data) +
  geom_sf(
    aes(fill = .resid),
    color = "black",
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

ggsave(
  file.path(DIR_FIG, "map_residuals.png"), p_resid,
  width = 6, height = 5.4
)


# 5. RESIDUAL TABEL ============================================================
message("--- Sektion 5: Genererer  residual tabel ---")

# Danske landenavne
master_panel <- readRDS(DIR_MASTER)
danish_names <- master_panel %>%
  select(iso3c, country_dan) %>%
  distinct()

# Forbered data
resid_table_data <- data_with_resid %>%
  left_join(danish_names, by = "iso3c") %>%
  select(country_dan, .resid) %>%
  arrange(desc(.resid))

# Lav tabel
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
  gtsave(file.path(DIR_TAB, "residual_list.html"))


# 6. SCRIPT FÆRDIG =============================================================
message(paste(
  "\n--- Script 11_extra_viz.R færdigt ---",
  "\nAlle tabeller er gemt i:", DIR_TAB,
  "\nAlle figurer er gemt i:", DIR_FIG
))
