# ---------------------------------------------------------------------------- #
#
#   Projekt:      BACHELOR PROJEKT
#   Script:       06_ols_data_trans.R
#   Forfatter:    Frederik Bender Bøeck-Nielsen
#   Dato:         07-12-2025
#   Beskrivelse:  Forbereder data til OLS modeller.
#                 1. Indlæser master_panel.rds og transformerer paneldata om til
#                    tværsnitsdata
#                 2. Opretter OLS-specifikke variabler
#                 3. Gemmer tværsnitsdata som ols_data.rds
#
# ---------------------------------------------------------------------------- #


# 1. OPSÆTNING AF ARBEJDSMILJØ =================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

# Indlæser pakker
library(conflicted) # håndtering af pakke konflikter
library(here) # robuste filstier
library(tidyverse) # data manipulation
library(labelled) # dataframe labels

# Håndterer konflikter
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Input filstier
DIR_DATA_PROC <- here("data", "_processed")
MASTER_PANEL <- file.path(DIR_DATA_PROC, "master_panel.rds")

# Output filsti
OLS_DATA <- file.path(DIR_DATA_PROC, "ols_data.rds")


# 2. DATAFORBEREDELSE ==========================================================
# Indlæser master panel og filtrerer for behandlingsgruppen, samt de
# specifikke variabler og årstal der skal bruges til OLS analysen
message("--- Sektion 2: Indlæser og forbereder data ---")

# Indlæser master panel fra 01_data_prep.R
master_panel <- readRDS(MASTER_PANEL)

# Filtrerer master_panel.rds
df_filtered <- master_panel %>%
  filter(group == "Behandlet") %>%
  filter(year %in% c(2014, 2021, 2024, 2025)) %>%
  select(
    iso3c, country_dan, year, post_com, milex_gdp_nato, milex_usd_log,
    border_rus, dist_rus, dist_blr, dist_ukr, gdp, gdp_local, gdp_cap, debt_gdp,
    us_troops
  )

# Omdanner til tværsnits datasæt
df_wide <- df_filtered %>%
  pivot_wider(
    id_cols = c(
      iso3c, country_dan, post_com, border_rus, dist_rus, dist_blr, dist_ukr
    ),
    names_from = year,
    values_from = c(
      milex_gdp_nato, milex_usd_log, gdp, gdp_local, gdp_cap, debt_gdp, us_troops
    )
  )


# 3. DATA TRANSFORMATION =======================================================
message("--- Sektion 3: Opretter OLS variabler og labels ---")

ols_data <- df_wide %>%
  mutate(
    # Opretter afhængige variabler
    milex_gdp_pre      = milex_gdp_nato_2021 - milex_gdp_nato_2014,
    milex_gdp_post     = milex_gdp_nato_2025 - milex_gdp_nato_2021,
    milex_usd_pre      = milex_usd_log_2021 - milex_usd_log_2014,
    milex_usd_post     = milex_usd_log_2024 - milex_usd_log_2021,

    # Opretter og log-transformerer kombinerede afstandsvariabler
    dist_enemy         = pmin(dist_rus, dist_blr, na.rm = TRUE),
    dist_conf          = pmin(dist_rus, dist_blr, dist_ukr, na.rm = TRUE),
    dist_rus_log       = log(dist_rus),
    dist_enemy_log     = log(dist_enemy),
    dist_conf_log      = log(dist_conf),

    # NATO-mål variabler (censurerer værdier >2 til 0, da det antages at lande
    # der bruger >2% ikke føler et "negativt press"/pres til at sænke udgifter).
    nato_gap_2014      = pmax(0, 2.0 - milex_gdp_nato_2014),
    nato_gap_2021      = pmax(0, 2.0 - milex_gdp_nato_2021),

    # Log-transformerer økonomiske variabler og "us_troops"
    gdp_2014_log       = log(gdp_2014),
    gdp_2021_log       = log(gdp_2021),
    gdp_cap_2014_log   = log(gdp_cap_2014),
    gdp_cap_2021_log   = log(gdp_cap_2021),
    debt_gdp_2021_log  = log(debt_gdp_2021),
    us_troops_2021_log = log(us_troops_2021),

    # BNP-vækst (log-differense) for at teste nævner-effekt
    gdp_growth_post    = log(gdp_local_2025) - log(gdp_local_2021)
  ) %>%
  select(
    iso3c, country_dan, milex_gdp_pre, milex_gdp_post, milex_usd_pre,
    milex_usd_post, border_rus, dist_rus_log, dist_enemy, dist_enemy_log,
    dist_conf_log, nato_gap_2014, nato_gap_2021, gdp_2014_log, gdp_2021_log,
    us_troops_2021_log, gdp_growth_post, gdp_cap_2021_log, debt_gdp_2021_log,
    post_com
  )

# Tilføjer labels til nye variabler
var_labels <- list(
  milex_gdp_pre      = "Ændring i forsvarsudgifter (% BNP), 2014-21 - kilde: NATO",
  milex_gdp_post     = "Ændring i forsvarsudgifter (% BNP), 2021-25 - kilde: NATO",
  milex_usd_pre      = "Ændring i forsvarsudgifter (log USD), 2014-21 - kilde: SIPRI",
  milex_usd_post     = "Ændring i forsvarsudgifter (log USD), 2021-24 - kilde: SIPRI",
  dist_rus_log       = "Afstand til Rusland (log km) - kilde: CEPII",
  dist_enemy_log     = "Afstand til strategisk rival (log km) - kilde: CEPII",
  dist_conf_log      = "Afstand til konfliktzone (log km) - kilde: CEPII",
  nato_gap_2014      = "Afstand til NATO's 2%-mål (% BNP), 2014 - kilde: NATO",
  nato_gap_2021      = "Afstand til NATO's 2%-mål (% BNP), 2021 - kilde: NATO",
  gdp_2014_log       = "BNP (log USD), 2014 - kilde: IMF",
  gdp_2021_log       = "BNP (log USD), 2021 - kilde: IMF",
  gdp_growth_post    = "BNP-vækst (%), 2021-25 - kilde: IMF",
  gdp_cap_2021_log   = "BNP pr. indbygger (log PPP), 2021 - kilde: IMF",
  debt_gdp_2021_log  = "Offentlig gæld (log % BNP), 2021 - kilde: IMF",
  us_troops_2021_log = "Antal amerikanske tropper (log), 2021 - kilde: DMDC"
)

ols_data <- ols_data %>% set_variable_labels(.labels = var_labels)


# 4. GEM OLS DATASÆT ===========================================================

message("\nOLS datasæt struktur:")
glimpse(ols_data)

message("\nManglende værdier i OLS datasæt:") # Ingen NAs
ols_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_count") %>%
  filter(na_count > 0) %>%
  print()

saveRDS(ols_data, file = OLS_DATA)


# 5. SCRIPT FÆRDIGT ============================================================

message(paste0(
  "\n--- Script 06_ols_data_trans færdigt ---",
  "\nTværsnits OLS datasæt skabt med N = ", nrow(ols_data), " lande",
  "\nGemt til: ", OLS_DATA
))
