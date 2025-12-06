# ---------------------------------------------------------------------------- #
#
#   Projekt:      BACHELOR PROJEKT
#   Script:       01_data_prep.R
#   Forfatter:    Frederik Bender Bøeck-Nielsen
#   Dato:         05-12-2025
#   Beskrivelse:  1. Indlæser og renser paneldata til analysen.
#                 2. Gemmer al data i master_panel.rds.
#                 3. Gemmer dedikeret es_panel.rds til event-study analysen.
#
# ---------------------------------------------------------------------------- #


# 1. OPSÆTNING AF ARBEJDSMILJØ =================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

# Indlæser pakker
library(conflicted) # håndtering af pakke konflikter
library(here) # robuste filstier
library(tidyverse) # data manipulation
library(readxl) # læs excel filer
library(janitor) # rens dataframes
library(countrycode) # konverter landekoder
library(haven) # .dta filer
library(troopdata) # DMDC data for antal amerikanske soldater
library(labelled) # dataframe labels

# håndterer konflikter
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Input filstier
DIR_SCRIPTS <- here("scripts")
DIR_DATA_RAW <- here("data", "raw")
COUNTRY_SAMPLE <- file.path(DIR_DATA_RAW, "country_sample.xlsx")
MILEX_SIPRI <- file.path(DIR_DATA_RAW, "sipri_milex_2025p.xlsx")
MILEX_NATO <- file.path(DIR_DATA_RAW, "nato_milex_2025-08-28p.xlsx")
DIST_CEPII <- file.path(DIR_DATA_RAW, "dist_cepii.dta")
ECON_IMF <- file.path(DIR_DATA_RAW, "imf_weo_2025-10-14p.csv")

# Output filstier
DIR_DATA_PROC <- here("data", "_processed")
if (!dir.exists(DIR_DATA_PROC)) dir.create(DIR_DATA_PROC, recursive = TRUE)
MASTER_PANEL <- file.path(DIR_DATA_PROC, "master_panel.rds")
ES_PANEL <- file.path(DIR_DATA_PROC, "es_panel.rds")

# Indlæser funktioner
source(file.path(DIR_SCRIPTS, "00_functions.R"))

# Definerer start år, behandlingsår og slut år
START_YEAR <- 2014 # Anneksering af Krim og Wales Summit
TREAT_YEAR <- 2022 # Fuldskala invasion af Ukraine
END_YEAR <- 2025 # Nuværende årstal

# Manuel frasotering
TREATMENT_SEC_MAN <- c("GRC", "BGR")
CONTROL_SEC_MAN <- c("CHL", "ISR", "MEX", "TUR", "USA")


# 2. DATARENS HJÆLPEFUNKTION ===================================================
# Samler renset data og giver besked i konsollen hvis der er manglende værdier.
message("--- Sektion 2: Definerer hjælpefunktion til datarens---")

merge_and_validate <- function(master_df, new_df, merge_by, source_name) {
  message(paste0("--> Fletter ", source_name, " data sammen med paneldata skabelon"))
  new_df <- new_df %>%
    select(all_of(merge_by), everything())
  new_cols <- setdiff(names(new_df), merge_by)
  merged_df <- left_join(master_df, new_df, by = merge_by)
  missing_data <- merged_df %>%
    filter(if_any(all_of(new_cols), is.na)) %>%
    select(iso3c, year, all_of(new_cols))
  if (nrow(missing_data) > 0) {
    warning(paste("ADVARSEL: Manglende værdier for ", source_name, "data:"))
    print(missing_data, n = 999)
  } else {
    message(paste("SUCCES: Ingen manglende værdier for ", source_name, "data."))
  }
  return(merged_df)
}


# 3. PANELDATA SKABELON ========================================================
# Indlæser country_sample.xlsx (eget datasæt), som indeholder information om
# analysens population (landekoder, geografisk lokation, årstal for NATO- og
# OECD-medlemskab, m.m.). Derefter oprettes en paneldata skabelon for at
# strømline datarens. Link til country_sample.xlsx findes i README.txt.
message("--- Sektion 3: Definerer population og opretter paneldata skabelon ---")

# definerer grupper
country_sample <- read_excel(COUNTRY_SAMPLE, sheet = "sample") %>%
  clean_names() %>%
  mutate(
    across(ends_with("_year"), as.numeric),
    group = case_when(
      region == "Europe" & nato_member_year < START_YEAR ~ "Behandlet",
      region == "Europe" & nato_member_year >= START_YEAR ~ "Behandlet_sek",
      oecd_member_year <= START_YEAR ~ "Kontrol",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    group = case_when(
      iso3c %in% TREATMENT_SEC_MAN ~ "Behandlet_sek",
      iso3c %in% CONTROL_SEC_MAN ~ "Kontrol_sek",
      TRUE ~ group
    )
  ) %>%
  filter(!is.na(group))

# printer grupper til konsol
message("Gruppe komposition (_sek-grupperne indgår ikke i analysen):")
country_sample %>%
  count(group, name = "antal lande (n)") %>%
  print()

# gemmer liste med landekoder
SAMPLE_ISO3C <- unique(country_sample$iso3c)

# Opretter paneldata skabelon
clean_panel <- country_sample %>%
  select(group, iso3c, country_dan, post_com) %>%
  crossing(year = START_YEAR:END_YEAR) %>%
  mutate(
    treat_dummy = ifelse(group == "Behandlet", 1, 0),
    event_time = year - TREAT_YEAR
  ) %>%
  arrange(group, iso3c, year)


# 4. DATAFORBEREDELSE ==========================================================
# Indlæser og renser al data. SAMPLE_ISO3C (liste med landekoder) anvendes til
# at filtrere dataen, mens funktionen merge_and_validate() anvendes til at
# flette dataen sammen og tjekke for manglende værdier.
message("--- Sektion 4: Indlæser, renser og kombinerer data ---")


## 4.1. SIPRI DATA -------------------------------------------------------------
# Forsvarsudgifter (mio. USD, 2023-priser)

sipri_usd <- read_excel(MILEX_SIPRI, sheet = "Constant (2023) US$", skip = 5) %>%
  clean_names() %>%
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    values_to = "milex_usd",
    names_prefix = "x",
    names_transform = as.integer
  ) %>%
  mutate(
    iso3c = countrycode(country, "country.name", "iso3c"),
    milex_usd = as.numeric(milex_usd) * 1e6
  ) %>%
  filter(iso3c %in% SAMPLE_ISO3C, between(year, START_YEAR, END_YEAR)) %>%
  select(iso3c, year, milex_usd)

# Sammenfletter SIPRI (USD) data med panel skabelon
clean_panel <- merge_and_validate(
  clean_panel,
  sipri_usd,
  c("iso3c", "year"),
  "SIPRI (USD)"
) # Alle 2025 obs. er NA

# Forsvarsudgifter (% BNP)
sipri_gdp <- read_excel(MILEX_SIPRI, sheet = "Share of GDP", skip = 5) %>%
  clean_names() %>%
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    values_to = "milex_gdp",
    names_prefix = "x",
    names_transform = as.integer
  ) %>%
  mutate(
    iso3c = countrycode(country, "country.name", "iso3c"),
    milex_gdp = as.numeric(milex_gdp) * 100
  ) %>%
  filter(iso3c %in% SAMPLE_ISO3C, between(year, START_YEAR, END_YEAR)) %>%
  select(iso3c, year, milex_gdp)

# Sammenfletter SIPRI (% BNP) data med panel skabelon
clean_panel <- merge_and_validate(
  clean_panel,
  sipri_gdp,
  c("iso3c", "year"),
  "SIPRI (GDP)"
) # Alle 2025 obs. er NA


## 4.2. NATO DATA --------------------------------------------------------------
# Forsvarsudgifter (% BNP)

nato_clean <- read_excel(
  MILEX_NATO,
  sheet = "Table 3",
  range = "A4:M35",
  col_names = TRUE
) %>%
  rename(country = ...1) %>%
  filter(!is.na(country), !str_detect(country, "Footnote")) %>%
  mutate(country = str_remove_all(country, "\\*")) %>%
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "milex_gdp_nato"
  ) %>%
  mutate(
    year = as.numeric(str_remove_all(year, "[^0-9]")),
    iso3c = countrycode(
      country,
      origin = "country.name",
      destination = "iso3c",
      custom_match = c("Türkiye" = "TUR")
    ),
    milex_gdp_nato = as.numeric(milex_gdp_nato)
  ) %>%
  filter(iso3c %in% SAMPLE_ISO3C, year >= START_YEAR & year <= END_YEAR) %>%
  drop_na(iso3c) %>%
  select(iso3c, year, milex_gdp_nato)

# Tilføjer 2025 estimat for Tyskland
# 2025 værdien for Tysklands forsvarsbudget (% BNP) mangler fra NATO's datasæt.
# Da der kun er tale om én enkelt observation og da Tyskland er et "vigtigt"
# land for analysen, er et troværdigt estimat blevet fundet hos IISS (2025).
# Deres skøn er 2,4% for 2025. Kilde:
# iiss.org/online-analysis/military-balance/2025/062/nato-agrees-on-investment-pledge/

nato_clean_mod <- nato_clean %>%
  mutate(
    milex_gdp_nato = if_else(iso3c == "DEU" & year == 2025, 2.4, milex_gdp_nato)
  )

print("2025 estimat for Tysklands forsvarsbudget (% BNP) tilføjet:")
print(filter(nato_clean_mod, iso3c == "DEU" & year == 2025))

# Sammenfletter NATO data med panel skabelon
clean_panel <- merge_and_validate(
  clean_panel,
  nato_clean_mod,
  c("iso3c", "year"),
  "NATO"
) # Alle NAs er for kontrolgruppen (nato data skal kun bruges til OLS hvor
# kontrolgruppen ikke indgår)


## 4.3. CEPII DATA -------------------------------------------------------------
# Geografi variabler
# Afstand er geodætisk afstand (fugleflugt) fra hovedstad til hovedstad

cepii_clean <- read_dta(DIST_CEPII) %>%
  as_tibble() %>%
  # Fixer Rumæniens landekode
  mutate(
    iso_o = if_else(iso_o == "ROM", "ROU", iso_o),
    iso_d = if_else(iso_d == "ROM", "ROU", iso_d)
  ) %>%
  filter(iso_o %in% c("RUS", "BLR", "UKR")) %>%
  pivot_wider(
    id_cols     = iso_d,
    names_from  = iso_o,
    values_from = c(distcap, contig)
  ) %>%
  rename(
    iso3c      = iso_d,
    border_rus = contig_RUS, # Delt grænse med Rusland
    border_blr = contig_BLR, # Delt grænse med Hviderusland
    border_ukr = contig_UKR, # Delt grænse med Ukraine
    dist_rus   = distcap_RUS, # Afstand til Rusland (km)
    dist_blr   = distcap_BLR, # Afstand til Belarus (km)
    dist_ukr   = distcap_UKR, # Afstand til Ukraine (km)
  ) %>%
  filter(iso3c %in% SAMPLE_ISO3C)

# Sammenfletter CEPII data med panel skabelon
clean_panel <- merge_and_validate(
  clean_panel,
  cepii_clean,
  "iso3c",
  "CEPII Distance"
) # Al data for Montenegro (MNE) er NA; det er fint, de er ikke med i analysen


## 4.4. IMF WEO DATA ------------------------------------------------------------
# Økonomiske variabler
# Der er en fejl i GDP (NGDPD) dataen for Albanien; de er i absolutte tal,
# mens observationerne for alle andre lande er i milliarder.
# Jeg transformerer værdierne til milliarder hvis de ikke er angivet som
# milliarder allerede (påvirker kun Albaniens data).

imf_data <- read_csv(ECON_IMF, show_col_types = FALSE, na = c("", "NA", "n/a", "--")) %>%
  clean_names() %>%
  filter(
    # Kommentar for hver indikator er IMF beskrivelsen
    indicator_id %in% c(
      # Gross Domestic Product (GDP), Current prices, US dollar
      "NGDPD",
      # Gross Domestic Product (GDP), Constant prices, Domestic currency
      "NGDP_R",
      # Gross Domestic Product (GDP), Constant prices, Per capita, purchasing
      # power parity (PPP) international dollar, ICP benchmark 2021
      "NGDPRPPPPC",
      # Gross debt, General government, Percent of GDP
      "GGXWDG_NGDP"
    )
  ) %>%
  mutate(
    iso3c = country_id,
    year = as.numeric(time_period),
    obs_value = as.numeric(obs_value),
    obs_value = case_when(
      # Fixer Albaniens GDP observationer
      indicator_id == "NGDPD" & (is.na(scale) | scale == "") ~ obs_value / 1e9,
      indicator_id == "NGDPD" & str_detect(scale, "Billions") ~ obs_value,
      TRUE ~ obs_value
    )
  ) %>%
  filter(iso3c %in% SAMPLE_ISO3C, between(year, START_YEAR, END_YEAR)) %>%
  select(iso3c, year, indicator_id, obs_value) %>%
  pivot_wider(names_from = indicator_id, values_from = obs_value) %>%
  rename(
    gdp       = NGDPD,
    gdp_local = NGDP_R,
    gdp_cap   = NGDPRPPPPC,
    debt_gdp  = GGXWDG_NGDP
  )

# Sammenfletter IMF økonomi data med panel skabelon
clean_panel <- merge_and_validate(
  clean_panel,
  imf_data,
  c("iso3c", "year"),
  "IMF WEO Data"
) # Ingen NAs


## 4.5 DMDC DATA ---------------------------------------------------------------
## Antal amerikanske soldater (tilgået via ustroops pakken)

dmdc_clean <- get_troopdata() %>%
  as_tibble() %>%
  filter(between(year, START_YEAR, END_YEAR)) %>%
  filter(iso3c %in% SAMPLE_ISO3C) %>%
  select(iso3c, year, us_troops = troops_ad)

# Sammenfletter DMDC data med panel skabelon
clean_panel <- merge_and_validate(
  clean_panel,
  dmdc_clean,
  c("iso3c", "year"),
  "DMDC"
) # Alle 2025 obs. er NA


# 5. TRANSFORMÉR OG GEM DATA ===================================================
message("--- Sektion 5: Transformerer og gemmer data ---")

# Opretter master_panel
master_panel <- clean_panel %>%
  arrange(iso3c, year) %>%
  # log-transformere forsvarsudgifter (mio. US)
  mutate(milex_usd_log = log(milex_usd)) %>%
  select(
    group, treat_dummy, post_com, iso3c, country_dan, year, event_time,
    milex_usd, milex_usd_log, milex_gdp, milex_gdp_nato, border_rus,
    dist_rus, dist_blr, dist_ukr, gdp, gdp_local, gdp_cap, debt_gdp,
    us_troops
  )

# Opretter variabel labels
var_labels <- list(
  # Meta
  group          = "gruppe (Behandlet/Kontrol)",
  treat_dummy    = "Behandlingsstatus (dummy)",
  post_com       = "Post-kommunist stat (dummy) - kilde: Britannica",
  iso3c          = "ISO 3166-1 alpha-3 landekoder - kilde: FN",
  country_dan    = "Danske landenavne",
  # Forsvarsudgifter
  milex_usd      = "Forsvarsudgifter (mio. USD, 2023-priser) - kilde: SIPRI",
  milex_usd_log  = "Forsvarsudgifter (log USD) - kilde: SIPRI",
  milex_gdp      = "Forsvarsudgifter (% BNP) - kilde: SIPRI",
  milex_gdp_nato = "Forsvarsudgifter (% BNP), kilde: NATO",
  # Geografi
  border_rus     = "Delt grænse med Rusland (dummy) - kilde: CEPII",
  dist_rus       = "Afstand til Rusland (km) - kilde: CEPII",
  dist_blr       = "Afstand til Hviderusland (km) - kilde: CEPII",
  dist_ukr       = "Afstand til Ukraine (km) - kilde: CEPII",
  # Økonomi
  gdp            = "BNP (mia. USD, løbende priser) - kilde: IMF",
  gdp_local      = "BNP (mia. lokal valuta, 2017-priser) - kilde: IMF",
  gdp_cap        = "BNP pr. indbygger (PPP, 2021-priser) - kilde: IMF",
  debt_gdp       = "Offentlig gæld (% BNP) - kilde: IMF",
  # Andet
  us_troops      = "Antal amerikanske tropper - kilde: DMDC"
)

master_panel <- master_panel %>% set_variable_labels(.labels = var_labels)


## 5.1. GEM MASTER PANEL -------------------------------------------------------

message("\nMaster paneldata struktur (2014-2025):")
glimpse(master_panel)

message("\nManglende værdier i Master paneldata:")
master_panel %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_count") %>%
  filter(na_count > 0) %>%
  print()
# Der er en del NAs i datasættet, men det er forventet da jeg har forsøgt at
# indhente 2025 observationer. Disse observationer er kun relevante for OLS.

saveRDS(master_panel, file = MASTER_PANEL)


## 5.2. GEM EVENT-STUDY PANEL --------------------------------------------------
# Opretter et fokuseret event-study datasæt der kun indeholder behandlings- og
# kontrolgruppen, samt data til og med 2024.

es_panel <- master_panel %>%
  filter(group %in% c("Kontrol", "Behandlet")) %>%
  filter(year <= 2024) %>%
  select(
    group, treat_dummy, iso3c, country_dan, year, event_time,
    milex_usd_log, milex_gdp
  )

message("\nEvent-study paneldata struktur (2014-2024):")
glimpse(es_panel)

message("\nManglende værdier i Event-Study paneldata:")
es_panel %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "na_count"
  ) %>%
  filter(na_count > 0) %>%
  print()
# Ingen NAs i event-study datasættet. Det bekræfter at NAs i master_panel.rds er
# en blanding af manglende 2025 obs. og manglende obs. for lande der ikke indgår
# i analysen.

saveRDS(es_panel, file = ES_PANEL)


# 56. SCRIPT FÆRDIGT ------------------------------------------------------------

message(paste(
  "\n--- Script 01_data_prep.R færdigt ---",
  "\n",
  "\n1. Master panel oprettet med N =", nrow(master_panel), "lande-år observationer.",
  "\n   Gemt til:", MASTER_PANEL,
  "\n",
  "\n2. Event-study panel oprettet med N =", nrow(es_panel), "lande-år observationer.",
  "\n   Gemt til:", ES_PANEL
))
