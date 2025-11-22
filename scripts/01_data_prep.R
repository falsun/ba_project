# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       01_data_processing.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-11-17 (Final)
#   Description:  Loads, cleans, merges, and transforms all data sources
#                 to create the final "master_panel.rds" for analysis.
#                 Starts directly at 2014 (no lag year).
#
# ---------------------------------------------------------------------------- #


# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

DIR_DATA_RAW <- here::here("data", "raw")
DIR_DATA_PROC <- here::here("data", "_processed")

if (!dir.exists(DIR_DATA_PROC)) dir.create(DIR_DATA_PROC, recursive = TRUE)

COUNTRY_SAMPLE <- file.path(DIR_DATA_RAW, "country_sample.xlsx")
SIPRI_MILEX <- file.path(DIR_DATA_RAW, "sipri_milex_2025p.xlsx")
MASTER_PANEL <- file.path(DIR_DATA_PROC, "master_panel.rds")

# Timing
START_YEAR <- 2014 # Analysis start year (Annexation of Crimea)
END_YEAR <- 2024
TREAT_YEAR <- 2022

# Group Definitions
TREATMENT_MAN <- c()
TREATMENT_SEC_MAN <- c("GRC", "BGR")
CONTROL_MAN <- c()
CONTROL_SEC_MAN <- c("CHL", "ISR", "MEX", "TUR", "USA")

# WDI Indicators
WDI_IND <- c(
  pop       = "SP.POP.TOTL", # Population, total
  gdp_cap   = "NY.GDP.PCAP.PP.KD", # GDP per capita, PPP (constant 2021 intl $)
  trade_gdp = "NE.TRD.GNFS.ZS" # Trade as % of GDP
)


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  here, # Robust file paths
  tidyverse, # Data manipulation
  readxl, # Read Excel files
  janitor, # Cleaning data frames
  countrycode, # Convert country codes
  WDI, # World Bank data
  vdemdata, # V-Dem data
  conflicted, # Conflicts
  labelled # Labels
)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


# 2. HELPER FUNCTION =========================================================
merge_and_validate <- function(master_df, new_df, merge_by, source_name) {
  message(paste0("--> Merging ", source_name, " data..."))

  new_df <- new_df %>% select(all_of(merge_by), everything())
  new_cols <- setdiff(names(new_df), merge_by)

  merged_df <- left_join(master_df, new_df, by = merge_by)

  missing_data <- merged_df %>%
    filter(if_any(all_of(new_cols), is.na)) %>%
    select(iso3c, year, all_of(new_cols))

  if (nrow(missing_data) > 0) {
    warning(paste("WARNING: Missing values for ", source_name, "data:"))
    print(missing_data, n = 50)
  } else {
    message(paste("SUCCESS: No missing values for ", source_name, "data."))
  }

  return(merged_df)
}


# 3. DEFINE COUNTRY SAMPLE & GROUPS ==========================================
message("--- Section 3: Defining Country Sample ---")

country_sample <- read_excel(COUNTRY_SAMPLE, sheet = "sample") %>%
  clean_names() %>%
  mutate(
    across(ends_with("_year"), as.numeric),
    group = case_when(
      region == "Europe" & nato_member_year < START_YEAR ~ "treatment",
      region == "Europe" & nato_member_year >= START_YEAR ~ "treatment_sec",
      oecd_member_year <= START_YEAR ~ "control",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    group = case_when(
      iso3c %in% TREATMENT_MAN ~ "treatment",
      iso3c %in% TREATMENT_SEC_MAN ~ "treatment_sec",
      iso3c %in% CONTROL_MAN ~ "control",
      iso3c %in% CONTROL_SEC_MAN ~ "control_sec",
      TRUE ~ group
    )
  ) %>%
  filter(!is.na(group))

message("Final group composition:")
country_sample %>%
  count(group, name = "n_countries") %>%
  print()

SAMPLE_ISO3C <- unique(country_sample$iso3c)


# 4. PREPARE DATA SOURCES ====================================================
message("--- Section 4: Preparing and Merging Data Sources ---")

## 4.1. Master Panel Framework
clean_panel <- country_sample %>%
  select(group, iso3c, subregion) %>% # Added 'region'
  crossing(year = START_YEAR:END_YEAR) %>%
  mutate(
    treat_dummy = ifelse(group == "treatment", 1, 0),
    post_treat = as.integer(year >= TREAT_YEAR)
  ) %>%
  arrange(group, iso3c, year)

## 4.2. SIPRI (Outcome Variables)

# Constant US$
sipri_usd <- read_excel(SIPRI_MILEX, sheet = "Constant (2023) US$", skip = 5) %>%
  clean_names() %>%
  pivot_longer(cols = starts_with("x"), names_to = "year", values_to = "milex_usd", names_prefix = "x", names_transform = as.integer) %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c"), milex_usd = as.numeric(milex_usd) * 1e6) %>%
  filter(iso3c %in% SAMPLE_ISO3C, between(year, START_YEAR, END_YEAR)) %>%
  select(iso3c, year, milex_usd)

clean_panel <- merge_and_validate(clean_panel, sipri_usd, c("iso3c", "year"), "SIPRI (USD)")

# Share of GDP
sipri_gdp <- read_excel(SIPRI_MILEX, sheet = "Share of GDP", skip = 5) %>%
  clean_names() %>%
  pivot_longer(cols = starts_with("x"), names_to = "year", values_to = "milex_gdp", names_prefix = "x", names_transform = as.integer) %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c"), milex_gdp = as.numeric(milex_gdp) * 100) %>%
  filter(iso3c %in% SAMPLE_ISO3C, between(year, START_YEAR, END_YEAR)) %>%
  select(iso3c, year, milex_gdp)

clean_panel <- merge_and_validate(clean_panel, sipri_gdp, c("iso3c", "year"), "SIPRI (GDP)")

## 4.3. WDI (Covariates)
wdi_clean <- WDI(country = SAMPLE_ISO3C, indicator = WDI_IND, start = START_YEAR, end = END_YEAR, extra = FALSE) %>%
  as_tibble() %>%
  select(iso3c, year, all_of(names(WDI_IND)))

clean_panel <- merge_and_validate(clean_panel, wdi_clean, c("iso3c", "year"), "WDI")

## 4.4. V-Dem (Covariate)
vdem_clean <- vdemdata::vdem %>%
  as_tibble() %>%
  filter(between(year, START_YEAR, END_YEAR)) %>%
  mutate(iso3c = countrycode(country_name, "country.name", "iso3c")) %>%
  filter(iso3c %in% SAMPLE_ISO3C) %>%
  select(iso3c, year, lib_dem = v2x_libdem)

clean_panel <- merge_and_validate(clean_panel, vdem_clean, c("iso3c", "year"), "V-Dem")


# 5. DATA TRANSFORMATION =====================================================
message("--- Section 5: Creating Variables and Transformations ---")

master_panel <- clean_panel %>%
  arrange(iso3c, year) %>%
  mutate(
    # Base Variables
    milex_cap = milex_usd / pop,

    # Log Transformations
    milex_usd_log = log(milex_usd),
    milex_gdp_log = log(milex_gdp),
    milex_cap_log = log(milex_cap),

    # Log Covariates (Future-proofing)
    pop_log = log(pop),
    gdp_cap_log = log(gdp_cap)
  ) %>%
  # --- REORDER COLUMNS ---
  select(
    group, treat_dummy, subregion, iso3c, year, post_treat, milex_usd_log,
    milex_cap, milex_cap_log, milex_gdp, milex_gdp_log, pop, pop_log, gdp_cap,
    gdp_cap_log, trade_gdp, lib_dem
  )


# 6. POLISH, INSPECT & SAVE ==================================================
message("--- Section 6: Saving Master Panel ---")

# Define Labels
# Format: Subject (Unit)
var_labels <- list(
  group         = "Treatment assignment",
  treat_dummy   = "Group dummy",
  subregion     = "Geographic region (UN definion)",
  iso3c         = "Country code",
  post_treat    = "Pre/Post dummy",

  # Main Outcomes
  milex_usd_log = "Log Mil. Exp. (Const. 2023 US$)",
  milex_gdp     = "Mil. Exp. (% of GDP)",
  milex_gdp_log = "Log Mil. Exp. (% of GDP)",
  milex_cap     = "Mil. Exp. Per Capita (Const. 2023 US$)",
  milex_cap_log = "Log Mil. Exp. Per Capita",

  # Controls
  pop           = "Population",
  pop_log       = "Log Population",
  gdp_cap       = "GDP Per Capita (PPP 2017 $)",
  gdp_cap_log   = "Log GDP Per Capita",
  trade_gdp     = "Trade (% of GDP)",
  lib_dem       = "Liberal Democracy Index (0-1)"
)

master_panel <- master_panel %>% set_variable_labels(.labels = var_labels)

message("\nFinal dataset structure (2014-2024):")
glimpse(master_panel)

message("\nMissing Values Check:")
master_panel %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_count") %>%
  filter(na_count > 0) %>%
  print()

saveRDS(master_panel, file = MASTER_PANEL)

message(paste(
  "\n--- Script 01_data_processing.R finished ---",
  "\nMaster panel saved to:", MASTER_PANEL
))
