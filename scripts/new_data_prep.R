# ---------------------------------------------------------------------------- #
#
#   Project:     NATO Defence Spending Bachelor's Thesis
#   Script:      01_data_prep.R
#   Author:      Frederik Bender Bøeck-Nielsen
#   Date:        2025-10-12
#   Description: This script loads, cleans, and merges multiple data sources
#                to create a final, analysis-ready panel dataset.
#
# ---------------------------------------------------------------------------- #


# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

DIR_DATA_RAW <- here::here("data", "raw")
DIR_DATA_PROC <- here::here("data", "_processed")

if (!dir.exists(DIR_DATA_PROC)) dir.create(DIR_DATA_PROC, recursive = TRUE)

COUNTRY_SAMPLE <- file.path(DIR_DATA_RAW, "country_sample.xlsx")
SIPRI_MILEX <- file.path(DIR_DATA_RAW, "sipri_milex_2025p.xlsx")
CLEAN_PANEL <- file.path(DIR_DATA_PROC, "clean_panel.rds")

START_YEAR <- 2014 # Russian annexation of Crimea and NATO Wales Summit
END_YEAR <- 2024 # Latest available data for most key variables
TREAT_YEAR <- 2022 # Full-scale Russian invasion of Ukraine

TREATMENT_SEC_MAN <- c()
CONTROL_SEC_MAN <- c("CHL", "ISR", "MEX", "TUR", "USA")

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
  janitor, # Cleaning data frames and column names
  countrycode, # Convert country names/codes
  WDI, # World Bank data
  vdemdata, # V-Dem data
  conflicted, # Function name conflicts
  labelled # Label attributes
)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

options(scipen = 999)


# 2. HELPER FUNCTION =========================================================
# A reusable function to merge new data and validate the join. This promotes
# DRY (Don't Repeat Yourself) principles and makes the main script body cleaner.

#' Merge New Data and Check for Missing Values
#'
#' @param master_df The main panel data frame.
#' @param new_df The new data frame to be merged.
#' @param merge_by A character vector of column names to join by.
#' @param source_name A string naming the data source for logging messages.
#' @return The merged data frame with validation messages printed to the console.
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
      iso3c %in% TREATMENT_SEC_MAN ~ "treatment_sec",
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

# 4.1. Master Panel Framework ------------------------------------------------
clean_panel <- country_sample %>%
  select(group, iso3c) %>%
  crossing(year = START_YEAR:END_YEAR) %>%
  mutate(post_treat = as.integer(year >= TREAT_YEAR)) %>%
  arrange(group, iso3c, year)

# 4.2. SIPRI (Outcome Variable) ----------------------------------------------

# Military spending as a share of GDP
sipri_gdp_clean <- read_excel(SIPRI_MILEX, sheet = "Share of GDP", skip = 5) %>%
  clean_names() %>%
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    values_to = "milex_gdp",
    names_prefix = "x",
    names_transform = as.integer
  ) %>%
  mutate(
    iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
    milex_gdp = as.numeric(milex_gdp) * 100
  ) %>%
  filter(iso3c %in% SAMPLE_ISO3C, between(year, START_YEAR, END_YEAR)) %>%
  select(iso3c, year, milex_gdp)

clean_panel <- merge_and_validate(clean_panel, sipri_gdp_clean, c("iso3c", "year"), "SIPRI")

# Military spending in constant US$
sipri_usd_clean <- read_excel(SIPRI_MILEX, sheet = "Constant (2023) US$", skip = 5) %>%
  clean_names() %>%
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    values_to = "milex_usd",
    names_prefix = "x",
    names_transform = as.integer
  ) %>%
  mutate(
    iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
    milex_usd = as.numeric(milex_usd)
  ) %>%
  filter(iso3c %in% SAMPLE_ISO3C, between(year, START_YEAR, END_YEAR)) %>%
  select(iso3c, year, milex_usd)

clean_panel <- merge_and_validate(clean_panel, sipri_usd_clean, c("iso3c", "year"), "SIPRI")

# 4.3. WDI (Covariates) ------------------------------------------------------
wdi_clean <- WDI(
  country = SAMPLE_ISO3C,
  indicator = WDI_IND,
  start = START_YEAR,
  end = END_YEAR,
  extra = FALSE
) %>%
  as_tibble() %>%
  select(iso3c, year, all_of(names(WDI_IND)))

clean_panel <- merge_and_validate(clean_panel, wdi_clean, c("iso3c", "year"), "WDI")

# 4.4. V-Dem (Covariate) -----------------------------------------------------
vdem_clean <- vdemdata::vdem %>%
  as_tibble() %>%
  filter(between(year, START_YEAR, END_YEAR)) %>%
  mutate(iso3c = countrycode(country_name, origin = "country.name", destination = "iso3c")) %>%
  filter(iso3c %in% SAMPLE_ISO3C) %>%
  select(iso3c, year, lib_dem = v2x_libdem)

clean_panel <- merge_and_validate(clean_panel, vdem_clean, c("iso3c", "year"), "V-Dem")


# 5. POLISH, INSPECT & SAVE DATA =============================================
message("--- Section 5: Applying variable labels, inspecting & saving data ---")

new_labels <- list(
  iso3c      = "Country Code",
  post_treat = "Dummy",
  milex_gdp  = "% of GDP",
  milex_usd  = "Constant 2023 US$",
  pop        = "Population",
  gdp_cap    = "PPP, constant 2017 intl. $",
  trade_gdp  = "% of GDP",
  lib_dem    = "Index (0-1)"
)

clean_panel <- clean_panel %>%
  set_variable_labels(.labels = new_labels)

message("\nFinal dataset structure:")
glimpse(clean_panel)

message("\nSummary of missingness across all variables:")
clean_panel %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_count") %>%
  filter(na_count > 0) %>%
  print()

saveRDS(clean_panel, file = CLEAN_PANEL)

message(paste(
  "\n--- Script 01_data_prep.R finished ---",
  "\nMaster panel data frame saved to:", CLEAN_PANEL
))
