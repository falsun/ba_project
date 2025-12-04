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
CEPII_DIST <- file.path(DIR_DATA_RAW, "dist_cepii.dta")
IMF_DEBT <- file.path(DIR_DATA_RAW, "imf_weo_2025-10-14p.csv")
MASTER_PANEL <- file.path(DIR_DATA_PROC, "master_panel.rds")

# Timing
START_YEAR <- 2014 # Analysis start year (Annexation of Crimea)
END_YEAR <- 2025
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
  labelled, # Labels
  haven # dta files
)

library(troopdata)

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

#message("Final group composition:")
#country_sample %>% count(group) %>% print()

SAMPLE_ISO3C <- unique(country_sample$iso3c)


# 4. PREPARE DATA SOURCES ====================================================
message("--- Section 4: Preparing and Merging Data Sources ---")

## 4.1. Master Panel Framework ----
clean_panel <- country_sample %>%
  select(group, iso3c, subregion, border_mar_rus, post_com) %>%
  crossing(year = START_YEAR:END_YEAR) %>%
  mutate(
    treat_dummy = ifelse(group == "treatment", 1, 0),
    post_treat = as.integer(year >= TREAT_YEAR)
  ) %>%
  arrange(group, iso3c, year)

## 4.2. SIPRI (Outcome Variables) ----
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

## 4.3. V-Dem (Liberal Democracy Index) ----
vdem_clean <- vdemdata::vdem %>%
  as_tibble() %>%
  filter(between(year, START_YEAR, END_YEAR)) %>%
  mutate(iso3c = countrycode(country_name, "country.name", "iso3c")) %>%
  filter(iso3c %in% SAMPLE_ISO3C) %>%
  select(iso3c, year, lib_dem = v2x_libdem)

clean_panel <- merge_and_validate(clean_panel, vdem_clean, c("iso3c", "year"), "V-Dem")

## 4.4. CEPII (Distance & Land Borders) ----
cepii_raw <- read_dta(CEPII_DIST)

dist_clean <- cepii_raw %>%
  as_tibble() %>%
  # Fix Romania ISO
  mutate(
    iso_o = if_else(iso_o == "ROM", "ROU", iso_o),
    iso_d = if_else(iso_d == "ROM", "ROU", iso_d)
  ) %>%
  # Filter for Conflict Zone Origins
  filter(iso_o %in% c("RUS", "BLR", "UKR")) %>%
  # Reshape Long to Wide
  pivot_wider(
    id_cols = iso_d,
    names_from = iso_o,
    values_from = c(distcap, contig)
  ) %>%
  # Rename for clarity
  rename(
    iso3c = iso_d,
    dist_rus = distcap_RUS,
    dist_blr = distcap_BLR,
    dist_ukr = distcap_UKR,
    border_land_rus = contig_RUS,
    border_land_blr = contig_BLR,
    border_land_ukr = contig_UKR
  ) %>%
  # Filter for our sample
  filter(iso3c %in% SAMPLE_ISO3C)

clean_panel <- merge_and_validate(clean_panel, dist_clean, "iso3c", "CEPII Distance")

## 4.6. IMF WEO (Government Debt) ----
imf_debt <- read_csv(IMF_DEBT, show_col_types = FALSE, na = c("", "NA", "n/a", "--")) %>%
  clean_names() %>%
  filter(indicator_id == "GGXWDG_NGDP") %>%
  mutate(
    iso3c = country_id,
    year = as.numeric(time_period),
    debt_gdp = as.numeric(obs_value)
  ) %>%
  filter(iso3c %in% SAMPLE_ISO3C, between(year, START_YEAR, END_YEAR)) %>%
  select(iso3c, year, debt_gdp)

clean_panel <- merge_and_validate(clean_panel, imf_debt, c("iso3c", "year"), "IMF Debt")

## 4.6. WDI (Covariates) ----
wdi_clean <- WDI(country = SAMPLE_ISO3C, indicator = WDI_IND, start = START_YEAR, end = END_YEAR, extra = FALSE) %>%
  as_tibble() %>%
  select(iso3c, year, all_of(names(WDI_IND)))

clean_panel <- merge_and_validate(clean_panel, wdi_clean, c("iso3c", "year"), "WDI")


# 6. LOAD AND CLEAN IEP GLOBAL PEACE INDEX DATA ------------------------------

print("Loading and cleaning Global Peace Index data from multiple sheets...")

# Define the path to the Excel file
gpi_filepath <- here("data", "raw", "iep_gpi_2025p.xlsx")

# Get all sheet names from the workbook
all_sheets <- readxl::excel_sheets(gpi_filepath)

# Filter for only the sheets that represent the years in our analysis period
years_to_process <- as.character(START_YEAR:END_YEAR)
relevant_sheets <- all_sheets[all_sheets %in% years_to_process]

# Define a function to read and process a single year's sheet
process_gpi_sheet <- function(sheet_name) {
  read_excel(
    gpi_filepath,
    sheet = sheet_name,
    skip = 5 # Skip the first 5 rows of messy headers as identified
  ) %>%
    as_tibble() %>%
    clean_names() %>%
    # Select the country identifier and the variable of interest
    select(
      iso3c = geocode,
      safe_sec = safety_and_security
    ) %>%
    # Add a year column based on the sheet name and ensure data types are correct
    mutate(
      year = as.numeric(sheet_name),
      safe_sec = as.numeric(safe_sec)
    )
}

# Use purrr::map_dfr to apply the function to each relevant sheet and
# row-bind the results into a single, tidy data frame.
gpi_clean <- map_dfr(relevant_sheets, process_gpi_sheet) %>%
  # Filter for only the countries in our final sample
  filter(iso3c %in% SAMPLE_ISO3C) %>%
  # Remove any rows where the score might be missing
  drop_na(safe_sec)


print("IEP Global Peace Index data cleaned and filtered.")
glimpse(gpi_clean)

clean_panel <- merge_and_validate(clean_panel, gpi_clean, c("iso3c", "year"), "GPI")


# 4. LOAD AND CLEAN NATO DEFENCE EXPENDITURE DATA ----------------------------

# The NATO Excel sheet is messy. We use the `range` argument to be precise
# and specify `col_names = TRUE` to use the year headers.
nato_raw <- read_excel(here("data", "raw", "nato_milex_2025-08-28p.xlsx"),
                       sheet = "Table 3",
                       range = "A4:M35", # Read only the relevant data block
                       col_names = TRUE)

nato_clean <- nato_raw %>%
  # The first column name is blank, so read_excel calls it ...1. We rename it.
  rename(country = ...1) %>%
  # Remove any footnote rows that might be included in the range
  filter(!is.na(country), !str_detect(country, "Footnote")) %>%
  # Clean country names by removing asterisks before standardization
  mutate(country = str_remove_all(country, "\\*")) %>%
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "milex_gdp_nato"
  ) %>%
  mutate(
    # Year columns like '2024e' are handled by removing non-numeric characters
    year = as.numeric(str_remove_all(year, "[^0-9]")),
    # Now countrycode will work on the cleaned names
    iso3c = countrycode(country, origin = "country.name", destination = "iso3c",
                        custom_match = c("Türkiye" = "TUR")),
    milex_gdp_nato = as.numeric(milex_gdp_nato)
  ) %>%
  # Keep only the countries and years relevant to our analysis
  filter(
    iso3c %in% SAMPLE_ISO3C,
    year >= START_YEAR & year <= END_YEAR
  ) %>%
  # Instead of a blanket drop_na(), we only drop rows where the country
  # code itself is NA. This keeps rows like Germany 2025 where the value is
  # NA but the country and year are valid.
  drop_na(iso3c) %>%
  select(iso3c, year, milex_gdp_nato)

print("NATO data cleaned and filtered (NA rows preserved).")
glimpse(nato_clean)


# 4.1 MANUALLY ADD MISSING 2025 ESTIMATE FOR GERMANY ----------------------

print("Manually adding sourced 2025 estimate for Germany...")

# The 2025 estimate for Germany is missing from the raw NATO source file.
# Based on external research (IISS, 2025), a credible estimate using
# the NATO definition of defence spending is 2.4% of GDP.
# We add this single data point here to complete the dataset for the
# robustness test. This makes the change transparent and reproducible.
# source: https://www.iiss.org/online-analysis/military-balance/2025/062/nato-agrees-on-investment-pledge/

nato_clean_mod <- nato_clean %>%
  mutate(
    milex_gdp_nato = if_else(
      # The condition to find the specific cell:
      iso3c == "DEU" & year == 2025,
      # The value to insert:
      2.4,
      # For all other rows, keep the original value:
      milex_gdp_nato
    )
  )

print("Value added. Verifying Germany's 2025 data:")
# This line will print the single row for Germany in 2025 to confirm the fix
print(filter(nato_clean_mod, iso3c == "DEU" & year == 2025))

clean_panel <- merge_and_validate(clean_panel, nato_clean_mod, c("iso3c", "year"), "NATO")

# 7 US TROOPS
dmdc_clean <- get_troopdata() %>%
  as_tibble() %>%
  filter(between(year, START_YEAR, END_YEAR)) %>%
  filter(iso3c %in% SAMPLE_ISO3C) %>%
  select(iso3c, year, us_troops = troops_ad)

clean_panel <- merge_and_validate(clean_panel, dmdc_clean, c("iso3c", "year"), "DMDC")

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

    # Minimum Distance to Conflict Zone
    dist_conf = pmin(dist_rus, dist_blr, dist_ukr, na.rm = TRUE),
    dist_enemy = pmin(dist_rus, dist_blr, na.rm = TRUE),

    # Combined Border Dummy
    # Uses CEPII land borders + Manual maritime border (preserved from country_sample)
    # Replace NAs with 0 to ensure boolean logic works
    border_conf = if_else(
      replace_na(border_land_rus, 0) == 1 |
        replace_na(border_land_blr, 0) == 1 |
        replace_na(border_land_ukr, 0) == 1 |
        replace_na(border_mar_rus, 0) == 1,
      1, 0
    ),
    border_conf_land = if_else(
      replace_na(border_land_rus, 0) == 1 |
        replace_na(border_land_blr, 0) == 1 |
        replace_na(border_land_ukr, 0) == 1,
      1, 0
    ),
    border_enemy = if_else(
      replace_na(border_land_rus, 0) == 1 |
        replace_na(border_land_blr, 0) == 1 |
        replace_na(border_mar_rus, 0) == 1,
      1, 0
    ),
    border_enemy_land = if_else(
      replace_na(border_land_rus, 0) == 1 |
        replace_na(border_land_blr, 0) == 1,
      1, 0
    ),
    border_rus = if_else(
      replace_na(border_land_rus, 0) == 1 |
        replace_na(border_mar_rus, 0) == 1,
      1, 0
    ),
  ) %>%
  # --- REORDER COLUMNS ---
  select(

    # META
    group, treat_dummy, subregion, post_com, iso3c, year, post_treat,

    # OUTCOMES
    milex_usd_log, milex_gdp, milex_gdp_nato,

    # GEOGRAPHY
    border_rus, border_land_rus, border_enemy, border_enemy_land, border_conf,
    border_conf_land, dist_rus, dist_conf, dist_enemy,

    # OTHER
    gdp_cap, debt_gdp, us_troops
  )


# 6. POLISH, INSPECT & SAVE ==================================================
message("--- Section 6: Saving Master Panel ---")

# Define Labels
# Format: Subject (Unit)
var_labels <- list(
  group          = "Treatment assignment",
  treat_dummy    = "Group (dummy)",
  subregion      = "Geographic region (UN definion)",
  post_com       = "Post-Communist (dummy)",
  iso3c          = "Country code",
  post_treat     = "Pre/Post (dummy)",

  # Main Outcomes
  milex_usd_log  = "Log Mil. Exp. (const. 2023 US$)",
  milex_gdp      = "Mil. Exp. (% of GDP)",
  milex_gdp_nato = "Mil. Exp. (% of GDP)",

  # Controls
  gdp_cap       = "GDP per Capita (PPP 2017 $)",
  debt_gdp      = "Debt (% of GDP)",

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
