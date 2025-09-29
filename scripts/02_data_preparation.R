# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       01_data_preparation.R
#   Author:       Frederik Bender Bøeck-Nielsen (with Gemini)
#   Date:         2025-09-14
#   Description:  This script prepares the master dataset for the DiD analysis.
#                 It loads and cleans the country overview, defines treatment and
#                 control groups based on pre-defined criteria, cleans SIPRI
#                 and NATO expenditure data, downloads control variables from
#                 the World Bank (WDI) and IMF, downloads democracy scores from 
#                 V-DEM and merges everything into a single data frame.
#
# ---------------------------------------------------------------------------- #


# 1. SETUP: LOAD PACKAGES ----------------------------------------------------

# pacman for installing and loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  here,       # for robust file paths
  tidyverse,  # Core data manipulation and visualization
  readxl,     # Reading Excel files
  janitor,    # Simple data cleaning functions
  countrycode,# Standardizing country names and codes
  WDI,        # Downloading data from the World Bank
  fs,         # For robust file path management
  vdemdata,   # VDem control variables
  haven,      # To read .dta files
)

# Set a global option to prevent R from using scientific notation for numbers
options(scipen = 999)


# 2. LOAD OVERVIEW & DEFINE ANALYSIS PERIOD & GROUPS -------------------------

# Set the start of the pre-treatment period and end of the post-treatment period
start_year <- 2014
end_year <- 2025

# Load sample country overview
country_sample_raw <- read_excel(here("data", "raw", "country_sample.xlsx"), sheet = "sample")

# Clean the overview and create the 'group' column for the DiD analysis
country_sample_clean <- country_sample_raw %>%
  clean_names() %>%
  # Ensure year columns are numeric, converting '-' to NA
  mutate(
    nato_member_year = as.numeric(nato_member_year),
    oecd_member_year = as.numeric(oecd_member_year),
    # Define treatment and control groups based on the specified criteria
    group = case_when(
      # Treatment: European NATO members that joined before 2014
      region == "Europe" & nato_member_year < 2014 ~ "treatment",
      # Control: Non-European OECD members that joined before 2014
      region != "Europe" & oecd_member_year < 2014 ~ "control",
      # All other countries are not part of this analysis
      TRUE ~ NA_character_
    )
  ) %>%
  # Keep only the countries that have been assigned to a group
  filter(!is.na(group)) %>%
  # remove Iceland since it doesn't have an actual military budget
  filter(iso3c != "ISL") %>%
  # remove turkey
  filter(iso3c != "TUR")

# Create a vector of ISO codes for the final sample to easily filter other datasets
final_sample_iso3c <- unique(country_sample_clean$iso3c)

print("Treatment and control groups defined:")
print(table(country_sample_clean$group))
glimpse(country_sample_clean)


# 3. LOAD AND CLEAN SIPRI MILITARY EXPENDITURE DATA --------------------------

sipri_raw <- read_excel(here("data", "raw", "sipri_milex_2025p.xlsx"), sheet = "Share of GDP", skip = 5)

sipri_clean <- sipri_raw %>%
  clean_names() %>%
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    values_to = "milex_gdp_sipri"
  ) %>%
  mutate(
    year = as.numeric(str_remove(year, "x")),
    iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
    milex_gdp_sipri = as.numeric(milex_gdp_sipri) * 100
  ) %>%
  # Keep only the countries and years relevant to our analysis
  filter(
    iso3c %in% final_sample_iso3c,
    year >= start_year & year <= end_year
  ) %>%
  select(iso3c, year, milex_gdp_sipri) %>%
  drop_na()

print("SIPRI data cleaned and filtered.")
glimpse(sipri_clean)


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
    iso3c %in% final_sample_iso3c,
    year >= start_year & year <= end_year
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


# 5. LOAD AND CLEAN IMF WEO ECONOMIC DATA -----------------------

print("Loading and cleaning all control variables from single IMF source...")

imf_weo_raw <- read_csv(here("data", "raw", "imf_weo_2025-04-22p.csv"))

# Define the exact names of the indicators we need to extract from the IMF file
imf_weo_indicators <- c(
  "Gross domestic product (GDP), Constant prices, Percent change",
  "Gross domestic product (GDP), Per capita, purchasing power parity (PPP) international dollar, ICP benchmarks 2017-2021",
  "Gross debt, General government, Percent of GDP",
  "Unemployment rate",
  "Gross domestic product (GDP), Current prices, US dollar"
)

imf_weo_clean <- imf_weo_raw %>%
  clean_names() %>%
  # Filter for the four specific indicators we need
  filter(indicator %in% imf_weo_indicators) %>%
  # PIVOT FROM LONG TO WIDE to create separate columns for each indicator
  pivot_wider(
    id_cols = c(country_id, time_period),
    names_from = indicator,
    values_from = obs_value
  ) %>%
  # Clean the new, long column names that pivot_wider creates
  clean_names() %>%
  # Rename the columns to our desired short names
  rename(
    iso3c = country_id,
    year = time_period,
    gdp_growth = gross_domestic_product_gdp_constant_prices_percent_change,
    gdp_cap = gross_domestic_product_gdp_per_capita_purchasing_power_parity_ppp_international_dollar_icp_benchmarks_2017_2021,
    debt_gdp = gross_debt_general_government_percent_of_gdp,
    un_rate = unemployment_rate,
    gdp_current = gross_domestic_product_gdp_current_prices_us_dollar
  ) %>%
  # Ensure all relevant columns are numeric
  mutate(across(c(year, gdp_cap, gdp_growth, debt_gdp, un_rate, gdp_current), as.numeric)) %>%
  # Filter for our specific sample of countries and the analysis time period
  filter(
    iso3c %in% final_sample_iso3c,
    year >= start_year & year <= end_year
  ) %>%
  # Keep only the final, clean columns
  select(iso3c, year, gdp_cap, gdp_growth, debt_gdp, un_rate, gdp_current)

print("IMF control variables cleaned and filtered.")
glimpse(imf_weo_clean)


# 6. LOAD AND CLEAN IEP GLOBAL PEACE INDEX DATA ------------------------------

print("Loading and cleaning Global Peace Index data from multiple sheets...")

# Define the path to the Excel file
gpi_filepath <- here("data", "raw", "iep_gpi_2025p.xlsx")

# Get all sheet names from the workbook
all_sheets <- readxl::excel_sheets(gpi_filepath)

# Filter for only the sheets that represent the years in our analysis period
years_to_process <- as.character(start_year:end_year)
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
  filter(iso3c %in% final_sample_iso3c) %>%
  # Remove any rows where the score might be missing
  drop_na(safe_sec)


print("IEP Global Peace Index data cleaned and filtered.")
glimpse(gpi_clean)


# 7. LOAD AND CLEAN CEPII GEODESIC DATA -------------------------------------

print("Loading and cleaning CEPII geodesic distance and border data...")

# Load the Stata .dta file and process it in a single pipeline
cepii_geo_clean <- read_dta(here("data", "raw", "dist_cepii.dta")) %>%
  as_tibble() %>%
  # Fix the outdated ISO code for Romania before any filtering
  mutate(
    iso_o = if_else(iso_o == "ROM", "ROU", iso_o),
    iso_d = if_else(iso_d == "ROM", "ROU", iso_d)
  ) %>%
  # First, filter for only the country pairs originating from Russia or Belarus
  filter(iso_o %in% c("RUS", "BLR")) %>%
  # Reshape the data from a long format to a wide format
  pivot_wider(
    # The new data frame will be identified by the destination country
    id_cols = iso_d,
    # The new column names will come from the origin country (RUS, BLR)
    names_from = iso_o,
    # The values for the new columns will come from the distance and border variables
    values_from = c(distcap, contig)
  ) %>%
  # Rename the newly created columns to our desired final names
  rename(
    iso3c = iso_d,
    dist_rus = distcap_RUS,
    dist_blr = distcap_BLR,
    border_land_rus = contig_RUS,
    border_land_blr = contig_BLR
  ) %>%
  # Finally, keep only the countries that are in our analysis sample
  filter(iso3c %in% final_sample_iso3c)

print("CEPII geodesic data cleaned and filtered (Romania ISO code corrected).")
glimpse(cepii_geo_clean)


# 7.1 CREATE COMBINED BORDER DUMMY VARIABLE -----------------------------------

print("Creating combined land and maritime border dummy variable...")

# Create the new modified dataframe as requested
cepii_geo_clean_mod <- cepii_geo_clean %>%
  # Join with the country sample data to get the maritime border variable
  left_join(
    select(country_sample_clean, iso3c, border_mar_rus),
    by = "iso3c"
  ) %>%
  # It's good practice to replace any potential NA values in border columns with 0
  mutate(
    border_land_rus = replace_na(border_land_rus, 0),
    border_land_blr = replace_na(border_land_blr, 0),
    border_mar_rus = replace_na(border_mar_rus, 0)
  ) %>%
  # Create the new combined border dummy variable
  mutate(
    border_rus_blr = if_else(
      # The condition: if a country has a 1 for ANY of the three border types...
      border_land_rus == 1 | border_land_blr == 1 | border_mar_rus == 1,
      # ...then assign 1, otherwise assign 0.
      1,
      0
    )
  )

print("Combined border dummy variable 'border_rus_blr' created.")
glimpse(cepii_geo_clean_mod)


# 8. LOAD AND CLEAN CEPII BACI TRADE DATA ------------------------------------

print("Loading and cleaning CEPII BACI trade data from yearly files...")

# --- 1. Load Country Codes and Find Russia's Numeric Code ---
cepii_baci_country_codes <- read_csv(here("data", "raw", "cepii_baci_country_codes.csv")) %>%
  clean_names() %>%
  # Keep only the numeric code and the ISO3 code we need for joining
  select(country_code, iso3c = country_iso3)

# Find the numeric code for Russia to use for filtering
russia_code <- cepii_baci_country_codes %>%
  filter(iso3c == "RUS") %>%
  pull(country_code) # pull() extracts the single value

# --- 2. Define a function to process a single yearly BACI file ---
process_baci_year <- function(file_path) {
  # Read the specific year's CSV file
  read_csv(file_path) %>%
    # Filter for flows where either the exporter (i) or importer (j) is Russia
    filter(i == russia_code | j == russia_code) %>%
    # Group by the exporter-importer pair to sum up all product values
    group_by(t, i, j) %>%
    # Calculate the total value for that pair. `v` is in thousands, so multiply by 1000.
    summarise(total_value_usd = sum(v) * 1000, .groups = "drop")
}

# --- 3. Find all yearly BACI files and apply the function to each ---
# List all files in the raw data folder that match the "cepii_baci_YYYY.csv" pattern
yearly_files <- fs::dir_ls(here("data", "raw"), regexp = "cepii_baci_\\d{4}\\.csv$")

# Use purrr::map_dfr to run the function on each file and row-bind the results
cepii_baci_raw_aggregated <- map_dfr(yearly_files, process_baci_year)

# --- 4. Final cleaning and reshaping ---
cepii_baci_clean <- cepii_baci_raw_aggregated %>%
  # Separate into exports and imports from our sample's perspective
  mutate(
    # If the exporter 'i' is Russia, it's an import for country 'j'
    # If the importer 'j' is Russia, it's an export for country 'i'
    trade_flow = if_else(i == russia_code, "imports_rus", "exports_rus"),
    # Identify the partner country code (which is one of our sample countries)
    partner_code = if_else(i == russia_code, j, i)
  ) %>%
  # Join with country codes to get the ISO3C for our sample countries
  left_join(cepii_baci_country_codes, by = c("partner_code" = "country_code")) %>%
  # Keep only the countries in our final sample and the relevant columns
  filter(iso3c %in% final_sample_iso3c) %>%
  select(iso3c, year = t, trade_flow, total_value_usd) %>%
  # Pivot to the wide format with separate columns for imports and exports
  pivot_wider(
    names_from = trade_flow,
    values_from = total_value_usd,
    values_fill = 0 # If a country has no trade in a year, fill with 0
  )

print("CEPII BACI trade data loaded, aggregated, and cleaned successfully.")
glimpse(cepii_baci_clean)


# 8.1. CALCULATE TRADE DEPENDENCY AND ADD TO BACI DATA -------------------------

print("Calculating trade dependency and adding it to the BACI data frame...")

cepii_baci_clean_mod <- cepii_baci_clean %>%
  # Join only the necessary GDP data from the imf_weo_clean object
  left_join(
    select(imf_weo_clean, iso3c, year, gdp_current),
    by = c("iso3c", "year")
  ) %>%
  # Calculate the trade dependency variable
  mutate(
    trade_rus = if_else(
      gdp_current > 0 & !is.na(gdp_current),
      ((exports_rus + imports_rus) / gdp_current) * 100,
      NA_real_ # Assign NA if GDP is zero or missing
    )
  ) %>%
  # Remove the temporary gdp_current column to keep this data frame clean
  select(-gdp_current)

print("Trade dependency calculated and added to the cepii_baci_clean data frame.")
glimpse(cepii_baci_clean_mod)


# 9. LOAD PRE-PROCESSED GOVERNMENT IDEOLOGY DATA -----------------------------

print("Loading pre-processed annual government ideology data...")

weighted_rile_clean <- readRDS(here("data", "processed", "weighted_rile_dataset.rds"))

print("Ideology data loaded.")
glimpse(weighted_rile_clean)


# 10. LOAD AND CLEAN V-DEM DEMOCRACY DATA ------------------------

vdem_clean <- vdemdata::vdem %>%
  as_tibble() %>%
  mutate(iso3c = countrycode(country_name, origin = "country.name", destination = "iso3c")) %>%
  # filter for the countries and years in our sample.
  filter(
    iso3c %in% final_sample_iso3c,
    year >= start_year & year <= end_year
  ) %>%
  # Finally, select and rename the variables
  select(
    iso3c,
    year,
    lib_dem = v2x_libdem
  )

print("V-Dem data for democracy and ideology cleaned and filtered (with correct LOCF).")
glimpse(vdem_clean)


# 11. LOAD POPULATION DATA FROM WDI -------------------------------------------

print("Downloading population data from World Bank WDI...")

# Use the WDI package to fetch total population for the sample countries and years
wdi_pop_clean <- WDI(
  country = final_sample_iso3c,
  indicator = "SP.POP.TOTL", # The indicator code for total population
  start = start_year,
  end = end_year
) %>%
  as_tibble() %>%
  # Rename the indicator column to a more intuitive name
  rename(pop = SP.POP.TOTL) %>%
  # Select only the necessary columns for the merge
  select(iso3c, year, pop)

print("Population data downloaded and cleaned.")
glimpse(wdi_pop_clean)


# 12. COMBINE AND SAVE FINAL DATASET ------------------------------------------

master_dataset_v1 <- country_sample_clean %>%
  tidyr::expand_grid(year = start_year:end_year) %>%
  left_join(sipri_clean, by = c("iso3c", "year")) %>%
  left_join(nato_clean_mod, by = c("iso3c", "year")) %>%
  left_join(imf_weo_clean, by = c("iso3c", "year")) %>%
  left_join(gpi_clean, by = c("iso3c", "year")) %>%
  left_join(cepii_geo_clean_mod, by = "iso3c") %>%
  left_join(cepii_baci_clean_mod, by = c("iso3c", "year")) %>%
  left_join(weighted_rile_clean, by = c("iso3c", "year")) %>%
  left_join(vdem_clean, by = c("iso3c", "year")) %>%
  left_join(wdi_pop_clean, by = c("iso3c", "year")) %>%
  # add post-treatment dummy variable that assigns 1 for post-treatment years,
  # and 0 for pre-treatment years. Post-treatment is lagged by 1 year, to account
  # for the time it takes to adjust defence budgets.
  mutate(
    post_treat = if_else(year >= 2023, 1, 0)
  ) %>%
  # select all the relevant columns for the master_dataset_v1
select(
  iso3c, group, year, post_treat, milex_gdp_sipri, milex_gdp_nato, gdp_cap,
  gdp_growth, debt_gdp, un_rate, trade_rus, safe_sec, lib_dem,
  annual_weighted_rile, pop, post_com, border_rus_blr, dist_rus, dist_blr
)

print("All datasets merged into final master data frame.")
glimpse(master_dataset_v1)

# Save for use in the next analysis script
saveRDS(master_dataset_v1, here("data", "processed", "master_dataset_v1.rds"))

print("Script 02 finished. Master dataset saved.")
