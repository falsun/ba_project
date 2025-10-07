# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       01_prepare_ideology_data.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-09-29
#   Description:  This script creates a duration-weighted annual government
#                 ideology panel dataset. It loads data from the ParlGov and
#                 Manifesto Project databases, integrates manual data
#                 corrections, calculates cabinet-level ideology scores using
#                 a primary and fallback method, and transforms the event-level
#                 data into a final annual panel.
#
# ---------------------------------------------------------------------------- #


# 1. SETUP: LOAD PACKAGES AND DATA -------------------------------------------

# pacman for installing and loading packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  here,
  tidyverse,
  readxl,
  lubridate,
  manifestoR,
  countrycode
)

# 2. DEFINE ANALYSIS SCOPE & LOAD DATA ---------------------------------------

start_year <- 2014
end_year <- 2025

# Load raw ParlGov data
parlgov_cabinets_raw <- read_csv(here("data", "raw", "view_cabinet.csv"))
parlgov_parties_raw  <- read_csv(here("data", "raw", "view_party.csv"))

# Load all manual data correction files
manual_matches_raw <- read_excel(here("data", "raw", "missing_government_data.xlsx"), sheet = "matched_parties")
manual_mismatches_raw <- read_excel(here("data", "raw", "missing_government_data.xlsx"), sheet = "mismatched_cmp")
manual_temporal_raw <- read_excel(here("data", "raw", "missing_government_data.xlsx"), sheet = "temporal_gaps")
presidential_data_raw  <- read_excel(here("data", "raw", "missing_government_data.xlsx"), sheet = "presidential_elections")


# Load Manifesto Project data
manifestoR::mp_setapikey(key = "b2f2e5d48f009fac1d6cc0b1b97b8dcc")
manifesto_data_raw <- manifestoR::mp_maindataset()

print("All raw ideology data loaded successfully.")


# 3. PREPARE & ENRICH PARLGOV DATA -------------------------------------------
print("Preparing and enriching ParlGov data...")

parlgov_clean <- parlgov_cabinets_raw %>%
  # Apply temporal corrections for specific party-date gaps.
  left_join(
    manual_temporal_raw %>%
      select(party_id = parlgov_party_id, cmp_temporal = manifesto_party_id) %>%
      distinct(party_id, .keep_all = TRUE),
    by = "party_id"
  ) %>%
  # Join with main party data.
  left_join(parlgov_parties_raw, by = "party_id") %>%
  # Correct known mismatches between ParlGov and Manifesto CMP codes.
  left_join(select(manual_mismatches_raw, parlgov_cmp, manifesto_cmp), by = c("cmp" = "parlgov_cmp")) %>%
  mutate(cmp_corrected = if_else(!is.na(manifesto_cmp), manifesto_cmp, cmp)) %>%
  # Add manually researched Manifesto codes for parties missing them in ParlGov.
  left_join(
    manual_matches_raw %>%
      select(party_id = parlgov_party_id, cmp_manual = manifesto_party_id) %>%
      mutate(cmp_manual = as.numeric(cmp_manual)),
    by = "party_id"
  ) %>%
  # Create a final, prioritized CMP code using the hierarchy of corrections.
  mutate(cmp_final = coalesce(cmp_temporal, cmp_corrected, cmp_manual, cmp)) %>%
  mutate(start_date = as.Date(start_date))


# 4. CREATE EVENT-LEVEL IDEOLOGY SCORES --------------------------------------
print("Calculating event-level ideology scores...")

# Clean and prepare the Manifesto Project dataset.
manifesto_clean <- manifesto_data_raw %>%
  select(cmp = party, manifesto_date = edate, rile) %>%
  mutate(manifesto_date = as.Date(manifesto_date)) %>%
  drop_na(rile)

# Calculate and combine event-level ideology scores for all governments.
all_partisan_govs_events <- bind_rows(
  # --- Parliamentary Systems ---
  .x = {
    # Identify partisan cabinet parties from ParlGov.
    parties_to_score <- parlgov_clean %>%
      filter(caretaker == 0, !is.na(cmp_final), cabinet_party == 1, seats > 0)

    # Primary Method: Find latest manifesto dated *before* cabinet start.
    perfect <- parties_to_score %>%
      inner_join(manifesto_clean, by = join_by(cmp_final == cmp, start_date >= manifesto_date)) %>%
      group_by(cabinet_id, party_id) %>%
      slice_max(order_by = manifesto_date, n = 1, with_ties = FALSE)

    # Combine with fallback method for parties with no preceding manifesto.
    bind_rows(
      perfect,
      parties_to_score %>%
        anti_join(perfect, by = c("cabinet_id", "party_id")) %>%
        inner_join(manifesto_clean, by = c("cmp_final" = "cmp")) %>%
        mutate(date_diff = abs(as.numeric(start_date - manifesto_date))) %>%
        group_by(cabinet_id, party_id) %>%
        slice_min(order_by = date_diff, n = 1, with_ties = FALSE)
    ) %>%
      # Calculate the seat-weighted average ideology for each cabinet event.
      group_by(cabinet_id, country_name.x, start_date) %>%
      summarise(weighted_ideology_rile = weighted.mean(rile, w = seats, na.rm = TRUE), .groups = 'drop') %>%
      rename(country_name = country_name.x)
  },

  # --- Presidential Systems ---
  .y = presidential_data_raw %>%
    mutate(start_date = as.Date(start_date), cmp = as.numeric(manifesto_party_id)) %>%
    left_join(manifesto_clean, by = "cmp", relationship = "many-to-many") %>%
    mutate(date_diff = abs(as.numeric(start_date - manifesto_date))) %>%
    group_by(manifesto_party_name, country_name, start_date) %>%
    slice_min(order_by = date_diff, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    rename(weighted_ideology_rile = rile) %>%
    mutate(cabinet_id = 99900 + row_number()) %>%
    select(cabinet_id, country_name, start_date, weighted_ideology_rile)

) %>%
  # Filter out any events where an ideology score could not be calculated.
  filter(!is.na(weighted_ideology_rile) & !is.nan(weighted_ideology_rile))


# 5. TRANSFORM TO ANNUAL DURATION-WEIGHTED PANEL -----------------------------
print("Transforming event-level data to annual panel...")

# Find the last year for which we have actual cabinet data.
last_data_year <- year(max(parlgov_clean$start_date, na.rm = TRUE))

# Create a complete timeline of all governments (partisan and caretaker),
# calculate their end dates, and impute ideology for caretaker cabinets (LOCF).
timeline_with_ideology <- parlgov_clean %>%
  select(country_name = country_name.x, cabinet_id, start_date, caretaker) %>%
  distinct() %>%
  bind_rows(
    select(all_partisan_govs_events, country_name, cabinet_id, start_date) %>%
      mutate(caretaker = 0)
  ) %>%
  distinct() %>%
  arrange(country_name, start_date) %>%
  group_by(country_name) %>%
  mutate(end_date = lead(start_date) - days(1)) %>%
  ungroup() %>%
  mutate(end_date = if_else(is.na(end_date), as.Date(paste0(last_data_year, "-12-31")), end_date)) %>%
  left_join(
    select(all_partisan_govs_events, cabinet_id, weighted_ideology_rile), by = "cabinet_id"
  ) %>%
  group_by(country_name) %>%
  fill(weighted_ideology_rile, .direction = "down") %>%
  ungroup() %>%
  filter(!is.na(weighted_ideology_rile))

# --- Calculate the Final Duration-Weighted Annual Score ---
weighted_rile_dataset <- expand_grid(
  country_name = unique(timeline_with_ideology$country_name),
  year = start_year:end_year
) %>%
  mutate(
    year_start = ymd(paste0(year, "-01-01")),
    year_end   = ymd(paste0(year, "-12-31"))
  ) %>%
  # Join each year with any government(s) in power during that year.
  inner_join(
    timeline_with_ideology,
    by = join_by(country_name, year_end >= start_date, year_start <= end_date)
  ) %>%
  # Calculate the number of days each government was in power within the year.
  mutate(
    effective_start = pmax(start_date, year_start),
    effective_end   = pmin(end_date, year_end),
    duration_in_year = as.numeric(effective_end - effective_start) + 1
  ) %>%
  # Calculate the final duration-weighted average ideology for the year.
  group_by(country_name, year) %>%
  summarise(
    annual_weighted_rile = weighted.mean(weighted_ideology_rile, w = duration_in_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Final cleaning and formatting.
  filter(year <= last_data_year) %>%
  mutate(iso3c = countrycode(country_name, origin = "country.name", destination = "iso3c")) %>%
  select(iso3c, year, annual_weighted_rile) %>%
  drop_na(iso3c) %>%
  arrange(iso3c, year)


# 6. SAVE FINAL DATASET ------------------------------------------------------
saveRDS(weighted_rile_dataset, here("data", "processed", "weighted_rile_dataset.rds"))
print(paste("Script 01 finished. 'Weighted_rile_dataset.rds' - with",
            nrow(weighted_rile_dataset), "observations - created and saved."))
