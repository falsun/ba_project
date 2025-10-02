# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       02_preliminary_analysis.R
#   Author:       Frederik Bender Bøeck-Nielsen (with Gemini)
#   Date:         2025-09-14
#   Description:  This script conducts the preliminary analysis for the DiD model.
#                 It loads the clean master dataset, creates a descriptive
#                 statistics table to check for pre-treatment balance, and
#                 generates a parallel trends plot to visually inspect the
#                 key DiD assumption.
#
# ---------------------------------------------------------------------------- #


# 1. SETUP: LOAD PACKAGES AND DATA -------------------------------------------

# pacman simplifies package management
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # Core data manipulation and visualization
  here,         # For robust, project-relative file paths
  gtsummary,    # For creating beautiful summary tables
  fixest,        # For event study model
  modelsummary
)

# Set a global option to prevent R from using scientific notation for numbers
options(scipen = 999)

# Load the master dataset created in the previous script
master_dataset <- readRDS(here("data", "processed", "master_dataset.rds"))

print("Master dataset loaded.")
glimpse(master_dataset)


# 2. DESCRIPTIVE STATISTICS (BALANCE TABLE) ----------------------------------
# The purpose of this table is to compare the treatment and control groups
# on key variables *before* the treatment occurred.

print("Generating pre-treatment descriptive statistics table...")

# Filter the data for the pre-treatment period (2014-2022)
pre_treatment_data <- master_dataset %>%
  filter(year < 2022)

# Create the summary table using the gtsummary package
balance_table <- pre_treatment_data %>%
  # Select the variables we want to summarize
  select(
    group,
    sipri_milex_share_gdp,
    gdp_per_capita,
    gdp_growth,
    debt_gdp,
    unemployment_rate,
    lib_dem
  ) %>%
  # Create the summary table, stratified by the 'group' column
  tbl_summary(
    by = group,
    label = list( # Custom labels for clarity
      sipri_milex_share_gdp ~ "Military Spending (% GDP, SIPRI)",
      gdp_per_capita ~ "GDP per Capita (PPP $)",
      gdp_growth ~ "GDP Growth (%)",
      debt_gdp ~ "Government Debt (% GDP)",
      unemployment_rate ~ "Unemployment Rate (%)",
      lib_dem ~ "Liberal Democracy Index (V-Dem)"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})", # Show mean and SD
    digits = all_continuous() ~ 2 # Round to 2 decimal places
  ) %>%
  # Add a header to clarify the columns
  modify_header(label ~ "**Variable**") %>%
  # Add a title to the table
  as_gt() %>%
  gt::tab_header(
    title = "Table 1: Pre-Treatment Descriptive Statistics (2014-2022)",
    subtitle = "Values are means with standard deviations in parentheses."
  )

# Display the table in the RStudio Viewer
balance_table


# 3. PARALLEL TRENDS PLOT ----------------------------------------------------
# This plot visually inspects the key assumption of the DiD model: that the
# treatment and control groups were on parallel trends *before* the treatment.

print("Generating parallel trends plot...")

# First, calculate the annual mean of the dependent variable for each group
trends_data <- master_dataset %>%
  group_by(year, group) %>%
  summarise(
    mean_milex = mean(sipri_milex_share_gdp, na.rm = TRUE),
    .groups = "drop" # Ungroup after summarising
  )

# Create the plot using ggplot2
parallel_trends_plot <- ggplot(trends_data, aes(x = year, y = mean_milex, color = group, group = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  # Add a vertical dashed line to mark the year of the invasion
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray40", linewidth = 1) +
  # Apply professional labels and a clean theme
  labs(
    title = "Parallel Trends of Military Spending Before 2022",
    subtitle = "Average Military Expenditure as a Share of GDP (SIPRI Data)",
    x = "Year",
    y = "Military Spending (% of GDP)",
    color = "Group"
  ) +
  scale_x_continuous(breaks = seq(min(trends_data$year), max(trends_data$year), by = 2)) + # Make x-axis cleaner
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# Display the plot
parallel_trends_plot


# 4. FORMAL PARALLEL TRENDS TEST (EVENT STUDY MODEL) ------------------------

print("Running event study model and generating plot...")

# --- FIX IS HERE: Create an explicit 0/1 treatment dummy ---
# This makes the model specification clearer and more robust for iplot().
master_dataset <- master_dataset %>%
  mutate(is_treatment = ifelse(group == "treatment", 1, 0))

ref_year <- 2021

# --- AND HERE: Rewritten model formula for clarity ---
# We now interact the 'year' factor with the new 'is_treatment' dummy.
# We also remove 'year' from the fixed effects, as it's implicitly handled by i().
event_study_model <- feols(
  sipri_milex_share_gdp ~ i(year, is_treatment, ref = ref_year) +
    gdp_per_capita + gdp_growth + debt_gdp + unemployment_rate + lib_dem |
    iso3c, # Year fixed effects are now implicit in the i() term
  data = master_dataset
)

print(summary(event_study_model))

event_study_table <- modelsummary(
  event_study_model,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_map = c("nobs", "r2.within"),
  title = "Event Study Estimates of the Effect on Military Spending (% of GDP)",
  output = "gt"
)

# Print the table
event_study_table


# The iplot() function will now work correctly with this unambiguous model.
event_study_plot <- iplot(
  event_study_model,
  main = "Event Study: Effect on Military Spending (rel. to 2021)",
  xlab = "Year",
  ylab = "Estimated Difference from Control Group (% of GDP)"
) +
  # We can add standard ggplot2 layers to customize the iplot() output
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = ref_year + 0.5, linetype = "dashed", color = "gray40") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

# Display the event study plot
event_study_plot

print("Script 02 finished. Descriptive table, parallel trends plot, event study model and plot have been generated")
