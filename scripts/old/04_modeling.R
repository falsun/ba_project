# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       04_modeling.R (Complete)
#   Author:       [Your Name]
#   Date:         [Current Date]
#   Description:  This script runs the main analysis. It begins with the event
#                 study as a diagnostic, then estimates the main TWFE DiD
#                 models both with and without control variables, and finally
#                 presents the results in a comparative table.
#
# ---------------------------------------------------------------------------- #


# 1. SETUP -------------------------------------------------------------------

pacman::p_load(
  tidyverse, fixest, modelsummary, broom, gt, performance
)


# 2. LOAD FINAL ANALYSIS DATA ------------------------------------------------

analysis_df <- readRDS("data/processed/final_analysis_data.rds")


# 3. RUN THE EVENT STUDY MODEL -----------------------------------------------

# We interact the treatment variable with a factor of 'year', setting 2021
# as the reference year.
# **THE FIX**: We add `cluster = ~country_code_iso3c` to tell `feols` to
# compute standard errors that are robust to serial correlation within each country.

model_event_study <- feols(
  milex_as_gdp_share ~ i(year, treatment, ref = 2021) | # Dynamic interaction
    country_code_iso3c + year,       # Two-way fixed effects
  cluster = ~country_code_iso3c,                        # Clustered standard errors
  data = analysis_df
)

# You can see a summary of the model, which now includes a note about the
# clustered standard errors at the bottom.
summary(model_event_study)


# 4. CREATE AND SAVE EVENT STUDY REGRESSION TABLE ----------------------------

# We will create a clean, standalone table for just this model.
# We'll use the same programmatic method as before to build the coef_map.

# Get the raw coefficient names from the model
raw_names <- names(coef(model_event_study))

# Create the "pretty" names for the table
pretty_names <- c(
  "Tr. x 2004", "Tr. x 2005", "Tr. x 2006", "Tr. x 2007", "Tr. x 2008",
  "Tr. x 2009", "Tr. x 2010", "Tr. x 2011", "Tr. x 2012", "Tr. x 2013",
  "Tr. x 2014", "Tr. x 2015", "Tr. x 2016", "Tr. x 2017", "Tr. x 2018",
  "Tr. x 2019", "Tr. x 2020", "Tr. x 2022", "Tr. x 2023", "Tr. x 2024"
)

# Combine into the final coef_map
rows_to_keep <- setNames(pretty_names, raw_names)

# Create the table
event_study_table <- modelsummary(
  model_event_study,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = rows_to_keep,
  gof_map = c("nobs", "r2.within"),
  title = "Event Study Estimates of the Effect on Military Spending (% of GDP)",
  output = "gt"
)

# Print the table
event_study_table

# Save the table in multiple formats
gtsave(event_study_table, filename = "output/tables/event_study_table.rtf")
gtsave(event_study_table, filename = "output/tables/event_study_table.tex")


# 5. CREATE AND SAVE EVENT STUDY PLOT ----------------------------------------

# The `iplot` function from `fixest` automatically uses the clustered
# standard errors from the model to draw the confidence intervals.

event_study_plot <- iplot(
  model_event_study,
  main = "Event Study: Effect on Military Spending (% of GDP)",
  xlab = "Year",
  ylab = "Coefficient Estimate & 95% Confidence Interval"
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Print the plot
event_study_plot


# Save the plot
ggsave("output/figures/event_study_plot.pdf", plot = event_study_plot, width = 11, height = 7, device = cairo_pdf)
ggsave("output/figures/event_study_plot.png", plot = event_study_plot, width = 11, height = 7, dpi = 300)


# 6. MAIN ANALYSIS: TWFE DID MODELS ------------------------------------------

# --- Model 1: Unconditional DiD ---
model_unconditional <- feols(
  milex_as_gdp_share ~ treatment * post |
    country_code_iso3c + year,
  cluster = ~country_code_iso3c,
  data = analysis_df
)

# --- Model 2: Conditional DiD (with Controls) ---
model_conditional <- feols(
  milex_as_gdp_share ~ treatment * post + log_gdp_per_cap + gdp_growth |
    country_code_iso3c + year,
  cluster = ~country_code_iso3c,
  data = analysis_df
)


# 7. CREATE AND SAVE FINAL REGRESSION TABLE (FOR MAIN RESULTS) ---------------

main_models_list <- list(
  "Model 1" = model_unconditional,
  "Model 2" = model_conditional
)

# **THE FIX**: We only need to find the interaction term's name ONCE.
# modelsummary is smart enough to apply the renaming to all models.
raw_interaction_name <- names(coef(model_unconditional))[grepl(":", names(coef(model_unconditional)))]

# Create a simpler, non-duplicated coefficient map.
rows_to_keep <- c(
  setNames("Treatment x Post (ATT)", raw_interaction_name),
  "log_gdp_per_cap" = "Log(GDP per Capita)",
  "gdp_growth" = "GDP Growth (%)"
)

final_regression_table <- modelsummary(
  main_models_list,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = rows_to_keep,
  gof_map = c("nobs", "r2.within"),
  title = "Main DiD Estimates of the Effect on Military Spending (% of GDP)",
  output = "gt"
)

# Print the final table
final_regression_table

# Save the final table
gtsave(final_regression_table, filename = "output/tables/final_did_table.rtf")
gtsave(final_regression_table, filename = "output/tables/final_did_table.tex")


# 8. SCRIPT COMPLETION -------------------------------------------------------
print("Script 04 (Definitive) finished: Event study and main DiD models run. All tables and plots saved.")