# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       05_robustness_and_extensions.R
#   Author:       [Your Name]
#   Date:         [Current Date]
#   Description:  This script conducts robustness checks and extensions.
#                 Part 1 tests the sensitivity of the main results to the
#                 exclusion of influential outliers (Greece).
#
# ---------------------------------------------------------------------------- #


# 1. SETUP -------------------------------------------------------------------

pacman::p_load(
  tidyverse, fixest, modelsummary, gt, augsynth
)


# 2. LOAD FINAL ANALYSIS DATA ------------------------------------------------

analysis_df <- readRDS("data/processed/final_analysis_data.rds")


# 3. ROBUSTNESS CHECK 1: EXCLUDING OUTLIERS (GREECE) -------------------------

# --- a. Create a new dataset that filters out Greece ---
# The ISO3c code for Greece is "GRC".
analysis_df_no_greece <- analysis_df %>%
  filter(country_code_iso3c != "GRC")

# --- b. Re-run the main conditional DiD model on the new dataset ---
# This is the same preferred specification from Script 4.
model_conditional_no_greece <- feols(
  milex_as_gdp_share ~ treatment * post + log_gdp_per_cap + gdp_growth |
    country_code_iso3c + year,
  cluster = ~country_code_iso3c,
  data = analysis_df_no_greece # IMPORTANT: Use the filtered data
)

# --- c. Load the original model for comparison ---
# To compare, we need to re-run the original model from Script 4 as well.
model_conditional_original <- feols(
  milex_as_gdp_share ~ treatment * post + log_gdp_per_cap + gdp_growth |
    country_code_iso3c + year,
  cluster = ~country_code_iso3c,
  data = analysis_df # Use the original, full dataset
)


# 4. CREATE AND SAVE COMPARATIVE TABLE ---------------------------------------

# Create a list of the two models for the table
robustness_models_list <- list(
  "(1) Main Model" = model_conditional_original,
  "(2) Excluding Greece" = model_conditional_no_greece
)

# Create a clean coefficient map
raw_interaction_name <- names(coef(model_conditional_original))[grepl(":", names(coef(model_conditional_original)))]
rows_to_keep <- c(
  setNames("Treatment x Post (ATT)", raw_interaction_name),
  "log_gdp_per_cap" = "Log(GDP per Capita)",
  "gdp_growth" = "GDP Growth (%)"
)

# Create the final table
robustness_outlier_table <- modelsummary(
  robustness_models_list,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = rows_to_keep,
  gof_map = c("nobs", "r2.within"),
  title = "Robustness Check: Excluding Outliers (Greece)",
  output = "gt"
)

# Print the table
robustness_outlier_table

# Save the table
gtsave(robustness_outlier_table, filename = "output/tables/robustness_check_no_greece.rtf")
gtsave(robustness_outlier_table, filename = "output/tables/robustness_check_no_greece.tex")


# 5. ROBUSTNESS CHECK 2: PLACEBO IN TIME (Corrected) ------------------------

# --- a. Create a placebo dataset ---
# We create a new 'post' variable pretending the treatment happened in 2020,
# as this is the most methodologically sound choice based on the event study results.
analysis_df_placebo <- analysis_df %>%
  mutate(post_placebo = ifelse(year >= 2020, 1, 0))

# --- b. Run the DiD model with the placebo variable ---
# We interact 'treatment' with our new 'post_placebo' variable.
model_placebo_2020 <- feols(
  milex_as_gdp_share ~ treatment * post_placebo + log_gdp_per_cap + gdp_growth |
    country_code_iso3c + year,
  cluster = ~country_code_iso3c,
  data = analysis_df_placebo # Use the placebo dataset
)


# 6. UPDATE AND SAVE FINAL COMPARATIVE TABLE ---------------------------------

# We will now create a final table that includes all three models:
# (1) The original main model
# (2) The model excluding Greece
# (3) The new, corrected placebo model

# Add the placebo model to our list
final_robustness_list <- list(
  "(1) Main Model" = model_conditional_original,
  "(2) Excluding Greece" = model_conditional_no_greece,
  "(3) Placebo (2020)" = model_placebo_2020
)

# Find the raw interaction names for all models
raw_interaction_main <- names(coef(model_conditional_original))[grepl(":", names(coef(model_conditional_original)))]
raw_interaction_placebo <- names(coef(model_placebo_2020))[grepl(":", names(coef(model_placebo_2020)))]

# Create the final, comprehensive coefficient map
final_rows_to_keep <- c(
  setNames("Treatment x Post (ATT)", raw_interaction_main),
  setNames("Treatment x Placebo Post", raw_interaction_placebo),
  "log_gdp_per_cap" = "Log(GDP per Capita)",
  "gdp_growth" = "GDP Growth (%)"
)


# Create the final table
final_robustness_table <- modelsummary(
  final_robustness_list,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = final_rows_to_keep,
  gof_map = c("nobs", "r2.within"),
  title = "Main Results and Robustness Checks",
  output = "gt"
)

# Print the final table
final_robustness_table

# Save the final table
gtsave(final_robustness_table, filename = "output/tables/final_robustness_table.rtf")
gtsave(final_robustness_table, filename = "output/tables/final_robustness_table.tex")


# 7. ADVANCED METHOD: AUGMENTED SYNTHETIC CONTROL (ASCM) ---------------------
#   Given the failure of the placebo test, we now turn to a more robust
#   method that does not rely on the strict parallel trends assumption.

# --- b. Prepare the data for ASCM ---
# The augsynth function requires a 'wide' format dataframe, where each row is a
# country and columns represent years. We also need to separate the outcome
# from the covariates.
# We will use only the pre-treatment data to build the synthetic control.
pre_treatment_df <- analysis_df %>%
  filter(year < 2022)

# --- c. Run the ASCM estimation ---
# The `augsynth()` function is powerful. We tell it the outcome, the treatment
# variable, the unit and time identifiers, and the data. It handles the rest.
# `progfunc = "ridge"` is a robust choice for the model that predicts the outcome.
ascm_estimate <- augsynth(
  milex_as_gdp_share ~ treatment | log_gdp_per_cap + gdp_growth,
  unit = country_code_iso3c,
  time = year,
  data = analysis_df,
  t_int = 2022, # Explicitly define the intervention year
  progfunc = "ridge",
  scm = TRUE
)

# --- d. Print the results summary ---
# This provides the Average Treatment Effect on the Treated (ATT)
# and its confidence interval, calculated using a robust method.
print("ASCM Average Treatment Effect on the Treated (ATT):")
summary(ascm_estimate)


# 8. CREATE AND SAVE ASCM PLOT AND TABLE -------------------------------------

# --- a. Create the main ASCM plot ---
ascm_plot <- plot(ascm_estimate) +
  labs(
    title = "ASCM Estimate of the Effect of the 2022 Invasion",
    subtitle = "Comparing the treated group to its synthetic counterfactual",
    caption = "The synthetic control is a data-driven weighted average of control group countries."
  )

# Print the plot
ascm_plot

# Save the plot
ggsave("output/figures/ascm_plot.pdf", plot = ascm_plot, width = 11, height = 7, device = cairo_pdf)


# --- b. Create a summary table for the ASCM result ---

# First, get the summary of our ASCM model
ascm_summary <- summary(ascm_estimate)

# Next, create our clean results data frame
ascm_results_df <- tibble(
  term = "Average Treatment Effect (ATT)",
  estimate = ascm_summary$average_att$Estimate,
  std.error = ascm_summary$average_att$Std.Error,
  p.value = 2 * pnorm(-abs(ascm_summary$average_att$Estimate / ascm_summary$average_att$Std.Error))
)

# Now, build the table with 'gt'
ascm_table <- ascm_results_df %>%
  gt() %>%
  tab_header(
    title = "ASCM Estimate of the Average Treatment Effect"
  ) %>%
  fmt_number(
    columns = c(estimate, std.error),
    decimals = 3
  ) %>%
  # Add significance stars to the estimate column
  text_transform(
    locations = cells_body(columns = estimate),
    fn = function(x) {
      est <- as.numeric(x)
      p <- ascm_results_df$p.value
      stars <- case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", TRUE ~ "")
      paste0(x, stars)
    }
  ) %>%
  
  # **THE FIX**: Use text_transform() again to format the standard error.
  # This is the correct, modern function for this task.
  text_transform(
    locations = cells_body(columns = std.error),
    fn = function(x) paste0("(", x, ")")
  ) %>%
  
  cols_hide(columns = p.value) %>%
  cols_label(
    term = "",
    estimate = "Estimate",
    std.error = ""
  ) %>%
  tab_footnote(
    footnote = "* p < 0.1, ** p < 0.05, *** p < 0.01",
    locations = cells_column_labels(columns = estimate)
  )

# Print the final table
ascm_table

# Save the table
gtsave(ascm_table, filename = "output/tables/ascm_att_table.rtf")
gtsave(ascm_table, filename = "output/tables/ascm_att_table.tex")


# --- c. Conduct Placebo Inference Test for ASCM p-value ---
# This is the state-of-the-art method for calculating statistical significance
# for a synthetic control estimate. It can be computationally intensive and may
# take a few minutes to run.

ascm_inference <- summary(ascm_estimate, type = "inference")

# Print the inference summary, which now includes a p-value
print("ASCM Inference Summary with P-Value:")
print(ascm_inference)

# The `plot()` function will now create a new plot showing the distribution
# of the placebo effects (in grey) against the real effect (in black).
placebo_plot <- plot(ascm_inference) +
  labs(
    title = "ASCM Placebo Inference Test",
    subtitle = "Comparing the real treatment effect to the distribution of placebo effects",
    caption = "The p-value is the proportion of placebo effects (grey) as extreme as the real effect (black)."
  )

# Print the placebo plot
placebo_plot

# Save the placebo plot
ggsave("output/figures/ascm_placebo_plot.pdf", plot = placebo_plot, width = 11, height = 7)


# 9. SCRIPT COMPLETION -------------------------------------------------------
print("Script 05 (Parts 1, 2, & 3) finished: Outlier, Placebo, and ASCM analyses complete.")