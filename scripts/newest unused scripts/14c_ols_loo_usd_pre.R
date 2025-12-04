# ---------------------------------------------------------------------------- #
#   Script: 12_sensitivity_jackknife.R
#   Description: Full Leave-One-Out Analysis for the FULL Model (Model 6)
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(broom)
library(here)
library(modelsummary)
library(lmtest)
library(sandwich)

DIR_SCRIPTS <- here::here("scripts")

# Source Theme Function (if exists)
if (file.exists(here::here("scripts", "00_functions.R"))) {
  source(here::here("scripts", "00_functions.R"))
} else {
  # Fallback if function script missing
  theme_gt_bachelor_project <- function(data) { data }
}

# Load Data
ols_data <- readRDS(here("data", "_processed", "ols_data.rds"))

# --- KEY CHANGE: Use Model 6 (Full Specification) ---
# This allows us to test if Debt stays insignificant
formula_full <- milex_usd_pre ~ dist_enemy_log + border_rus + nato_gap_2014 + debt_gdp_2014_log

# ---------------------------------------------------------------------------- #
# 1. RUN JACKKNIFE LOOP
# ---------------------------------------------------------------------------- #

# Create a list of all countries
countries <- ols_data$iso3c

# Loop through each country, remove it, run model, save results
jackknife_results <- map_dfr(countries, function(dropped_country) {

  # Filter Data (N-1)
  data_subset <- ols_data %>% filter(iso3c != dropped_country)

  # Run Model
  model <- lm(formula_full, data = data_subset)

  # Get Robust Standard Errors (HC3)
  tidy_res <- tidy(coeftest(model, vcov = vcovHC(model, type = "HC3")))

  # Extract relevant coefficients (Added Debt)
  tidy_res %>%
    filter(term %in% c("dist_enemy_log", "border_rus", "nato_gap_2014", "debt_gdp_2014_log")) %>%
    select(term, estimate, p.value) %>%
    mutate(dropped_iso3c = dropped_country)
})

# ---------------------------------------------------------------------------- #
# 2. CALCULATE REFERENCE LINES (TRUE FULL MODEL COEFFICIENTS)
# ---------------------------------------------------------------------------- #

# Fit the full model once to get the "True" benchmarks
model_full <- lm(formula_full, data = ols_data)

# Extract the true coefficients into a dataframe
true_coeffs <- tidy(model_full) %>%
  filter(term %in% c("dist_enemy_log", "border_rus", "nato_gap_2014", "debt_gdp_2014_log")) %>%
  mutate(
    term_label = case_when(
      term == "dist_enemy_log" ~ "Log afstand til fjende (km)",
      term == "border_rus" ~ "Delt grænse med Rusland (dummy)",
      term == "nato_gap_2014" ~ "Afstand til 2% mål (procentpoint)",
      term == "debt_gdp_2014_log" ~ "Log offentlig gæld (% BNP)",
      TRUE ~ term
    )
  )

# ---------------------------------------------------------------------------- #
# 3. VISUALIZE STABILITY (Corrected Order)
# ---------------------------------------------------------------------------- #

# Define the exact labels and their desired order
label_levels <- c(
  "Delt grænse med Rusland (dummy)",  # First
  "Log afstand til fjende (km)",      # Second
  "Afstand til 2% mål (procentpoint)",               # Third
  "Log offentlig gæld (% BNP)"        # Fourth
)

plot_data <- jackknife_results %>%
  mutate(
    # Create the label text first
    term_label_text = case_when(
      term == "dist_enemy_log" ~ "Log afstand til fjende (km)",
      term == "border_rus" ~ "Delt grænse med Rusland (dummy)",
      term == "nato_gap_2014" ~ "Afstand til 2% mål (procentpoint)",
      term == "debt_gdp_2014_log" ~ "Log offentlig gæld (% BNP)",
      TRUE ~ term
    ),
    # Convert to Factor with explicit levels to control sort order
    term_label = factor(term_label_text, levels = label_levels),

    significance = ifelse(p.value < 0.05, "Signifikant", "Insignifikant")
  )

# --- CRITICAL: Do the same for the Reference Line Data ---
# If you don't factorize this too, ggplot might get confused or drop lines
true_coeffs <- true_coeffs %>%
  mutate(
    term_label_text = case_when(
      term == "dist_enemy_log" ~ "Log afstand til fjende (km)",
      term == "border_rus" ~ "Delt grænse med Rusland (dummy)",
      term == "nato_gap_2014" ~ "Afstand til 2% mål (procentpoint)",
      term == "debt_gdp_2014_log" ~ "Log offentlig gæld (% BNP)",
      TRUE ~ term
    ),
    term_label = factor(term_label_text, levels = label_levels)
  )

p_jack <- ggplot(plot_data, aes(x = dropped_iso3c, y = estimate)) +

  # A. The True Model Reference Line (Per Facet)
  geom_hline(data = true_coeffs, aes(yintercept = estimate),
             linetype = "dashed", color = "grey40") +

  # B. The Jackknife Dots
  geom_point(aes(color = significance), size = 2) +

  facet_wrap(~term_label, scales = "free_y", ncol = 1) +

  scale_color_manual(values = c("Signifikant" = "black", "Insignifikant" = "#c0392b")) +

  scale_y_continuous(
    # This function takes the axis limits (x) and returns exactly 3 evenly spaced points
    breaks = function(x) seq(from = x[1], to = x[2], length.out = 3),
    labels = scales::number_format(accuracy = 0.01, decimal.mark = ",")
  ) +

  labs(
    title = "Jackknife Sensitivity Analysis (Model 6)",
    y = "Koefficient",
    x = "Ekskluderet Land",
    caption = "Stiplet linje indikerer reel koefficient.",
    color = "P < 0.05"
  ) +
  theme_bachelor_project() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    strip.text = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.major.x = element_line()
  )

# Save
ggsave(here("_output", "_figures", "ols_loo_usd_pre.png"), p_jack, width = 8, height = 8, bg = "white")
# ---------------------------------------------------------------------------- #
# 3. SUMMARY STATS (The "Stability Check")
# ---------------------------------------------------------------------------- #
summary_stats <- jackknife_results %>%
  group_by(term) %>%
  summarise(
    Avg_Coeff = mean(estimate),
    Min_P = min(p.value),
    Max_P = max(p.value),
    # For Border/Gap, we want Stable = TRUE if Max_P < 0.05 (Always significant)
    # For Debt, we want Stable = TRUE if Min_P > 0.05 (Never significant)
    Stable_Sig = all(p.value < 0.05),
    Stable_Null = all(p.value > 0.05)
  )

print(summary_stats)
