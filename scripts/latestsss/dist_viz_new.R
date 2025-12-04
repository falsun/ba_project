# ---------------------------------------------------------------------------- #
#
#   Script:       12b_functional_form_plots.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Description:  Generates scatter plots for the Best Fit functional forms.
#                 Plots Raw Distance (km) on a Log Axis for readability,
#                 with the specific regression curve overlaid.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, broom, glue, here, ggrepel, scales)

DIR_FIG <- here::here("_output", "_figures", "_functional_forms")
DIR_DATA <- here::here("data", "_processed")

if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

OLS_DATA <- file.path(DIR_DATA, "ols_data.rds")
if (!file.exists(OLS_DATA)) stop("Run Data Prep first.")

# Load theme if available
if (file.exists(here::here("scripts", "00_functions.R"))) {
  source(here::here("scripts", "00_functions.R"))
} else {
  theme_bachelor_project <- function(...) theme_minimal()
}

# 1. PREPARE DATA ============================================================
message("--- Section 1: Loading Data ---")

# Load OLS Data
df <- readRDS(OLS_DATA) %>%
  mutate(
    # Outcome Variables (Renamed for ease)
    y_gdp_war    = mil_gdp_nato_post_dif,
    y_gdp_crimea = mil_gdp_nato_pre_dif
  )

# Define Scenarios (GDP Only)
SCENARIOS <- list(
  "war" = list(
    y_col = "y_gdp_war",
    title = "War Shock (2021-2025)",
    file_tag = "gdp_war"
  ),
  "crimea" = list(
    y_col = "y_gdp_crimea",
    title = "Crimea Legacy (2014-2021)",
    file_tag = "gdp_crimea"
  )
)


# 2. PLOTTING FUNCTION =======================================================
message("--- Section 2: Defining Plot Function ---")

plot_form_on_raw_dist <- function(data, y_col, formula_str, form_name, title, file_tag) {

  # 1. Run Model to get Stats
  # We construct the formula using the transformed variables for calculation
  # (We need to know which transformed var corresponds to the formula string)

  # Map formula string to the variable name in 'df' for the lm() stats
  # This ensures the stats match the curve
  reg_formula <- case_when(
    form_name == "Log-Linear"   ~ paste(y_col, "~ log(dist_min)"),
    form_name == "Inverse Log"  ~ paste(y_col, "~ I(1/log(dist_min))"),
    form_name == "Inverse Sqrt" ~ paste(y_col, "~ I(1/sqrt(dist_min))")
  )

  mod <- lm(as.formula(reg_formula), data = data)
  stats <- tidy(mod)
  r2 <- summary(mod)$r.squared

  # Extract slope (2nd term)
  slope <- formatC(stats$estimate[2], format = "f", digits = 2)
  p_val <- formatC(stats$p.value[2], format = "f", digits = 4)

  # 2. Create Plot
  # We plot x = dist_min (RAW) and let geom_smooth draw the curve
  p <- ggplot(data, aes(x = dist_min, y = .data[[y_col]])) +

    # LOESS (Blue Dashed) - Reference
    geom_smooth(method = "loess", formula = y ~ x,
                color = "#2980b9", se = FALSE, linetype = "dashed", linewidth = 0.8) +

    # Model Fit (Red) - Uses the specific functional form
    geom_smooth(method = "lm", formula = as.formula(formula_str),
                color = "#c0392b", fill = "#c0392b", alpha = 0.1, se = TRUE) +

    geom_point(color = "#2c3e50", size = 2.5, alpha = 0.8) +
    geom_text_repel(aes(label = iso3c), size = 3, box.padding = 0.3, color = "grey30") +

    # X-Axis: Log Scale with KM Labels
    scale_x_log10(breaks = c(200, 500, 1000, 2000, 4000), labels = comma_format()) +

    labs(
      title = glue("{title}: {form_name}"),
      subtitle = glue("{form_name} Fit: Slope={slope}, p={p_val}, R2={round(r2, 2)}"),
      x = "Min. Distance to Conflict Zone (km)",
      y = "Change in Military Spending (% GDP)",
      caption = glue("Model: {reg_formula}")
    ) +
    theme_bachelor_project()

  # Save
  clean_form <- str_replace_all(str_to_lower(form_name), " ", "_")
  ggsave(file.path(DIR_FIG, glue("{file_tag}_{clean_form}.png")), p, width = 8, height = 6, bg = "white")
}


# 3. EXECUTE PLOTS ===========================================================
message("--- Section 3: Generating Plots ---")

for (s_name in names(SCENARIOS)) {
  s <- SCENARIOS[[s_name]]

  # A. Log-Linear: y ~ log(x)
  plot_form_on_raw_dist(df, s$y_col, "y ~ log(x)", "Log-Linear", s$title, s$file_tag)

  # B. Inverse Log: y ~ 1/log(x)
  # Note: geom_smooth formula uses 'x' as the placeholder for the x-axis data (dist_min)
  plot_form_on_raw_dist(df, s$y_col, "y ~ I(1/log(x))", "Inverse Log", s$title, s$file_tag)

  # C. Inverse Square Root: y ~ 1/sqrt(x)
  plot_form_on_raw_dist(df, s$y_col, "y ~ I(1/sqrt(x))", "Inverse Sqrt", s$title, s$file_tag)
}

message("\n--- Script 12b_functional_form_plots.R finished ---")
message("Plots saved to: _output/_figures/_functional_forms")
