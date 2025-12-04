# ---------------------------------------------------------------------------- #
#
#   Script:       12_thesis_visualizations.R (NARRATIVE VERSION)
#   Description:  Generates plots with RAW KM labels but TRANSFORMED SCALES.
#                  This ensures straight regression lines AND readable axes.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, broom, glue, here, conflicted,
  ggrepel, scales
)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

DIR_SCRIPTS <- here::here("scripts")
DIR_FIG <- here::here("_output", "_figures", "_thesis_visuals")
DIR_DATA <- here::here("data", "_processed")

if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

MASTER_PANEL <- file.path(DIR_DATA, "master_panel.rds")
if (file.exists(file.path(DIR_SCRIPTS, "00_functions.R"))) {
  source(file.path(DIR_SCRIPTS, "00_functions.R"))
}

# --- DEFINE FIXED THRESHOLDS ---
FIXED_G_SPLINE_THRESHOLD <- 1200
FIXED_G_SPLINE_THRESHOLD_CRIMEA <- 600


# 1. PREPARE DATA ============================================================
message("--- Section 1: Preparing Cross-Sectional Data ---")

master_panel <- readRDS(MASTER_PANEL)

VARS_FOR_ANALYSIS <- c("milex_cap", "milex_gdp")

# Filter for Treatment Group + Key Years
df_cross <- master_panel %>%
  filter(group == "treatment") %>%
  filter(year %in% c(2014, 2021, 2024)) %>%
  select(iso3c, year, dist_min, dist_min_log, all_of(VARS_FOR_ANALYSIS))

# Create Wide Format & Calculate Deltas
df_delta <- df_cross %>%
  pivot_wider(
    id_cols = c(iso3c, dist_min, dist_min_log),
    names_from = year,
    values_from = all_of(VARS_FOR_ANALYSIS)
  )

# Calculate ALL necessary change columns
# Note: We DO NOT need pre-calculated transformed X columns for plotting anymore,
# because we will transform the scale inside ggplot.
plot_data_combined <- df_delta %>%
  mutate(
    # Changes for milex_cap
    change_cap_war = milex_cap_2024 - milex_cap_2021,
    change_cap_crimea = milex_cap_2021 - milex_cap_2014,
    change_cap_total = milex_cap_2024 - milex_cap_2014,

    # Changes for milex_gdp
    change_gdp_war = milex_gdp_2024 - milex_gdp_2021,
    change_gdp_crimea = milex_gdp_2021 - milex_gdp_2014,
    change_gdp_total = milex_gdp_2024 - milex_gdp_2014,

    # Spline variables (Still needed for the Spline Formula)
    dist_near_1200 = pmin(dist_min, FIXED_G_SPLINE_THRESHOLD),
    dist_far_1200 = pmax(dist_min - FIXED_G_SPLINE_THRESHOLD, 0),
    dist_near_600 = pmin(dist_min, FIXED_G_SPLINE_THRESHOLD_CRIMEA),
    dist_far_600 = pmax(dist_min - FIXED_G_SPLINE_THRESHOLD_CRIMEA, 0)
  )


# --- DEFINE ALL PLOTTING SCENARIOS ---
PLOTTING_SCENARIOS <- tribble(
  ~y_col, ~var_type, ~period_title, ~file_tag, ~y_axis_label,
  "change_cap_war", "cap", "War Shock (2021–2024)", "cap_shock", "Increase in Spending ($ US / Capita)",
  "change_cap_crimea", "cap", "Crimea Legacy (2014–2021)", "cap_legacy", "Increase in Spending ($ US / Capita)",
  "change_cap_total", "cap", "Total Shift (2014–2024)", "cap_total", "Increase in Spending ($ US / Capita)",

  "change_gdp_war", "gdp", "War Shock (2021–2024)", "gdp_shock", "Increase in Spending (% GDP)",
  "change_gdp_crimea", "gdp", "Crimea Legacy (2014–2021)", "gdp_legacy", "Increase in Spending (% GDP)",
  "change_gdp_total", "gdp", "Total Shift (2014–2024)", "gdp_total", "Increase in Spending (% GDP)"
)


# 2. PLOTTING FUNCTIONS ======================================================
message("--- Section 2: Defining Plotting Functions ---")

# --- Universal Base Plotting Function ---
# Handles the logic of plotting RAW x but TRANSFORMING the scale
create_base_plot <- function(data, x_var = "dist_min", y_var, title, subtitle, y_label, caption, x_transform = "identity") {

  x_aes <- sym(x_var)
  y_aes <- sym(y_var)

  p <- ggplot(data, aes(x = !!x_aes, y = !!y_aes)) +
    # LOESS (Blue Dashed)
    geom_smooth(method = "loess", formula = y ~ x,
                color = "#2980b9", se = FALSE, linetype = "dashed", linewidth = 0.8) +

    geom_point(color = "#2c3e50", size = 2.5, alpha = 0.8) +
    geom_text_repel(aes(label = iso3c), size = 3, box.padding = 0.3, color = "grey30") +

    labs(
      title = title,
      subtitle = subtitle,
      x = "Min. Distance to Conflict Zone (km)", # Always "km" for readability
      y = y_label,
      caption = caption
    ) +
    theme_bachelor_project()

  # --- Apply Scale Transformation ---
  if (x_transform == "log10") {
    # Log Scale: Breaks at logical km points
    p <- p + scale_x_continuous(trans = "log10",
                                breaks = c(200, 500, 1000, 2000, 4000),
                                labels = comma_format())
  } else if (x_transform == "sqrt") {
    # Sqrt Scale: Breaks at logical km points
    p <- p + scale_x_continuous(trans = "sqrt",
                                breaks = c(100, 500, 1000, 2000, 3000, 4000),
                                labels = comma_format())
  } else {
    # Linear Scale
    p <- p + scale_x_continuous(labels = comma_format())
  }

  return(p)
}


# --- 1. LOG-LINEAR PLOTS ---
plot_log_linear <- function(scenario) {
  y_col <- scenario$y_col

  # Stats Calculation
  lm_fit <- lm(as.formula(glue("{y_col} ~ dist_min_log")), data = plot_data_combined)
  stats <- broom::tidy(lm_fit)
  r2_val <- summary(lm_fit)$r.squared
  slope_est <- formatC(stats$estimate[2], format = "g", digits = 3)
  p_val <- stats$p.value[2]

  # Plotting: Use RAW dist_min, but transform axis to log10
  p <- create_base_plot(
    data = plot_data_combined,
    y_var = y_col,
    title = glue("{scenario$period_title}: Log-Linear Fit"),
    subtitle = glue("Log-Linear (Red): Slope={slope_est}, p={sprintf('%.3f', p_val)}, R2={round(r2_val, 3)}."),
    y_label = scenario$y_axis_label,
    caption = "X-axis uses a Logarithmic Scale to visualize the exponential decay.",
    x_transform = "log10"
  ) +
    # geom_smooth needs formula y ~ log(x) because we are plotting raw x on log scale
    geom_smooth(method = "lm", formula = y ~ log(x), color = "#c0392b", fill = "#c0392b", alpha = 0.1, se = TRUE)

  ggsave(file.path(DIR_FIG, glue("{scenario$file_tag}_LOG_LINEAR.png")), p, width = 8, height = 6, bg = "white")
}


# --- 2. SQUARE ROOT PLOTS ---
plot_square_root <- function(scenario) {
  y_col <- scenario$y_col

  # Stats Calculation (Model uses transformed var)
  lm_fit <- lm(as.formula(glue("{y_col} ~ sqrt(dist_min)")), data = plot_data_combined)
  stats <- broom::tidy(lm_fit)
  r2_val <- summary(lm_fit)$r.squared
  slope_est <- formatC(stats$estimate[2], format = "g", digits = 3)
  p_val <- stats$p.value[2]

  # Plotting: Use RAW dist_min, but transform axis to sqrt
  p <- create_base_plot(
    data = plot_data_combined,
    y_var = y_col,
    title = glue("{scenario$period_title}: Square Root Fit"),
    subtitle = glue("Square Root (Red): Slope={slope_est}, p={sprintf('%.3f', p_val)}, R2={round(r2_val, 3)}."),
    y_label = scenario$y_axis_label,
    caption = "X-axis uses a Square Root Scale to visualize hyper-diminishing returns.",
    x_transform = "sqrt"
  ) +
    # geom_smooth needs formula y ~ sqrt(x) because we are plotting raw x
    geom_smooth(method = "lm", formula = y ~ sqrt(x), color = "#c0392b", fill = "#c0392b", alpha = 0.1, se = TRUE)

  ggsave(file.path(DIR_FIG, glue("{scenario$file_tag}_SQRT.png")), p, width = 8, height = 6, bg = "white")
}


# --- 3. QUADRATIC PLOTS ---
plot_quadratic <- function(scenario) {
  y_col <- scenario$y_col

  lm_fit <- lm(as.formula(glue("{y_col} ~ poly(dist_min, 2)")), data = plot_data_combined)
  stats <- broom::tidy(lm_fit)
  r2_val <- summary(lm_fit)$r.squared
  slope_sq <- formatC(stats$estimate[3], format = "g", digits = 3)
  p_val_sq <- stats$p.value[3]

  # Plotting: Quadratic is best seen on a Linear Scale to show the bend
  p <- create_base_plot(
    data = plot_data_combined,
    y_var = y_col,
    title = glue("{scenario$period_title}: Quadratic Fit"),
    subtitle = glue("Quadratic (Red): Squared Term={slope_sq}, p={sprintf('%.3f', p_val_sq)}, R2={round(r2_val, 3)}."),
    y_label = scenario$y_axis_label,
    caption = "Quadratic model plotted on linear distance scale to show curvature.",
    x_transform = "linear"
  ) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "#c0392b", fill = "#c0392b", alpha = 0.1, se = TRUE)

  ggsave(file.path(DIR_FIG, glue("{scenario$file_tag}_QUADRATIC.png")), p, width = 8, height = 6, bg = "white")
}


# --- 4. SPLINE PLOTS ---
plot_spline <- function(scenario) {
  y_col <- scenario$y_col
  period <- scenario$period_title

  if (str_detect(scenario$file_tag, "legacy")) {
    T_val <- FIXED_G_SPLINE_THRESHOLD_CRIMEA
  } else {
    T_val <- FIXED_G_SPLINE_THRESHOLD
  }

  # Logic for formula selection
  if (T_val == 1200) {
    formula_str <- glue("{y_col} ~ dist_near_1200 + dist_far_1200")
    smooth_formula <- y ~ pmin(x, 1200) + pmax(x - 1200, 0)
  } else {
    formula_str <- glue("{y_col} ~ dist_near_600 + dist_far_600")
    smooth_formula <- y ~ pmin(x, 600) + pmax(x - 600, 0)
  }

  lm_fit <- lm(as.formula(formula_str), data = plot_data_combined)
  stats <- broom::tidy(lm_fit)
  r2_val <- summary(lm_fit)$r.squared
  est_near <- formatC(stats$estimate[2], format = "g", digits = 3)
  p_val_near <- stats$p.value[2]
  est_far <- formatC(stats$estimate[3], format = "g", digits = 3)
  p_val_far <- stats$p.value[3]

  # Plotting: Spline Kinks are best seen on Linear Scale
  p <- create_base_plot(
    data = plot_data_combined,
    y_var = y_col,
    title = glue("{period}: Spline Model Kink at {T_val} km"),
    subtitle = glue("Near: Slope={est_near} (p={sprintf('%.3f', p_val_near)}) | Far: Slope={est_far} (p={sprintf('%.3f', p_val_far)}) | R2={round(r2_val, 3)}"),
    y_label = scenario$y_axis_label,
    caption = "Spline plotted on linear distance scale to show the structural break (kink).",
    x_transform = "linear"
  ) +
    geom_smooth(method = "lm", formula = smooth_formula,
                color = "#c0392b", fill = "#c0392b", alpha = 0.1, se = TRUE)

  ggsave(file.path(DIR_FIG, glue("{scenario$file_tag}_SPLINE_{T_val}km.png")), p, width = 8, height = 6, bg = "white")
}


# 3. EXECUTION LOOP ==========================================================
message("--- Section 3: Executing Plotting Loops ---")

message("Generating Log-Linear Comparison Plots (All 6 scenarios)...")
pwalk(PLOTTING_SCENARIOS, function(...) plot_log_linear(list(...)))

message("Generating Square Root Plots (milex_cap only)...")
PLOTTING_SCENARIOS %>%
  filter(var_type == "cap") %>%
  pwalk(function(...) plot_square_root(list(...)))

message("Generating Quadratic Plots (milex_gdp only)...")
PLOTTING_SCENARIOS %>%
  filter(var_type == "gdp") %>%
  pwalk(function(...) plot_quadratic(list(...)))

message("Generating Spline Plots (milex_gdp only)...")
PLOTTING_SCENARIOS %>%
  filter(var_type == "gdp") %>%
  pwalk(function(...) plot_spline(list(...)))

message("\n--- Script 12_thesis_visualizations.R finished. ---")
