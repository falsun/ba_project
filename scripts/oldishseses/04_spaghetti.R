# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       04_outcome_spaghetti_interactive.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-11-09
#   Description:  This script loops through all outcome variables and creates
#                 interactive (plotly) spaghetti plots for exploration.
#
# ---------------------------------------------------------------------------- #


# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

DIR_DATA <- here::here("data", "_processed")
DIR_SCRIPTS <- here::here("scripts")
DIR_FIG <- here::here("_output", "_figures", "_eda", "_interactive_spaghetti_plots")

if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

MASTER_PANEL <- file.path(DIR_DATA, "master_panel.rds")
TREATMENT_YEAR <- 2022

# Using the variable list from your 03_outcome_eda.R script
VARS_FOR_PLOTTING <- c(
  "milex_usd_log"    = "Log(Milex, Constant 2023 US$)",
  "milex_gdp"        = "Milex (% of GDP)",
  "milex_gdp_log"    = "Log(Milex % of GDP)",
  "milex_gov"        = "Milex (% of Govt. Spending)",
  "milex_gov_log"    = "Log(Milex % of Govt. Spending)",
  "milex_cap"        = "Milex per Capita",
  "milex_cap_log"    = "Log(Milex per Capita)",
  "pop"              = "Population",
  "gdp_cap"          = "GDP per Capita (PPP, constant 2017 intl. $)",
  "trade_gdp"        = "Trade Openness (% of GDP)",
  "lib_dem"          = "V-Dem Liberal Democracy Score (0-1 Index)"
)


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, glue, plotly, htmlwidgets, crosstalk, conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

source(file.path(DIR_SCRIPTS, "00_functions.R"))


# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

master_panel <- readRDS(MASTER_PANEL)

ts_df <- master_panel %>%
  filter(group %in% c("control", "treatment")) %>%
  mutate(group = factor(group, levels = c("treatment", "control")))

# --- Create the SharedData object ONCE, outside the loop ---
shared_ts_df <- crosstalk::SharedData$new(ts_df, key = ~iso3c)


# 3. GENERATE ALL INTERACTIVE PLOTS ==========================================
message("--- Section 3: Generating All Interactive Spaghetti Plots ---")

# --- Loop over each VARIABLE ---
for (var in names(VARS_FOR_PLOTTING)) {
  # Set dynamic variable names
  var_sym <- sym(var)
  var_label <- VARS_FOR_PLOTTING[var]

  message(paste("\n--- Processing Variable:", var_label, "---"))

  tryCatch(
    {
      # --- 1. Create the base ggplot ---
      p_static <- shared_ts_df %>%
        ggplot(aes(
          x = year,
          y = !!var_sym,
          group = iso3c,
          color = group, # Color by group
          text = paste(
            "Country:", iso3c,
            "\nYear:", year,
            "\nValue:", round(!!var_sym, 2)
          )
        )) +
        geom_line(alpha = 0.5) +
        geom_vline(xintercept = TREATMENT_YEAR - 1, linetype = "dashed", color = "grey40") +
        scale_x_continuous(breaks = seq(2014, 2024, by = 2)) +
        scale_color_project_qual(name = NULL) + # Use your theme colors
        theme_bachelor_project() +
        labs(
          title = glue::glue("Individual Trends: {var_label}"),
          x = NULL,
          y = var_label
        ) +
        theme(axis.text.x = element_text()) # Force x-axis text

      # --- 2. Convert to interactive plotly & apply highlight ---
      p_interactive <- plotly::ggplotly(p_static, tooltip = "text") %>%
        plotly::highlight(
          on = "plotly_hover",
          off = "plotly_doubleclick",
          persistent = FALSE,
          dynamic = TRUE
          # Note: We remove 'color = "orange"' so it
          # uses the default (brighten/thicken) on the
          # existing blue/orange line.
        )

      # --- 3. Save as an .html file ---
      file_name <- glue::glue("interactive_spag_{var}.html") # Dynamic filename
      file_path <- file.path(DIR_FIG, file_name)

      htmlwidgets::saveWidget(p_interactive, file_path, selfcontained = TRUE)

      message(paste("  --- Plot saved to:", file_path, "---"))
    },
    error = function(e) {
      message(paste("     ERROR for", var, ":", e$message))
    }
  )
}

# 4. SCRIPT COMPLETION =======================================================
message(paste(
  "\n--- Script 04_outcome_spaghetti_interactive.R finished ---",
  "\nAll interactive .html plots saved to:", DIR_FIG
))
