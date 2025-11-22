# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       07_robustness_permutation_test.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-11-20
#   Description:  Runs a Fisher Randomization Test (Permutation Test).
#                 1. Shuffles treatment labels B times.
#                 2. Estimates the Main Model (Group Trends) on shuffled data.
#                 3. Compares the Actual Average ATT to the placebo distribution.
#                 4. Outputs distribution plots only.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

# --- Set Variables to Test ---
VARS_TO_TEST <- c("milex_cap")

# --- Parameters ---
B <- 5000 # Number of permutations
SEED <- 1234  # For reproducibility

# --- Define Directories ---
DIR_DATA    <- here::here("data", "_processed")
DIR_SCRIPTS <- here::here("scripts")
DIR_FIG     <- here::here("_output", "_figures", "_robustness")

if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

MASTER_PANEL <- file.path(DIR_DATA, "master_panel.rds")


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Data manipulation
  fixest,     # feols
  broom,      # tidy()
  glue,       # String interpolation
  here,       # File paths
  scales,     # Plot scales
  conflicted
)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mean", "base")

source(file.path(DIR_SCRIPTS, "00_functions.R")) # Custom themes

options(scipen = 999)


# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

master_panel <- readRDS(MASTER_PANEL)

# Create standard analysis dataframe
analysis_df <- master_panel %>%
  filter(group %in% c("control", "treatment")) %>%
  mutate(event_time = year - 2022)

# Get the unique list of units and their TRUE treatment status
unit_list <- analysis_df %>%
  distinct(iso3c, treat_dummy)


# 3. START PERMUTATION LOOP ==================================================
message(paste("--- Starting Permutation Tests for", length(VARS_TO_TEST), "variables ---"))
message(paste("--- Permutations per variable:", B, "---"))

for (VAR_TO_TEST in VARS_TO_TEST) {
  message(paste("\n=== Processing:", VAR_TO_TEST, "==="))

  # --------------------------------------------------------------------------
  # 3.1. CALCULATE ACTUAL STATISTIC (AVERAGE ATT)
  # --------------------------------------------------------------------------

  fml_actual <- as.formula(glue(
    "{VAR_TO_TEST} ~ i(event_time, treat_dummy, ref = c(-1, -8)) | iso3c + year + treat_dummy[year]"
  ))

  mod_actual <- feols(fml_actual, data = analysis_df, cluster = ~iso3c)

  # Identify Post-Treatment Terms (2022, 2023, 2024)
  coef_names <- names(coef(mod_actual))
  post_terms <- coef_names[grepl("event_time::", coef_names) & !grepl("-", coef_names)]

  # Calculate Average ATT
  stat_actual <- NA
  if(length(post_terms) > 0) {
    actual_coefs <- coef(mod_actual)[post_terms]
    stat_actual <- mean(actual_coefs)
  }

  message(paste("   Actual Average ATT:", round(stat_actual, 3)))


  # --------------------------------------------------------------------------
  # 3.2. RUN PERMUTATIONS
  # --------------------------------------------------------------------------
  placebo_stats <- numeric(B)
  set.seed(SEED)

  pb <- txtProgressBar(min = 0, max = B, style = 3)

  for (i in 1:B) {

    # A. Shuffle Treatment Labels
    unit_list_shuffled <- unit_list %>%
      mutate(treat_dummy_placebo = sample(treat_dummy)) %>%
      select(iso3c, treat_dummy_placebo)

    # B. Join shuffled labels
    data_placebo <- analysis_df %>%
      select(-treat_dummy) %>%
      left_join(unit_list_shuffled, by = "iso3c")

    # C. Run Placebo Model
    fml_placebo <- as.formula(glue(
      "{VAR_TO_TEST} ~ i(event_time, treat_dummy_placebo, ref = c(-1, -8)) | iso3c + year + treat_dummy_placebo[year]"
    ))

    mod_placebo <- try(feols(fml_placebo, data = data_placebo, cluster = ~iso3c), silent = TRUE)

    if (inherits(mod_placebo, "try-error")) {
      placebo_stats[i] <- NA
    } else {
      # D. Extract Average Placebo ATT
      p_coefs <- names(coef(mod_placebo))
      p_terms <- p_coefs[grepl("event_time::", p_coefs) & !grepl("-", p_coefs)]

      if(length(p_terms) > 0) {
        placebo_coefs <- coef(mod_placebo)[p_terms]
        placebo_stats[i] <- mean(placebo_coefs)
      } else {
        placebo_stats[i] <- NA
      }
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)

  # --------------------------------------------------------------------------
  # 3.3. CALCULATE ONE-SIDED P-VALUE & PLOT
  # --------------------------------------------------------------------------
  placebo_stats_clean <- na.omit(placebo_stats)
  n_clean <- length(placebo_stats_clean)

  # One-sided test: Proportion of placebos >= actual
  p_val_frt <- (sum(placebo_stats_clean >= stat_actual) + 1) / (n_clean + 1)

  message(paste("   Permutation p-value:", round(p_val_frt, 3)))

  # Plot Distribution
  plot_df <- data.frame(stats = placebo_stats_clean)

  p_label <- paste("p =", gtsummary::style_pvalue(p_val_frt, digits = 3))

  p_frt <- ggplot(plot_df, aes(x = stats)) +
    # Histogram
    geom_histogram(bins = 50, fill = "grey80", color = "white") +
    geom_vline(aes(xintercept = stat_actual), color = "grey40", linetype = "dashed") +
    annotate("text", x = stat_actual, y = Inf,
             label = p_label,
             vjust = 2, hjust = 1.1,
             color = "black", family = "IBM Plex Sans") +
    labs(
      title = glue("Permutation Test (Main Model): {VAR_TO_TEST}"),
      subtitle = glue("Distribution under {B} random assignments"),
      x = "Average Post-Treatment ATT",
      y = "Frequency",
      caption = "Black line indicates actual average post-treatment ATT"
    ) +
    theme_bachelor_project() +
    theme(panel.grid.major.y = element_blank())

  ggsave(file.path(DIR_FIG, glue("frt_plot_{VAR_TO_TEST}.png")), p_frt, width = 8, height = 5, bg = "white")
}

# 4. SCRIPT COMPLETION =======================================================
message(paste(
  "\n--- Script 07_robustness_permutation_test.R finished ---",
  "\nAll plots saved to:", DIR_FIG
))
