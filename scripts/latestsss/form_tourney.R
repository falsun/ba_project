# ---------------------------------------------------------------------------- #
#
#   Script:       25_functional_form_stability_test.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Description:  Automated "Tournament" to find the single best functional form
#                 across BOTH periods and ALL jackknife samples.
#                 Uses the Full Model (Dist + Gap + Debt).
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION ===========================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, broom, glue, here, gt)

DIR_DATA <- here::here("data", "_processed")
DIR_TAB  <- here::here("_output", "_tables", "_sensitivity")
if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)

OLS_DATA <- file.path(DIR_DATA, "ols_data.rds")
if (!file.exists(OLS_DATA)) stop("ols_data.rds not found.")

# --- DEFINE CANDIDATES ---
# Only test the realistic contenders to keep output clean
FORMS <- list(
  "Log-Linear"   = "dist_enemy_log",
  "Inverse Log"  = "dist_enemy_inv_log",
  "Inverse Sqrt" = "dist_enemy_inv_sqrt"
)

# --- DEFINE PERIODS & VARS ---
PERIODS <- list(
  "Crimea (14-21)" = list(y = "milex_usd_pre",  gap = "nato_gap_2014", debt = "debt_gdp_2014_log"),
  "War (21-25)"    = list(y = "milex_usd_post", gap = "nato_gap_2021", debt = "debt_gdp_2021_log")
)


# 1. DATA PREP ===============================================================
df <- readRDS(OLS_DATA) %>%
  mutate(
    dist_enemy_log = log(dist_enemy),
    dist_enemy_inv_log = 1 / log(dist_enemy),
    dist_enemy_inv_sqrt = 1 / sqrt(dist_enemy)
  )

# 2. THE TOURNAMENT ENGINE ===================================================
message("--- Running Stability Tournament ---")

results_list <- list()
counter <- 0

# A. Loop over Periods
for (p_name in names(PERIODS)) {
  p_cfg <- PERIODS[[p_name]]

  # B. Loop over Jackknife (Drop 1 Country at a time)
  # We adds "Full Sample" as the first iteration
  countries_to_drop <- c("NONE", unique(df$iso3c))

  for (drop_iso in countries_to_drop) {

    # Create Data Subset
    if (drop_iso == "NONE") {
      data_sub <- df
    } else {
      data_sub <- df %>% filter(iso3c != drop_iso)
    }

    # C. Loop over Functional Forms
    iter_results <- map_dfr(names(FORMS), function(form_name) {
      dist_var <- FORMS[[form_name]]

      # Construct FULL Model Formula (Dist + Gap + Debt)
      f <- as.formula(glue("{p_cfg$y} ~ {dist_var} + {p_cfg$gap} + {p_cfg$debt}"))

      # Fit Model
      mod <- lm(f, data = data_sub)

      tibble(
        Period = p_name,
        Dropped = drop_iso,
        Form = form_name,
        BIC = BIC(mod)
      )
    })

    # Calculate Delta BIC for this specific iteration (Regret)
    # Who won this specific battle?
    min_bic <- min(iter_results$BIC)

    iter_results <- iter_results %>%
      mutate(
        Is_Winner = ifelse(BIC == min_bic, 1, 0),
        BIC_Regret = BIC - min_bic # How much worse than the winner?
      )

    counter <- counter + 1
    results_list[[counter]] <- iter_results
  }
}

full_results <- bind_rows(results_list)


# 3. AGGREGATE SCORES (THE DECISION MATRIX) ==================================
message("--- Calculating Final Scores ---")

# Score 1: Win Rate (How often is it the absolute best?)
# Score 2: Total Regret (Sum of Delta BIC). Lower is better.
# Score 3: Average Regret (Avg Delta BIC). < 2 means "indistinguishable from best".

final_scorecard <- full_results %>%
  group_by(Form) %>%
  summarise(
    Global_Win_Rate = mean(Is_Winner),
    Avg_BIC_Penalty = mean(BIC_Regret),
    Max_BIC_Penalty = max(BIC_Regret), # Worst case scenario
    .groups = "drop"
  ) %>%
  arrange(Avg_BIC_Penalty)

# Create detailed table split by period
period_scorecard <- full_results %>%
  group_by(Period, Form) %>%
  summarise(
    Win_Rate = mean(Is_Winner),
    Avg_BIC_Penalty = mean(BIC_Regret),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Period,
    values_from = c(Win_Rate, Avg_BIC_Penalty),
    names_glue = "{Period}_{.value}"
  )


# 4. OUTPUT TABLES ===========================================================

# Save the Global Scorecard
gt_global <- final_scorecard %>%
  gt() %>%
  tab_header(
    title = md("**Global Functional Form Stability**"),
    subtitle = "Aggregated across both periods and all jackknife samples"
  ) %>%
  fmt_percent(columns = Global_Win_Rate, decimals = 1) %>%
  fmt_number(columns = contains("BIC"), decimals = 2) %>%
  cols_label(
    Avg_BIC_Penalty = "Avg BIC Loss",
    Max_BIC_Penalty = "Worst Case Loss"
  ) %>%
  tab_style(
    style = cell_fill(color = "#e6f7ff"),
    locations = cells_body(rows = Avg_BIC_Penalty == min(Avg_BIC_Penalty))
  )

gtsave(gt_global, file.path(DIR_TAB, "stability_global_scorecard.html"))

# Print to Console
print(final_scorecard)
print(period_scorecard)

message("\n--- Script 25_functional_form_stability_test.R finished ---")
