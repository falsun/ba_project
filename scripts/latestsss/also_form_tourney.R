# ---------------------------------------------------------------------------- #
#
#   Script:       25_functional_form_sensitivity.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Description:  Performs "Jackknife" (Leave-One-Out) Model Selection.
#                 Tests if the "Best Fit" functional form changes when
#                 individual countries are excluded.
#
# ---------------------------------------------------------------------------- #

# 0. CONFIGURATION ===========================================================
message("--- Section 0: Loading Configuration ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, broom, glue, here, gt)

DIR_DATA_PROC <- here::here("data", "_processed")
DIR_TAB       <- here::here("_output", "_tables", "_sensitivity")

if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)

OLS_DATA <- file.path(DIR_DATA_PROC, "ols_data.rds")
if (!file.exists(OLS_DATA)) stop("ols_data.rds not found.")

# --- DEFINE CANDIDATE FORMS ---
# Only test the top contenders to save processing time and clarity
FORMS_TO_TEST <- list(
  "Log-Linear"   = "~ dist_enemy_log",
  "Inverse"      = "~ dist_enemy_inv",
  "Inv Sqrt"     = "~ dist_enemy_inv_sqrt",
  "Inv Log"      = "~ dist_enemy_inv_log",
  # We use a fixed threshold for the spline here to make it comparable across iterations
  "Spline (800)" = "~ pmin(dist_enemy, 800) + pmax(dist_enemy - 800, 0)"
)

# --- DEFINE SCENARIOS ---
SCENARIOS <- list(
  "gdp_war" = list(y = "milex_usd_post", title = "War Shock (GDP)"),
  "gdp_crim" = list(y = "milex_usd_pre", title = "Crimea Legacy (GDP)")
)


# 1. DATA PREP ===============================================================
df <- readRDS(OLS_DATA) %>%
  mutate(
    dist_enemy_inv = 1 / dist_enemy,
    dist_enemy_inv_sqrt = 1 / sqrt(dist_enemy),
    dist_enemy_inv_log = 1 / log(dist_enemy),
    dist_enemy_log = log(dist_enemy)
  )

# 2. JACKKNIFE FUNCTION ======================================================
run_jackknife_selection <- function(data, y_col, forms_list) {

  # Get list of all countries
  countries <- unique(data$iso3c)
  results <- list()

  # Loop: Drop one country at a time
  for (dropped_iso in countries) {

    # Create N-1 Dataset
    df_sub <- data %>% filter(iso3c != dropped_iso)

    # Fit all candidate forms on this subset
    iteration_res <- map_dfr(names(forms_list), function(form_name) {
      f <- as.formula(paste(y_col, forms_list[[form_name]]))
      mod <- lm(f, data = df_sub)

      tibble(
        Dropped_Country = dropped_iso,
        Form = form_name,
        BIC = BIC(mod)
      )
    })

    # Find the Winner for this iteration
    winner <- iteration_res %>% arrange(BIC) %>% slice(1)
    results[[dropped_iso]] <- winner
  }

  bind_rows(results)
}


# 3. EXECUTE =================================================================
message("--- Section 3: Running Jackknife Tests ---")

all_summaries <- list()

for (s_name in names(SCENARIOS)) {
  scen <- SCENARIOS[[s_name]]
  message(glue("Testing Stability for: {scen$title}"))

  # Run Jackknife
  jk_results <- run_jackknife_selection(df, scen$y, FORMS_TO_TEST)

  # Summarize: How often did each form win?
  summary_table <- jk_results %>%
    count(Form, name = "Wins") %>%
    mutate(
      Total_Runs = nrow(jk_results),
      Win_Rate = Wins / Total_Runs,
      Period = scen$title
    ) %>%
    arrange(desc(Wins))

  all_summaries[[s_name]] <- summary_table

  # --- Detailed Heatmap Data ---
  # We save which country caused a switch
  # Find the "Global Winner" (Full Sample)
  global_mod_res <- map_dfr(names(FORMS_TO_TEST), function(fn) {
    mod <- lm(as.formula(paste(scen$y, FORMS_TO_TEST[[fn]])), data = df)
    tibble(Form = fn, BIC = BIC(mod))
  }) %>% arrange(BIC) %>% slice(1)

  global_winner <- global_mod_res$Form

  # Identify "Flip" countries (Where winner != Global Winner)
  flippers <- jk_results %>%
    filter(Form != global_winner) %>%
    rename(New_Winner = Form) %>%
    mutate(Original_Winner = global_winner)

  if(nrow(flippers) > 0) {
    message("  ! Warning: The Best Fit changed when dropping these countries:")
    print(flippers)
  } else {
    message("  * Stable: The same functional form won in all 22 iterations.")
  }
}

final_summary <- bind_rows(all_summaries)

# 4. SAVE SUMMARY TABLE ======================================================
gt_table <- final_summary %>%
  group_by(Period) %>%
  gt() %>%
  tab_header(
    title = md("**Functional Form Stability Test**"),
    subtitle = "Jackknife (Leave-One-Out) Analysis: How often is a form the best fit?"
  ) %>%
  fmt_percent(columns = Win_Rate, decimals = 0) %>%
  cols_label(Form = "Functional Form", Wins = "Times Won (N=22)") %>%
  tab_style(
    style = cell_fill(color = "#e6f7ff"),
    locations = cells_body(rows = Win_Rate > 0.5)
  )

gtsave(gt_table, file.path(DIR_TAB, "table_form_stability.html"))

message("\n--- Script 25_functional_form_sensitivity.R finished ---")
