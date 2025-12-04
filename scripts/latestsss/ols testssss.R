# ---------------------------------------------------------------------------- #
#   Script:       99_ols_quick_test.R
#   Description:  Lightweight sandbox for testing model specifications.
#                 Prints summary table + Full Diagnostic Suite (Tests & Outliers).
# ---------------------------------------------------------------------------- #

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, broom, glue, lmtest, car)

# 1. LOAD DATA
# Set iso3c as row names so Cook's Distance reports Country Codes, not numbers
ols_data <- readRDS(here::here("data", "_processed", "ols_data.rds")) %>%
  column_to_rownames("iso3c")

model_denom_check <- lm(milex_gdp_post ~ dist_enemy_log + border_rus + nato_gap_2021 + debt_gdp_2021_log + gdp_local_post, data = ols_data)

summary(model_denom_check)

# Did Border states grow slower than Non-Border states?
t.test(gdp_local_post ~ border_rus, data = ols_data)

# 2. RUN MODELS
# The script will automatically find any object named "p..."
pre_1 <- lm(milex_gdp_pre ~ dist_enemy_log + border_rus + nato_gap_2014 + debt_gdp_2014_log, data = ols_data)
pre_2 <- lm(milex_gdp_pre ~ dist_enemy_log + border_rus + nato_gap_2014 + post_com, data = ols_data)
pre_3 <- lm(milex_gdp_pre ~ dist_enemy_log + border_rus + nato_gap_2014 + us_troops_2014_log, data = ols_data)
pre_4 <- lm(milex_gdp_pre ~ dist_enemy_log + border_rus + nato_gap_2014 + gdp_cap_2014_log, data = ols_data)
pre_5 <- lm(milex_gdp_pre ~ dist_enemy_log + border_rus + nato_gap_2014 + gdp_2014_log, data = ols_data)
pre_6 <- lm(milex_gdp_pre ~ dist_enemy_log + border_rus + nato_gap_2014 + gdp_local_post, data = ols_data)

post_1 <- lm(milex_gdp_post ~ dist_enemy_log + border_rus + nato_gap_2021 + debt_gdp_2021_log, data = ols_data)
post_2 <- lm(milex_gdp_post ~ dist_enemy_log + border_rus + nato_gap_2021 + post_com, data = ols_data)
post_3 <- lm(milex_gdp_post ~ dist_enemy_log + border_rus + nato_gap_2021 + us_troops_2021_log, data = ols_data)
post_4 <- lm(milex_gdp_post ~ dist_enemy_log + border_rus + nato_gap_2021 + gdp_cap_2021_log, data = ols_data)
post_5 <- lm(milex_gdp_post ~ dist_enemy_log + border_rus + nato_gap_2021 + gdp_2021_log, data = ols_data)
post_6 <- lm(milex_gdp_post ~ dist_enemy_log + border_rus + nato_gap_2021 + gdp_local_post, data = ols_data)


# 3. AUTOMATED SUMMARY FUNCTION ==============================================
generate_summary_table <- function() {

  model_names <- ls(pattern = "^p", envir = .GlobalEnv)
  models <- mget(model_names, envir = .GlobalEnv)

  map_dfr(model_names, function(name) {
    mod <- models[[name]]
    formula_str <- as.character(formula(mod))[3]

    # --- CALCULATE DIAGNOSTICS ---

    # 1. Shapiro-Wilk (Normality)
    sw_p <- tryCatch(shapiro.test(residuals(mod))$p.value, error = function(e) NA)

    # 2. Breusch-Pagan (Homoskedasticity)
    bp_p <- tryCatch(lmtest::bptest(mod)$p.value, error = function(e) NA)

    # 3. Ramsey RESET (Specification Error)
    reset_p <- tryCatch(lmtest::resettest(mod)$p.value, error = function(e) NA)

    # 4. Variance Inflation Factor (VIF)
    # car::vif returns a named vector. We catch errors for 1-var models.
    vif_vals <- tryCatch(car::vif(mod), error = function(e) NULL)

    # 5. Cook's Distance (Influential Observations)
    cooks <- cooks.distance(mod)
    threshold <- 4 / nobs(mod)
    outliers <- names(cooks)[cooks > threshold]
    outliers_str <- if(length(outliers) > 0) paste(outliers, collapse=", ") else ""


    # --- BUILD TABLE ---
    fit <- glance(mod)

    tidy(mod) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        Model = name,
        Specification = formula_str,

        # Sorting Helpers
        BIC_raw = BIC(mod),

        # Basic Stats
        Estimate = formatC(estimate, format = "f", digits = 3),
        P_Value  = formatC(p.value, format = "f", digits = 3),
        Adj_R2   = formatC(fit$adj.r.squared, format = "f", digits = 2),
        AIC      = formatC(AIC(mod), format = "f", digits = 1),
        BIC      = formatC(BIC(mod), format = "f", digits = 1),

        # Diagnostic Columns (Model Level)
        Shapiro_P = formatC(sw_p, format = "f", digits = 2),
        BP_P      = formatC(bp_p, format = "f", digits = 2),
        RESET_P   = formatC(reset_p, format = "f", digits = 2),

        # VIF (Term Level) - FIXED LOGIC
        # We look up the VIF value using the term name as the index
        VIF_val = if (!is.null(vif_vals)) vif_vals[term] else NA,
        VIF = ifelse(is.na(VIF_val), "NA", formatC(VIF_val, format = "f", digits = 3)),

        # Outliers (Same for all rows in model)
        Cooks_Outliers = outliers_str
      ) %>%
      select(Model, Term = term, Estimate, P_Value, Adj_R2, BIC,
             Shapiro_P, BP_P, RESET_P, VIF, Cooks_Outliers, BIC_raw)
  }) %>%
    arrange(BIC_raw) %>% # Sort by BIC
    select(-BIC_raw)     # Remove sorting column
}

# 4. PRINT RESULTS
results_table <- generate_summary_table()
print(results_table, n = 100)
