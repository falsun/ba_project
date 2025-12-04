# ---------------------------------------------------------------------------- #
#   Script:       99_ols_quick_test.R
#   Description:  Lightweight sandbox for testing model specifications.
#                 Prints summary table + Full Diagnostic Suite (Tests & Outliers).
# ---------------------------------------------------------------------------- #

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, broom, glue, lmtest, car)

# 1. LOAD DATA & CREATE TRANSFORMATIONS
# Set iso3c as row names so Cook's Distance reports Country Codes
# We create the functional forms here so they are ready for the models below
ols_data <- readRDS(here::here("data", "_processed", "ols_data.rds")) %>%
  column_to_rownames("iso3c") %>%
  mutate(
    # --- NATO Gap Transformations (NEW) ---
    # Note: We add +0.01 to handle countries with 0 gap (log(0) is -Inf)

    # 1. Log Forms (Diminishing Pressure?)
    nato_gap_2014_log = log(nato_gap_2014 + 0.01),
    nato_gap_2021_log = log(nato_gap_2021 + 0.01),

    # 2. Squared Forms (Accelerating Pressure?)
    nato_gap_2014_sq  = nato_gap_2014^2,
    nato_gap_2021_sq  = nato_gap_2021^2,

    # 3. Inverse Forms (Explosive Pressure near 2%?)
    nato_gap_2014_inv = 1 / (nato_gap_2014 + 0.01),
    nato_gap_2021_inv = 1 / (nato_gap_2021 + 0.01),

    # 4. Inverse Sqrt
    nato_gap_2014_inv_sqrt = 1 / sqrt(nato_gap_2014 + 0.01),
    nato_gap_2021_inv_sqrt = 1 / sqrt(nato_gap_2021 + 0.01),

    # Inverse Log
    nato_gap_2014_inv_log = 1 / log(nato_gap_2014 + 0.01),
    nato_gap_2021_inv_log = 1 / log(nato_gap_2021 + 0.01),

    nato_gap_2014_sqrt = sqrt(nato_gap_2014 + 0.01),
    nato_gap_2021_sqrt = sqrt(nato_gap_2021 + 0.01),

    milex_gdp_nato_2014_log = log(milex_gdp_nato_2014)
  )

# 2. RUN MODELS
# The script will automatically find any object named "model_..."

# --- PRE-INVASION MODELS (2014-2021) ---

#m_pre_1 <- lm(mil_gdp_nato_pre_dif ~ dist_min_log, data = ols_data)
#m_pre_2 <- lm(mil_gdp_nato_pre_dif ~ milex_gdp_nato_2014, data = ols_data)
#m_pre_3 <- lm(mil_gdp_nato_pre_dif ~ debt_gdp_2014, data = ols_data)
#m_pre_4 <- lm(mil_gdp_nato_pre_dif ~ gdp_cap_2014, data = ols_data)
#m_pre_5 <- lm(mil_gdp_nato_pre_dif ~ gdp_cap_2014_log, data = ols_data)

#m_pre_6 <- lm(mil_gdp_nato_pre_dif ~ dist_min_log * milex_gdp_nato_2014, data = ols_data)
#m_pre_7 <- lm(mil_gdp_nato_pre_dif ~ dist_min_log * I(milex_gdp_nato_2014^2), data = ols_data)

m_pre_3 <- lm(mil_gdp_nato_pre_dif ~ dist_min_log * milex_gdp_nato_2014, data = ols_data)

# --- POST-INVASION MODELS (2021-2024) ---


# 3. AUTOMATED SUMMARY FUNCTION ==============================================
generate_summary_table <- function() {

  model_names <- ls(pattern = "^m_", envir = .GlobalEnv)
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
        Estimate = formatC(estimate, format = "f", digits = 4),
        P_Value  = formatC(p.value, format = "f", digits = 4),
        Adj_R2   = formatC(fit$adj.r.squared, format = "f", digits = 3),
        AIC      = formatC(AIC(mod), format = "f", digits = 1),
        BIC      = formatC(BIC(mod), format = "f", digits = 1),

        # Diagnostic Columns (Model Level)
        Shapiro_P = formatC(sw_p, format = "f", digits = 3),
        BP_P      = formatC(bp_p, format = "f", digits = 3),
        RESET_P   = formatC(reset_p, format = "f", digits = 3),

        # VIF (Term Level) - FIXED LOGIC
        # We look up the VIF value using the term name as the index
        VIF_val = if (!is.null(vif_vals)) vif_vals[term] else NA,
        VIF = ifelse(is.na(VIF_val), "NA", formatC(VIF_val, format = "f", digits = 2)),

        # Outliers (Same for all rows in model)
        Cooks_Outliers = outliers_str
      ) %>%
      select(Model, Specification, Term = term, Estimate, P_Value, Adj_R2, AIC, BIC,
             Shapiro_P, BP_P, RESET_P, BIC_raw)
  }) %>%
    arrange(BIC_raw) %>% # Sort by BIC
    select(-BIC_raw)     # Remove sorting column
}

# 4. PRINT RESULTS
results_table <- generate_summary_table()
print(results_table, n = 100)
