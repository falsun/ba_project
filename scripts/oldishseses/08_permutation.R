# ============================================================================ #
# 5. PERMUTATION TEST (FISHER RANDOMIZATION TEST)
# ============================================================================ #
message("--- Section 5: Running Permutation Test ---")

# MUST BE RUN RIGHT AFTER 05_EVENT_STUDY.R

# --- 5.1. Set Parameters ---

set.seed(1234) # For reproducible shuffling
B <- 5000 # Number of permutations (5000 is good)
VAR_TO_TEST <- "milex_cap" # The variable from your table

# --- 5.2. Get the *Actual* Test Statistic ---

# 1. Define the formula for this variable
formula_actual <- as.formula(glue(
  "{VAR_TO_TEST} ~ i(event_time, treat_dummy, ref = -1) | iso3c + year"
))

# 2. Run the actual model
model_actual <- feols(
  formula_actual,
  data = analysis_df,
  cluster = ~iso3c
)

# 3. Get the actual F-statistic (this is your "f_test_lo" logic)
post_treat_terms_actual <- c(
  "event_time::0:treat_dummy",
  "event_time::1:treat_dummy",
  "event_time::2:treat_dummy"
)

f_test_actual <- wald(model_actual, post_treat_terms_actual)
stat_actual <- f_test_actual$stat

print(paste("Actual F-statistic for", VAR_TO_TEST, ":", round(stat_actual, 3)))


# --- 5.3. Prepare for the Loop ---

# Vector to store the 5000 placebo F-stats
placebo_stats <- numeric(B)

# Get your 30 unique countries (iso3c)
all_countries <- unique(analysis_df$iso3c)

# Get the *real* treatment labels for these 30 countries
# This is more robust than just c(rep(1, 22), rep(0, 8))
# It ensures we're shuffling the real set of labels
labels_to_shuffle <- analysis_df %>%
  distinct(iso3c, treat_dummy) %>%
  pull(treat_dummy)


# --- 5.4. Run the Permutation Loop ---
message(paste("Starting", B, "permutations... This may take a few minutes."))
start_time <- Sys.time()

for (i in 1:B) {
  # 1. Shuffle the labels
  shuffled_labels <- sample(labels_to_shuffle)

  # 2. Create a "placebo key" mapping country to a *fake* label
  placebo_key <- data.frame(
    iso3c = all_countries,
    treat_dummy_placebo = shuffled_labels
  )

  # 3. Create the placebo dataset by joining the fake labels
  data_placebo <- analysis_df %>%
    left_join(placebo_key, by = "iso3c")

  # 4. Define the *placebo formula*
  # We use treat_dummy_placebo instead of treat_dummy
  formula_placebo <- as.formula(glue(
    "{VAR_TO_TEST} ~ i(event_time, treat_dummy_placebo, ref = -1) | iso3c + year"
  ))

  # 5. Run the placebo model
  model_placebo <- feols(
    formula_placebo,
    data = data_placebo,
    cluster = ~iso3c
  )

  # 6. Get the placebo F-statistic
  # We must rename the terms to match the "treat_dummy_placebo"
  post_treat_terms_placebo <- c(
    "event_time::0:treat_dummy_placebo",
    "event_time::1:treat_dummy_placebo",
    "event_time::2:treat_dummy_placebo"
  )

  f_test_placebo <- try(
    wald(model_placebo, post_treat_terms_placebo),
    silent = TRUE
  )

  # 7. Store the result
  if (inherits(f_test_placebo, "try-error")) {
    placebo_stats[i] <- NA
  } else {
    placebo_stats[i] <- f_test_placebo$stat
  }

  # Optional: Print progress
  if (i %% 500 == 0) {
    print(paste("Completed iteration", i, "of", B))
  }
}

end_time <- Sys.time()
print(paste("Total time for", B, "iterations:", end_time - start_time))


# --- 5.5. Calculate Final Permutation p-value ---

# Clean up any NAs (from models that failed to converge, etc.)
placebo_stats_clean <- na.omit(placebo_stats)
B_clean <- length(placebo_stats_clean)

# Calculate the p-value
# (Number of placebo stats >= actual stat) + 1 / (Total simulations + 1)
numerator <- sum(placebo_stats_clean >= stat_actual, na.rm = TRUE) + 1
denominator <- B_clean + 1
p_value_frt <- numerator / denominator

print(paste("Permutation p-value:", p_value_frt))
print(paste("Based on", B_clean, "successful permutations."))

# --- 5.6. Plot the Distribution ---

# Create a data frame for plotting
placebo_df <- data.frame(stats = placebo_stats_clean)

# Plot the histogram
plot_frt <- ggplot(placebo_df, aes(x = stats)) +
  geom_histogram(bins = 50, fill = "grey80", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = stat_actual), color = "#ce6a85", linewidth = 1.5) +
  labs(
    title = "Permutation Test (FRT) Distribution",
    subtitle = paste("Actual F-statistic =", round(stat_actual, 3), " |  Permutation p-value =", round(p_value_frt, 4)),
    x = "Placebo F-Statistics",
    y = "Count"
  ) +
  theme_bachelor_project() # Using your theme

print(plot_frt)
ggsave(
  file.path(DIR_FIG, glue("frt_plot_{VAR_TO_TEST}.png")),
  plot_frt,
  width = 8, height = 5, bg = "white"
)
