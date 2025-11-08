# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       03_event_study.R
#   Author:       Frederik Bender Bøeck-Nielsen
#   Date:         2025-11-07
#   Description:  This script runs the event study (TWFE) model to
#                 visually and statistically test the parallel trends
#                 assumption.
#
# ---------------------------------------------------------------------------- #


# 0. CONFIGURATION & PARAMETERS ==============================================
message("--- Section 0: Loading Configuration ---")

DIR_DATA <- here::here("data", "_processed")
DIR_SCRIPTS <- here::here("scripts")
DIR_TAB <- here::here("_output", "_tables")
DIR_FIG <- here::here("_output", "_figures")

if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

FD_PANEL <- file.path(DIR_DATA, "fd_panel.rds")

OUTPUT_PLOT_ES <- file.path(DIR_FIG, "event_study_plot.png")
OUTPUT_TABLE_ES_IMG <- file.path(DIR_TAB, "table_event_study.png")
OUTPUT_TABLE_ES_RDS <- file.path(DIR_TAB, "table_event_study.rds")


# 1. ENVIRONMENT SETUP =======================================================
message("--- Section 1: Setting Up Environment ---")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, fixest, modelsummary, gt, broom, conflicted)

conflict_prefer("filter", "dplyr")

source(file.path(DIR_SCRIPTS, "00_functions.R"))

options(scipen = 999)


# 2. PREPARE DATA ============================================================
message("--- Section 2: Loading and Preparing Data ---")

fd_panel <- readRDS(FD_PANEL)

fd_panel_es <- fd_panel %>%
  filter(group %in% c("control", "treatment")) %>%
  mutate(
    treat_dummy = ifelse(group == "treatment", 1, 0),
    event_time = year - 2022
  )


# 3. RUN EVENT STUDY MODEL ===================================================
message("--- Section 3: Running Event Study Model (TWFE with i()) ---")

model_es <- feols(
  log_milex_gdp ~ i(event_time, treat_dummy, ref = -1) | iso3c + year,
  data = fd_panel_es,
  cluster = ~iso3c
)

summary(model_es)

message("--- Tidying model results for plot and table ---")

coef_labels <- c(
  "event_time::-8:treat_dummy" = "2014",
  "event_time::-7:treat_dummy" = "2015",
  "event_time::-6:treat_dummy" = "2016",
  "event_time::-5:treat_dummy" = "2017",
  "event_time::-4:treat_dummy" = "2018",
  "event_time::-3:treat_dummy" = "2019",
  "event_time::-2:treat_dummy" = "2020",
  "event_time::0:treat_dummy"  = "2022",
  "event_time::1:treat_dummy"  = "2023",
  "event_time::2:treat_dummy"  = "2024"
)

# 1. Extract model coefficients and stats using broom
tidy_results <- tidy(model_es, conf.int = TRUE)
glance_results <- glance(model_es)

# 2. Create a tibble from our coefficient map to join
labels_df <- enframe(coef_labels, name = "term", value = "label")

# 3. Prepare the data for the gt table
table_data <- tidy_results %>%
  left_join(labels_df, by = "term") %>%
  filter(!is.na(label)) %>%
  mutate(
    event_time_num = as.numeric(str_replace_all(term, "event_time::|:treat_dummy", "")),
    label = factor(label, levels = unname(coef_labels))
  ) %>%
  arrange(event_time_num)


# 4. GENERATE AND SAVE EVENT STUDY PLOT ======================================
message("--- Section 4: Generating and Saving Event Study Plot (Manual ggplot) ---")

# We use the 'table_data' object created in Section 3
plot_es <- ggplot(
  table_data,
  aes(
    x = event_time_num, y = estimate,
    ymin = conf.low, ymax = conf.high
  )
) +
  geom_ribbon(alpha = 0.2, fill = "blue") +
  geom_point(color = "blue") +
  geom_errorbar(width = 0.1, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey") +
  scale_x_continuous(breaks = table_data$event_time_num) +
  labs(
    title = "Event Study (TWFE): Effect on log(Military Expenditure)",
    x = "Years Relative to Treatment (2022)",
    y = "Coefficient (95% CI)"
  ) +
  theme_bachelor_project()

print(plot_es)
ggsave(OUTPUT_PLOT_ES, plot_es, width = 8, height = 5, bg = "white")
message(paste("Event study plot saved to:", OUTPUT_PLOT_ES))


# 5. GENERATE AND SAVE RESULTS TABLE =========================================
message("--- Section 5: Generating and Saving Results Table ---")

table_data_for_gt <- table_data %>%
  mutate(
    stars = case_when(
      p.value <= 0.001 ~ "***",
      p.value <= 0.01 ~ "**",
      p.value <= 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(label, estimate, std.error, conf.low, conf.high, p.value, stars)

table_es_gt <- gt(table_data_for_gt) %>%
  tab_header(
    title = "Event Study Model Results (TWFE)"
  ) %>%
  cols_label(
    label = "Year",
    estimate = "ATT",
    std.error = "Std. Error",
    p.value = "p-value",
    stars = "",
    conf.low = "95% CI"
  ) %>%
  fmt_number(
    columns = c(estimate, std.error),
    decimals = 3
  ) %>%
  fmt_number(
    columns = c(conf.low, conf.high),
    decimals = 3
  ) %>%
  cols_merge(
    columns = c(conf.low, conf.high),
    pattern = "[{1}, {2}]"
  ) %>%
  fmt_number(
    columns = p.value,
    decimals = 3,
    drop_trailing_zeros = TRUE
  ) %>%
  cols_merge(
    columns = c(p.value, stars),
    pattern = "{1}{2}"
  ) %>%
  cols_align(
    align = "left",
    columns = p.value
  ) %>%
  tab_footnote(
    footnote = "* p < 0.05, ** p < 0.01, *** p < 0.001",
    locations = cells_column_labels(columns = p.value)
  ) %>%
  tab_source_note(
    source_note = md(paste(
      "**Obs.:**", glance_results$nobs,
      "&nbsp;&nbsp;&nbsp;&nbsp; **Adj. R2:**", round(glance_results$adj.r.squared, 3),
      "&nbsp;&nbsp;&nbsp;&nbsp; **Within R2:**", round(r2(model_es, "wr2"), 3),
      "&nbsp;&nbsp;&nbsp;&nbsp; **RMSE:**", round(glance_results$sigma, 3)
    ))
  ) %>%
  theme_gt_bachelor_project()

print(table_es_gt)

print(table_es_gt)
gtsave(table_es_gt, file = OUTPUT_TABLE_ES_IMG, vwidth = 1000)
saveRDS(table_es_gt, file = OUTPUT_TABLE_ES_RDS)


# 6. SCRIPT COMPLETION =======================================================
message(paste(
  "\n--- Script 03_event_study.R finished ---",
  "\nAll output (table and plot) saved to:", here::here("_output")
))
