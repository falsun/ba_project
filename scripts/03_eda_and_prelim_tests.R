# ---------------------------------------------------------------------------- #
#
#   Project:      NATO Defence Spending Bachelor's Thesis
#   Script:       03_eda_and_prelim_tests.R
#   Author:       [Your Name]
#   Date:         [Current Date]
#   Description:  This script conducts exploratory data analysis (EDA). It
#                 generates a descriptive summary statistics table and creates
#                 the parallel trends plot to visually inspect the key
#                 assumption of the DiD model.
#
# ---------------------------------------------------------------------------- #


# 1. SETUP -------------------------------------------------------------------

# Load necessary packages
pacman::p_load(
  tidyverse, # For data manipulation and ggplot2
  gtsummary, # For beautiful summary tables
  gt,        # For saving gtsummary tables
  svglite   # For saving plots in SVG format
)


# 2. LOAD FINAL ANALYSIS DATA ------------------------------------------------

# Load the master dataset created by the previous script.
analysis_df <- readRDS("data/processed/final_analysis_data.rds")


# 3. CREATE DESCRIPTIVE SUMMARY TABLE ("Table 1") ----------------------------

# We'll create a publication-ready summary table using the gtsummary package.
# This table now includes our new time-varying control variables.

summary_table <- analysis_df %>%
  # **THE FIX**: Add the new control variables to the select() command
  select(
    dist_sqroot,
    milex_as_gdp_share,
    milex_as_gov_share,
    milex_constant_2023_usd,
    log_gdp_per_cap, # <-- ADDED
    gdp_growth,      # <-- ADDED
    treatment
  ) %>%
  # Create the summary table, stratified by the 'treatment' variable
  tbl_summary(
    by = treatment,
    # Add descriptive labels for all variables
    label = list(
      milex_as_gdp_share ~ "Military Spending (% of GDP)",
      milex_as_gov_share ~ "Military Spending (% of Govt. Budget)",
      milex_constant_2023_usd ~ "Military Spending (Constant 2023 US$, millions)",
      dist_sqroot ~ "Distance to Russia (sqrt km/100)",
      log_gdp_per_cap ~ "Log(GDP per Capita)", # <-- ADDED
      gdp_growth ~ "GDP Growth (Annual %)"     # <-- ADDED
    ),
    # Specify the statistics to show for continuous variables
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  ) %>%
  modify_spanning_header(
    stat_1 ~ "**Control Group** (Farther from Russia)",
    stat_2 ~ "**Treatment Group** (Closer to Russia)"
  ) %>%
  modify_caption("### Table 1: Descriptive Statistics by Treatment Group") %>%
  modify_footnote(
    all_stat_cols() ~ "Mean (Standard Deviation) shown for continuous variables."
  )

# Print the table to the RStudio Viewer
summary_table

# --- Save the table in multiple professional formats ---
gtsave(as_gt(summary_table), filename = "output/tables/table_1_descriptive_stats.rtf")
gtsave(as_gt(summary_table), filename = "output/tables/table_1_descriptive_stats.tex")
gtsave(as_gt(summary_table), filename = "output/tables/table_1_descriptive_stats.png")


# --- 4. CHECK FOR OUTLIERS IN THE DEPENDENT VARIABLE ---

outlier_boxplot <- ggplot(analysis_df, aes(x = factor(treatment), y = milex_as_gdp_share)) +
  geom_boxplot(aes(fill = factor(treatment)), alpha = 0.7) +
  scale_fill_manual(
    name = "Groups",
    values = c("0" = "#0072B2", "1" = "#D55E00"),
    labels = c("Control", "Treatment")
  ) +
  scale_x_discrete(labels = c("Control Group", "Treatment Group")) +
  labs(
    title = "Distribution of Military Spending (% of GDP)",
    subtitle = "Boxplot to identify potential outliers",
    x = "",
    y = "Military Spending (% of GDP)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Print the plot
outlier_boxplot

# Save the plot
ggsave("output/figures/outlier_boxplot.png", plot = outlier_boxplot, width = 8, height = 6)


# --- INVESTIGATE THE OUTLIERS ---

# First, we need to calculate the outlier thresholds (the top of the "whiskers")
outlier_thresholds <- analysis_df %>%
  group_by(treatment) %>%
  summarise(
    q3 = quantile(milex_as_gdp_share, 0.75, na.rm = TRUE),
    iqr = IQR(milex_as_gdp_share, na.rm = TRUE),
    upper_whisker = q3 + 1.5 * iqr
  )

print("Outlier Thresholds:")
print(outlier_thresholds)

# Now, filter the main dataframe to find the rows that are above these thresholds
outlier_data <- analysis_df %>%
  left_join(outlier_thresholds, by = "treatment") %>%
  filter(milex_as_gdp_share > upper_whisker) %>%
  select(country, year, milex_as_gdp_share, treatment) %>%
  arrange(treatment, desc(milex_as_gdp_share))

print("Potential Outlier Observations:")
print(outlier_data)


# 5. CREATE PARALLEL TRENDS PLOT FOR SHARE OF GDP -----------------------------

# First, calculate the annual average spending for each group
avg_spending_trends_gdp <- analysis_df %>%
  group_by(treatment, year) %>%
  summarise(
    avg_milex_share = mean(milex_as_gdp_share, na.rm = TRUE)
  ) %>%
  ungroup()

# Now, create the plot using ggplot2
parallel_trends_plot_gdp <- ggplot(avg_spending_trends_gdp,
                               aes(
                                 x = year,
                                 y = avg_milex_share,
                                 color = factor(treatment)
                               )) +
  geom_line(linewidth = 1) +
  geom_point(size = 0) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "black", linewidth = 1) +
  scale_x_continuous(
    breaks = seq(2004, 2024, 2),
    minor_breaks = seq(2004, 2024, 1)
  ) +
  scale_y_continuous(
    breaks = seq(1.2, 2.6, 0.2),
    minor_breaks = seq(1.2, 2.6, 0.1)
  ) +
  coord_fixed(
    ratio = 10,
    xlim = c(2003.5, 2024.5),
    ylim = c(1.15, 2.65),
    expand = FALSE
  ) +
  scale_color_manual(
    name = "",
    values = c("0" = "#0072B2", "1" = "#D55E00"),
    labels = c("Control", "Treatment")
  ) +
  labs(
    title = "Parallel trends test 1: Share of GDP",
    subtitle = "Observed means",
    x = "Year",
    y = "Defence Expenditure (% of GDP)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.key.width = unit(2, "cm"),
    panel.grid.major = element_line(linewidth = 0.5, color = "grey85"),
    panel.grid.minor = element_line(linewidth = 0.5, color = "grey90")
  )

# Print the plot to the RStudio plot viewer
parallel_trends_plot_gdp

# --- Save the plot in multiple professional formats ---
ggsave("output/figures/parallel_trends_plot_gdp.pdf", plot = parallel_trends_plot_gdp, width = 11, height = 7, device = cairo_pdf)
ggsave("output/figures/parallel_trends_plot_gdp.svg", plot = parallel_trends_plot_gdp, width = 11, height = 7)
ggsave("output/figures/parallel_trends_plot_gdp.png", plot = parallel_trends_plot_gdp, width = 11, height = 7, dpi = 300)


# 6. CREATE PARALLEL TRENDS PLOT FOR SHARE OF GOVT. SPENDING -------------------

# First, calculate the annual average spending for the new variable
avg_spending_trends_gov <- analysis_df %>%
  group_by(treatment, year) %>%
  summarise(
    avg_milex_share = mean(milex_as_gov_share, na.rm = TRUE)
  ) %>%
  ungroup()

# Now, create the plot using ggplot2
parallel_trends_plot_gov <- ggplot(avg_spending_trends_gov,
                                   aes(
                                     x = year,
                                     y = avg_milex_share,
                                     color = factor(treatment)
                                   )) +
  geom_line(linewidth = 1) +
  geom_point(size = 0) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "black", linewidth = 1) +
  scale_x_continuous(
    breaks = seq(2004, 2024, 2),
    minor_breaks = seq(2004, 2024, 1)
  ) +
  scale_y_continuous(
    breaks = seq(2.6, 5.8, 0.4),
    minor_breaks = seq(2.6, 5.8, 0.2)
  ) +
  coord_fixed(
    ratio = 5,
    xlim = c(2003.5, 2024.5),
    ylim = c(2.5, 5.9),
    expand = FALSE
  ) +
  scale_color_manual(
    name = "",
    values = c("0" = "#0072B2", "1" = "#D55E00"),
    labels = c("Control", "Treatment")
  ) +
  # Update titles and labels
  labs(
    title = "Parallel trends test 2: Share of Govt. Spending",
    subtitle = "Observed means",
    x = "Year",
    y = "Defence Expenditure (% of Govt. Spending)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.key.width = unit(2, "cm"),
    panel.grid.major = element_line(linewidth = 0.5, color = "grey85"),
    panel.grid.minor = element_line(linewidth = 0.5, color = "grey90")
  )

# Print the plot
parallel_trends_plot_gov

# --- Save the plot with a new, unique name ---
ggsave("output/figures/parallel_trends_plot_gov.pdf", plot = parallel_trends_plot_gov, width = 11, height = 7, device = cairo_pdf)
ggsave("output/figures/parallel_trends_plot_gov.svg", plot = parallel_trends_plot_gov, width = 11, height = 7)
ggsave("output/figures/parallel_trends_plot_gov.png", plot = parallel_trends_plot_gov, width = 11, height = 7, dpi = 300)


# 7. CREATE PARALLEL TRENDS PLOT FOR CONSTANT (2023) USD ---------------------

# First, calculate the annual average spending for the new variable
avg_spending_trends_usd <- analysis_df %>%
  group_by(treatment, year) %>%
  summarise(
    avg_milex_share = mean(milex_constant_2023_usd, na.rm = TRUE)
  ) %>%
  ungroup()

# Now, create the plot using ggplot2
parallel_trends_plot_usd <- ggplot(avg_spending_trends_usd,
                                   aes(
                                     x = year,
                                     y = avg_milex_share,
                                     color = factor(treatment)
                                   )) +
  geom_line(linewidth = 1) +
  geom_point(size = 0) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "black", linewidth = 1) +
  scale_x_continuous(
    breaks = seq(2004, 2024, 2),
    minor_breaks = seq(2004, 2024, 1)
  ) +
  # Adjust the y-axis for the new variable's scale (large numbers)
  scale_y_continuous(
    breaks = seq(6000, 24000, 2000),
    minor_breaks = seq(6000, 24000, 1000),
    # Use comma format for thousands
    labels = scales::label_comma()
  ) +
  # Adjust ratio and limits for the new scale
  coord_fixed(
    ratio = 0.0010, # Ratio is very different due to the large y-axis scale
    xlim = c(2003.5, 2024.5),
    ylim = c(5500, 24500),
    expand = FALSE
  ) +
  scale_color_manual(
    name = "",
    values = c("0" = "#0072B2", "1" = "#D55E00"),
    labels = c("Control", "Treatment")
  ) +
  # Update titles and labels
  labs(
    title = "Parallel trends test 3: Constant (2023) US$",
    subtitle = "Observed means",
    x = "Year",
    y = "Defence Expenditure (Millions, Constant 2023 US$)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.key.width = unit(2, "cm"),
    panel.grid.major = element_line(linewidth = 0.5, color = "grey85"),
    panel.grid.minor = element_line(linewidth = 0.5, color = "grey90")
  )

# Print the plot
parallel_trends_plot_usd

# --- Save the plot with a new, unique name ---
ggsave("output/figures/parallel_trends_plot_usd.pdf", plot = parallel_trends_plot_usd, width = 11, height = 7, device = cairo_pdf)
ggsave("output/figures/parallel_trends_plot_usd.svg", plot = parallel_trends_plot_usd, width = 11, height = 7)
ggsave("output/figures/parallel_trends_plot_usd.png", plot = parallel_trends_plot_usd, width = 11, height = 7, dpi = 300)


# 8. CREATE INDEXED PARALLEL TRENDS PLOTS FOR ALL DVs ---------------------
#   *UPDATED*: This version forces all three plots to share the exact same
#   y-axis scale for perfect visual comparability.

# --- a. Prepare indexed data for all variables ---

# 1. Share of GDP
indexed_data_gdp <- analysis_df %>%
  group_by(treatment, year) %>%
  summarise(avg_value = mean(milex_as_gdp_share, na.rm = TRUE), .groups = 'drop') %>%
  group_by(treatment) %>%
  mutate(indexed_value = (avg_value / first(avg_value)) * 100, variable = "GDP Share") %>%
  ungroup()

# 2. Share of Govt. Spending
indexed_data_gov <- analysis_df %>%
  group_by(treatment, year) %>%
  summarise(avg_value = mean(milex_as_gov_share, na.rm = TRUE), .groups = 'drop') %>%
  group_by(treatment) %>%
  mutate(indexed_value = (avg_value / first(avg_value)) * 100, variable = "Govt. Share") %>%
  ungroup()

# 3. Constant (2023) US$
indexed_data_usd <- analysis_df %>%
  group_by(treatment, year) %>%
  summarise(avg_value = mean(milex_constant_2023_usd, na.rm = TRUE), .groups = 'drop') %>%
  group_by(treatment) %>%
  mutate(indexed_value = (avg_value / first(avg_value)) * 100, variable = "Constant USD") %>%
  ungroup()

# --- b. Calculate a single, global y-axis range for all plots ---

# Combine all data to find the absolute min and max
all_indexed_data <- bind_rows(indexed_data_gdp, indexed_data_gov, indexed_data_usd)
# Round to the nearest 10 for clean limits
global_y_min <- floor(min(all_indexed_data$indexed_value) / 10) * 10
global_y_max <- ceiling(max(all_indexed_data$indexed_value) / 10) * 10
global_y_limits <- c(global_y_min, global_y_max)

# Calculate the new ratio based on this global range
# x_range = 2024.5 - 2003.5 = 21
# y_range = global_y_max - global_y_min
global_ratio <- 21 / (global_y_max - global_y_min)


# --- c. Create a reusable plotting function with manual ylim ---

create_indexed_plot <- function(data, title, y_label) {
  ggplot(data, aes(x = year, y = indexed_value, color = factor(treatment))) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 100, linetype = "dotted", color = "grey50") +
    geom_vline(xintercept = 2021.5, linetype = "dashed", color = "black", linewidth = 1) +
    scale_x_continuous(
      breaks = seq(2004, 2024, 2),
      minor_breaks = seq(2004, 2024, 1)
    ) +
    # This ensures the grid lines match across plots
    scale_y_continuous(breaks = seq(global_y_min, global_y_max, 20)) +
    # **THE FIX**: Use the global limits and ratio for all plots
    coord_fixed(
      ratio = global_ratio,
      xlim = c(2003.5, 2024.5),
      ylim = global_y_limits,
      expand = FALSE
    ) +
    scale_color_manual(
      name = "",
      values = c("0" = "#0072B2", "1" = "#D55E00"),
      labels = c("Control", "Treatment")
    ) +
    labs(
      title = title,
      subtitle = "Observed means, indexed to 2004 = 100",
      x = "Year",
      y = y_label
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.key.width = unit(2, "cm"),
      panel.grid.major = element_line(linewidth = 0.5, color = "grey85"),
      panel.grid.minor = element_line(linewidth = 0.5, color = "grey90")
    )
}

# --- d. Generate and save all plots ---

parallel_trends_plot_gdp <- create_indexed_plot(indexed_data_gdp, "Parallel Trends: Share of GDP", "Index (2004 = 100)")
parallel_trends_plot_gov <- create_indexed_plot(indexed_data_gov, "Parallel Trends: Share of Govt. Spending", "Index (2004 = 100)")
parallel_trends_plot_usd <- create_indexed_plot(indexed_data_usd, "Parallel Trends: Constant (2023) US$", "Index (2004 = 100)")

print(parallel_trends_plot_gdp)
print(parallel_trends_plot_gov)
print(parallel_trends_plot_usd)

ggsave("output/figures/parallel_trends_plot_gdp_indexed.pdf", plot = parallel_trends_plot_gdp, width = 11, height = 7, device = cairo_pdf)
ggsave("output/figures/parallel_trends_plot_gov_indexed.pdf", plot = parallel_trends_plot_gov, width = 11, height = 7, device = cairo_pdf)
ggsave("output/figures/parallel_trends_plot_usd_indexed.pdf", plot = parallel_trends_plot_usd, width = 11, height = 7, device = cairo_pdf)


# 9. SCRIPT COMPLETION -------------------------------------------------------
print("Script 03 finished: Summary table and parallel trends plots have been generated and saved to the 'output' folder.")