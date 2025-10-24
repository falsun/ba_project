# ---------------------------------------------------------------------------- #
#
#   Project:     NATO Defence Spending Bachelor's Thesis
#   Script:      00_functions.R
#   Author:      Frederik Bender Bøeck-Nielsen
#   Date:        2025-10-17 (Final Refactor)
#   Description: Contains all reusable plotting functions for the project.
#
# ---------------------------------------------------------------------------- #

# 1. ENVIRONMENT SETUP =======================================================
# Load required packages for the functions to work
pacman::p_load(tidyverse, ggrepel, patchwork, ggridges, hrbrthemes, scales)


# 2. CUSTOM THEME ============================================================

# --- 1. DEFINE THE PROJECT'S OFFICIAL COLOR PALETTE ---
project_colors <- list(
  # Qualitative Palette
  treatment = "#118ACB",
  control   = "#EDF67D",

  # Diverging Palette
  high = "#4F9D69",
  mid  = "#FFFFFF",
  low  = "#E54B4B"
)


# --- 2. CREATE THE CUSTOM PLOT THEME ---
theme_bachelor_project <- function(...) {
  hrbrthemes::theme_ipsum(
    base_family = "IBM Plex Sans",
    ...
  ) +
    theme(
      legend.position = "top",
      legend.justification = 'center',
      legend.title = element_blank(),
      plot.title = element_text(size = 18),
      plot.subtitle = element_text(size = 14),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}


# --- 3. CREATE CUSTOM COLOR & FILL SCALES ---
scale_color_project_qual <- function(...) {
  scale_color_manual(
    values = c("treatment" = project_colors$treatment, "control" = project_colors$control),
    limits = c("treatment", "control"),
    ...
  )
}

scale_fill_project_qual <- function(...) {
  scale_fill_manual(
    values = c("treatment" = project_colors$treatment, "control" = project_colors$control),
    limits = c("treatment", "control"),
    ...
  )
}

scale_color_project_div <- function(...) {
  scale_color_gradient2(
    low = project_colors$low,
    mid = project_colors$mid,
    high = project_colors$high,
    midpoint = 0,
    ...
  )
}

scale_fill_project_div <- function(...) {
  scale_fill_gradient2(
    low = project_colors$low,
    mid = project_colors$mid,
    high = project_colors$high,
    midpoint = 0,
    ...
  )
}


# 3. CORRELATION MATRIX PLOT =================================================
#' Creates and saves a correlation matrix plot using the project's custom theme.
#'
#' @param data A dataframe.
#' @param vars A character vector of variable names to include in the matrix.
#' @param title An optional string for the plot's title. NULL by default.
#' @param output_dir The directory where the plot will be saved.
#' @param file_name The file name for the saved plot.
#' @return The ggplot object of the correlation plot.
#'
create_corr_plot <- function(data, vars, title = NULL, output_dir, file_name) {

  corr_matrix <- data %>%
    select(all_of(vars)) %>%
    cor(use = "pairwise.complete.obs")

  # Create the plot using ggcorrplot
  corr_plot <- ggcorrplot(
    corr_matrix,
    method = "square",
    type = "lower",
    lab = TRUE,
    lab_size = 3.5,
    outline.col = "white"
  ) +
    scale_fill_project_div(name = "Correlation",
                           limits = c(-1, 1)) +
    theme_bachelor_project() +
    theme(panel.grid.major.y = element_blank(), legend.position = "right") +

    labs(
      x = NULL,
      y = NULL
    )

  if (!is.null(title)) {
    corr_plot <- corr_plot + labs(title = title)
  }

  ggsave(file.path(output_dir, file_name), corr_plot, width = 8, height = 6, bg = "white")

  return(corr_plot)
}


# 4. HISTOGRAM + BOX PLOT STACK ==============================================
#' Creates a stacked histogram and box plot using the project's custom theme.
#'
#' @param data A dataframe containing the data to plot.
#' @param var_name The name of the variable to plot (unquoted symbol).
#' @param var_label A string for the plot's axis label.
#' @param manual_breaks A numeric vector for the x-axis breaks.
#' @param manual_binwidth A single numeric value to override automatic binwidth.
#' @param output_dir The directory where the plot will be saved.
#' @param title An optional title for the plot.
#' @return The combined ggplot object.
#'
create_distribution_plot <- function(data, var_name, var_label, manual_breaks = NULL,
                                     manual_binwidth = NULL, output_dir, title = NULL) {

  # --- 4.1. SETUP ---
  var_name_enquo <- enquo(var_name)
  current_var_name <- quo_name(var_name_enquo)

  zero_formatter <- function(x) ifelse(x == 0, "0", x)
  x_scale_layer <- if (!is.null(manual_breaks)) {
    if (current_var_name %in% c("milex_gdp", "lib_dem")) {
      scale_x_continuous(breaks = manual_breaks, labels = zero_formatter)
    } else {
      scale_x_continuous(breaks = manual_breaks)
    }
  }
  if (!is.null(manual_breaks)) {
    lower_limit <- min(manual_breaks)
    upper_limit <- max(manual_breaks)
  } else {
    lower_limit <- 0
    if (startsWith(current_var_name, "log_")) { lower_limit <- NA }
    upper_limit <- NA
  }
  calculate_fd_bw <- function(x) { 2 * IQR(x, na.rm = TRUE) / (length(na.omit(x))^(1/3)) }
  bin_width <- if (!is.null(manual_binwidth)) {
    message(glue::glue(">> For '{current_var_name}', using manual binwidth: {manual_binwidth}"))
    manual_binwidth
  } else {
    bw_treatment <- calculate_fd_bw(data %>% filter(group == "treatment") %>%
                                      pull({{ var_name_enquo }}))
    bw_control   <- calculate_fd_bw(data %>% filter(group == "control") %>%
                                      pull({{ var_name_enquo }}))
    avg_bw <- mean(c(bw_treatment, bw_control), na.rm = TRUE)
    message(glue::glue(">> For '{current_var_name}', using unbiased FD binwidth: {round(avg_bw, 3)}"))
    avg_bw
  }
  outlier_data <- data %>%
    group_by(group) %>%
    mutate(
      iqr = IQR({{ var_name_enquo }}, na.rm = TRUE),
      upper_bound = quantile({{ var_name_enquo }}, 0.75, na.rm = TRUE) + 1.5 * iqr,
      lower_bound = quantile({{ var_name_enquo }}, 0.25, na.rm = TRUE) - 1.5 * iqr,
      is_outlier = {{ var_name_enquo }} > upper_bound | {{ var_name_enquo }} < lower_bound,
      median_val = median({{ var_name_enquo }}, na.rm = TRUE),
      deviation = abs({{ var_name_enquo }} - median_val)
    ) %>%
    filter(is_outlier) %>%
    group_by(group, iso3c) %>%
    slice_max(order_by = deviation, n = 1, with_ties = FALSE) %>%
    ungroup()

  # --- 4.2. CREATE FOUR INDIVIDUAL PLOTS SETUP ---
  data_treat <- data %>% filter(group == "treatment")
  treatment_hist <- ggplot(data_treat, aes(x = {{ var_name_enquo }}, fill = group)) +
    geom_histogram(binwidth = bin_width, color = "white", alpha = 1) +
    geom_hline(yintercept = 0, color = "#cccccc", linetype = "solid", linewidth = 0.25) +
    scale_fill_project_qual() +

    theme_bachelor_project() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          plot.margin = margin(b = 0, unit = "pt")) +
    labs(x = NULL, y = NULL)

  treatment_box <- ggplot(data_treat, aes(x = {{ var_name_enquo }}, fill = group)) +
    geom_boxplot(width = 0.5, alpha = 1) +
    geom_text_repel(data = filter(outlier_data, group == "treatment"),
                    aes(x = {{ var_name_enquo }}, y = 0, label = iso3c),
                    size = 3.5, max.overlaps = Inf, nudge_y = 0.25, direction = "x") +
    scale_fill_project_qual(guide = "none") +
    theme_bachelor_project() +
    theme(
      axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(t = 5, b = 5, unit = "pt")
    ) +
    labs(x = NULL, y = NULL)

  data_control <- data %>% filter(group == "control")
  control_hist <- ggplot(data_control, aes(x = {{ var_name_enquo }}, fill = group)) +
    geom_histogram(binwidth = bin_width, color = "white", alpha = 1) +
    geom_hline(yintercept = 0, color = "#cccccc", linetype = "solid", linewidth = 0.25) +
    scale_fill_project_qual() +
    theme_bachelor_project() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          plot.margin = margin(t = 5, b = 0, unit = "pt")) +
    labs(x = NULL, y = NULL)

  control_box <- ggplot(data_control, aes(x = {{ var_name_enquo }}, fill = group)) +
    geom_boxplot(width = 0.5, alpha = 1) +
    geom_text_repel(data = filter(outlier_data, group == "control"),
                    aes(x = {{ var_name_enquo }}, y = 0, label = iso3c),
                    size = 3.5, max.overlaps = Inf, nudge_y = 0.25, direction = "x") +
    scale_fill_project_qual(guide = "none") +
    theme_bachelor_project() +
    theme(
      axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(t = 5, unit = "pt")
    ) +
    labs(x = var_label, y = NULL)

  # --- 4.3. ASSEMBLE & SAVE ---
  final_plot <- treatment_hist / treatment_box / control_hist / control_box

  final_plot <- final_plot &
    coord_cartesian(xlim = c(lower_limit, upper_limit)) &
    x_scale_layer

  if (!is.null(title)) {
    final_plot <- final_plot + plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(family = "IBM Plex Sans", size = 18, face = "bold"))
    )
  }

  final_plot <- final_plot +
    plot_layout(heights = c(8, 1, 8, 1), guides = 'collect') &
    theme(
      legend.position = 'top'
    )

  file_name <- glue::glue("{current_var_name}_dist.png")
  ggsave(file.path(output_dir, file_name), final_plot, width = 7, height = 9, bg = "white")

  return(final_plot)
}


# 5. AGGREGATED TIME SERIES PLOT =============================================
#' Creates an aggregated time-series plot using the project's custom theme.
#'
#' @param data A dataframe containing the panel data.
#' @param var_name The name of the variable to plot (unquoted symbol).
#' @param var_label A string for the plot's axis label.
#' @param treatment_year The year the treatment occurs.
#' @param output_dir The directory where the plot will be saved.
#' @param title An optional title for the plot.
#' @return A ggplot object.
#'
create_aggregated_ts_plot <- function(data, var_name, var_label, treatment_year,
                                      output_dir, title = NULL) {

  var_name_enquo <- enquo(var_name)
  current_var_name <- quo_name(var_name_enquo)

  agg_data <- data %>%
    group_by(group, year) %>%
    summarise(mean_value = mean({{ var_name_enquo }}, na.rm = TRUE), .groups = "drop")

  ts_plot <- ggplot(agg_data, aes(x = year, y = mean_value, color = group, group = group)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5) +
    geom_vline(xintercept = treatment_year - 1, linetype = "dashed", color = "grey40") +
    scale_x_continuous(breaks = seq(2014, 2024, by = 2)) +
    scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.01)) +
    scale_color_project_qual(name = NULL) +
    theme_bachelor_project() +

    labs(
      x = NULL,
      y = NULL
    )

  if (!is.null(title)) {
    ts_plot <- ts_plot + labs(title = title)
  }

  file_name <- glue::glue("{current_var_name}_ts_aggregated.png")
  ggsave(file.path(output_dir, file_name), ts_plot, width = 8, height = 6, bg = "white")

  return(ts_plot)
}


# 6. INDIVIDUAL TIME SERIES PLOT =============================================
create_spaghetti_ts_plot <- function(data, var_name, var_label, treatment_year,
                                     output_dir, title = NULL) {

  var_name_enquo <- enquo(var_name)
  current_var_name <- quo_name(var_name_enquo)

  spaghetti_plot <- data %>%
    filter(!is.na(group)) %>%

    ggplot(aes(x = year, y = {{ var_name_enquo }}, color = group, group = iso3c)) +
    geom_line(alpha = 0.6) +
    geom_vline(xintercept = treatment_year - 1, linetype = "dashed", color = "grey40") +
    scale_x_continuous(breaks = seq(2014, 2024, by = 2)) +
    scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.01)) +
    scale_color_project_qual(name = NULL) +
    theme_bachelor_project() +
    labs(
      x = NULL,
      y = NULL
    )

  if (!is.null(title)) {
    spaghetti_plot <- spaghetti_plot + labs(title = title)
  }

  file_name <- glue::glue("{current_var_name}_ts_spaghetti.png")
  ggsave(file.path(output_dir, file_name), spaghetti_plot, width = 8, height = 6, bg = "white")

  return(spaghetti_plot)
}
