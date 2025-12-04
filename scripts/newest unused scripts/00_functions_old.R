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

## --- 2.1. DEFINE THE PROJECT'S OFFICIAL COLOR PALETTE -----------------------
project_colors <- list(
  # Qualitative Palette
  treatment = "#0077b6",
  control   = "#f48c06",
  # Diverging Palette
  high      = "#4F9D69",
  mid       = "#FFFFFF",
  low       = "#E54B4B"
)


## --- 2.2 CREATE THE CUSTOM PLOT THEME ----
theme_bachelor_project <- function(...) {
  hrbrthemes::theme_ipsum(
    base_family = "IBM Plex Serif",
    ...
  ) +
    theme(
      legend.position      = c(0.02, 0.98),
      legend.justification = c("left", "top"),
      legend.title         = element_blank(),
      plot.title           = element_text(size = 11, color = "black"),
      plot.subtitle        = element_text(size = 9, color = "black"),
      axis.title.x         = element_text(size = 9, color = "black"),
      axis.title.y         = element_text(size = 9, color = "black"),
      axis.text.x          = element_text(size = 11, color = "black"),
      axis.text.y          = element_text(size = 11, color = "black"),
      legend.text          = element_text(size = 11, color = "black"),
      panel.grid.minor     = element_blank(),
      panel.grid.major     = element_blank(),
      plot.margin          = margin(t = 0, r = 0, b = 0, l = 0)
    )
}

#' Custom X-axis for Bachelor Project (Danish formatting)
#' Defaults to comma decimal, dot thousands, and allows extra arguments like limits
scale_x_dk <- function(...) {
  scale_x_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
    ...
  )
}

#' Custom Y-axis (Danish formatting)
#' Allows optional control over accuracy (decimals) per plot
scale_y_dk <- function(accuracy = NULL, ...) {
  scale_y_continuous(
    # Pass the accuracy argument here (NULL lets R decide automatically)
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",", accuracy = accuracy),
    ...
  )
}


## --- 3. CREATE THE CUSTOM GT TABLE THEME ---
theme_gt_bachelor_project <- function(data) {
  numeric_cols <- data$`_data` %>%
    select(where(is.numeric)) %>%
    names()
  p_val_col <- intersect(numeric_cols, "p.value")
  other_numeric_cols <- setdiff(numeric_cols, p_val_col)

  data %>%
    fmt(
      columns = all_of(p_val_col),
      fns = function(x) {
        gtsummary::style_pvalue(x, digits = 3, decimal.mark = ",")
      }
    ) %>%
    fmt_number(
      columns = all_of(other_numeric_cols),
      decimals = 3,
      dec_mark = ",",
      sep_mark = "."
    ) %>%
    opt_table_font(
      font = "IBM Plex Serif",
      color = "black",
      add = FALSE
    ) %>%
    tab_options(
      table.font.size = "11pt",
      footnotes.marks = "letters"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(columns = everything())
    )
}


# --- 3. CREATE CUSTOM COLOR & FILL SCALES ---
project_qual_colors_map <- c(
  "treatment"         = project_colors$treatment,
  "treated"           = project_colors$treatment,
  "Treated"           = project_colors$treatment,
  "behandlet"         = project_colors$treatment,
  "control"           = project_colors$control,
  "synthetic control" = project_colors$control,
  "Synthetic Control" = project_colors$control,
  "syntetisk kontrol" = project_colors$control,
  "donorpulje"        = project_colors$control
)

# --- 3.1 Qualitative Scales ---
scale_color_project_qual <- function(...) {
  scale_color_manual(
    values = project_qual_colors_map,
    ...
  )
}

scale_fill_project_qual <- function(...) {
  scale_fill_manual(
    values = project_qual_colors_map,
    ...
  )
}

scale_color_project_div <- function(midpoint = 0, ...) {
  scale_color_gradient2(
    low      = project_colors$low,
    mid      = project_colors$mid,
    high     = project_colors$high,
    midpoint = midpoint,
    ...
  )
}

scale_fill_project_div <- function(midpoint = 0, ...) {
  scale_fill_gradient2(
    low      = project_colors$low,
    mid      = project_colors$mid,
    high     = project_colors$high,
    midpoint = midpoint,
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
    scale_fill_project_div(
      name = "Correlation",
      limits = c(-1, 1)
    ) +
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
    lower_limit <- NA
    upper_limit <- NA
  }
  calculate_fd_bw <- function(x) {
    2 * IQR(x, na.rm = TRUE) / (length(na.omit(x))^(1 / 3))
  }
  bin_width <- if (!is.null(manual_binwidth)) {
    message(glue::glue(">> For '{current_var_name}', using manual binwidth: {manual_binwidth}"))
    manual_binwidth
  } else {
    bw_treatment <- calculate_fd_bw(data %>% filter(group == "treatment") %>%
      pull({{ var_name_enquo }}))
    bw_control <- calculate_fd_bw(data %>% filter(group == "control") %>%
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
    theme(
      axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      plot.margin = margin(b = 0, unit = "pt")
    ) +
    labs(x = NULL, y = NULL)

  treatment_box <- ggplot(data_treat, aes(x = {{ var_name_enquo }}, fill = group)) +
    geom_boxplot(width = 0.5, alpha = 1) +
    geom_text_repel(
      data = filter(outlier_data, group == "treatment"),
      aes(x = {{ var_name_enquo }}, y = 0, label = iso3c),
      size = 3.5, max.overlaps = Inf, nudge_y = 0.25, direction = "x"
    ) +
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
    theme(
      axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      plot.margin = margin(t = 5, b = 0, unit = "pt")
    ) +
    labs(x = NULL, y = NULL)

  control_box <- ggplot(data_control, aes(x = {{ var_name_enquo }}, fill = group)) +
    geom_boxplot(width = 0.5, alpha = 1) +
    geom_text_repel(
      data = filter(outlier_data, group == "control"),
      aes(x = {{ var_name_enquo }}, y = 0, label = iso3c),
      size = 3.5, max.overlaps = Inf, nudge_y = 0.25, direction = "x"
    ) +
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
    plot_layout(heights = c(8, 1, 8, 1), guides = "collect") &
    theme(
      legend.position = "top"
    )

  file_name <- glue::glue("dist_{current_var_name}.png")
  ggsave(file.path(output_dir, file_name), final_plot, width = 7, height = 9, bg = "white")

  return(final_plot)
}


# 5. OUTLIER REPORT TABLE ====================================================
#'
#' Identifies outliers (1.5*IQR rule) and generates a gt table report.
#' Adds Z-scores for context.
#'
#' @param data A dataframe.
#' @param var_name The name of the variable to scan (unquoted symbol).
#' @param var_label A string for the table's title.
#' @param output_dir The directory where the .html table will be saved.
#' @param title An optional title for the table (used in tab_header).
#'
create_outlier_table <- function(data, var_name, var_label, output_dir, title = NULL) {
  var_name_enquo <- enquo(var_name)
  current_var_name <- quo_name(var_name_enquo)

  # 1. Identify outliers and calculate Z-scores
  outlier_data <- data %>%
    group_by(group) %>%
    mutate(
      # IQR Logic
      iqr = IQR({{ var_name_enquo }}, na.rm = TRUE),
      upper_bound = quantile({{ var_name_enquo }}, 0.75, na.rm = TRUE) + 1.5 * iqr,
      lower_bound = quantile({{ var_name_enquo }}, 0.25, na.rm = TRUE) - 1.5 * iqr,

      # Calculate Z-Score for context
      mean_val = mean({{ var_name_enquo }}, na.rm = TRUE),
      sd_val = sd({{ var_name_enquo }}, na.rm = TRUE),
      z_score = ({{ var_name_enquo }} - mean_val) / sd_val,

      # Determine Type
      outlier_type = case_when(
        {{ var_name_enquo }} > upper_bound ~ "High",
        {{ var_name_enquo }} < lower_bound ~ "Low",
        TRUE ~ NA_character_
      ),
      is_outlier = !is.na(outlier_type)
    ) %>%
    filter(is_outlier) %>%
    ungroup() %>%
    # Select columns
    select(iso3c, year, group, outlier_type, value = {{ var_name_enquo }}, z_score) %>%
    # Sort by VALUE (High -> Low)
    # This puts High outliers at the top and Low outliers at the bottom
    arrange(desc(value))

  # 2. If no outliers, print a message and exit
  if (nrow(outlier_data) == 0) {
    message(paste("  No outliers found for", current_var_name))
    return(invisible(NULL))
  }

  # 3. Create and save the gt table
  message(paste("  Saving outlier table for", current_var_name))

  outlier_table <- gt(outlier_data) %>%
    tab_header(
      title = gt::md(glue::glue("**{title}**")),
      subtitle = "Outliers defined by 1.5*IQR rule"
    ) %>%
    cols_label(
      iso3c = "Country",
      year = "Year",
      group = "Group",
      outlier_type = "Type",
      value = "Value",
      z_score = "Z-Score"
    ) %>%
    fmt_number(
      columns = c(value, z_score),
      decimals = 3
    ) %>%
    # Removed data_color() as requested
    theme_gt_bachelor_project() %>%
    fmt_number(
      columns = year,
      decimals = 0,
      use_seps = FALSE
    )

  # Save as .html
  file_name <- glue::glue("outliers_{current_var_name}.html")
  gtsave(outlier_table, file = file.path(output_dir, file_name))

  return(invisible(outlier_table))
}


# 6. AGGREGATED TIME SERIES PLOT =============================================
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
    geom_vline(xintercept = treatment_year - 1, linetype = "dashed", color = "grey40") +
    scale_x_continuous(breaks = seq(2014, 2024, by = 2)) +
    scale_color_project_qual(name = NULL) +
    theme_bachelor_project() +
    labs(
      x = NULL,
      y = NULL
    )

  if (!is.null(title)) {
    ts_plot <- ts_plot + labs(title = title)
  }

  file_name <- glue::glue("ts_agg_{current_var_name}.png")
  ggsave(file.path(output_dir, file_name), ts_plot, width = 8, height = 6, bg = "white")

  return(ts_plot)
}


# 7. INDIVIDUAL TIME SERIES PLOT =============================================
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


# 8. SDID PLOT STOCHASTIC (BOOSTRAP) CALCULATIONS ============================
get_raw_sdid_plot <- function(estimate_obj, overlay = FALSE) {
  p_raw <- plot(estimate_obj,
    overlay = as.numeric(overlay),
    line.width = 1.2,
    point.size = 0,
    trajectory.linetype = 1,
    trajectory.alpha = 1,
    effect.alpha = 1,
    diagram.alpha = 0.6,
    se.method = SE_METHOD
  )
  return(p_raw)
}


# 9. SDID PLOT STYLING =======================================================
style_sdid_plot <- function(raw_plot, vline_year, plot_title) {
  p_styled <- raw_plot +
    geom_vline(xintercept = vline_year - 1, linetype = "dashed", color = "grey40") +
    scale_x_continuous(breaks = seq(2014, 2024, by = 2), limits = c(2014, 2024)) +
    scale_y_continuous(labels = scales::label_percent(
      scale = 1,
      accuracy = 0.01,
      decimal.mark = ","
    )) +
    scale_color_project_qual(
      name = NULL,
      breaks = c("treated", "synthetic control"),
      labels = c("behandlet", "syntetisk kontrol")
    ) +
    theme_bachelor_project() +
    labs(x = NULL, y = NULL, title = plot_title)

  p_styled$layers[[7]] <- NULL
  return(p_styled)
}


# 10. DUMBBELL ===============================================================
#' Creates a dumbbell plot showing change between two years.
#'
#' @param data A dataframe containing panel data.
#' @param var_name The variable to plot on the x-axis (unquoted symbol).
#' @param var_label A label for the x-axis.
#' @param start_year The starting year for the dumbbell.
#' @param end_year The ending year for the dumbbell.
#' @param output_dir The directory to save the plot in.
#' @return A ggplot object.
#'
create_dumbbell_plot <- function(data, var_name, var_label, start_year, end_year, output_dir) {
  var_name_enquo <- enquo(var_name)
  current_var_name <- quo_name(var_name_enquo)
  base_font <- "IBM Plex Sans"

  plot_data <- data %>%
    filter(group == "treatment", year %in% c(start_year, end_year)) %>%
    select(iso3c, year, {{ var_name_enquo }})

  wide_data <- plot_data %>%
    pivot_wider(
      names_from = year,
      values_from = {{ var_name_enquo }},
      names_prefix = "year_"
    ) %>%
    mutate(
      iso3c = fct_reorder(iso3c, .data[[paste0("year_", end_year)]]),
      point_type_start = factor(as.character(start_year), levels = c(as.character(start_year), as.character(end_year))),
      point_type_end = factor(as.character(end_year), levels = c(as.character(start_year), as.character(end_year)))
    )

  color_values <- setNames(
    c("grey50", project_colors["treatment"]),
    c(as.character(start_year), as.character(end_year))
  )

  dumbbell_plot <- ggplot(wide_data, aes(y = iso3c)) +
    geom_segment(aes(x = .data[[paste0("year_", start_year)]], xend = .data[[paste0("year_", end_year)]], yend = iso3c), color = "grey70", linewidth = 0.8) +
    geom_point(aes(x = .data[[paste0("year_", start_year)]], color = point_type_start), size = 3) +
    geom_point(aes(x = .data[[paste0("year_", end_year)]], color = point_type_end), size = 3) +
    geom_vline(xintercept = 2, linetype = "dashed", color = "black", linewidth = 0.8) +
    scale_fill_project_qual() +
    theme_bachelor_project() +
    theme(
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12)
    ) +
    labs(
      title = glue::glue("{var_label} {start_year} vs. {end_year}"),
      x = var_label,
      y = NULL
    )

  file_name <- glue::glue("{current_var_name}_dumbbell_treatment_{start_year}_{end_year}.png")
  ggsave(file.path(output_dir, file_name), dumbbell_plot, width = 8, height = 10, bg = "white")

  return(dumbbell_plot)
}
