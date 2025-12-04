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
  treated = "#0077b6",
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
      plot.caption         = element_text(size = 9, color = "black"),
      panel.grid.minor     = element_blank(),
      panel.grid.major     = element_blank(),
      plot.margin          = margin(t = 0, r = 0, b = 0, l = 0)
    )
}

#' Custom X-axis for Bachelor Project (Danish formatting)
#' Defaults to comma decimal, dot thousands, and allows extra arguments like limits
scale_x_dk <- function(...) {
  scale_x_continuous(
    # Pass the accuracy argument here (NULL lets R decide automatically)
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",", accuracy = accuracy),
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

# --- 3. CREATE CUSTOM COLOR & FILL SCALES ---
project_qual_colors_map <- c(
  "Behandlet"         = project_colors$treated,
  "Kontrol"           = project_colors$control
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

## --- 2.3 CREATE THE CUSTOM GT TABLE THEME ----
theme_gt_bachelor_project <- function(gt_object) {
  gt_object %>%
    # 1. Aesthetics (Fonts and Colors)
    opt_table_font(
      font = "IBM Plex Serif",
      color = "black",
      add = FALSE
    ) %>%
    tab_options(
      table.font.size = "11pt",
      footnotes.marks = "letters"
    ) %>%
    # 2. Bold Column Headers
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(columns = everything())
    )
}


# 3. OUTLIER REPORT ===============================================================
print_outlier_report <- function(data, var_name, var_label) {
  var_name_enquo <- enquo(var_name)

  # 1. Identify outliers and calculate Z-scores
  outlier_data <- data %>%
    group_by(group) %>%
    mutate(
      # IQR Logic
      iqr = IQR({{ var_name_enquo }}, na.rm = TRUE),
      upper_bound = quantile({{ var_name_enquo }}, 0.75, na.rm = TRUE) + 1.5 * iqr,
      lower_bound = quantile({{ var_name_enquo }}, 0.25, na.rm = TRUE) - 1.5 * iqr,

      # Calculate Z-Score
      mean_val = mean({{ var_name_enquo }}, na.rm = TRUE),
      sd_val = sd({{ var_name_enquo }}, na.rm = TRUE),
      z_score = ({{ var_name_enquo }} - mean_val) / sd_val,

      # Simple Boolean check: Is it above upper OR below lower?
      is_outlier = {{ var_name_enquo }} > upper_bound | {{ var_name_enquo }} < lower_bound
    ) %>%
    filter(is_outlier) %>%
    ungroup() %>%
    # Select only the specific columns you want
    select(iso3c, year, value = {{ var_name_enquo }}, z_score) %>%
    # Round numbers for clean console printing
    mutate(across(c(value, z_score), \(x) round(x, 3))) %>%
    arrange(desc(value))

  # 2. Print to Console
  if (nrow(outlier_data) > 0) {
    message(glue::glue(">> Outliers (1.5 * IQR) for {var_label}:"))

    # Print the dataframe
    print(as.data.frame(outlier_data))

  } else {
    message(glue::glue("\n>> Ingen outliers for {var_label}."))
  }
}


# 4. EVENT-STUDY DIST ==========================================================
create_distribution_plot <- function(data, var_name, var_label, manual_breaks = NULL,
                                     manual_binwidth = NULL, manual_caption = NULL, output_dir = NULL) {

  # --- 1. SETUP & HELPER VARS ---
  var_enquo <- enquo(var_name)
  var_str   <- quo_name(var_enquo)

  # Define formatting for x-axis (0 instead of 0.0)
  fmt_zero  <- function(x) ifelse(x == 0, "0", x)

  # Determine Limits & Scales
  # If manual breaks exist, use them for limits and formatting
  if (!is.null(manual_breaks)) {
    global_limits <- range(manual_breaks)
    # Apply specific formatting only to milex_gdp/lib_dem
    lbl_fun <- if (var_str %in% c("milex_gdp", "lib_dem")) fmt_zero else waiver()
    x_scale <- scale_x_continuous(breaks = manual_breaks, labels = lbl_fun)
  } else {
    # Fallback: Range of data
    global_limits <- range(pull(data, {{ var_enquo }}), na.rm = TRUE)
    x_scale <- scale_x_continuous()
  }

  # --- 2. CALCULATE BINWIDTH ---
  # If manual is provided, use it. Otherwise, calculate unbiased FD average.
  if (!is.null(manual_binwidth)) {
    final_bw <- manual_binwidth
    message(glue::glue(">> '{var_str}': Manual binwidth {final_bw}"))
  } else {
    calc_bw <- function(x) 2 * IQR(x, na.rm = TRUE) / (length(na.omit(x))^(1/3))
    treat_bw <- calc_bw(data %>% filter(group == "Behandlet") %>% pull({{ var_enquo }}))
    cntrl_bw <- calc_bw(data %>% filter(group == "Kontrol")   %>% pull({{ var_enquo }}))
    final_bw <- mean(c(treat_bw, cntrl_bw), na.rm = TRUE)
    message(glue::glue(">> '{var_str}': Calc binwidth {round(final_bw, 3)}"))
  }

  # --- 3. IDENTIFY OUTLIERS (LABELS ONLY) ---
  # We only need the row to label it, we don't need Z-scores here.
  outliers <- data %>%
    group_by(group) %>%
    mutate(
      iqr_val = IQR({{ var_enquo }}, na.rm = TRUE),
      upper   = quantile({{ var_enquo }}, 0.75, na.rm = TRUE) + 1.5 * iqr_val,
      lower   = quantile({{ var_enquo }}, 0.25, na.rm = TRUE) - 1.5 * iqr_val,
      is_out  = {{ var_enquo }} > upper | {{ var_enquo }} < lower,
      dev     = abs({{ var_enquo }} - median({{ var_enquo }}, na.rm = TRUE))
    ) %>%
    filter(is_out) %>%
    # Keep only the most extreme outlier per group/country for clean labeling
    group_by(group, iso3c) %>%
    slice_max(dev, n = 1, with_ties = FALSE) %>%
    ungroup()

  # --- 4. PLOT COMPONENTS ---
  # Common theme elements to reduce repetition
  common_theme <- list(
    scale_fill_project_qual(guide = "none"), # Hide guide in boxes, override in Treat Hist
    theme_bachelor_project()
  )

  # A. Treatment Histogram (Top)
  p_th <- ggplot(data %>% filter(group == "Behandlet"), aes(x = {{ var_enquo }}, fill = group)) +
    geom_histogram(binwidth = final_bw, color = "white") +
    scale_fill_project_qual() + # Keep legend here
    theme_bachelor_project() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()
          ) +
    labs(x = NULL, y = "Frekvens")

  # B. Treatment Boxplot
  p_tb <- ggplot(data %>% filter(group == "Behandlet"), aes(x = {{ var_enquo }}, fill = group)) +
    geom_boxplot(width = 0.5) +
    geom_text_repel(data = filter(outliers, group == "Behandlet"),
                    aes(x = {{ var_enquo }}, y = 0, label = iso3c),
                    size = 3.5, nudge_y = 0.25, direction = "x", family = "IBM Plex Serif") +
    common_theme +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank()
          ) +
    labs(x = NULL, y = NULL)

  # C. Control Histogram
  p_ch <- ggplot(data %>% filter(group == "Kontrol"), aes(x = {{ var_enquo }}, fill = group)) +
    geom_histogram(binwidth = final_bw, color = "white") +
    common_theme + # Legend hidden by default in common_theme (guide=none)
    # BUT we want the label "Kontrol" to appear.
    # Since guide="none" hides it, we override it just for text or rely on theme placement.
    # Actually, your previous code relied on the fill scale defaults.
    # Let's trust the 'scale_fill_project_qual' default behavior for consistency:
    scale_fill_project_qual() +
    theme_bachelor_project() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()
          ) +
    labs(x = NULL, y = NULL)

  # D. Control Boxplot (Bottom)
  p_cb <- ggplot(data %>% filter(group == "Kontrol"), aes(x = {{ var_enquo }}, fill = group)) +
    geom_boxplot(width = 0.5) +
    geom_text_repel(data = filter(outliers, group == "Kontrol"),
                    aes(x = {{ var_enquo }}, y = 0, label = iso3c),
                    size = 3.5, nudge_x = 0.05, direction = "x", family = "IBM Plex Serif") +
    common_theme +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()
          ) +
    labs(x = var_label, y = NULL, caption = manual_caption)

  # --- 5. ASSEMBLE & SAVE ---
  final_plot <- (p_th / p_tb / p_ch / p_cb) + plot_layout(heights = c(8, 1, 8, 1))

  final_plot <- final_plot & coord_cartesian(xlim = global_limits) & x_scale

  if (!is.null(output_dir)) {
    ggsave(file.path(output_dir, glue::glue("dist_{var_str}.png")),
           final_plot, width = 8, height = 6, bg = "white")
  }

  return(final_plot)
}


# OLS DIST ========================================
create_ols_distribution_plot <- function(data, var_name, var_label,
                                         manual_breaks = NULL, manual_binwidth = NULL,
                                         manual_nudges = NULL,
                                         manual_caption = NULL, output_dir = NULL) {

  # --- 1. SETUP & HELPER VARS ---
  var_enquo <- enquo(var_name)
  var_str   <- quo_name(var_enquo)

  # Helper for x-axis formatting
  fmt_zero  <- function(x) ifelse(x == 0, "0", x)

  # Determine Limits & Scales
  if (!is.null(manual_breaks)) {
    global_limits <- range(manual_breaks)
    # Apply special formatting for percentage variables
    lbl_fun <- if (grepl("milex_gdp", var_str) || grepl("gap", var_str)) fmt_zero else waiver()
    x_scale <- scale_x_continuous(breaks = manual_breaks, labels = lbl_fun)
  } else {
    # Fallback to data range
    global_limits <- range(pull(data, {{ var_enquo }}), na.rm = TRUE)
    x_scale <- scale_x_continuous()
  }

  # --- 2. CALCULATE BINWIDTH ---
  if (!is.null(manual_binwidth)) {
    final_bw <- manual_binwidth
    message(glue::glue(">> '{var_str}': Manual binwidth {final_bw}"))
  } else {
    # Calculate Freedman-Diaconis for the single variable
    vals <- na.omit(pull(data, {{ var_enquo }}))
    iqr_val <- IQR(vals)
    # Safety fallback if IQR is 0
    if (iqr_val == 0) iqr_val <- diff(range(vals)) / 30

    final_bw <- 2 * iqr_val / (length(vals)^(1/3))
    message(glue::glue(">> '{var_str}': Calc binwidth {round(final_bw, 3)}"))
  }

  # --- 3. IDENTIFY OUTLIERS ---
  outliers <- data %>%
    mutate(
      iqr_val = IQR({{ var_enquo }}, na.rm = TRUE),
      upper   = quantile({{ var_enquo }}, 0.75, na.rm = TRUE) + 1.5 * iqr_val,
      lower   = quantile({{ var_enquo }}, 0.25, na.rm = TRUE) - 1.5 * iqr_val,
      is_out  = {{ var_enquo }} > upper | {{ var_enquo }} < lower,
      dev     = abs({{ var_enquo }} - median({{ var_enquo }}, na.rm = TRUE))
    ) %>%
    filter(is_out) %>%
    # Ensure we only label each country once (max deviation)
    group_by(iso3c) %>%
    slice_max(dev, n = 1, with_ties = FALSE) %>%
    ungroup()%>%

    # --- 2. NEW LOGIC: APPLY MANUAL NUDGES ---
    mutate(y_pos = 0) # Default position is exactly on the line

  # If manual nudges exist, apply them
  if (!is.null(manual_nudges)) {
    # We loop through the iso3c codes provided in the list
    for (iso in names(manual_nudges)) {
      # If the outlier exists in our data, update its y_pos
      outliers$y_pos[outliers$iso3c == iso] <- manual_nudges[[iso]]
    }
  }

  # --- 4. PLOT COMPONENTS ---

  # A. Histogram (Top)
  p_hist <- ggplot(data, aes(x = {{ var_enquo }})) +
    geom_histogram(binwidth = final_bw, color = "white", fill = "grey40") +
    theme_bachelor_project() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
    ) +
    labs(x = NULL, y = "Frekvens")

  # B. Boxplot (Bottom)
  p_box <- ggplot(data, aes(x = {{ var_enquo }})) +
    geom_boxplot(width = 0.5, fill = "grey40", outlier.shape = NA) +
    geom_text(
      data = outliers,
      aes(y = y_pos, label = iso3c),
      size = 3.5,
      family = "IBM Plex Serif"
    ) +
    theme_bachelor_project() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
    ) +
    labs(x = var_label, y = NULL, caption = manual_caption)

  # --- 5. ASSEMBLE & SAVE ---
  final_plot <- (p_hist / p_box) + plot_layout(heights = c(8, 1))

  final_plot <- final_plot & coord_cartesian(xlim = global_limits) & x_scale

  if (!is.null(output_dir)) {
    ggsave(file.path(output_dir, glue::glue("ols_dist_{var_str}.png")),
           final_plot, width = 7, height = 6, bg = "white")
  }

  return(final_plot)
}


# 5. DUMBBELL ===============================================================
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
  base_font <- "IBM Plex Serif"

  plot_data <- data %>%
    filter(group == "Behandlet", year %in% c(start_year, end_year)) %>%
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
    c("grey50", project_colors["Behandlet"]),
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
