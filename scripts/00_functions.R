# ---------------------------------------------------------------------------- #
#
#   Projekt:      BACHELOR PROJEKT
#   Script:       00_functions.R
#   Forfatter:    Frederik Bender Bøeck-Nielsen
#   Dato:         05-12-2025
#   Beskrivelse:  Indeholder brugerdefinerede temaer (til figurer og tabeller),
#                 samt enkelte funktioner der bruges mere end én gang.
#
# ---------------------------------------------------------------------------- #


# 1. OPSÆTNING AF ARBEJDSMILJØ =================================================

# Indlæs pakker
library(tidyverse)
library(ggrepel)
library(patchwork)
library(ggridges)
library(hrbrthemes)
library(scales)


# 2. BRUGERDEFINEREDE TEMAER ===================================================

## 2.1. FARVEPALETTE -----------------------------------------------------------
project_colors <- list(treated = "#0077b6", control = "#f48c06")
# Brug automatisk de angivede farver til behandings- og kontrolgruppen
project_qual_colors_map <- c(
  "Behandlet" = project_colors$treated,
  "Kontrol" = project_colors$control
)

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

## 2.2. PLOT TEMA --------------------------------------------------------------
ba_theme <- function(...) {
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
scale_x_dk <- function(accuracy = NULL, ...) {
  scale_x_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",", accuracy = accuracy),
    ...
  )
}

#' Custom Y-axis (Danish formatting)
#' Allows optional control over accuracy (decimals) per plot
scale_y_dk <- function(accuracy = NULL, ...) {
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",", accuracy = accuracy),
    ...
  )
}


## 2.3. GT TABEL TEMA ----------------------------------------------------------
ba_theme_gt <- function(gt_object) {
  gt_object %>%
    opt_table_font(font = "IBM Plex Serif", color = "black", add = FALSE) %>%
    tab_options(table.font.size = "11pt", footnotes.marks = "letters") %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(columns = everything())
    )
}


# 3. OUTLIER RAPPORT ===========================================================
print_outlier_report <- function(data, var_name, var_label) {
  var_name_enquo <- enquo(var_name)

  if ("group" %in% names(data)) {
    data_calc <- data %>% group_by(group)
  } else {
    data_calc <- data
  }

  outlier_data <- data_calc %>%
    mutate(
      # 1,5 * IQR
      iqr = IQR({{ var_name_enquo }}, na.rm = TRUE),
      upper_bound = quantile({{ var_name_enquo }}, 0.75, na.rm = TRUE) + 1.5 * iqr,
      lower_bound = quantile({{ var_name_enquo }}, 0.25, na.rm = TRUE) - 1.5 * iqr,
      # Calculate Z-Score
      mean_val = mean({{ var_name_enquo }}, na.rm = TRUE),
      sd_val = sd({{ var_name_enquo }}, na.rm = TRUE),
      z_score = ({{ var_name_enquo }} - mean_val) / sd_val,
      is_outlier = {{ var_name_enquo }} > upper_bound | {{ var_name_enquo }} < lower_bound
    ) %>%
    filter(is_outlier) %>%
    ungroup() %>%
    select(
      iso3c,
      any_of(c("year", "group")),
      value = {{ var_name_enquo }},
      z_score
    ) %>%
    mutate(across(c(value, z_score), \(x) round(x, 3))) %>%
    arrange(desc(value))
  # Print til konsol
  if (nrow(outlier_data) > 0) {
    message(glue::glue("\nOutliers (1,5 * IQR) for {var_label}:"))
    print(as.data.frame(outlier_data))
  } else {
    message(glue::glue("\nIngen outliers for {var_label}."))
  }
}


# 4. P-VÆRDI FORMATERING =======================================================
format_p_val <- function(x) {
  sapply(x, function(val) {
    if (is.na(val)) {
      return(NA)
    }
    if (val < 0.001) {
      return("<0,001")
    }
    formatC(val, digits = 3, format = "f")
  })
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
    ba_theme() +
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
