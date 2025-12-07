# ---------------------------------------------------------------------------- #
#
#   Projekt:      BACHELOR PROJEKT
#   Script:       00_functions.R
#   Forfatter:    Frederik Bender Bøeck-Nielsen
#   Dato:         07-12-2025
#   Beskrivelse:  Indeholder brugerdefineret farvepalet, ggplot2 tema, gt tabel
#                 tema og én enkelt funktion til at identificere outliers.
#
# ---------------------------------------------------------------------------- #


# 1. OPSÆTNING AF ARBEJDSMILJØ =================================================

# Indlæs pakker
library(tidyverse) # data manipulation
library(hrbrthemes) # ggplot2 tema
library(gt) # gt tabel tema
library(glue) # formatér beskeder


# 2. BRUGERDEFINEREDE TEMAER ===================================================

## 2.1. FARVEPALETTE -----------------------------------------------------------
project_colors <- list(treated = "#0077b6", control = "#f48c06")
# Brug automatisk de angivede farver til behandings- og kontrolgruppen
project_qual_colors_map <- c(
  "Behandlet" = project_colors$treated,
  "Kontrol" = project_colors$control
)

scale_color_project_qual <- function(...) {
  scale_color_manual(values = project_qual_colors_map, ...)
}

scale_fill_project_qual <- function(...) {
  scale_fill_manual(values = project_qual_colors_map, ...)
}

## 2.2. GGPLOT2 TEMA -----------------------------------------------------------
ba_theme <- function(...) {
  theme_ipsum(base_family = "IBM Plex Serif", ...) +
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

  # sørger for at funktionen både fungerer for at én samlet gruppe observationer,
  # samt observationer fordelt på grupper (behandlet og kontrol)
  if ("group" %in% names(data)) {
    data_calc <- data %>% group_by(group)
  } else {
    data_calc <- data
  }

  outlier_data <- data_calc %>%
    mutate(
      # 1,5 * IQR til at identificere outliers
      iqr = IQR({{ var_name_enquo }}, na.rm = TRUE),
      upper_bound = quantile({{ var_name_enquo }}, 0.75, na.rm = TRUE) + 1.5 * iqr,
      lower_bound = quantile({{ var_name_enquo }}, 0.25, na.rm = TRUE) - 1.5 * iqr,
      # Udregn Z-Score
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
    message(glue("\nOutliers (1,5 * IQR) for {var_label}:"))
    print(as.data.frame(outlier_data))
  } else {
    message(glue("\nIngen outliers for {var_label}."))
  }
}
