# ---------------------------------------------------------------------------- #
#
#   Projekt:      BACHELOR PROJEKT
#   Script:       02_es_eda.R
#   Forfatter:    Frederik Bender Bøeck-Nielsen
#   Dato:         05-12-2025
#   Beskrivelse:  EDA (eksplorativ data analyse) for event-study modellerne, med
#                 henblik på at sammenligne behandlings- og kontrolgruppen, og
#                 undersøge deres distribuering.
#                 1. Genererer deskriptiv statistik tabel.
#                 2. Generer kombineret histogram + boxplot figurer.
#                 3. Genererer aggregerede tidsserie figurer.
#
# ---------------------------------------------------------------------------- #


# 1. OPSÆTNING AF ARBEJDSMILJØ =================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

# Indlæser pakker
library(conflicted) # håndtering af pakke konflikter
library(here) # robuste filstier
library(tidyverse) # data manipulation
library(gtsummary)
library(gt)
library(glue)
library(psych) # udregn skew

# Håndterer konflikter
conflict_prefer("filter", "dplyr")
conflict_prefer("alpha", "ggplot2")

# Input filstier
DIR_SCRIPTS <- here("scripts")
DIR_DATA <- here("data", "_processed")
ES_PANEL <- file.path(DIR_DATA, "es_panel.rds")

# Output filstier
DIR_TAB <- here("_output", "_tables", "_es_eda")
DIR_FIG <- here("_output", "_figures", "_es_eda")
if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

# Indlæser funktioner
source(file.path(DIR_SCRIPTS, "00_functions.R"))

# Sætter komma som decimal-tegn
options(OutDec = ",")


## 1.1. PARAMETRE --------------------------------------------------------------

# Definerer behandlingsår
TREAT_YEAR <- 2022

# Variabler
VARS_FOR_EDA <- c(
  "milex_gdp"     = "Forsvarsudgifter (% BNP)",
  "milex_usd_log" = "Forsvarsudgifter (Log USD)"
)

# Histogram breaks
MANUAL_BREAKS <- list(
  "milex_gdp"     = seq(0, 3, by = 0.5),
  "milex_usd_log" = seq(18, 26, by = 2)
)

# Histogram binwidths
MANUAL_BINWIDTHS <- list("milex_gdp" = 0.25, "milex_usd_log" = 1)

# Histogram + Boxplot captions
MANUAL_CAPTIONS <- list(
  "milex_gdp" = "Den eneste outlier er Sydkorea (KOR) 2020, med en Z-score på 2,19."
)


# 2. DATAFORBEREDELSE ==========================================================
message("--- Sektion 2: Indlæser og forbereder data ---")

# Indlæser event-study panel fra 01_data_prep.R
es_panel <- readRDS(ES_PANEL) %>%
  mutate(group = factor(group, levels = c("Behandlet", "Kontrol")))

# Filtrerer for pre-treatment perioden
pre_panel <- es_panel %>%
  filter(year < TREAT_YEAR) %>%
  select(group, iso3c, year, all_of(names(VARS_FOR_EDA)))

# beregner landegennemsnit til udregning af SMD (Hedges' g)
country_avg_df <- pre_panel %>%
  group_by(group, iso3c) %>%
  summarise(across(all_of(names(VARS_FOR_EDA)), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

# Aggregerer data til tidsserie plots
agg_data <- es_panel %>%
  group_by(group, year) %>%
  summarise(
    milex_gdp = mean(milex_gdp, na.rm = TRUE),
    milex_usd_log = mean(milex_usd_log, na.rm = TRUE),
    .groups = "drop"
  )


# 3. DESKRIPTIV STATISTIK TABEL ================================================
message("--- Sektion 3: Genererer deskriptiv statistik tabel ---")

# Hjælpefunktion til udregning af skævhed (skew)
skew <- function(x) as.numeric(psych::skew(x, na.rm = TRUE))

# Udregner SMD
smd_lookup <- country_avg_df %>%
  select(group, all_of(names(VARS_FOR_EDA))) %>%
  tbl_summary(by = group, missing = "no") %>%
  add_difference(test = everything() ~ "smd") %>%
  getElement("table_body") %>%
  transmute(
    variable,
    smd_formatted = formatC(estimate, digits = 2, format = "f", decimal.mark = ",")
  )

# Opretter tabel
sum_stat_table <- pre_panel %>%
  select(group, all_of(names(VARS_FOR_EDA))) %>%
  tbl_summary(
    by = group,
    label = as.list(VARS_FOR_EDA),
    missing = "no",
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{mean} ({sd})", "{min}–{max} ({skew})"),
    digits = all_continuous() ~ 2
  ) %>%
  # Tilføjer SMD og oversætter til dansk
  modify_table_body(
    ~ .x %>%
      left_join(smd_lookup, by = "variable") %>%
      mutate(
        label = case_when(
          label == "Mean (SD)" ~ "Gns. (SD)",
          str_detect(label, "Min") ~ "Min.–maks. (skævhed)",
          TRUE ~ label
        ),
        smd_formatted = ifelse(label == "Gns. (SD)", smd_formatted, "")
      )
  ) %>%
  modify_header(
    label = "",
    stat_1 = "Behandlet (N = {n})",
    stat_2 = "Kontrol (N = {n})",
    smd_formatted = "SMD"
  ) %>%
  as_gt() %>%
  fmt_markdown(columns = c(stat_1, stat_2)) %>%
  cols_align(align = "right", columns = c(stat_1, stat_2, smd_formatted)) %>%
  tab_footnote(
    footnote = "Standardiseret gennemsnitsforskel (Hedges' g). Beregnet på landegennemsnit.",
    locations = cells_column_labels(columns = smd_formatted)
  ) %>%
  tab_source_note("N repræsenterer antal lande-år observationer.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = label, rows = row_type == "label")
  ) %>%
  ba_theme_gt()

# Gemmer tabel
gtsave(sum_stat_table, file = file.path(DIR_TAB, "summary_statistics.html"))


# 4. DISTRIBUERINGSPLOTS (KOMBINERET HISTOGRAM + BOXPLOT) ======================
message("--- Sektion 4: Generer distribueringsplots ---")

## 4.1. HJÆLPEFUNKTION ---------------------------------------------------------
create_distribution_plot <- function(
  data,
  var_name,
  var_label,
  manual_breaks = NULL,
  manual_binwidth = NULL,
  manual_caption = NULL,
  output_dir = NULL
) {
  var_enquo <- enquo(var_name)
  var_str <- quo_name(var_enquo)
  fmt_zero <- function(x) ifelse(x == 0, "0", x)

  # Plot breaks
  if (!is.null(manual_breaks)) {
    global_limits <- range(manual_breaks)
    lbl_fun <- if (var_str %in% c("milex_gdp")) fmt_zero else waiver()
    x_scale <- scale_x_continuous(breaks = manual_breaks, labels = lbl_fun)
  } else {
    global_limits <- range(pull(data, {{ var_enquo }}), na.rm = TRUE)
    x_scale <- scale_x_continuous()
  }

  # Udregn optimal (unbiased) bin width hvis manuel bin width ikke er angivet
  if (!is.null(manual_binwidth)) {
    final_bw <- manual_binwidth
    message(glue::glue(">> '{var_str}': Manuel binwidth {final_bw}"))
  } else {
    calc_bw <- function(x) 2 * IQR(x, na.rm = TRUE) / (length(na.omit(x))^(1 / 3))
    treat_bw <- calc_bw(data %>% filter(group == "Behandlet") %>% pull({{ var_enquo }}))
    cntrl_bw <- calc_bw(data %>% filter(group == "Kontrol") %>% pull({{ var_enquo }}))
    final_bw <- mean(c(treat_bw, cntrl_bw), na.rm = TRUE)
    message(glue::glue(">> '{var_str}': Beregnet unbiased binwidth {round(final_bw, 3)}"))
  }

  # Identificer outliers
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
    group_by(group, iso3c) %>%
    slice_max(dev, n = 1, with_ties = FALSE) %>%
    ungroup()

  # PLOT ELEMENTER
  # Common theme for at reducere repetion
  common_theme <- list(
    scale_fill_project_qual(guide = "none"),
    ba_theme()
  )

  # Behandlet histogram
  p_th <- ggplot(
    data %>%
      filter(group == "Behandlet"),
    aes(x = {{ var_enquo }}, fill = group)
  ) +
    geom_histogram(binwidth = final_bw, color = "white") +
    scale_fill_project_qual() +
    ba_theme() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(x = NULL, y = "Frekvens")

  # Behandlet boxplot
  p_tb <- ggplot(
    data %>%
      filter(group == "Behandlet"),
    aes(x = {{ var_enquo }}, fill = group)
  ) +
    geom_boxplot(width = 0.5) +
    geom_text_repel(
      data = filter(outliers, group == "Behandlet"),
      aes(x = {{ var_enquo }}, y = 0, label = iso3c),
      size = 3.5,
      nudge_y = 0.25,
      direction = "x",
      family = "IBM Plex Serif"
    ) +
    common_theme +
    theme(
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    labs(x = NULL, y = NULL)

  # Kontrol histogram
  p_ch <- ggplot(
    data %>%
      filter(group == "Kontrol"),
    aes(x = {{ var_enquo }}, fill = group)
  ) +
    geom_histogram(binwidth = final_bw, color = "white") +
    common_theme +
    scale_fill_project_qual() +
    ba_theme() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(x = NULL, y = NULL)

  # Kontrol boxplot
  p_cb <- ggplot(data %>% filter(group == "Kontrol"), aes(x = {{ var_enquo }}, fill = group)) +
    geom_boxplot(width = 0.5) +
    geom_text_repel(
      data = filter(outliers, group == "Kontrol"),
      aes(x = {{ var_enquo }}, y = 0, label = iso3c),
      size = 3.5,
      nudge_x = 0.05,
      direction = "x",
      family = "IBM Plex Serif"
    ) +
    common_theme +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    labs(x = var_label, y = NULL, caption = manual_caption)

  # Samler plots og gemmer
  final_plot <- (p_th / p_tb / p_ch / p_cb) + plot_layout(heights = c(8, 1, 8, 1))
  final_plot <- final_plot & coord_cartesian(xlim = global_limits) & x_scale
  if (!is.null(output_dir)) {
    ggsave(
      file.path(output_dir, glue::glue("dist_{var_str}.png")), final_plot,
      width = 8, height = 6
    )
  }

  return(final_plot)
}

## 4.2. GENERER PLOTS ----------------------------------------------------------
for (var in names(VARS_FOR_EDA)) {
  create_distribution_plot(
    data = pre_panel,
    var_name = !!sym(var),
    var_label = VARS_FOR_EDA[var],
    manual_breaks = MANUAL_BREAKS[[var]],
    manual_binwidth = MANUAL_BINWIDTHS[[var]],
    manual_caption = MANUAL_CAPTIONS[[var]],
    output_dir = DIR_FIG
  )
}


# 5. AGGREGERET TIDSSERIE PLOTS ================================================
message("--- Sektion 5: Generer aggregeret tidsserie plots ---")

## 5.1. HJÆLPEFUNKTION ---------------------------------------------------------
save_trend_plot <- function(y_var, y_axis_label) {
  ts_plot <- ggplot(
    agg_data,
    aes(x = year, y = .data[[y_var]], color = group, group = group)
  ) +
    geom_line() +
    geom_vline(
      xintercept = TREAT_YEAR - 1,
      linetype = "dashed",
      color = "grey40"
    ) +
    scale_x_continuous(breaks = seq(2014, 2024, by = 1)) +
    scale_color_project_qual(name = NULL) +
    ba_theme() +
    labs(x = NULL, y = y_axis_label)

  ggsave(
    here(DIR_FIG, glue("agg_ts_{y_var}.png")), ts_plot,
    width = 8, height = 6
  )
}

# Forsvarsudgifter (% BNP)
save_trend_plot(
  y_var = "milex_gdp",
  y_axis_label = "Forsvarsudgifter (% BNP)"
)

# Forsvarsudgifter (Log USD)
save_trend_plot(
  y_var = "milex_usd_log",
  y_axis_label = "Forsvarsudgifter (Log USD)"
)


# 6. TJEK FOR OUTLIERS =========================================================
# Kalder funktion fra 00_functions.R til at tjekke for outliers (1,5 * IQR).
# Hvis der er nogle, så printes de til konsollen med deres respektive Z-Scores.
message("--- Sektion 6: Tjekker for outliers (1,5 * IQR + Z-Score) ---")

for (var in names(VARS_FOR_EDA)) {
  print_outlier_report(
    data = pre_panel,
    var_name = !!sym(var),
    var_label = VARS_FOR_EDA[var]
  )
}


# 7. SCRIPT FÆRDIG =============================================================

message(paste(
  "\n--- Script 02_es_eda.R færdigt ---",
  "\nAlle tabeller er gemt i:", DIR_TAB,
  "\nAlle figurer er gemt i:", DIR_FIG
))
