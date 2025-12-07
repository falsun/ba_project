# ---------------------------------------------------------------------------- #
#
#   Projekt:      BACHELOR PROJEKT
#   Script:       07_ols_eda.R
#   Forfatter:    Frederik Bender Bøeck-Nielsen
#   Dato:         06-12-2025
#   Beskrivelse:  EDA for OLS data.
#                 1. Gemmer deskriptiv statistik tabel
#                 2. Generer kombineret histogram + boxplot figurer
#                 3. Tjekker for outliers (1,5 * IQR)
#
# ---------------------------------------------------------------------------- #


# 1. OPSÆTNING AF ARBEJDSMILJØ =================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

# Indlæser pakker
library(conflicted)
library(here)
library(tidyverse)
library(glue)
library(gt)
library(ggcorrplot)
library(patchwork)
library(moments)

# håndterer konflikter
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("alpha", "ggplot2")

# Input filstier
DIR_SCRIPTS <- here("scripts")
DIR_DATA <- here("data", "_processed")
OLS_DATA <- file.path(DIR_DATA, "ols_data.rds")

# Output filstier
DIR_TAB <- here("_output", "_tables", "_ols_eda")
DIR_FIG <- here("_output", "_figures", "_ols_eda")
if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

# Indlæser funktioner og brugerdefinerede temaer
source(file.path(DIR_SCRIPTS, "00_functions.R"))

# Sætter komma som decimal-tegn
options(OutDec = ",")


## 1.1. PARAMETRE --------------------------------------------------------------

# Variabler
VARS_FOR_OLS <- c(
  "milex_gdp_pre"       = "Ændring i forsvarsudgifter 2014-21 (% BNP)",
  "milex_gdp_post"      = "Ændring i forsvarsudgifter 2021-25 (% BNP)",
  "milex_usd_pre"       = "Ændring i forsvarsudgifter 2014-21 (log US$)",
  "milex_usd_post"      = "Ændring i forsvarsudgifter 2021-24 (log US$)",
  "dist_enemy_log"      = "Afstand til fjende (log km)",
  "nato_gap_2014"       = "Afstand til 2% mål i 2014 (procentpoint)",
  "nato_gap_2021"       = "Afstand til 2% mål i 2021 (procentpoint)",
  "gdp_2014_log"        = "BNP i 2014 (log milliarder)",
  "gdp_2021_log"        = "BNP i 2021 (log milliarder)",
  "gdp_cap_2021_log"    = "BNP per indbygger i 2021 (log)",
  "debt_gdp_2021_log"   = "Offentlig gæld i 2021 (log % BNP)",
  "gdp_growth_post"     = "BNP vækst 2021-25 (procentpoint)",
  "us_troops_2021_log"  = "Antal amerikanske tropper i 2021 (log)"
)

# Dummies
VARS_TO_SKIP <- c("border_rus", "post_com")

# Manuelle Breaks
MANUAL_BREAKS_OLS <- list(
  "milex_gdp_pre"      = seq(-0.2, 1.4, by = 0.4),
  "milex_gdp_post"     = seq(0, 2.5, by = 0.5),
  "milex_usd_pre"      = seq(-0.2, 1.2, by = 0.2),
  "milex_usd_post"     = seq(0, 0.8, by = 0.2),
  "dist_enemy_log"     = seq(4.5, 8.5, by = 0.5),
  "nato_gap_2014"      = seq(0, 1.8, by = 0.2),
  "nato_gap_2021"      = seq(0, 1.8, by = 0.2),
  "gdp_2014_log"       = seq(2, 9, by = 1),
  "gdp_2021_log"       = seq(2, 9, by = 1),
  "gdp_cap_2021_log"   = seq(9.5, 12, by = 0.5),
  "debt_gdp_2021_log"  = seq(2.5, 5.5, by = 0.5),
  "gdp_growth_post"    = seq(-0.05, 0.20, by = 0.05),
  "us_troops_2021_log" = seq(1, 11, by = 2)
)

# Manuelle Binwidths
MANUAL_BINWIDTHS_OLS <- list(
  "milex_gdp_pre"      = 0.20,
  "milex_gdp_post"     = 0.25,
  "milex_usd_pre"      = 0.20,
  "milex_usd_post"     = 0.10,
  "dist_enemy_log"     = 0.50,
  "nato_gap_2014"      = 0.20,
  "nato_gap_2021"      = 0.20,
  "gdp_2014_log"       = 1.00,
  "gdp_2021_log"       = 1.00,
  "gdp_cap_2021_log"   = 0.25,
  "debt_gdp_2021_log"  = 0.50,
  "gdp_growth_post"    = 0.025,
  "us_troops_2021_log" = 1.00
)

# Manuelle Captions
MANUAL_CAPTIONS_OLS <- list(
  "milex_gdp_pre"    = "Z-scores: Litauen (2,36) og Letland (2,74).",
  "milex_usd_pre"    = "Z-scores: Letland (2,05) og Litauen (2,96).",
  "dist_enemy_log"   = "Z-scores: Litauen (-2,89) og Letland (-1,55).",
  "gdp_cap_2021_log" = "Z-scores: Albanien (-2,67) og Luxembourg (2,57).",
  "gdp_growth_post"  = "Z-scores: Estland (-2,24), Luxembourg (-1,39), Tyskland (-1,38), Portugal (1,15), Spanien (1,51), Albanien (1,72) og Kroatien (1,98)."
)

# Manuelle nudges (iso3c outlier labels overlap i boxplot)
MANUAL_NUDGES_OLS <- list(
  "gdp_growth_post" = c("LUX" = 0.05, "DEU" = -0.05, "ESP" = -0.05, "ALB" = 0.05)
)


# 2. DATAFORBEREDELSE ==========================================================
message("--- Sektion 2: Indlæser og forbereder data ---")

# Indlæser tværsnitsdata fra 06_ols_data_trans.R
ols_data <- readRDS(OLS_DATA) %>%
  select(iso3c, all_of(names(VARS_FOR_OLS)))


# 3. DESKRIPTIV STATISTIK TABEL ================================================
message("--- Sektion 3: Generer deskriptiv statistik tabel ---")

var_meta <- enframe(VARS_FOR_OLS, name = "var_code", value = "label") %>%
  mutate(
    category = case_when(
      str_detect(var_code, "milex") ~ "Afhængige variabler",
      var_code %in% c(
        "gdp_cap_2021_log", "gdp_growth_post",
        "debt_gdp_2021_log", "us_troops_2021_log",
        "post_com"
      ) ~ "Alternative forklaringer",
      TRUE ~ "Uafhængige variabler"
    ),
    category = factor(category, levels = c(
      "Afhængige variabler",
      "Uafhængige variabler",
      "Alternative forklaringer"
    )),
    label = factor(label, levels = unname(VARS_FOR_OLS))
  ) %>%
  arrange(category, label)

summary_stats <- ols_data %>%
  select(all_of(var_meta$var_code)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "var_code",
    values_to = "value"
  ) %>%
  group_by(var_code) %>%
  summarize(
    Mean = mean(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    Min = min(value, na.rm = TRUE),
    Max = max(value, na.rm = TRUE),
    Skew = moments::skewness(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(var_meta, by = "var_code") %>%
  arrange(category, label)

tbl_ols_summary <- summary_stats %>%
  group_by(category) %>%
  select(label, Mean, SD, Min, Max, Skew) %>%
  gt() %>%
  fmt_number(
    columns = c(Mean, SD, Min, Max, Skew),
    decimals = 2,
    dec_mark = ",",
    sep_mark = "."
  ) %>%
  cols_merge(columns = c(Mean, SD), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(Min, Max), pattern = "{1}–{2}") %>%
  cols_label(
    label = "",
    Mean = "Gns. (SD)",
    Min = "Min.–maks.",
    Skew = "Skævhed"
  ) %>%
  tab_source_note("N = 22 for alle variabler.") %>%
  cols_align(align = "right", columns = c(Mean, Min)) %>%
  cols_align(align = "left", columns = label) %>%
  ba_theme_gt() %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  gtsave(file = file.path(DIR_TAB, "ols_summary_stats.html"))


# 4. OUTLIER REPORT ============================================================
# Kalder funktion fra 00_functions.R til at teste for outliers (1,5 * IQR regel).
# Hvis der er outliers printes de til konsollen sammen med deres Z-score.
message("--- Sektion 4: Tjekker for outliers (1,5 * IQR) ---")

for (var in names(VARS_FOR_OLS)) {
  print_outlier_report(
    data = ols_data,
    var_name = !!sym(var),
    var_label = VARS_FOR_OLS[var]
  )
}


# 5. DISTRIBUERINGSPLOT ========================================================
# Generer kombineret histogram + boxplot
message("--- Sektion 5: Genererer Distribueringsplots ---")

## 5.1. HJÆLPEFUNKTION ---------------------------------------------------------
create_ols_distribution_plot <- function(
  data,
  var_name,
  var_label,
  manual_breaks = NULL,
  manual_binwidth = NULL,
  manual_nudges = NULL,
  manual_caption = NULL,
  output_dir = NULL
) {
  var_enquo <- enquo(var_name)
  var_str <- quo_name(var_enquo)
  # x-axis formatering
  fmt_zero <- function(x) ifelse(x == 0, "0", x)
  # Limits og scales
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
  # Beregn optimal binwidth (Freedman-Diaconis)
  if (!is.null(manual_binwidth)) {
    final_bw <- manual_binwidth
    message(glue(">> '{var_str}': Manual binwidth {final_bw}"))
  } else {
    vals <- na.omit(pull(data, {{ var_enquo }}))
    iqr_val <- IQR(vals)
    if (iqr_val == 0) iqr_val <- diff(range(vals)) / 30

    final_bw <- 2 * iqr_val / (length(vals)^(1 / 3))
    message(glue(">> '{var_str}': Calc binwidth {round(final_bw, 3)}"))
  }
  # Identificer outliers
  outliers <- data %>%
    mutate(
      iqr_val = IQR({{ var_enquo }}, na.rm = TRUE),
      upper   = quantile({{ var_enquo }}, 0.75, na.rm = TRUE) + 1.5 * iqr_val,
      lower   = quantile({{ var_enquo }}, 0.25, na.rm = TRUE) - 1.5 * iqr_val,
      is_out  = {{ var_enquo }} > upper | {{ var_enquo }} < lower,
      dev     = abs({{ var_enquo }} - median({{ var_enquo }}, na.rm = TRUE))
    ) %>%
    filter(is_out) %>%
    group_by(iso3c) %>%
    slice_max(dev, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(y_pos = 0)
  # Manuelle nudges (for at undgå iso3c overlap)
  if (!is.null(manual_nudges)) {
    for (iso in names(manual_nudges)) {
      outliers$y_pos[outliers$iso3c == iso] <- manual_nudges[[iso]]
    }
  }
  # PLOT ELEMENTER
  # Histogram (top)
  p_hist <- ggplot(data, aes(x = {{ var_enquo }})) +
    geom_histogram(binwidth = final_bw, color = "white", fill = "grey40") +
    ba_theme() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    labs(x = NULL, y = "Frekvens")
  # Boxplot (bund)
  p_box <- ggplot(data, aes(x = {{ var_enquo }})) +
    geom_boxplot(width = 0.5, fill = "grey40", outlier.shape = NA) +
    geom_text(
      data = outliers,
      aes(y = y_pos, label = iso3c),
      size = 3.5,
      family = "IBM Plex Serif"
    ) +
    ba_theme() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    labs(x = var_label, y = NULL, caption = manual_caption)
  # Saml og gem
  final_plot <- (p_hist / p_box) + plot_layout(heights = c(8, 1))
  final_plot <- final_plot & coord_cartesian(xlim = global_limits) & x_scale
  if (!is.null(output_dir)) {
    ggsave(
      file.path(output_dir, glue("ols_dist_{var_str}.png")), final_plot,
      width = 7, height = 6
    )
  }
  return(final_plot)
}

## 5.2. GENERER PLOTS ----------------------------------------------------------
for (var in names(VARS_FOR_OLS)) {
  if (var %in% VARS_TO_SKIP) {
    message(glue(">> Skipping plot for binary variable: {var}"))
    next
  }

  # Tjek for manuelt angivede indstillinger
  curr_breaks <- if (var %in% names(MANUAL_BREAKS_OLS)) MANUAL_BREAKS_OLS[[var]] else NULL
  curr_bw <- if (var %in% names(MANUAL_BINWIDTHS_OLS)) MANUAL_BINWIDTHS_OLS[[var]] else NULL
  curr_cap <- if (var %in% names(MANUAL_CAPTIONS_OLS)) MANUAL_CAPTIONS_OLS[[var]] else NULL
  curr_nudges <- if (var %in% names(MANUAL_NUDGES_OLS)) MANUAL_NUDGES_OLS[[var]] else NULL

  # Generer plots
  create_ols_distribution_plot(
    data = ols_data,
    var_name = !!sym(var),
    var_label = VARS_FOR_OLS[var],
    manual_breaks = curr_breaks,
    manual_binwidth = curr_bw,
    manual_nudges = curr_nudges,
    manual_caption = curr_cap,
    output_dir = DIR_FIG
  )
}


# 5. SCRIPT FÆRDIG =============================================================

message(paste(
  "\n--- Script 07_ols_eda.R færdigt ---",
  "\nAlle tabeller er gemt i:", DIR_TAB,
  "\nAlle figurer er gemt i:", DIR_FIG
))
