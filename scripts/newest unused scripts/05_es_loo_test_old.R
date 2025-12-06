# ---------------------------------------------------------------------------- #
#
#   Projekt:      BACHELOR PROJEKT
#   Script:       05_es_loo.R
#   Forfatter:    Frederik Bender Bøeck-Nielsen
#   Dato:         06-12-2025
#   Beskrivelse:  Leave-one-out test for event-study modellerne
#                 1. Gemmer et plot for hver variabel, med ændringen i 2024 ATT.
#                 2. Opretter en samlet tabel for hver variabel, med post-
#                    treatment ATTs og pre-trend F-test p-værdier.
#
# ---------------------------------------------------------------------------- #


# 1. OPSÆTNING AF ARBEJDSMILJØ =================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

# indlæser pakker
library(conflicted) # håndtering af pakke konflikter
library(here) # robuste filstier
library(tidyverse) # data manipulation
library(fixest) # event-study modeller
library(broom)
library(glue)
library(gt)
library(gtsummary)
library(scales)

# håndterer konflikter
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# Input filstier
DIR_SCRIPTS <- here::here("scripts")
DIR_DATA <- here::here("data", "_processed")
ES_PANEL <- file.path(DIR_DATA, "es_panel.rds")

# Output filstier
DIR_TAB <- here::here("_output", "_tables", "_es_robustness")
DIR_FIG <- here::here("_output", "_figures", "_es_robustness")
if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

# Indlæser funktioner og brugerdefinerede temaer
source(file.path(DIR_SCRIPTS, "00_functions.R"))

# Sætter komma som decimal-tegn
options(OutDec = ",")


## 1.1. PARAMETRE --------------------------------------------------------------

# Definerer behandlingsår
TREAT_YEAR <- 2022

# Variabler
VARS_TO_TEST <- c("milex_usd_log", "milex_gdp")


# 2. DATAFORBEREDELSE ==========================================================
message("--- Sektion 2: Indlæser og forbereder data ---")

es_panel <- readRDS(ES_PANEL)
all_units_loo <- unique(es_panel$iso3c)

# Lookup table for gruppestatus
unit_info <- es_panel %>%
  distinct(iso3c, group, country_dan) %>%
  rename(dropped_unit = iso3c)


# 3. HELPER FUNCTION: RUN MAIN MODEL & EXTRACT ===============================
run_main_spec <- function(data, var_name) {
  # Main Model Formula (Group Trends + iso3c Cluster)
  fml <- as.formula(glue("{var_name} ~ i(event_time, treat_dummy, ref = c(-1, -8)) + i(treat_dummy, year, ref=0) | iso3c + year"))

  mod <- feols(fml, data = data, cluster = ~iso3c)

  # 1. Extract Estimates for Post-Treat Years
  res <- tidy(mod) %>%
    filter(term %in% c("event_time::0:treat_dummy", "event_time::1:treat_dummy", "event_time::2:treat_dummy")) %>%
    select(term, estimate, p.value) %>%
    mutate(year = case_when(
      grepl("::0:", term) ~ "att_2022",
      grepl("::1:", term) ~ "att_2023",
      grepl("::2:", term) ~ "att_2024"
    )) %>%
    select(year, estimate, p.value)

  # 2. Extract Pre-Trend F-Test
  coefs <- names(coef(mod))
  pre_terms <- coefs[grepl("event_time::", coefs) & grepl("-", coefs)]
  p_pre <- NA
  if (length(pre_terms) > 0) {
    ft <- try(wald(mod, pre_terms), silent = TRUE)
    if (!inherits(ft, "try-error")) p_pre <- ft$p[1]
  }

  # Pivot to wide format
  res_wide <- res %>%
    pivot_wider(names_from = year, values_from = c(estimate, p.value)) %>%
    mutate(pre_trend_p = p_pre)

  return(res_wide)
}


# 4. MAIN LOOP ===============================================================
message(paste("--- Starting LOO loop for", length(VARS_TO_TEST), "variables ---"))

for (VAR_TO_TEST in VARS_TO_TEST) {
  message(paste("\n--- Processing:", VAR_TO_TEST, "---"))

  # A. Run Baseline (Full Sample)
  baseline <- run_main_spec(es_panel, VAR_TO_TEST) %>%
    mutate(dropped_unit = "Baseline")

  # Capture Baseline 2024 estimate (for plot line)
  base_att_2024 <- baseline$estimate_att_2024

  # B. Run LOO Loop
  loo_res <- map_dfr(all_units_loo, function(unit) {
    dat_sub <- es_panel %>% filter(iso3c != unit)
    run_main_spec(dat_sub, VAR_TO_TEST) %>%
      mutate(dropped_unit = unit)
  })

  # C. Combine & Add Group Info
  final_df <- bind_rows(baseline, loo_res) %>%
    left_join(unit_info, by = "dropped_unit") %>%
    mutate(
      # If it's the Baseline row, keep "Baseline".
      # Otherwise, use country_dan (and fallback to ISO if missing).
      display_name = ifelse(dropped_unit == "Baseline",
        "Baseline",
        coalesce(country_dan, dropped_unit)
      ),

      # Determine group for coloring (Baseline logic remains)
      group = ifelse(dropped_unit == "Baseline", "Baseline", group)
    ) %>%
    # Use the new display_name as the main identifier moving forward
    mutate(dropped_unit = display_name) %>%
    select(-display_name, -country_dan) %>% # Clean up
    # --- NEW LOGIC END ---


    arrange(estimate_att_2024)

  # --- OUTPUT 1: THE TABLE ---

  table_data <- final_df %>%
    mutate(
      p_pre_fmt = gtsummary::style_pvalue(pre_trend_p, digits = 3),
      att_2022_fmt = glue("{round(estimate_att_2022, 3)} ({gtsummary::style_pvalue(p.value_att_2022, digits = 3)})"),
      att_2023_fmt = glue("{round(estimate_att_2023, 3)} ({gtsummary::style_pvalue(p.value_att_2023, digits = 3)})"),
      att_2024_fmt = glue("{round(estimate_att_2024, 3)} ({gtsummary::style_pvalue(p.value_att_2024, digits = 3)})")
    ) %>%
    select(dropped_unit, group, att_2024_fmt, att_2023_fmt, att_2022_fmt, p_pre_fmt)

  gt_tab <- gt(table_data) %>%
    cols_label(
      dropped_unit = "Fjernet Enhed",
      group = "Gruppe",
      att_2024_fmt = "ATT 2024",
      att_2023_fmt = "ATT 2023",
      att_2022_fmt = "ATT 2022",
      p_pre_fmt = "Pre-Trend F-Test (p)"
    ) %>%
    cols_align(align = "right", columns = 2:5) %>%
    theme_gt_bachelor_project() %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = dropped_unit == "Baseline")
    ) %>%
    tab_source_note("Sorteret efter 2024 ATT (stigende). ATT kolloner har p-værdier i parentes.")

  gtsave(gt_tab, file = file.path(DIR_TAB, glue("loo_table_{VAR_TO_TEST}.html")))


  # --- OUTPUT 2: THE PLOT ---

  # Prepare data: remove baseline for dots (it's a line), reorder for S-curve
  plot_df <- final_df %>%
    filter(dropped_unit != "Baseline") %>%
    mutate(dropped_unit = fct_reorder(dropped_unit, estimate_att_2024, .desc = TRUE))

  p_loo <- ggplot(plot_df, aes(x = estimate_att_2024, y = dropped_unit)) +
    geom_vline(xintercept = base_att_2024, color = "grey40", linetype = "dashed") +
    geom_point(aes(color = group), size = 2.5) +
    scale_color_project_qual(name = NULL) +
    labs(
      x = "2024 ATT",
      y = NULL,
      caption = "Stiplet linje indikerer reel 2024 ATT.\nPre-trend F-test er insignifikant (p > 0,05) og 2024 ATT er signifikant (p < 0,05) på tværs af samtlige udeladelser."
    ) +
    theme_bachelor_project() +
    theme(
      panel.grid.major.y = element_line(),
      legend.position = "bottom"
    )

  ggsave(file.path(DIR_FIG, glue("loo_plot_{VAR_TO_TEST}.png")), p_loo, width = 8, height = 6)
}


# 4. SCRIPT FÆRDIG =============================================================

message(paste(
  "\n--- Script 04_es_models.R færdigt ---",
  "\nAlle tabeller er gemt i:", DIR_TAB,
  "\nAlle figurer er gemt i:", DIR_FIG
))
