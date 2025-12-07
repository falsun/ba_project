# ---------------------------------------------------------------------------- #
#
#   Projekt:      BACHELOR PROJEKT
#   Script:       05_es_robust.R
#   Forfatter:    Frederik Bender Bøeck-Nielsen
#   Dato:         07-12-2025
#   Beskrivelse:  Leave-one-out og permutationstest (Fisher randomiseringstest)
#                 for event-study modellerne.
#                 1. Kører LOO test for de to afhængige variabler og gemmer
#                    plots og tabeller med resultaterne.
#                 2. Kører permutationstest for de to afhængige variabler og
#                    gemmer resultaterne i plots.
#
# ---------------------------------------------------------------------------- #


# 1. OPSÆTNING AF ARBEJDSMILJØ =================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

# indlæser pakker
library(conflicted) # håndtering af pakke konflikter
library(here) # robuste filstier
library(tidyverse) # data manipulation og visualiseringer
library(fixest) # event-study modeller
library(broom) # tidy model output
library(glue) # formatering af beskeder, labels og formler
library(gt) # tabelformatering
library(gtsummary) # tabeller og formatering af p-værider

# håndterer konflikter
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mean", "base")

# Input filstier
DIR_SCRIPTS <- here("scripts")
DIR_DATA <- here("data", "_processed")
ES_PANEL <- file.path(DIR_DATA, "es_panel.rds")

# Output filstier
DIR_TAB <- here("_output", "_tables", "_es_robust")
DIR_FIG <- here("_output", "_figures", "_es_robust")
if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

# Indlæser funktioner og brugerdefinerede temaer
source(file.path(DIR_SCRIPTS, "00_functions.R"))

# Sætter komma som decimal-tegn
options(OutDec = ",")


## 1.1. PARAMETRE --------------------------------------------------------------

# Variabler for Loo test
VARS_TO_TEST_LOO <- c(
  "milex_gdp"     = "Forsvarsudgifter (% BNP)",
  "milex_usd_log" = "Forsvarsudgifter (Log USD)"
)

# Variabler for Perm test
VARS_TO_TEST <- c("milex_usd_log", "milex_gdp")

# Sæt Seed for reproducerbarhed og angiv antal permutationer
SEED <- 1234
PERMS <- 5000


# 2. DATAFORBEREDELSE ==========================================================
message("--- Sektion 2: Indlæser og forbereder data ---")

# Indlæser event-study panel fra 01_data_prep.R
es_panel <- readRDS(ES_PANEL)

# For LOO test
all_countries <- unique(es_panel$country_dan)
group_lookup <- setNames(es_panel$group, es_panel$country_dan)

# For permutationstest
unit_list <- es_panel %>%
  distinct(iso3c, treat_dummy)


# 3. LEAVE-ONE-OUT TEST ========================================================
message(paste("--- Sektion 3: Kører leave-one-out test ---"))

## 3.1. HJÆLPEFUNKTION ---------------------------------------------------------
run_loo_spec <- function(data, var_name, dropped_name) {
  # Estimer Model
  fml <- as.formula(glue(
    "{var_name} ~ i(event_time, treat_dummy, ref = c(-1, -8)) + i(treat_dummy, year, ref=0) | country_dan + year"
  ))
  mod <- feols(fml, data = data, cluster = ~country_dan)
  # Pre-trend F-test
  pre_terms <- grep("event_time::-", names(coef(mod)), value = TRUE)
  p_pre <- wald(mod, pre_terms)$p
  # 2022-24 koefficienter
  res <- tidy(mod) %>%
    filter(term %in% c("event_time::0:treat_dummy", "event_time::1:treat_dummy", "event_time::2:treat_dummy")) %>%
    mutate(year = case_when(
      grepl("::0:", term) ~ "att_2022",
      grepl("::1:", term) ~ "att_2023",
      grepl("::2:", term) ~ "att_2024"
    )) %>%
    select(year, estimate, p.value) %>%
    pivot_wider(names_from = year, values_from = c(estimate, p.value))
  # Samler resultater
  res %>%
    mutate(dropped_unit = dropped_name, pre_trend_p = p_pre)
}



## 3.2. KØR LOO TEST -----------------------------------------------------------
for (var_code in names(VARS_TO_TEST_LOO)) {
  message(paste("Behandler", var_code))
  # Baseline model
  baseline_res <- run_loo_spec(es_panel, var_code, "Baseline") %>%
    mutate(group = "Baseline")
  # Gem 2024 baseline estimat til plot
  base_att_2024 <- baseline_res$estimate_att_2024

  ### 3.2.1. LOO LOOP ----
  loo_res <- map_dfr(all_countries, function(country) {
    dat_sub <- es_panel %>% filter(country_dan != country)
    run_loo_spec(dat_sub, var_code, country) %>%
      mutate(group = group_lookup[country])
  })
  # Saml data
  final_df <- bind_rows(baseline_res, loo_res) %>%
    arrange(estimate_att_2024)

  ## 3.2.2. LOO TABEL ----
  table_data <- final_df %>%
    mutate(
      p_pre_fmt    = style_pvalue(pre_trend_p, digits = 3),
      att_2022_fmt = glue("{round(estimate_att_2022, 3)} ({style_pvalue(p.value_att_2022, digits = 3)})"),
      att_2023_fmt = glue("{round(estimate_att_2023, 3)} ({style_pvalue(p.value_att_2023, digits = 3)})"),
      att_2024_fmt = glue("{round(estimate_att_2024, 3)} ({style_pvalue(p.value_att_2024, digits = 3)})")
    ) %>%
    select(dropped_unit, group, att_2024_fmt, att_2023_fmt, att_2022_fmt, p_pre_fmt)
  # Genererer resultattabel
  gt_tab <- gt(table_data) %>%
    cols_label(
      dropped_unit = "Fjernet Enhed",
      group        = "Gruppe",
      att_2024_fmt = "ATT 2024",
      att_2023_fmt = "ATT 2023",
      att_2022_fmt = "ATT 2022",
      p_pre_fmt    = "Pre-Trend F-Test (p)"
    ) %>%
    cols_align(align = "right", columns = 3:6) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = dropped_unit == "Baseline")
    ) %>%
    tab_source_note(
      "Sorteret efter 2024 ATT (stigende). ATT kolonner har p-værdier i parentes."
    ) %>%
    ba_theme_gt() %>%
    gtsave(file = file.path(DIR_TAB, glue("loo_table_{var_code}.html")))

  ## 3.2.3. LOO PLOT ----
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
    ba_theme() +
    theme(panel.grid.major.y = element_line(), legend.position = "bottom")
  # gem plot
  ggsave(
    file.path(DIR_FIG, glue("loo_plot_{var_code}.png")), p_loo,
    width = 8, height = 6
  )
}


# 4. PERMUTATIONSTEST ==========================================================
message(paste("--- Sektion 5: Kører permutationstest med", PERMS, "permutationer ---"))

for (VAR_TO_TEST in VARS_TO_TEST) {
  message(paste("Behandler", VAR_TO_TEST))
  # Baseline model
  fml_actual <- as.formula(glue("{VAR_TO_TEST} ~ i(event_time, treat_dummy, ref = c(-1, -8)) + i(treat_dummy, year, ref=0) | iso3c + year"))
  mod_actual <- feols(fml_actual, data = es_panel, cluster = ~iso3c)
  # Gennemsnitlig post-treatment ATT
  coef_names <- names(coef(mod_actual))
  post_terms <- coef_names[grepl("event_time::", coef_names) & !grepl("-", coef_names)]
  stat_actual <- NA
  if (length(post_terms) > 0) {
    actual_coefs <- coef(mod_actual)[post_terms]
    stat_actual <- mean(actual_coefs)
  }
  message(paste("Gns. post-treatment ATT for baseline model:", round(stat_actual, 3)))

  # 4.1. KØR PERMUTATIONER -----------------------------------------------------
  placebo_stats <- numeric(PERMS)
  set.seed(SEED)
  pb <- txtProgressBar(min = 0, max = PERMS, style = 3)
  for (i in 1:PERMS) {
    # Randomiserer behandlingsstatus
    unit_list_shuffled <- unit_list %>%
      mutate(treat_dummy_placebo = sample(treat_dummy)) %>%
      select(iso3c, treat_dummy_placebo)
    data_placebo <- es_panel %>%
      select(-treat_dummy) %>%
      left_join(unit_list_shuffled, by = "iso3c")
    # Estimer placebo model
    fml_placebo <- as.formula(glue("{VAR_TO_TEST} ~ i(event_time, treat_dummy_placebo, ref = c(-1, -8)) + i(treat_dummy_placebo, year, ref=0) | iso3c + year"))
    mod_placebo <- try(
      feols(fml_placebo, data = data_placebo, cluster = ~iso3c),
      silent = TRUE
    )
    if (inherits(mod_placebo, "try-error")) {
      placebo_stats[i] <- NA
    } else {
      # Gns. post-treatment placebo ATT
      p_coefs <- names(coef(mod_placebo))
      p_terms <- p_coefs[grepl("event_time::", p_coefs) & !grepl("-", p_coefs)]

      if (length(p_terms) > 0) {
        placebo_coefs <- coef(mod_placebo)[p_terms]
        placebo_stats[i] <- mean(placebo_coefs)
      } else {
        placebo_stats[i] <- NA
      }
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)

  # 4.2. TEST SIGNIFIKANS AF BASELINE MODEL ------------------------------------
  placebo_stats_clean <- na.omit(placebo_stats)
  n_clean <- length(placebo_stats_clean)
  # Beregner empirisk p-værdi (inkl. +1 korrektion for at undgå p = 0)
  p_val_frt <- (sum(placebo_stats_clean >= stat_actual) + 1) / (n_clean + 1)
  message(paste("Permutationstest p-værdi:", round(p_val_frt, 3)))

  # 4.3. GENERER PERMUTATION PLOTS ---------------------------------------------
  plot_df <- data.frame(stats = placebo_stats_clean)
  p_label <- paste("p =", style_pvalue(p_val_frt, digits = 3))
  p_frt <- ggplot(plot_df, aes(x = stats)) +
    geom_histogram(bins = 50, fill = "grey80", color = "white") +
    geom_vline(aes(xintercept = stat_actual),
      color = "grey40", linetype = "dashed"
    ) +
    annotate(
      "text",
      x = stat_actual,
      y = Inf,
      label = p_label,
      vjust = 2,
      hjust = 1.1,
      color = "black",
      family = "IBM Plex Serif"
    ) +
    labs(
      x = "Gennemsnitlig Post-Treatment ATT",
      y = "Frekvens",
      caption = "Distribuering under 5.000 tilfældige allokeringer.\nStiplet linje indikerer reel gennemsnitlig post-treatment ATT."
    ) +
    ba_theme()
  # Gem plot
  ggsave(file.path(DIR_FIG, glue("frt_plot_{VAR_TO_TEST}.png")), p_frt,
    width = 8, height = 5
  )
}


# 5. SCRIPT FÆRDIG =============================================================

message(paste(
  "\n--- Script 05_es_robust.R færdigt ---",
  "\nAlle tabeller er gemt i:", DIR_TAB,
  "\nAlle figurer er gemt i:", DIR_FIG
))
