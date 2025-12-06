# ---------------------------------------------------------------------------- #
#
#   Projekt:      BACHELOR PROJEKT
#   Script:       02_es_models.R
#   Forfatter:    Frederik Bender Bøeck-Nielsen
#   Dato:         06-12-2025
#   Beskrivelse:  Event-study modeller
#                 1. Estimerer modeller (TWFE og TWFE + gruppetrends)
#                 2. Generer plots
#                 3. Gemmer resultater i tabeller
#                 4. Generer diagnostisk plot og tabel for at vurdere om
#                    gruppetendenser fungerer efter hensigten.
#
# ---------------------------------------------------------------------------- #


# 1. OPSÆTNING AF ARBEJDSMILJØ =================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

# indlæser pakker
library(conflicted) # håndtering af pakke konflikter
library(here) # robuste filstier
library(tidyverse) # data manipulation
library(fixest) # event-study modeller
library(modelsummary)
library(gt)
library(broom)
library(glue)
library(scales)
library(MASS) # simulering af uniforme konfidensintervaller
library(gtsummary)

# håndterer konflikter
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# Input filstier
DIR_SCRIPTS <- here::here("scripts")
DIR_DATA <- here::here("data", "_processed")
ES_PANEL <- file.path(DIR_DATA, "es_panel.rds")

# Output filstiler
DIR_TAB <- here::here("_output", "_tables", "_es_models")
DIR_FIG <- here::here("_output", "_figures", "_es_models")
if (!dir.exists(DIR_TAB)) dir.create(DIR_TAB, recursive = TRUE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

# Indlæser funktioner
source(file.path(DIR_SCRIPTS, "00_functions.R"))

# Sætter komma som decimal-tegn
options(OutDec = ",")

# Sætter seed for reproducerbarhed
set.seed(1234)


## 1.1. PARAMETRE --------------------------------------------------------------

# Definerer behandlingsår
TREAT_YEAR <- 2022

# Variabler
VARS_FOR_ANALYSIS <- c("milex_gdp", "milex_usd_log")

# Variabel labels
VAR_LABELS <- c(
  "milex_gdp"     = "Forsvarsudgifter (% BNP)",
  "milex_usd_log" = "Forsvarsudgifter (log US$)"
)

# Koefficient labels
COEF_LABELS <- c(
  "event_time::-8:treat_dummy" = "2014",
  "event_time::-7:treat_dummy" = "2015",
  "event_time::-6:treat_dummy" = "2016",
  "event_time::-5:treat_dummy" = "2017",
  "event_time::-4:treat_dummy" = "2018",
  "event_time::-3:treat_dummy" = "2019",
  "event_time::-2:treat_dummy" = "2020",
  "event_time::0:treat_dummy"  = "2022",
  "event_time::1:treat_dummy"  = "2023",
  "event_time::2:treat_dummy"  = "2024"
)

# Antal simulationer (uniforme konfidensintervaller)
N_SIMULATIONS <- 5000

# Konfidensniveau (uniforme konfidensintervaller)
CONF_LEVEL <- 0.95

# Definerer event-study modeller
model_specs <- list(
  # TWFE
  "base" = list(
    suffix      = "twfe",
    formula_str = "{var_name} ~ i(event_time, treat_dummy, ref = -1) | iso3c + year",
    cluster_fml = "~iso3c",
    ref_points  = -1
  ),
  # TWFE + gruppetrends
  "main" = list(
    suffix      = "group_trends",
    formula_str = "{var_name} ~ i(event_time, treat_dummy, ref = c(-1, -8)) + i(treat_dummy, year, ref=0) | iso3c + year",
    cluster_fml = "~iso3c",
    ref_points  = c(-1, -8)
  )
)


# 3. EVENT STUDY MODELLER ======================================================
message("--- Sektion 3: Estimerer event-study modeller ---")

es_panel <- readRDS(ES_PANEL)
labels_df <- enframe(COEF_LABELS, name = "term", value = "label")

# Ydre loop: Variabler
for (var_name in VARS_FOR_ANALYSIS) {
  message(paste("Behandler variabel:", var_name))
  # Indre loop: Model Specifikationer
  for (spec_name in names(model_specs)) {
    spec <- model_specs[[spec_name]]
    message(paste(spec$suffix))

    ## 3.1. ESTIMER MODEL ------------------------------------------------------
    model_es <- feols(
      as.formula(glue(spec$formula_str)),
      data    = es_panel,
      cluster = as.formula(spec$cluster_fml)
    )
    # Pre-trend F-test
    pre_terms <- grep("event_time::-", names(coef(model_es)), value = TRUE)
    p_pre <- wald(model_es, pre_terms)$p
    # Tidy data
    table_data <- tidy(model_es, conf.int = TRUE) %>%
      inner_join(labels_df, by = "term") %>%
      mutate(
        event_time_num = as.numeric(str_extract(term, "-?\\d+")),
        label          = factor(label, levels = unname(COEF_LABELS))
      ) %>%
      arrange(event_time_num)
    # Uniforme konfidensintervaller
    es_terms <- table_data$term
    vcov_mat <- vcov(model_es)[es_terms, es_terms]
    se_vec <- table_data$std.error
    sim_draws <- MASS::mvrnorm(
      n     = N_SIMULATIONS,
      mu    = rep(0, length(es_terms)),
      Sigma = vcov_mat
    )
    sim_t_stats <- sweep(abs(sim_draws), 2, se_vec, "/")
    crit_val_unif <- quantile(apply(sim_t_stats, 1, max), CONF_LEVEL)
    table_data <- table_data %>%
      mutate(
        uniform.low  = estimate - crit_val_unif * std.error,
        uniform.high = estimate + crit_val_unif * std.error
      )

    # 3.2. EVENT-STUDY PLOTS ---------------------------------------------------
    plot_breaks <- sort(unique(c(table_data$event_time_num, spec$ref_points)))
    label_map <- setNames(
      c(as.character(table_data$label), "2021", "2014"),
      c(table_data$event_time_num, -1, -8)
    )
    ref_val <- mean(
      es_panel[[var_name]][es_panel$event_time == -1 & es_panel$group == "Behandlet"],
      na.rm = TRUE
    )
    p_es <- ggplot(table_data, aes(x = event_time_num, y = estimate)) +
      geom_hline(yintercept = 0, color = "grey40") +
      geom_vline(xintercept = -1, linetype = "dashed", color = "grey40") +
      # Uniforme konfidensintervaller
      geom_ribbon(aes(ymin = uniform.low, ymax = uniform.high),
        alpha = 0.15, fill = "grey40"
      ) +
      geom_errorbar(aes(ymin = uniform.low, ymax = uniform.high), width = 0) +
      # Punktvise konfidensintervaller
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
        alpha = 0.30, fill = "grey40"
      ) +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
      geom_point() +
      scale_x_continuous(
        breaks = plot_breaks,
        labels = label_map[as.character(plot_breaks)]
      ) +
      scale_y_continuous(labels = function(b) {
        b[b == 0] <- glue("0 ({round(ref_val, 2)})")
        return(b)
      }) +
      labs(x = NULL, y = "ATT") +
      theme_bachelor_project()

    ggsave(
      file.path(DIR_FIG, glue("es_{spec$suffix}_plot_{var_name}_2.png")),
      p_es,
      width = 8, height = 5
    )

    # 3.3. EVENT-STUDY TABELLER ------------------------------------------------
    tbl_es <- table_data %>%
      select(label, estimate, std.error, conf.low, conf.high, p.value) %>%
      gt() %>%
      cols_label(
        label     = "År",
        estimate  = "ATT",
        std.error = "Std. fejl",
        conf.low  = "95% KI",
        p.value   = "p-værdi"
      ) %>%
      fmt_number(
        columns = c(estimate, std.error, conf.low, conf.high),
        decimals = 3, dec_mark = ",", sep_mark = "."
      ) %>%
      cols_merge(columns = c(conf.low, conf.high), pattern = "[{1}; {2}]") %>%
      tab_source_note(source_note = md(glue(
        "**Pre-Trend F-Test (p):** {style_pvalue(p_pre, digits=3)}<br>**Obs.:** {nobs(model_es)}"
      ))) %>%
      fmt(
        columns = p.value,
        fns = function(x) style_pvalue(x, digits = 3)
      ) %>%
      theme_gt_bachelor_project() %>%
      gtsave(file = file.path(DIR_TAB, glue("es_{spec$suffix}_{var_name}.html")))
  }
}


# 4. GRUPPETENDENSER DIAGNOSTIK ================================================
message("--- Sektion 4: Kører Trend Correction Diagnostics ---")

# --- A. PLOTTING LOOP (Kører per variabel) ------------------------------------

for (var_name in VARS_FOR_ANALYSIS) {
  var_label <- VAR_LABELS[var_name]
  message(glue("   Genererer plot for: {var_label}"))

  # 1. Estimer Model
  f_main <- as.formula(glue("{var_name} ~ i(event_time, treat_dummy, ref = c(-1, -8)) + i(treat_dummy, year, ref=0) | iso3c + year"))
  mod_main <- feols(f_main, data = es_panel, cluster = ~iso3c)

  # 2. Hent ATT koefficienter + Referencepunkter
  coef_data <- tidy(mod_main) %>%
    filter(grepl("event_time::", term)) %>%
    mutate(event_time = as.numeric(str_extract(term, "-?\\d+"))) %>%
    select(event_time, att = estimate) %>%
    add_row(event_time = c(-1, -8), att = 0) # Tilføj referencer manuelt

  # 3. Konstruer Plot Data (Baseline + ATT)
  plot_data <- es_panel %>%
    filter(group == "Kontrol") %>%
    summarise(
      Kontrol = mean(!!sym(var_name), na.rm = TRUE),
      .by = c(year, event_time)
    ) %>%
    left_join(coef_data, by = "event_time") %>%
    mutate(
      att = replace_na(att, 0),
      Behandlet = Kontrol + att
    ) %>%
    pivot_longer(c(Kontrol, Behandlet), names_to = "group", values_to = "mean_val")

  # 4. Generer Plot
  p_recon <- ggplot(plot_data, aes(x = year, y = mean_val, color = group)) +
    geom_vline(xintercept = TREAT_YEAR - 1, linetype = "dashed", color = "grey40") +
    geom_line() +
    scale_x_continuous(breaks = seq(2014, 2024, by = 2)) +
    scale_color_project_qual(name = NULL) +
    labs(
      y = var_label, x = NULL, color = NULL,
      caption = "Behandlet linje er konstrueret som: Kontrolgruppen + Event-study koefficienterne."
    ) +
    theme_bachelor_project()

  ggsave(
    file.path(DIR_FIG, glue("trend_correction_plot_{var_name}_2.png")),
    p_recon,
    width = 8, height = 6, bg = "white"
  )
}


# --- B. CONSOLIDATED TABLE (Kører én gang for alle) ---------------------------
message("   Genererer samlet tabel for trend corrections...")

# 1. Beregn hældninger og korrektioner
trend_table_data <- map_dfr(VARS_FOR_ANALYSIS, function(v_code) {
  # Estimer model for at hente hældningskoefficienten (slope)
  f_main <- as.formula(glue("{v_code} ~ i(event_time, treat_dummy, ref = c(-1, -8)) + i(treat_dummy, year, ref=0) | iso3c + year"))
  mod <- feols(f_main, data = es_panel, cluster = ~iso3c)

  # Udtræk hældning (Interaction mellem treat_dummy og year)
  slope <- coef(mod)[["treat_dummy::1:year"]]

  # Returner data for årene 2014-2024
  tibble(
    year = 2014:2024,
    var  = VAR_LABELS[v_code],
    corr = slope * (year - 2014)
  )
})

# 2. Formater og gem tabel
tbl_trend <- trend_table_data %>%
  pivot_wider(names_from = var, values_from = corr) %>%
  gt() %>%
  cols_label(year = "År") %>%
  fmt_number(columns = -year, decimals = 3, dec_mark = ",", sep_mark = ".") %>%
  tab_source_note(
    source_note = "Tabellen viser den isolerede årlige effekt af den gruppespecifikke lineære tidstendens."
  ) %>%
  theme_gt_bachelor_project() %>%
  gtsave(file = file.path(DIR_TAB, "trend_correction_combined_2.html"))


# 6. SCRIPT FÆRDIG =============================================================

message(paste(
  "\n--- Script 04_es_models.R færdigt ---",
  "\nAlle tabeller er gemt i:", DIR_TAB,
  "\nAlle figurer er gemt i:", DIR_FIG
))
