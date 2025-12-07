# ---------------------------------------------------------------------------- #
#
#   Projekt:      BACHELOR PROJEKT
#   Script:       03_es_prelims.R
#   Forfatter:    Frederik Bender Bøeck-Nielsen
#   Dato:         07-12-2025
#   Beskrivelse:  Forudsætningstests for event-study modeller.
#                 1. Tester validiteten af kontrolgruppen (placebo-in-space).
#                 2. Tester for tværsnitsafhængighed (Pesaran's CD).
#                 3. Tester for stationaritet (Levin-Lin-Chu).
#
# ---------------------------------------------------------------------------- #


# 1. OPSÆTNING AF ARBEJDSMILJØ =================================================
message("--- Sektion 1: Opsætter arbejdsmiljø ---")

# indlæser pakker
library(conflicted) # håndtering af pakke konflikter
library(here) # robuste filstier
library(tidyverse) # data manipulation og visualiseringer
library(fixest) # event-study modeller
library(plm) # CSD og LLC test
library(broom) # tidy model output
library(gtsummary) # tabel og formatering af p-værdier
library(gt) # formatér tabel
library(glue) # formatér beskeder, labels og formler

# håndterer konflikter
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("lag", "dplyr")

# Input filstier
DIR_SCRIPTS <- here("scripts")
DIR_DATA <- here("data", "_processed")
ES_PANEL <- file.path(DIR_DATA, "es_panel.rds")

# Output filstier
DIR_TAB <- here("_output", "_tables", "_es_prelims")
DIR_FIG <- here("_output", "_figures", "_es_prelims")
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
VARS_TO_TEST <- c(
  "milex_gdp"     = "Forsvarsudgifter (% BNP)",
  "milex_usd_log" = "Forsvarsudgifter (log USD)"
)


# 2. DATAFORBEREDELSE ==========================================================
message("--- Sektion 2: Indlæser og forbereder data ---")

# Indlæser event-study panel
es_panel <- readRDS(ES_PANEL)

# Filtrerer for pre-treatment perioden (for CSD og LCC tests)
pre_treat_df <- es_panel %>%
  filter(year < TREAT_YEAR)

# Filtrerer data for placebo-in-space test
control_ids <- es_panel %>%
  filter(group == "Kontrol") %>%
  distinct(country_dan) %>%
  pull(country_dan)

control_data <- es_panel %>%
  filter(country_dan %in% control_ids)


# 3. PLACEBO-IN-SPACE TEST =====================================================
message("--- Sektion 3: Estimerer placebo-in-space modeller ---")

# Loop til at køre testen for både forsvarsudgifter (% BNP) og (USD)
for (var_code in names(VARS_TO_TEST)) {
  var_label <- VARS_TO_TEST[[var_code]]
  message(paste("Kører placebo-in-space test for", var_label))
  plot_data_control <- map_dfr(control_ids, function(placebo_country) {
    # Tildeler placebo treatment
    data_loop <- control_data %>%
      mutate(treat_dummy_placebo = +(country_dan == placebo_country))
    # Estimerer event-study model med TWFE (vcov = "iid" grundet lille N)
    f_placebo <- as.formula(glue(
      "{var_code} ~ i(event_time, treat_dummy_placebo, ref = -1) | country_dan + year"
    ))
    mod_placebo <- feols(f_placebo, data = data_loop, vcov = "iid")
    # Pre-Trend F-Test (parallelle trends)
    pre_terms <- grep("event_time::-", names(coef(mod_placebo)), value = TRUE)
    p_str <- style_pvalue(wald(mod_placebo, pre_terms)$p, digits = 3)
    # Output til plotting
    tidy(mod_placebo, conf.int = TRUE) %>%
      filter(grepl("event_time::", term)) %>%
      mutate(
        country = glue("{placebo_country} ({p_str})"),
        event_time_num = as.numeric(str_extract(term, "-?\\d+"))
      )
  })

  # Samler alle placebo event-study plots it ét faceted plot
  p_control <- ggplot(plot_data_control, aes(x = event_time_num, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "grey40") +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0,
      color = "black"
    ) +
    geom_point(color = "black") +
    facet_wrap(~country, scales = "free_y", strip.position = "bottom") +
    coord_cartesian(clip = "off") +
    labs(
      x = NULL,
      y = "Placebo ATT",
      caption = "tal i parentes er pre-trend F-test p-værdier."
    ) +
    ba_theme() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 9, color = "black"),
      strip.text = element_text(hjust = 0.5),
      plot.margin = margin(t = 5)
    )

  ggsave(
    file.path(DIR_FIG, glue("placebo_control_plot_{var_code}.png")), p_control,
    width = 10, height = 7
  )
}


# 4. TVÆRSNITSAFHÆNGIGHEDSTEST (PESARAN'S CD) ==================================
# Kører Pesaran's CD test på en simpel TWFE model og en TWFE + gruppetrends
# model - for både forsvarsudgifter (% BNP) og forsvarsudgifter (USD). Udtrækker
# p-værdierne så de efterfølgende kan samles i en tabel.
message("--- Sektion 4: Tester for tværsnitsafhængighed (Pesaran's CD) ---")

cd_comparison_results <- map_dfr(names(VARS_TO_TEST), function(var_code) {
  data_clean <- pre_treat_df %>%
    select(iso3c, year, treat_dummy, all_of(var_code))
  get_cd_pval <- function(f) {
    mod <- feols(as.formula(f), data = data_clean)
    pd <- pdata.frame(
      data_clean %>%
        mutate(r = residuals(mod)),
      index = c("iso3c", "year")
    )
    pcdtest(r ~ 1, data = pd)$p.value
  }
  tibble(
    variable = var_code,
    # TWFE
    p_val_base = get_cd_pval(glue("{var_code} ~ 1 | iso3c + year")),
    # TWFE + gruppetrends
    p_val_main = get_cd_pval(glue(
      "{var_code} ~ i(treat_dummy, year, ref=0) | iso3c + year"
    ))
  )
})

# Generer tabel med p-værdier fra CSD test
tbl_cd_combined <- cd_comparison_results %>%
  mutate(variable = VARS_TO_TEST[variable]) %>%
  gt() %>%
  cols_label(
    variable = "",
    p_val_base = "TWFE (p)",
    p_val_main = "TWFE + gruppetendenser (p)"
  ) %>%
  fmt(
    columns = starts_with("p_val"),
    fns = function(x) style_pvalue(x, digits = 3)
  ) %>%
  cols_align(align = "right", columns = starts_with("p_val")) %>%
  tab_footnote(
    locations = cells_column_labels(columns = p_val_main),
    footnote = "Inkluderer en gruppespecifik lineær tidstendens for behandlingsgruppen."
  ) %>%
  tab_source_note(
    source_note = "H₀: Tværsnitsuafhængighed. En p-værdi < 0,05 indikerer tværsnitsafhængighed."
  ) %>%
  ba_theme_gt() %>%
  gtsave(file = file.path(DIR_TAB, "csd_test.html"))


# 5. STATIONARITETSTEST (LEVIN-LIN-CHU) ========================================
message("--- Sektion 5: Tester for stationaritet (Levin-Lin-Chu) ---")

pur_test_results <- map_dfr(names(VARS_TO_TEST), function(var_code) {
  p_data_bal <- pre_treat_df %>%
    select(iso3c, year, all_of(var_code)) %>%
    pdata.frame(index = c("iso3c", "year")) %>%
    make.pbalanced(balance.type = "shared.individuals")
  map_dfr(
    c("intercept", "trend"),
    function(spec_type) {
      test_res <- purtest(
        object = p_data_bal[[var_code]],
        test   = "levinlin",
        exo    = spec_type,
        lags   = 0
      )
      tibble(
        variable = var_code,
        spec     = spec_type,
        p.value  = as.numeric(test_res$statistic$p.value)
      )
    }
  )
})

# Generer tabel med p-værdier fra LLC test
tbl_pur_clean <- pur_test_results %>%
  pivot_wider(names_from = spec, values_from = p.value, names_prefix = "p_") %>%
  mutate(variable = VARS_TO_TEST[variable]) %>%
  gt() %>%
  cols_label(
    variable = "",
    p_intercept = "Konstantled (p)",
    p_trend = "Konstantled + tidstendens (p)"
  ) %>%
  fmt(
    columns = starts_with("p_"),
    fns = function(x) style_pvalue(x, digits = 3)
  ) %>%
  cols_align(align = "right", columns = starts_with("p_val")) %>%
  tab_source_note(
    source_note = "H₀: Serierne er ikke-stationære (har enhedsrødder). En p-værdi < 0,05 indikerer stationaritet."
  ) %>%
  ba_theme_gt() %>%
  gtsave(file = file.path(DIR_TAB, "llc_test.html"))


# 6. SCRIPT FÆRDIG =============================================================

message(paste(
  "\n--- Script 03_es_prelims.R færdigt ---",
  "\nAlle tabeller er gemt i:", DIR_TAB,
  "\nAlle figurer er gemt i:", DIR_FIG
))
