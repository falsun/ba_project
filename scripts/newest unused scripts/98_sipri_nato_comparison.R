# ---------------------------------------------------------------------------- #
#   Script:       15_data_validation_sipri_vs_nato.R
#   Description:  Validates the switch from SIPRI to NATO data for Part 2.
#                 Checks correlation and visual alignment of trends.
# ---------------------------------------------------------------------------- #

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, glue, gt, patchworked)

# 1. LOAD DATA
# Assuming you have merged NATO data into your master_panel in Script 01
# or have a separate file. Let's assume master_panel has both cols now.
master_panel <- readRDS(here::here("data", "_processed", "master_panel.rds"))

# Filter for European NATO (Treatment Group)
df_comp <- master_panel %>%
  filter(group == "treatment") %>%
  select(iso3c, year,
         sipri_gdp = milex_gdp,      # The variable used in Part 1
         nato_gdp  = milex_gdp_nato  # The variable for Part 2 (includes 2025)
  ) %>%
  filter(year <= 2024) # Only compare overlapping years

# 2. STATISTICAL VALIDATION (Correlation)
# We check the correlation of the VALUES and the CHANGES (First Difference)

correlation_levels <- cor(df_comp$sipri_gdp, df_comp$nato_gdp, use = "complete.obs")

# Calculate annual changes to see if they "move" together
df_changes <- df_comp %>%
  group_by(iso3c) %>%
  mutate(
    d_sipri = sipri_gdp - lag(sipri_gdp),
    d_nato  = nato_gdp - lag(nato_gdp)
  ) %>%
  ungroup()

correlation_changes <- cor(df_changes$d_sipri, df_changes$d_nato, use = "complete.obs")

# Print Result
message(glue("Correlation (Levels): {round(correlation_levels, 4)}"))
message(glue("Correlation (Changes): {round(correlation_changes, 4)}"))

# If Levels > 0.95 and Changes > 0.80, you are safe.


# 3. VISUAL VALIDATION (The "Bridge Plot")
# We plot the aggregate average of both sources over time

df_plot <- df_comp %>%
  group_by(year) %>%
  summarise(
    SIPRI = mean(sipri_gdp, na.rm = TRUE),
    NATO  = mean(nato_gdp, na.rm = TRUE)
  ) %>%
  pivot_longer(-year, names_to = "Source", values_to = "Spending_GDP")

p_bridge <- ggplot(df_plot, aes(x = year, y = Spending_GDP, color = Source)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("NATO" = "#2c3e50", "SIPRI" = "#c0392b")) +
  scale_x_continuous(breaks = 2014:2024) +
  labs(
    title = "Data Validation: SIPRI vs. NATO Reporting",
    subtitle = glue("Correlation of Levels: r = {round(correlation_levels, 3)}"),
    y = "Average Military Spending (% GDP)",
    x = "Year",
    caption = "Comparison of European NATO treatment group averages."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save
ggsave(here::here("_output", "_figures", "validation_sipri_vs_nato.png"), p_bridge, width = 8, height = 5, bg = "white")

# 4. EXTENSION VISUALIZATION
# Show how adding 2025 (NATO) extends the trend
# (Requires your NATO data to actually have 2025)

df_nato_full <- master_panel %>%
  filter(group == "treatment") %>%
  select(year, milex_gdp_nato) %>%
  group_by(year) %>%
  summarise(NATO_Full = mean(milex_gdp_nato, na.rm = TRUE))

p_extend <- ggplot() +
  # Historical Overlap
  geom_line(data = df_plot, aes(x = year, y = Spending_GDP, color = Source), size = 1, alpha = 0.6) +
  # The 2025 Extension
  geom_line(data = df_nato_full %>% filter(year >= 2024),
            aes(x = year, y = NATO_Full), color = "#2c3e50", linetype = "dashed", size = 1.2) +
  geom_point(data = df_nato_full %>% filter(year == 2025),
             aes(x = year, y = NATO_Full), color = "#2c3e50", size = 4) +
  annotate("text", x = 2025, y = df_nato_full$NATO_Full[df_nato_full$year==2025],
           label = "2025 Projection", vjust = -1.5) +
  labs(
    title = "The Value of 2025 Data",
    subtitle = "Including 2025 captures the continued upward trajectory.",
    y = "% GDP"
  ) +
  theme_minimal()

ggsave(here::here("_output", "_figures", "validation_nato_extension.png"), p_extend, width = 8, height = 5, bg = "white")
