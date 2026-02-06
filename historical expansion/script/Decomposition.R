#######################################
#Purpose: Meter graphs
#Author: XIAOMING ZHANG
#Date: November 13th 2025
######################################################



pacman::p_load(knitr, lfe,fixest, modelsummary, stargazer, tidyverse, dplyr, here, sf, haven, ggplot2, readxl,  writexl, janitor, randomizr, RCT, purrr, RODBC, DBI)

getwd()


#Utility graph------


# 1. Make METER-LEVEL long usage (NOT aggregated)

utility_long_meter <- utility %>%
  pivot_longer(
    cols = matches("^\\d{4}_usage$"),
    names_to = "year",
    names_pattern = "(\\d{4})_usage",
    values_to = "usage"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  select(meter_id, village_id, year, usage)


# 2. Expand to all years per meter
wanted_years <- 2011:2022

utility_long_meter <- utility_long_meter %>%
  group_by(meter_id, village_id) %>%
  complete(year = wanted_years, fill = list(usage = 0)) %>%
  ungroup()


# 3. Join FE variables (they live in expansion_join)
utility_meter_with_FE <- utility_long_meter %>%
  left_join(
    expansion_join %>% 
      mutate(
        elec15_17 = electrified_year %in% c("2015","2016","2017"),
        elec12_14 = electrified_year %in% c("2012","2013","2014"),
        EARP      = earp_lv == 1 | earp_mv == 1
      ) %>%
      select(District, village_id, cell_id, primary_school, 
             cell_office, health_center, elec15_17, elec12_14, 
             EARP, electrified_year),
    by = "village_id"
  )


# 4. Apply filters (same as before)
utility_meter_filtered <- utility_meter_with_FE %>%
  filter(!District %in% c("Nyabihu","Ngororero","Nyamasheke","Rubavu")) %>%
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>%
  mutate(
    # usage = usage / (100 * 365),
    cell_id = substr(village_id, 1, 6),
    sector_id = substr(village_id, 1, 4),
    district_id = substr(village_id, 1, 2)
  )


# sum(utility_meter_filtered$usage[utility_meter_filtered$year == 2014])
# [1] 32328844
# > sum(utility_reg$usage[utility_reg$year == 2014])
# [1] 32328844
# > sum(utility_meter_filtered$usage[utility_meter_filtered$year == 2020])
# [1] 43571863
# > sum(utility_reg$usage[utility_reg$year == 2020])
# [1] 43571863

# 5. Pivot wide at METER LEVEL
utility_plot_wide <- utility_meter_filtered %>%
  mutate(year_label = paste0(year, "_usage")) %>%
  select(meter_id, village_id, year_label, usage,
         District, cell_id, primary_school, cell_office, health_center,
         elec15_17, elec12_14, EARP, electrified_year,
         sector_id, district_id) %>%
  pivot_wider(
    names_from = year_label,
    values_from = usage,
    values_fill = 0
  )

# Restrict to meters with positive use in BOTH 2014 and 2020
utility_pos_14_20 <- utility_plot_wide %>%
  filter(`2014_usage` > 0& 
         `2020_usage` > 0)

# Calculate averages
avg_stats <- utility_pos_14_20 %>%
  summarise(
    avg_2014_usage = mean(`2014_usage`, na.rm = TRUE),
    avg_2020_usage = mean(`2020_usage`, na.rm = TRUE),
    
    avg_log_2014_usage = mean(log(`2014_usage`), na.rm = TRUE),
    avg_log_2020_usage = mean(log(`2020_usage`), na.rm = TRUE),
    
    n_meters = n()
  )

avg_stats




#Decomposing------
# --- Define the transition years ---

t0 <- 2014
t1_years <- 2015:2022

compute_decomposition <- function(df, t0, t1) {
  
  # dynamic column names
  u0 <- sym(paste0(t0, "_usage"))
  u1 <- sym(paste0(t1, "_usage"))
  
  # classify meters
  classified <- df %>%
    transmute(
      usage_t0 = !!u0,
      usage_t1 = !!u1,
      stay =  (usage_t0 > 0 & usage_t1 > 0),
      join =  (usage_t0 == 0 & usage_t1 > 0),
      exit =  (usage_t0 > 0 & usage_t1 == 0)
    )
  
  # counts
  N_stay <- sum(classified$stay)
  N_join <- sum(classified$join)
  N_exit <- sum(classified$exit)
  
  # averages
  Xbar_stay_t0  <- mean(classified$usage_t0[classified$stay], na.rm = TRUE)
  Xbar_stay_t1  <- mean(classified$usage_t1[classified$stay], na.rm = TRUE)
  Xbar_join_t1  <- mean(classified$usage_t1[classified$join], na.rm = TRUE)
  Xbar_exit_t0  <- mean(classified$usage_t0[classified$exit], na.rm = TRUE)
  
  # growth ratio among stayers
  stay_ratios <- log(classified$usage_t1[classified$stay] /
    classified$usage_t0[classified$stay])
  ratio_bar <- mean(stay_ratios, na.rm = TRUE)
  
  # aggregate use
  X_t1 <- N_stay * Xbar_stay_t1 + N_join * Xbar_join_t1
  X_t0 <- N_stay * Xbar_stay_t0 + N_exit * Xbar_exit_t0
  
  # ---- Decomposition terms ----
  T1_joins <- log(1 + N_join / N_stay)
  
  T2_sel_join <- log(
    1 + (N_join / (N_stay + N_join)) *
      ( (Xbar_join_t1 / Xbar_stay_t1) - 1 )
  )
  
  T3_exits <- log(1 - N_exit / (N_stay + N_exit))
  
  T4_sel_exit <- -log(
    1 + (N_exit / (N_stay + N_exit)) *
      ( (Xbar_exit_t0 / Xbar_stay_t0) - 1 )
  )
  
  T5_growth <- ratio_bar
  
  T6_inequality <- log(Xbar_stay_t1 / Xbar_stay_t0) - ratio_bar
  
  total_log_change <- log(X_t1) - log(X_t0)
  
  tibble(
    t0 = t0,
    t1 = t1,
    N_stay = N_stay,
    N_join = N_join,
    N_exit = N_exit,
    T1_joins,
    T2_sel_join,
    T3_exits,
    T4_sel_exit,
    T5_growth,
    T6_inequality,
    total_log_change
  )
}



##No restriction-----
decomp_fullsample <- map_dfr(
  t1_years,
  ~ compute_decomposition(utility_plot_wide, t0 = 2014, t1 = .x)
)


plot_df <- decomp_fullsample %>%
  pivot_longer(
    cols = c(
      T1_joins,
      T2_sel_join,
      T3_exits,
      T4_sel_exit,
      T5_growth,
      T6_inequality,
      total_log_change
    ),
    names_to = "component",
    values_to = "value"
  )

component_labels <- c(
  T1_joins      = "Join: Quantity",
  T2_sel_join      = "Join: Selection",
  T3_exits     = "Exit: Quantity",
  T4_sel_exit      = "Exit: Selection",
  T5_growth    = "Growth (mean log use)",
  T6_inequality     = "Inequality (log gap)",
  total_log_change = "Total Change"
)

plot_df$component <- factor(plot_df$component,
                            levels = names(component_labels),
                            labels = component_labels)

p8 <- ggplot(plot_df, aes(x = t1, y = value, color = component)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    x = "Year (t1)",
    y = "Log contribution",
    color = "Component",
    title = "Full-Sample Decomposition of Electricity Use\n(t0 = 2014, national level)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

p8


ggsave(file.path(output_path, "plot","decomposition.png"),
       p8, width = 8, height = 5, dpi = 300)




##Elec12_14------


utility_plot_wide_12_14 <- utility_plot_wide %>% 
  filter(elec12_14 == TRUE)


decomp_12_14 <- map_dfr(
  t1_years,
  ~ compute_decomposition(utility_plot_wide_12_14, t0 = 2014, t1 = .x)
)


plot_df_12_14<- decomp_12_14 %>%
  pivot_longer(
    cols = c(
      T1_joins,
      T2_sel_join,
      T3_exits,
      T4_sel_exit,
      T5_growth,
      T6_inequality,
      total_log_change
    ),
    names_to = "component",
    values_to = "value"
  )

component_labels <- c(
  T1_joins      = "Join: Quantity",
  T2_sel_join      = "Join: Selection",
  T3_exits     = "Exit: Quantity",
  T4_sel_exit      = "Exit: Selection",
  T5_growth    = "Growth (mean log use)",
  T6_inequality     = "Inequality (log gap)",
  total_log_change = "Total Change"
)

plot_df_12_14$component <- factor(plot_df_12_14$component,
                            levels = names(component_labels),
                            labels = component_labels)

p8.1 <- ggplot(plot_df_12_14, aes(x = t1, y = value, color = component)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    x = "Year (t1)",
    y = "Log contribution",
    color = "Component",
    title = "Full-Sample Decomposition of Electricity Use electrified 2012-2014\n(t0 = 2014, national level)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

p8.1


ggsave(file.path(output_path, "plot","decomposition(2012-2014).png"),
       p8, width = 8, height = 5, dpi = 300)


