#######################################
#Purpose: Meter graphs
#Author: XIAOMING ZHANG
#Date: November 13th 2025
######################################################



pacman::p_load(knitr, lfe,fixest, modelsummary, stargazer, tidyverse, dplyr, here, sf, haven, ggplot2, readxl,  writexl, janitor, randomizr, RCT, purrr, RODBC, DBI)

getwd()


#Utility graph------
usage_cols <- grep("_usage$", names(utility), value = TRUE)
usage_years <- as.integer(sub("_usage", "", usage_cols))

utility_plot <- utility %>%
  mutate(
    # matrix of usage values
    usage_mat = as.matrix(select(., all_of(usage_cols))),
    
    # first usage year (vectorized)
    first_usage_year = usage_years[max.col(usage_mat > 0, ties.method = "first")],
    
    # if a row has no positive usage, replace with NA
    first_usage_year = ifelse(rowSums(usage_mat > 0) == 0, NA, first_usage_year),
    
    # electrified year
    electrified_year = pmin(meter_installed_year, first_usage_year, na.rm = TRUE)
  ) %>%
  select(-usage_mat) %>% 
  filter(electrified_year >= 2012 & electrified_year <= 2014)


#New dataset------



utility_long  <- utility %>% 
  pivot_longer(
    cols = matches("^\\d{4}_usage$"),
    names_to = "year",
    names_pattern = "(\\d{4})_usage",
    values_to = "usage"
  ) %>% 
  group_by(village_id, year) %>% 
  summarise(usage = sum(usage, na.rm = TRUE), .groups = "drop") %>% 
  select(village_id, year, usage) %>% 
  mutate(year = as.numeric(year))

utility_long_join <- utility_long 



expansion_utility_join <- expansion_join %>% 
  mutate(
    `elec15_17` = ifelse(electrified_year %in% c("2015", "2016", "2017"), 1, 0),
    `elec12_14` = ifelse(electrified_year %in% c("2012", "2013", "2014"), 1, 0),
    `EARP` = ifelse(earp_lv == 1 | earp_mv == 1, 1, 0)
  ) %>% 
  select(District, village_id, cell_id, primary_school, cell_office, health_center, elec15_17, elec12_14, EARP, electrified_year)



wanted_years <- c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)

utility_plot <- expansion_utility_join %>% 
  full_join(utility_long_join, by = "village_id") %>%
  group_by(village_id) %>%
  complete(year = wanted_years) %>%
  fill(District, cell_id, primary_school, cell_office, health_center,
       elec15_17, elec12_14, EARP, electrified_year, .direction = "downup") %>% 
  filter(!is.na(year)) %>%
  mutate(usage = ifelse(is.na(usage), 0, usage)) %>%
  ungroup()




utility_plot <- utility_reg %>%   
  filter(! District %in% c("Nyabihu", "Ngororero", "Nyamasheke", "Rubavu")) %>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>% 
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    cell_id    = substr(village_id, 1, 6),
    sector_id  = substr(village_id, 1, 4),
    district_id= substr(village_id, 1, 2)
  ) %>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>% 
  mutate(
    usage = ifelse(is.na(usage), 0, usage),
    usage = usage/ (100*365)
  ) 






##Log10 scale-------



qs <- seq(0.01, 0.99, 0.01)

p1 <- utility_plot %>%
  select(meter_id, `2010_usage`:`2022_usage`) %>%
  gather(var, val, -meter_id) %>%
  mutate(year = as.numeric(substr(var, 1, 4))) %>%
  select(meter_id, year, usage = val) %>%
  filter(usage > 0) %>%
  
  group_by(year) %>%
  reframe(
    q = qs,
    usage = quantile(usage, probs = qs)
  ) %>%
  ungroup() %>%
  mutate(year = factor(year)) %>%
  
  ggplot(aes(x = q, y = usage, color = year, group = year)) +
  geom_line(linewidth = 1) +
  scale_y_log10() +
  labs(
    title = "Usage Quantile Curves (Log Scale, Year as Factor)",
    x = "Quantile",
    y = "Usage (log scale)"
  ) +
  theme_minimal(base_size = 14)

p1

ggsave(file.path(output_path, "plot","utility_quantile_log10.png"),
       p1, width = 8, height = 5, dpi = 300)


#Log10 scale four years-----
qs <- seq(0.01, 0.99, 0.01)

p2 <- utility_plot %>%
  group_by(province, district, sector, cell, village) %>%
  ungroup() %>%
  
  select(meter_id, `2010_usage`:`2022_usage`) %>%
  gather(var, val, -meter_id) %>%
  mutate(year = as.numeric(substr(var, 1, 4))) %>%
  select(meter_id, year, usage = val) %>%
  filter(usage > 0) %>%  # keep only positive usage
  
  # compute quantile curves for each year
  group_by(year) %>%
  reframe(
    q = qs,
    usage = quantile(usage, probs = qs)
  ) %>%
  ungroup() %>%
  
  # keep years 2012, 2015, 2018, 2021...
  filter(year >= 2012 & year %% 3 == 0) %>%
  mutate(year = factor(year)) %>%
  
  # plot quantile curves
  ggplot(aes(x = q, y = usage, color = year, group = year)) +
  geom_line(linewidth = 1) +
  scale_y_log10() +
  labs(
    title = "Usage Quantile Curves (Log Scale, four years)",
    x = "Quantile",
    y = "Usage (log scale)"
  ) +
  theme_minimal(base_size = 14)

p2

ggsave(file.path(output_path, "plot","utility_quantile_log10(4years).png"),
       p2, width = 8, height = 5, dpi = 300)

# Year >= 2012----



qs <- seq(0.01, 0.99, 0.01)

p3 <- utility_plot %>%
  group_by(province, district, sector, cell, village) %>%
  ungroup() %>%
  
  select(meter_id, `2010_usage`:`2022_usage`) %>%
  gather(var, val, -meter_id) %>%
  mutate(year = as.numeric(substr(var, 1, 4))) %>%
  select(meter_id, year, usage = val) %>%
  filter(usage > 0) %>%
  
  # compute quantiles for each year
  group_by(year) %>%
  reframe(
    q = qs,
    usage = quantile(usage, probs = qs)
  ) %>%
  ungroup() %>%
  
  # keep all years from 2012 onward
  filter(year >= 2012) %>%
  mutate(year = factor(year)) %>%
  
  # plot
  ggplot(aes(x = q, y = usage, color = year, group = year)) +
  geom_line(linewidth = 1) +
  scale_y_log10() +
  labs(
    title = "Usage Quantile Curves (All Years ≥ 2012)",
    x = "Quantile",
    y = "Usage (log scale)"
  ) +
  theme_minimal(base_size = 14)


p3

ggsave(file.path(output_path, "plot","utility_quantile_log10(greater than 2012).png"),
       p3, width = 8, height = 5, dpi = 300)


#Zero filled------


qs <- seq(0.01, 0.99, 0.01)

p4 <- utility %>%
  group_by(province, district, sector, cell, village) %>%
  ungroup() %>%
  
  select(meter_id, `2010_usage`:`2022_usage`) %>%
  gather(var, val, -meter_id) %>%
  mutate(year = as.numeric(substr(var, 1, 4))) %>%
  select(meter_id, year, usage = val) %>%
  filter(usage > 0) %>%
  
  # fill in missing year–meter combinations with usage = 0
  complete(meter_id, year, fill = list(usage = 0)) %>%
  
  # compute quantile curves for each year
  group_by(year) %>%
  reframe(
    q = qs,
    usage = quantile(usage, probs = qs)
  ) %>%
  ungroup() %>%
  
  # keep only years >= 2012
  filter(year >= 2012) %>%
  mutate(year = factor(year)) %>%
  
  # plot
  ggplot(aes(x = q, y = usage, color = year, group = year)) +
  geom_line(linewidth = 1) +
  scale_y_log10() +
  labs(
    title = "Usage Quantile Curves (Zero-Filled)",
    x = "Quantile",
    y = "Usage (log scale)"
  ) +
  theme_minimal(base_size = 14)

p4

ggsave(file.path(output_path, "plot","utility_quantile_log10(zero_filled).png"),
       p4, width = 8, height = 5, dpi = 300)


#Anchoring to 2012-----


qs <- seq(0.01, 0.99, 0.01)

df <- utility_plot %>%
  group_by(province, district, sector, cell, village) %>%
  filter(
    min(meter_installed_year, na.rm = TRUE) >= 2012 &
      min(meter_installed_year, na.rm = TRUE) <= 2014
  ) %>%
  ungroup() %>%
  
  select(meter_id, `2010_usage`:`2022_usage`) %>%
  gather(var, val, -meter_id) %>%
  mutate(year = as.numeric(substr(var, 1, 4))) %>%
  select(meter_id, year, usage = val) %>%
  filter(usage > 0) %>%
  
  group_by(year) %>%
  reframe(
    q = qs,
    usage = quantile(usage, probs = qs)
  ) %>%
  ungroup()

# ---- Compute proper 2012 baseline 
df_norm <- df %>%
  filter(year %in% c(2012, 2015, 2018, 2021)) %>%   # Or whatever years you want
  mutate(year = factor(year)) %>%
  group_by(q) %>%
  mutate(dusage = usage / usage[year == 2012]) %>%
  ungroup() %>%
  filter(year != 2012)

# ---- Plot 
p5 <- ggplot(df_norm, aes(q, dusage, color = year, group = year)) +
  geom_line(linewidth = 1) +
  scale_y_log10() +
  labs(
    title = "Usage Quantile Ratios Relative to 2012",
    x = "Quantile",
    y = "Usage Ratio (log scale)"
  ) +
  theme_minimal(base_size = 14)


p5

ggsave(file.path(output_path, "plot","utility_quantile_log10(anchored 2012).png"),
       p5, width = 8, height = 5, dpi = 300)


# ---- Plot 
p6 <- ggplot(df_norm, aes(q, dusage, color = year, group = year)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Usage Quantile Ratios Relative to 2012",
    x = "Quantile",
    y = "Usage Ratio (level scale)"
  ) +
  theme_minimal(base_size = 14)


p6

ggsave(file.path(output_path, "plot","utility_quantile_level(anchored 2012).png"),
       p6, width = 8, height = 5, dpi = 300)



#Lorenz curve-------
# 
# # choose the usage variable you want to analyze
# usage_var <- "2020_usage"   # change this to any year
# 
# lorenz_df <- utility %>%
#   filter(!is.na(.data[[usage_var]]), .data[[usage_var]] > 0) %>%  # remove missing and zero
#   arrange(.data[[usage_var]]) %>%                                # sort low → high
#   mutate(
#     n = n(),
#     p = row_number() / n,                                        # cumulative share of meters
#     cum_usage = cumsum(.data[[usage_var]]),                      # cumulative usage
#     Lp = cum_usage / sum(.data[[usage_var]])                     # cumulative share of usage
#   )
# 
# p7 <- ggplot(lorenz_df, aes(x = p, y = Lp)) +
#   geom_line(linewidth = 1.2, color = "steelblue") +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
#   labs(
#     title = paste("Lorenz Curve for", usage_var),
#     x = "Cumulative share of meters",
#     y = "Cumulative share of usage"
#   ) +
#   theme_minimal(base_size = 14)
# 
# 
# 
# p7
# 
# ggsave(file.path(output_path, "plot","lorenz_curve.png"),
#        p6, width = 8, height = 5, dpi = 300)
# 


# Vector of usage years available
usage_years <- 2010:2022

utility_decomp <- utility %>% 
  rowwise() %>%
  mutate(
    # Store usage vector as a list-column
    usage_vec = list(c_across(paste0(usage_years, "_usage"))),
    
    # First year where usage > 0
    first_usage_year = {
      uv <- unlist(usage_vec)
      idx <- which(uv > 0)[1]
      if (!is.na(idx)) usage_years[idx] else NA
    },
    
    # First electrification = min(installed year, first year with usage > 0)
    first_year_elec = min(c(meter_installed_year, first_usage_year), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(-usage_vec)

utility.1 <- utility_decomp %>% 
  filter(first_year_elec >= 2012 & first_year_elec <= 2014)





##All years-----


# Identify all usage columns (e.g., 2011_usage ... 2022_usage)
usage_cols <- grep("_usage$", names(utility), value = TRUE)

# Extract numeric years
usage_years <- as.integer(str_remove(usage_cols, "_usage"))

# Function to compute Lorenz curve for one year
make_lorenz <- function(df, usage_var) {
  df %>%
    filter(!is.na(.data[[usage_var]]), .data[[usage_var]] > 0) %>%
    arrange(.data[[usage_var]]) %>%
    mutate(
      year = as.integer(str_remove(usage_var, "_usage")),
      n = n(),
      p = row_number() / n,
      cum_usage = cumsum(.data[[usage_var]]),
      Lp = cum_usage / sum(.data[[usage_var]])
    ) %>%
    select(year, p, Lp)
}

# Build Lorenz curves for all years
lorenz_all <- map_dfr(usage_cols, ~ make_lorenz(utility, .x))


p7 <- ggplot(lorenz_all, aes(x = p, y = Lp, color = factor(year), group = year)) +
  geom_line(linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Lorenz Curves for Electricity Usage (All Years)",
    x = "Cumulative Share of Meters",
    y = "Cumulative Share of Usage",
    color = "Year"
  ) +
  theme_minimal(base_size = 14)

p7

ggsave(file.path(output_path, "plot","lorenz_curve.png"),
       p7, width = 8, height = 5, dpi = 300)



lorenz_all.1 <- map_dfr(usage_cols, ~ make_lorenz(utility.1, .x))

p7.1 <- ggplot(lorenz_all.1, aes(x = p, y = Lp, color = factor(year), group = year)) +
  geom_line(linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Lorenz Curves for Electricity Usage (All Years, electrified 2012-2014)",
    x = "Cumulative Share of Meters",
    y = "Cumulative Share of Usage",
    color = "Year"
  ) +
  theme_minimal(base_size = 14)

p7.1


ggsave(file.path(output_path, "plot","lorenz_curve(12_14).png"),
       p7.1, width = 8, height = 5, dpi = 300)





#Decomposing------


##Full sample-----


decompose_one_fullsample <- function(df, t0, t1) {
  
  # dynamic column names
  u0 <- sym(paste0(t0, "_usage"))
  u1 <- sym(paste0(t1, "_usage"))
  
  # classify each meter
  classified <- df %>%
    transmute(
      usage_t0 = !!u0,
      usage_t1 = !!u1,
      pos_t0   = usage_t0 > 0,
      pos_t1   = usage_t1 > 0,
      
      stay =  pos_t0 & pos_t1,
      join = !pos_t0 & pos_t1,
      exit =  pos_t0 & !pos_t1
    )
  
  # --- Compute global (full sample) aggregates ---
  N_stay <- sum(classified$stay)
  N_join <- sum(classified$join)
  N_exit <- sum(classified$exit)
  
  Xbar_stay_t0 <- mean(classified$usage_t0[classified$stay], na.rm = TRUE)
  Xbar_stay_t1 <- mean(classified$usage_t1[classified$stay], na.rm = TRUE)
  Xbar_join_t1 <- mean(classified$usage_t1[classified$join], na.rm = TRUE)
  Xbar_exit_t0 <- mean(classified$usage_t0[classified$exit], na.rm = TRUE)
  
  X_t0 <- sum(classified$usage_t0, na.rm = TRUE)
  X_t1 <- sum(classified$usage_t1, na.rm = TRUE)
  
  # ---- NEW logbar values (mean of individual logs) ----
  logbar_t0 <- mean(log(classified$usage_t0[classified$stay]), na.rm = TRUE)
  logbar_t1 <- mean(log(classified$usage_t1[classified$stay]), na.rm = TRUE)
  
  # ---- Construct the updated  ----
  T5_growth_new <- logbar_t1 - logbar_t0
  
  T6_ineq_new <- (log(Xbar_stay_t1) - log(Xbar_stay_t0)) -
    (logbar_t1 - logbar_t0)
  
  # ---- All other terms identical ----
  T1_join_qty <- log(1 + N_join / N_stay)
  
  T2_join_sel <- log(1 + (N_join / (N_stay + N_join)) *
                       (Xbar_join_t1 / Xbar_stay_t1 - 1))
  
  T3_exit_qty <- log(1 - (N_exit / (N_stay + N_exit)))
  
  T4_exit_sel <- -log(1 + (N_exit / (N_stay + N_exit)) *
                        (Xbar_exit_t0 / Xbar_stay_t0 - 1))
  
  total_log_change <- log(X_t1) - log(X_t0)
  
  tibble(
    t0 = t0,
    t1 = t1,
    N_stay, N_join, N_exit,
    
    T1_join_qty,
    T2_join_sel,
    T3_exit_qty,
    T4_exit_sel,
    
    T5_growth_new,
    T6_ineq_new,
    
    total_log_change
  )
}



#elec12_14---------


utility_decomp <- utility %>% 
  rowwise() %>%
  mutate(
    # Store usage vector as a list-column
    usage_vec = list(c_across(paste0(usage_years, "_usage"))),
    
    # First year where usage > 0
    first_usage_year = {
      uv <- unlist(usage_vec)
      idx <- which(uv > 0)[1]
      if (!is.na(idx)) usage_years[idx] else NA
    },
    
    # First electrification = min(installed year, first year with usage > 0)
    first_year_elec = min(c(meter_installed_year, first_usage_year), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(-usage_vec)

utility.1 <- utility_decomp %>% 
  filter(first_year_elec %in% c("2012", "2013", "2014") | first_year_elec == 9999) %>% 
  filter(! district %in% c("Nyabihu", "Ngororero", "Nyamasheke", "Rubavu")) %>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) 
  

compare_usage <- function(df, name){
  out <- df %>%
    summarise(
      `2014_usage` = sum(`2014_usage`, na.rm = TRUE),
      `2020_usage` = sum(`2020_usage`, na.rm = TRUE),
      log_change   = log(`2020_usage`) - log(`2014_usage`)
    ) %>%
    mutate(dataset = name) %>%
    select(dataset, everything())
  
  return(out)
}

# Run comparison for each dataset
out_utility       <- compare_usage(utility, "utility raw")
out_utility_1     <- compare_usage(utility.1, "utility restrict to sample")

# Combine results
usage_comparison <- bind_rows(
  out_utility,
  out_utility_1
)

usage_comparison


decomp_fullsample <- map_dfr(2015:2022, ~ decompose_one_fullsample(utility.1, t0 = 2014, t1 = .x))
View(decomp_fullsample)


plot_df <- decomp_fullsample %>%
  pivot_longer(
    cols = c(
      T1_join_qty, 
      T2_join_sel, 
      T3_exit_qty, 
      T4_exit_sel,
      T5_growth_new,
      T6_ineq_new,
      total_log_change
    ),
    names_to = "component",
    values_to = "value"
  )

component_labels <- c(
  T1_join_qty      = "Join: Quantity",
  T2_join_sel      = "Join: Selection",
  T3_exit_qty      = "Exit: Quantity",
  T4_exit_sel      = "Exit: Selection",
  T5_growth_new    = "Growth (mean log use)",
  T6_ineq_new      = "Inequality (log gap)",
  total_log_change = "Total Change"
)

plot_df$component <- factor(plot_df$component,
                            levels = names(component_labels),
                            labels = component_labels)

p8.1 <- ggplot(plot_df, aes(x = t1, y = value, color = component)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    x = "Year (t1)",
    y = "Log contribution",
    color = "Component",
    title = "Full-Sample Decomposition of Electricity Use (electrified 2012-2014)\n(t0 = 2014, national level)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

p8.1

##full sample
decomp_fullsample <- map_dfr(2015:2022, ~ decompose_one_fullsample(utility, t0 = 2014, t1 = .x))
View(decomp_fullsample)


plot_df <- decomp_fullsample %>%
  pivot_longer(
    cols = c(
      T1_join_qty, 
      T2_join_sel, 
      T3_exit_qty, 
      T4_exit_sel,
      T5_growth_new,
      T6_ineq_new,
      total_log_change
    ),
    names_to = "component",
    values_to = "value"
  )

component_labels <- c(
  T1_join_qty      = "Join: Quantity",
  T2_join_sel      = "Join: Selection",
  T3_exit_qty      = "Exit: Quantity",
  T4_exit_sel      = "Exit: Selection",
  T5_growth_new    = "Growth (mean log use)",
  T6_ineq_new      = "Inequality (log gap)",
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



#elec12_14
utility.1 <- utility_decomp %>% 
  filter(first_year_elec >= 2012 & first_year_elec <= 2014)

decomp_fullsample <- map_dfr(2015:2022, ~ decompose_one_fullsample(utility.1, t0 = 2014, t1 = .x))
View(decomp_fullsample)


plot_df <- decomp_fullsample %>%
  pivot_longer(
    cols = c(
      T1_join_qty, 
      T2_join_sel, 
      T3_exit_qty, 
      T4_exit_sel,
      T5_growth_new,
      T6_ineq_new,
      total_log_change
    ),
    names_to = "component",
    values_to = "value"
  )

component_labels <- c(
  T1_join_qty      = "Join: Quantity",
  T2_join_sel      = "Join: Selection",
  T3_exit_qty      = "Exit: Quantity",
  T4_exit_sel      = "Exit: Selection",
  T5_growth_new    = "Growth (mean log use)",
  T6_ineq_new      = "Inequality (log gap)",
  total_log_change = "Total Change"
)

plot_df$component <- factor(plot_df$component,
                            levels = names(component_labels),
                            labels = component_labels)

p8.1 <- ggplot(plot_df, aes(x = t1, y = value, color = component)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    x = "Year (t1)",
    y = "Log contribution",
    color = "Component",
    title = "Full-Sample Decomposition of Electricity Use (electrified 2012-2014)\n(t0 = 2014, national level)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

p8.1

ggsave(file.path(output_path, "plot","decomposition(12_14).png"),
       p8.1, width = 8, height = 5, dpi = 300)









# 
# 
# #Village-level---------
# decompose_one_wide <- function(df, t0, t1) {
#   
#   # dynamic column names
#   u0 <- sym(paste0(t0, "_usage"))
#   u1 <- sym(paste0(t1, "_usage"))
#   
#   # classify each meter
#   classified <- df %>%
#     transmute(
#       meter_id,
#       village_id,
#       usage_t0 = !!u0,
#       usage_t1 = !!u1,
#       pos_t0 = usage_t0 > 0,
#       pos_t1 = usage_t1 > 0,
#       
#       stay =  pos_t0 &  pos_t1,
#       join = !pos_t0 &  pos_t1,
#       exit =  pos_t0 & !pos_t1
#     )
#   
#   # compute village-level decomposition inputs
#   result_village <- classified %>%
#     group_by(village_id) %>%
#     summarise(
#       # counts
#       N_stay = sum(stay),
#       N_join = sum(join),
#       N_exit = sum(exit),
#       
#       # averages
#       Xbar_stay_t0 = mean(usage_t0[stay], na.rm = TRUE),
#       Xbar_stay_t1 = mean(usage_t1[stay], na.rm = TRUE),
#       Xbar_join_t1 = mean(usage_t1[join], na.rm = TRUE),
#       Xbar_exit_t0 = mean(usage_t0[exit], na.rm = TRUE),
#       
#       # total usage
#       X_t0 = sum(usage_t0, na.rm = TRUE),
#       X_t1 = sum(usage_t1, na.rm = TRUE),
#       
#       # avg ratio for stayers
#       avg_ratio_stay = mean(usage_t1[stay] / usage_t0[stay], na.rm = TRUE),
#       
#       .groups = "drop"
#     ) %>%
#     filter(N_stay > 0, is.finite(avg_ratio_stay))   # avoid division by zero
#   
#   
#   # ========== 6 decomposition terms ==========
#   result_village <- result_village %>%
#     mutate(
#       # Term 1: joins quantity effect
#       T1_join_qty = log(1 + N_join / N_stay),
#       
#       # Term 2: joins composition (selection)
#       T2_join_sel = log(1 + (N_join / (N_stay + N_join)) *
#                           (Xbar_join_t1 / Xbar_stay_t1 - 1)),
#       
#       # Term 3: exits quantity
#       T3_exit_qty = log(1 - (N_exit / (N_stay + N_exit))),
#       
#       # Term 4: exit composition
#       T4_exit_sel =
#         -log(1 + (N_exit / (N_stay + N_exit)) *
#                (Xbar_exit_t0 / Xbar_stay_t0 - 1)),
#       
#       # Term 5: growth among stayers = ratio of averages
#       T5_growth = log(Xbar_stay_t1 / Xbar_stay_t0),
#       
#       # Term 6: inequality (difference between ratio of averages and average of ratios)
#       T6_ineq = log((Xbar_stay_t1 / Xbar_stay_t0) / avg_ratio_stay),
#       
#       # overall log change
#       total_log_change = log(X_t1) - log(X_t0),
#       
#       t0 = t0,
#       t1 = t1
#     )
#   
#   return(result_village)
# }
# 
# 
# 
# decomp_results <- map_dfr(2015:2022, ~ decompose_one_wide(utility, t0 = 2014, t1 = .x))
# 
# 
# decomp_results <- decomp_results %>%
#   mutate(
#     sum_terms = T1_join_qty + T2_join_sel + T3_exit_qty +
#       T4_exit_sel + T5_growth + T6_ineq,
#     diff = total_log_change - sum_terms
#   )
# 
# decomp_results %>%
#   summarise(
#     max_abs_diff = max(abs(diff), na.rm = TRUE),
#     mean_abs_diff = mean(abs(diff), na.rm = TRUE)
#   )
# 
# 
# plot_df <- decomp_results %>%
#   group_by(t1) %>%
#   summarise(
#     join_qty  = mean(T1_join_qty, na.rm = TRUE),
#     join_sel  = mean(T2_join_sel, na.rm = TRUE),
#     exit_qty  = mean(T3_exit_qty, na.rm = TRUE),
#     exit_sel  = mean(T4_exit_sel, na.rm = TRUE),
#     growth    = mean(T5_growth, na.rm = TRUE),
#     inequality= mean(T6_ineq, na.rm = TRUE),
#     total     = mean(total_log_change, na.rm = TRUE),
#     .groups   = "drop"
#   ) %>%
#   tidyr::pivot_longer(
#     cols = c(join_qty, join_sel, exit_qty, exit_sel, growth, inequality, total),
#     names_to = "component",
#     values_to = "value"
#   )
# 
# 
# component_labels <- c(
#   join_qty   = "Joiners: Quantity",
#   join_sel   = "Joiners: Composition",
#   exit_qty   = "Exiters: Quantity",
#   exit_sel   = "Exiters: Composition",
#   growth     = "Growth (Stayers)",
#   inequality = "Inequality (Stayers)",
#   total      = "Total Change"
# )
# 
# plot_df$component <- factor(plot_df$component, levels = names(component_labels), labels = component_labels)
# 
# 
# library(ggplot2)
# 
# ggplot(plot_df, aes(x = t1, y = value, color = component)) +
#   geom_line(size = 1.1) +
#   geom_point(size = 2) +
#   facet_wrap(~ component, scales = "free_y", ncol = 3) +
#   labs(
#     x = "Year (t1)",
#     y = "Component contribution (log units)",
#     title = "Decomposition of Change in Electricity Use (t0 = 2014)"
#   ) +
#   theme_minimal(base_size = 13) +
#   theme(
#     legend.position = "none",
#     strip.text = element_text(face = "bold")
#   )
