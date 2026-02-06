#######################################
#Purpose: Using EARP as a control for analysis
#Author: XIAOMING ZHANG
#Date: September 10th 2025
######################################################
pacman::p_load(knitr, lfe,fixest, modelsummary, stargazer, tidyverse, dplyr, here, sf, haven, ggplot2, readxl,  writexl, janitor, randomizr, RCT, purrr, RODBC, DBI)

getwd()

dropbox <- 'C:/Users/wb614406/Dropbox'

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion/data"
)

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion/outputs"
)


#Sample restriction----

expansion_join_drop12_17 <- expansion_join%>% 
  filter(electrified_year %in% c("2012", "2013", "2014","2015", "2016", "2017") |electrified_year == "9999" ) %>% 
  mutate(
    `elec12_14` = ifelse(electrified_year %in% c("2012", "2013", "2014"), 1, 0),
    `elec15_17` = ifelse(electrified_year %in% c("2015", "2016", "2017"), 1, 0)
    
  ) %>% 
  anti_join(earp_existing_mv, by = c("village_id" = "Village_ID")) %>% 
  filter(! District %in% c("Ngororero", "Nyabihu", "Nyamasheke", "Rubavu")) %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  )






#Event study with ISIC restriction----


##ISIC selection------

#utility_long join



ec_2011 <- read_xlsx(path = file.path(data_path, "2011", "group_long_2011(isic).xlsx"))

ec_2011 <- ec_2011 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n) %>% 
  group_by(village_id, isic_level1) %>% 
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE)) %>% 
  select(village_id, num_establishment, total_employee, isic_level1) %>% 
  mutate(year = 2011) %>% 
  ungroup()

ec_2014 <- read_xlsx(path = file.path(data_path, "2014", "group_long_2014(isic).xlsx"))

ec_2014 <- ec_2014 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n,
         isic_level1 = isic_level1_main) %>% 
  group_by(village_id, isic_level1) %>% 
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE)) %>% 
  select(village_id, num_establishment, total_employee, isic_level1) %>% 
  mutate(year = 2014) %>% 
  ungroup()


ec_2017 <- read_xlsx(path = file.path(data_path, "2017", "group_long_2017(isic).xlsx"))

ec_2017 <- ec_2017 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n,
         isic_level1 = isic_1_digit) %>% 
  group_by(village_id, isic_level1) %>% 
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE)) %>% 
  select(village_id, num_establishment, total_employee, isic_level1) %>% 
  mutate(year = 2017) %>% 
  ungroup()

ec_2020 <- read_xlsx(path = file.path(data_path, "2020", "group_long_2020(isic).xlsx"))

ec_2020 <- ec_2020 %>% 
  mutate(village_id = as.character(village_id)) %>% 
  rename(num_establishment = n) %>% 
  group_by(village_id, isic_level1) %>% 
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE)) %>% 
  select(village_id, num_establishment, total_employee, isic_level1) %>% 
  mutate(year = 2020) %>% 
  ungroup()


ec_all <- rbind(ec_2011, ec_2014, ec_2017, ec_2020)



elec12_17_join <- expansion_join_drop12_17%>% 
  select(village_id,elec12_14, elec15_17, cell_id, sector_id, district_id) 

elec12_17_did_isic<- left_join( elec12_17_join, ec_all, by = c("village_id"))


elec12_17_did_isic <- elec12_17_did_isic %>%
  complete(year, village_id, isic_level1,
           fill = list(num_establishment = 0, total_employee = 0)) %>% 
  filter(!is.na(year))   %>% 
  filter(!is.na(isic_level1)) %>% 
  select(year, village_id, num_establishment, total_employee, isic_level1)


elec12_17 <- expansion_join_drop12_17%>%
  select(village_id, elec12_14, elec15_17, earp_existing_mv, health_center, primary_school, cell_office, secondary_school, sector_district_office,
         industry, market, residential_consumer, non_residential_consumer, imidugudu) 


elec12_17_did_isic <- left_join(elec12_17_did_isic, elec12_17, by = c("village_id"))



elec12_17_did_isic <- elec12_17_did_isic %>% 
  mutate(
    cell_id    = substr(village_id, 1, 6),
    sector_id  = substr(village_id, 1, 4),
    district_id= substr(village_id, 1, 2)
  ) %>% 
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  ) %>%
  filter(!is.na(isic_level1)) %>%
  mutate(
    isic_level1 = as.character(isic_level1)  ) %>% 
  rename(
    isic = isic_level1
  )  





# Private sector data clean-------


join_drop12_17 <- expansion_join_drop12_17 %>% 
  select(`elec12_14`, elec15_17, village_id, cell_id, sector_id, cell_office, health_center, primary_school, secondary_school, 
         sector_district_office, market, industry, residential_consumer,  non_residential_consumer, imidugudu, dist_na_rd, population)

elec12_17_p <- elec12_17_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(9,7,3,19), 1, 0)) %>%
  filter(private_sector == 1) %>%
  group_by(year, village_id) %>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE), .groups="drop") %>%
  mutate(total_employee = pmin(total_employee, 100)) %>% 
  left_join(join_drop12_17, by = c("village_id") )  %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year), 
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  ) %>% 
  mutate(
    log1_num_establishment = log(num_establishment + 1),
    log1_total_employee = log(total_employee + 1)
  ) 

elec12_17_p <- elec12_17_p %>%
  mutate(
    treatment = ifelse(elec12_14 == 1 | elec15_17 == 1, 1, 0),
    
    period = case_when(
      treatment == 0 ~ "1-3 years before electrification",
      
      elec12_14 == 1 & year == 2011 ~ "1-3 years before electrification",
      elec12_14 == 1 & year == 2014 ~ "electrified 0-2 years",
      elec12_14 == 1 & year == 2017 ~ "electrified 3-5 years",
      elec12_14 == 1 & year == 2020 ~ "electrified 6-8 years",
      
      elec15_17 == 1 & year == 2011 ~ "4-6 years before electrification",
      elec15_17 == 1 & year == 2014 ~ "1-3 years before electrification",
      elec15_17 == 1 & year == 2017 ~ "electrified 0-2 years",
      elec15_17 == 1 & year == 2020 ~ "electrified 3-5 years",
      
      # Fallback (if no match)
      TRUE ~ NA_character_
    ),
    
    # Ensure consistent factor ordering
    period = factor(
      period,
      levels = c(
        "4-6 years before electrification",
        "1-3 years before electrification",
        "electrified 0-2 years",
        "electrified 3-5 years",
        "electrified 6-8 years"
      )
    )
  ) %>% 
  mutate(dummy = 1) %>%
  pivot_wider(
    names_from = period,
    values_from = dummy,
    values_fill = 0,
    names_prefix = "period_"
  ) %>% 
  clean_names() %>% 
  mutate(
    year = as.factor(year)
  )

##regression --------

reg_est <- felm(
  num_establishment ~ 
    period_electrified_0_2_years +
    period_electrified_3_5_years +
    period_electrified_6_8_years +
    period_4_6_years_before_electrification +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p 
)

summary(reg_est)




reg_emp <- felm(
  total_employee ~ 
    period_electrified_0_2_years +
    period_electrified_3_5_years +
    period_electrified_6_8_years +
    period_4_6_years_before_electrification +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer  |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p
)

summary(reg_emp)



library(broom)
library(dplyr)
library(ggplot2)
library(ggrepel)

## --- 1. Extract event-study coefficients ------------------------------
extract_eventstudy <- function(model, outcome_label) {
  tidy(model) %>%
    filter(grepl("^period_", term)) %>%
    mutate(
      outcome = outcome_label,
      period_label = term %>%
        gsub("^period_", "", .) %>%
        trimws()
    ) %>%
    select(period_label, estimate, std.error, p.value, outcome)
}



## --- 2. Extract for both outcomes -------------------------------------
est_df <- extract_eventstudy(reg_est, "Establishments")
emp_df <- extract_eventstudy(reg_emp, "Employees")


## --- 3. Combine and compute CI, stars, labels -------------------------
plot_df <- bind_rows(est_df, emp_df) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.1  ~ "*",
      TRUE ~ ""
    ),
    label = paste0(round(estimate, 2), stars),
    # numeric order for plotting
    time_since = case_when(
      period_label == "electrified_0_2_years" ~ "0 to 2",
      period_label == "electrified_3_5_years" ~ "3 to 5",
      period_label == "electrified_6_8_years" ~ "6 to 8",
      period_label == "4_6_years_before_electrification" ~ "-6 to -4",
      
    )
  ) %>% 
  add_row(time_since = "-3 to -1", outcome = "Establishments", estimate = 0, conf.low = NA, conf.high = NA, label = "0") %>% 
  add_row(time_since = "-3 to -1", outcome = "Employees", estimate = 0, conf.low = NA, conf.high = NA, label = "0") %>% 
  mutate(
    time_since = factor(time_since, levels = c(
      "-6 to -4",
      "-3 to -1",
      "0 to 2",
      "3 to 5",
      "6 to 8"
    ))
  )


## --- 4. plot: Establishments ------------------------------------------

plot_est <- plot_df %>%
  filter(outcome == "Establishments") 


p_est_bw <- ggplot(plot_est, aes(x = time_since, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, color = "gray40", linewidth = 0.6, na.rm = TRUE) +
  geom_point(size = 2.2, color = "black") +
  labs(
    x = "Years Since Electrification",
    y = "Number of Establishments\n(Private Sector)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(color = "gray40", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )



p_est_bw



## --- 2. Prepare Employees plot Data -------------------------
# plot: Employees
plot_emp <- plot_df %>%
  filter(outcome == "Employees")

p_emp_bw <- ggplot(plot_emp, aes(x = time_since, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, color = "gray40", linewidth = 0.6, na.rm = TRUE) +
  geom_point(size = 2.2, color = "black") +
  labs(
    x = "Years Since Electrification",
    y = "Total Employees\n(Private Sector)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(color = "gray40", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )


p_emp_bw





## --- 6. Display --------------------------------------------------------
p_est_bw
p_emp_bw

## --- 7. Save -----------------------------------------------------------
ggsave(file.path(output_path, "plot"
                 , "eventstudy_establishments_bw.png"),
       p_est_bw, width = 8, height = 6, dpi = 400, bg = "white", scale = 0.6)

ggsave(file.path(output_path, "plot", "eventstudy_employees_bw.png"),
       p_emp_bw, width = 8, height = 6, dpi = 400, bg = "white", scale = 0.6)














# Manufacture -------

elec12_17_p.3 <- elec12_17_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(3), 1, 0)) %>%
  filter(private_sector == 1) %>%
  group_by(year, village_id) %>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE), .groups="drop") %>%
  mutate(total_employee = pmin(total_employee, 100)) %>% 
  left_join(join_drop12_17, by = c("village_id") )  %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year), 
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  ) %>% 
  mutate(
    log1_num_establishment = log(num_establishment + 1),
    log1_total_employee = log(total_employee + 1)
  ) 

elec12_17_p.3 <- elec12_17_p.3 %>%
  mutate(
    treatment = ifelse(elec12_14 == 1 | elec15_17 == 1, 1, 0),
    
    period = case_when(
      treatment == 0 ~ "1-3 years before electrification",
      
      elec12_14 == 1 & year == 2011 ~ "1-3 years before electrification",
      elec12_14 == 1 & year == 2014 ~ "electrified 0-2 years",
      elec12_14 == 1 & year == 2017 ~ "electrified 3-5 years",
      elec12_14 == 1 & year == 2020 ~ "electrified 6-8 years",
      
      elec15_17 == 1 & year == 2011 ~ "4-6 years before electrification",
      elec15_17 == 1 & year == 2014 ~ "1-3 years before electrification",
      elec15_17 == 1 & year == 2017 ~ "electrified 0-2 years",
      elec15_17 == 1 & year == 2020 ~ "electrified 3-5 years",
      
      TRUE ~ NA_character_
    ),
    period = factor(
      period,
      levels = c(
        "4-6 years before electrification",
        "1-3 years before electrification",
        "electrified 0-2 years",
        "electrified 3-5 years",
        "electrified 6-8 years"
      )
    )
  ) %>% 
  mutate(dummy = 1) %>%
  pivot_wider(
    names_from = period,
    values_from = dummy,
    values_fill = 0,
    names_prefix = "period_"
  ) %>% 
  clean_names() %>% 
  mutate(
    year = as.factor(year)
  )

## regression --------
reg_est.3 <- felm(
  num_establishment ~ 
    period_electrified_0_2_years +
    period_electrified_3_5_years +
    period_electrified_6_8_years +
    period_4_6_years_before_electrification +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.3 
)

summary(reg_est.3)

reg_emp.3 <- felm(
  total_employee ~ 
    period_electrified_0_2_years +
    period_electrified_3_5_years +
    period_electrified_6_8_years +
    period_4_6_years_before_electrification +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer  |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.3
)

summary(reg_emp.3)

## Extract for both outcomes
est_df.3 <- extract_eventstudy(reg_est.3, "Establishments")
emp_df.3 <- extract_eventstudy(reg_emp.3, "Employees")

## Combine and compute CI, stars, labels
plot_df.3 <- bind_rows(est_df.3, emp_df.3) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.1  ~ "*",
      TRUE ~ ""
    ),
    label = paste0(round(estimate, 2), stars),
    time_since = case_when(
      period_label == "electrified_0_2_years" ~ "0 to 2",
      period_label == "electrified_3_5_years" ~ "3 to 5",
      period_label == "electrified_6_8_years" ~ "6 to 8",
      period_label == "4_6_years_before_electrification" ~ "-6 to -4",
      
    )
  ) %>% 
  add_row(time_since = "-3 to -1", outcome = "Establishments", estimate = 0, conf.low = NA, conf.high = NA, label = "0") %>% 
  add_row(time_since = "-3 to -1", outcome = "Employees", estimate = 0, conf.low = NA, conf.high = NA, label = "0") %>% 
  mutate(
    time_since = factor(time_since, levels = c(
      "-6 to -4",
      "-3 to -1",
      "0 to 2",
      "3 to 5",
      "6 to 8"
    ))
  )
## --- 4. plot: Establishments ------------------------------------------

plot_est.3 <- plot_df.3 %>%
  filter(outcome == "Establishments") 

p_est_bw.3 <- ggplot(plot_est.3, aes(x = time_since, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, color = "gray40", linewidth = 0.6, na.rm = TRUE) +
  geom_point(size = 2.2, color = "black") +
  labs(
    x = "Years Since Electrification",
    y = "Number of Establishments\n(Manufacture)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(color = "gray40", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )



p_est_bw.3



## --- 5. plot: Employees ------------------------------------------
plot_emp.3 <- plot_df.3 %>%
  filter(outcome == "Employees")

p_emp_bw.3 <- ggplot(plot_emp.3, aes(x = time_since, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, color = "gray40", linewidth = 0.6, na.rm = TRUE) +
  geom_point(size = 2.2, color = "black") +
  labs(
    x = "Years Since Electrification",
    y = "Total Employees\n(Manufacture)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(color = "gray40", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )

p_emp_bw.3


## --- 6. Display --------------------------------------------------------
p_est_bw.3
p_emp_bw.3

## --- 7. Save -----------------------------------------------------------
ggsave(file.path(output_path, "plot", "eventstudy_establishments_bw(manufacture).png"),
       p_est_bw.3, width = 8, height = 6, dpi = 400, bg = "white", scale = 0.6)

ggsave(file.path(output_path, "plot", "eventstudy_employees_bw(manufacture).png"),
       p_emp_bw.3, width = 8, height = 6, dpi = 400, bg = "white", scale = 0.6)









# wholesale -------

elec12_17_p.7 <- elec12_17_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(7), 1, 0)) %>%
  filter(private_sector == 1) %>%
  group_by(year, village_id) %>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE), .groups="drop") %>%
  mutate(total_employee = pmin(total_employee, 100)) %>% 
  left_join(join_drop12_17, by = c("village_id") )  %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year), 
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  ) %>% 
  mutate(
    log1_num_establishment = log(num_establishment + 1),
    log1_total_employee = log(total_employee + 1)
  ) 

elec12_17_p.7 <- elec12_17_p.7 %>%
  mutate(
    treatment = ifelse(elec12_14 == 1 | elec15_17 == 1, 1, 0),
    
    period = case_when(
      treatment == 0 ~ "1-3 years before electrification",
      
      elec12_14 == 1 & year == 2011 ~ "1-3 years before electrification",
      elec12_14 == 1 & year == 2014 ~ "electrified 0-2 years",
      elec12_14 == 1 & year == 2017 ~ "electrified 3-5 years",
      elec12_14 == 1 & year == 2020 ~ "electrified 6-8 years",
      
      elec15_17 == 1 & year == 2011 ~ "4-6 years before electrification",
      elec15_17 == 1 & year == 2014 ~ "1-3 years before electrification",
      elec15_17 == 1 & year == 2017 ~ "electrified 0-2 years",
      elec15_17 == 1 & year == 2020 ~ "electrified 3-5 years",
      
      TRUE ~ NA_character_
    ),
    period = factor(
      period,
      levels = c(
        "4-6 years before electrification",
        "1-3 years before electrification",
        "electrified 0-2 years",
        "electrified 3-5 years",
        "electrified 6-8 years"
      )
    )
  ) %>% 
  mutate(dummy = 1) %>%
  pivot_wider(
    names_from = period,
    values_from = dummy,
    values_fill = 0,
    names_prefix = "period_"
  ) %>% 
  clean_names() %>% 
  mutate(
    year = as.factor(year)
  )

## regression --------
reg_est.7 <- felm(
  num_establishment ~ 
    period_electrified_0_2_years +
    period_electrified_3_5_years +
    period_electrified_6_8_years +
    period_4_6_years_before_electrification +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.7 
)

summary(reg_est.7)

reg_emp.7 <- felm(
  total_employee ~ 
    period_electrified_0_2_years +
    period_electrified_3_5_years +
    period_electrified_6_8_years +
    period_4_6_years_before_electrification +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer  |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.7
)

summary(reg_emp.7)

## Extract for both outcomes
est_df.7 <- extract_eventstudy(reg_est.7, "Establishments")
emp_df.7 <- extract_eventstudy(reg_emp.7, "Employees")

## Combine and compute CI, stars, labels
plot_df.7 <- bind_rows(est_df.7, emp_df.7) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.1  ~ "*",
      TRUE ~ ""
    ),
    label = paste0(round(estimate, 2), stars),
    time_since = case_when(
      period_label == "electrified_0_2_years" ~ "0 to 2",
      period_label == "electrified_3_5_years" ~ "3 to 5",
      period_label == "electrified_6_8_years" ~ "6 to 8",
      period_label == "4_6_years_before_electrification" ~ "-6 to -4",
      
    )
  ) %>% 
  add_row(time_since = "-3 to -1", outcome = "Establishments", estimate = 0, conf.low = NA, conf.high = NA, label = "0") %>% 
  add_row(time_since = "-3 to -1", outcome = "Employees", estimate = 0, conf.low = NA, conf.high = NA, label = "0") %>% 
  mutate(
    time_since = factor(time_since, levels = c(
      "-6 to -4",
      "-3 to -1",
      "0 to 2",
      "3 to 5",
      "6 to 8"
    ))
  )



## plot: Establishments

plot_est.7 <- plot_df.7 %>%
  filter(outcome == "Establishments") 

p_est_bw.7 <- ggplot(plot_est.7, aes(x = time_since, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, color = "gray40", linewidth = 0.6, na.rm = TRUE) +
  geom_point(size = 2.2, color = "black") +
  labs(
    x = "Years Since Electrification",
    y = "Number of Establishments\n(Wholesale)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(color = "gray40", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )

p_est_bw.7



plot_emp.7 <- plot_df.7 %>%
  filter(outcome == "Employees")

p_emp_bw.7 <- ggplot(plot_emp.7, aes(x = time_since, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, color = "gray40", linewidth = 0.6, na.rm = TRUE) +
  geom_point(size = 2.2, color = "black") +
  labs(
    x = "Years Since Electrification",
    y = "Total Employees\n(Wholesale)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(color = "gray40", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )

p_emp_bw.7


p_est_bw.7
p_emp_bw.7

ggsave(file.path(output_path, "plot", "eventstudy_establishments_bw(wholesale).png"),
       p_est_bw.7, width = 8, height = 6, dpi = 400, bg = "white", scale = 0.6)

ggsave(file.path(output_path, "plot", "eventstudy_employees_bw(wholesale).png"),
       p_emp_bw.7, width = 8, height = 6, dpi = 400, bg = "white", scale = 0.6)
























































# food&accomodation -------

elec12_17_p.9 <- elec12_17_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(9), 1, 0)) %>%
  filter(private_sector == 1) %>%
  group_by(year, village_id) %>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE), .groups="drop") %>%
  mutate(total_employee = pmin(total_employee, 100)) %>% 
  left_join(join_drop12_17, by = c("village_id") )  %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year), 
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  ) %>% 
  mutate(
    log1_num_establishment = log(num_establishment + 1),
    log1_total_employee = log(total_employee + 1)
  ) 

elec12_17_p.9 <- elec12_17_p.9 %>%
  mutate(
    treatment = ifelse(elec12_14 == 1 | elec15_17 == 1, 1, 0),
    
    period = case_when(
      treatment == 0 ~ "1-3 years before electrification",
      
      elec12_14 == 1 & year == 2011 ~ "1-3 years before electrification",
      elec12_14 == 1 & year == 2014 ~ "electrified 0-2 years",
      elec12_14 == 1 & year == 2017 ~ "electrified 3-5 years",
      elec12_14 == 1 & year == 2020 ~ "electrified 6-8 years",
      
      elec15_17 == 1 & year == 2011 ~ "4-6 years before electrification",
      elec15_17 == 1 & year == 2014 ~ "1-3 years before electrification",
      elec15_17 == 1 & year == 2017 ~ "electrified 0-2 years",
      elec15_17 == 1 & year == 2020 ~ "electrified 3-5 years",
      
      TRUE ~ NA_character_
    ),
    period = factor(
      period,
      levels = c(
        "4-6 years before electrification",
        "1-3 years before electrification",
        "electrified 0-2 years",
        "electrified 3-5 years",
        "electrified 6-8 years"
      )
    )
  ) %>% 
  mutate(dummy = 1) %>%
  pivot_wider(
    names_from = period,
    values_from = dummy,
    values_fill = 0,
    names_prefix = "period_"
  ) %>% 
  clean_names() %>% 
  mutate(
    year = as.factor(year)
  )

## regression --------
reg_est.9 <- felm(
  num_establishment ~ 
    period_electrified_0_2_years +
    period_electrified_3_5_years +
    period_electrified_6_8_years +
    period_4_6_years_before_electrification +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.9 
)

summary(reg_est.9)

reg_emp.9 <- felm(
  total_employee ~ 
    period_electrified_0_2_years +
    period_electrified_3_5_years +
    period_electrified_6_8_years +
    period_4_6_years_before_electrification +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer  |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.9
)

summary(reg_emp.9)

## Extract for both outcomes
est_df.9 <- extract_eventstudy(reg_est.9, "Establishments")
emp_df.9 <- extract_eventstudy(reg_emp.9, "Employees")

## Combine and compute CI, stars, labels
plot_df.9 <- bind_rows(est_df.9, emp_df.9) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.1  ~ "*",
      TRUE ~ ""
    ),
    label = paste0(round(estimate, 2), stars),
    time_since = case_when(
      period_label == "electrified_0_2_years" ~ "0 to 2",
      period_label == "electrified_3_5_years" ~ "3 to 5",
      period_label == "electrified_6_8_years" ~ "6 to 8",
      period_label == "4_6_years_before_electrification" ~ "-6 to -4",
      
    )
  ) %>% 
  add_row(time_since = "-3 to -1", outcome = "Establishments", estimate = 0, conf.low = NA, conf.high = NA, label = "0") %>% 
  add_row(time_since = "-3 to -1", outcome = "Employees", estimate = 0, conf.low = NA, conf.high = NA, label = "0") %>% 
  mutate(
    time_since = factor(time_since, levels = c(
      "-6 to -4",
      "-3 to -1",
      "0 to 2",
      "3 to 5",
      "6 to 8"
    ))
  )



## plot: Establishments

plot_est.9 <- plot_df.9 %>%
  filter(outcome == "Establishments") 

p_est_bw.9 <- ggplot(plot_est.9, aes(x = time_since, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, color = "gray40", linewidth = 0.6, na.rm = TRUE) +
  geom_point(size = 2.2, color = "black") +
  labs(
    x = "Years Since Electrification",
    y = "Number of Establishments\n(Food & Accomodation)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(color = "gray40", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )

p_est_bw.9



plot_emp.9 <- plot_df.9 %>%
  filter(outcome == "Employees")

p_emp_bw.9 <- ggplot(plot_emp.9, aes(x = time_since, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, color = "gray40", linewidth = 0.6, na.rm = TRUE) +
  geom_point(size = 2.2, color = "black") +
  labs(
    x = "Years Since Electrification",
    y = "Total Employees\n(Food & Accomodation)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(color = "gray40", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )

p_emp_bw.9


p_est_bw.9
p_emp_bw.9

ggsave(file.path(output_path, "plot", "eventstudy_establishments_bw(food&accomodation).png"),
       p_est_bw.9, width = 8, height = 6, dpi = 400, bg = "white", scale = 0.6)

ggsave(file.path(output_path, "plot", "eventstudy_employees_bw(food&accomodation).png"),
       p_emp_bw.9, width = 8, height = 6, dpi = 400, bg = "white", scale = 0.6)
















# barbershop -------

elec12_17_p.19 <- elec12_17_did_isic %>%
  mutate(private_sector = ifelse(isic %in% c(19), 1, 0)) %>%
  filter(private_sector == 1) %>%
  group_by(year, village_id) %>%
  summarise(num_establishment = sum(num_establishment, na.rm = TRUE),
            total_employee = sum(total_employee, na.rm = TRUE), .groups="drop") %>%
  mutate(total_employee = pmin(total_employee, 100)) %>% 
  left_join(join_drop12_17, by = c("village_id") )  %>%
  mutate(
    cell_year = paste0(cell_id, "_", year),
    cell_office_year = paste0(cell_office, "_", year),
    health_center_year = paste0(health_center, "_", year),
    primary_school_year = paste0(primary_school, "_", year),
    secondary_school_year = paste0(secondary_school, "_", year),
    sector_district_office_year = paste0(sector_district_office, "_", year),
    industry_year = paste0(industry, "_", year),
    market_year = paste0(market, "_", year),
    imidugudu_year = paste0(imidugudu, "_", year), 
    p_1_2011 = ifelse(year == 2011, 1, 0),
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  ) %>% 
  mutate(
    log1_residential_consumer = log1p(residential_consumer),
    log1_non_residential_consumer = log1p(non_residential_consumer),
    log_residential_consumer = ifelse(residential_consumer > 0, log(residential_consumer), 0),
    log_non_residential_consumer = ifelse(non_residential_consumer > 0, log(non_residential_consumer), 0),
    any_residential_consumer = ifelse(residential_consumer > 0, 1, 0),
    any_non_residential_consumer = ifelse(non_residential_consumer >0, 1, 0)
  ) %>% 
  mutate(
    log1_num_establishment = log(num_establishment + 1),
    log1_total_employee = log(total_employee + 1)
  ) 

elec12_17_p.19 <- elec12_17_p.19 %>%
  mutate(
    treatment = ifelse(elec12_14 == 1 | elec15_17 == 1, 1, 0),
    
    period = case_when(
      treatment == 0 ~ "1-3 years before electrification",
      
      elec12_14 == 1 & year == 2011 ~ "1-3 years before electrification",
      elec12_14 == 1 & year == 2014 ~ "electrified 0-2 years",
      elec12_14 == 1 & year == 2017 ~ "electrified 3-5 years",
      elec12_14 == 1 & year == 2020 ~ "electrified 6-8 years",
      
      elec15_17 == 1 & year == 2011 ~ "4-6 years before electrification",
      elec15_17 == 1 & year == 2014 ~ "1-3 years before electrification",
      elec15_17 == 1 & year == 2017 ~ "electrified 0-2 years",
      elec15_17 == 1 & year == 2020 ~ "electrified 3-5 years",
      
      TRUE ~ NA_character_
    ),
    period = factor(
      period,
      levels = c(
        "4-6 years before electrification",
        "1-3 years before electrification",
        "electrified 0-2 years",
        "electrified 3-5 years",
        "electrified 6-8 years"
      )
    )
  ) %>% 
  mutate(dummy = 1) %>%
  pivot_wider(
    names_from = period,
    values_from = dummy,
    values_fill = 0,
    names_prefix = "period_"
  ) %>% 
  clean_names() %>% 
  mutate(
    year = as.factor(year)
  )

## regression --------
reg_est.19 <- felm(
  num_establishment ~ 
    period_electrified_0_2_years +
    period_electrified_3_5_years +
    period_electrified_6_8_years +
    period_4_6_years_before_electrification +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.19 
)

summary(reg_est.19)

reg_emp.19 <- felm(
  total_employee ~ 
    period_electrified_0_2_years +
    period_electrified_3_5_years +
    period_electrified_6_8_years +
    period_4_6_years_before_electrification +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer  |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.19
)

summary(reg_emp.19)

## Extract for both outcomes
est_df.19 <- extract_eventstudy(reg_est.19, "Establishments")
emp_df.19 <- extract_eventstudy(reg_emp.19, "Employees")

## Combine and compute CI, stars, labels
plot_df.19 <- bind_rows(est_df.19, emp_df.19) %>%
  mutate(
    conf.low = estimate - 1.196 * std.error,
    conf.high = estimate + 1.196 * std.error,
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.1  ~ "*",
      TRUE ~ ""
    ),
    label = paste0(round(estimate, 2), stars),
    time_since = case_when(
      period_label == "electrified_0_2_years" ~ "0 to 2",
      period_label == "electrified_3_5_years" ~ "3 to 5",
      period_label == "electrified_6_8_years" ~ "6 to 8",
      period_label == "4_6_years_before_electrification" ~ "-6 to -4",
      
    )
  ) %>% 
  add_row(time_since = "-3 to -1", outcome = "Establishments", estimate = 0, conf.low = NA, conf.high = NA, label = "0") %>% 
  add_row(time_since = "-3 to -1", outcome = "Employees", estimate = 0, conf.low = NA, conf.high = NA, label = "0") %>% 
  mutate(
    time_since = factor(time_since, levels = c(
      "-6 to -4",
      "-3 to -1",
      "0 to 2",
      "3 to 5",
      "6 to 8"
    ))
  )


## plot: Establishments

plot_est.19 <- plot_df.19 %>%
  filter(outcome == "Establishments") 

p_est_bw.19 <- ggplot(plot_est.19, aes(x = time_since, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, color = "gray40", linewidth = 0.6, na.rm = TRUE) +
  geom_point(size = 2.2, color = "black") +
  labs(
    x = "Years Since Electrification",
    y = "Number of Establishments\n(Other)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(color = "gray40", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )

p_est_bw.19



plot_emp.19 <- plot_df.19 %>%
  filter(outcome == "Employees")

p_emp_bw.19 <- ggplot(plot_emp.19, aes(x = time_since, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, color = "gray40", linewidth = 0.6, na.rm = TRUE) +
  geom_point(size = 2.2, color = "black") +
  labs(
    x = "Years Since Electrification",
    y = "Total Employees\n(Other)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major.y = element_line(color = "gray40", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )

p_emp_bw.19


p_est_bw.19
p_emp_bw.19

ggsave(file.path(output_path, "plot", "eventstudy_establishments_bw(other).png"),
       p_est_bw.19, width = 8, height = 6, dpi = 400, bg = "white", scale = 0.6)

ggsave(file.path(output_path, "plot", "eventstudy_employees_bw(other).png"),
       p_emp_bw.19, width = 8, height = 6, dpi = 400, bg = "white", scale = 0.6)





#Stargazer------



list <- c(
  #private sector
  reg_est,
  reg_emp,
  #Manufacture
  reg_est.3,
  reg_emp.3, 
  #wholesale
  reg_est.7,
  reg_emp.7,
  #food&accomodation
  reg_est.9,
  reg_emp.9,
  #barbershop
  reg_est.19,
  reg_emp.19
)







# combine models
reg_list <- list(
  # Private sector
  reg_est, reg_emp,
  # Manufacture
  reg_est.3, reg_emp.3,
  # Wholesale
  reg_est.7, reg_emp.7,
  # Food & Accommodation
  reg_est.9, reg_emp.9,
  # Barbershop
  reg_est.19, reg_emp.19
)

# temporary tex output path
out_file <- file.path(output_path, "regressions", "raw", "eventstudy_all.tex")

stargazer(
  reg_list,
  type = "latex",
  out = out_file,
  title = "Event Study: Electrification Impacts by Sector",
  label = "tab:eventstudy_sector",
  column.labels = c(
    "Num. Establishments", "Total Employees",
    "Num. Establishments", "Total Employees",
    "Num. Establishments", "Total Employees",
    "Num. Establishments", "Total Employees",
    "Num. Establishments", "Total Employees"
  ),
  dep.var.labels.include = FALSE,
  keep.stat = c("n", "rsq"),
  omit.stat = c("adj.rsq", "ser", "f", "ll", "aic", "bic"),
  header = FALSE,
  digits = 3,
  float = TRUE,
  float.env = "table",
  table.placement = "!htbp"
)
extract_terms_period <- function(tex_file_path) {
  # Read LaTeX file
  lines <- readLines(tex_file_path)
  
  # Helper to grab coefficient + SE + blank line
  extract_chunk <- function(indexes) {
    chunks <- lapply(indexes, function(idx) {
      end <- min(idx + 2, length(lines))
      lines[idx:end]
    })
    unlist(chunks)
  }
  
  # ---- Extract all lines that contain "period" ----
  period_idx <- grep("period", lines, fixed = TRUE)
  period_terms <- extract_chunk(period_idx)
  
  # ---- Extract Observations and R2 lines ----
  obs_line <- lines[grep("^Observations\\s*&", lines)]
  r2_line  <- lines[grep("^R\\$\\^\\{2\\}\\$\\s*&", lines)]
  
  # Return list
  list(
    period = period_terms,
    obs = obs_line,
    r2 = r2_line
  )
}


extracted_sector <- extract_terms_period(
  file.path(output_path, "regressions", "raw", "eventstudy_all.tex")
)



writeLines(c(
  "\\begin{table}[hbt!]",
  "\\centering",
  "\\caption{Event Study: Electrification Impacts by Sector (Selected Coefficients)}",
  "\\label{tab:eventstudy_sector_period}",
  "\\small",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{@{\\extracolsep{5pt}}lcccccccccc}",
  "\\\\[-1.8ex]\\hline",
  "\\hline \\\\[-1.8ex]",
  " & \\multicolumn{10}{c}{\\textit{Dependent variable:}} \\\\",
  "\\cline{2-11}",
  "\\\\[-1.8ex] & \\multicolumn{2}{c}{Private Sector} & \\multicolumn{2}{c}{Manufacturing} & \\multicolumn{2}{c}{Wholesale} & \\multicolumn{2}{c}{Food and Accom.} & \\multicolumn{2}{c}{Barbershop} \\\\",
  "\\\\[-1.8ex] & num\\_establishment & total\\_employee & num\\_establishment & total\\_employee & num\\_establishment & total\\_employee & num\\_establishment & total\\_employee & num\\_establishment & total\\_employee \\\\",
  " & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) & (10) \\\\",
  "\\hline \\\\[-1.8ex]",
  
  # ---- Insert extracted coefficient chunks ----
  extracted_sector$period,
  
  "\\hline \\\\[-1.8ex]",
  
  # ---- Summary stats ----
  extracted_sector$obs,
  extracted_sector$r2,
  
  # ---- Fixed Effects section ----
  "FE: Village & X & X & X & X & X & X & X & X & X & X \\\\",
  "FE: Cell-Year & X & X & X & X & X & X & X & X & X & X \\\\",
  "FE: Cell Office-Year & X & X & X & X & X & X & X & X & X & X \\\\",
  "FE: Health Center-Year & X & X & X & X & X & X & X & X & X & X \\\\",
  "FE: Primary School-Year & X & X & X & X & X & X & X & X & X & X \\\\",
  "FE: Secondary School-Year & X & X & X & X & X & X & X & X & X & X \\\\",
  "FE: Sector/District Office-Year & X & X & X & X & X & X & X & X & X & X \\\\",
  "FE: Industry-Year & X & X & X & X & X & X & X & X & X & X \\\\",
  "FE: Market-Year & X & X & X & X & X & X & X & X & X & X \\\\",
  "FE: Imidugudu-Year & X & X & X & X & X & X & X & X & X & X \\\\",
  
  "\\hline",
  "\\end{tabular}",
  "}%",   # close resizebox properly here
  "\\end{table}"
),
file.path(output_path, "regressions", "eventstudy_sector_period_FE.tex"))








#Above and below median --------
median <- elec12_17_p %>% 
  filter(year == 2011  ) %>% 
  mutate(
    est_median.1 = median(num_establishment),
    emp_median.1 = median(total_employee)
  ) %>% 
  mutate(
    est_median = ifelse(num_establishment > est_median.1, 1, 0),
    emp_median = ifelse(total_employee > emp_median.1,1 , 0)
  ) %>% 
  select(village_id, est_median, emp_median)


elec12_17_p_median <- left_join(elec12_17_p, median)


##Number of establishments--------

elec12_17_p.1 <- elec12_17_p_median%>% 
  filter(est_median == 1)

plot_est.1 <- felm(
  num_establishment ~ 
    period_electrified_0_2_years +
    period_electrified_3_5_years +
    period_electrified_6_8_years +
    period_4_6_years_before_electrification +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.1
)

summary(plot_est.1)



elec12_17_p.0 <- elec12_17_p_median %>% 
  filter(est_median == 0)

plot_est.0<- felm(
  num_establishment ~ 
    period_electrified_0_2_years +
    period_electrified_3_5_years +
    period_electrified_6_8_years +
    period_4_6_years_before_electrification +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer  |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.0
)

summary(plot_est.0)





elec12_17_p.1 <- elec12_17_p_median%>% 
  filter(emp_median == 1)

plot_emp.1 <- felm(
  total_employee ~ 
    period_electrified_0_2_years +
    period_electrified_3_5_years +
    period_electrified_6_8_years +
    period_4_6_years_before_electrification +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.1
)

summary(plot_emp.1)



elec12_17_p.0 <- elec12_17_p_median %>% 
  filter(emp_median == 0)

plot_emp.0<- felm(
  total_employee ~ 
    period_electrified_0_2_years +
    period_electrified_3_5_years +
    period_electrified_6_8_years +
    period_4_6_years_before_electrification +
    year * log1_residential_consumer + 
    year * log1_non_residential_consumer  |
    village_id + cell_year + 
    cell_office_year + health_center_year +
    primary_school_year + secondary_school_year +
    sector_district_office_year + industry_year +
    market_year + imidugudu_year |
    0 | sector_id,
  data = elec12_17_p.0
)

summary(plot_emp.0)






modelsummary(
  list(
    "Est. Above Median" = plot_est.1,
    "Est. Below Median" = plot_est.0,
    "Emp. Above Median" = plot_emp.1,
    "Emp. Below Median" = plot_emp.0
  ),
  coef_map = c(
    "period_electrified_0_2_years" = "0–2 years after",
    "period_electrified_3_5_years" = "3–5 years after",
    "period_electrified_6_8_years" = "6–8 years after",
    "period_4_6_years_before_electrification" = "4–6 years before"
  ),
  gof_omit = "R2|AIC|BIC|Log|F|Within|Residual|Std|adj|df",  # keep it clean
  stars = TRUE,
  fmt = 3,
  output = "markdown"
)


# --- 2. Extract for both outcomes -------------------------------------
est1_df <- extract_eventstudy(plot_est.1, "Establishments")
est0_df <- extract_eventstudy(plot_est.0, "Establishments")
emp1_df <- extract_eventstudy(plot_emp.1, "Employees")
emp0_df <- extract_eventstudy(plot_emp.0, "Employees")


# --- 3. Combine and compute CI, stars, labels -------------------------
plot_df <- bind_rows(est1_df, est0_df, emp1_df, emp0_df) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.1  ~ "*",
      TRUE ~ ""
    ),
    label = paste0(round(estimate, 2), stars),
    # numeric order for plotting
    time_since = case_when(
      period_label == "electrified_0_2_years" ~ "electrified 0-2 years",
      period_label == "electrified_3_5_years" ~ "electrified 3-5 years",
      period_label == "electrified_6_8_years" ~ "electrified 6-8 years",
      period_label == "4_6_years_before_electrification" ~ "4-6 years before electrification",
      
    )
  ) %>% 
  add_row(time_since = "1-3 years before electrification", outcome = "Establishments", estimate = 0, conf.low = NA, conf.high = NA, label = "0") %>% 
  add_row(time_since = "1-3 years before electrification", outcome = "Employees", estimate = 0, conf.low = NA, conf.high = NA, label = "0") %>% 
  mutate(
    time_since = factor(time_since, levels = c(
      "4-6 years before electrification",
      "1-3 years before electrification",
      "electrified 0-2 years",
      "electrified 3-5 years",
      "electrified 6-8 years"
    ))
  )


# --- 4. plot: Establishments ------------------------------------------

plot_est <- plot_df %>%
  filter(outcome == "Establishments") 


# plot: Establishments
p_est_bw <- ggplot(plot_est, aes(x = time_since, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, color = "gray40", linewidth = 0.6, na.rm = TRUE) +
  geom_point(size = 2.2, color = "black") +
  labs(
    title = "Event Study: Electrification Impact on Establishments",
    x = "Years Since Electrification",
    y = "Coefficient"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )


p_est_bw



# --- 2. Prepare Employees plot Data -------------------------
# plot: Employees
plot_emp <- plot_df %>%
  filter(outcome == "Employees")

p_emp_bw <- ggplot(plot_emp, aes(x = time_since, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1, color = "gray40", linewidth = 0.6, na.rm = TRUE) +
  geom_point(size = 2.2, color = "black") +
  labs(
    title = "Event Study: Electrification Impact on Total Employment",
    x = "Years Since Electrification",
    y = "Coefficient"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )

p_emp_bw





# --- 6. Display --------------------------------------------------------
p_est_bw
p_emp_bw

# --- 7. Save -----------------------------------------------------------
ggsave(file.path(output_path, "plot"
                 , "eventstudy_establishments_bw.png"),
       p_est_bw, width = 8, height = 6, dpi = 400, bg = "white", scale = 1)

ggsave(file.path(output_path, "plot", "eventstudy_employees_bw.png"),
       p_emp_bw, width = 8, height = 6, dpi = 400, bg = "white", scale = 1)











