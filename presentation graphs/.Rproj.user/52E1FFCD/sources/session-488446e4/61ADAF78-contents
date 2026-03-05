

##########################
#Author: Xiaoming Zhang
#Date of last modification: 07182024
#purpose:NISR establishment census analysis
############################


#library----
# install.packages("plm")

pacman::p_load(fixest, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, plm, haven, stringr, modelsummary, kableExtra, stargazer, lfe)


#read file----

if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/EAQIP/datawork/Historical data"
)

rwa_regress <- read_xlsx(path = file.path(data_path, "rwa_regress_0907.xlsx"))

rwa_regress_village <- read_xlsx(path = file.path(data_path, "rwa_regress_villagelevel.xlsx"))


#barplot----

bar_plot <- rwa_regress_village %>% 
  mutate()







#Just means----

descriptive <- rwa_event %>% 
  group_by(
    year, status
  ) %>% 
  summarise(
    num_establishment = sum(num_establishment),
    total_employee = sum(total_employee)
  )


filter <- descriptive %>% 
  filter(status %in% c("elec12_14", "elec15_17", "never_elec")) %>% 
  mutate(
    status = case_when(
      status == "elec12_14" ~ "2012-2014",
      status == "elec15_17" ~ "2015-2017",
      status == "never_elec" ~ "not yet electrified"
    )
  ) 




ggplot(filter, aes(x = as.factor(year), y = num_establishment, color = status, group = status)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = num_establishment), vjust = -0.5, size = 6) +  # Add data labels
  labs(
    title = "Number of Establishments by Electrification Status",
    x = "Year",
    y = "Number of Establishments",
    color = "Electrification Status"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 26),  # General text size
    axis.title.x = element_text(size = 26),  # X-axis label size
    axis.title.y = element_text(size = 26),  # Y-axis label size
    axis.text = element_text(size = 28),
    plot.title = element_text(size = 35) 
  )

ggplot(filter, aes(x = as.factor(year), y = total_employee, color = status, group = status)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = num_establishment), vjust = -0.5, size = 6) +  # Add data labels
  labs(
    title = "Number of Employees by Electrification Status",
    x = "Year",
    y = "Number of Establishments",
    color = "Electrification Status"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 26),  # General text size
    axis.title.x = element_text(size = 26),  # X-axis label size
    axis.title.y = element_text(size = 26),  # Y-axis label size
    axis.text = element_text(size = 28),
    plot.title = element_text(size = 35) 
  )


#Event Study----

rwa_event <- rwa_regress %>% 
  filter(year == 2011 | year == 2014 | year == 2017 | year == 2020)



##Num establishment----

#12_14
elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )  

table(elec12_14$year)

establishment12_14 <- felm(num_establishment ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|village_id + year, cluster = "village_id" ,data = elec12_14)


summary(establishment12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )


establishment15_17 <- felm(num_establishment ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|village_id +year, cluster = "village_id",data = elec15_17)



##18_20
elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

establishment18_20 <- felm(num_establishment ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|village_id +year, cluster = "village_id",data = elec18_20)


##21_22
elec21_22 <- rwa_event %>% 
  filter(elec21_22 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

establishment21_22 <- felm(num_establishment ~ p_3_2011:elec21_22 + p_2_2014:elec21_22 + p0_2020:elec21_22|village_id +year, cluster = "village_id",data = elec21_22)

summary(establishment21_22)

establishment <- list(
  `For 2014` = establishment12_14,
  `For 2017` = establishment15_17,
  `For 2020` = establishment18_20
)




stargazer(
  establishment,
  title = "Event Study Number of Establishments as outcome",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)




##Graph====



library(broom)
summary(establishment12_14)
summary(establishment15_17)
summary(establishment18_20)
summary(establishment21_22)

# Extract coefficients and standard errors from each model
coeff12_14 <- tidy(establishment12_14) %>% mutate(Period = "2012-2014")
coeff15_17 <- tidy(establishment15_17) %>% mutate(Period = "2015-2017")
coeff18_20 <- tidy(establishment18_20) %>% mutate(Period = "2018-2020")
coeff21_22 <- tidy(establishment21_22) %>% mutate(Period = "2021-2022")

# Combine the coefficients
coefficients <- bind_rows(coeff12_14, coeff15_17, coeff18_20, coeff21_22)

# Filter out intercepts and irrelevant terms, and create a 'Year' variable
coefficients <- coefficients %>% 
  filter(!grepl("Intercept", term)) %>%
  mutate(
    Year = case_when(
      grepl("p0_2014", term) ~ 2014,
      grepl("p0_2017", term) ~ 2017,
      grepl("p1_2017", term) ~ 2017,
      grepl("p0_2020", term) ~ 2020,
      grepl("p1_2020", term) ~ 2020,
      grepl("p2_2020", term) ~ 2020,
      grepl("p_2_2011", term) ~ 2011,
      grepl("p_3_2011", term) ~ 2011,
      grepl("p_2_2014", term) ~ 2014
    )
  ) %>% 
  mutate(
    Year = as.factor(Year)
  ) %>% 
  complete(
    Period, Year, fill = list(estimate = 0, std.error = 0, statistic = 0, p.value = 0)
  )

View(coefficients)

# Add significance stars based on p-values
coefficients <- coefficients %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ) # Combine estimate with stars
  ) %>% 
  mutate(
    significance = ifelse(is.na(term), "", significance),
    label = paste0(round(estimate, 2), significance) 
  )


##2012-2017----
coefficients_filtered <- coefficients %>% 
  filter(Period == "2015-2017" | Period == "2012-2014") %>%
  mutate(Period = case_when(
    Period == "2015-2017" ~ "2015-2017 vs not yet electrified",
    Period == "2012-2014" ~ "2012-2014 vs not yet electrified"
  )) %>% 
  


# Plot with ggplot
ggplot(coefficients_filtered, aes(x = Year, y = estimate, color = Period, group = Period)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +  
  geom_label(aes(label = label), fill = "white", color = "black", size = 9, label.padding = unit(0.2, "lines")) +  # Adding labels with significance stars
  labs(
    title = "Difference in Number of Establishments",
    x = "Year",
    y = "Estimate",
    color = "Electrification Period"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 26),  # General text size
    axis.title.x = element_text(size = 26),  # X-axis label size
    axis.title.y = element_text(size = 26),  # Y-axis label size
    axis.text = element_text(size = 28),
    plot.title = element_text(size = 35) 
  )






coefficients12_14 <- coefficients_filtered %>% 
  filter(Period == "2012-2014 vs not yet electrified")


# Plot with ggplot
ggplot(coefficients12_14, aes(x = Year, y = estimate, color = Period, group = Period)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +  
  geom_label(aes(label = label), fill = "white", color = "black", size = 9, label.padding = unit(0.2, "lines")) +  # Adding labels with significance stars
  labs(
    title = "Difference in Number of Employees",
    x = "Year",
    y = "Estimate",
    color = "Electrification Period"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 26),  # General text size
    axis.title.x = element_text(size = 26),  # X-axis label size
    axis.title.y = element_text(size = 26),  # Y-axis label size
    axis.text = element_text(size = 28),
    plot.title = element_text(size = 35) 
  )



coefficients15_17 <- coefficients_filtered %>% 
  filter(Period == "2015-2017 vs not yet electrified")


# Plot with ggplot
ggplot(coefficients15_17, aes(x = Year, y = estimate, color = Period, group = Period)) +
  geom_point(size = 3) +
  geom_line(size = 1, color = "#00BFC4") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2, color = "#00BFC4") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +  
  geom_label(aes(label = label), fill = "white", color = "black", size = 9, label.padding = unit(0.2, "lines")) +  # Adding labels with significance stars
  labs(
    title = "Difference in Number of Employees",
    x = "Year",
    y = "Estimate",
    color = "Electrification Period"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 26),  # General text size
    axis.title.x = element_text(size = 26),  # X-axis label size
    axis.title.y = element_text(size = 26),  # Y-axis label size
    axis.text = element_text(size = 28),
    plot.title = element_text(size = 35) 
  )





##Total_employee ----

##12-14
elec12_14 <- rwa_event %>% 
  filter(elec12_14 == 1 | never_elec == 1) %>% 
  mutate(
    p0_2014 = ifelse(year == 2014, 1, 0),
    p1_2017 = ifelse(year == 2017, 1, 0),
    p2_2020 = ifelse(year == 2020, 1, 0)
  )  

table(elec12_14$year)

employee12_14 <- felm(total_employee ~ p0_2014:elec12_14 + p1_2017:elec12_14 + p2_2020:elec12_14|village_id + year, cluster = "village_id",  data = elec12_14)

summary(employee12_14)

##15_17
elec15_17 <- rwa_event %>% 
  filter(elec15_17 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p0_2017 = ifelse(year == 2017, 1, 0),
    p1_2020 = ifelse(year == 2020, 1, 0)
  )


employee15_17 <- felm(total_employee ~ p_2_2011:elec15_17 + p0_2017:elec15_17 + p1_2020:elec15_17|village_id +year, cluster = "village_id",  data = elec15_17)

summary(employee15_17)


##18_20
elec18_20 <- rwa_event %>% 
  filter(elec18_20 == 1 | never_elec == 1) %>% 
  mutate(
    p_3_2011 = ifelse(year == 2011 , 1, 0),
    p_2_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

employee18_20 <- felm(total_employee ~ p_3_2011:elec18_20 + p_2_2014:elec18_20 + p0_2020:elec18_20|village_id +year, cluster = "village_id",  data = elec18_20)


##21_22
elec21_22 <- rwa_event %>% 
  filter(elec21_22 == 1 | never_elec == 1) %>% 
  mutate(
    p_2_2011 = ifelse(year == 2011 , 1, 0),
    p_1_2014 = ifelse(year == 2014, 1, 0),
    p0_2020 = ifelse(year == 2020, 1, 0)
  )

employee21_22 <- felm(total_employee ~ p_2_2011:elec21_22 + p_1_2014:elec21_22 + p0_2020:elec21_22|village_id +year, cluster = "village_id",  data = elec21_22)

summary(employee21_22)


summary(employee12_14)
summary(employee15_17)
summary(employee18_20)

employee <- list(
  `For 2014` = employee12_14,
  `For 2017` = employee15_17,
  `For 2020` = employee18_20
)



stargazer(
  employee,
  title = "Event Study Number of Total employee as outcome",
  stars = TRUE,
  notes = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
)


modelsummary(
  employee,
  output = "huxtable",
  type = "html",
  title = "Event Study Total Employee as outcome ",
  stars = TRUE,
  note = "Exclude districts Ngororero, Nyabihu, Nyamasheke, Rubavu"
  # out = file.path(output_path, "es_ntl.html")
)

##Graph====

library(broom)

# Extract coefficients and standard errors from each model
coeff12_14 <- tidy(employee12_14) %>% mutate(Period = "2012-2014")
coeff15_17 <- tidy(employee15_17) %>% mutate(Period = "2015-2017")
coeff18_20 <- tidy(employee18_20) %>% mutate(Period = "2018-2020")
coeff21_22 <- tidy(employee21_22) %>% mutate(Period = "2021-2022")


# Combine the coefficients
coefficients <- bind_rows(coeff12_14, coeff15_17, coeff18_20, coeff21_22)

# Filter out intercepts and irrelevant terms, and create a 'Year' variable
coefficients <- coefficients %>% 
  filter(!grepl("Intercept", term)) %>%
  mutate(
    Year = case_when(
      grepl("p0_2014", term) ~ 2014,
      grepl("p0_2017", term) ~ 2017,
      grepl("p1_2017", term) ~ 2017,
      grepl("p0_2020", term) ~ 2020,
      grepl("p1_2020", term) ~ 2020,
      grepl("p2_2020", term) ~ 2020,
      grepl("p_2_2011", term) ~ 2011,
      grepl("p_3_2011", term) ~ 2011,
      grepl("p_2_2014", term) ~ 2014,
      grepl("p_1_2014", term) ~ 2014
      
    )
  ) %>% 
  mutate(
    Year = as.factor(Year)
  ) %>%
  complete(
    Period, Year, fill = list(estimate = 0, std.error = 0, statistic = 0, p.value = 0)
  )

# Add significance stars based on p-values
coefficients <- coefficients %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ) # Combine estimate with stars
  ) %>% 
  mutate(
    significance = ifelse(is.na(term), "", significance),
    label = paste0(round(estimate, 2), significance) 
  )

##2012-2017----
coefficients_filtered <- coefficients %>% 
  filter(Period == "2015-2017" | Period == "2012-2014") %>%
  mutate(Period = case_when(
    Period == "2015-2017" ~ "2015-2017 vs not yet electrified",
    Period == "2012-2014" ~ "2012-2014 vs not yet electrified"
  ))


coefficients12_14 <- coefficients_filtered %>% 
  filter(Period == "2012-2014 vs not yet electrified")



# Plot with ggplot
ggplot(coefficients_filtered, aes(x = Year, y = estimate, color = Period, group = Period)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +  
  geom_label(aes(label = label), fill = "white", color = "black", size = 9, label.padding = unit(0.2, "lines")) +  # Adding labels with significance stars
  labs(
    title = "Difference in Number of Employees",
    x = "Year",
    y = "Estimate",
    color = "Electrification Period"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 26),  # General text size
    axis.title.x = element_text(size = 26),  # X-axis label size
    axis.title.y = element_text(size = 26),  # Y-axis label size
    axis.text = element_text(size = 28),
    plot.title = element_text(size = 35) 
  )


coefficients12_14 <- coefficients_filtered %>% 
  filter(Period == "2012-2014 vs not yet electrified")


# Plot with ggplot
ggplot(coefficients12_14, aes(x = Year, y = estimate, color = Period, group = Period)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +  
  geom_label(aes(label = label), fill = "white", color = "black", size = 9, label.padding = unit(0.2, "lines")) +  # Adding labels with significance stars
  labs(
    title = "Difference in Number of Employees",
    x = "Year",
    y = "Estimate",
    color = "Electrification Period"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 26),  # General text size
    axis.title.x = element_text(size = 26),  # X-axis label size
    axis.title.y = element_text(size = 26),  # Y-axis label size
    axis.text = element_text(size = 28),
    plot.title = element_text(size = 35) 
  )



coefficients15_17 <- coefficients_filtered %>% 
  filter(Period == "2015-2017 vs not yet electrified")


# Plot with ggplot
ggplot(coefficients15_17, aes(x = Year, y = estimate, color = Period, group = Period)) +
  geom_point(size = 3) +
  geom_line(size = 1, color = "#00BFC4") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2, color = "#00BFC4") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +  
  geom_label(aes(label = label), fill = "white", color = "black", size = 9, label.padding = unit(0.2, "lines")) +  # Adding labels with significance stars
  labs(
    title = "Difference in Number of Employees",
    x = "Year",
    y = "Estimate",
    color = "Electrification Period"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 26),  # General text size
    axis.title.x = element_text(size = 26),  # X-axis label size
    axis.title.y = element_text(size = 26),  # Y-axis label size
    axis.text = element_text(size = 28),
    plot.title = element_text(size = 35) 
  )

