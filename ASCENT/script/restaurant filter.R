##########################
#Author: Xiaoming Zhang
#Date of last modification: 01142025
#purpose: EC graph
############################


#library----
# install.packages("plm")

pacman::p_load(fixest, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, plm, haven, stringr, modelsummary, kableExtra, stargazer, lfe)

if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/EAQIP/datawork/Historical data"
)

output_path <- file.path(
  DROPBOX, "Rwanda Energy/ASCENT/datawork"
)

#EC 2020----

isic_2020_capital_group <- read_xlsx(
  file.path(data_path, "Establishment census", "outputs0605", "2020", "group_long_2020(isic).xlsx"), 
  sheet = "capital_group"
)

isic_capital <- isic_2020_capital_group %>% 
  filter(isic_level1 == 9) %>% 
  mutate(village_id = as.character(village_id)) 


isic_capital <- left_join( isic_capital, rwa_villages, by = c("village_id" = "Village_ID"))

isic_capital <- isic_capital %>% 
  select(
    Province, District, Sector,  Cell, Name, village_id, everything()
  ) %>% 
  arrange(desc(n)) %>% 
  mutate(employed_capital_group = as.factor(employed_capital_group))

#Regression----

reg_model <- lm(total_employee ~ employed_capital_group, data = isic_capital)

# View the summary of the regression results
summary(reg_model)

stargazer(reg_model, type = "text", 
          title = "Regression Results: Employee Count and Capital Group", 
          out = "regression_output.txt")


#Filter for level 2----
isic_capital_filter <- isic_capital %>% 
  filter(employed_capital_group >= 2) %>% 
  arrange(desc(n))
  
write_xlsx(isic_capital_filter, path = file.path(output_path, "ISIC9_capital2(village).xlsx"))

#More than 2_cell----


isic_cell_2 <- isic_capital_filter %>% 
  group_by( Province, District, Sector, Cell, Cell_ID) %>% 
  summarise(
    n = sum(n), 
    total_employee = sum(total_employee)
  ) %>% 
  arrange(
    desc(n)
  )

write_xlsx(isic_cell_2, path = file.path(output_path, "ISIC9_capital2(cell).xlsx"))



cell <- isic_cell_2%>% 
  select(n) %>% 
  rename(`Number of Establishment` = n) %>% 
  group_by(`Number of Establishment`) %>% 
  summarise(n = n()) %>% 
  arrange(n)

# Create bar plot with continuous line for distribution
ggplot(cell, aes(x = `Number of Establishment`, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_smooth(aes(x = `Number of Establishment`, y = n), method = "loess", se = FALSE, color = "red") +
  labs(title = "Number of Restaurant Establishment by cell\n Capital level 2+ (more than 500.000) ", x = "Number of Establishment", y = "Frequency") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),  # Increase size of x and y axis labels
    axis.text = element_text(size = 12),   # Increase size of x and y axis tick labels
    plot.title = element_text(size = 16, hjust = 0.5),  # Increase plot title size and center it
    legend.title = element_text(size = 14),  # Increase legend title size (if applicable)
    legend.text = element_text(size = 12)    # Increase legend text size (if applicable)
  )


#Split by 1 and 3+----

sum(isic_capital$n[is.na(isic_capital$employed_capital_group)], na.rm = TRUE)
sum(isic_capital$n, na.rm = TRUE)

#279

isic_capital1 <- isic_capital %>% 
  filter(
    employed_capital_group == 1 | employed_capital_group == 2
  ) %>% 
  group_by( Province, District, Sector, Cell, Cell_ID) %>% 
  summarise(
    n = sum(n), 
    total_employee = sum(total_employee)
  ) %>% 
  arrange(
    desc(n)
  )


isic_capital1_establishment <- isic_capital1%>% 
  select(n) %>% 
  rename(`Number of Establishment` = n) %>% 
  group_by(`Number of Establishment`) %>% 
  summarise(n = n()) %>% 
  arrange(n)

ggplot(isic_capital1_establishment, aes(x = `Number of Establishment`, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_smooth(aes(x = `Number of Establishment`, y = n), method = "loess", se = FALSE, color = "red") +
  labs(title = "Number of Restaurant Establishment by cell\n Capital level 1&2 (less than 15.000.000) ", x = "Number of Establishment", y = "Frequency") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),  # Increase size of x and y axis labels
    axis.text = element_text(size = 12),   # Increase size of x and y axis tick labels
    plot.title = element_text(size = 16, hjust = 0.5),  # Increase plot title size and center it
    legend.title = element_text(size = 14),  # Increase legend title size (if applicable)
    legend.text = element_text(size = 12)    # Increase legend text size (if applicable)
  )

isic_capital1_employee<- isic_capital1 %>% 
  mutate(total_employee = ifelse(total_employee > 100, 100, total_employee)) %>% 
  group_by(total_employee) %>% 
  summarise(n = n()) %>% 
  arrange(n)

ggplot(isic_capital1_employee, aes(x = total_employee, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_smooth(aes(x = total_employee, y = n), method = "loess", se = FALSE, color = "red") +
  labs(title = "Number of Restaurant employee by cell\n Capital level 1&2 (less than 15.000.000)\n winsorized at 100 ", x = "Number of employee", y = "Frequency") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),  # Increase size of x and y axis labels
    axis.text = element_text(size = 12),   # Increase size of x and y axis tick labels
    plot.title = element_text(size = 16, hjust = 0.5),  # Increase plot title size and center it
    legend.title = element_text(size = 14),  # Increase legend title size (if applicable)
    legend.text = element_text(size = 12)    # Increase legend text size (if applicable)
  )



isic_capital3 <- isic_capital %>% 
  filter(
    employed_capital_group ==3 | employed_capital_group == 4
  )%>% 
  group_by( Province, District, Sector, Cell, Cell_ID) %>% 
  summarise(
    n = sum(n), 
    total_employee = sum(total_employee)
  ) %>% 
  arrange(
    desc(n)
  )


isic_capital3_establishment <- isic_capital3 %>% 
  select(n) %>% 
  rename(`Number of Establishment` = n) %>% 
  group_by(`Number of Establishment`) %>% 
  summarise(n = n()) %>% 
  arrange(n)
  
ggplot(isic_capital3_establishment, aes(x = `Number of Establishment`, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_smooth(aes(x = `Number of Establishment`, y = n), method = "loess", se = FALSE, color = "red") +
  labs(title = "Number of Restaurant Establishment by cell\n Capital level 3&4 (more than 15.000.000) ", x = "Number of Establishment", y = "Frequency") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),  # Increase size of x and y axis labels
    axis.text = element_text(size = 12),   # Increase size of x and y axis tick labels
    plot.title = element_text(size = 16, hjust = 0.5),  # Increase plot title size and center it
    legend.title = element_text(size = 14),  # Increase legend title size (if applicable)
    legend.text = element_text(size = 12)    # Increase legend text size (if applicable)
  )
  
isic_capital3_employee<- isic_capital3 %>% 
  mutate(total_employee = ifelse(total_employee > 50, 50, total_employee)) %>% 
  group_by(total_employee) %>% 
  summarise(n = n()) %>% 
  arrange(n)

ggplot(isic_capital3_employee, aes(x = total_employee, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_smooth(aes(x = total_employee, y = n), method = "loess", se = FALSE, color = "red") +
  labs(title = "Number of Restaurant employee by cell\n Capital level 3&4 (more than 15.000.000) \n winsorized at 50", x = "Number of employee", y = "Frequency") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),  # Increase size of x and y axis labels
    axis.text = element_text(size = 12),   # Increase size of x and y axis tick labels
    plot.title = element_text(size = 16, hjust = 0.5),  # Increase plot title size and center it
    legend.title = element_text(size = 14),  # Increase legend title size (if applicable)
    legend.text = element_text(size = 12)    # Increase legend text size (if applicable)
  )
  
#Cut off by 2----
# 
# 
# isic_capital1 <- isic_capital %>% 
#   filter(
#     employed_capital_group == 1 
#   ) %>% 
#   group_by( Province, District, Sector, Cell, Cell_ID) %>% 
#   summarise(
#     n = sum(n), 
#     total_employee = sum(total_employee)
#   ) %>% 
#   arrange(
#     desc(n)
#   )
# 
# 
# isic_capital1_establishment <- isic_capital1%>% 
#   select(n) %>% 
#   rename(`Number of Establishment` = n) %>% 
#   group_by(`Number of Establishment`) %>% 
#   summarise(n = n()) %>% 
#   arrange(n)
# 
# ggplot(isic_capital1_establishment, aes(x = `Number of Establishment`, y = n)) +
#   geom_bar(stat = "identity", fill = "skyblue", color = "black") +
#   geom_smooth(aes(x = `Number of Establishment`, y = n), method = "loess", se = FALSE, color = "red") +
#   labs(title = "Number of Restaurant Establishment by cell\n Capital level 1 (less than 500.000) ", x = "Number of Establishment", y = "Frequency") +
#   theme_minimal() +
#   theme(
#     axis.title = element_text(size = 14),  # Increase size of x and y axis labels
#     axis.text = element_text(size = 12),   # Increase size of x and y axis tick labels
#     plot.title = element_text(size = 16, hjust = 0.5),  # Increase plot title size and center it
#     legend.title = element_text(size = 14),  # Increase legend title size (if applicable)
#     legend.text = element_text(size = 12)    # Increase legend text size (if applicable)
#   )
# 
# isic_capital1_employee<- isic_capital1 %>% 
#   mutate(total_employee = ifelse(total_employee > 100, 100, total_employee)) %>% 
#   group_by(total_employee) %>% 
#   summarise(n = n()) %>% 
#   arrange(n)
# 
# ggplot(isic_capital1_employee, aes(x = total_employee, y = n)) +
#   geom_bar(stat = "identity", fill = "skyblue", color = "black") +
#   geom_smooth(aes(x = total_employee, y = n), method = "loess", se = FALSE, color = "red") +
#   labs(title = "Number of Restaurant employee by cell\n Capital level 1 (less than 500.000)\n winsorized at 100 ", x = "Number of employee", y = "Frequency") +
#   theme_minimal() +
#   theme(
#     axis.title = element_text(size = 14),  # Increase size of x and y axis labels
#     axis.text = element_text(size = 12),   # Increase size of x and y axis tick labels
#     plot.title = element_text(size = 16, hjust = 0.5),  # Increase plot title size and center it
#     legend.title = element_text(size = 14),  # Increase legend title size (if applicable)
#     legend.text = element_text(size = 12)    # Increase legend text size (if applicable)
#   )



isic_capital3 <- isic_capital %>% 
  filter(
    employed_capital_group == 3 | employed_capital_group == 4 |total_employee > 10 
  )%>% 
  group_by( Province, District, Sector, Cell, Cell_ID) %>% 
  summarise(
    n = sum(n), 
    total_employee = sum(total_employee)
  ) %>% 
  arrange(
    desc(n)
  )

write_xlsx(isic_capital3, path = file.path(output_path, "ISIC9_capital3+(cell).xlsx"))

isic_capital3_establishment <- isic_capital3 %>% 
  select(n) %>% 
  rename(`Number of Restaurant` = n) %>% 
  group_by(`Number of Restaurant`) %>% 
  summarise(n = n()) %>% 
  arrange(n)

ggplot(isic_capital3_establishment, aes(x = `Number of Restaurant`, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_smooth(aes(x = `Number of Restaurant`, y = n), method = "loess", se = FALSE, color = "red") +
  labs(title = "Number of Restaurant Establishment by cell\n Capital level 3&4 (more than 15 million)", x = "Number of Restaurant", y = "Frequency") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),  # Increase size of x and y axis labels
    axis.text = element_text(size = 12),   # Increase size of x and y axis tick labels
    plot.title = element_text(size = 16, hjust = 0.5),  # Increase plot title size and center it
    legend.title = element_text(size = 14),  # Increase legend title size (if applicable)
    legend.text = element_text(size = 12)    # Increase legend text size (if applicable)
  )

# isic_capital3_employee<- isic_capital3 %>% 
#   mutate(total_employee = ifelse(total_employee > 100, 100, total_employee)) %>% 
#   group_by(total_employee) %>% 
#   summarise(n = n()) %>% 
#   arrange(n)
# 
# ggplot(isic_capital3_employee, aes(x = total_employee, y = n)) +
#   geom_bar(stat = "identity", fill = "skyblue", color = "black") +
#   geom_smooth(aes(x = total_employee, y = n), method = "loess", se = FALSE, color = "red") +
#   labs(title = "Number of Restaurant employee by cell\n Capital level 3&4 (more than 15.000.000) \n winsorized at 50", x = "Number of employee", y = "Frequency") +
#   theme_minimal() +
#   theme(
#     axis.title = element_text(size = 14),  # Increase size of x and y axis labels
#     axis.text = element_text(size = 12),   # Increase size of x and y axis tick labels
#     plot.title = element_text(size = 16, hjust = 0.5),  # Increase plot title size and center it
#     legend.title = element_text(size = 14),  # Increase legend title size (if applicable)
#     legend.text = element_text(size = 12)    # Increase legend text size (if applicable)
#   )
# 
#   
  