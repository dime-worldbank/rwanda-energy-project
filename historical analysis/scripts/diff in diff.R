
##########################
#Author: Xiaoming Zhang
#Date: 02272024
#purpose:Difference in difference analysis 
############################



pacman::p_load(raster,exactextractr,dplyr, rgdal,here,ggplot2,sf,tidyr,readxl,writexl, stringr, stargazer)

getwd()

#ReadDropbox----

if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/datawork/Historical data"
)


# rwa_wide----
rwa_wide <- read_xlsx(path = file.path(data_path, "ntl_wide_92_21(connect11&22+rd).xlsx"))


#0227 diff in diff----


#2011----

rwa_wide <- read_xlsx(path = file.path(data_path, "ntl_wide_92_21(connect11&22+rd).xlsx"))

rwa_long <- rwa_wide %>% 
  pivot_longer(
    cols = matches("^(19|20)"),  
    names_to = "Year",
    values_to = "Value"
  )


rwa_long_did <- rwa_long %>% 
  filter(Year >= 2011) %>% 
  mutate(year_2021 = ifelse(Year == 2021, 1, 0))

after_2011 <- lm(Value ~ connect11_mv + year_2021*connect11_mv, data = rwa_long_did)

summary(after_2011)

stargazer(
  after_2011,
  type = "html",
  title = "Table: Difference in differences",
  out = "DiD2011.html")


#Mission_graph----


rwa_long_did11 <- rwa_long %>% 
  filter(Year %in% c("2011", "2021")) %>% 
  mutate(year_2021 = ifelse(Year == 2021, 1, 0))

after_2011_mission <- lm(Value ~ connect11_mv + year_2021*connect11_mv, data = rwa_long_did11)

summary(after_2011_mission)

stargazer(
  after_2011_mission,
  type = "html",
  title = "Table: Difference in differences",
  out = "DiD2011_mission.html")













#pre trend----



rwa_long_did_pre <- rwa_long %>% 
  filter(Year < 2011) %>% 
  mutate(year_2021 = ifelse(Year == 2021, 1, 0))

pre_trend <- lm(Value ~ connect11_mv, data = rwa_long_did_pre)

summary(did_pre)



stargazer(
  pre_trend, after_2011,
  column.labels = c("pre 2011 trend", "post 2011 connection"),
  type = "html",
  title = "Table: Difference in differences",
  out = "DiD2011(pre).html")

?stargazer()


#2022-----


long_2022 <- rwa_long %>% 
  # filter(Year >= 2011) %>%
  mutate(year_2021 = ifelse(Year == 2021, 1, 0))

##mv----
mv_2022 <- lm(Value ~ connect22_mv + year_2021 + connect11_mv + year_2021*connect22_mv, data = long_2022)

summary(mv_2022)

##lv----
lv_2022 <- lm(Value ~ connect22_lv + year_2021 + connect11_mv + year_2021*connect22_lv, data = long_2022)

summary(lv_2022)

stargazer(
  mv_2022,
  type = "html",
  title = "Table: Difference in differences (2022 MV line)",
  out = "DiD22MV_notfilter2011.html")


stargazer(
  lv_2022,
  type = "html",
  title = "Table: Difference in differences (2022 LV line)",
  out = "DiD22LV_notfilter2011.html")



# calculate the means
cohort_means <- rwa_long %>% 
  group_by(Year) %>% 
  summarize(cohort_mean = mean(Value, na.rm = TRUE)) 

cohort_means <- cohort_means %>% 
  ungroup()

### plot the means----

ggplot(data = cohort_means, aes(x = Year, y = cohort_mean, group = Year)) + 
  geom_point() + 
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +  # Add a smooth line
  labs(x = "Year", y = "Avg Nightlight Value") +
  theme_classic() + 
  labs(title = "Time Series Rwanda 1992- 2021")


ggplot(data = cohort_means, aes(x = Year, y = cohort_mean, group = 1)) + 
  geom_point() + 
  geom_line() +
  # geom_smooth(method = "loess", se = FALSE) +  # Add a smooth line
  labs(x = "Year", y = "Avg Nightlight Value") +
  theme_classic() + 
  labs(title = "Time Series Rwanda 1992-2021")


ggplot(data = cohort_means, aes(x = Year, y = cohort_mean, group = 1)) + 
  geom_point() + 
  stat_smooth(method = "loess", se = FALSE) +
  geom_text(aes(x = Inf, y = Inf, label = sprintf("y = %.2fx + %.2f", coef(geom_smooth(method = "loess", se = FALSE))[1], coef(geom_smooth(method = "loess", se = FALSE))[2])),
            hjust = 1, vjust = 1, size = 3, parse = TRUE) +
  labs(x = "Year", y = "Avg Nightlight Value") +
  theme_classic() + 
  labs(title = "Time Series Rwanda 1992-2021")
