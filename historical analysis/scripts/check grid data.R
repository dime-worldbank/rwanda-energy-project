##########################
#Author: Xiaoming Zhang
#Date: 01302024
#purpose: Comparing different nightlight data, choosing the harmonized one, and running Event Study 
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

#Read wide data----

rwa_wide <- read_xlsx(path = file.path(data_path, "ntl_wide_92_21(connect11&22+rd).xlsx"))

#Select for 2011
rwa_2011 <- rwa_wide %>% 
  select(Village_ID, connect11_mv, `2011` , road) %>% 
  mutate(connect11_mv = as.factor(connect11_mv)) %>% 
  rename(`2011_ntl_value` = `2011`)
       

# summary(rwa_2011$`2011`)
# 
# ggplot(rwa_2011, aes(x = `2011`)) +
#   geom_histogram(fill = "lightblue", color = "black", bins = 30, alpha = 0.7) +
#   labs(title = "Distribution of 2011", x = "2011 Values", y = "Frequency")
# 
# quantile(rwa_2011$`2011`, 0.75)


mv_11_check <- lm(`2011_ntl_value` ~ connect11_mv + road, data = rwa_2011)
# 
# summary(mv_11_check)
# 
# stargazer(
#   mv_11_check,
#   type = "html",
#   title = "Table 1: 2011 MV line and 2011 nightlights",
#   out = "mv11_check.html")


#select for 2022-----


viirs <- read_xlsx(path = file.path(data_path, "Nightlight Data/viirs_2014-2023(monthly&yearly).xlsx"))

viirs2022 <- viirs %>% 
  select(Village_ID, `2022`)

rwa_2022 <- rwa_wide %>% 
  select(Village_ID, road, connect22_lv, connect22_mv)

rwa_2022 <- left_join(rwa_2022, viirs2022, by = c("Village_ID"))

rwa_2022 <- rwa_2022 %>% 
  mutate(`2022` = ifelse(`2022` > quantile(`2022`, 0.99), quantile(`2022`, 0.99), `2022`))

summary(rwa_2022$`2022`)

#Checking 2022 on 2021----

rwa_2022 <- rwa_wide %>% 
  rename(`2021_ntl_value` = `2021`)

mv_21_check <- lm(`2021_ntl_value` ~ connect22_mv + road, data = rwa_2022)

# summary(mv_21_check)
# 
# stargazer(
#   mv_21_check,
#   type = "html",
#   title = "Table 2: 2022 MV line and 2021 nightlights  ",
#   out = "mv21_mvcheck.html")


lv_21_check <- lm(`2021_ntl_value` ~ connect22_lv + road, data = rwa_2022)

# summary(lv_21_check)

# stargazer(
#   mv_21_check,
#   type = "html",
#   title = "Table 3: 2022 LV line and 2021 nightlights  ",
#   out = "mv21_lvcheck.html")





















#Dropping 2011 connected----

rwa_wide.1 <- rwa_wide %>% 
  filter(connect11_mv == 0) %>% 
  rename(`2021_ntl_value` = `2021`)

mv_21_check_fil <- lm(`2021_ntl_value` ~ connect22_mv + road, data = rwa_wide.1)

# summary(mv_21_check_fil)
# 
# stargazer(
#   mv_21_check_fil,
#   type = "html",
#   title = "Table 5: 2022 MV line and 2021 nightlights dropping 2011 connections ",
#   out = "mv21_check_fil.html")


stargazer(
  mv_11_check,
  mv_21_check,
  mv_21_check_fil,
  lv_21_check,
  column.labels = c("2011 MV line", "2022 MV line",  "2022 MV filtering 2011 connections", "2022 LV line"),
  type = "html",
  title = "Table 1: MV line and nightlights",
  out = "grid_check.html"
)















#Distribution----

ggplot(rwa_2022, aes(x = `2022`)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30, alpha = 0.7) +
  labs(title = "Distribution of 2022", x = "2011 Values", y = "Frequency")



#Diff in diff----


rwa_long_did <- rwa_long %>% 
  filter( connect11_mv  == 0 & Year == c(2011:2021)) %>% 
  mutate(
    treated = ifelse(connect22_mv== 1, 1, 0),
    numYear = as.numeric(Year)-2011,
    Year = as.numeric(Year)
  )


###regression----


didrwa <- lm(Value ~ numYear*connect22_mv + connect22_mv + numYear, data = rwa_long_did)

summary(didrwa)

stargazer(
  didrwa,
  type = "html",
  title = "Table: Difference in differences(2022) 2011-2021",
  out = "DiD2022mv.html")

# Measure the before-and-after change in
# outcomes for the program participants,
# then subtract the before-and-after change
# in outcomes of the non-participants to
# find the relative change in outcomes for
# program participants.

rwa_long21 <- rwa_wide %>% 
  pivot_longer(
    cols = `2021`,
    names_to = Value
  )

?pivot_longer()


did <- lm( `2021` ~ connect22_mv + connect11_mv + connect11_mv*connect22_mv, data = rwa_wide)

summary(did)

stargazer(
  did,
  type = "html",
  title = "Table: Difference in differences",
  out = "DiD2022mv.html")


















#Try the harmonization maybe?----

viirs_har <- viirs %>% 
  select(Village_ID, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`) %>% 
  rename(
    `2014_viirs` = `2014`,
    `2015_viirs` = `2015`,
    `2016_viirs` = `2016`,
    `2017_viirs` = `2017`,
    `2018_viirs` = `2018`,
    `2019_viirs` = `2019`,
    `2020_viirs` = `2020`,
    `2021_viirs` = `2021`,
    `2022_viirs` = `2022`,
    `2023_viirs` = `2023`
  )

rwa_wide_har <- left_join(rwa_wide, viirs_har, by = c("Village_ID"))

har20_21 <- lm(`2021` ~ `2020`, data = rwa_wide_har)
summary(har20_21)

har20_21 <- lm(`2021_viirs` ~ `2020_viirs`, data = rwa_wide_har)
summary(har20_21)

har22_21 <- 






