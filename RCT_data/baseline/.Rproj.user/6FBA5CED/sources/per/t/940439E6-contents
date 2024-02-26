##############
#Author: Xiaoming Zhang
#Date: 1.19.2024
#Purpose: Scope construction, trials and errors for randomization
#############


pacman::p_load(tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr)

getwd()

#Dropbox----


if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

#Method One----
path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data/four_district_2401.xlsx"
)

#read file----
four_district_2401 <- read_xlsx(path)

#surveyed_customer <- st_read(dsn = path) 

#Scope specify---
four_district_2401 <- four_district_2401 %>% 
  mutate(scope_2401 = ifelse(district %in% c("Rutsiro", "Rusizi"), scope_1024, scope_2401))

#scope----

four_district_2401 <- four_district_2401 %>% 
  mutate(
    SAS = ifelse(nep_revision %in% c("SAS", "SAS 2023", "GE_Temporaly SAS", "Microgrid"), "SAS", "non SAS")
  )

four_district_2401 <- four_district_2401 %>% 
  mutate(any_offgrid = case_when(
    et_sum !=0 | priority_0 == 0 ~ "partial",
    .default = "newly"
  ))

four_district_2401 <- four_district_2401 %>% 
  mutate(
    meter = ifelse(meter_eucl > 0.2*total_hh, "meter", "no meter")
  )

table(four_district_2401$meter)

# four_district_2401.1 <- four_district_2401 %>% 
#   mutate(
#     meter = ifelse(meter_eucl > 10, "meter", "no meter")
#   )
# 
# table(four_district_2401.1$meter)

four_district_2401 <- four_district_2401 %>% 
  mutate(
    any_grid = case_when(
      grid_status %in% c("newly") & nep_revision%in% c("GE", "Fill In") & meter %in% c("no meter")  ~ "newly",
      .default = "partial"
    )
  )

four_district_2401 <- four_district_2401 %>% 
    mutate(
      status = case_when(
        any_grid == "newly" & any_offgrid == "newly" ~ "newly",
        .default = "partial"
      )
    )


four_district_2401 <- four_district_2401 %>% 
  mutate(
    status = ifelse(ubudehe_1 < 20, "partial", status)
  )

#three district----

three_scope_2401 <- four_district_2401 %>% 
  filter(district %in% c("Karongi", "Rutsiro", "Rulindo") & scope_2401==1)

table(three_scope_2401$status)

three_scope_newly <- three_scope_2401 %>% 
  filter(status == "newly")

#Make graph----

meter <- three_scope_2401 %>% 
  mutate(meter_percent = round(meter_eucl/total_hh,2)) 
  

#meter distribution----

meter_graph <- meter %>% 
  filter(meter_percent < 0.3) %>% 
  select(meter_percent)

meter_graph <- meter_graph %>% 
  group_by(meter_percent) %>% 
  summarise(n_village = n())



p1 <- ggplot(meter_graph, aes(x =meter_percent)) +
  geom_bar(fill = "lightblue", color = "black") +
  geom_vline(xintercept = c(0.15, 0.2, 0.25), linetype = "dashed", color = "red") +
  labs(
    title = "Meter Percentage Distribution in Three District Main Scope",
    x = "Meter Percentage",
    y = "Frequency"
  )



#To graph again----
four_district_2401.1 <- four_district_2401 %>% 
  mutate(
    any_grid = case_when(
      grid_status %in% c("newly") & nep_revision%in% c("GE", "Fill In")   ~ "newly",
      .default = "partial"
    )
  )
four_district_2401.1 <- four_district_2401.1 %>% 
  mutate(any_offgrid = case_when(
    et_sum !=0 | priority_0 == 0 ~ "partial",
    .default = "newly"
  ))

four_district_2401.1 <- four_district_2401.1 %>% 
  mutate(
    status = case_when(
      any_grid == "newly" & any_offgrid == "newly" ~ "newly",
      .default = "partial"
    )
  )


four_district_2401.1 <- four_district_2401.1 %>% 
  mutate(
    status = ifelse(ubudehe_1 < 20, "partial", status)
  )

#three district----

three_scope_2401.1 <- four_district_2401.1 %>% 
  filter(district %in% c("Karongi", "Rutsiro", "Rulindo") & scope_2401==1)

three_scope_2401.1 <- three_scope_2401.1 %>% 
  mutate(meter_percent = round(meter_eucl/total_hh, 2))

three_scope_2401.1 <- three_scope_2401.1 %>% 
  mutate(meter_percent = ifelse(meter_percent >1, round(meters_xy/total_hh,2), meter_percent))

table(three_scope_2401.1$status)

three_scope_group <- three_scope_2401.1 %>% 
  group_by(status) %>% 
  summarise(
    min = min(meter_percent),
    Q1 = quantile(meter_percent, 0.25),
    median = median(meter_percent),
    mean = mean(meter_percent),
    Q3 = quantile(meter_percent, 0.75),
    max = max(meter_percent),
    
  )

three_scope_2401.1 %>% 
  group_by(status) %>% 
  summary(
   meter_percent
  )

#Make graph----

meter <- three_scope_2401.1 %>% 
  filter(status == "newly") %>% 
  mutate(meter_percent = round(meter_eucl/total_hh,2)) 


#meter distribution----

meter_graph <- meter %>% 
  filter(meter_percent < 0.30 & ubudehe_1 >=20) 
  select(meter_percent)



p1 <- ggplot(meter_graph, aes(x =meter_percent)) +
  geom_bar(fill = "lightblue", color = "black") +
  geom_vline(xintercept = c(0.15, 0.2, 0.25), linetype = "dashed", color = "red") +
  labs(
    title = "Meter Percentage Distribution for Newly villages",
    x = "Meter Percentage(<30%)",
    y = "Frequency"
  )


table(three_scope_2401.1$status)

#Including more partial village----

partial <- three_scope_2401.1 %>% 
  filter(
    status == "partial" & grid_status == "newly" & any_offgrid == "newly" & ubudehe_1 >=20)


partial10 <- partial %>% 
  filter(meter_percent <=0.1 )

partial20 <- partial %>% 
  filter(meter_percent <=0.2 & meter_percent > 0.1)

###play with offgrid---

partial_offgrid <- three_scope_2401.1 %>% 
  filter(
    status == "partial" & nep_revision %in% c("GE", "Fill In") & ubudehe_1 >=20)


partial10_ <- partial %>% 
  filter(meter_percent <=0.1 )

partial20 <- partial %>% 
  filter(meter_percent <=0.2 & meter_percent > 0.1)











#NOT USED status-----

three_scope_2401 <- three_scope_2401 %>%
  mutate(
    meter = ifelse(meter_eucl > 10, "meter", "no meter")
    )


three_scope_2401 <- three_scope_2401.1 %>% 
  mutate(
    any_grid = case_when(
      grid_status %in% c("newly") & nep_revision%in% c("GE", "Fill In") & meter %in% c("no meter")  ~ "newly",
      .default = "partial"
    )
  )

three_scope_2401 <- three_scope_2401 %>% 
  mutate(
    status = case_when(
      any_grid == "newly" & any_offgrid == 0 ~ "newly",
      .default = "partial"
    )
  )

table(three_scope_2401$status)

#Comparison 

three_scope_2401 %>% 
  group_by(nep_revision) %>% 
  summarise(
    no_meter = sum(meter == "no meter"),
    yes_meter = sum(meter != "no meter")
  )

three_scope_2401 %>% 
  group_by(grid_status) %>% 
  summarise(
    no_meter = sum(meter == "no meter"),
    yes_meter = sum(meter != "no meter")
  )

#no grid no meter but not GE----


no_meter <- three_scope_2401 %>% 
  filter(meter == "no meter") %>% 
  summarise(
    `GE & Grid` = sum(nep_revision %in% c("GE") & grid_status %in% c("newly")),
    `GE & parial_grid` = sum(nep_revision %in% c("GE") & grid_status %in% c("partial")),
    `Fill In & Grid` = sum(nep_revision %in% c("Fill In") & grid_status %in% c("newly")),
    `Fill In & partial_grid` = sum(nep_revision %in% c("Fill In") & grid_status %in% c("partial")),
    `!GE/Fill In, newly grid` = sum(nep_revision != "GE" & nep_revision != "Fill In" & grid_status %in% c("newly"))
    )

no_meter.1 <- three_scope_2401 %>% 
  filter(meter == "no meter") %>% 
  summarise(
    GE = sum( nep_revision %in% c("GE", "Fill In")),
    newly_Grid = sum(grid_status %in% c("newly")),
    `GE&newly_Grid` = sum(nep_revision %in% c("GE", "Fill In") & grid_status %in% c("newly")),
    `!GE&newly_Grid` = sum(nep_revision != "GE" & nep_revision != "Fill In" & grid_status != "newly")
  )

three_scope_2401 %>% 
  filter(meter == "no meter") %>% 
  group_by(nep_revision) %>% 
    summarise(
      newly = sum(grid_status %in% c("newly")),
      partial = sum(grid_status %in% c("partial"))
    )


no_meter_grid <- three_scope_2401 %>% 
  filter(nep_revision != "GE" & nep_revision != "Fill In" & grid_status %in% c("newly") & meter == "no meter")


#36 villages relationship with et and priority list

no_meter_grid %>% 
  group_by(nep_revision) %>% 
  summarise(
    et = sum(et_sum > 0),
    prioriy_0 = sum(priority_0 ==1),
    priority_1 = sum(priority_1 == 1),
    priority_2 = sum(priority_2 == 1)
  )

no_meter_grid %>% 
  group_by(nep_revision) %>% 
  summarise(
   n = n()
  )

#the 23/36 villages that doesn't have et or priority, and is not microgrid----

no_meter_grid23 <- no_meter_grid %>% 
  filter(et_sum == 0 & priority_0 == 1 & nep_revision != "Microgrid")

no_meter_grid1 <- three_scope_2401 %>% 
  filter(nep_revision %in% c("GE", "Fill In") & grid_status == "partial" & meter == "no meter")

write_xlsx(no_meter_grid23, here("outputs", "offgrid_23villages.xlsx"))


#Mismatch of off grid----

###GE_temporaly SAS & Microgrid

off_grid.1 <- three_scope_2401 %>% 
  filter(nep_revision %in% c("GE_Temporaly SAS", "Microgrid"))

off_grid.1 %>% 
  group_by(nep_revision) %>% 
  summarise(
    et = sum(et_sum > 0),
    prioriy_0 = sum(priority_0 ==1),
    priority_1 = sum(priority_1 == 1),
    priority_2 = sum(priority_2 == 1)
  )


off_grid.2 <- three_scope_2401 %>% 
  filter(nep_revision %in% c("SAS", "SAS 2023"))

off_grid.2 %>% 
  group_by(nep_revision) %>% 
  summarise(
    et = sum(et_sum > 0),
    prioriy_0 = sum(priority_0 ==1),
    priority_1 = sum(priority_1 == 1),
    priority_2 = sum(priority_2 == 1)
  )

three_scope_2401 %>% 
  group_by(priority_0) %>% 
  summarise(
    et = sum(et_sum > 0),
    no_et = sum(et_sum == 0)
  )

table(three_scope_2401$priority_0)


#redo the off_grid status
table(three_scope_2401$status)

three_scope_2401 <- three_scope_2401 %>% 
  mutate(
  SAS = ifelse(nep_revision %in% c("SAS", "SAS 2023", "GE_Temporaly SAS", "Microgrid"), "SAS", "non SAS")
  )

table(three_scope_2401$SAS)

three_scope_2401.1 <- three_scope_2401 %>% 
  mutate(any_offgrid = case_when(
    et_sum !=0 | priority_0 == 0 | SAS == "SAS" ~ "partial",
    .default = "newly"
  ))

three_scope_2401.1 <- three_scope_2401.1 %>% 
  mutate(
    status = case_when(
      any_offgrid == "newly" & any_grid %in% c("newly") ~ "newly",
      .default = "partial"
    ) 
  )

table(three_scope_2401.1$status)
table(three_scope_2401$status)

offgrid_test <- three_scope_2401 %>% 
  select(village_id, any_offgrid, et_sum, nep_revision, SAS, priority_0)
offgrid_test.2 <- three_scope_2401.1 %>% 
  select(village_id, any_offgrid)

offgrid_test <- left_join(offgrid_test, offgrid_test.2, by = c("village_id"))

new_scope <- three_scope_2401 %>% 
  filter(status == "newly") %>% 
  group_by(district) %>% 
  summarise(
    village_number = n(),
    ubudehe_1 = sum(ubudehe_1)
  )


any_grid <- three_scope_2401 %>% 
  filter(any_grid == "newly")

table(any_grid$nep_revision)
any_all <- any_grid %>% 
  filter(et_sum == 0 & priority_0 == 1)


# revisit offgrid criteria 01192024

et_sale <- read_xlsx(here("data", "rbf_edcl_overview.xlsx"), sheet = "Sales")
et_pre<-read_xlsx(here("data", "rbf_edcl_overview.xlsx"), sheet = "Pre-registrations")

et_sale <- et_sale %>% 
  mutate(village_id = floor(as.numeric(`Household ID`)/10000))

et_pre <- et_pre %>% 
  mutate(village_id = floor(as.numeric(`Household ID`)/10000))



et_sale <- et_sale %>% 
  mutate(percent = round(`Subsidy (RWF)`/ (`Subsidy (RWF)` + `Subsidized Price (RWF)`), 2))

et_pre <- et_pre %>% 
  mutate(percent = round(`Subsidy (RWF)`/ (`Subsidy (RWF)` + `Subsidized Price (RWF)`), 2))

#Filter for 95----
et_pre_95 <- et_pre %>% 
  filter(percent >= 0.95)

et_sale_95 <- et_sale %>% 
  filter(percent >= 0.95)

et_sale_1 <- et_sale_95 %>% 
  filter(`Ubudehe category` == 1)

et_pre_1 <- et_pre_95 %>% 
  filter(`Ubudehe category` == 1)

#Six month ago----

et_sale_6month <- et_sale_1 

# %>%
#   mutate(`Pre-registration date` = as.Date(`Pre-registration date`)) %>%
#   filter(`Pre-registration date` >= Sys.Date() - months(6))


et_pre_6month <- et_pre_1 

# %>% 
#   mutate(`Pre-registration date` = as.Date(`Pre-registration date`)) %>%
#   filter(`Pre-registration date` >= Sys.Date() - months(6))


#ET in general

et_pre_group <- et_pre_6month %>% 
  group_by(village_id) %>% 
  summarise(count = n())

et_sale_group <- et_sale_6month %>% 
  group_by(village_id) %>% 
  summarise(count = n())


et_group <- full_join(et_sale_group, et_pre_group, by = c("village_id"))

et_group <- et_group %>% 
  mutate(count_sale = ifelse(is.na(count.x), 0, count.x),
         count_pre = ifelse(is.na(count.y), 0, count.y),
         et_sum = count_sale + count_pre)

et_group_join <- et_group %>% 
  select(village_id, count_sale, count_pre, et_sum) %>% 
  mutate(village_id = as.factor(village_id))

View(et_group_join) 

et_group_join <- et_group_join %>% 
  select(village_id, et_sum)


#Join with three_scope_


four_district_test <- left_join(four_district_2401, et_group_join, by = c("village_id"))

four_district_test <- four_district_test %>% 
  mutate(et_sum.y = ifelse(is.na(et_sum.y), 0, et_sum.y)) 


#Redo the randomization---


#scope



four_district_2401 <- four_district_2401 %>% 
  mutate(scope_2401 = ifelse(district %in% c("Karongi", "Rulindo"), scope_2401, scope_1024))

four_district_2401 <- four_district_2401 %>% 
  mutate(
    meter = ifelse(meter_eucl > 10, "meter", "no meter")
  )


four_district_2401 <- four_district_2401 %>% 
  mutate(
    any_grid = case_when(
      grid_status %in% c("newly") & nep_revision%in% c("GE", "Fill In") & meter %in% c("no meter")  ~ "newly",
      .default = "partial"
    )
  )


four_district_2401 <- four_district_2401 %>% 
  mutate(
    any_grid = case_when(
      grid_status %in% c("newly") & nep_revision%in% c("GE", "Fill In") & meter %in% c("no meter")  ~ "newly",
      .default = "partial"
    )
  )


four_district_2401 <- four_district_2401 %>% 
  mutate(
    SAS = ifelse(nep_revision %in% c("SAS", "SAS 2023", "GE_Temporaly SAS", "Microgrid"), "SAS", "non SAS")
  )

table(four_district_2401$SAS)

four_district_2401 <- four_district_2401 %>% 
  mutate(any_offgrid = case_when(
    et_sum !=0 | priority_0 == 0 | SAS == "SAS" ~ "partial",
    .default = "newly"
  ))

four_district_2401 <- four_district_2401 %>% 
  mutate(status = case_when(
    any_grid == "newly" & any_offgrid == "newly" ~ "newly",
    .default = "partial"
  ))

write_xlsx(four_district_2401, path = here("data", "four_district_2401.xlsx"))

three_scope_2401 <- four_district_2401 %>% 
  filter(district %in% c("Karongi", "Rulindo", "Rutsiro") & scope_2401 == 1)

table(three_scope_2401$status)

#Do the comparison----

new_scope <- three_scope_2401 %>% 
  filter(status == "newly") %>% 
  group_by(district) %>% 
  summarise(
    village_number = n(),
    ubudehe_1 = sum(ubudehe_1)
  )


three_scope_newly <- three_scope_2401 %>% 
  filter(status == "newly")



#At least 10 ubudehe villages----


three_scope_ubudehe10 <- three_scope_2401 %>% 
  mutate(
    status = ifelse(
      ubudehe_1 < 10, "partial", status
    )
  )

new_scope <- three_scope_ubudehe20 %>% 
  filter(status == "newly") %>% 
  group_by(district) %>% 
  summarise(
    village_number = n(),
    ubudehe_1 = sum(ubudehe_1)
  )

three_scope_ubudehe20 <- three_scope_2401 %>% 
  mutate(
    status = ifelse(
      ubudehe_1 <= 20, "partial", status
    )
  )







#0129Triple check----


four_district_2401.1 <- four_district_2401 %>% 
  mutate(any_offgrid = case_when(
    et_sum !=0 | priority_0 == 0 ~ "partial",
    .default = "newly"
  ))

four_district_2401.1 <- four_district_2401.1 %>% 
  mutate(
    any_grid = case_when(
      grid_status %in% c("newly") & nep_revision%in% c("GE", "Fill In")  ~ "newly",
      .default = "partial"
    )
  )

four_district_2401.1 <- four_district_2401.1  %>% 
  mutate(status = case_when(
    any_grid == "newly" & any_offgrid == "newly" ~ "newly",
    .default = "partial"
  ))


three_scope_2401.1 <- four_district_2401.1 %>% 
  filter(district %in% c("Karongi", "Rulindo", "Rutsiro") & scope_2401 == 1)

three_scope_2401.1 <- three_scope_2401.1 %>% 
  mutate(
    status = ifelse(ubudehe_1 <20, "partial", status)
  )

table(three_scope_2401.1$status)

three_scope_2401.1 <- three_scope_2401.1 %>% 
 mutate(meter_percent = round(meter_eucl/total_hh,2)) 

#Changing different criterias to see----

newly <- three_scope_2401.1 %>% 
 filter(status == "newly")

newly.1 <- newly %>% 
  filter(ubudehe_1 >= 40)

newly.2 <- newly.1 %>% 
  filter(meter_percent < 0.30)


newly.3 <- newly %>% 
  mutate(ubudehe_1_percent = round(ubudehe_1/total_hh, 2))

newly.3 <- newly.3 %>% 
  filter(ubudehe_1_percent > meter_percent)

newly.4 <- newly.3 %>% 
  filter(ubudehe_1 >40)

newly_2530 <- newly %>% 
  filter(meter_percent < 0.3 & meter_percent >=0.25)


newly_2530_path <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/outputs/newly_25-30percent.xlsx"
)

write_xlsx(
  newly_2530, path = newly_2530_path, 
)











#Do the comparison----

new_scope.1 <- three_scope_2401.1 %>% 
  filter(status == "newly") %>% 
  group_by(district) %>% 
  summarise(
    village_number = n(),
    ubudehe_1 = sum(ubudehe_1)
  )



#Graph the one village---


one_village <- rwa_villages %>% 
  filter(Village_ID == "41080506")

one_village_LV <- st_intersection(existing_LV, one_village)

rulindo_meter <- st_read(dsn = here("data", "eucl meter", "rulindo_meter.shp"))
rulindo_meter <- st_transform(rulindo_meter, crs = st_crs(one_village))
one_village_meter <- st_intersection(one_village, rulindo_meter)

one_village_LV <- st_transform(one_village_LV, crs = st_crs(one_village))
one_village_meter <- st_transform(one_village_meter, crs = st_crs(one_village))




base_one <- ggplot() +
  geom_sf(data = one_village)

base_one


existing_LV<- "blue"
meter_color <- "red"

one_surveyed_plot <- base_one +
  geom_sf(data = one_village_meter, aes(color = "Meters"), size = 0.5) +
  # geom_sf(data = karongi_HV, aes(color = "Existing"))+
  geom_sf(data = one_village_LV, aes(color = "Existing"))+
  # geom_sf(data = karongi_MV, aes(color = "Existing")) +
  geom_sf(data = karongi_LV_surveyed, aes(color = "Surveyed"))+
  scale_color_manual(
    values = c(`Existing` =`existing_LV`, `Meters` = `meter_color`),
    guide = guide_legend(title.position = "top")
  ) +
  theme_void() + 
  theme(plot.background = element_rect(fill = 'white', color = 'white') ) +
  labs(title = "Rulindo Village: 41080506")


one_surveyed_plot

write_xlsx(no_meter_grid1, path = here("outputs", "partial_1village.xlsx"))


#New 900 village priority list-----

#Dropbox----


path_off900 <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data/Additional Offgrid scope-NEP 2023 Final-Priorities.xlsx"
)

path_off <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data",
  "baseline/data/data/Offgrid Scope Priorities.xlsx"
)

#read file----
offgrid.2<- read_xlsx(path_off900)
offgrid.1<- read_xlsx(path_off)

if (all.equal(offgrid.1, offgrid.2)) {
  print("The data frames are the same.")
} else {
  print("The data frames are different.")
  print(all.equal(offgrid.1, offgrid.2))
}


#0131----

four_district_2401.1 <- four_district_2401 %>% 
  mutate(any_offgrid = case_when(
    et_sum !=0 | priority_0 == 0 ~ "partial",
    .default = "newly"
  ))

four_district_2401.1 <- four_district_2401.1 %>% 
  mutate(
    any_grid = case_when(
      grid_status %in% c("newly") & nep_revision%in% c("GE", "Fill In")  ~ "newly",
      .default = "partial"
    )
  )

four_district_2401.1 <- four_district_2401.1  %>% 
  mutate(status = case_when(
    any_grid == "newly" & any_offgrid == "newly" ~ "newly",
    .default = "partial"
  ))


four_district_2401.1  <- four_district_2401.1 %>% 
  mutate(
    status = ifelse(ubudehe_1 <20, "partial", status)
  )

four_district_2401.1 <- four_district_2401.1%>% 
  mutate(scope_2401 = ifelse(district %in% c("Rutsiro", "Rusizi"), scope_1024, scope_2401))

four_district_2401.1 <- four_district_2401.1 %>% 
  mutate(meter_percent = round(meter_eucl/total_hh, 2))
  
  
write_xlsx(four_district_2401.1, here("data", "four_district.xlsx"))
  
three_scope_2401.1 <- four_district_2401.1 %>% 
  filter(district %in% c("Karongi", "Rulindo", "Rutsiro") & scope_2401 == 1)


table(three_scope_2401.1$status)



#Check for the meter and no lines----

no_line <- three_scope_2401.1 %>% 
  filter(meter_eucl != 0 & length_lv == 0 )

no_line.1 <- no_line %>% 
  filter(any_offgrid == "newly")

no_bufferlin <- three_scope_2401.1 %>% 
  filter(meter_eucl!= 0 & grid_status == "newly")

path_noline <- file.path(
  DROPBOX,
  "Rwanda Energy/datawork/RCT_data/baseline/documentation/meter_no_lv.xlsx"
)




write_xlsx(no_line, path = path_noline)


#25 ubudehe_1----

three_scope_2401.2 <- three_scope_2401.1 %>% 
  filter(ubudehe_1 >= 25 & status == "newly" & meter_percent < 0.3)
