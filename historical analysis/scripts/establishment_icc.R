
##########################
#Author: Xiaoming Zhang
#Date: 02132024
#purpose:establishment census analysis
############################


#library----

pacman::p_load(tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, haven, stringr)


#read file----

if (Sys.getenv("USERNAME") == "wb614406"){
  DROPBOX <- file.path("C:/Users/wb614406/Dropbox")
}

data_path <- file.path(
  DROPBOX, "Rwanda Energy/datawork/Historical data"
)

nisr_2011 <- read_sav(file.path(data_path, "2011", "rec-2011-data-v2.sav"))
nisr_2014 <- read_sav(file.path(data_path, "2014", "rec-2014-data-v2.sav"))
nisr_2017 <- read_dta(file.path(data_path, "2017", "data-rwa-nisr-ec-2017_stata.dta"))
nisr_2020 <- read_dta(file.path(data_path, "2020", "Data-rwa-nisr-ec-2020_stata.dta"))

nisr14_sector<- read_xlsx(file.path(data_path, "Sector2014.xlsx"))
nisr17_sector <- read_xlsx(file.path(data_path, "Sector2017.xlsx"))
nisr20_sector <- read_xlsx(file.path(data_path, "Sector2020.xlsx"))

#Village_id for establishment 2014

nisr_2014$Village_ID <- substr(nisr_2014$key, 1, 8)

nisr_2014 <- nisr_2014 %>% 
  select(Village_ID, everything())


nisr_2011 <- nisr_2011 %>% 
  mutate(Village_ID = ID1*10000000 + ID2*1000000 + ID3*10000 + ID4*100 +ID5) %>% 
  select(Village_ID, everything())

n_distinct(nisr_2011$Village_ID)

nisr_2011 <- nisr_2011 %>% 
  mutate(sector_ID = ID1*1000 + ID2*100 + ID3 ) %>% 
  select(sector_ID, everything())


analysis_2011_sector <- nisr_2011 %>% 
  group_by(sector_ID) %>% 
  summarize(
    num_establishments = n(),
    total_worker = sum(ifelse(Total_emp2 != 999999, Total_emp2, 0)), 
    within_market = sum(S03 == 1, na.rm = TRUE),
    outside_market = sum(S03 == 2, na.rm = TRUE),
    status_working = sum(S04 == 1, na.rm = TRUE),
    status_temp_closed = sum(S04 == 2, na.rm = TRUE),
    status_perm_closed = sum(S04 == 3, na.rm = TRUE),
    household_sector = sum(S05 == 1, na.rm = TRUE),
    private_sector = sum(S05 == 2, na.rm = TRUE),
    public_sector = sum(S05 == 3, na.rm = TRUE),
    mixed_sector = sum(S05 == 4, na.rm = TRUE),
    cooperative_sector = sum(S05 == 5, na.rm = TRUE),
    private_education_institution = sum(S05 == 6, na.rm = TRUE),
    private_health_institution = sum(S05 == 7, na.rm = TRUE),  # Fixed this line
    local_npo = sum(S05 == 8, na.rm = TRUE),  # Fixed this line
    IO = sum(S05 == 9, na.rm = TRUE),
    head_office = sum(S15 == 1, na.rm = TRUE),
    single_unit_establishment = sum(S15 == 2, na.rm = TRUE),
    branch_national_enterprise = sum(S15 == 3, na.rm = TRUE),
    branch_international_enterprise = sum(S15 == 4, na.rm = TRUE)
  )

analysis_2011_village <- nisr_2011 %>% 
  group_by(Village_ID) %>% 
  summarize(
    num_establishments = n(),
    total_worker = sum(ifelse(Total_emp2 != 999999, Total_emp2, 0)), 
    within_market = sum(S03 == 1, na.rm = TRUE),
    outside_market = sum(S03 == 2, na.rm = TRUE),
    status_working = sum(S04 == 1, na.rm = TRUE),
    status_temp_closed = sum(S04 == 2, na.rm = TRUE),
    status_perm_closed = sum(S04 == 3, na.rm = TRUE),
    household_sector = sum(S05 == 1, na.rm = TRUE),
    private_sector = sum(S05 == 2, na.rm = TRUE),
    public_sector = sum(S05 == 3, na.rm = TRUE),
    mixed_sector = sum(S05 == 4, na.rm = TRUE),
    cooperative_sector = sum(S05 == 5, na.rm = TRUE),
    private_education_institution = sum(S05 == 6, na.rm = TRUE),
    private_health_institution = sum(S05 == 7, na.rm = TRUE),  
    local_npo = sum(S05 == 8, na.rm = TRUE),  
    IO = sum(S05 == 9, na.rm = TRUE),
    head_office = sum(S15 == 1, na.rm = TRUE),
    single_unit_establishment = sum(S15 == 2, na.rm = TRUE),
    branch_national_enterprise = sum(S15 == 3, na.rm = TRUE),
    branch_international_enterprise = sum(S15 == 4, na.rm = TRUE)
  )


analysis_2011_village <- read_xlsx(path = file.path(data_path, "analysis_2011(village).xlsx"))


#clean dataset to fit----

nisr_2011_analysis <- nisr_2011 %>% 
  rename(workers = Total_emp2) %>% 
  mutate(workers = ifelse(workers== 999999 | is.na(workers), 0, workers))

analysis_2011_village.1 <- analysis_2011_village

analysis_2011_village.1$Sector_ID <-substr(analysis_2011_village.1$Village_ID, 1, 4)

analysis_2011_village.1 <- analysis_2011_village.1 %>% 
  mutate(total_worker = ifelse(is.na(total_worker), 0, total_worker))




#Just the sdiance----



sector_sd <- analysis_2011_sector %>%
  summarise(across(everything(), sd, na.rm = TRUE))


village_sd <- analysis_2011_village.1%>%
  summarise(across(everything(), sd, na.rm = TRUE))


sector_cv <- analysis_2011_sector %>%
  summarise(across(everything(), ~ (sd(.)/mean(., na.rm = TRUE)) ))

village_cv <- analysis_2011_village.1 %>%
  summarise(across(everything(), ~ (sd(.)/mean(., na.rm = TRUE))))


sector_sd <- sector_sd %>% 
  pivot_longer(
    cols = everything(),
    names_to = "category",
    values_to = "sector_sd"
  )

village_sd <- village_sd %>% 
  pivot_longer(
    cols = everything(),
    names_to = "category",
    values_to = "village_sd"
  )


sector_cv <- sector_cv %>% 
  pivot_longer(
    cols = everything(),
    names_to = "category",
    values_to = "sector_cv"
  )

village_cv <- village_cv %>% 
  pivot_longer(
    cols = everything(),
    names_to = "category",
    values_to = "village_cv"
  )

sd <- left_join(sector_sd, village_sd, by = c("category"))
sd <- left_join(sd, sector_cv, by = c("category"))
sd <- left_join(sd, village_cv, by = c("category"))

sd <- sd %>% 
  filter(category != "sector_ID")

write_xlsx(sd, path = file.path(data_path, "nisr_2011_sd&cv.xlsx"))



#Sector and Village comparison----



ntl_wide <- read_xlsx(path = file.path(data_path, "ntl_wide_92_21(connect11&22+rd).xlsx"))
existing_lv <- st_read(dsn = file.path(data_path, "Existing Electrical Network_2022", "Existing_LVLine.shp"))
existing_mv <- st_read(dsn = file.path(data_path, "Existing Electrical Network_2022", "Existing_MVLine.shp"))

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp" ))
rwa_sector <- st_read(dsn = file.path(data_path, "rwa_sector", "Sector.shp" ))

rwa_villages <- st_make_valid(rwa_villages)

existing_lv <- st_transform(existing_lv, crs = st_crs(rwa_sector))
existing_mv <- st_transform(existing_mv, crs = st_crs(rwa_sector))
rwa_villages <- st_transform(rwa_villages, crs = st_crs(rwa_sector))

##Village-----
village_connect <- ntl_wide %>% 
  select(Village_ID, connect22_lv)

villages_all <- left_join(rwa_villages, village_connect, by = c("Village_ID"))


village_base <- ggplot() +
  geom_sf(data = villages_all, fill = NA) + 
  geom_sf(data = subset(villages_all, connect22_lv == 1), aes(fill = "Electrified"), size = 0) +
  scale_fill_manual(
    values = c("Electrified" = "lightblue"),
    name = ""
  )

village_base


##Sector----
sector_connect <- st_intersection(rwa_sector, existing_lv)


sector_connect <- sector_connect %>% 
  mutate(connect22_lv = 1) %>% 
  distinct(Sect_ID, .keep_all = TRUE)

sector_connect <- sector_connect %>% 
  select(Sect_ID, connect22_lv) %>% 
  st_drop_geometry

sector_all <- left_join(rwa_sector, sector_connect, by = c("Sect_ID"))

sector_base <- ggplot() +
  geom_sf(data = rwa_villages, fill = NA) + 
  geom_sf(data = subset(sector_all, connect22_lv == 1), aes(fill = "Electrified"), size = 0) +
  scale_fill_manual(
    values = c("Electrified" = "lightblue"),
    name = ""
  )

sector_base

####Karongi zoom in-----

karongi_sector <- sector_all %>% 
  filter(District %in% c("Karongi"))

karongi_villages <- villages_all %>% 
  filter(District %in% c("Karongi"))

karongi_lv <- st_intersection(karongi_villages, existing_lv)
karongi_mv <- st_intersection(karongi_villages, existing_mv)
karongi_meter <- st_read(dsn = file.path(data_path, "eucl meter", "karongi_meter.shp"))
karongi_meter <- st_transform(karongi_meter, crs = st_crs(rwa_sector))

karongi_meter_join <- karongi_meter %>% 
  mutate(meter_2023 = 1) %>% 
  distinct(Village_ID, .keep_all = TRUE) %>% 
  st_drop_geometry()

karongi_meter_join <- karongi_meter %>% 
  mutate(meter_2023 = 1) %>% 
  distinct(Village_ID, .keep_all = TRUE) %>% 
  st_drop_geometry()


karongi_villages <- left_join(karongi_villages, karongi_meter_join, by = c("Village_ID"))

karongi_plot_sector <- ggplot() +
  geom_sf(data = subset(karongi_sector, connect22_lv == 1), aes(fill = "Electrified"), size = 0) +  
  geom_sf(data = karongi_villages, fill = NA, color = "grey") + 
  geom_sf(data = karongi_sector, size = 1, fill = NA) + 
  # geom_sf(data = karongi_meter, aes(color = "meter"), size = 0.5)+
  geom_sf(data = karongi_mv, aes(color = "MV"), size = 0.5)+
  geom_sf(data = karongi_lv, aes(color = "LV"), size = 0.5)+
  
  scale_fill_manual(
    values = c("Electrified" = "lightblue"),
    name = ""
  ) +
  scale_color_manual(
    values = c("MV" = "red", "LV" = "green", "meter" = "orange"),
    name = ""
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
  ) +
  labs(title = "Karongi Electrifying Status Sector level (2022 grid) ")

karongi_plot_sector



karongi_plot_sector <- ggplot() +
  geom_sf(data = subset(karongi_sector, connect22_lv == 1), aes(fill = "Electrified"), size = 0) +  
  geom_sf(data = karongi_villages, fill = NA, color = "grey") + 
  geom_sf(data = karongi_sector, size = 1, fill = NA) + 
  geom_sf(data = karongi_meter, aes(color = "meter"), size = 0.5)+
  geom_sf(data = karongi_mv, aes(color = "MV"), size = 0.5)+
  geom_sf(data = karongi_lv, aes(color = "LV"), size = 0.5)+
  
  scale_fill_manual(
    values = c("Electrified" = "lightblue"),
    name = ""
  ) +
  scale_color_manual(
    values = c("MV" = "red", "LV" = "green", "meter" = "orange"),
    name = ""
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5, size = 20),  # Adjust title font size here
    plot.margin = margin(t = 20)  
  ) +
  labs(title = "Karongi Electrifying Status Sector level (2022 grid)")


karongi_plot_village <- ggplot() +
  geom_sf(data = subset(karongi_villages, meter_2023 == 1), aes(fill = "Electrified"), size = 0) +  
  geom_sf(data = karongi_villages, fill = NA, color = "grey") + 
  geom_sf(data = karongi_sector, size = 1, fill = NA) + 
  geom_sf(data = karongi_meter, aes(color = "meter"), size = 0.5)+
  geom_sf(data = karongi_mv, aes(color = "MV"), size = 0.5)+
  geom_sf(data = karongi_lv, aes(color = "LV"), size = 0.5)+
  scale_fill_manual(
    values = c("Electrified" = "lightblue"),
    name = ""
  ) +
  scale_color_manual(
    values = c("MV" = "red", "LV" = "green", "meter" = "orange"),
    name = ""
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5, size = 20),  # Adjust title font size here 
    plot.margin = margin(t = 20)  
  ) +
  labs(title = "Karongi Electrifying Status Village level (2022 grid) ")

karongi_plot_village

##Join with the establishment census----

sector_2011 <- read_xlsx(path = file.path(data_path, "analysis_2011.xlsx"))
village_2011 <- read_xlsx(path = file.path(data_path, "analysis_2011(village).xlsx"))

sector_2011_join <- sector_2011 %>% 
  select(sector_id, num_establishments_11)

sector_all <- left_join(sector_all, sector_2011_join, by = c("Sect_ID" = "sector_id"))

sector_all <- sector_all %>% 
  mutate(num_establishments = ifelse(is.na(num_establishments_11), 0, num_establishments_11))


existing_mv11 <- st_read(dsn = file.path(data_path, "Existing Electrical Network_2011", "Existing_MVLine.shp"))
existing_mv11 <- st_transform(existing_mv11, crs = st_crs(rwa_sector))

mv11_sector <- st_intersection(existing_mv11, rwa_sector)

mv11_sector <- mv11_sector %>% 
  mutate(connect11_mv = 1) %>% 
  distinct(Sect_ID, .keep_all =  TRUE) %>% 
  select(Sect_ID, connect11_mv) 

mv11_sector <- mv11_sector %>% 
  st_drop_geometry()

# sector_all <- sector_all %>% 
#   select(-connect11_mv)

sector_all <- left_join(sector_all, mv11_sector, by = c("Sect_ID"))

sector_all <- sector_all %>% 
  mutate(connect11_mv = ifelse(is.na(connect11_mv), 0, connect11_mv))



#Village
village_2011_join <- village_2011 %>% 
  select(Village_ID, num_establishments) %>% 
  mutate(Village_ID = as.character(Village_ID),
         num_establishments = ifelse(is.na(num_establishments), 0, num_establishments))

villages_all <- left_join(villages_all, village_2011_join, by = c("Village_ID"))

villages_all <- villages_all %>% 
  mutate(num_establishments = ifelse(is.na(num_establishments), 0, num_establishments))

existing_mv11 <- ntl_wide %>% 
  select(Village_ID, connect11_mv)

villages_all <- left_join(villages_all, existing_mv11, by = c("Village_ID"))


#Join the mv line with the analysis data
# 
# sector_2011 <- read_xlsx(path = file.path(data_path, "analysis_2011.xlsx"))
# village_2011 <- read_xlsx(path = file.path(data_path, "analysis_2011(village).xlsx"))

village_2011_join <- village_2011 %>% 
  select(Village_ID, total_worker, status_working) %>% 
  mutate(Village_ID = as.character(Village_ID))

villages_all <- left_join(villages_all, village_2011_join, by = c("Village_ID"))

villages_all <- villages_all %>% 
  mutate(total_worker = ifelse(is.na(total_worker), 0 , total_worker),
         status_working = ifelse(is.na(status_working), 0, status_working))

sector_2011_join <- sector_2011 %>% 
  select(sector_id, total_worker_11, status_working_11)

sector_all <- left_join(sector_all, sector_2011_join, by = c("Sect_ID" = "sector_id"))

sector_all <- sector_all %>% 
  mutate(
    total_worker_11 = ifelse(is.na(total_worker_11), 0, total_worker_11),
    status_working_11 = ifelse(is.na(status_working_11), 0, status_working_11)
  )



#Dataset for the balance table----

sector_status <- sector_all %>% 
  select(Sect_ID, connect22_lv) %>% 
  st_drop_geometry() %>% 
  rename(
    connect22_lv_sector = connect22_lv
  ) %>% 
  mutate(
    Sect_ID = as.character(Sect_ID)
  )

villages_all.1 <- villages_all %>% 
  st_drop_geometry() 

villages_all.1 <- left_join(villages_all.1, sector_status, by = c("Sector_ID" = "Sect_ID"))

table(villages_all.1$connect22_lv_sector)

villages_all.1 <- villages_all.1 %>% 
  mutate(
    connect22_lv_sector = ifelse(is.na(connect22_lv_sector), 0, connect22_lv_sector)
  )

table(villages_all.1$connect22_lv_sector)

balance_village <- villages_all.1 %>% 
  select(connect22_lv, num_establishments, total_worker, status_working) %>% 
  st_drop_geometry()

balance_village <- balance_village %>% 
  rename(total_employee = total_worker) %>% 
  mutate(total_worker = total_employee + num_establishments)


balance_village_sector <- villages_all.1%>% 
  select(connect22_lv_sector, num_establishments, total_worker, status_working) %>% 
  st_drop_geometry()

balance_village_sector <- balance_village_sector %>% 
  rename(total_employee = total_worker) %>% 
  mutate(total_worker = total_employee + num_establishments)




#Balance_table function----
balance_table<-function(data, treatment, caption) {
  
  variables<-NULL 
  
  data <- data %>% dplyr::arrange(!!rlang::sym(treatment))
  
  valores_trat <- base::unique(dplyr::pull(data, !!rlang::sym(treatment)))
  
  trats<-valores_trat[2:base::length(valores_trat)]
  
  bal_tables<-purrr::map(trats, function (x)
    data %>%
      dplyr::filter(!!rlang::sym(treatment) == valores_trat[1] | !!rlang::sym(treatment) ==  !!x))
  
  
  
  bal_tables<-purrr::map(bal_tables, function (x) x %>%
                           tidyr::pivot_longer(names_to = "variables", values_to = "value", -treatment) )
  
  
  # Creo por separado la primera para poner la mean de control 
  
  bal_tables<-purrr::map(bal_tables, function(x) x %>% 
                           dplyr::group_by(variables) %>% 
                           dplyr::summarise(mean_control = stats::t.test(value~!!rlang::sym(treatment))$estimate[1],
                                            mean_trat = stats::t.test(value~!!rlang::sym(treatment))$estimate[2], 
                                            p_value = stats::t.test(value~!!rlang::sym(treatment))$p.value))
  
  valores_trat<-base::as.character(valores_trat[2:base::length(valores_trat)])
  
  bal_tables <- purrr::map2_dfc(.x = bal_tables, .y = valores_trat,
                                function(x,y) dplyr::rename_all(x, ~stringr::str_c(., y)))
  
  #Quedandome solo con una de variables y mean_control
  means_control <- base::names(bal_tables %>% dplyr::select(dplyr::contains("control")) )
  variables_nombres <- base::names(bal_tables %>% dplyr::select(dplyr::contains("variables")))
  means_trat <- base::names(bal_tables %>% dplyr::select(dplyr::contains("trat")))
  p_values <-base::names(bal_tables %>% dplyr::select(dplyr::contains("p_value")))
  
  bal_tables<-
    bal_tables %>% 
    dplyr::select(tidyselect::all_of(variables_nombres[1]), 
                  tidyselect::all_of(means_control[1]), 
                  tidyselect::all_of(means_trat), 
                  tidyselect::all_of(p_values))
  
  # Convert to data frame for kable
  bal_tables_df <- as.data.frame(bal_tables)
  
  # return(bal_tables_df)
  bal_tables_df %>% dplyr::mutate_if(is.numeric, funs(as.character(signif(., 3)))) %>%
    kable(bal_tables, format = "html", caption = caption) %>%
    kable_styling(bootstrap_options = c("striped"),
                  # full_width = T,
                  font_size = 15) %>%
    scroll_box(height = "300px")
  
}

balance_table(balance_village, "connect22_lv", "village level with treatment at village level")

balance_table(balance_village_sector, "connect22_lv_sector", "village level with treatment at sector level")


village_bt %>% 
  kable(caption = "Your Table Title")

View(village_bt)














balance_village.1 <- balance_village %>% 
  group_by(connect11_mv) %>% 
  summarise(num_establishments = mean(num_establishments),
            status_working = mean(status_working),
            total_employee = mean(total_employee),
            total_worker = mean(total_worker))

#Balance_sheet-----

library(gtsummary)
install.packages("vtable")
library(vtable)

####Village-----
balance_village <- villages_all %>% 
  select(connect11_mv, num_establishments, total_worker, status_working) %>% 
    st_drop_geometry()

balance_village <- balance_village %>% 
  rename(total_employee = total_worker) %>% 
  mutate(total_worker = total_employee + num_establishments)


balance_village  %>% 
  tbl_summary(by = connect11_mv) %>%  
  add_p()

?tbl_summary()


sumtable(balance_village, group = "connect11_mv", group.test = TRUE, test = "t.test")

?sumtable


###Sector-----

balance_sector <- sector_all %>% 
  select(connect11_mv, num_establishments_11, total_worker_11, status_working_11) %>% 
  st_drop_geometry()

balance_sector <- balance_sector %>% 
  mutate(total_worker_11 = ifelse(is.na(total_worker_11), 0, total_worker_11))

balance_sector <- balance_sector %>% 
  rename(total_employee = total_worker_11) %>% 
  mutate(total_worker = total_employee + num_establishments_11)

balance_sector%>% 
  tbl_summary(by = connect11_mv) %>%  
  add_p()






###kARONGI ZOON----
karongi_est_vill <- rwa_villages %>% 
  filter(District %in% c("Karongi"))

karongi_est_sect <- rwa_sector%>% 
  filter(District %in% c("Karongi"))

###pLOT----



karongi_plot_sector <- ggplot() +
  geom_sf(data = subset(karongi_sector, connect22_lv == 1), aes(fill = "Electrified"), size = 0) +  
  geom_sf(data = karongi_villages, fill = NA, color = "grey") + 
  geom_sf(data = karongi_sector, size = 1, fill = NA) + 
  geom_sf(data = karongi_meter, aes(color = "meter"), size = 0.5)+
  geom_sf(data = karongi_mv, aes(color = "MV"), size = 0.5)+
  geom_sf(data = karongi_lv, aes(color = "LV"), size = 0.5)+
  
  scale_fill_manual(
    values = c("Electrified" = "lightblue"),
    name = ""
  ) +
  scale_color_manual(
    values = c("MV" = "red", "LV" = "green", "meter" = "orange"),
    name = ""
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
  ) +
  labs(title = "Karongi Electrifying Status Sector level (2022 grid) ")

karongi_plot_sector








karongi_plot_village <- ggplot() +
  geom_sf(data = subset(karongi_villages, meter_2023 == 1), aes(fill = "Electrified"), size = 0) +  
  geom_sf(data = karongi_villages, fill = NA, color = "grey") + 
  geom_sf(data = karongi_sector, size = 1, fill = NA) + 
  # geom_sf(data = karongi_meter, aes(color = "meter"), size = 0.5)+
  geom_sf(data = karongi_mv, aes(color = "MV"), size = 0.5)+
  geom_sf(data = karongi_lv, aes(color = "LV"), size = 0.5)+
  scale_fill_manual(
    values = c("Electrified" = "lightblue"),
    name = ""
  ) +
  scale_color_manual(
    values = c("MV" = "red", "LV" = "green", "meter" = "orange"),
    name = ""
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),
    legend.margin = margin(r=10),
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(t = 20)  
  ) +
  labs(title = "Karongi Electrifying Status Village level (2022 grid) ")

karongi_plot_village








