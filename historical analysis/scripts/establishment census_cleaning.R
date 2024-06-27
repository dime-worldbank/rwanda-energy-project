
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

nisr_2014 <- read_xlsx(file.path(data_path, "Sector2014.xlsx"))
nisr_2017 <- read_xlsx(file.path(data_path, "Sector2017.xlsx"))
nisr_2020 <- read_xlsx(file.path(data_path, "Sector2020.xlsx"))

rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))
rwa_district <- st_read(dsn = file.path(data_path, "rwa_district", "District.shp"))
rwa_sector <- st_read(dsn = file.path(data_path, "rwa_sector", "Sector.shp"))

rwa_district <- st_transform(rwa_district, crs = st_crs(rwa_district))
rwa_sector <- st_transform(rwa_sector, crs = st_crs(rwa_villages))



existing_MV11 <- st_read(dsn = file.path(data_path, "Existing Electrical Network_2011", "Existing_MVLine.shp"))
existing_HV11 <- st_read(dsn = file.path(data_path, "Existing Electrical Network_2011", "Existing_HVLine.shp"))

existing_MV11 <- st_transform(existing_MV11, crs = st_crs(rwa_villages))
existing_HV11 <- st_transform(existing_HV11, crs = st_crs(rwa_villages))

existing_LV22 <- st_read(dsn = file.path(data_path, "Existing Electrical Network_2022", "Existing_LVLine.shp"))
existing_MV22 <- st_read(dsn = file.path(data_path, "Existing Electrical Network_2022", "Existing_MVLine.shp"))
existing_HV22 <- st_read(dsn = file.path(data_path, "Existing Electrical Network_2022", "Existing_HVLine.shp"))

existing_LV22 <- st_transform(existing_LV22, crs = st_crs(rwa_villages))
existing_MV22 <- st_transform(existing_MV22, crs = st_crs(rwa_villages))
existing_HV22 <- st_transform(existing_HV22, crs = st_crs(rwa_villages))


#Analysis 2011----
#(this was saved)

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
    total_worker = sum(S14CA + S14CB + S14CC + S14CD),
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
    total_worker = sum(S14CA + S14CB + S14CC + S14CD),
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

#Only on number of establishment joined for each year----

establishment_2011 <- analysis_2011_sector %>% 
  rename( establishment_2011 = num_establishments) %>% 
  select(sector_ID, establishment_2011) %>% 
  clean_names()


establishment_2014 <- nisr_2014 %>% 
  rename(establishment_2014 = num_establishments) %>% 
  select(Sector_ID, establishment_2014) %>% 
  clean_names()

establishment_2017 <- nisr_2017 %>% 
  rename(establishment_2017 = num_establishments) %>% 
  select(Sector_ID, establishment_2017) %>% 
  clean_names()

establishment_2020 <- nisr_2020 %>% 
  rename(establishment_2020 = num_establishments) %>% 
  select(Sector_ID, establishment_2020) %>% 
  clean_names()

num_establishment <- left_join(rwa_sector, establishment_2011, by = c("Sect_ID" = "sector_id"))

num_establishment <- left_join(num_establishment, establishment_2014, by = c("Sect_ID" = "sector_id"))
num_establishment <- left_join(num_establishment, establishment_2017, by = c("Sect_ID" = "sector_id"))
num_establishment <- left_join(num_establishment, establishment_2020, by = c("Sect_ID" = "sector_id"))

summary(num_establishment$establishment_2020)

num_establishment_plot <- num_establishment %>%
  mutate(
    across(starts_with("establishment"), ~ ifelse(. > 500, 500, .))
  )

mv_color <- "green"
hv_color <- "red"

plot_11 <- ggplot() + 
  geom_sf(data = rwa_sector, fill = NA, color = "lightgrey") + 
  geom_sf(data = num_establishment_plot, color = NA, aes(fill = establishment_2011)) +
  geom_sf(data = existing_MV11, aes(color = "MV"), size = 0.3) + 
  geom_sf(data = existing_HV11, aes(color = "HV"), size = 0.3) +
  scale_fill_gradient(low = "white", high = "grey20") + 
  scale_color_manual(name = "Type", values = c(MV = mv_color, HV = hv_color)) +
  theme_classic() +
  labs(title = "Rwanda Establishment 2011 & Grid lines")

plot_11

ggsave("plot_11.png", plot_11, width = 12, height = 8.4, units = "in", dpi = 300)


plot_14 <- ggplot() + 
  geom_sf(data = rwa_sector, fill = NA, color = "lightgrey") + 
  geom_sf(data = num_establishment_plot, color = NA, aes(fill = establishment_2014)) +
  geom_sf(data = existing_MV11, aes(color = "MV"), size = 0.3) + 
  geom_sf(data = existing_HV11, aes(color = "HV"), size = 0.3) +
  scale_fill_gradient(low = "white", high = "grey20") + 
  scale_color_manual(name = "Type", values = c(MV = mv_color, HV = hv_color)) +
  theme_classic() +
  labs(title = "Rwanda Establishment 2014 & Grid lines")

plot_14

ggsave("plot_14.png", plot_14, width = 12, height = 8.4, units = "in", dpi = 300)


plot_17 <- ggplot() + 
  geom_sf(data = rwa_sector, fill = NA, color = "lightgrey") + 
  geom_sf(data = num_establishment_plot, color = NA, aes(fill = establishment_2017) )+
  geom_sf(data = existing_MV11, aes(color = "MV"), size = 0.3) + 
  geom_sf(data = existing_HV11, aes(color = "HV"), size = 0.3) +
  scale_fill_gradient(low = "white", high = "grey20") + 
  scale_color_manual(name = "Type", values = c(MV = mv_color, HV = hv_color)) +
  theme_classic() +
  labs(title = "Rwanda Establishment 2017 & Grid lines")

plot_17

ggsave("plot_17.png", plot_17, width = 12, height = 8.4, units = "in", dpi = 300)


plot_20 <- ggplot() + 
  geom_sf(data = rwa_sector, fill = NA, color = "lightgrey") + 
  geom_sf(data = num_establishment_plot, color = NA, aes(fill = establishment_2020) )+
  geom_sf(data = existing_MV11, aes(color = "MV"), size = 0.3) + 
  geom_sf(data = existing_HV11, aes(color = "HV"), size = 0.3) +
  scale_fill_gradient(low = "white", high = "grey20") + 
  scale_color_manual(name = "Type", values = c(MV = mv_color, HV = hv_color)) +
  theme_classic() +
  labs(title = "Rwanda Establishment 2020 & Grid lines")

plot_20

ggsave("plot_20.png", plot_20, width = 12, height = 8.4, units = "in", dpi = 300)


#Create gif----
library(magick)

# Directory where your image files are located
dir_path <- "C:/Users/wb614406/github/rwanda-energy-project/historical analysis/outputs/establishment plot"

# List image files in the directory
imgs <- list.files(dir_path, full.names = TRUE)

# Read in the images
img_list <- lapply(imgs, image_read)

# Join the images together
img_joined <- image_join(img_list)

# Animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 1)

# View animated image
img_animated

# Save to disk
image_write(img_animated, path = "C:/Users/wb614406/github/rwanda-energy-project/historical analysis/outputs/establishment plot/rwa_establishment.gif")


#Clean the dataset a little bit----

###2011----

View(analysis_2011_sector)
analysis_2011 <- analysis_2011_sector %>% 
  rename_with(~ paste0(., "_11"), everything()) %>% 
  as.data.frame()

analysis_2011 <- analysis_2011 %>% 
  rename(sector_ID = sector_ID_11) %>% 
  clean_names()

###2014----

analysis_2014 <- nisr_2014 %>% 
  rename(
    total_worker = Total_2,
    within_market = Q2_market_place,
    outside_market = Q2_outside_market,
    industrial = Q2_indus_zone,
    status_working = Q3_working,
    # status_temp_closed = Q3_,
    # status_perm_closed = ,
    # household_sector = ,
    private_sector = Q8_private_sector,
    public_sector = Q8_pub_sector,
    mixed_sector = Q8_mixed_sector,
    ngo_rwa = Q8_NGO_Rwanda,
    ngo_intl = Q8_NGO_Intl,
    # private_education_institution = ,
    # private_health_institution = ,  # Fixed this line
    # local_npo = ,  # Fixed this line
    # IO = ,
    head_office = Q18_head_office,
    single_unit_establishment = Q18_single_unit,
    branch = Q18_branch,
    sub_branch = Q18_sub_branch,
    regis_sector = Q27A_yes,
    regis_district = Q27B_yes
    # branch_national_enterprise = ,
    # branch_international_enterprise = 
  )

analysis_2014 <- analysis_2014 %>% 
  rename_with(~ paste0(., "_14"), everything()) %>% 
  as.data.frame()

analysis_2014 <- analysis_2014 %>% 
  rename(sector_ID = Sector_ID_14) %>% 
  clean_names()



##2017----


analysis_2017 <- nisr_2017 %>% 
  rename(
    total_worker = Total_workers,
    within_market = q2_market_place,
    outside_market = q2_outside_market,
    industrial = q2_indus_zone,
    icpc = q2_ICPCs,
    status_working = q3_working,
    status_temp_closed = q3_temp_closed,
    private_sector = q7_private_sector,
    public_sector = q7_pub_sector,
    cooperative = q7_cooperative,
    mixed_sector = q7_PPP,
    ngo_rwa = q7_NGO_Rwanda,
    ngo_intl = q7_NGO_Intl,
    # private_education_institution = ,
    # private_health_institution = ,  # Fixed this line
    # local_npo = ,  # Fixed this line
    # IO = ,
    head_office = q16_head_office,
    single_unit_establishment =q16_single_unit,
    branch = q16_branch,
    sub_branch = q16_sub_branch,
    regis_sector = q22a,
    regis_district = q22b
    # branch_national_enterprise = ,
    # branch_international_enterprise = 
  )

analysis_2017 <- analysis_2017 %>% 
  rename_with(~ paste0(., "_17"), everything()) %>% 
  as.data.frame()

analysis_2017 <- analysis_2017 %>% 
  rename(sector_ID = Sector_ID_17) %>% 
  clean_names()



##2020----



analysis_2020 <- nisr_2020 %>% 
  rename(
    total_worker = q19_24,
    within_market = q2_market_place,
    outside_market = q2_outside_market,
    industrial = q2_indus_zone,
    icpc = q2_ICPCs_Udukiriro,
    status_working = q3_working,
    status_temp_closed = q3_temp_closed,
    private_sector = q7_private_sector,
    public_sector = q7_pub_sector,
    cooperative = q7_cooperative,
    mixed_sector = q7_PPP,
    ngo_rwa = q7_NGO_Rwanda,
    ngo_intl = q7_NGO_Intl,
    # private_education_institution = ,
    # private_health_institution = ,  # Fixed this line
    # local_npo = ,  # Fixed this line
    # IO = ,
    head_office = q16_head_office,
    single_unit_establishment =q16_single_unit,
    branch = q16_branch,
    sub_branch = q16_sub_branch,
    regis_sector = q22a,
    regis_district = q22b
    # branch_national_enterprise = ,
    # branch_international_enterprise = 
  )

analysis_2020 <- analysis_2020 %>% 
  rename_with(~ paste0(., "_20"), everything()) %>% 
  as.data.frame()

analysis_2020 <- analysis_2020 %>% 
  rename(sector_ID = Sector_ID_20) %>% 
  clean_names()


#save-xlsx----
write_xlsx(analysis_2011, path = file.path(data_path, "analysis_2011.xlsx"))
write_xlsx(analysis_2011_village, path = file.path(data_path, "analysis_2011(village).xlsx"))

write_xlsx(analysis_2014, path = file.path(data_path, "analysis_2014.xlsx"))
write_xlsx(analysis_2017, path = file.path(data_path, "analysis_2017.xlsx"))
write_xlsx(analysis_2020, path = file.path(data_path, "analysis_2020.xlsx"))



















