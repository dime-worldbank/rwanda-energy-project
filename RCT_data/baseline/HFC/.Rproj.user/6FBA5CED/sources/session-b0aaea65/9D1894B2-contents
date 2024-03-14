########################################################################################################
#                                                                                                      #
#                     HIGH-FREQUENCY CHECKS  -- ADMINISTRATIVE-LEVEL HIGH-FREQUENCY CHECKS             #
#                                                                                                      #
########################################################################################################

## PURPOSE      Create one or multiple administrative-level check sheets in the HFC dashboard.
## District (21)
## Villages  (364)
## Households

## AUTHOR      Juliana Guerrero (adapted from Adrien Ciret & Marc-Andrea Fiorina)

## LAST UPDATE  October 5, 2023

########################################################################################################

## 1. Import Data ----
# 

# preload list of hh  
sample_data <- read.csv(file.path(dropbox,
                                  'Rwanda Roads Data/Primary data/HH survey/endline/preloads/preload_data_HH.csv'))




########################################################################################################

hfc_data = hfc_constr
#hfc_constr = hfc_data
# Save useful data 
hfc_constr <- hfc_constr %>% select(starttime,ID_03,enumerator_name, ID_04, ID_05,ID_05_enter,
                                    startdate, ends_with("dur"), village, cell, sector, district, pl_vill_id,
                                    #id_03, id_04, id_05,
                                    pl_id_06, pl_id_07, pl_id_08, pl_id_09,
                                    survey_complete, 
                                    consent, pl_vill_id, 
                                    end_survey,survey_dur,enddate,startdate,
                                    enddate, survey_dur,
                                    ## Land module
                                    c_00_1old,  c_14_00,  c_23_1, c_23_2, c_23_3, 
                                    c_00_2, c_00 , c_00_1b,  c_00_1b1,  c_41, c_41_num, c_41_5_1,   
                                    c_41_7_1,  c_00_1a, c_00_1a1,
                                    
                                    ## Permanent crops
                                    d_01,  d_02_10_1,  d_02_10_2, d_02_10_3, d_02_10_4, d_02_10_5, d_02_10_6,
                                    d_02_10_7,d_02_10_8,d_02_10_9,#d_02_10_10, 
                                    d_02_10_a_1, d_02_10_a_2, d_02_10_a_3, d_02_10_a_4, d_02_10_a_5, d_02_10_a_6, 
                                    d_02_10_a_7,d_02_10_a_8,d_02_10_a_9,#d_02_10_a_10,
                                    
                                    ## Seasonal crops
                                    perm_HQ_1,  perm_HQ_2, perm_HQ_3, perm_HQ_4, perm_HQ_5, 
                                    perm_HQ_6, perm_HQ_7,perm_HQ_8,perm_HQ_9,#perm_HQ_10, 
                                    
                                    ## Livestock
                                    d_61_1, d_61_2, d_61_3, d_61_4, d_61_5, d_61_6, d_61_7, d_61_8, d_61_a, d_62_1, d_62_2 , d_62_3, d_62_4, d_62_5, d_62_6, d_62_7, d_62_8 , d_66_2_1_1, d_66_2_2_1, d_66_2_3_1, d_66_2_4_1, d_66_2_5_1, d_66_2_6_1, d_66_2_7_1, d_66_2_8_1, d_66_4_1_1, d_66_4_2_1, d_66_4_3_1, d_66_4_4_1, d_66_4_5_1, d_66_4_6_1, d_66_4_7_1, d_66_4_8_1, d_66_a_1, d_66_a_2, d_66_a_3, d_66_a_4, d_66_a_5, d_66_a_6, d_66_a_7, d_66_a_8,
                                    
                                    ## Module E
                                    e_05_1, e_05_2, e_05_3, e_05_4, e_05_5, e_05_6, e_05_7, e_05_8, e_05_9, e_05_10, e_05_11 )


## 1. Village

# 1.1 Merging village-wise sampling data with HFC
sample_data =  sample_data %>%
  rename(village=id_06,
         cell = id_07,
         sector = id_08,
         district = id_09)

# total surveys to complete by village
village_final <- sample_data %>%
  
  group_by(across(c(village,district,sector,cell))) %>% 
  
  summarize(total = n()) %>%
  
  ungroup() 

#  submitted forms
village_svy = hfc_constr %>% 
  select(-c(village,cell,district,sector)) %>% 
  mutate(ID_05_enter=paste0("'",ID_05_enter)) %>% 
  left_join(sample_data %>% select(headid,village,cell,sector,district),
            by=c('ID_05_enter'='headid')) %>% 
  group_by(village,cell,district,sector) %>% 
  mutate(n_subs=n(),
         complete     = sum(survey_complete, na.rm = TRUE)) %>% 
  mutate(attrition = sum(consent==0,na.rm=T)) %>% 
  select(village,cell,district,sector,n_subs,complete,attrition)

# 1.2 Creating dataset to use for exporting village wise stats
village_final = village_final %>% 
  left_join(village_svy,by=c("village" = "village",
                             "district" = "district",
                             "cell" = "cell",
                             "sector" = "sector")) %>% 
  distinct()



# 1.3 Creating dataset to use for exporting daily village data
village_daily <- hfc_constr %>%
  
  group_by(across(c(village,district,sector,cell)), enddate) %>%
  count() %>%
  ungroup() %>%
  arrange(enddate) %>%
  pivot_wider(names_from = enddate, values_from = n)  


# 1.4 Creating dataset to use for exporting data from the last day of data collection
village_last_day <- hfc_constr %>%
  
  group_by(village,district,sector,cell) %>%
  
  filter(enddate == max((enddate), na.rm = TRUE)) %>%
  
  mutate(n = n()) %>%
  
  summarize(
    
    last_date       = unique(enddate),
    
    ld_survey_num   = first(n),#,#first(n, na.rm = TRUE),
    
    ld_complete        = sum(survey_complete, na.rm = TRUE)
    
  ) %>% select(village,district,sector,cell,last_date,ld_survey_num,ld_complete)


# 1.5 Merging data from total, daily and last day numbers at the village level
village_final <- village_final %>% 
  
  left_join(village_last_day) %>%
  
  left_join(village_daily)




# saving output

hfc_sheet %>%
  
  sheet_write(data = village_final, sheet = "village_data")

1


########################################################################################################

## 2. District

# 2.1 Merging District-wise sampling data with HFC
# 

# 2.2 Creating dataset to use for exporting region wise stats
district <- sample_data %>%
  group_by(district) %>% # need to fill in villagecode
  summarize(total = n()) %>%
  ungroup() 

# data from survey hfc_constr %>% 

district_svy = hfc_constr %>% 
  select(-c(village,cell,district,sector)) %>% 
  mutate(ID_05_enter=paste0("'",ID_05_enter)) %>% 
  left_join(sample_data %>% select(headid,village,cell,sector,district),
            by=c('ID_05_enter'='headid')) %>% 
  group_by(district) %>% 
  mutate(n_subs=n(),
         complete     = sum(survey_complete, na.rm = TRUE)) %>% 
  mutate(attrition = sum(consent==0,na.rm=T)) %>% 
  select(district,n_subs,complete,attrition)

# final
district_final = district %>% 
  left_join(district_svy,by=c("district" = "district"
  )) %>% 
  distinct()

## daily
district_daily <- hfc_constr %>%
  group_by(across(c(district)), enddate) %>%
  count() %>%
  ungroup() %>%
  arrange(enddate) %>%
  pivot_wider(names_from = enddate, values_from = n)  
## last day
district_last_day <- hfc_constr %>%
  
  group_by(district) %>%
  
  filter(enddate == max((enddate), na.rm = TRUE)) %>%
  
  mutate(n = n()) %>%
  
  summarize(
    
    last_date       = unique(enddate),
    
    ld_survey_num   = first(n),#,#first(n, na.rm = TRUE),
    
    ld_complete        = sum(survey_complete, na.rm = TRUE)
    
  ) %>% select(district,last_date,ld_survey_num,ld_complete)


district_final <- district_final %>% 
  
  left_join(district_last_day) %>%
  
  left_join(district_daily) %>% 
  filter(district!='')

# save output

hfc_sheet %>%
  
  sheet_write(data = district_final, sheet = "district_data")

1



########################################################################################################

## 3. Cell
cell <- sample_data %>%
  group_by(district,sector,cell) %>% # need to fill in villagecode
  summarize(total = n()) %>%
  ungroup() 


# data from survey attrition = sum(consent==0,na.rm=T)
cell_svy = hfc_constr %>% 
  select(-c(village,cell,district,sector)) %>% 
  mutate(ID_05_enter=paste0("'",ID_05_enter)) %>% 
  left_join(sample_data %>% select(headid,village,cell,sector,district),
            by=c('ID_05_enter'='headid')) %>% 
  group_by(district,sector,cell) %>% 
  mutate(n_subs=n(),
         complete     = sum(survey_complete, na.rm = TRUE)) %>% #,
  mutate(attrition = sum(consent==0,na.rm=T)) %>% 
  select(district,sector,cell,n_subs,complete,attrition)

# final
cell_final = cell %>% 
  left_join(cell_svy,by=c("district" = "district",
                          "sector"="sector",
                          "cell"="cell"
  )) %>% 
  distinct()

## daily
cell_daily <- hfc_constr %>%
  group_by(across(c(district,sector,cell)), enddate) %>%
  count() %>%
  ungroup() %>%
  arrange(enddate) %>%
  pivot_wider(names_from = enddate, values_from = n)  
## last day
cell_last_day <- hfc_constr %>%
  group_by(district,sector,cell) %>%
  filter(enddate == max((enddate), na.rm = TRUE)) %>%
  mutate(n = n()) %>%
  summarize(
    
    last_date       = unique(enddate),
    
    ld_survey_num   = first(n),#,#first(n, na.rm = TRUE),
    
    ld_complete        = sum(survey_complete, na.rm = TRUE)
    
  ) %>% select(district,sector,cell,last_date,ld_survey_num,ld_complete)


cell_final <- cell_final %>% 
  left_join(cell_last_day) %>%
  left_join(cell_daily) %>% 
  filter(district!='')

# save output

hfc_sheet %>%
  
  sheet_write(data = cell_final, sheet = "cell_data")

1

########################################################################################################

## 4. Tracking Sheet



# Marking HHs surveyed
tracking_surveyed <- hfc_constr %>% 
  
  select(ID_05, ID_05_enter,startdate, ID_03,enumerator_name) %>% mutate(
    
    surveyed = "Surveyed"
    
  )



tracking <- sample_data %>% 
  
  
  rename(ID_05 = headid) %>% 
  
  group_by(ID_05)  %>%
  
  mutate(total = row_number()) %>%
  
  ungroup() %>%
  
  select(district, sector, cell, village, 
         
         
         ID_05,  total) %>%
  
  distinct() %>%
  
  left_join(tracking_surveyed, by = c("ID_05" = "ID_05")) %>%
  
  group_by(ID_05) %>%
  
  mutate_at(vars(matches("surveyed")),~replace(., is.na(.), "Not Surveyed") ) %>%  
  
  distinct(.) %>% 
  
  arrange(startdate, surveyed) 

# Exporting Tracking Sheet

hfc_sheet %>% 
  sheet_write(data = tracking, sheet = "track_data")

1