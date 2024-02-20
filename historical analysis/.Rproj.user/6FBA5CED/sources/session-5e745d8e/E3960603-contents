##########################
#Author: Xiaoming Zhang
#Date: 02142024
#purpose:establishment census analysis
############################


#library----
#Please remove # and run the following line if pacman was not installed in your R session

#install.packages("pacman")

pacman::p_load(tidyverse, dplyr, here, sf, readxl, writexl, janitor, haven, stringr, install = TRUE)


#For establishment 2017
#Read file----

#If it's not a dta file please change the read_dta to read_xlsx or read.csv etc. 

establishments2017 <- read_dta()#paste here the file path of your dataset, please replace all"\" with "/"


#Generate Village_ID-----

#Run this if the data does not include a Village_ID column for the establishments

establishments2017 <- establishments2017 %>% 
  mutate(Village_ID = ID1*10000000 + ID2*1000000 + ID3*10000 + ID4*100 +ID5) %>% 
  select(Village_ID, everything())

#ID1= Province_ID
#ID2 = District_ID
#ID3 = Sector_ID
#ID4 = Cell_ID
#ID5 = Village_ID


#Aggregate on the village level----


#The question numbers are from document "q-rwa-nisr-ec-2017-eng.docx" which is the establishment census itself in 2017. 
#Please refer back to this questionnaire if anything is unclear

village2017 <- establishments2017%>% 
  group_by(Village_ID) %>% 
  summarize(
    num_establishments = n(),
    total_worker = sum(Q17_3), #Q17.3. TOTAL NUMBER OF WORKING PERSONS IN ALL BRANCHES IN RWANDA INCLUDING THE HEAD OFFICE
    within_market = sum(Q2 == 1, na.rm = TRUE), # Q2 Working place: 1. within market place, 2. Outside market place, 3. Industrial zone, 4. ICPCs-(Udukiriro)
    outside_market = sum(Q2 == 2, na.rm = TRUE),
    industrial_zone = sum(Q2 = 3, na.rm = TRUE),
    ICPCs = sum(Q2 = 4, na.rm = TRUE),
    status_working = sum(Q3 == 1, na.rm = TRUE), # Q3 working status: 1. working, 2. closed temporarily 3. closed permanently 
    status_temp_closed = sum(Q3 == 2, na.rm = TRUE),
    status_perm_closed = sum(Q3 == 3, na.rm = TRUE),
    private_sector = sum(Q7 == 1, na.rm = TRUE), # Q7 institutional sector: 1. Private sector, 2. Cooperative, 3. Public Sector 4. Mixed sector, 5. NGO(Rwanda), 6. NGO(International)
    cooperative_sector = sum(Q7 == 2, na.rm = TRUE),
    public_sector = sum(Q7 == 3, na.rm = TRUE),
    mixed_sector = sum(Q7 == 4, na.rm = TRUE),
    ngo_rwa = sum(Q7 = 5, na.rm = TRUE),
    ngo_intl = sum(Q7 = 6, na.rm = TRUE),
    head_office = sum(Q16 == 1, na.rm = TRUE), #Q16 establishment type: 1.head office, 2. single unit establishment, 3. branch, 4. sub branch
    single_unit_establishment = sum(Q16 == 2, na.rm = TRUE),
    branch = sum(Q16 == 3, na.rm = TRUE),
    sub_branch = sum(Q16 == 4, na.rm = TRUE),
    registered_sector = sum(Q22_1 == 1, na.rm = TRUE), # Q22, Is the establishment registered in any of the following institutions? 1. sector (1. Yes, 2. No) 2. District(1. Yes, 2. No)
    registered_district = sum(Q22_2 == 1, na.rm = TRUE)
  )

write.csv(village2017, path = file.path()) #replace filepath with the folder path plus name of the new saved file, it could be village2017.csv, remember to replace "/" with "\"



#For establishment 2020
#Read file----

#If it's not a dta file please change the read_dta to read_xlsx or read.csv etc. 

establishments2020 <- read_dta()#paste here the file path of your dataset, please replace all"\" with "/"


#Generate Village_ID-----

#Run this if the data does not include a Village_ID column for the establishments

establishments2020 <- establishments2020 %>% 
  mutate(Village_ID = ID1*10000000 + ID2*1000000 + ID3*10000 + ID4*100 +ID5) %>% 
  select(Village_ID, everything())

#ID1= Province_ID
#ID2 = District_ID
#ID3 = Sector_ID
#ID4 = Cell_ID
#ID5 = Village_ID


#Aggregate on the village level----


#The question numbers are from document "Establishment Census 2020 Questionnaire in English.pdf" which is the establishment census itself in 2017. 
#Please refer back to this questionnaire if anything is unclear
#replace with actual name of data column

village2020 <- establishments2020%>% 
  group_by(Village_ID) %>% 
  summarize(
    num_establishments = n(),
    total_worker = sum(Q17_3), #Q17.3. TOTAL NUMBER OF WORKING PERSONS IN ALL BRANCHES IN RWANDA INCLUDING THE HEAD OFFICE
    within_market = sum(Q2 == 1, na.rm = TRUE), # Q2 Working place: 1. within market place, 2. Outside market place, 3. Industrial zone, 4. ICPCs-(Udukiriro)
    outside_market = sum(Q2 == 2, na.rm = TRUE),
    industrial_zone = sum(Q2 = 3, na.rm = TRUE),
    ICPCs = sum(Q2 = 4, na.rm = TRUE),
    status_working = sum(Q3_1 == 1, na.rm = TRUE), # Q3.1 working status: 1. working, 2. closed temporarily 3. closed permanently 
    status_temp_closed = sum(Q3_1 == 2, na.rm = TRUE), # replace Q3_1 with the actual name of column
    status_perm_closed = sum(Q3_1 == 3, na.rm = TRUE),
    private_sector = sum(Q7 == 1, na.rm = TRUE), # Q7 institutional sector: 1. Private sector, 2. Cooperative, 3. Public Sector 4. Mixed sector, 5. NGO(Rwanda), 6. NGO(International)
    cooperative_sector = sum(Q7 == 2, na.rm = TRUE),
    public_sector = sum(Q7 == 3, na.rm = TRUE),
    mixed_sector = sum(Q7 == 4, na.rm = TRUE),
    ngo_rwa = sum(Q7 = 5, na.rm = TRUE),
    ngo_intl = sum(Q7 = 6, na.rm = TRUE),
    head_office = sum(Q16 == 1, na.rm = TRUE), #Q16 establishment type: 1.head office, 2. single unit establishment, 3. branch, 4. sub branch
    single_unit_establishment = sum(Q16 == 2, na.rm = TRUE),
    branch = sum(Q16 == 3, na.rm = TRUE),
    sub_branch = sum(Q16 == 4, na.rm = TRUE),
    registered_sector = sum(Q22_1 == 1, na.rm = TRUE), # Q22, Is the establishment registered in any of the following institutions? 1. sector (1. Yes, 2. No) 2. District(1. Yes, 2. No)
    registered_district = sum(Q22_2 == 1, na.rm = TRUE)
  )



write.csv(village2020, path = file.path()) #replace filepath with the folder path plus name of the new saved file, it could be village2020.csv, remember to replace "/" with "\"




#For establishment 2014
#Read file----

#If it's not a dta file please change the read_dta to read_xlsx or read.csv etc. 

establishments2014 <- read_dta()#paste here the file path of your dataset, please replace all"\" with "/"


#Generate Village_ID-----

#Run this if the data does not include a Village_ID column for the establishments

establishments2014 <- establishments2014 %>% 
  mutate(Village_ID = ID1*10000000 + ID2*1000000 + ID3*10000 + ID4*100 +ID5) %>% 
  select(Village_ID, everything())

#ID1= Province_ID
#ID2 = District_ID
#ID3 = Sector_ID
#ID4 = Cell_ID
#ID5 = Village_ID


#Aggregate on the village level----


#The question numbers are from document "q-rwa-nisr-ec-2014-eng.docx" which is the establishment census itself in 2017. 
#Please refer back to this questionnaire if anything is unclear
#replace with actual name of data column

village2014 <- establishments2014%>% 
  group_by(Village_ID) %>% 
  summarize(
    num_establishments = n(),
    total_worker = sum(Q20), #Q20. TOTAL NUMBER OF WORKING PERSONS IN ALL BRANCHES IN RWANDA INCLUDING THE HEAD OFFICE
    within_market = sum(Q2 == 1, na.rm = TRUE), # Q2 Working place: 1. within market place, 2. Outside market place, 3. Industrial zone, 4. ICPCs-(Udukiriro)
    outside_market = sum(Q2 == 2, na.rm = TRUE),
    industrial_zone = sum(Q2 = 3, na.rm = TRUE),
    status_working = sum(Q3 == 1, na.rm = TRUE), # Q3 working status: 1. working, 2. closed temporarily 3. closed permanently 
    status_temp_closed = sum(Q3 == 2, na.rm = TRUE), # replace Q3_1 with the actual name of column
    status_perm_closed = sum(Q3 == 3, na.rm = TRUE),
    private_sector = sum(Q8 == 1, na.rm = TRUE), # Q8 institutional sector: 1. Private sector, 2. Cooperative, 3. Public Sector 4. Mixed sector, 5. NGO(Rwanda), 6. NGO(International)
    public_sector = sum(Q8 == 3, na.rm = TRUE),
    mixed_sector = sum(Q8 == 2, na.rm = TRUE),
    ngo_rwa = sum(Q8 = 4, na.rm = TRUE),
    ngo_intl = sum(Q8 = 5, na.rm = TRUE),
    head_office = sum(Q18 == 1, na.rm = TRUE), #Q18 establishment type: 1.head office, 2. single unit establishment, 3. branch, 4. sub branch
    single_unit_establishment = sum(Q18 == 2, na.rm = TRUE),
    branch = sum(Q18 == 3, na.rm = TRUE),
    sub_branch = sum(Q18 == 4, na.rm = TRUE),
    registered_sector = sum(Q27_1 == 1, na.rm = TRUE), # Q27, Is the establishment registered in any of the following institutions? 1. sector (1. Yes, 2. No) 2. District(1. Yes, 2. No)
    registered_district = sum(Q27_2 == 1, na.rm = TRUE)
  )

write.csv(village2014, path = file.path()) #replace filepath with the folder path plus name of the new saved file, it could be village2014.csv, remember to replace "/" with "\"









































