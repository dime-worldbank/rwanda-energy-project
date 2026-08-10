#######################################################
#Purpose: installed readyboard
#Author: Xiaoming
#Date: 9.25.2025
####################################################################



pacman::p_load(knitr, stargazer, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr, lfe, install = TRUE)
library(googlesheets4)
getwd()

conflicted::conflict_prefer_all("dplyr")
# Import Data ----
dropbox <- 'C:/Users/wb614406/Dropbox'

output_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/baseline analysis/output"
)

data_path_1 <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data"
)

hfc_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/HFC/data"
)


data_path_2 <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/Updated scope villages& households"
)



#Read Dime-----

complete  <- read_xlsx(path = file.path(data_path_2, "survey status of vulnerable households in sample villages_final.xlsx"))


rulindo_dime <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rulindo.xlsx"),
                          sheet = "household list")
rulindo_dime_village <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rulindo.xlsx"),
                                  sheet = "village list")

rulindo_15 <- complete %>% 
  filter(`Dropped from scope due to 15kv` == "Yes")


rulindo_dime_village_no15 <- rulindo_dime_village %>% 
  filter(!village_id %in% rulindo_15$villageid_key)


rutsiro_dime <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rutsiro.xlsx"),
                          sheet = "household list")
rutsiro_dime_village <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rutsiro.xlsx"),
                                  sheet = "village list")


karongi_dime <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Karongi.xlsx"),
                          sheet = "household list")
karongi_dime_village <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Karongi.xlsx"),
                                  sheet = "village list")





##Rusizi lot 1---------

rusizi1_dime <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rusizi-1.xlsx"),
                          sheet = "household list")
rusizi1_dime_village <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rusizi-1.xlsx"),
                                  sheet = "village list")



##Rusizi lot 2-------


rusizi2_dime <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rusizi-2.xlsx"),
                          sheet = "household list")
rusizi2_dime_village <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rusizi-2.xlsx"),
                                  sheet = "village list")


rusizi_dime_village <- rbind(rusizi1_dime_village, rusizi2_dime_village)

rusizi_dime <- rbind(rusizi1_dime, rusizi2_dime)



#Karongi-----

karongi_installed_raw <- read_xlsx(path = file.path(data_path_2, "Readyboard EPC negotiation", "Karongi_installed.xlsx"))

karongi_installed <- karongi_installed_raw %>% 
  clean_names() %>% 
  filter(comment == "Installed" | comment == "INSTALL") %>% 
  mutate(installed_id = nid) %>%
  mutate(
    nid = ifelse(last_name == "Mukarubuga" , "1195470022711048", installed_id)
  ) |>
  rename(village_id = villageid_key) |> 
  mutate(installed = 1) |> 
  select(village_id, nid, installed, installed_id)





dup_df<- tibble(duplicate_id = karongi_installed$installed_id[duplicated(karongi_installed$installed_id)])


check <- karongi_installed %>%
  filter(!nid %in% karongi_dime$nid)


#Rutsiro-----------

rutsiro_installed_raw <- read_xlsx(path = file.path(data_path_2, "Readyboard EPC negotiation", "Rutsiro_installed.xlsx"))

rutsiro_installed <- rutsiro_installed_raw %>% 
  clean_names() %>% 
  filter(!is.na(villageid_key)) %>% 
  filter(comment == "installed" ) %>% 
  mutate(installed_id = nid) |>
  rename(village_id = villageid_key) |> 
  mutate(installed = 1) |> 
  select(village_id, nid, installed, installed_id)






dup_df<- tibble(duplicate_id = rutsiro_installed$installed_id[duplicated(rutsiro_installed$installed_id)])


check <- rutsiro_installed %>%
  filter(!nid %in% rutsiro_dime$nid)




##New Rulindo Installed------

rulindo_installed_raw<- read_xlsx(path =file.path(data_path_2, "Readyboard EPC negotiation", "Rulindo Ready Boards installed by Team 02 04082026.xlsx" ), sheet = "Sheet1") |> 
  clean_names()

rulindo_installed<- read_xlsx(path =file.path(data_path_2, "Readyboard EPC negotiation", "Rulindo Ready Boards installed by Team 02 04082026.xlsx" ), sheet = "Sheet1") |> 
  clean_names() |>
  mutate(
    installed = ifelse(grepl("not installed", comment, ignore.case = TRUE), 0, 1)
  ) |> 
  filter(installed == 1) |> 
  mutate(installed_id = nid) |> 
  mutate(
    nid = ifelse(nid == "1195780036166019", "1198080112173019", nid),
    nid = ifelse(nid == "1198180105341033", "1198180105341030", nid),
    nid = ifelse(nid == "1196380047274105", "1196580044343077", nid),
    nid = ifelse(nid == "1198270134559079", "119827013455073", nid),
    nid = ifelse(nid == "1199280052386004", "1199280052386000", nid),
    nid = ifelse(nid == "1195980045986090", "1195980045986098", nid),
    nid = ifelse(nid == "1200080015448090", "1200080015448092", nid),
    nid = ifelse(nid == "1198380117993040", "1198380117993042", nid)

  ) |> 
  rename(village_id = village_id_key) %>% 
  mutate(installed = 1 ) %>% 
  select(village_id, nid, installed, installed_id)


dup_df<- tibble(duplicate_id = rulindo_installed$installed_id[duplicated(rulindo_installed$installed_id)])


check <- rulindo_installed %>%
  filter(!nid %in% rulindo_dime$nid)


id_issues <- rulindo_installed_raw%>% 
  filter(nid %in% check$nid)

rulindo_raw_check <- rulindo_installed_raw %>% 
  filter(nid %in% check$nid) |> 
  mutate(across(everything(), str_to_lower))

write_xlsx(
  dup_df,
  path = file.path(data_path_2, "Readyboard EPC negotiation", "Rulindo", "Duplicate NID in Rulindo readyboard installation.xlsx")
)









##Rusizi----

##Rusizi1-----


library(tidyverse)
library(readxl)
library(janitor)

# 1. Read and clean installed dataset


rusizi1_installed_raw <- read_xlsx(
  file.path(data_path_2, "Readyboard EPC negotiation", "Rusizi_installed.xlsx"),
  sheet = "lot 1"
) %>%
  clean_names() %>%
  mutate(
    id = gsub("[^0-9]", "", id),
    first_name = str_to_lower(first_name),
    last_name  = str_to_lower(last_name),
    full_name  = paste(last_name, first_name)
  ) %>%
  mutate(
    id = case_when(
      id == "1197270035899039" & first_name == "mukankurunziza" ~ "1197270035896039",
      id == "1194078708729150" & last_name == "mukaniyitanga" ~ NA_character_,
      id == "1199570048726820" & last_name == "uwayezu" ~ NA_character_,
      TRUE ~ id
    )
  )

# 2. Clean DIME dataset



rusizi1_dime <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rusizi-1.xlsx"),
                          sheet = "household list")
rusizi1_dime_village <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rusizi-1.xlsx"),
                                  sheet = "village list")



##Rusizi lot 2-------


rusizi2_dime <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rusizi-2.xlsx"),
                          sheet = "household list")
rusizi2_dime_village <- read_xlsx(path = file.path(data_path_2, "EDCL", "Readyboard by lot", "Lot_Rusizi-2.xlsx"),
                                  sheet = "village list")

rusizi1_dime_clean <- rusizi1_dime %>%
  mutate(
    nid = gsub("[^0-9]", "", nid),
    first_name = str_to_lower(first_name),
    last_name  = str_to_lower(last_name),
    full_name  = paste(last_name, first_name)
  )


# 3. Match by ID

rusizi1_match_id <- rusizi1_installed_raw %>%
  left_join(
    rusizi1_dime_clean %>% select(nid, villageid_key),
    by = c("id" = "nid")
  )

# 4. Match remaining by FULL NAME + village


rusizi1_remaining_name <- rusizi1_match_id %>%
  filter(is.na(villageid_key))

rusizi1_match_name <- rusizi1_remaining_name %>%
  left_join(
    rusizi1_dime_clean %>% select(full_name, villageid_key, nid),
    by = c("full_name", "village_id" = "villageid_key")
  )


# 5. Match remaining by LAST NAME + village


rusizi1_remaining_last <- rusizi1_match_name %>%
  filter(is.na(nid)) %>% 
  select(-nid)

rusizi1_match_last <- rusizi1_remaining_last %>%
  left_join(
    rusizi1_dime_clean %>% select(last_name, villageid_key, nid),
    by = c("last_name", "village_id" = "villageid_key")
  )


# 6. Combine matches

rusizi1_installed_final <- bind_rows(
  rusizi1_match_id %>% filter(!is.na(villageid_key)) %>% mutate(nid = id),
  rusizi1_match_name %>% filter(!is.na(nid)),
  rusizi1_match_last %>% filter(!is.na(nid))
) %>%
  distinct(village_id, nid, .keep_all = TRUE) %>%
  mutate(installed = 1) %>%
  select(village_id, nid, installed, installed_id = id)


# 7. Check unmatched installed households

rusizi1_unmatched <- rusizi1_installed_raw %>%
  filter(!id %in% rusizi1_installed_final$installed_id)


write_xlsx(
  rusizi1_unmatched,
  path = file.path(
    data_path_2,
    "Readyboard EPC negotiation",
    "Rusizi",
    paste0(
      "Rusizi1 households installed not on list_",
      Sys.Date(),
      ".xlsx"
    )
  )
)


#Rusizi1 two villages unmatched-----

rusizi1_installed_two_villages<- read_xlsx(file.path(data_path_2, "Readyboard EPC negotiation", "Rusizi1 households installed not on list_0512_07282026.xlsx")) 

rusizi1_dime_two <- rusizi1_dime |> 
  filter(villageid_key %in% rusizi1_installed_two_villages$village_id)

write_xlsx(rusizi1_dime_two, path = file.path(data_path_2, "Readyboard EPC negotiation", "rusizi1_two_villages.xlsx"))

rusizi1_dime_two_installed <- read_xlsx(file.path(data_path_2, "Readyboard EPC negotiation", "Rusizi1_matching_two_villages.xlsx")) |> 
  mutate(installed = 1) |> 
  mutate(village_id = ifelse("Installed Village" == "Mutara", "36030205", "36030101")) |> 
  rename(nid = `Matched National ID`,
         installed_id = `Installed National ID` ) |> 
  select(village_id, nid, installed, installed_id )


rusizi1_installed <- rbind(rusizi1_dime_two_installed, rusizi1_installed_final)



##Rusizi2----

rusizi2_installed <- read_xlsx(path =file.path(data_path_2, "Readyboard EPC negotiation", "Rusizi_installed.xlsx" ), sheet = "lot 2")   %>% 
  clean_names() %>% 
  mutate(
    nid = ifelse(id == "1195770018361066", "12630104", id)
  ) %>%
  rename(installed_id = id) %>% 
  mutate(installed = 1 ) %>% 
  select(village_id, nid, installed, installed_id)

dup_df.2 <- tibble(duplicate_id = rusizi2_installed$installed_id[duplicated(rusizi2_installed$installed_id)])


dup_df.2 <- rusizi2_installed %>% 
  filter(installed_id %in% dup_df.2$duplicate_id)


check <- rusizi2_installed %>%
  filter(!nid %in% rusizi2_dime$nid)





#Join to master--------

rusizi_installed <- rbind(rusizi1_installed, rusizi2_installed)

rusizi_installed_join <- rusizi_installed %>% 
  select(-village_id)

installed_join <- rbind(rusizi_installed, karongi_installed, rulindo_installed, rutsiro_installed) |> 
  select(-village_id)

master <- read_xlsx(path = file.path(data_path_2, "Readyboard EPC negotiation", "master.xlsx"))

master_installed <- left_join(master, installed_join)



write_xlsx(master_installed, path = file.path(data_path_2, "Readyboard EPC negotiation", "master_installed.xlsx"))



write_xlsx(master_installed, path = file.path(data_path_2, "Latest Scope", "readyboard_installed.xlsx"))

