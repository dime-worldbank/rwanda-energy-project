# Reconstruct Kazera's Count of Ready Board Implementation
# Break it down by villages 
# Compare it with Xiaoming's count 

# libraries
#rm(list = ls())

library(pacman)
pacman::p_load(readxl, writexl, here, tidyverse)
source(here("PATHS.R"))

# Rutsiro #####
# Import Dataset 
readyboard_rutsiro1 = read_xlsx(file.path(READYBOARD_K, "Rutsiro", "Ready Board List-EDALI.xlsx"))
readyboard_rutsiro2 = read_xlsx(file.path(READYBOARD_K, "Rutsiro", "Ready Board List-Team 3.xlsx"))
readyboard_rutsiro3 = read_xlsx(file.path(READYBOARD_K, "Rutsiro", "Ready Board List.xlsx"))

# key variables
key_variables <- c(
  "district", "sector", "cell", "village", "villageid_key",
  "first_name", "last_name", "gender", "nid", "install"
)

# Filter only for the installed 
rutsiro1 <- readyboard_rutsiro1 |>
  rename(Install = `Installation Status: If Install, Just Mark Install. If Not Install, Put The Reason`) |>
  filter(Install == "Installed") |>
  clean_names()|>
  rename(nid = n_id) |>
  select(all_of(key_variables))

rutsiro2 = readyboard_rutsiro2 |>
  rename(Install = `Installation Status\r\nIf install, just mark install\r\nIf Not Install, put the reason`) |>
  filter(Install == "Installed") |>
  clean_names() |>
  select(all_of(key_variables))

rutsiro3 = readyboard_rutsiro3 |>
  rename(Install = `Installation Status\r\nIf install, just mark install\r\nIf Not Install, put the reason`) |>
  filter(Install == "installed") |>
  clean_names() |>
  select(all_of(key_variables))

installed_rutsiro = rbind(rutsiro1, rutsiro2, rutsiro3)

# Summarize village 
village_rutsiro = installed_rutsiro |>
  group_by(village) |>
  summarise(
    n_villageid = n_distinct(villageid_key),   # 1 = one id per village (what you want)
    villageid   = first(villageid_key),        # the id itself (safe to show if n_villageid == 1)
    n_1           = n(),                          # installed ready boards per village
    .groups = "drop"
  )

# Bring Xiaoming's Data 


# Rulindo ####

# Import Dataset
readyboard_rulindo1 = read_xlsx(file.path(READYBOARD_K, "Rulindo", "Ready Boards installed by Team 02 04082026.xlsx"))
readyboard_rulindo2 = read_xlsx(file.path(READYBOARD_K, "Rulindo", "Team 01 Ready Board Name List -Lot_Rulindo Site.xlsx"))

# Filter for only 
rulindo1 <- readyboard_rulindo1 |>
  rename(villageid_key = `village id key`) |>
  mutate(
    install = trimws(install), # removes whitespace at the beginning and end of a text string 
    install = ifelse(
      grepl("not installed", trimws(install)),
      "not installed",
      "installed"
    )
  ) |>
  filter(install == "installed") |>
  select(all_of(key_variables))

rulindo2 <- readyboard_rulindo2 |>
  mutate(
    install = ifelse(
      grepl("NOT", install), "not installed", "installed")
    ) |>
  filter(install == "installed") |>
  select(all_of(key_variables))

installed_rulindo = rbind(rulindo1, rulindo2)

# Rusizi1
readyboard_rusizi_l1 = read_xlsx(file.path(READYBOARD_K, "Rusizi", "02 Rusizi Lot1 readyboard tracker by village 03032026.xlsx"))

rusizi_l1 = readyboard_rusizi_l1 |>
  mutate(install = "installed", 
         district = "Rusizi") |>
  rename(nid = id) |>
  select(all_of(key_variables))

installed_rusizi_l1 = rusizi_l1

# Rusizi2
readyboard_rusizi_l2 = read_xlsx(file.path(READYBOARD_K, "Rusizi", "03 Rusizi Lot2 readyboard tracker by village 03032026.xlsx"))

rusizi_l2 = readyboard_rusizi_l2 |>
  mutate(install = "installed") |>
  select(all_of(key_variables))

installed_rusizi_l2 = rusizi_l2

# Karongi ####
# Import Dataset
readyboard_karongi1 = read_xlsx(file.path(READYBOARD_K, "Karongi", "Ready Board List-Belat-Final.xlsx"))
readyboard_karongi2 = read_xlsx(file.path(READYBOARD_K, "Karongi", "READY BOARD KARONGI _ Final (JECO).xlsx"))
readyboard_karongi3 = read_xlsx(file.path(READYBOARD_K, "Karongi", "READY BOARD KARONGI (2).xlsx"))
readyboard_karongi4 = read_xlsx(file.path(READYBOARD_K, "Karongi", "READY BOARD   LIST GBI Final 05072026.xlsx"))

karongi1_raw = readyboard_karongi1 |> 
  mutate(install = if_else(grepl("Not", install), "not installed", "installed")) |>
  #  filter(install == "installed") |>
  clean_names() |>
  select(all_of(key_variables))

karongi2_raw = readyboard_karongi2 |> 
  mutate(install = if_else(grepl("Installed", install), "installed", "not installed")) |>
  #  filter(install == "installed") |>
  clean_names() |>
  rename(villageid_key = village_id_key) |>
  select(all_of(key_variables))

karongi3_raw = readyboard_karongi3 |>
  mutate(install = str_to_lower(install)) |>
  mutate(install = if_else(grepl("install", install), "installed", "not installed")) |>
  clean_names() |>
  #  filter(install == "installed") |>
  select(all_of(key_variables))

karongi4_raw = readyboard_karongi4 |> 
  mutate(install = "installed") |>
  rename(last_name = Name) |>
  clean_names() |>
  select(all_of(key_variables))

installed_karongi_raw = rbind(karongi1_raw, karongi2_raw, karongi3_raw, karongi4_raw)

karongi1 = readyboard_karongi1 |> 
  mutate(install = if_else(grepl("Not", install), "not installed", "installed")) |>
  filter(install == "installed") |>
  clean_names() |>
  select(all_of(key_variables))

karongi2 = readyboard_karongi2 |> 
  mutate(install = if_else(grepl("Installed", install), "installed", "not installed")) |>
  filter(install == "installed") |>
  clean_names() |>
  rename(villageid_key = village_id_key) |>
  select(all_of(key_variables))

karongi3 = readyboard_karongi3 |>
  mutate(install = str_to_lower(install)) |>
  mutate(install = if_else(grepl("install", install), "installed", "not installed")) |>
  clean_names() |>
  filter(install == "installed") |>
  select(all_of(key_variables))

karongi4 = readyboard_karongi4 |> 
  mutate(install = "installed") |>
  rename(last_name = Name) |>
  clean_names() |>
  filter(install == "installed")
  select(all_of(key_variables))

installed_karongi = rbind(karongi1, karongi2, karongi3, karongi4)

karongi_dup_df <- tibble(
  duplicated_nid = installed_karongi_raw$nid[
  duplicated(installed_karongi_raw$nid)
]
)
