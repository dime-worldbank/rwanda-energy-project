#Author: Yeji Kim
#Date: 2026-09-03
#Key Functions: 
# 1. Sanity check for unique assignment of IDs and village name variables
# 2. Assign admin codes to village level 

#----------------------------
# Setup
#----------------------------
rm(list = ls())
library(pacman)
pacman::p_load(here, readxl, readr)
source(here("PATHS.R"))

#----------------------------
# Import Data
#----------------------------
# Complete National Villages List 
villages_list <- read_excel(
  file.path(DATA_RCT, "0.Rwanda-Villages-Masterfile.xlsx"))

# Sample Village List
villages_180 <- read_csv(file.path(DATA_ML_SCOPE, "A-villages-180.csv"))

#----------------------------------
# Sanity Check
#----------------------------------
# Is ID uniquely assigned for each administrative level?
district_flag = villages_list |> group_by(District) |> 
  summarize(n = n_distinct(Distr_ID)) |> filter(n>1) # 0
sector_flag = villages_list |> group_by(District, Sector) |> 
  summarize(n = n_distinct(Sector_ID))|> filter(n>1) # 1
cell_flag = villages_list |> group_by(District, Sector, Cell) |> 
  summarize(n = n_distinct(Cell_ID)) |> filter(n>1) # 0
village_flag = villages_list |> group_by(District, Sector, Cell, Name) |> 
  summarize(n = n_distinct(Village_ID)) |> filter(n>1) # 6

# One sector and 6 villages have more than one ID assigned
village_flag = villages_list |>
  group_by(District, Sector, Cell, Name) |>
  summarise(n = n_distinct(Village_ID), 
            ids = paste(sort(unique(Village_ID)), collapse = ",")) |> filter(n>1)

# Within our scope of four villages,
# ONE village named "Rusekabuye" has two IDs 41130307,41130308
# Is this in our 180 villages? 

Rusekabuye_name = villages_180 |> filter(village == "Rusekabuye") # 0
Rusekabuye_ID = villages_180 |> filter(villageid_key == "41130307" | villageid_key == "41130308") # 0

# Thankfully no
# Let's then use this as masterlist reference

# Another observation is that there are two variables for village names
# Check if Name and Name.original is the same variable for four district I am interested in
villages_list_4d  = villages_list |> 
  filter(District %in% c("Karongi", "Rulindo", "Rusizi", "Rutsiro")) |>
  mutate(Vill.name.flag = if_else(Name == Name.original, 0, 1)) |>
  rename(Village = Name,
         Village_unclean = Name.original)|>
  select(Prov_ID, Province, Distr_ID, District, Sector_ID, Sector, Cell_ID, Cell, Village_ID, Village, Village_unclean, Vill.name.flag)|>
  mutate(across(c(Province, District, Sector, Cell, Village), str_to_title))

name_flag = villages_list_4d |> filter(Vill.name.flag > 0) # 8 observations 

#--------------
# Join
#--------------
# If villages_180 names are clean it should have 180 obs
villages_ij = villages_list_4d |> 
  inner_join(villages_180 |> mutate(Village_ID = as.character(villageid_key))
             |> rename(
                       District = district,
                       Sector = sector,
                       Cell = cell,
                       Village = village),
             by = c("District", "Sector", "Cell", "Village", "Village_ID")) |>
  select(-villageid_key)

villages_scope = villages_ij |>
  rename(provinceid_key = Prov_ID,
         province_key = Province,
         districtid_key = Distr_ID,
         district_key = District,
         sectorid_key = Sector_ID,
         sector_key = Sector,
         cellid_key = Cell_ID,
         cell_key = Cell,
         villageid_key = Village_ID,
         village_key = Village) |>
  select(-c(Village_unclean, Vill.name.flag))

# a perfect match!

# Save Output

openxlsx::write.xlsx(
  villages_list_4d,
  file.path(DATA_ML_SCOPE, "B.Rwanda-4-Districts-Villages-Masterfile.xlsx"),
  overwrite = TRUE
)

openxlsx::write.xlsx(
  villages_ij,
  file.path(DATA_ML_SCOPE, "B.villages-180-coded.xlsx"),
  overwrite = TRUE
)

write.csv(
  villages_scope,
  file.path(DATA_ML_PRELOAD, "scope_villages.csv")
)

openxlsx::write.xlsx(
  name_flag,
  file.path(DATA_ML_SCOPE, "B.double-names-villages.xlsx"),
  overwrite = TRUE
)

