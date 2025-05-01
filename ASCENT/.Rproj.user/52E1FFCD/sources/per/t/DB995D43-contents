library(tidyverse)
library(readstata13)
library(foreign)
library(lfe)
library(sf)
library(stringr)
library(writexl)
library(readxl)


setwd("C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/")

# ec <- read.dta13("datawork/admin_data/Establishment Census/2017/")
# ec <- read.dta13("datawork/admin_data/Establishment Census/2014/data-rwa-nisr-ec-2014_stata.dta")
ec <- read.dta13("C:/Users/wb614406/Dropbox/Rwanda Energy/ASCENT/tempcode/ec2020/EC2020_RTDA_WB.dta")

labels <- data.frame(var = names(ec), label = attributes(ec)$var.labels)

# SAMPLE: URBAN RESTAURANTS
ecclean <- ec %>%
  filter(q6_1 == "Accommodation and food service activities" & q1_5_1 == "Urban") %>%
  rename(nemployees = q18_9, paysvat = q24a, size = size,
         dist = q1_2, sect = q1_3_name, cell = q1_4_name)

# ecclean %>%
#   ggplot(aes(x = nemployees, col = paysvat, group = paysvat)) +
#   geom_density(aes(y = ..density..)) +
#   scale_x_log10()

# ecclean %>% count(paysvat)

### CHECKING MOST RESTAURANTS BY DIST/SECT/CELL
counts <- ecclean %>%
  group_by(dist, sect, cell) %>%
  summarise(ntotal = n(),
            ntotalvat = sum(paysvat == "Yes" & nemployees >= 2),
            ntotal234 = sum(size %in% c(2, 3, 4) & nemployees >= 2),
            nemp10 = sum(nemployees >= 10)) %>%
  ungroup

# counts %>%
#   arrange(-ntotalvat) %>%
#   head(50) %>% data.frame

# counts %>%
#   arrange(-ntotal234) %>%
#   head(50) %>% data.frame

# STEPS (BASED ON NTOTALVAT):
# STEP 1: TOP 8 (KIGALI + 1 GISENYI + 1 HUYE) ARE "PRIORITY"
# = 8 CELLS, 246 VAT-PAYING RESTAURANTS
# SPREAD ACROSS 6 SECTORS
# NYARUGENGE/KIMISAGARA+NYARUGENGE, GASABO/KIMIHURURA+REMERA, HUYE/NGOMA, RUBAVU/GISENYI
priority <- counts %>%  filter(ntotalvat >= 18)
nonpriority <- counts %>% filter(ntotalvat < 18)

write_xlsx(priority, path = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/ASCENT/datawork/RCT data/Priority cell.xlsx"))
write_xlsx(non_priority, path = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/ASCENT/datawork/RCT data/None-priority cell.xlsx"))


# STEP 2: IE CELLS (a) >= 3 VAT PAYING RESTAURANTS, (b) AT LEAST 2 SUCH CELLS IN SECTOR
# = 50 CELLS, 325 VAT-PAYING RESTAURANTS
# SPREAD ACROSS 20 SECTORS
# NYARUGENGE/MUHIMA+NYAKABANDA+RWEZAMENYO, GASABO/GISOZI+KACYIRU+KIMIHURURA+KIMIRONKO, KICUKIRO/GATENGA+GIKONDO+KANOMBE+NIBOYE+NYARUGANGA,
# NYANZA/BUSASAMANA, MUHANGA/NYAMABUYE, KARONGI/BWISHYURA, RUBAVU/GISENYI, RUSIZI/GIHUNDWE+KAMEMBE, MUSANZE/MUHOZA, KAYONZA/MUKARANGE
ie <- nonpriority %>% filter(ntotalvat >= 3) %>%
  group_by(dist, sect) %>% filter(n() >= 2) %>% ungroup

write_xlsx(ie, path = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/ASCENT/datawork/RCT data/IE cell.xlsx"))


iealt <- nonpriority %>% filter(ntotalvat >= 2) %>%
  group_by(dist, sect) %>% filter(n() >= 2) %>% ungroup

# STEP 3: HOLDOUT CELLS = REMAINDER
# = 248 CELLS, 233 VAT-PAYING RESTAURANTS
holdout <- nonpriority %>% anti_join(ie)

write_xlsx(holdout, path = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/ASCENT/datawork/RCT data/Holdout cell.xlsx"))

#Electricity access-----
#Overlay these cells with 2022 LV lines to make sure they have electricity 

rwa_cell <- st_read(dsn = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/data/rwa_cell/Cell.shp"))

rwa_cell <- st_make_valid(rwa_cell)

counts_id <- counts %>% 
  mutate(sect = str_to_title(sect)) %>%
    mutate(
      cell = str_replace(cell, " I", " i"),
      cell = str_replace(cell, " Ii", " ii"),
      cell = str_replace_all(cell, " B", " b"),
      cell = str_replace_all(cell, " A", " a"),
      cell = str_replace_all(cell, "Nyamata y' Umujyi", "Nyamata y' umujyi")
      ) %>% 
    left_join(rwa_cell, by = c("cell" = "Name", "sect" = "Sector"))

  
lv_22 <- st_read(dsn = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/ASCENT/datawork/Existing Electrical Network_2022/Existing_LVLine.shp"))

lv_22 <- st_transform(lv_22, crs = st_crs(rwa_cell))
electrified_cell <-st_intersection(rwa_cell, lv_22)  

electrified_cell <- electrified_cell %>% 
  distinct(Cell_ID, .keep_all = TRUE)

electrified_count <- counts_id %>% 
  filter(!Cell_ID %in% electrified_cell$Cell_ID)

write_xlsx(electrified_count, path = file.path("C:/Users/wb614406/Dropbox/Rwanda Energy/ASCENT/datawork/RCT data/No 2022 LV cell.xlsx"))




