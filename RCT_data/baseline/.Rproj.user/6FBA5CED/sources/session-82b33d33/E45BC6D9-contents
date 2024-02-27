##############
#Author: Xiaoming Zhang
#Date: 2.26.2024
#Purpose: Randomization primary construction
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
  "baseline/data/data/four_district_2402.xlsx"
)

#read file----
four_district_2402 <- read_xlsx(path)



#Status update----

#"newly" is in the randomization scope, "partial" is not

four_district_2402.1 <- four_district_2402 %>% 
  mutate(any_offgrid = case_when(
    et_sum != 0 | priority_0 == 0 ~ "partial",
    .default = "newly"
  ))

four_district_2402.1 <- four_district_2402.1 %>% 
  mutate(
    any_grid = case_when(
      grid_status %in% c("newly") & nep_revision%in% c("GE", "Fill In")  ~ "newly",
      .default = "partial"
    )
  )

four_district_2402.1 <- four_district_2402.1  %>% 
  mutate(status = case_when(
    any_grid == "newly" & any_offgrid == "newly" ~ "newly",
    .default = "partial"
  ))

table(four_district_2402.1$scope_2401)

four_district_2402.1  <- four_district_2402.1 %>% 
  mutate(
    status = ifelse(ubudehe_1 <20, "partial", status)
  )


four_district_2402.1 <- four_district_2402.1 %>% 
  mutate(meter_percent = round(meter_eucl/total_hh, 2))

table(four_district_2402.1$status)

four_district_2402.2  <- four_district_2402.1 %>% 
  mutate(
    status = ifelse(meter_percent >= 0.3, "partial", status)
  )

table(four_district_2402.2$status)



#Karongi+Rutsiro+Rulindo----

three_scope_2402.1 <- four_district_2402.2 %>% 
  filter(district %in% c("Karongi", "Rulindo", "Rutsiro") & scope_2402 == 1)


table(three_scope_2402.1$status)









#randomization----

three_scope_rand <- three_scope_2402.1 %>% 
  mutate(strata_randomization = paste(lot, "-", status))

table(three_scope_rand$strata_randomization)


###Newly----
threescope_newly <- three_scope_rand%>% 
  filter(status == "newly")

table(threescope_newly$strata_randomization)




set.seed(938485)

newly_treatment <- block_ra(
  blocks = threescope_newly$strata_randomization,
  num_arms = 4,
  prob_each = c(0.25, 0.25, 0.25, 0.25)
)

threescope_newly$treatment <- newly_treatment

#Descriptive table of statistics
newly_count <- threescope_newly %>% 
  group_by(strata_randomization) %>% 
  summarise(
    village_number = n(),
    control_villages = sum(ifelse(treatment == "T1", 1, 0)),
    readyboard_villages = sum(ifelse(treatment == "T2", 1, 0)),
    ubudehe_1 = sum(ifelse(treatment == "T2", ubudehe_1, 0)),
    offgrid_villages = sum(ifelse(treatment == "T3", 1, 0)),
    tbt_villages = sum(ifelse(treatment == "T4", 1, 0))
  )





######partial randomization----
threescope_partial <- three_scope_rand %>% 
  filter(status == "partial")

set.seed(8567)

partial_treatment <- block_ra(
  blocks = threescope_partial$strata_randomization,
  num_arms = 2,
  prob_each = c(0.5, 0.5)
)


threescope_partial$treatment <- partial_treatment

partial_count <- threescope_partial %>% 
  group_by(strata_randomization) %>% 
  summarise(
    village_number = n(),
    ubudehe_1 = sum(ifelse(treatment == "T2", ubudehe_1, 0)),
    control_villages = sum(ifelse(treatment == "T1", 1, 0)),
    readyboard_villages = sum(ifelse(treatment == "T2", 1, 0)),
  )
  
  
  
  
  