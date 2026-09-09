#----------------------------------------------------------------------
#Author: Yeji Kim
#Date: 2026-09-01
#Purpose: extract 180 sample villages 
#Output: villages_180_full = read_csv(file.path(OUTPUT_ML, "A-villages-180.csv"))
#----------------------------------------------------------------------

#-----------#
# Setup ####
#-----------#

# Clear environment (Optional)
rm(list = ls())

# Libraries 
pacman::p_load(here, openxlsx, dplyr, tidyr, stringr, readr, knitr)

# Set path
here() #should be individual-path/github/rwanda-energy-project
source(here("PATHS.R"))

#-----------#
# Import Data ####
#-----------#

# Households ####
baseline_1973 <- read.xlsx(
  # n=1973, 1973 unique households, 180 villages across 1727 variables
  file.path(DATA_ANALYSIS, "baseline_1973_clean.xlsx"))

vulnerable_hh <- openxlsx::read.xlsx( 
  # n=1973, 1973 unique households, 180 villages across 10 variables 
  file.path(DATA_CTO_ML, "0-midline-vulnerable-household-sample.xlsx"))

# Villages #### 
villages_181 = read.xlsx(
  # Outcome of '0. complete status for EDCL.R' script in baseline analysis pipeline  
  # villages_181 is created using 'complete_status' and filtering for `Dropped from scope due to 15kv` == "No"
  file.path(OUTPUT_ANALYSIS, "Surveyed 181 villages.xlsx")) # list of 181 villages 

#### Villages ####
baseline_treatment <- read.xlsx( 
  # n= 193 villages and their survey, scope status
  file.path(DATA_CTO_BL, "scope_193_0807.xlsx")) #village treatment

#-----------#
# Analysis
#-----------#

# Step 1 ####
# Pull sample villages from baseline sample
villages_180 = baseline_1973 |> group_by(village) |> summarise(n=n()) |> ungroup()
# Filter out the mismatch village 
village_1 = villages_181 |> anti_join(villages_180 |> mutate(villageid_key = as.character(village)), by = "villageid_key") 
#32110109, Rwamigega

# Step 1-2
# Filter out the 180 villages from vulnerable_hh
villages_180_full = vulnerable_hh |> 
  select(district, sector, cell, village, villageid_key) |>
  group_by(villageid_key) |> 
  slice(1) |>
  mutate(district = str_to_title(district))

# Step 1-3 
# Check if the villages match with baseline_1973 villages
check = villages_180_full |> anti_join(villages_180 |> mutate(villageid_key = as.character(village)), by  = "villageid_key") 

# Step 2
# Merge the 180 villages treatment status
villages_180_full_treatment = villages_180_full |> 
  inner_join(baseline_treatment |> select(strata, treat, village_id, name) |>
               rename(village = name,
                      villageid_key = village_id),
             by = join_by(village, villageid_key))

# Step 2-1 
# From which strata are the 13 dropped villages from?
villages_dropped = baseline_treatment |> 
  select(strata, treat, village_id, name) |> 
  rename(village = name, villageid_key = village_id) |>
  anti_join(villages_180_full, by = join_by(village, villageid_key)) |>
  arrange(strata, treat)

# Step 2-2
# What is the size of the overall treatment arms?
baseline_sum = baseline_treatment |> group_by(treat) |> summarize(baseline = n())
midline_sum = villages_180_full_treatment |> group_by(treat) |> summarize(midline = n())

treatment_dist <- baseline_sum |>
  full_join( midline_sum, by = "treat") |>
  mutate(dropped = baseline - midline)

treatment_dist <- treatment_dist |>
  bind_rows(
    treatment_dist |> summarise(
      treat = "Total", # what text will be in treat column
      across(c(baseline, midline, dropped), sum) # operation for "Total" row 
    )
  )

# Save Output
# Save 180 villages
write_csv(villages_180_full, file.path(DATA_ML_SCOPE, "A-villages-180.csv"))

# Save 180 villages with treatment 
write_csv(villages_180_full_treatment, file.path(DATA_ML_SCOPE, "A-villages-180-treatment.csv"))

# Save 13 villages dropped
write_csv(villages_dropped, file.path(DATA_ML_SCOPE, "A-villages-13-dropped.csv"))

# treatment distribution table (LATEX)
writeLines(
  treatment_dist |>
    kable(
      format = "latex", booktabs = TRUE,
      col.names = c("Arm", "Baseline (193)", "Midline (180)", "Dropped"),
      align = "lrrr",
      caption = "Village retention from baseline to midline sample"
    ),
  file.path(DATA_ML_SCOPE, "A-treatment-distribution.tex")
)