
# Datapaths are defined at Github / rwanda-energy-project/PATHS.R

# Sample of 5569 Households for Baseline Survey 
# Eligible, non-eligible, and consented, not consenting all mixed
# Contains phonenumber and second_phonenumber
# TODO is this actually the og data?
baseline_hh_og   <- read.csv(file.path(DATA_CTO_BL, "REP_baseline_test_WIDE (2).csv"))
baseline_hh_treatment <- read.xlsx(file.path(DATA_ANALYSIS, "Updated scope villages& households", "Latest Scope", "household_list(survey status & scope & treatment).xlsx"))

# Sample of 1973 Households from baseline analysis 
vulnerable_hh <- openxlsx::read.xlsx(file.path(DATA_CTO_ML, "0-midline-vulnerable-household-sample.xlsx"))
baseline_1973 <- read.xlsx(file.path(DATA_ANALYSIS, "baseline_1973_clean.xlsx"))

