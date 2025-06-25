########################################################################################################
#                                                                                                      #
#                                  HIGH-FREQUENCY CHECKS  -- MAIN                                      #
#                                                                                                      #
########################################################################################################

## PURPOSE      High-frequency checks for screening

## AUTHOR  Xiaoming Zhang 

## LAST UPDATE  June 17th 2025

####################################################################################################### 
# remove objects
rm(list=ls())

hfc_code_filepath <- "C:/Users/wb614406/OneDrive - WBG/Documents/GitHub/rwanda-energy-project/RCT_data/baseline/Screening/"

## 4. Run R Scripts ----


source(paste0(hfc_code_filepath,   "1.screening_constr.R"))
print("Script 1 Done.")


source(paste0(hfc_code_filepath,   "2.screening_admin.R"))
print("Script 2 Done.")


source(paste0(hfc_code_filepath,   "3.screening_enum.R"))
print("Script 3 Done.")


source(paste0(hfc_code_filepath,   "4.backcheck.R")) 
print("Script 4 Done.")
