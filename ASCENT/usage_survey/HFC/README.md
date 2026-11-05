## How to run test HFC
Date: 2026-11-05
Author: Yeji

### Purpose
We create a synthetic data that emulates real time data to test current HFC pipeline captures logical errors, attrition, and data quality issues. 
This step allows us to test the pipeline script, review current dashboard components, and revise the script if needed. 

### Prerequisite
1. Macro PATHS for each local machine should be defined at Github/rwanda-energy-project/PATHS.R
2. Check the HFC_MODE_DEFAULT (L14) in Github/rwanda-energy-project/ASCENT/usage_survey/HFC/scripts/hfc_config.R is "test" 
3. Need the most updated questionnaire excel sheet in OneDrive - WBG/Rwanda Energy Project/ASCENT_clean_cooking/questionnaires
 folder named in 'clean_cooking_questionnaire_YYMMDD.xlsx' format.
OneDrive folder is owned by Tanay, if not already shared, request access first.

### Script 
There is one main script for HFC test run: Github/rwanda-energy-project/ASCENT/usage_survey/HFC/test/run_test.R 

This master script will run five main subscripts
(1) 0.generate_synthetic_data.R : generates synthetic data, 
(2) 0.hfc_deidentify.R : deidentifies the synthetic data of Personally Idenfiable Informaiton (PII), 
(3) 1.hfc_main.R : the main HFC script that creates HFC_construct (the main cleaned data) and run different tests, 
(4) 11.hfc_backcheck.R backcheck : samples observations for a backchecker to call back), and finally 
(5) compare_expected_flags.R :  tests whether the pipeline successfully spotted synthetic data of its intentional errors.



