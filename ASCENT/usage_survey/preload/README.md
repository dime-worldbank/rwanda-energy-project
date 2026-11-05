# ASCENT Usage Survey Preload data 

## Overview
This phone survey targets 1,000 sample households from beneficiary list that received cooking stove from government subsidy program. 

## Preload 
The R scrips contained in this 'preload' folder creates three data required for the form to be uploaded in SurveyCTO.  

1) enumerator.csv (for testing, uses test_enumerator.csv)
2) scope_villages.csv (contains the administrative divisions -- district, sector, cell, village -- of the beneficiary households)
3) hoousehold_sample.csv (contains 1300 household sample, 1000 for the main survey and 300 for replacement)

### TODO 
1) test_enumerator. csv and 2) scope_villages.csv is not yet created as of 2926-11-05. This is because confirming the sampling method and cleaning of the raw beneficiary data is needed. 

## Raw Beneficiary List Data 

### Folder location - 

`C:\Users\wb636130\Dropbox\Rwanda Energy\ASCENT\questionnaires\preload` clean-cooking-beneficiaries-list-raw.csv 

### Key variables: 

household id, beneficiary name and phone number, district, cell, village names, cook stove product name, date purchased,  subsidy amount and amount paid by the customer. 

### Important note
a)  the raw data has 45% of missing household IDs. 

b) Also, the administrative levels only contain district, cell, village names and sector name is MISSING. 

For cascade selection of District > Sector > Cell > Village > HHID in the survey form, we need each administrative level's ID. The detailed approach to address this gap is described below in 1.beneficiary_list.R. 

## What each script does 

### 1.beneficiary_list.R
- Function 0: Check for missing obs in key variables 
- Function 1: Remove missing hhid 
- Function 2: From product name, categorize them into stove_type (LPG and EPC)
- Function 3: Add official admin codes (province, sector, id keys) from the EAQIP village list (vills.csv), matched on district + cell + village names

### TODO - decide how to proceed 
Function 3 first matches on the district, cell, village names and based on the name match, assign each admin level's key and sector & sector key. 

Result: 
- 43,033 households matched for district, cell, village names 
- 6 households had ambiguous sectors (two possible sectors could exist for the same pair of district, cell, village)

   District   Cell       Village    Official candidates (sector, villageid_key)
   Gasabo     Gasagara   Gasagara   Gikomero 12030103  or  Rusororo 12140202
   Gatsibo    Kigabiro   Kigabiro   Gasange  53010102  or  Remera   53120305
   Nyagatare  Kabeza     Kabeza     Gatunda  52010202  or  Rwimiyaga 52130202

#### Issue: 

The issue rose when ensuring the Household ID correctly reflects the District(DD), Sector(DDSS), Cell(DDSSCC), Village IDs (or keys) (DDSSCCVV). 

Ideally, Household IDs should fully incorporate the village IDs and add household digits after. However, of 43,033 observations only 45% household IDs correclty encoded village IDs.  

This mismatch needs to be addressed before creating the preload data. 

#### Todo - The question then is: 

Do we keep the HHID provided in the raw dataset? (there is no clear alternative)

If so, do we keep the inconsistencies and still include District, Sector, Cell, Village questions in the form?

(This way, we will create the household_sample that has District, Sector, Cell, Village names and IDs from the vills.csv but the household IDs from the raw data)

### 2.sampling.R

From cleaned beneficiary_list from 1.beneficiary_list.R samples 1,300 households. 


We sample equal number per strarum, and it is 2X2 metric of Stove Type X Year of Purchase. 

Since most of the sample (67%) has received LPG, the weight of each sampled households from LPG stratum is 2-9 times greater than EPC stratum.

Stratum	Frame	Main	Replacements	Weight
EPC 2025	2,290	250	75	9.2
EPC 2026	5,573	250	75	22.3
LPG 2025	21,519	250	75	86.1
LPG 2026	11,986	250	75	47.9

#### Todo - confirm method 
If we want the sample size to reflect the sample pool size (i.e. main), a different sampling strategy should be applied. 
