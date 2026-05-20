# Baseline Analysis - README

## Project Overview
This directory contains R scripts for analyzing baseline survey data from the **Rwanda Energy Access and Quality Improvement Project (R-EAQIP)**. The project involves an RCT with stratified randomization across three treatment arms and a control group.

---

## Dataset Summary

### Primary Data Sources

1. **HFC Survey Data**
   - Location: Rwanda Energy/EAQIP/datawork/HFC/data
   - Content: Household surveys from Lattanzio (demographics, energy usage, income, savings, well-being)
   - Key Variables: Demographics (A1-A5), Well-being (B4-B5), Savings (E1-E4), Energy (H4-H8), WTP (J1-J6)

2. **Screening Data** 
   - Location: Rwanda Energy/EAQIP/datawork/Screening/data
   - Format: REP_screening_WIDE.csv

3. **Vulnerable Households List**
   - Tracks vulnerable households and completion status

4. **Treatment Assignments**
   - 193 villages across 4 arms: Control, T1 (Readyboard), T2 (SHS), T3 (Combined)

5. **EPC Readyboard Beneficiary Lists**
   - By district: Rulindo, Karongi, Rutsiro, Rusizi

---

## Script Descriptions

### 0. complete status for EDCL.R
Tracks survey completion status and administrative achievements. Creates summaries by district and treatment arm.

**Outputs**: Achievement tables, completion tracking spreadsheets

---

### 1. 5 clean descriptives.R
Applies comprehensive data cleaning pipeline: removes outliers (3SD rule), handles missing values, constructs income variables.

**Outputs**: Cleaned dataset, village_size_distribution.png

---

### 1. descriptives shorter.R  
Generates balance tables across treatment arms using cluster-robust standard errors.

**Outputs**: balance_table_results.xlsx, LaTeX tables

---

### 2. balance table.R
Tests baseline covariate balance across treatment arms.

---

### 3. Willingness to Pay.R
Analyzes household willingness to pay for energy services across districts and treatment arms.

---

### 4. log regression.R
Estimates treatment effects using OLS regression with cluster-robust inference.

---

### 5. bbl plots.R
Creates binned scatter plots and visualizations of baseline balance.

---

### epc readyboard.R
Matches household data with Readyboard installation records. Identifies villages with/without installations.

**Outputs**: master_krr.xlsx, readyboard coverage analysis

---

### gps match customer.R
Data validation and deduplication using GPS coordinates.

---

### name match customer.R
Data quality tool using fuzzy name matching.

---

## Treatment Arms

- **Control (C)**: No intervention
- **T1**: Readyboard installation
- **T2**: Solar Home System (SHS)  
- **T3**: Readyboard + SHS combined

## Study Coverage

- **193 villages**: Initial randomization
- **181 villages**: After dropping 15kV villages
- **4 districts**: Rulindo, Karongi, Rutsiro, Rusizi

---

## Key Dependencies

	idyverse, dplyr, eadxl, writexl, ggplot2, room, sandwich, clubSandwich

---

## Running Scripts

1. Execute in numerical order (0-5)
2. Each assumes cleaned data from previous steps
3. Update Dropbox paths as needed

---

## Important Notes

- Missing codes: -77 (don't know), -88 (refuse), -99 (other)
- Outliers removed if > 3 SD or winsorized at 95th percentile
- Cluster-robust standard errors at village level
- All scripts use pacman::p_load() for automatic package loading
