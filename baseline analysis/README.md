# Baseline Analysis - README

## Project Overview
This directory contains R scripts for analyzing baseline survey data from the **Rwanda Energy Access and Quality Improvement Project (R-EAQIP)**. The project involves an RCT with stratified randomization across three treatment arms (Readyboard, SHS, and combined) and a control group across multiple villages in Rwanda.

---

## Dataset Summary

### Primary Data Sources

1. **HFC Survey Data** (`hfc_constr_raw`)
   - Location: `Rwanda Energy/EAQIP/datawork/HFC/data`
   - Format: Household survey responses from Lattanzio in both rounds
   - Content: Comprehensive household information including demographics, energy usage, income, savings, and well-being indicators
   - Key Variables: Demographics (A1-A5), Well-being (B4-B5), Savings (E1-E4), Energy expenditure (H4-H8), Willingness to pay (J1-J6)

2. **Screening Data** 
   - Location: `Rwanda Energy/EAQIP/datawork/Screening/data`
   - Format: `REP_screening_WIDE.csv`
   - Content: Screening survey results from pilot enumerators

3. **Vulnerable Households List**
   - Location: `Updated scope villages& households/vulnerable households in sample villages.xlsx`
   - Content: List of vulnerable households in sampled villages with completion status tracking

4. **Treatment Assignments**
   - Location: `RCT_data/baseline/data/scope_193_0807.xlsx`
   - Content: Stratified randomization results (193 villages across 4 arms: Control, T1-Readyboard, T2-SHS, T3-Combined)

5. **EPC Readyboard Beneficiary Lists**
   - Beneficiary lists by district (Rulindo, Karongi, Rutsiro, Rusizi-1, Rusizi-2)
   - Track which households have Readyboard installations

---

## Script Descriptions

### 0. complete status for EDCL.R
**Purpose**: Track survey completion status and administrative achievements

**Key Functions**:
- Loads HFC household survey data and screening data
- Merges treatment assignments with household data
- Compares 193 villages (all randomized) vs 181 villages (after dropping 15kV villages)
- Calculates completion metrics by district and treatment arm
- Tracks enumerator assignments and survey dates

**Inputs**:
- HFC construction data (`hfc_constr_0728.xlsx`)
- Vulnerable households list
- Treatment assignments
- Enumerator information

**Outputs**:
- `achievements by district.tex` - Summary table of villages and households reached
- `treatment compare.tex` - Distribution of villages by treatment arm
- `survey status of vulnerable households in sample villages_final(enumerator).xlsx` - Comprehensive completion tracking
- Excel files documenting surveyed villages and completion status

---

### 1. 5 clean descriptives.R
**Purpose**: Clean HFC household data and generate preliminary descriptive statistics

**Key Functions**:
- Loads raw HFC data and applies comprehensive data cleaning pipeline
- Handles missing values and outliers (3 standard deviations rule)
- Constructs derived variables (e.g., household income from occupation data)
- Creates weighted summaries accounting for village-level survey design

**Data Cleaning Steps**:
1. **Global cleaning**: Replace special codes (-77, -88, -99) with NA
2. **Age variables**: Remove negative values, remove 3SD outliers
3. **Well-being indicators**: Remove negative values
4. **Savings variables**: Remove outliers, apply 95th percentile winsorization
5. **Energy expenditure**: Replace NAs with 0, remove outliers
6. **Lighting hours**: Standardize to 0-24 hour scale
7. **Income construction**: Calculate weekly income from various occupations (hourly, daily, weekly, seasonal)

**Key Outputs**:
- Cleaned household dataset with derived variables
- `village_size_distribution.png` - Distribution of households surveyed per village
- Summary statistics by treatment arm

---

### 1. descriptives shorter.R  
**Purpose**: Generate balance tables and baseline descriptive statistics

**Key Functions**:
- Constructs a flexible balance table function for hypothesis testing
- Compares baseline characteristics across treatment arms
- Performs heteroskedasticity-robust inference with cluster-robust standard errors
- Generates LaTeX-formatted tables for publication

**Balance Table Variables**:
- Income variables (head/member primary & secondary occupations)
- Savings (formal and informal)
- Energy expenditure (candles, biomass)
- Well-being indicators (energy reliability, efficiency, accessibility, satisfaction)
- Subjective ladder measures (personal well-being, cooking, charging, lighting, energy satisfaction)

**Function Features**:
- Weighted and unweighted means
- Cluster-robust standard errors (CR2 type)
- P-value significance markers (***, **, *, .)
- Binary variables displayed as percentages
- Sample sizes reported

**Outputs**:
- `balance_table_results.xlsx` - Household-level balance table
- `balance_table_results(village_level).xlsx` - Village-level balance table
- LaTeX tables for academic publications

---

### 2. balance table.R
**Purpose**: Analyze baseline covariate balance and create final balance tables

**Status**: Focuses on balance testing across treatment arms

**Key Analysis**:
- Tests whether treatment assignment is random (no significant baseline differences)
- Uses treatment variable as key variable
- Generates summary tables by district and treatment status

---

### 3. Willingness to Pay.R
**Purpose**: Analyze household willingness to pay (WTP) for energy services

**Key Variables Analyzed**:
- `J1_final` - Final WTP amount
- `J2_1`, `J3_1` - WTP responses
- `J4_2`, `J5_2` - WTP for different time horizons
- `wtp_12`, `wtp_24` - Annualized WTP estimates

**Analysis Type**:
- Descriptive statistics of WTP across districts
- WTP variations by treatment arm
- Correlations with household characteristics

---

### 4. log regression.R
**Purpose**: Estimate treatment effects using OLS regression models

**Analysis**:
- Regresses outcomes on treatment indicators
- Controls for baseline characteristics
- Cluster-robust inference at village level
- Outputs regression tables in publication format

**Expected Outputs**:
- Regression coefficients with standard errors
- Treatment effect magnitudes
- Statistical significance tests
- LaTeX/publication-ready tables

---

### 5. bbl plots.R
**Purpose**: Create visualization of baseline balance across variables

**Outputs**:
- Binned scatter plots (BBL plots) showing balance
- Distribution plots of key variables by treatment arm
- Graphical representation of treatment arm comparisons

---

### descriptives.R
**Purpose**: Generate general descriptive statistics

**Status**: Earlier version; see "1. 5 clean descriptives.R" and "1. descriptives shorter.R" for current implementation

---

### epc readyboard.R
**Purpose**: Match household survey data with Readyboard installation records

**Key Functions**:
- Loads EPC (Energy Project Consultant) beneficiary lists by district
- Merges with household survey data using name and NID matching
- Creates master file linking households to Readyboard status
- Identifies villages with no Readyboard installations
- Compares with DIME readyboard allocation lists

**Data Sources**:
- EPC beneficiary lists: Rulindo, Karongi, Rutsiro, Rusizi (Lot 1 & 2)
- DIME readyboard allocation lists by lot
- Baseline survey data from HFC and screening

**Key Variables Created**:
- `readyboard` - Whether household is in Readyboard beneficiary list (1/0)
- `surveyed` - Whether household was surveyed
- `vulnerable` - Indicator for vulnerable households

**Outputs**:
- `master_krr.xlsx` - Master household file with Readyboard status
- `Village with no readyboard(karongi&rutsiro&rusizi).xlsx` - Villages missing Readyboard installations
- Analysis of coverage by district and treatment arm

---

### gps match customer.R
**Purpose**: Match survey households using GPS coordinates

**Status**: Data validation and deduplication tool

---

### name match customer.R
**Purpose**: Match survey households using name-based fuzzy matching

**Status**: Data quality and deduplication tool

---

### old log.R
**Purpose**: Earlier version of log regression analysis

**Status**: Legacy code; current version is "4. log regression.R"

---

### untitled.R
**Purpose**: Placeholder/incomplete script

**Status**: Under development or experimental code

---

## Supporting Files

### Output Files

1. **Tables** (`output/tables/`)
   - `stratified_randomization_results.tex` - Randomization check
   - `treatment_compare.tex` - Treatment arm distribution
   - `achievements_by_district.tex` - Survey completion achievements
   - `balance_table_results.xlsx` - Household-level balance statistics
   - `balance_table_results(village_level).xlsx` - Village-level balance statistics

2. **Figures** (`output/figures/`)
   - `village_size_distribution.png` - Distribution of households per village
   - `primary_lighting_energy_source.png` - Energy source visualization

3. **Data Files**
   - `regression_output.latex` - Model output in LaTeX format
   - `regression_output.txt` - Model output in text format
   - `village_unmatched_summary.xlsx` - Tracking unmatched villages

---

## Key Analysis Outputs

### Treatment Arms
- **Control (C)**: No intervention
- **T1**: Readyboard installation
- **T2**: Solar Home System (SHS) installation  
- **T3**: Readyboard + SHS (combined intervention)

### Study Coverage
- **193 villages**: All villages in initial randomization
- **181 villages**: After dropping villages with 15kV electricity access (outside scope)
- **4 districts**: Rulindo, Karongi, Rutsiro, Rusizi

### Key Metrics Tracked
- Vulnerable households identified per village
- Households reached by Lattanzio enumerators
- Households successfully surveyed
- Readyboard installation coverage
- Completion rates by district and treatment arm

---

## Data Flow Summary


Raw Survey Data (HFC)
        ↓
[0. Complete Status] → Completion tracking & achievements
        ↓
[1. Descriptives] → Data cleaning & derived variables
        ↓
[2. Balance Table] → Baseline covariate balance
        ↓
[3. WTP Analysis] → Willingness to pay estimation
        ↓
[4-5. Regression & Plots] → Treatment effects & visualization
        ↓
[EPC Readyboard] → Link to implementation data
```

---

## Running the Scripts

### Execution Order
1. Run scripts in numerical order (0 → 5)
2. Each script assumes cleaned data from previous steps
3. Update Dropbox paths if data location changes

### Dependencies
- Required packages: `tidyverse`, `dplyr`, `readxl`, `writexl`, `ggplot2`, `broom`, `sandwich`, `clubSandwich`
- Data paths point to shared Dropbox directory

---

## Notes for Researchers

- All scripts use `pacman::p_load()` for automatic package loading
- Dropbox path is hardcoded; modify for different systems
- Results use robust standard errors clustered at village level
- Missing value codes: -77 (don't know), -88 (refuse), -99 (other)
- Outliers: Generally removed if > 3 standard deviations or winsorized at 95th percentile
- Weights account for village-level sampling design