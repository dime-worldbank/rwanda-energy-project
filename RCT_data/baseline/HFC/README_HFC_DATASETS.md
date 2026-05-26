# HFC (High-Frequency Checks) Datasets - README

## Project Overview

This directory contains all **High-Frequency Checks (HFC)** data processing scripts and workflows for the **Rwanda Energy Access and Quality Improvement Project (R-EAQIP)** RCT. HFC monitors data quality, survey completion, enumerator performance, and data consistency in real-time during fieldwork.

---

## Primary Datasets

### Raw Data Sources

#### 1. **CTO Survey Submissions - HFC Data**
- **File**: `REP_baseline_test_WIDE.csv`
- **Location**: `Rwanda Energy/EAQIP/datawork/HFC/data/`
- **Format**: Wide CSV export from CommCare
- **Frequency**: Real-time submissions from field
- **Content**: Complete household survey responses

**Key Variables**:
- Submission metadata: `SubmissionDate`, `starttime`, `endtime`, `caseid`
- Household identifiers: `household_id`, `district`, `sector`, `cell`, `village`
- Demographics: `gender`, `age`, `education`, `occupation`
- Energy access: Primary lighting source, energy expenditure
- Well-being: Energy reliability, satisfaction scores
- Savings: Formal and informal savings
- Willingness to pay for energy services

**Data Quality Codes** (converted to NA):
- `-66` = Other
- `-77` = Don't know
- `-88` = Refused
- `-99` = No response

#### 2. **Previous Round HFC Data**
- **File**: `hfc_constr_*.xlsx` (timestamped versions)
- **Location**: `Rwanda Energy/EAQIP/datawork/HFC/data/`
- **Format**: Cleaned Excel exports
- **Content**: Archived versions of cleaned HFC data

---

## HFC Processing Scripts (1-8)

### Script 2: `2.hfc_construct.R` - Data Construction & Cleaning

**Purpose**: Load raw CTO data and perform initial cleaning

**Key Functions**:
1. Parse dates/times (DMY HMS for submissions, YMD HMS for module timestamps)
2. Convert special codes (-66, -88, -77, -99) to NA
3. Filter to main fieldwork (2024-11-11 onward)
4. Construct duration variables (survey length in minutes)

**Outputs**: `hfc_constr` cleaned dataset

---

### Script 3: `3.hfc_admin.R` - Administrative Tracking

**Purpose**: Track survey administration, enumerator assignments, and fieldwork progress

**Key Metrics**:
- Daily submissions count
- Completion rates by district/sector
- Enumerator workload distribution
- Village and market coverage

**Outputs**: `hfc_admin_summary.xlsx` with daily/weekly metrics

---

### Script 4: `4.hfc_outliers.R` - Outlier Detection

**Purpose**: Identify statistical outliers and data quality issues

**Methods**:
- 3 standard deviation rule for numeric variables
- Range checks (age 15-120, duration 5-120 min)
- Logical outliers (negative values, inconsistent responses)
- Domain knowledge checks

**Outputs**: `hfc_outliers_flagged.xlsx` with flagged records and reasons

---

### Script 5: `5.hfc_logic.R` - Logic Checks & Consistency

**Purpose**: Validate internal consistency and logical constraints

**Checks**:
- Skip logic validation
- Consistency across related variables
- Constraint violations (dates, ages)
- Cross-variable consistency
- Time-based checks (survey too fast/slow)

**Outputs**: `hfc_logic_checks.xlsx` with detailed error log

---

### Script 6: `6.hfc_enum.R` - Enumerator Performance

**Purpose**: Monitor individual enumerator quality and productivity

**Metrics**:
- Surveys completed per day
- % submissions with outliers/errors
- Consent refusal rates
- Median survey duration
- Performance ranking (composite score)

**Outputs**: `hfc_enum_performance.xlsx` with scorecards and rankings

---

### Script 7: `7.hfc_stats.R` - Descriptive Statistics

**Purpose**: Generate comprehensive baseline descriptive statistics

**Statistics**:
- Household-level demographics, access, expenditure, savings
- Summary measures: Mean, median, SD, min, max
- Missing data rates and distribution shapes
- Stratified by district, sector, village, treatment arm

**Outputs**: 
- `hfc_descriptives_full.xlsx` - All statistics
- `hfc_descriptives_district.xlsx` - District summaries
- `hfc_descriptives_village.xlsx` - Village aggregates

---

### Script 8: `8.hfc_bc.R` - Backcheck Export

**Purpose**: Export data for quality assurance re-interviews

**Sampling Strategy**:
- 10% stratified random sample
- 100% of critical errors, 50% of major errors
- All surveys with outliers
- All from flagged enumerators

**Outputs**:
- `hfc_backcheck_sample.xlsx` - Sampled households
- `hfc_backcheck_form.pdf` - Printable questionnaire
- Backcheck tracking sheet

---

## HFC Shiny Dashboard

**File**: `HFC_24/app.R`

**Features**:
- Real-time daily dashboard (submissions, completion rates)
- Enumerator performance tracking
- Data quality monitoring
- Geographic coverage maps
- Automatic quality alerts
- Data export capabilities

**Running**:
```r
shiny::runApp("HFC/HFC_24/app.R")
```

---

## Data Quality Targets

- **Consent Rate**: > 95%
- **Completion Rate**: > 90%
- **Records with Errors**: < 5%
- **Backcheck Accuracy**: > 90%

---

## Required Packages

```r
tidyverse, lubridate, googlesheets4, googledrive, 
writexl, shiny, sf, ggplot2
```

---

## Running Scripts

```r
# Run entire pipeline
source("HFC/scripts/1.hfc_main.R")

# Or run individual scripts
source("HFC/scripts/2.hfc_construct.R")
source("HFC/scripts/3.hfc_admin.R")
source("HFC/scripts/4.hfc_outliers.R")
source("HFC/scripts/5.hfc_logic.R")
source("HFC/scripts/6.hfc_enum.R")
source("HFC/scripts/7.hfc_stats.R")
source("HFC/scripts/8.hfc_bc.R")
```

---

## Output Files Summary

| Output | Script | Content |
|--------|--------|---------|
| `hfc_constr_*.xlsx` | 2 | Cleaned survey data |
| `hfc_admin_summary.xlsx` | 3 | Daily/weekly metrics |
| `hfc_outliers_flagged.xlsx` | 4 | Outlier cases |
| `hfc_logic_checks.xlsx` | 5 | Logic errors |
| `hfc_enum_performance.xlsx` | 6 | Enumerator scorecards |
| `hfc_descriptives_full.xlsx` | 7 | Complete statistics |
| `hfc_backcheck_sample.xlsx` | 8 | Re-interview sample |

---

## Survey Scope

- **Districts**: Rulindo, Karongi, Rutsiro, Rusizi (4)
- **Sectors**: 30 total
- **Villages**: 103+ villages
- **Markets**: 17
- **Target Households**: 2,500-3,000

---

## Notes

- Google authentication required (confirm with `1` input when prompted)
- Raw data is encrypted; remember to re-encrypt after updates
- Outputs updated with each run (timestamped versions)
- Backcheck samples persist for field implementation
- HFC data integrates with main RCT pipeline for baseline analysis
