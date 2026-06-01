# RCT Baseline Data Processing - README

## Project Overview

This directory contains all baseline data processing scripts and outputs for the **Rwanda Energy Access and Quality Improvement Project (R-EAQIP)** RCT. The project involves a stratified randomized controlled trial across four districts in Rwanda (Rulindo, Karongi, Rutsiro, and Rusizi) with households assigned to treatment and control arms.

---

## Directory Structure

```
baseline/
├── scripts/                          # Main numbered processing scripts (1-6)
│   ├── 1.hh_head_final.R
│   ├── 2.scope_clean.R
│   ├── 3.randomization.R
│   ├── 4.output for stakeholders.R
│   ├── 5. Replacement.R
│   ├── 6. Number approached for EDCl.R
│   └── [other supporting scripts]
├── data/                            # Raw and processed data files
├── HFC/                             # High-frequency check data
├── Screening/                       # Screening survey data
├── HFC second/                      # Second round HFC data
└── outputs/                         # Final processed outputs
```

---

## Core Numbered Scripts (1-6)

### Script 1: `1.hh_head_final.R`

**Purpose**: Extract, clean, and consolidate household head information from all four districts

**Inputs**:
- `four_district_2405.xlsx` - Master household data
- District-specific HH head files:
  - `KARONGI HH Heads_062024.xlsx`
  - `RULINDO_final.xlsx`
  - `RUTSIRO HH_2024.xlsx`
  - `Rusizi data June 27,2024.xlsx`
- `Village.shp` - Rwanda village shapefile

**Key Functions**:
1. Loads household head lists for each district
2. Standardizes text formatting (title case)
3. Joins with geographic data (District, Sector, Cell, Village)
4. Fuzzy matching for unmatched villages (stringdist, max_dist=2)
5. Consolidates across four districts
6. Creates unique household identifiers (HHIDs) manually constructed for the baseline experiment

**Important Note on Household IDs (HHID)**:
- **HHIDs are constructed manually for the baseline experiment** rather than sourced from government data
- This allows for consistent tracking across the RCT even when households are not in official registries
- **Link between NID (National ID) and HHID**: Found in the output household datasets and maintained through all downstream scripts
- The NID-to-HHID mapping is critical for customer verification and deployment tracking in the intervention phase

**Outputs**:
- Cleaned, consolidated household head dataset
- Geographic identifiers (village_id, sector, cell, district)
- **NID-to-HHID mapping** (maintained throughout baseline data processing)

---

### Script 2: `2.scope_clean.R`

**Purpose**: Clean and finalize scope of villages and households in the RCT

**Inputs**:
- `four_district_2408.xlsx` - Full district dataset
- `List of Villages_in_EPC Rulindo_Updated.xlsx` - Rulindo scope
- `Final Village List Rusizi with customers Lot 1 & 2.xlsx` - Rusizi scope

**Key Functions**:
1. Creates scope flags for villages based on implementation status
2. Handles Rusizi lot assignments (Lot-1 and Lot-2)
3. Updates Rulindo scope based on EPC service area
4. Validates village codes against shapefile
5. Standardizes scope indicators

**Outputs**:
- Updated `four_district_[date].xlsx` with clean scope variables

---

### Script 3: `3.randomization.R`

**Purpose**: Execute stratified randomization across treatment and control arms

**Inputs**:
- `four_district_2408.xlsx` - Scoped villages dataset
- Stratification: District, Lot (for Rusizi)

**Randomization Design**:
- **Treatment Arms** (4):
  - C: Control
  - T1: Readyboard only
  - T2: Solar Home System (SHS) only
  - T3: Readyboard + SHS (combined)
- **Total Villages**: 193 (after removing 15kV grid-connected)
- **Strategy**: Block randomization within district × lot

**Key Functions**:
1. Filters to scoped villages only
2. Groups by stratification blocks
3. Uses randomizr for block randomization
4. Assigns equal numbers to each arm
5. Creates treatment indicators and labels

**Outputs**:
- `scope_193_0807.xlsx` - Randomized villages with treatment assignments
- Randomization verification tables

---

### Script 4: `4.output for stakeholders.R`

**Purpose**: Create stakeholder-ready outputs and deployment files

**Inputs**:
- `scope_193_0807.xlsx` - Randomized villages
- Deployment and household lists

**Key Functions**:
1. Prepares CTO survey deployment files
2. Creates village-level tracking sheets
3. Generates stakeholder communications with:
   - Village lists by treatment arm
   - Contact information
   - Geographic identifiers
4. Formats outputs for field teams
5. Produces summary statistics

**Outputs**:
- Stakeholder-ready village lists
- CTO deployment configurations
- Geographic and contact sheets
- Summary tables by district/arm

---

### Script 5: `5. Replacement.R`

**Purpose**: Handle household-level replacement and tracking for incomplete surveys

**Inputs**:
- `household_join_1111.xlsx` - Initial household assignments
- `household_head_clean.xlsx` - Cleaned household heads
- `scope_193_0807.xlsx` - Randomized villages
- `household_select_0807.xlsx` - Primary selected households
- `household_backup_0807.xlsx` - Backup households
- CTO survey submission data (`household_head.csv`)

**Key Functions**:
1. Loads primary and backup household lists by lot
2. Matches CTO submissions with selected households
3. Tracks survey completion status
4. Implements replacement logic:
   - Primary not surveyed → use backup
   - Multiple replacement rounds
   - Tracks reasons and dates

**Replacement Logic**:
- Primary and backup paired per village
- Multiple replacement rounds as needed
- Full tracking from selection through survey

**Outputs**:
- `household_join_[date].xlsx` - Final roster with survey status
- Replacement tracking tables
- Survey completion flags by lot/district

---

### Script 6: `6. Number approached for EDCl.R`

**Purpose**: Track and report household outreach and survey status for EDCL deployment

**Inputs**:
- Lot-specific household lists from Survey Firm:
  - `Lot_Karongi.xlsx`
  - `Lot_Rutsiro.xlsx`
  - `Lot_Rulindo.xlsx`
  - `Lot_Rusizi-1.xlsx`
  - `Lot_Rusizi-2.xlsx`

**Key Metrics**:
1. Households in sample per lot
2. Households approached
3. Households surveyed
4. Completion rates by district/lot
5. Replacement rates

**Key Functions**:
1. Loads primary + replacement sheets per lot
2. Combines into unified list
3. Aggregates across lots
4. Calculates completion statistics

**Outputs**:
- Consolidated household tracking file
- Summary statistics by lot/district
- Survey completion rates

---

## Primary Datasets Used

### Master Data Files

| File | Location | Contents |
|------|----------|----------|
| `four_district_2408.xlsx` | `data/` | Master household file for 4 districts |
| `household_head_clean.xlsx` | `data/data/` | Cleaned household head roster |
| `Village.shp` | `data/rwa_villages/` | Rwanda administrative boundaries |

### Scope & Randomization

| File | Location | Purpose |
|------|----------|---------|
| `scope_193_0807.xlsx` | `data/Updated scope villages& households/` | Final randomized 193 villages |
| `household_select_0807.xlsx` | `data/Updated scope villages& households/` | Primary selected households |
| `household_backup_0807.xlsx` | `data/Updated scope villages& households/` | Backup replacement households |

### Screening & Survey Data

| File | Location | Type |
|------|----------|------|
| `REP_screening_WIDE.csv` | `Screening/data/` | Raw screening survey |
| `Household and village list for deployment.xlsx` | `Screening/data/` | Deployment roster |

---

## Supporting Scripts (Non-Numbered)

### Screening Folder (`Screening/`)

**0.main.R** - Master orchestration script

**1.screening_constr.R** - Data construction
- Loads screening responses
- Cleans dates/metadata
- Matches with deployment list
- Creates completion flags

**2.screening_admin.R** - Administrative checks
- Data quality metrics
- Enumerator performance
- Geographic coverage

**3.screening_enum.R** - Enumerator analysis
- Workload/productivity
- Data quality by enumerator
- Timing analysis

**4.backcheck.R** - Quality assurance
- Backcheck validation
- Consistency checks
- Quality flags

### Other Scripts

| Script | Purpose |
|--------|---------|
| `cleancooking.R` | Clean cooking adoption analysis |
| `enumerator.R` | Enumerator tracking |
| `offgrid.R` | Off-grid access analysis |
| `power calculations.R` | Statistical power calculations |
| `scope_constrution.R` | Detailed scope documentation |

---

## Key Statistics & Design

### Geographic Scope
- **Districts**: Rulindo, Karongi, Rutsiro, Rusizi
- **Villages (Initial)**: ~200
- **Villages (Final)**: 193 (after removing 15kV grid)
- **Target Households**: 2,500-3,000

### Treatment Allocation
- **Control (C)**: ~25% of villages
- **T1 (Readyboard)**: ~25% of villages
- **T2 (SHS)**: ~25% of villages
- **T3 (Combined)**: ~25% of villages

### Implementation Lots (Rusizi)
- **Lot 1**: Western area
- **Lot 2**: Eastern area

---

## Data Processing Workflow

```
RAW DATA
├── HH Head Lists (4 districts)
├── Village Shapefile
├── Scope Lists
├── Deployment Lists
└── CTO Survey Data

    ↓ SCRIPT 1

HH HEAD IDENTIFICATION & CONSOLIDATION
(4 district sources → unified clean roster)

    ↓ SCRIPT 2

SCOPE FINALIZATION
(Apply filters, handle Rusizi lots, update scope)

    ↓ SCRIPT 3

STRATIFIED RANDOMIZATION
(Block randomize by district/lot → 4 treatment arms)

    ↓ SCRIPT 4

STAKEHOLDER OUTPUTS
(Format for deployment, CTO configs)

    ↓ SCRIPT 5

HOUSEHOLD REPLACEMENT & TRACKING
(Match submissions, implement replacement, track completion)

    ↓ SCRIPT 6

EDCL DEPLOYMENT TRACKING
(Aggregate across lots, completion statistics)

    ↓

SCREENING (Parallel)
├── Data construction
├── Admin checks
├── Enumerator analysis
└── Quality assurance
```

---

## Running the Scripts

### Prerequisites
- R 4.0+
- Key packages (auto-loaded):
  - `tidyverse`, `dplyr`, `readxl`, `writexl`, `janitor`
  - `sf`, `randomizr`, `fuzzyjoin`, `googlesheets4`

### Execution Order
```r
source("scripts/1.hh_head_final.R")
source("scripts/2.scope_clean.R")
source("scripts/3.randomization.R")
source("scripts/4.output for stakeholders.R")
source("scripts/5. Replacement.R")
source("scripts/6. Number approached for EDCl.R")
source("Screening/0.main.R")  # Can run in parallel
```

### Important Notes
- All scripts use hardcoded Dropbox paths
- Modify `DROPBOX` variable if needed:
  ```r
  DROPBOX <- "C:/Users/[USERNAME]/Dropbox"
  ```
- Missing codes: -77 (don't know), -88 (refused), -99 (other)
- Fuzzy matching: max distance = 2
- Results clustered at village/district level

