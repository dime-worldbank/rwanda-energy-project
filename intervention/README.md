# Intervention Analysis

Implementation and tracking of the Rwanda Energy Access and Quality Improvement Project (R-EAQIP) intervention rollout, focusing on Readyboard electrification and off-grid solar deployment across treatment villages.

## Overview

This folder contains scripts for managing the intervention implementation, customer verification, and deployment tracking for the randomized controlled trial (RCT) component of R-EAQIP. The interventions include:

- **Readyboard**: Grid-connected smart metering technology (T1)
- **Solar Off-Grid**: Off-grid solar electrification systems (T2)
- **Combined**: Both Readyboard and Solar off-grid (T3)
- **Control**: No intervention (C)

## Datasets Used

### Primary Data Sources

- **Village Treatment Scope**: Updated list of villages randomly assigned to treatment arms with household sample sizes
- **Customer Lists**: EDCL (Electricity Utility) customer database and verified customer lists
- **Readyboard EPC Data**: 
  - Readyboard installation records by district (Karongi, Rutsiro, etc.)
  - Installation status and comments
  - National ID (NID) verification
- **GPS Coordinates**: Household GPS locations for matching with customer database
- **Deployment Records**: Installation confirmation and site verification data
- **Screening Survey**: Baseline household enumeration data
- **HFC Data**: Household Fuel Consumption data for matching and verification
- **Administrative Boundaries**: Village, cell, sector, and district shapefiles

**Important Note on Household IDs (HHID)**:
- **HHIDs are constructed manually for the baseline experiment** rather than sourced from government data
- This allows for consistent tracking across the RCT even when households are not in official registries
- **Link between NID (National ID) and HHID**: Created in the RCT baseline phase and maintained in:
  - Scope datasets: `scope_193_0807.xlsx` and related household files in `RCT_data/baseline/data/`
  - HFC datasets: `RCT_data/baseline/HFC/data/`
  - Used for all customer matching and deployment verification (see `2. name match customer.R` and `3. gps match customer.R`)

## Script Organization

### Treatment & Scope Definition (0.x Scripts)

- **0. Treatment and scope.R**: Main treatment assignment script. Loads village treatment status from randomization, assigns treatment names (Control/Readyboard/Solar/Combined), and prepares household scope with village identifiers across four target districts.

### Scope Verification (1.x Scripts)

- **1.5. scope verification.R**: Compares randomization scope with EDCL customer database. Verifies coverage of target villages, identifies discrepancies between planned scope and field reality, and flags villages to be dropped due to infrastructure constraints (e.g., 15kV limitations).

- **1.epc readyboard updated.R**: Updates Readyboard eligible customer list from EPC (Energy Provisioning Company) negotiation records. Prepares updated customer rosters for Karongi and Rutsiro districts with installation status tracking.

- **epc readyboard.R**: Original Readyboard EPC customer list (archived version).

### Customer Verification (2.x & 3.x Scripts)

- **2. distance to lv.R**: Calculates distance from each household to nearest Low Voltage (LV) line. Uses spatial analysis to determine feasibility of grid connection. Essential for stratification and determining grid vs. off-grid eligibility.

- **2. name match customer.R**: Matches household survey data with EDCL customer database using fuzzy name matching. Resolves name spelling variations and identifies already-connected customers.

- **3. gps match customer.R**: Matches households to EDCL customer database using GPS coordinates. Complements name matching with spatial verification. Identifies duplicate customers and resolves conflicts between multiple household records.

### Deployment Tracking (4.x & Verification Scripts)

- **4. Installed readyboard.R**: Tracks actual Readyboard installations by district. Processes district-specific installation records (Karongi, Rutsiro), flags installation status (Installed/Pending), and resolves duplicate entries using NID verification.

- **deployment final check.R**: Final deployment verification. Confirms all treatment assignments are matched to installation records, identifies missing installations, and validates deployment completeness before endline survey.

- **verify right list(deployment).R**: Validates that correct household list is being used for deployment. Cross-checks deployed households against treatment assignment and scope to prevent rollout errors.

### District-Specific Verification

- **rulindo verification.R**: Verification for Rulindo district (one of the four target districts). Contains district-specific cleaning and validation steps.

- **screening survey.R**: Baseline screening survey processing. Prepares household enumeration data for the baseline period for use in verification steps.

## Treatment Arms

The RCT uses a 2x2 design:

| Treatment | Description | Code |
|-----------|-------------|------|
| Control | No intervention | C |
| Readyboard | Grid-connected smart meter | T1 |
| Solar Off-Grid | Off-grid solar system | T2 |
| Readyboard + Solar | Both interventions | T3 |

## Key Execution Workflow

### Phase 1: Scope & Assignment
1. **0. Treatment and scope.R** - Load randomization and define treatment villages

### Phase 2: Verification
2. **1.5. scope verification.R** - Verify village inclusion in EDCL database
3. **1.epc readyboard updated.R** - Prepare Readyboard customer eligibility list

### Phase 3: Customer Matching
4. **2. distance to lv.R** - Calculate LV distance for grid feasibility
5. **2. name match customer.R** - Fuzzy match households to EDCL database
6. **3. gps match customer.R** - Verify matches using GPS coordinates

### Phase 4: Installation & Deployment
7. **4. Installed readyboard.R** - Track Readyboard installations by district
8. **deployment final check.R** - Validate deployment completeness
9. **verify right list(deployment).R** - Confirm correct household list is deployed

## Key Variables

- **village_id**: Unique village identifier
- **treat**: Treatment arm (C/T1/T2/T3)
- **nid**: National ID (for customer matching)
- **gps_lat/gps_lon**: Household GPS coordinates
- **distance_to_lv**: Distance to nearest Low Voltage line (meters)
- **customer_id**: EDCL customer database ID
- **installed**: Installation status (Installed/Pending/Not Eligible)
- **district**: Four target districts (Karongi, Rutsiro, Rulindo, Gicumbi)

## Notes

- Analysis uses spatial matching (GPS coordinates) and fuzzy text matching (names) for household-to-customer reconciliation
- Distance to LV line determines eligibility: grid connection (Readyboard) vs. off-grid (Solar)
- Installation tracking uses National ID verification to resolve duplicates
- Multiple verification steps ensure deployment fidelity to randomization assignment
- All path references use Dropbox sync folder structure

## Output

Scripts generate:
- Verified customer lists by treatment arm
- Installation tracking databases
- Deployment verification reports
- GPS-matched household-customer linkages
- Distance calculations for grid feasibility