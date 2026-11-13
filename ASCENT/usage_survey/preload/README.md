# ASCENT Usage Survey Preload data

## Overview
This folder builds the identified preload attachments for the clean cooking usage survey.

## Raw beneficiary list location
`DATA_ASCENT_PRELOAD/clean-cooking-beneficiaries-list-raw.csv`

## Scripts

### 1.beneficiary_list.R
- Cleans the beneficiary list.
- Matches district/cell/village names to official admin codes.
- Excludes ambiguous location matches.
- Creates new `householdid_key` values and keeps original `household_id` for traceability.
- Writes `beneficiaries-list-intermediate.csv`.

See `beneficiary_cleaning_reasoning.rmd` for missingness checks and HHID decision rationale.

After cleaning, the `beneficiaries-list-intermediate.csv` has following distribution. 
          | LPG    | LPG PAYG   | EPC  
2025 Q1Q2 | 5212   | -          |
2025 Q3Q4 | 18260  | 1175       | 2851 
2026 Q1Q2 | 16559  | 1558       | 8688
2026 Q3Q4 | 1519   | -          | 331 
Total     | 41550  | 2739       | 11872  

### 2.sampling.R
- Based on the availability and sample size of the distribution (i.e. stove type crossed by purchase period), we construct seven stratum 

1) 25Q1Q2 LPG
2) - 4)  25Q3Q4 LPG, LPG PAYG, EPC
5) - 7)  26 LPG, LPG PAYG, EPC 

The sample size are: 
          | LPG    | LPG PAYG   | EPC  
2025 Q1Q2 | 5212   | -          |
2025 Q3Q4 | 18260  | 1175       | 2851 
2026      | 18078  | 1558       | 9019

Our desired weight across these strarum are: 
          | LPG    | LPG PAYG   | EPC  
2025 Q1Q2 | 3x     | -          |
2025 Q3Q4 | x      | 8x         | 1.5x
2026      | x      | 8x         | 1.5x

Therefore, for n = 1,000 our distribution should be:  
          | LPG    | LPG PAYG   | EPC  
2025 Q1Q2 | 171    | -          |
2025 Q3Q4 | 199    | 102        | 47 
2026      | 197    | 136        | 148

For replacement n = 300, we need mutually exclusive sample of: 
          | LPG    | LPG PAYG   | EPC  
2025 Q1Q2 | 51     | -          |
2025 Q3Q4 | 60     | 31         | 14
2026      | 59     | 41         | 44

- Builds seven nonempty sampling groups from stove type × purchase period; the 2026 quarters are pooled.
- Allocates each draw proportional to group size × the relative weight shown above, with largest-remainder rounding and capacity checks.
- Main and replacement sizes are `N_MAIN = 1,000` and `N_REPLACE = 300`. Replacements are drawn from the remaining frame and do not overlap the main sample.
- Pilot and pilot replacement sizes are `N_PILOT = 200` and `N_PILOT_REPLACE = 60`. They are drawn from the pool left after the main and replacement samples, without overlap.
- The listed group allocations are expected from the undeduplicated counts. Actual allocations can differ because the sampler first keeps one beneficiary per phone.
- Summary outputs include all nine stove-by-period cells, including zero-count cells.
- Writes:
  - `usage_survey_sample.csv`
  - `usage_survey_sample_summary.csv`
  - `usage_survey_pilot_sample.csv`
  - `usage_survey_pilot_sample_summary.csv`
  - `usage_survey_strata_distribution.csv` (actual distribution across all nine stove-by-period cells)

All five sampling outputs are written to both `DATA_ASCENT_PRELOAD/` (Dropbox) and `SHARED_ASCENT_CC/questionnaires/preload/` (OneDrive), so the copies mirror after each successful run.

### 3.preload.R
Creates the raw form preload files from sampled households:
- `test_enumerators.csv` (one row per sampled district): `districtid_key`, `district_key`, `enumeratorid_key`, `enumerator_key`
- `scope_villages.csv`: admin keys and names at all levels (`district*`, `sector*`, `cell*`, `village*`)
- `household_head.csv`: `householdid_key`, `household_id`, `villageid_key`, `village`, `hh_head_name`, `phone_clean`, `phone2_clean`
- `stove_type.csv` (`householdid_key`, `stove_type`, `stove_type_code`, `product_name`, `product_name_code`)

Mode handling:
- `PRELOAD_MODE=live` uses `usage_survey_sample.csv` (main + replacement)
- `PRELOAD_MODE=pilot` uses `usage_survey_pilot_sample.csv`

Output locations:
- Dropbox: `DATA_ASCENT_PRELOAD/main/` or `DATA_ASCENT_PRELOAD/pilot/`
- OneDrive: `SHARED_ASCENT_CC/questionnaires/preload/main/` or `.../pilot/`
- Live mode also writes a compatibility copy to `DATA_ASCENT_PRELOAD/`

## Notes
- Freeze the ID mapping before drawing and fielding.
- If the beneficiary list or name ordering changes, IDs can change and old sample/preload files become incompatible.
