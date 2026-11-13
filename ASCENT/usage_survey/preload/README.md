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

After cleaning, `beneficiaries-list-intermediate.csv` has the following distribution:

| Purchase period | LPG | LPG PAYG | EPC |
|---|---:|---:|---:|
| 2025 Q1-Q2 | 5,212 | 0 | 0 |
| 2025 Q3-Q4 | 18,260 | 1,175 | 2,851 |
| 2026 Q1-Q2 | 16,559 | 1,558 | 8,688 |
| 2026 Q3-Q4 | 1,519 | 0 | 331 |
| **Total** | **41,550** | **2,733** | **11,870** |

### 2.sampling.R
The sampling frame is grouped into seven nonempty stove-type × purchase-period groups. The 2026 quarters are pooled for sampling. Allocation is proportional to group size multiplied by its relative weight:

| Purchase period | Stove type | Frame n | Relative weight | Main target (n=1,000) | Replacement target (n=300) |
|---|---|---:|---:|---:|---:|
| 2025 Q1-Q2 | LPG | 5,212 | 3x | 171 | 51 |
| 2025 Q3-Q4 | LPG | 18,260 | 1x | 199 | 60 |
| 2025 Q3-Q4 | LPG PAYG | 1,175 | 8x | 102 | 31 |
| 2025 Q3-Q4 | EPC | 2,851 | 1.5x | 47 | 14 |
| 2026 | LPG | 18,078 | 1x | 197 | 59 |
| 2026 | LPG PAYG | 1,558 | 8x | 136 | 41 |
| 2026 | EPC | 9,019 | 1.5x | 148 | 44 |
| **Total** |  | **56,153** |  | **1,000** | **300** |

These are expected allocations from the undeduplicated counts above. Actual counts can differ because the sampler first keeps one beneficiary per phone.

- Pilot and pilot replacement sizes are `N_PILOT = 200` and `N_PILOT_REPLACE = 60`. They are drawn from the pool left after the main and replacement samples, without overlap.
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
