# Midline pre-survey

Scripts that build the midline sample and SurveyCTO preloads: household preload, 180 sample villages, EDCL grid-customer eligibility.
Run in this order: `1`, `1b`, `2`, `2b`, (manual review), `2c`. Run from the repo root; paths come from `PATHS.R`.
Scripts are in `scripts/`; `explore/` holds one-off checks.

All data paths start at the shared Dropbox folder, `Dropbox/Rwanda Energy/EAQIP/`.

| Short name | Folder |
|---|---|
| baseline analysis data | `Dropbox/Rwanda Energy/EAQIP/datawork/baseline analysis/data/` |
| baseline analysis output | `Dropbox/Rwanda Energy/EAQIP/datawork/baseline analysis/output/` |
| baseline CTO data | `Dropbox/Rwanda Energy/EAQIP/datawork/HFC/data/baseline-second-round/` |
| midline CTO data | `Dropbox/Rwanda Energy/EAQIP/datawork/HFC/data/midline/` |
| national villages | `Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/data/` |
| village scope | `Dropbox/Rwanda Energy/EAQIP/questionnaires/02-midline/00-village-scope/` |
| SHS eligibility | `Dropbox/Rwanda Energy/EAQIP/questionnaires/02-midline/01-SHS-eligibility/` (`raw/`, `intermediate/`, `final/`) |
| CTO preload | `Dropbox/Rwanda Energy/EAQIP/questionnaires/02-midline/02-CTO-preload/` |

## 1-hh-preloads.R

- **Input**
  - `Dropbox/Rwanda Energy/EAQIP/datawork/HFC/data/midline/0-midline-vulnerable-household-sample.xlsx`
  - `Dropbox/Rwanda Energy/EAQIP/datawork/baseline analysis/data/baseline_1973_clean.xlsx`
  - `Dropbox/Rwanda Energy/EAQIP/datawork/HFC/data/baseline-second-round/scope_193_0807.xlsx`
- **Function:** Cleans baseline phone numbers (10-digit 07XXXXXXXX), attaches the village treatment arm to the 1,973 baseline households, and checks that treatment is constant within village and that household IDs encode their village. Builds the household head list and the SHS-eligible list (T2 and T3, 906 households).
- **Output** (all in `Dropbox/Rwanda Energy/EAQIP/questionnaires/02-midline/02-CTO-preload/`)
  - `contact_info.csv`: all 1,973 households with clean phones and treatment
  - `household_head.csv`: names, NID and admin names
  - `shs_eligible.csv`: eligible households only

## 1b-hh-treatment-count.R

- **Input:** `Dropbox/Rwanda Energy/EAQIP/questionnaires/02-midline/02-CTO-preload/contact_info.csv`
- **Function:** Counts villages and households per district and treatment arm (should total 1,973 households).
- **Output:** `Dropbox/Rwanda Energy/EAQIP/datawork/HFC/data/midline/treatment_village_household_count.xlsx`

## 2-village-preload.R

- **Input**
  - `Dropbox/Rwanda Energy/EAQIP/datawork/baseline analysis/data/baseline_1973_clean.xlsx`
  - `Dropbox/Rwanda Energy/EAQIP/datawork/HFC/data/midline/0-midline-vulnerable-household-sample.xlsx`
  - `Dropbox/Rwanda Energy/EAQIP/datawork/baseline analysis/output/Surveyed 181 villages.xlsx`
  - `Dropbox/Rwanda Energy/EAQIP/datawork/HFC/data/baseline-second-round/scope_193_0807.xlsx`
  - `Dropbox/Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/data/0.Rwanda-Villages-Masterfile.xlsx`
- **Function:** Extracts the 180 midline villages (the 181-village list minus Rwamigega, which has no baseline household) and checks their treatment arms. Checks that admin IDs are unique in the national master list. Attaches admin codes to the 180 villages and prints the 13 dropped villages, the arm retention table and villages with two names (not saved).
- **Output**
  - `Dropbox/Rwanda Energy/EAQIP/questionnaires/02-midline/00-village-scope/villages-4-districts-masterfile.xlsx`: master list for the four districts
  - `Dropbox/Rwanda Energy/EAQIP/questionnaires/02-midline/00-village-scope/villages-180-coded.xlsx`: 180 villages with admin codes
  - `Dropbox/Rwanda Energy/EAQIP/questionnaires/02-midline/02-CTO-preload/scope_villages.csv`: village preload

## 2b-villages-fuzzy-matching.R

- **Input**
  - `Dropbox/Rwanda Energy/EAQIP/questionnaires/02-midline/00-village-scope/villages-4-districts-masterfile.xlsx`
  - `Dropbox/Rwanda Energy/EAQIP/questionnaires/02-midline/00-village-scope/villages-180-coded.xlsx`
  - `Dropbox/Rwanda Energy/EAQIP/questionnaires/02-midline/01-SHS-eligibility/raw/260827-grid-customer-list.xlsx` (one sheet per district)
- **Function:** Cleans EDCL village names and exact-matches them to the master list. For each unmatched village it lists the master-list villages in the same cell, so the right name can be picked by hand.
- **Output** (all in `Dropbox/Rwanda Energy/EAQIP/questionnaires/02-midline/01-SHS-eligibility/intermediate/`)
  - `grid-customer-list-harmonized.xlsx`: cleaned household rows
  - `edcl-master-matched-villages.xlsx`: villages that matched exactly
  - `manual-village-fuzzy-matching-raw.xlsx`: unmatched villages with candidate names

**Manual step:** copy `manual-village-fuzzy-matching-raw.xlsx` to `manual-village-fuzzy-matching-review.xlsx` (same folder) and fill `VILLAGE_CLEAN` for each unmatched village. This file is not written by any script.

## 2c-EDCL-180-sample.R

- **Input**
  - `Dropbox/Rwanda Energy/EAQIP/questionnaires/02-midline/00-village-scope/villages-4-districts-masterfile.xlsx`
  - `Dropbox/Rwanda Energy/EAQIP/questionnaires/02-midline/00-village-scope/villages-180-coded.xlsx`
  - `grid-customer-list-harmonized.xlsx`, `edcl-master-matched-villages.xlsx`, `manual-village-fuzzy-matching-review.xlsx` in `Dropbox/Rwanda Energy/EAQIP/questionnaires/02-midline/01-SHS-eligibility/intermediate/`
- **Function:** Adds admin codes to the reviewed villages, stacks them with the matched ones, keeps the villages inside the 180 sample and attaches the EDCL customer households in those villages.
- **Output** (all in `Dropbox/Rwanda Energy/EAQIP/questionnaires/02-midline/01-SHS-eligibility/final/`)
  - `edcl-sample-villages.xlsx`
  - `edcl-sample-households.xlsx`

## Used outside this folder

`contact_info.csv`, `shs_eligible.csv`, `household_head.csv` and `scope_villages.csv` are read by the midline HFC scripts (`RCT_data/midline/HFC/scripts`) and pulled by the SurveyCTO form by file name. Keep their names and columns unchanged.

## explore/

One-off checks that write no files (for example `260825-sample-villages-discrepency-inspection.R`, which traces why the baseline analysis shows 180 villages instead of 181).
