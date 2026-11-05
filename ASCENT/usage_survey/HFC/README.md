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




## Decision log

Decisions that change what the HFC measures. Newest first. Each entry says what changed, why, and
where it lives in the code, so a reviewer can check it without rereading the scripts.

### 2026-11-05 — HFC updated to the 2026-11-04 form revision (Yeji)

**Context.** The active form (`clean_cooking_questionnaire_261104.xlsx`, saved 4:04 PM) restructured
the survey; the scripts had been written for the earlier version now in `questionnaires/archive/`.
Fields were dropped on purpose (confirmed by Yeji). Changes in the form:

| Area | Before (archived form) | Now (active form) |
|---|---|---|
| Roster | `hh_member_count`, `hh_child_count`, `tu_*` roster | `A1_1`, repeat `hh_roster` (`rel_to_hhh`, `member_gender`, `member_dob`, `member_age`), `respondent_selection` |
| Time use | inside the `tu_*` roster | `members_cooking` -> `cooking_time_loop`; `members_firewood` -> `firewood_time_loop` |
| Fuels | household `energy_sources` + fuel loop | per stove: `stove_fuels` -> nested `stove_fuel_loop` (WIDE: `<field>_<stove>_<fuel>`) |
| Removed | `started_program_stove`, `months_regular_use`, `days_week_typical`, `seasonal_change`, `current_season`, `primary_stove`, `stove_role`, `used_yesterday`, `events_yesterday`, `cook_where`, `nolocate`, `no_consent_reason`, `audio`, `finish`, `comment` | |

**Definitions changed** (all in `scripts/2.hfc_construct.R`, used by 3, 6, 9, 10):

| Definition | Before | Now | Why |
|---|---|---|---|
| Complete | located + consented + `finish = 1` | located + consented + `observe_stoves` answered | No `finish` field; `observe_stoves` is the last required question, so a form saved part-way does not count. |
| Operational days per year (`op_days_year`) | `months_regular_use` x `days_week_typical` x 52/12 | program stove `days_used_7d` x 365/7 | Seasonality questions removed. **Caveat:** a 7-day snapshot, annualised; it does not capture seasonal non-use. |
| Primary stove (`primary_stove` in usage params) | `stove_role = 1` for the program stove | program stove has the highest `cooking_share` among stoves used in the last 7 days (ties count) | `stove_role` removed. |
| `started_use` (usage param) | `started_program_stove = 1` | dropped | Field removed; `used_7d` remains. |

**Logic checks** (`scripts/5.hfc_logic.R`):

- Dropped (fields removed): `multiple_primary`, `primary_stove_not_used`, `no_regular_months_but_used`,
  `typical_vs_7d_days`, `program_stove_never_used`, `children_mismatch`.
- Rewired to new fields: `heads_not_one` (`rel_to_hhh`), `nobody_cooks` (stove used but `members_cooking`
  empty; now complete surveys only), `cook_hours_high` (`cooking_hours_7d`), `firewood_not_source` (firewood
  collected but no stove lists firewood in `stove_fuels`), `program_stove_not_used_30d` (program stove not in
  `stoves_used`; no longer conditional on `started_program_stove`).
- New:

| Check | Kind | Rule |
|---|---|---|
| `events_below_days` | inconsistent | a stove's `cooking_events_7d` < `days_used_7d` |
| `stove_fuel_mismatch` | inconsistent | LPG / PAYG LPG stove used without gas, or EPC used without electricity, in its `stove_fuels` |
| `fuel_own_above_qty` | inconsistent | `fuel_own_7d` > `fuel_qty_7d` |
| `used_but_not_working` | verify | `days_used_7d` > 0 but `stove_working = 0` |
| `child_cook` | verify | a cooking member younger than `CHILD_COOK_AGE` (10, TBD from pilot; `1.hfc_main.R`) |

**Other changes.**

- Outliers (`4.hfc_outliers.R`): `A1_1`, `cooking_events_7d`, `cooking_hours_7d`, `firewood_hours_7d`; fuel
  quantity and spending as before (within fuel / unit). `events_yesterday` dropped.
- Not-located list (`3.hfc_admin.R`): no reason column (`nolocate` removed from the form).
- Privacy (`hfc_privacy.R`, `0.hfc_deidentify.R`): member names and `member_dob` are blocked; only
  `member_age` reaches the HFC. Repeat columns are kept at their exact nesting depth.
- Back-check priority (`11.hfc_backcheck.R`): `program_stove_not_used_30d` replaces `program_stove_never_used`
  at priority 1; `used_but_not_working` 2; `child_cook` replaces `typical_vs_7d_days` at 5.
- Synthetic data (`test/0.generate_synthetic_data.R`) rebuilt for the new structure, one planted household per
  check. `run_test.R` passes: 34/34 expected results matched.

**Open items.**

- The form's `settings` version is `261104_activity_first_timeuse`, not a number. SurveyCTO needs a numeric
  version; the HFC now warns instead of stopping. Fix in the form before deploying.
- Confirm on the first pilot export that nested repeats are named `<field>_<stove>_<fuel>` and that SurveyCTO's
  0/1 columns per `select_multiple` choice are ignored (the parser reads repeat fields at their exact depth).
- `CHILD_COOK_AGE`, `DURATION_MIN`, `DURATION_MAX`: set from the pilot.

### 2026-11-05 — Sample and form preload (Yeji)

- **Sample** (`ASCENT/usage_survey/preload/2.sampling.R`): 1,000 main + 300 replacement households, 4 strata
  (stove type LPG / EPC x year the stove was provided, 2025 / 2026), equal allocation (250 + 75 per stratum).
  Frame: beneficiaries with a household ID, LPG or EPC (Umunota induction cooker excluded), one row per phone.
  Within strata: systematic draw after sorting by district, cell, village. Seed 20261105.
- **Design weight** = stratum frame size / 250 (frame households one sampled household stands for). Only for
  estimates that pool strata; results by stratum need no weight. Recompute on completed interviews.
- **Form preload** is now `usage_survey_preload.csv` (written by `2.sampling.R`: `householdid_key`, `stove_type`
  code 1 LPG / 3 EPC, `stove_year`), replacing `beneficiary-list-intermediate.csv`. `hfc_config.R` and the
  synthetic data use the new name. **Form edit pending:** `pull_stovetype` (survey sheet row 36, column K)
  `pulldata('beneficiary-list-intermediate', ...)` -> `pulldata('usage_survey_preload', ...)`.
- Replacement rule (when a household counts as unreachable): to set with the survey firm before calls start.
