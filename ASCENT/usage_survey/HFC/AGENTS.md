# Clean cooking usage survey HFC: agent instructions

High-frequency checks for the **ASCENT clean cooking usage survey** (Ci-Dev): about 1,000
beneficiary households with LPG, PAYG smart LPG or electric pressure cooker (EPC) stoves. Same
structure and conventions as the EAQIP midline HFC (`RCT_data/midline/HFC/AGENTS.md`): read that
file for the shared rules on privacy, paths, coding style and testing.

Author: Tanay (using Claude) ?? Last modified: 2026-11-05 (Yeji: updated to the 2026-11-04 form revision)

## Rules (in addition to the root CLAUDE.md)

- Code only in this repo. Data, outputs, figures and tables live in the shared OneDrive folder
  `ASCENT_clean_cooking` (`SHARED_ASCENT_CC` in `PATHS.R`). Never write data into the repo.
- All paths and the run mode come from `scripts/hfc_config.R`. No script builds its own paths.
- Logic flags are soft. "inconsistent" flags count toward the <5% target; "verify" flags are
  plausible answers (e.g. program stove never used) that feed the back-check list. Do not turn
  them into form constraints or tighten them without asking.
- Do not change a parameter definition in `9.hfc_usage.R` without asking: the report uses the
  same definitions.

## Run

From the repo root (Tanay: `C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe`; Yeji: see root CLAUDE.md):

```
Rscript ASCENT/usage_survey/HFC/scripts/0.hfc_deidentify.R   # raw export + preload -> deidentified
Rscript ASCENT/usage_survey/HFC/scripts/1.hfc_main.R         # checks, dashboard, usage preview
Rscript ASCENT/usage_survey/HFC/scripts/11.hfc_backcheck.R   # call list (identified; core team only)
Rscript ASCENT/usage_survey/HFC/test/run_test.R              # end-to-end test on synthetic data
```

Mode: `HFC_MODE_DEFAULT` in `hfc_config.R` (`test`, `pilot`, `live`). Before going live, set
`fieldwork_start` for the mode and confirm the TBDs below.

## Folders (OneDrive `ASCENT_clean_cooking/`)

| Path | Contents | Shared with |
|---|---|---|
| `questionnaires/clean_cooking_questionnaire_YYMMDD.xlsx` | The one active form (older ones in `archive/`) | core team |
| `datawork/HFC/data/` | Raw SurveyCTO export `LPG_usage_survey_WIDE_YYMMDD.csv` (one active) | core team |
| `datawork/HFC/preload/` | Raw preload: `test_enumerators.csv`, `scope_villages.csv`, `household_head.csv`, `stove_type.csv` (from `preload/3.preload.R`) | core team |
| `datawork/HFC/deidentified/` | Output of script 0; the only input of scripts 2-10 | core team |
| `datawork/HFC/output/` | `hfc_constr.xlsx`, dashboard, `usage_params.csv`, `checks/` (flags, back-check lists) | core team |
| `share/main/` | Current dashboard (deidentified) | survey firm |
| `datawork/HFC/test/`, `pilot/`, `share/test/` | Same layout for test and pilot | core team |
| `report/` | Overleaf report skeleton; exhibits written by the analysis scripts | core team |

## Strata and key definitions

- Stratum = preload stove type (`stove_type.csv`, `stove_type_code`): 1 LPG, 3 EPC (2 PAYG LPG is not
  separated in the sample). The sample is also stratified by `stove_year` (2025, 2026); see
  `ASCENT/usage_survey/preload/3.preload.R`.
- Complete = located, consented and `observe_stoves` answered (the form has no `finish` field; `observe_stoves`
  is the last required question). Duplicate = more than one consented submission per `hh_id`.
- Program stove answers (`prog_*`) come from the stove-loop instance whose `stove_type = program_stove_type`.
- Operational days per year = program stove `days_used_7d` x 365/7 (0 if not used; no seasonality in the form).
- Primary stove = the program stove has the highest `cooking_share` among stoves used in the last 7 days (ties count).
- Decisions behind these definitions, and the check list: `README.md`, "Decision log".

## Form structure (2026-11-04 revision)

- Roster: `A1_1` members, repeat `hh_roster` (`rel_to_hhh`, `member_gender`, `member_dob`, `member_age`; names and
  DOB are dropped by `0.hfc_deidentify.R`).
- Stoves: repeat `stove_loop` (one instance per `stoves_used`), with the **nested** repeat `stove_fuel_loop`
  (one instance per `stove_fuels`). WIDE columns: `<field>_<stove>` and `<field>_<stove>_<fuel>`.
- Time use: `members_cooking` -> repeat `cooking_time_loop` (`cooking_hours_7d`); `members_firewood` -> repeat
  `firewood_time_loop` (`firewood_hours_7d`).
- Repeat columns are read at their exact depth (`rep_cols()` / `rep_long()` in `2.hfc_construct.R`), so
  SurveyCTO's 0/1 columns per `select_multiple` choice are ignored. Confirm the naming on the first pilot export.

## TBD before live

- `fieldwork_start` for pilot and live (`hfc_config.R`); `DURATION_MIN` / `DURATION_MAX` / `CHILD_COOK_AGE` from the pilot (`1.hfc_main.R`).
- Form `settings` version must be numeric (currently `261104_activity_first_timeuse`; the HFC warns).
- Production enumerator list name (the form searches `test_enumerators`).
- Contact column names in `household_head.csv` (`CONTACT_COLS` in `11.hfc_backcheck.R`).
- Form `pull_stovetype` (survey sheet, row 36, column K) must read `stove_type` instead of
  `beneficiary-list-intermediate`, and `stove_type.csv` must be attached to the form in SurveyCTO.
- Where the shared folder appears on Yeji's machine (`SHARED_ASCENT_CC` in `PATHS.R`).



