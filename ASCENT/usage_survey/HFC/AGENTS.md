# Clean cooking usage survey HFC: agent instructions

High-frequency checks for the **ASCENT clean cooking usage survey** (Ci-Dev): about 1,000
beneficiary households with LPG, PAYG smart LPG or electric pressure cooker (EPC) stoves. Same
structure and conventions as the EAQIP midline HFC (`RCT_data/midline/HFC/AGENTS.md`): read that
file for the shared rules on privacy, paths, coding style and testing.

Author: Tanay (using Claude) · Last modified: 2026-11-04

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
| `datawork/HFC/preload/` | Raw preload: `test_enumerators.csv`, `scope_villages.csv`, `household_head.csv`, `beneficiary-list-intermediate.csv` | core team |
| `datawork/HFC/deidentified/` | Output of script 0; the only input of scripts 2-10 | core team |
| `datawork/HFC/output/` | `hfc_constr.xlsx`, dashboard, `usage_params.csv`, `checks/` (flags, back-check lists) | core team |
| `share/main/` | Current dashboard (deidentified) | survey firm |
| `datawork/HFC/test/`, `pilot/`, `share/test/` | Same layout for test and pilot | core team |
| `report/` | Overleaf report skeleton; exhibits written by the analysis scripts | core team |

## Strata and key definitions

- Stratum = preload stove type (`beneficiary-list-intermediate.csv`, `stove_type`): 1 LPG, 2 PAYG LPG, 3 EPC.
- Complete = located, consented and `finish = 1`. Duplicate = more than one consented submission per `hh_id`.
- Program stove answers (`prog_*`) come from the stove-loop instance whose `stove_type = program_stove_type`.
- Operational days per year = `months_regular_use` x `days_week_typical` x 52/12 (0 if never used).

## TBD before live

- `fieldwork_start` for pilot and live (`hfc_config.R`); `DURATION_MIN` / `DURATION_MAX` from the pilot (`1.hfc_main.R`).
- Production enumerator list name (the form searches `test_enumerators`).
- Contact column names in `household_head.csv` (`CONTACT_COLS` in `11.hfc_backcheck.R`).
- `stove_type` in the beneficiary list must hold codes 1/2/3, not labels (the form converts codes to labels).
- Where the shared folder appears on Yeji's machine (`SHARED_ASCENT_CC` in `PATHS.R`).
