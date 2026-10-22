# Midline HFC — agent instructions

High-frequency checks (HFC) for the **R-EAQIP midline survey**. HFC runs daily during
fieldwork on the cumulative SurveyCTO export and flags data-quality problems while
enumerators are still in the field. It is monitoring, not analysis cleaning.

Baseline HFC (reference only): `RCT_data/baseline/HFC/scripts`.
Author: Yeji (using Claude) · Last modified: 2026-10-21

---

## Current status

- Week of 2026-10-19: memory file done; synthetic test data done (2026-10-21); next is porting scripts 1–9, and any unnumbered scripts, decide if this is useful to track the daily accomplishments and HFC and asks .
- 2026-10-21: scripts 1–9 ported; test mode passes `test/compare_expected_flags.R` (79/79). Live-mode TBDs remain (see `1.hfc_main.R` config).

## Rules for the agent

- **Never edit anything in `RCT_data/baseline/`.** Copy from it; build in `RCT_data/midline/HFC/`.
- **No hard-coded user paths** (`C:/Users/...`). All paths come from `PATHS.R` objects (listed below).
- **Code lives in the repo; data lives in Dropbox or the WBG OneDrive.** Never write data files
  (real or synthetic) into the repo.
- **Identifiable data** (names, phone numbers, NID, GPS, photos, audio) may be stored only in Dropbox
  and the WBG OneDrive (approved). Raw exports and media stay in Dropbox; HFC outputs for the
  dashboard go to OneDrive. Share OneDrive folders with identifiable data only with the core team;
  pages for wider audiences show IDs and aggregates only.
  Synthetic data must use invented names and numbers.
- Do not change a variable definition (complete, eligible, etc.) without asking.
- For multi-file tasks, show a plan first and wait for approval.
- When editing a script, return the **full script**, not a diff.
- Ask before installing packages or deleting files.

## Coding conventions

- R, tidyverse, native pipe `|>`. dplyr ≥ 1.2 (`recode_values()`, not `case_match()`).
- Script section order: setup → config/constants → helper functions → data loading → checks → export.
- Keep per-district named objects (`Karongi_*`, `Rulindo_*`, …) rather than lists when splitting by district.
- Long loops/scripts print progress: `[i/n]` step messages, start time, elapsed time, rows processed.
- Use `filter(col)` not `filter(df$col)` inside pipes.
- Use dates relative to `Sys.Date()`, never hard-coded calendar dates for "recent" filters.
- Unknown parameters go in the config section as named constants marked `# TBD`, never buried in code.

---

## Paths

- Midline HFC uses **two** objects from `PATHS.R`: `DROPBOX_EAQIP` (the `Rwanda Energy/EAQIP` Dropbox
  folder) and `ONEDRIVE_HFC_ML` (`OneDrive - WBG/Rwanda_Energy_Project/EAQIP_survey/HFC_26`, the
  shared dashboard folder).
  Do not use or add any other `PATHS.R` objects for midline HFC.
- Every path is written out in full from `DROPBOX_EAQIP`, **once**, in the config section of
  `1.hfc_main.R`. Scripts 2–9 use only the objects defined there and never build paths themselves.

| Object (in `1.hfc_main.R`) | Path from `DROPBOX_EAQIP` | Contents |
|---|---|---|
| `form_path` | `datawork/HFC_midline/test/0_REP_SurveyCTO_Test_Form.xlsx` | Form definition |
| `data_dir` | test: `datawork/HFC_midline/test/synthetic_data` · live: `datawork/HFC_midline/data` | SurveyCTO export (+ `media/` live) |
| `preload_dir` | test: `datawork/HFC_midline/test/synthetic_data/preload` · live: `questionnaires/02-midline/02-CTO-preload` | Preload CSVs |
| `media_dir` | test: `datawork/HFC_midline/test/synthetic_data/media` (none) · live: `datawork/HFC_midline/data/media` | SurveyCTO media (audio audits) |
| `output_dir` | test: `datawork/HFC_midline/test/output` · live: `datawork/HFC_midline/output` | Local HFC outputs |
| `audio_dir` | `output_dir` + `/audio_check` | Daily audio audit lists and copied audio files |
| `archive_dir` | `output_dir` + `/archive` | Previous versions of `hfc_constr.xlsx` and the dashboard |
| `checks_dir` | `output_dir` + `/checks` | `hfc_flags.csv` (all flags; used by the test comparison) |
| `share_dir` | `ONEDRIVE_HFC_ML` + test: `/test` · live: `/main` (`SHARE_FOLDER`; `pilot` pending, pilot not started) | Current dashboard, shared with the team and the firm |
| `share_archive_dir` | `share_dir` + `/archive` | Previous shared dashboards |

## Run mode

`HFC_MODE` (`"test"` or `"live"`) is set at the top of `1.hfc_main.R` and picks the paths above.
Test-mode dashboards go to `HFC_26/test` only, never to `HFC_26/main`.

---

## Survey scope

- Panel follow-up of **1,973 baseline respondent households** in **180 villages**
  (193 randomized; 13 dropped from scope due to 15kV line).
- Districts: Karongi, Rulindo, Rusizi, Rutsiro.
- Treatment is assigned at **village level** (originally `scope_193_0807.xlsx`, column `treat`).
  The HFC takes it from `contact_info.csv` (`treat`), joined on `hh_id`:

| Code | Arm | Households |
|---|---|---|
| C | Control | 530 |
| T1 | Readyboard | 537 |
| T2 | Solar off-grid (SHS) | 483 |
| T3 | Readyboard + SHS | 423 |

- `household_id` = `villageid_key` × 10000 + household number × 10 + check digit
  (digit sum mod 10). The village can be recovered from the ID: all but the last 4 digits.

## Data sources

| What | File | Location |
|---|---|---|
| Form definition | `0_REP_SurveyCTO_Test_Form.xlsx` (sheets `survey`, `choices`, `settings`) | `form_path` |
| Survey export | `<form_id>_WIDE.csv` (SurveyCTO, **wide** CSV, cumulative) | `data_dir` |
| Preload: enumerators | `test_enumerators.csv` (rename to production before launch) | `preload_dir` |
| Preload: villages | `scope_villages.csv` | `preload_dir` |
| Preload: households | `household_head.csv` | `preload_dir` |
| Preload: phones + treatment | `contact_info.csv` (`householdid_key, phone_clean, phone2_clean, treat`) | `preload_dir` |
| Preload: SHS eligible list | `shs_eligible.csv` (`householdid_key`, eligible households only) | `preload_dir` |

- Form id is currently `REP_readyboard_test` (TBD: production form id).
- The form definition is the source of truth for variable names, types, choice codes,
  relevance and constraints. Read it; do not hard-code these from memory.
- There is **no previous round of HFC data** for midline.

## Key variables (midline form)

- **Submission metadata**: `KEY`, `SubmissionDate`, `starttime`, `endtime`, `formdef_version`,
  `deviceid`, `enumerator`, `survey_duration`; module timestamps `stamp_*_1` / `stamp_*_2`.
- **Identifiers**: `hh_id`, `district`, `sector`, `cell`, `village`.
- **Treatment arm**: not in the form/export. Join `treat` from `contact_info.csv`
  (`householdid_key` = `hh_id`) in script 2; all arm-level checks use this `treat`.
- **Tracking & consent**: `locate`, `nolocate`, `consent`, `no_consent_reason`, `finish`, `conclusion_nofinish`.
- **Household & contact**: `hh_member_count`, `first_name`, `last_name`, `confirm_phonenumber`,
  `phonenumber_revise`, `phonenumber`, `confirm_second_phonenumber`, `second_phonenumber_revise`, `second_phonenumber`.
- **Grid connection & spending**:
  - Meter: `meter_confirm`, `meter_confirm_working`, `meter_date`, `meter_location`, `meter_payment`, `mt_1`–`mt_7`
  - Readyboard: `readyboard_confirm`, `readyboard_confirm_working`, `readyboard_date`, `readyboard_payment`, `rb_1`–`rb_7`
  - Internal wiring: `additional_wire_confirm`, `additional_wire_confirm_working`, `additional_wire_date`, `additional_wire_payment`, `iw_1`–`iw_7`
  - "Other (specify)" texts are `<var>_other` (e.g. `mt_3_other`, `rb_4_other`).
  - Electricity use: `electricity_usage_confirm`, `electricity_usage_payment`, `eu_1`–`eu_6`
- **Solar Home System**: `shs_confirm`, `shs_confirm_other` (TBD: likely should be `shs_confirm_working`), `shs_number`, `shs_date`, `shs_type`,
  `shs_install`, `shs_own`, `shs_payment`, `shs_1`–`shs_7`.
- **SHS nudge**: `pull_shs_eligible`, `nosolar_confirm`, `shs_willing`, `shs_data_consent`.
- **Device observation**: `observe_electricity_connection`, `obs_gps`, `obs_shs`, `obs_consent`,
  `meter_photo`, `meter_number`, `readyboard_photo`, `shs_photo`.
- **Mobile phone charging**: `F1_0`, `F1_1`; repeat (wide: `_1`, `_2`, … suffix) `F2_1` location,
  `F2_2` frequency, `F2_3` pays to charge, `F2_4` monthly fee.

The midline form has **no demographics module** (no gender/age/education). The form exports
`a_duration_min`, `b_duration_min`, `c_duration_min`, `nudge_duration_min` and `interview_duration`,
but compute module durations in R from the `stamp_*` pairs (seconds since form start).

## Special codes

| Code | Meaning | Where |
|---|---|---|
| -77 | Other (specify) | most choice lists |
| -88 | Refused / Don't know / Not sure | `noconsent`, `solartype`, `shs_willing` |

- Codes **-66 and -99 are not used** in midline.
- In **categorical** variables, keep -77/-88 as categories (e.g. "not sure" in `shs_willing` is a result).
- Convert -77/-88 to `NA` only in **continuous** variables (amounts, counts) before outlier and summary stats.

## Key definitions

- **Attempt**: any submission. **Located**: `locate == 1`. **Consented**: `consent == 1`.
- **Complete**: `locate == 1 & consent == 1 & finish == 1`, deduplicated on `hh_id`.
- **Duplicate**: same `hh_id` with more than one consented submission.
- **SHS-eligible (preload)**: household is in `shs_eligible.csv` (eligible households only,
  `eligible = 1`); in the form, `pull_shs_eligible == 1` (pulldata of column `eligible`).
- **SHS-eligible (confirmed at door)**: `pull_shs_eligible == 1 & shs_confirm == 0 & electrified == 0`
  (the relevance of the `nosolar` group). `electrified` is a form calculate: 0 if no meter, or meter
  only (no readyboard, no wiring, no grid use); else 1. `nosolar_confirm` is asked only for these
  households and is always 1 (form constraint), so it is not used to define eligibility.
- **Nudge outcomes**: `shs_willing` (1 yes / 0 no / -88 not sure); `shs_data_consent == 1`.
- **Enumerator correction**: `nosolar_confirm` cannot be 0 (form constraint forces a return to 6.1).

## Config constants (set in `1.hfc_main.R`)

- `FIELDWORK_START`: TBD (date). Submissions before it are excluded.
- `CURRENT_FORMDEF`: TBD (numeric). Lower versions are flagged.
- `DURATION_MIN`, `DURATION_MAX`: TBD from pilot (minutes).
- Test-mode values (the synthetic answer key assumes these): `FIELDWORK_START = 2026-10-12`,
  `CURRENT_FORMDEF` = the test form's `settings$version`, `DURATION_MIN = 15`, `DURATION_MAX = 90`.

---

## Pipeline (`RCT_data/midline/HFC/scripts/`)

All scripts share one R session; `1.hfc_main.R` sources 2–9 in order. Scripts 3–9 depend on
objects created earlier (`hfc_constr`, `duplicates`, length checks) and cannot run alone.

| Script | Purpose | Main outputs |
|---|---|---|
| `1.hfc_main.R` | Runner: sets `HFC_MODE`, paths and config; sources 2–9; prints timing per script | — |
| `2.hfc_construct.R` | Load export; parse dates; filter fieldwork start; codes → NA (continuous only); join enumerator + admin names; module durations; duplicates | `hfc_constr`, `duration_data`, `duplicates` |
| `3.hfc_admin.R` | Progress by village, district, **treatment arm** (households: attempted = complete + not complete; `*_pct` as 0-100 numbers; T2/T3 nudge eligible / agreed / data consent); households not located | `village_data`, `district_data`, `treatment_data`, `not_located` |
| `4.hfc_outliers.R` | ±3 SD on log(1+x) for amounts and counts (`mt_1`, `rb_1`, `iw_1`, `eu_1`, `eu_6`, `shs_1`, `shs_6`, `F2_4`, `hh_member_count`, `F1_1`) | `outlier_data` |
| `5.hfc_logic.R` | Consistency checks (see below); summary by date (per enumerator: `n_logic_issue` in `enumerator_check`) | `logic_data`, `logic_summary` |
| `6.hfc_enum.R` | Per enumerator: submissions/day, locate/consent/completion rates, duration, flags | `enumerator_check` |
| `7.hfc_stats.R` | Key variables (yes/no as % yes, with the base `asked_if`), choice frequencies, energy access; overall and by arm | `desc_stats_data`, `desc_cat_data`, `energy_access` |
| `8.hfc_audio.R` | Daily audio audit sample (TBD: confirm). **No backcheck in midline**: no backcheck sample or comparison | `audio_sample` |
| `9.hfc_shs.R` | **SHS nudge funnel** for T2/T3 (see below) | `shs_funnel`, `shs_funnel_village` |
| `10.hfc_dashboard.R` | Excel dashboard workbook: "Dashboard" sheet (key rates vs targets, progress by arm + nudge, by district, logic issues, energy access, key variables, daily chart), then every output as a tab; definition row under each dashboard table | `HFC_midline_dashboard.xlsx` |

### Logic checks for script 5

- Start/end/submission date mismatches; negative module durations; consented survey outside
  `DURATION_MIN`–`DURATION_MAX` (per submission).
- Treatment compliance: readyboard reported in C/T2 villages; SHS-nudge block answered outside T2/T3.
- Grid consistency: `electricity_usage_confirm == 1` but `meter_confirm == 0`;
  `observe_electricity_connection` disagrees with `meter_confirm`.
- SHS consistency: `obs_shs == 1` but `shs_confirm == 0`.
- Removed 2026-10-22 (not daily logic issues): old form version, `shs_number` > 3, phone changed
  from preload. Preload mismatch (`shs_eligible.csv` household not in T2/T3) is a one-time warning
  in script 2.

### SHS funnel for script 9 (T2 + T3, by arm and village)

Households in arm → attempted → located → consented → preload-eligible →
door-confirmed eligible (`pull_shs_eligible == 1 & shs_confirm == 0 & electrified == 0`) →
`shs_willing` (yes / no / not sure) →
`shs_data_consent == 1`. Report counts and % of the previous step.

### Known baseline bugs — fix when porting

1. Script 6: `enumerator %in% location` compares to a data frame → use `location$enumerator`.
2. Script 8: `formal_savings != formal_savings` self-comparison; `case_when` keeps only the first mismatch.
   (Not applicable: no backcheck in midline.)
3. Script 2: comment says dmy but code uses `mdy_hms()` → check against a real midline export.
4. Script 1: bare `1` lines do nothing; authenticate once with `googlesheets4::gs4_auth(email = ...)`.
5. Script 5: `formdef_version` compared as a string → compare as numeric.
6. Script 8: hard-coded backcheck date → `submissiondate == Sys.Date() - 1`, excluding already-backchecked `hh_id`.
   (Applies to the audio sample: yesterday's submissions, excluding already-audited `hh_id`.)
7. `HFC_24/app.R` is the RStudio Old Faithful template, not a dashboard.

## Outputs

- `output_dir` (live: `datawork/HFC_midline/output`; test: `datawork/HFC_midline/test/output`) holds
  only the current `hfc_constr.xlsx` (constructed data) and `HFC_midline_dashboard.xlsx`. Each run
  first moves the previous versions to `archive_dir` (`output_dir/archive`), stamped with their
  last-modified time (`_YYYYMMDD_HHMM`). If a file is open in Excel, a time-stamped copy is written
  instead and a warning is shown.
- `checks_dir` (`output_dir/checks`): `hfc_flags.csv`, all flags (`KEY, hh_id, enumerator, check,
  detail`; check names match `expected_flags.csv`), read by `test/compare_expected_flags.R`.
- Excel dashboard (`10.hfc_dashboard.R`, openxlsx): written to `output_dir`; reads the HFC outputs,
  does not recompute checks. Tab order: Dashboard, enumerator_check, district_data, village_data,
  duplicates, logic tabs, then the rest. Replaces the Google Sheet and the planned Shiny app.
- Sharing (`share_dashboard()` in `1.hfc_main.R`): after each run the current dashboard is copied
  to `share_dir` on the WBG OneDrive, and the previous one there is moved to `share_archive_dir`
  (stamped `_YYYYMMDD_HHMM`, like the Dropbox archive). The team and the survey firm get the same
  workbook. On Windows the move and copy run through `cmd`: once the HFC packages are loaded, the
  R process itself cannot write into the OneDrive folder (likely an IT restriction).

## Data quality targets

Locate rate > 90% · Consent rate > 95% · Completion rate > 90% · Records with logic errors < 5%. (No backcheck in midline.)

## Testing

- Generator script (code, in repo): `RCT_data/midline/HFC/test/0.generate_synthetic_data.R`;
  documentation in `RCT_data/midline/HFC/test/README.md` (in repo, not Dropbox).
- Synthetic data (in Dropbox, `datawork/HFC_midline/test/synthetic_data`): `REP_readyboard_test_WIDE.csv`, `preload/*.csv`,
  and the answer key `expected_flags.csv` (`KEY, hh_id, check, expected_flag, description`).
- Synthetic data includes planted problems (duplicates, outliers, each logic-check violation,
  T2/T3 households at every SHS-funnel step) so every check has at least one expected hit.
- A test run passes when the HFC flags match `expected_flags.csv`: no missed flags, no unexpected ones.
- Answer-key conventions: duration checks apply to consented submissions only; enumerator rows
  have empty `KEY`/`hh_id` (enumerator id in `description`); funnel rows carry counts in `expected_flag`.
- Regenerate from the repo root after any form change (field names and codes are read from the form).

## Required packages

tidyverse, lubridate, readxl, writexl, janitor, scales, googlesheets4, shiny, sf.