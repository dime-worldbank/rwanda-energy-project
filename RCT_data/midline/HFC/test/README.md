# Midline HFC test: synthetic data

`0.generate_synthetic_data.R` builds a fake SurveyCTO export, preload files and an answer key
for testing the midline HFC pipeline (`../scripts/`).

**Synthetic data only.** All names, phone numbers (`0700xxxxxx`), NIDs, IDs and coordinates are
invented. The generator reads only the form definition, never real household data. The data
lives in Dropbox, not in this repo.

## Regenerate

From the repo root:

```
Rscript RCT_data/midline/HFC/test/0.generate_synthetic_data.R
```

- Seeded (`set.seed(20261021)`), with static fieldwork dates, so every run gives the same data.
- Field names, choice codes and relevance are read from the form
  (`datawork/HFC_midline/test/0_REP_SurveyCTO_Test_Form.xlsx`). Re-run after any form change.
- The script validates the data against the form (relevance, required, constraints, choice codes)
  and against the answer key, and writes nothing if validation fails.

## Output (Dropbox: `datawork/HFC_midline/test/synthetic_data/`)

- `REP_readyboard_test_WIDE.csv`: 203 submissions (200 households attempted), SurveyCTO wide
  format, repeat fields `_1`..`_12`.
- `preload/`: `household_head.csv`, `contact_info.csv`, `shs_eligible.csv`, `scope_villages.csv`,
  `test_enumerators.csv`. 210 households in 20 villages, 4 districts, 4 arms; 8 enumerators.
- `expected_flags.csv`: answer key (`KEY, hh_id, check, expected_flag, description`).
  - Record-level flags (scripts 2, 4, 5): one row per flagged submission, `expected_flag = 1`.
  - Enumerator flags (script 6): `KEY`/`hh_id` empty, enumerator id in `description`.
  - SHS funnel (script 9): `expected_flag` = household count per step and arm.
  - `7.hfc_stats: code_-77/-88`: special codes that must show up as categories (not errors).

A test run passes when the HFC flags match `expected_flags.csv`: no missed flags, no unexpected ones.

## Test settings assumed by the answer key (set the same in `1.hfc_main.R`)

- Fieldwork: 2026-10-12 to 2026-10-16. `FIELDWORK_START = 2026-10-12`.
- `CURRENT_FORMDEF` = the form's `settings$version` (format `yymmddHHMM`, must be above the
  planted old version 2610150900).
- `DURATION_MIN = 15`, `DURATION_MAX = 90` minutes, on `survey_duration / 60`, consented submissions only.
- Outliers: |z| > 3 on log(1 + x), pooled over all submissions (`F2_4` over all repeat instances).
- Enumerator flags: consent rate among located < 95%; at least 3 consented surveys < `DURATION_MIN`.
- Treatment arm is not in the export: join `treat` from the deidentified `preload/household_frame.csv` (made by `scripts/0.hfc_deidentify.R` from `contact_info.csv`) on `hh_id = householdid_key`.
- Datetimes use the SurveyCTO export format `Oct 12, 2026 8:03:11 AM` (parse with `mdy_hms()`); dates `Nov 1, 2024`.
- One household refuses and completes on a revisit: not a duplicate (only one consented submission).

If you change any constant in the generator's config section, update this file.
