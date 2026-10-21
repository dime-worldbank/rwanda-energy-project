Port the baseline HFC scripts to midline and test them on the synthetic data.

Read `RCT_data/midline/HFC/AGENTS.md` first and follow it. In particular: never edit
`RCT_data/baseline/`, code in the repo, data in Dropbox, all paths from `DROPBOX_EAQIP` defined once
in `1.hfc_main.R`.

## Environment

- Run R with `& "C:\Users\wb636130\AppData\Local\Programs\R\R-4.5.2\bin\x64\Rscript.exe" <script.R>`
  from the repo root.
- Test data: `file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/synthetic_data")`, built by
  `RCT_data/midline/HFC/test/0.generate_synthetic_data.R`. Settings it assumes:
  `RCT_data/midline/HFC/test/README.md`.

## Goal

A midline HFC pipeline in `RCT_data/midline/HFC/scripts/` that runs end to end in test mode and
flags exactly what `expected_flags.csv` lists: no missed flags, no unexpected ones.

## Inputs

- Baseline scripts to port: `RCT_data/baseline/HFC/scripts/1.hfc_main.R` to `8.hfc_bc.R`.
  Midline has no backcheck: `8.hfc_bc.R` becomes `8.hfc_audio.R` (audio audit sample only).
  Read them for structure and intent; rewrite for midline rather than copying line by line.
- Out of scope unless I say so: `building.R`, `daily_accomplisments.R`, `data quality.R`,
  `duplicates.R`, `issues.R`, `training sample.R`, `HFC_24/app.R`. Tell me if any of them contain
  a check that AGENTS.md is missing.
- Form definition (variable names, codes, relevance): `form_path` in AGENTS.md. Do not hard-code
  these from memory.
- Pipeline, checks, key definitions, special codes and the known baseline bugs: AGENTS.md.

## Outputs (code, in the repo)

- `RCT_data/midline/HFC/scripts/1.hfc_main.R` to `9.hfc_shs.R`, as in the AGENTS.md pipeline
  table. `9.hfc_shs.R` is new (no baseline version).
- `RCT_data/midline/HFC/test/compare_expected_flags.R`: reads the HFC outputs from a test run and
  `expected_flags.csv`, and prints, by check, expected vs flagged, missed and unexpected. Stops with
  an error if anything is missed or unexpected.
- Test-run outputs go to `output_dir` (test: `datawork/HFC_midline/test/output`), never the Google Sheet.

## Rules for the port

- Test mode (`HFC_MODE = "test"`) uses the test settings in `test/README.md`. Live-mode constants
  stay `# TBD` in the config section.
- Fix the 7 known baseline bugs listed in AGENTS.md; list where each was fixed.
- Use midline variable names from the form (e.g. `*_confirm_working`, `<var>_other`). Treatment
  arm comes from `contact_info.csv` (`treat`), not from the export.
- Each check must produce output whose names match the `check` column in `expected_flags.csv`
  (e.g. `5.hfc_logic: duration_short`), so the comparison is a join, not a mapping table.
- Keep each script readable on its own: header, section order and progress messages as in AGENTS.md.

## How to work

1. First show me a plan:
   - each baseline script → midline script: what is kept, dropped, added;
   - each check in `expected_flags.csv` → the script and output object that produces it;
   - anything in the baseline that AGENTS.md doesn't cover, and any open question.
   Wait for my OK.
2. Then work in batches, running the pipeline and `compare_expected_flags.R` after each one:
   - Batch 1: `1.hfc_main.R`, `2.hfc_construct.R`, `compare_expected_flags.R` (duplicates).
   - Batch 2: `3.hfc_admin.R`, `4.hfc_outliers.R`, `5.hfc_logic.R`.
   - Batch 3: `6.hfc_enum.R`, `7.hfc_stats.R`, `8.hfc_audio.R`, `9.hfc_shs.R`.
   Stop after each batch, show me the comparison result, and wait for my OK before the next.
3. Finish with: files created, the full comparison table (all checks pass), the baseline bugs
   fixed, and the TBDs left for live mode.
