Generate a synthetic midline SurveyCTO dataset to test the HFC pipeline.

Read `RCT_data/midline/HFC/AGENTS.md` first and follow it. In particular: synthetic data only,
invented names and numbers, code in the repo, data in Dropbox.

## Environment

- R is not on the system PATH. Run R scripts with:
  `& "C:\Users\wb636130\AppData\Local\Programs\R\R-4.5.2\bin\x64\Rscript.exe" <script.R>`
- Paths: `source("PATHS.R")` from the repo root gives `DROPBOX_EAQIP`. Build every other path
  from it, written out in full, in the config section of the script.

## Goal

A fake but realistic export that the HFC pipeline can run on, where I know in advance which
records every check should flag. The answer key matters as much as the data.

## Inputs

- Form definition: `file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/0_REP_SurveyCTO_Test_Form.xlsx")`
  (sheets `survey`, `choices`, `settings`). Use it for variable names, types, choice codes,
  relevance, constraints and required flags. Do not hard-code these from memory.
- Treatment arms, ID rule, special codes, key definitions, logic checks and the SHS funnel: `AGENTS.md`.

## Outputs

Code (in the repo):
- `RCT_data/midline/HFC/test/0.generate_synthetic_data.R`: the generator. Seeded
  (`set.seed(20261021)`), re-runnable, section order and progress messages as in AGENTS.md.
- `RCT_data/midline/HFC/test/README.md`: short note on how the data was built and how to regenerate it.

Data (in Dropbox, `file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/synthetic_data")`):
1. `preload/`: synthetic `household_head.csv`, `contact_info.csv`, `shs_eligible.csv`,
   `scope_villages.csv`, `test_enumerators.csv`, with the same columns as the real preload
   files and IDs that follow the household_id rule.
2. `REP_readyboard_test_WIDE.csv`: the synthetic export in SurveyCTO wide format
   (one row per submission, repeat fields suffixed `_1`, `_2`, …, metadata columns
   `KEY`, `SubmissionDate`, `starttime`, `endtime`, `formdef_version`, `deviceid`).
3. `expected_flags.csv`: the answer key, one row per planted problem:
   `KEY, hh_id, check (script + check name), expected_flag, description`.

## Design

- About 200 households in about 20 villages across all 4 districts and all 4 arms
  (keep arm shares close to 530/537/483/423). 8 enumerators, 5 fieldwork days.
- Scenario-first: decide each household's situation (not located, refused, meter only,
  meter + readyboard, SHS, preload-eligible and confirmed, etc.), then fill the fields that
  situation implies.
- Respect the form: a field is filled only when its relevance is true, required fields are
  never empty when relevant, values satisfy constraints and use valid choice codes. The only
  exceptions are the deliberately planted problems below.
- Plausible values: amounts in Rwf on realistic ranges, dates within the last 2 years
  (`month-year` fields on the 1st of the month), durations of 20–60 minutes.

## Planted problems (each at least once, each listed in expected_flags.csv)

- Duplicates: same `hh_id` submitted twice with consent, by the same enumerator and by different enumerators.
- Outliers: at least one high value for each variable in the script 4 outlier list.
- Every logic check listed in AGENTS.md under "Logic checks for script 5".
- Old `formdef_version`; start date ≠ end date; end date ≠ submission date; one too-short and one too-long survey.
- One enumerator with a low consent rate and one with many short surveys.
- SHS funnel: T2 and T3 households at every step (not located, refused, not preload-eligible,
  preload-eligible but has meter + readyboard, confirmed eligible with `shs_willing` = 1 / 0 / -88,
  `shs_data_consent` = 1 / 0).
- At least one `-77` "other" with a specify text and one `-88` in each list that allows it.

## Validation (run before finishing)

In the generator, for every field in the `survey` sheet, translate its relevance expression
to R (`${x}` → `x`, `=` → `==`, `and` → `&`, `or` → `|`, `selected(${x}, 'v')` → `str_detect`)
and report any field that is filled when not relevant, or empty when relevant and required.
The only rows allowed to fail are those in `expected_flags.csv`. Print a summary table and
stop with an error if anything else fails.

## How to work

1. First show me a plan: the scenario table (scenario → number of households → which arms)
   and the list of planted problems mapped to checks. Wait for my OK.
2. Then write the generator, run it, fix errors and re-run until validation passes.
3. Finish with: files created, row counts by arm and scenario, and the validation summary.