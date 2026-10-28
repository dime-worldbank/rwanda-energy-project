# Midline HFC scripts

High-frequency checks for the midline SurveyCTO survey. Only `1.hfc_main.R` is run;
it sources scripts 2-10 in order in one R session.

```
Rscript RCT_data/midline/HFC/scripts/1.hfc_main.R      # from the repo root
```

Set `HFC_MODE` at the top of `1.hfc_main.R`: `"test"` reads the synthetic data
(see [../test/README.md](../test/README.md)), `"live"` reads the real export.
The pipeline reads deidentified inputs written by `0.hfc_deidentify.R` (rules in `hfc_privacy.R`).
It uses an **allowlist**: only form fields that are numbers, dates, choices or calculates are kept.
Names, phones, GPS (including `-Latitude`-style sub-columns), other free text (`comment`), audio,
photos, text audits and device ids are dropped. `hh_id` and the treatment arm are kept. The preload
files are cut to the columns the HFC needs (`household_frame.csv`: `householdid_key`, `villageid_key`,
`treat`). Scripts 1-10 stop if an input still has an identifying column. "Other (specify)" text
fields (`*_other`) are kept on purpose.
Run `0.hfc_deidentify.R` first, and again after each new export.
All paths, targets and thresholds are set in `1.hfc_main.R`.

## What each script does

Each script adds its tables to `hfc_outputs`. Script 10 writes one tab per table
to the dashboard workbook.

| Script | What it does | Dashboard tabs it creates |
|---|---|---|
| `1.hfc_main.R` | Runner. Sets the mode, paths, config (fieldwork start, duration limits, key-rate targets `LOCATE_TARGET`, `CONSENT_TARGET`, `COMPLETION_TARGET`, `LOGIC_TARGET`), the helpers (`add_flags()`, `archive_old()`, `save_current()`), runs scripts 2-10 and saves `hfc_constr.xlsx` and `checks/hfc_flags.csv`. | `run_info` |
| `2.hfc_construct.R` | Loads the SurveyCTO export, parses dates, keeps submissions from the fieldwork start on, sets special codes (-77, -88) to NA in continuous variables, adds enumerator and village names and the treatment arm, computes module durations and `nudge_eligible`, finds duplicate `hh_id`s. Builds `hfc_constr`, the dataset every later script uses. | `duration_data`, `duplicates` |
| `3.hfc_admin.R` | Fieldwork progress against the preload (target households): attempted, located, consented, complete, with `*_pct` rates and submissions per day. The SHS nudge columns (T2/T3) are in the arm table. Also lists households never located. | `village_data`, `district_data`, `treatment_data`, `not_located` |
| `4.hfc_outliers.R` | Outliers in amounts and counts: values beyond ±3 SD of log(1 + x). | `outlier_data` |
| `5.hfc_logic.R` | 10 logic checks: dates and durations, treatment compliance (readyboard outside T1/T3, nudge outside T2/T3), grid and SHS consistency. One row per submission x issue, and a summary by date. | `logic_data`, `logic_summary` |
| `6.hfc_enum.R` | One row per enumerator: submissions per day, locate / consent / completion rates, durations, and counts of duplicates, logic issues and outliers. Flag columns are named after their threshold (e.g. `consent_rate_below_95`). | `enumerator_check` |
| `7.hfc_stats.R` | Descriptives for consented households, overall and by arm: key variables (% yes or mean / sd / min / median / max), frequencies of every choice field (special codes kept), and energy access (meter, readyboard, wiring, grid use, SHS, none). | `desc_stats_data`, `desc_cat_data`, `energy_access` |
| `8.hfc_audio.R` | Disabled in the deidentified HFC pipeline; audio is not used in the dashboard or HFC checks. | — |
| `9.hfc_shs.R` | SHS nudge funnel for T2 and T3: households in arm, then attempted, located, consented, eligible, willing (yes / no / not sure), data consent. By arm and by village. | `shs_funnel`, `shs_funnel_village` |
| `10.hfc_dashboard.R` | Writes the workbook: the `Dashboard` tab first, then every table above as a formatted tab. | `Dashboard` |

## The `Dashboard` tab

| Block | Source |
|---|---|
| Key rates vs targets | scripts 3 and 5, targets from `1.hfc_main.R` |
| Progress by arm, with the SHS nudge | `treatment_data` (script 3) |
| Progress by district | `district_data` (script 3) |
| Logic issues | `logic_summary` (script 5) |
| Energy access | `energy_access` (script 7) |
| Key variables | `desc_stats_data` (script 7) |
| Daily submissions chart | `hfc_constr` (script 2) |

Each block has a grey definition row under its column names.

## Tab order

`Dashboard`, `enumerator_check`, `district_data`, `village_data`, `duplicates`,
`logic_summary`, `logic_data`, then the other tabs, with `run_info` last
(`TAB_FIRST` in `10.hfc_dashboard.R`).

## Output files

In `output_dir` (Dropbox, `datawork/HFC_midline/output`, or `test/output` in test mode):

| File | Contents |
|---|---|
| `HFC_midline_dashboard.xlsx` | The dashboard workbook (all tabs above) |
| `hfc_constr.xlsx` | The constructed dataset from script 2 |
| `archive/` | Earlier versions of both files, renamed `<name>_YYYYMMDD_HHMM.xlsx` |
| `checks/hfc_flags.csv` | Every flagged row, used by `test/compare_expected_flags.R` |
| `audio_check/<YYYYMMDD>/` | Audio audit list and files |

On the WBG OneDrive (`ONEDRIVE_HFC_ML` in `PATHS.R`: `Rwanda_Energy_Project/EAQIP_survey/HFC_26`),
each run also updates the shared copy that the team and the survey firm open:

| File | Contents |
|---|---|
| `HFC_26/<folder>/HFC_midline_dashboard.xlsx` | The current dashboard (`<folder>` = `test` in test mode, `main` in live mode; `pilot` pending) |
| `HFC_26/<folder>/archive/` | Earlier dashboards, renamed `HFC_midline_dashboard_YYYYMMDD_HHMM.xlsx` |

R cannot write to OneDrive directly on WBG laptops (probably an IT restriction), so on Windows the
move and copy run through `cmd` (`share_dashboard()` in `1.hfc_main.R`).

If a file is open in Excel, it cannot be replaced. The script then saves a copy
with the time added to the name and gives a warning.
