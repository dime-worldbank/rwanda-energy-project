# Midline HFC scripts

High-frequency checks for the midline SurveyCTO survey. 
It is ran everyday to ensure quality data collection, monitoring possible errors, and performance. 
This README.md includes steps on how to run HFCs and create dashboard, and summaries for each scripts. 

## How to run HFC

When running it for the first time on your local computers, follow below steps. 

### (1) Define path
Ensure relative folder paths are defined. 
Go to `Github/rwanda-energy-project/PATHS.R` and add a block for your Windows username
(`Sys.getenv("USERNAME")`) that sets GITHUB, ONEDRIVE, DROPBOX and SHARED_EAQIP for your computer.
The macro folder paths are used throughout the scripts, so without it, it will show the PATHS does not exist.

**Where the HFC data lives (`SHARED_EAQIP`).** WBG laptops cannot install Dropbox, so the midline HFC
data is kept in a shared WBG OneDrive folder, `EAQIP_HFC_shared` (owned by Tanay, shared with Yeji).
Both scripts (`0.hfc_deidentify.R`, `1.hfc_main.R`) and the test scripts read and write only through
`SHARED_EAQIP`. Everything else in the repo still uses `DROPBOX_EAQIP`.

To get the folder on your computer: open the share link in the browser, click **Add shortcut to My files**,
then in File Explorer right-click `EAQIP_HFC_shared` > **Copy as path** and paste it in your PATHS.R block:

```
# Yeji work
SHARED_EAQIP <- file.path("C:/Users/wb636130/OneDrive - WBG/Rwanda Energy Project/EAQIP_HFC_shared")

# Tanay work
SHARED_EAQIP <- file.path(ONEDRIVE, "EAQIP_HFC_shared")
```

If `SHARED_EAQIP` is not set for a user, it falls back to `DROPBOX_EAQIP`, so the old Dropbox setup still works.

The shared folder mirrors the Dropbox `EAQIP` subfolders:

```
EAQIP_HFC_shared/
├── questionnaires/02-midline/
│   ├── REP_questionnaire_SurveyCTO_prep_YYMMDD.xlsx   <- live form (one only)
│   ├── !-archived-survey/                              <- older forms; pilot form REP_questionnaire_SurveyCTO_26102027.xlsx
│   └── 02-CTO-preload/                                 <- raw preload (live): contact_info, shs_eligible, scope_villages, enumerators
│       └── pilot/                                      <- raw preload (pilot), with test_enumerators.csv
└── datawork/HFC_midline/
    ├── data/                                           <- raw live export REP_readyboard_test_WIDE_YYMMDD.csv (one only)
    │   └── !-archived/                                 <- older exports
    ├── deidentified/                                   <- written by script 0 (live); preload/ holds the deidentified preload
    ├── output/                                         <- written by script 1 (live): dashboard, hfc_constr, archive/, checks/
    ├── pilot/{data, deidentified, output}              <- same layout for pilot mode
    └── test/{synthetic_data, deidentified, output}     <- same layout for test mode
```

Raw files (`data/`, `02-CTO-preload/`) contain names, phone numbers and GPS. Never save them in
`deidentified/` or in this repo, and only share the folder with team members who need the raw data.

### (2) Download the Survey response in WIDE format.

Go to SurveyCTO website, choose 4. Export tab, and download data for REP_readyboard_test. 
When downloading choose 'Wide format(default)' and REP_readyboard_test.csv will download.

(For data of 'actual data collection period' and not pilot period) 
Move this data to below folder 
`EAQIP_HFC_shared\datawork\HFC_midline\data` (pilot data goes to `datawork\HFC_midline\pilot\data`)

You may see a downloaded data of previous date exists in the folder. 
In that case move any previous data (indicated by the date in title) to `!-archived` folder.  

Only the most up-to-date data should live under data folder, and change the most recent csv file name to 'REP_readyboard_test_WIDE_YYMMDD.csv'.
YYMMDD should be the latest data collection date (6 digits, e.g. `_261029`); the scripts do not find the file otherwise. 

### (3) Now we run two HFC scripts 

`0.hfc_deidentify.R` and `1.hfc_main.R`. 

For each script, Section 2 (Config/ constants) sets one of the three possible HFCs mode -- test, pilot, and live -- that one should adjust as per purpose of the checks. 

-- Test (no longer needed) mode was used to test the pipeline in pre-pilot period. It used synthetic data and the dashboard does not indicate pipeline malfunctions and since have been archived. 

-- Pilot (October 27th, 28th) mode read and produced dashboard for 75 survey data from two days of survey pilot, which happened in Gicumbi province.

-- Live (Actual data collection, from October 31st; earlier submissions in the export are pilot/test and are dropped by `fieldwork_start` in `1.hfc_main.R`) mode sets the data path for actual data collection by the survey firm. This mode is what should be selected in Section 2. Ensure that HFC_MODE is set to live in this case. 

```
# ---- 2. Config / constants ---------------------------------------------------

HFC_MODE <- "live"                   # "test", "pilot (Oct 27, 28)", or "live (Oct 31 -) "
stopifnot(HFC_MODE %in% c("test", "pilot", "live"))
```

As indicated in the title, script 0 deidentifies identifiable personal data not needed for HFCs (such as names, phone numbers, national IDs, GPS coordinates -- full list of deidentified variables are in `hfc_privacy.R`). 

Script 1 runs all the other scripts from 2 to 10, creating midline dashboard tracking aggergated data collection and breakdown by variables of interest.

Set the same `HFC_MODE` in both scripts, then run them from the repo root (RStudio: open the project, or `setwd()` to the repo folder):

```
source("RCT_data/midline/HFC/scripts/0.hfc_deidentify.R")
source("RCT_data/midline/HFC/scripts/1.hfc_main.R")
source("RCT_data/midline/HFC/scripts/11.hfc_backcheck.R")   # optional: back-check call list
```

Script 11 updates `output/checks/backcheck_list.xlsx`, to catch fake interviews. It reads the hard
checks of the last run of script 1 (`checks/hfc_flags.csv`), the raw export (names, phones, GPS) and the two
baseline exports (`DATA_CTO/baseline-first-round` and `baseline-second-round`, `coordinate-Latitude/Longitude`).
Sheets:

- `backcheck_YYYYMMDD`: households to call, one sheet per run date. Each run adds a sheet; earlier sheets, and
  anything typed into them, are kept (a re-run on the same day replaces that day's sheet). GPS issue
  households, at most 15 per list (`GPS_SAMPLE_MAX`; random, seeded), plus 1 hard-check household per
  enumerator, or 5 if the enumerator has more than 10 hard-check submissions (most checks first, then at
  random, seeded). Numbered (`no`); hard checks first, then GPS issues, each sorted by survey date (newest
  first) and enumerator. Blank columns for the back-checker.
- `enumerators`: `hard_checks_total`, `gps_issues_total`, then one column per survey day for each, and the
  hard-check quota.
- `gps_issues_all` (all days so far): midline `obs_gps` more than 1 km (`GPS_MAX_KM`) + midline accuracy +
  baseline accuracy from **every** baseline GPS point of the household. Accuracy (`obs_gps-Accuracy`,
  `coordinate-Accuracy`) is the phone's own error estimate in metres; a missing accuracy counts as 0.
  `threshold_km` shows the limit used.
- `hard_checks_all` (all days so far): submissions with a hard check, with the household head's name.

The last three are replaced on each run. Before each save, the previous workbook is copied to `checks/!archive/`.

Left out of the call list: households in an earlier `backcheck_YYYYMMDD` sheet or in an earlier
one-file-per-day list (`backcheck_list_YYYYMMDD.xlsx` in `checks/` or `checks/!archive/`, sheet `backcheck`), matched on `hh_id` (the script
stops if one cannot be read), and households with no valid phone number. Valid
number (primary and secondary alike): the preloaded number if the respondent confirmed it, the new number if they
did not, or the number collected when nothing was preloaded; it must be a Rwandan mobile number (07XXXXXXXX).

It reads the raw export, so it is kept out of script 1 (scripts 2-10 only see deidentified data). The workbook
contains names, phone numbers and GPS: never copy it to the folder shared with the firm.

Script 1 needs the packages `tidyverse`, `readxl`, `writexl` and `openxlsx` (`install.packages(c("writexl", "openxlsx"))` if missing).
Warnings like `package 'dplyr' was built under R version 4.4.3` are harmless.

Times: SurveyCTO writes `starttime`, `endtime` and `SubmissionDate` in the time zone of the computer that
downloaded the export (e.g. US Eastern), not Rwanda time. Script 2 reads them in `EXPORT_TZ` (default: this
computer's time zone, set in `1.hfc_main.R`) and converts them to Rwanda time (`TZ`) before the
`fieldwork_start` filter, daily counts and date checks. Run the HFC on the same computer that downloaded the
export, or set `EXPORT_TZ` by hand (e.g. `"America/New_York"`).

Things to check before a live run:
- Script 0 (live) looks for the form `questionnaires/02-midline/REP_questionnaire_SurveyCTO_LIVE.xlsx`,
  while script 1 (live) looks for `REP_questionnaire_SurveyCTO_prep_YYMMDD.xlsx` (6 digits). Use one name for both.
- If only the deidentified preload is available (no raw preload), script 0 stops at the preload step. Put the
  deidentified preload files directly in `deidentified/preload/` (or `pilot/deidentified/preload/`) and run script 1 only.


### (4) Midline dashboard

The output will be saved in below folder in excel sheets titled `HFC_midline_dashboard.xlsx`.

`EAQIP_HFC_shared\datawork\HFC_midline\output` (pilot: `datawork\HFC_midline\pilot\output`)

Only the most up-to-date dashboard will be found under `output` folder and the script will automatically move outdated dashboard to `output\archive` folder.

The folder structure is in step (1). 

## What each script does

Each script adds its tables to `hfc_outputs`. Script 10 writes one tab per table
to the dashboard workbook.

| Script | What it does | Dashboard tabs it creates |
|---|---|---|
| `1.hfc_main.R` | Runner. Sets the mode, paths, config (fieldwork start, duration limits, key-rate targets `LOCATE_TARGET`, `CONSENT_TARGET`, `COMPLETION_TARGET`), the helpers (`add_flags()`, `archive_old()`, `save_current()`), runs scripts 2-10 and saves `hfc_constr.xlsx` and `checks/hfc_flags.csv`. | `run_info` |
| `2.hfc_construct.R` | Loads the SurveyCTO export, parses dates, keeps submissions from the fieldwork start on, sets special codes (-77, -88) to NA in continuous variables, adds enumerator and village names and the treatment arm, computes module durations and `nudge_eligible`, finds duplicate `hh_id`s. Builds `hfc_constr`, the dataset every later script uses. | `duration_data`, `duplicates` |
| `3.hfc_admin.R` | Fieldwork progress against the preload (target households): attempted, located, consented, complete, with `*_pct` rates and submissions per day. The SHS nudge columns (T2/T3) are in the arm table. Also lists households never located. | `village_data`, `district_data`, `treatment_data`, `not_located` |
| `4.hfc_outliers.R` | Outliers in amounts and counts: values beyond ±3 SD of log(1 + x). | `outlier_data` |
| `5.hfc_logic.R` | Hard checks on consented submissions: electricity devices and home phone charging vs. grid electricity use. Soft flags: dates and durations (start and end on different days; upload `UPLOAD_DELAY_DAYS` = 2 or more days after the end, since next-day sync is normal), treatment compliance (readyboard outside T1/T3, nudge outside T2/T3), enumerator observation vs. reported readyboard, wiring and SHS, grid and SHS consistency. One row per submission x issue, with summaries. | `hard_checks`, `hard_checks_summary`, `flags_data`, `flags_summary` |
| `6.hfc_enum.R` | One row per enumerator: submissions per day, locate / consent / completion rates, durations, and counts of duplicates, hard checks, soft flags and outliers. Flag columns are named after their threshold (e.g. `consent_rate_below_95`). | `enumerator_check` |
| `7.hfc_stats.R` | Descriptives for consented households, overall and by arm: key variables (% yes or mean / sd / min / median / max), frequencies of every choice field (special codes kept), and energy access (meter, readyboard, wiring, grid use, SHS, none). | `desc_stats_data`, `desc_cat_data`, `energy_access` |
| `8.hfc_audio.R` | Disabled in the deidentified HFC pipeline; audio is not used in the dashboard or HFC checks. | — |
| `9.hfc_shs.R` | SHS nudge funnel for T2 and T3: households in arm, then attempted, located, consented, eligible, willing (yes / no / not sure), data consent. By arm and by village. | `shs_funnel`, `shs_funnel_village` |
| `10.hfc_dashboard.R` | Writes the workbook: the `Dashboard` tab first, then every table above as a formatted tab. | `Dashboard` |
| `11.hfc_backcheck.R` | Run separately after script 1 (reads the raw export). Back-check workbook: GPS vs. baseline issues, hard checks with names, counts per enumerator, and the households to call. | — (writes `checks/backcheck_list_YYYYMMDD.xlsx`) |

## The `Dashboard` tab

| Block | Source |
|---|---|
| Key rates vs targets | script 3, targets from `1.hfc_main.R` |
| Progress by arm, with the SHS nudge | `treatment_data` (script 3) |
| Progress by district | `district_data` (script 3) |
| Hard checks | `hard_checks_summary` (script 5) |
| Flags (soft checks) | `flags_summary` (script 5) |
| Energy access | `energy_access` (script 7) |
| Key variables | `desc_stats_data` (script 7) |
| Daily submissions chart | `hfc_constr` (script 2) |

Each block has a grey definition row under its column names.

## Tab order

`Dashboard`, `enumerator_check`, `district_data`, `village_data`, `duplicates`,
`hard_checks_summary`, `hard_checks`, `flags_summary`, `flags_data`, then the other tabs, with `run_info` last
(`TAB_FIRST` in `10.hfc_dashboard.R`).

## Output files

In `output_dir` (`SHARED_EAQIP/datawork/HFC_midline/output`, or `pilot/output` / `test/output` in those modes):

| File | Contents |
|---|---|
| `HFC_midline_dashboard.xlsx` | The dashboard workbook (all tabs above) |
| `hfc_constr.xlsx` | The constructed dataset from script 2 |
| `archive/` | Earlier versions of both files, renamed `<name>_YYYYMMDD_HHMM.xlsx` |
| `checks/hfc_flags.csv` | Every flagged row, used by `test/compare_expected_flags.R` and `11.hfc_backcheck.R` |
| `checks/backcheck_list.xlsx` | Back-check workbook from script 11, one `backcheck_YYYYMMDD` sheet per run (names, phone numbers, GPS) |
| `audio_check/<YYYYMMDD>/` | Audio audit list and files |

On the WBG OneDrive (`ONEDRIVE_HFC_ML` in `PATHS.R`: `Rwanda_Energy_Project/EAQIP_survey/HFC_26`),
each run also updates the shared copy that the team and the survey firm open:

| File | Contents |
|---|---|
| `HFC_26/<folder>/HFC_midline_dashboard.xlsx` | The current dashboard (`<folder>` = `test_deidentified`, `pilot_deidentified` or `main_deidentified`) |
| `HFC_26/<folder>/archive/` | Earlier dashboards, renamed `HFC_midline_dashboard_YYYYMMDD_HHMM.xlsx` |

`ONEDRIVE_HFC_ML` is built from each user's `ONEDRIVE` path. On Tanay's computer it currently points to
Tanay's own OneDrive (`Rwanda Energy Project/EAQIP_survey/HFC_26`), not the folder shared with the firm,
so the firm only sees dashboards published from Yeji's computer until that path is changed.

R cannot write to OneDrive directly on WBG laptops (probably an IT restriction), so on Windows the
move and copy run through `cmd` (`share_dashboard()` in `1.hfc_main.R`).

If a file is open in Excel, it cannot be replaced. The script then saves a copy
with the time added to the name and gives a warning.
