# Aggregator Survey — Descriptive Statistics

Reproducible R analysis of the three main Aggregator survey waves
(Baseline, Midline, Endline). All raw data lives in the sibling
folders (`../Baseline/`, `../Midline/`, `../Endline/`, `../Sampling/`)
and is read read-only.

## Project layout

```
analysis/
├── aggregator.Rproj   open this in RStudio first
├── main.R             entry point — sources everything in R/
├── R/
│   ├── 00_setup.R     packages and paths
│   ├── 01_load.R      read the 3 main wave CSVs
│   ├── 02_clean.R     light type coercion, wave factor
│   ├── 03_descriptives.R   build all tables and figures
│   └── 04_export.R    write outputs to output/
├── output/
│   ├── tables/        CSV + HTML
│   └── figures/       PNG
└── renv.lock          pinned package versions (created by renv::init)
```

## How to reproduce

1. Open `aggregator.Rproj` in RStudio (or `setwd()` into this folder).
2. First time only — restore the pinned packages:
   ```r
   install.packages("renv")
   renv::restore()
   ```
   If `renv.lock` is not yet committed, run `renv::init()` instead; that
   will install the packages listed in `R/00_setup.R` and create the
   lockfile.
3. Run the whole pipeline:
   ```r
   source("main.R")
   ```
4. Outputs land in `output/tables/` and `output/figures/`.

## Scope

Version 1 covers only the three main wave CSVs (one row per aggregator
interview). The repeat-group child files (`*-last_trip_crops.csv`,
`*-crop_sales.csv`, `*-last_trips.csv`, `*-crops_transport.csv`) and
the link to `Sampling/forms/aggregator_assignment.csv` are left as
TODOs in `R/01_load.R` for a follow-up pass.

## Expected row counts

| Wave     | Submissions |
|----------|-------------|
| Baseline | 379         |
| Midline  | 359         |
| Endline  | 367         |
