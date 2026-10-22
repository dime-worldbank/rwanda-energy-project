# rwanda-energy-project

Research code for the Rwanda energy RCT (R-EAQIP) and related analyses. Code lives in this repo;
data lives in Dropbox or the WBG OneDrive.

## Folder instructions

Folder-specific rules live in `AGENTS.md` files. Before working on files in a folder, read the
nearest `AGENTS.md` in that folder or its parents and follow it. Where it is more specific than
this file, it wins.

## Data boundaries

- Never write data files (real or synthetic) into this repo. Data goes to Dropbox or the WBG OneDrive.
- Identifiable data (names, phone numbers, NID, GPS, photos, audio) may be stored only in Dropbox and
  the WBG OneDrive (both approved). Never anywhere else (repo, other cloud services, email, chat tools).
- Do not read or open raw data without the researcher's explicit permission. Reading form
  definitions and column names is fine when a task asks for it.
- Never edit `RCT_data/baseline/`. Copy from it instead.

## Environment

- R is not on the PATH. Run scripts from the repo root with:
  `& "C:\Users\wb636130\AppData\Local\Programs\R\R-4.5.2\bin\x64\Rscript.exe" <script.R>`
- Paths come from `PATHS.R` (`source("PATHS.R")` from the repo root). No hard-coded user paths.

## Conventions

- R, tidyverse, native pipe `|>`.
- Show a plan and wait for approval before multi-file tasks.
- Ask before installing packages or deleting files.
