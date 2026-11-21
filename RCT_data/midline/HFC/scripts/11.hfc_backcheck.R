# ==============================================================================
# 11.hfc_backcheck.R
# Midline HFC: back-check master list (checks/backcheck_list.xlsx).
# hard_checks and not_located reproduce the dashboard rows with contact and GPS details.
# gps_issues is recalculated from the current midline and baseline exports each run.
# enumerator and summary show counts and overlaps by submission KEY.
# backchecked / backcheck-date are carried over from the existing workbook; they are set by
# 11b.hfc_backcheck_daily.R, which samples the daily list from this master.
# Before each save the previous workbook is copied to checks/!archive (as backcheck_list_YYYYMMDD_HHMM.xlsx).
#
# Reads the raw export (names, phones, GPS) like 0.hfc_deidentify.R, so it is kept out of
# 1.hfc_main.R: scripts 2-10 only ever see deidentified data. Do not copy the workbook to
# the folder shared with the survey firm. Run it after 1.hfc_main.R, with the same HFC_MODE.
#
# Run from the repo root:
#   Rscript RCT_data/midline/HFC/scripts/11.hfc_backcheck.R
#
# Author: Tanay, Yeji (using Claude) · Last modified: 2026-11-19
# ==============================================================================


# ---- 1. Setup ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(openxlsx)
})
if (!file.exists("PATHS.R")) stop("Run this script from the repo root (PATHS.R not found).")
source("PATHS.R")


# ---- 2. Config / constants ---------------------------------------------------

HFC_MODE <- "mop-up"                   # same as 1.hfc_main.R: "pilot", "live", or "mop-up"
stopifnot(HFC_MODE %in% c("pilot", "live", "mop-up"))

FORM_ID <- "REP_readyboard_test"

TZ        <- "Africa/Kigali"
EXPORT_TZ <- Sys.timezone()            # time zone of the export (see 1.hfc_main.R)

# Same fieldwork start as MODE_CONFIG in 1.hfc_main.R
FIELDWORK_START <- switch(
  HFC_MODE,
  pilot = as.Date("2026-10-27"),
  live = as.Date("2026-10-31"),
  `mop-up` = as.Date("2026-11-20")
)

MODE_DIR <- switch(HFC_MODE,
                   pilot = file.path(SHARED_EAQIP, "datawork/HFC_midline/pilot"),
                   live  = file.path(SHARED_EAQIP, "datawork/HFC_midline"),
                   `mop-up` = file.path(SHARED_EAQIP, "datawork/HFC_midline"))

DROPBOX_MODE_DIR <- switch(HFC_MODE,
                           pilot = file.path(DROPBOX_EAQIP, "datawork/HFC_midline/pilot"),
                           live = file.path(DROPBOX_EAQIP, "datawork/HFC_midline"),
                           `mop-up` = file.path(DROPBOX_EAQIP, "datawork/HFC_midline"))
DROPBOX_CHECKS_DIR <- file.path(DROPBOX_MODE_DIR, "output/checks")

raw_data_dir <- switch(HFC_MODE,
           pilot = file.path(MODE_DIR, "data"),
           live = file.path(MODE_DIR, "data"),
           `mop-up` = file.path(MODE_DIR, "mop-up-data"))
preload_dir <- switch(HFC_MODE,
          pilot = file.path(MODE_DIR, "deidentified/preload"),
          live = file.path(MODE_DIR, "deidentified/preload"),
          `mop-up` = file.path(MODE_DIR, "mop-up-deidentified/preload"))
output_dir <- switch(HFC_MODE,
         pilot = file.path(MODE_DIR, "output"),
         live = file.path(MODE_DIR, "output"),
         `mop-up` = file.path(MODE_DIR, "mop-up-output"))
raw_preload_dir <- switch(HFC_MODE,
                          pilot = file.path(SHARED_EAQIP, "questionnaires/02-midline/02-CTO-preload/pilot"),
           live = file.path(SHARED_EAQIP, "questionnaires/02-midline/02-CTO-preload"),
                          `mop-up` = file.path(SHARED_EAQIP, "questionnaires/02-midline/02-CTO-preload/mop-up"))
checks_dir <- file.path(output_dir, "checks")

# Baseline exports with the household GPS (coordinate-Latitude / -Longitude)
BASELINE_EXPORTS <- c(first  = file.path(DATA_CTO, "baseline-first-round", "REP_baseline_test_WIDE.csv"),
                      second = file.path(DATA_CTO, "baseline-second-round", "REP_baseline_test_WIDE.csv"))

# GPS issue: midline GPS further than GPS_MAX_KM + midline accuracy + baseline accuracy from every
# baseline point. Accuracy is the phone's own error estimate (m); a missing accuracy counts as 0.
GPS_MAX_KM     <- 1
out_path    <- file.path(checks_dir, "backcheck_list.xlsx")
archive_dir <- file.path(checks_dir, "!archive")
SUMMARY_SHEETS <- c("hard_checks", "not_located", "gps_issues", "enumerator", "summary")


# ---- 3. Helper functions -----------------------------------------------------

read_chr <- \(path, ...) read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE, ...)

# Export timestamps (Korean AM/PM markers or English) in EXPORT_TZ, as Rwanda time (as in 2.hfc_construct.R)
parse_export_datetime <- function(x) {
  x <- str_replace_all(str_squish(x), c("오전" = "AM", "오후" = "PM"))
  with_tz(parse_date_time(x, orders = c("ymd p IMS", "mdy IMS p", "ymd HMS"), locale = "C",
                          tz = EXPORT_TZ, quiet = TRUE), TZ)
}

# Phone number as 07XXXXXXXX, or NA if it is not a valid Rwandan mobile number
clean_phone <- function(x) {
  d <- str_remove_all(coalesce(x, ""), "[^0-9]")
  d <- str_replace(d, "^250", "0")
  d <- if_else(str_detect(d, "^7[0-9]{8}$"), paste0("0", d), d)
  if_else(str_detect(d, "^07[0-9]{8}$"), d, NA_character_)
}

valid_phone <- function(pulled, confirm, revised) {
  if_else(str_detect(coalesce(pulled, ""), "^[0-9]+$") &
            !is.na(clean_phone(pulled)) & confirm %in% "1",
          clean_phone(pulled), revised)
}

has_phone <- function(primary, secondary) {
  nzchar(str_trim(coalesce(primary, ""))) | nzchar(str_trim(coalesce(secondary, "")))
}

# Great-circle distance in km
haversine_km <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * 6371 * asin(pmin(1, sqrt(a)))
}

unreadable <- \(what) \(e) stop("Cannot read ", what, " (", conditionMessage(e), "). Close it in Excel, ",
                                "check its access permissions, and re-run.")

copy_to_shared <- function(source, destination) {
  status <- system2(
    "cmd",
    c("/c", "copy", "/Y", shQuote(normalizePath(source, mustWork = TRUE), type = "cmd"),
      shQuote(normalizePath(destination, mustWork = FALSE), type = "cmd")),
    stdout = FALSE,
    stderr = FALSE
  )
  status == 0
}

ensure_shared_dir <- function(path) {
  if (dir.exists(path)) return(TRUE)
  status <- system2("cmd", c("/c", "mkdir", shQuote(normalizePath(path, mustWork = FALSE), type = "cmd")),
                    stdout = FALSE, stderr = FALSE)
  status == 0
}


# ---- 4. Inputs ---------------------------------------------------------------

active_exports <- list.files(raw_data_dir, pattern = paste0("^", FORM_ID, "_WIDE_[0-9]{6}\\.csv$"), full.names = TRUE)
if (length(active_exports) != 1) {
  stop("Expected exactly one ", FORM_ID, "_WIDE_YYMMDD.csv in:\n", raw_data_dir, "\nFound: ", length(active_exports))
}
message("      source export: ", basename(active_exports))

# Back-check dates already stamped in the existing master (one date per KEY across sheets)
prior_keys <- tibble(KEY = character(), backcheck_date = character())
if (file.exists(out_path)) {
  prior_keys <- map(intersect(c("hard_checks", "not_located", "gps_issues"),
                              tryCatch(excel_sheets(out_path), error = unreadable(basename(out_path)))),
                    \(tab) read_excel(out_path, sheet = tab, col_types = "text")) |>
    map(\(d) if (all(c("KEY", "backcheck-date") %in% names(d))) transmute(d, KEY, backcheck_date = `backcheck-date`)) |>
    list_rbind() |>
    filter(!is.na(KEY), !is.na(backcheck_date)) |>
    arrange(backcheck_date) |>
    distinct(KEY, .keep_all = TRUE)
}

dashboard_path <- if (HFC_MODE == "mop-up") {
  file.path(DROPBOX_MODE_DIR, "mop-up-output", "HFC_midline_dashboard.xlsx")
} else {
  file.path(output_dir, "HFC_midline_dashboard.xlsx")
}
if (!file.exists(dashboard_path)) stop("Run 1.hfc_main.R first (", dashboard_path, " not found).")
dashboard_hard <- read_excel(dashboard_path, sheet = "hard_checks")
dashboard_not_located <- read_excel(dashboard_path, sheet = "not_located")

raw      <- read_chr(active_exports)
enums    <- read_chr(file.path(preload_dir, "enumerators.csv"))
village_file <- if (HFC_MODE == "mop-up") "mop-up-scope-villages.csv" else "scope_villages.csv"
villages <- read_chr(file.path(raw_preload_dir, village_file))
households <- read_chr(file.path(preload_dir, "household_frame.csv"))
contact_phones <- if (HFC_MODE == "mop-up") {
  read_chr(file.path(raw_preload_dir, "mop-up-contact-information.csv"),
           col_select = c(householdid_key, primary_phone, secondary_phone)) |>
    transmute(hh_id = householdid_key, preload_primary = primary_phone, preload_secondary = secondary_phone)
} else {
  read_chr(file.path(raw_preload_dir, "contact_info.csv"),
           col_select = c(householdid_key, phone_clean, phone2_clean)) |>
    transmute(hh_id = householdid_key, preload_primary = phone_clean, preload_secondary = phone2_clean)
}
if (anyNA(contact_phones$hh_id) || anyDuplicated(contact_phones$hh_id)) stop("Missing or duplicate household IDs in the phone preload.")
if (!"comment" %in% names(raw)) raw$comment <- NA_character_

# Baseline GPS points (both rounds; a household can have several)
missing_bl <- BASELINE_EXPORTS[!file.exists(BASELINE_EXPORTS)]
if (length(missing_bl) > 0) stop("Baseline export(s) not found:\n", paste(missing_bl, collapse = "\n"))
baseline_gps <- imap(BASELINE_EXPORTS, \(p, round) {
  read_chr(p, col_select = c(hh_id, `coordinate-Latitude`, `coordinate-Longitude`, `coordinate-Accuracy`)) |>
    transmute(hh_id, baseline_round = round,
              bl_lat = as.numeric(`coordinate-Latitude`), bl_lon = as.numeric(`coordinate-Longitude`),
              bl_accuracy_m = as.numeric(`coordinate-Accuracy`))
}) |>
  list_rbind() |>
  filter(!is.na(hh_id), !is.na(bl_lat), !is.na(bl_lon), bl_lat != 0, bl_lon != 0)

# ---- 5. Submissions ------------------------------------------------------------

# One row per midline submission in fieldwork, with names, valid phones and location
subs <- raw |>
  mutate(start = parse_export_datetime(starttime), survey_date = as_date(start),
         submission_date = as_date(parse_export_datetime(SubmissionDate), tz = TZ)) |>
  filter(!is.na(survey_date), survey_date >= FIELDWORK_START) |>
  transmute(KEY, hh_id, enumerator, village, start, survey_date, submission_date,
            survey_min = round(as.numeric(survey_duration) / 60, 1),
            hh_head_name, comment,
            primary_phone = valid_phone(pull_phonenumber, confirm_phonenumber, phonenumber_revise),
            secondary_phone = valid_phone(pull_second_phonenumber, confirm_second_phonenumber,
                                          second_phonenumber_revise),
            ml_lat = as.numeric(`obs_gps-Latitude`), ml_lon = as.numeric(`obs_gps-Longitude`),
            ml_accuracy_m = as.numeric(`obs_gps-Accuracy`)) |>
  left_join(select(enums, enumerator = enumeratorid_key, enumerator_name = enumerator_key), by = "enumerator") |>
  left_join(select(villages, village = villageid_key, district = district_key, sector = sector_key,
                   cell = cell_key, village_name = village_key,
                   district_id = districtid_key, sector_id = sectorid_key,
                   cell_id = cellid_key, village_id = villageid_key), by = "village") |>
  left_join(select(households, hh_id = householdid_key, treat), by = "hh_id", relationship = "many-to-one")

if (anyNA(subs$KEY) || anyDuplicated(subs$KEY)) stop("Missing or duplicate submission KEYs in the raw export.")
if (any(!dashboard_hard$KEY %in% subs$KEY) || any(!dashboard_not_located$KEY %in% subs$KEY)) {
  stop("Dashboard submissions do not match the current raw export. Re-run the HFC before the backcheck.")
}


# ---- 6. GPS: midline vs. closest baseline point --------------------------------

gps_compare <- subs |>
  filter(!is.na(ml_lat), !is.na(ml_lon), ml_lat != 0, ml_lon != 0) |>
  select(KEY, hh_id, ml_lat, ml_lon, ml_accuracy_m) |>
  inner_join(baseline_gps, by = "hh_id", relationship = "many-to-many") |>
  mutate(distance_km = round(haversine_km(ml_lat, ml_lon, bl_lat, bl_lon), 2),
         threshold_km = round(GPS_MAX_KM + (coalesce(ml_accuracy_m, 0) + coalesce(bl_accuracy_m, 0)) / 1000, 2),
         excess_km = distance_km - threshold_km) |>
  # The baseline point that fits best (smallest distance beyond its threshold)
  slice_min(excess_km, n = 1, with_ties = FALSE, by = KEY)

gps_issues <- gps_compare |>
  filter(excess_km > 0) |>
  select(KEY, distance_km, threshold_km, baseline_round, bl_lat, bl_lon, bl_accuracy_m) |>
  inner_join(subs, by = "KEY")

n_no_ml_gps <- sum(is.na(subs$ml_lat) | subs$ml_lat %in% 0)
n_no_bl_gps <- sum(!subs$KEY %in% gps_compare$KEY) - n_no_ml_gps
message(sprintf("      GPS: %d submissions compared, %d more than %g km + accuracy from baseline; not compared: %d no midline GPS, %d no baseline GPS",
                nrow(gps_compare), nrow(gps_issues), GPS_MAX_KM, n_no_ml_gps, n_no_bl_gps))


# ---- 7. Back-check tables -----------------------------------------------------

contacts <- subs |> select(KEY, hh_head_name, primary_phone, secondary_phone)
hard_rows <- dashboard_hard |>
  left_join(contacts, by = "KEY", relationship = "many-to-one") |>
  left_join(subs |> select(KEY, sector_key = sector, cell_key = cell,
            district_id, sector_id, cell_id, village_id),
       by = "KEY", relationship = "many-to-one") |>
  mutate(submissiondate = as.Date(submissiondate), gps_issue = as.integer(KEY %in% gps_issues$KEY)) |>
  select(KEY, submissiondate, district_key, sector_key, cell_key, village_key,
    district_id, sector_id, cell_id, village_id, treat, hh_id, hh_head_name,
         enumerator, enumerator_key, primary_phone, secondary_phone, issue, gps_issue, comment)
not_located_rows <- dashboard_not_located |>
  left_join(contacts, by = "KEY", relationship = "many-to-one") |>
  left_join(subs |> select(KEY, district_id, sector_id, cell_id, village_id),
       by = "KEY", relationship = "many-to-one") |>
  left_join(contact_phones, by = "hh_id", relationship = "many-to-one") |>
  mutate(submissiondate = as.Date(attempt_date),
         primary_phone = preload_primary, secondary_phone = preload_secondary) |>
  select(KEY, submissiondate, district_key, sector_key, cell_key, village_key,
    district_id, sector_id, cell_id, village_id, treat, hh_id, hh_head_name,
         enumerator, enumerator_key, primary_phone, secondary_phone, nolocate, nolocate_label, nolocate_other)
gps_rows <- gps_issues |>
  arrange(desc(distance_km)) |>
  transmute(KEY, submission_date, district_key = district, sector_key = sector, cell_key = cell,
       village_key = village_name, district_id, sector_id, cell_id, village_id,
       treat, hh_id, hh_head_name, enumerator, enumerator_key = enumerator_name,
            primary_phone, secondary_phone, distance_km, comment)

enum_counts <- subs |>
  distinct(enumerator, enumerator_name) |>
  left_join(count(dashboard_hard, enumerator, name = "n_hard_checks"), by = "enumerator") |>
  left_join(count(dashboard_not_located, enumerator, name = "n_not_located"), by = "enumerator") |>
  left_join(count(gps_issues, enumerator, name = "n_gps_issues"), by = "enumerator") |>
  mutate(across(c(n_hard_checks, n_not_located, n_gps_issues), \(x) coalesce(x, 0L))) |>
  arrange(as.numeric(enumerator))

summarise_issues <- function(hard, not_located, gps, population) {
  tibble(
    population = population,
    issue = c("hard_checks", "not_located", "gps_issues"),
    total = c(nrow(hard), nrow(not_located), nrow(gps)),
    with_gps_issue = c(sum(hard$KEY %in% gps$KEY), sum(not_located$KEY %in% gps$KEY), NA_integer_),
    without_gps_issue = c(sum(!hard$KEY %in% gps$KEY), sum(!not_located$KEY %in% gps$KEY), NA_integer_),
    gps_not_compared = c(sum(!hard$KEY %in% gps_compare$KEY), sum(!not_located$KEY %in% gps_compare$KEY), NA_integer_),
    from_hard_checks = c(NA_integer_, NA_integer_, sum(gps$KEY %in% hard$KEY)),
    from_not_located_only = c(NA_integer_, NA_integer_, sum(gps$KEY %in% not_located$KEY & !gps$KEY %in% hard$KEY)),
    standalone = c(NA_integer_, NA_integer_, sum(!gps$KEY %in% hard$KEY & !gps$KEY %in% not_located$KEY))
  )
}
summary_rows <- bind_rows(
  summarise_issues(hard_rows, not_located_rows, gps_rows, "all"),
  summarise_issues(filter(hard_rows, has_phone(primary_phone, secondary_phone)),
                   filter(not_located_rows, has_phone(primary_phone, secondary_phone)),
                   filter(gps_rows, has_phone(primary_phone, secondary_phone)), "has_phone")
)

mark_backchecked <- function(data) {
  data |>
    left_join(prior_keys, by = "KEY", relationship = "many-to-one") |>
    mutate(backchecked = if_else(is.na(backcheck_date), "NO", "YES"), `backcheck-date` = backcheck_date) |>
    select(-backcheck_date)
}
hard_rows <- mark_backchecked(hard_rows)
not_located_rows <- mark_backchecked(not_located_rows)
gps_rows <- mark_backchecked(gps_rows)

sheets <- list(
  hard_checks = hard_rows,
  not_located = not_located_rows,
  gps_issues = gps_rows,
  enumerator = enum_counts,
  summary = summary_rows
)

# ---- 8. Update the master workbook -------------------------------------------

head_style <- createStyle(textDecoration = "bold", fgFill = "#DDEBF7")

if (file.exists(out_path)) {
  wb <- tryCatch(loadWorkbook(out_path), error = unreadable(basename(out_path)))
} else {
  wb <- createWorkbook()
}

for (nm in names(sheets)) {
  df <- sheets[[nm]]
  if ("comment" %in% names(df)) df <- df |> mutate(comment = str_replace_all(comment, "\r\n", "\n"))
  if (nm %in% names(wb)) removeWorksheet(wb, nm)
  addWorksheet(wb, nm)
  writeData(wb, nm, df, headerStyle = head_style)
  txt <- match(intersect(c("hh_id", "primary_phone", "secondary_phone"), names(df)), names(df))
  if (length(txt) > 0 && nrow(df) > 0) {
    addStyle(wb, nm, createStyle(numFmt = "@"), rows = seq_len(nrow(df)) + 1, cols = txt, gridExpand = TRUE)
  }
  setColWidths(wb, nm, seq_along(df), "auto")
  freezePane(wb, nm, firstRow = TRUE)
}

for (nm in intersect(c("hard_checks_all", "enumerators", "gps_issues_all"), names(wb))) removeWorksheet(wb, nm)
sheet_order <- c(SUMMARY_SHEETS, setdiff(names(wb), SUMMARY_SHEETS))
worksheetOrder(wb) <- match(sheet_order, names(wb))
activeSheet(wb) <- "summary"

dir.create(checks_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(DROPBOX_CHECKS_DIR, recursive = TRUE, showWarnings = FALSE)
pool_candidate <- tempfile(pattern = "backcheck_pool_", tmpdir = DROPBOX_CHECKS_DIR, fileext = ".xlsx")
tryCatch(saveWorkbook(wb, pool_candidate, overwrite = TRUE),
         error = \(e) stop("Could not build the main backcheck workbook in Dropbox (", conditionMessage(e), ")."))

if (file.exists(out_path)) {
  if (!ensure_shared_dir(archive_dir)) stop("The main workbook archive directory is unavailable.")
  backup <- file.path(archive_dir, paste0("backcheck_list_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
  if (!copy_to_shared(out_path, backup)) {
    stop("The main workbook could not be archived. Pool candidate remains at ", pool_candidate)
  }
}
if (!copy_to_shared(pool_candidate, out_path)) {
  stop("The main workbook could not be updated. Pool candidate remains at ", pool_candidate)
}
unlink(pool_candidate)

message(sprintf("[11.hfc_backcheck] %d hard checks, %d not located, %d GPS issues -> %s",
                nrow(hard_rows), nrow(not_located_rows), nrow(gps_rows), out_path))
