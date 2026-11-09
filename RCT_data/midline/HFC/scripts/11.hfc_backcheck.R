# ==============================================================================
# 11.hfc_backcheck.R
# Midline HFC: back-check workbook, to catch fake interviews. Four sheets:
#   gps_issues  midline GPS more than GPS_MAX_KM + midline accuracy + baseline accuracy
#               from every baseline GPS point of the household (all days so far)
#   hard_checks submissions with a hard check from 5.hfc_logic.R, with names (all days so far)
#   enumerators GPS issues and hard-check submissions per enumerator, per day and in total
#   backcheck   households to call: every GPS issue household, plus 1 hard-check household
#               per enumerator (HARD_N_HIGH if the enumerator has more than HARD_HIGH_AT
#               hard-check submissions). Households in an earlier list and households with
#               no valid phone number are left out.
#
# Reads the raw export (names, phones, GPS) like 0.hfc_deidentify.R, so it is kept out of
# 1.hfc_main.R: scripts 2-10 only ever see deidentified data. Do not copy the workbook to
# the folder shared with the survey firm. Run it after 1.hfc_main.R, with the same HFC_MODE.
#
# Run from the repo root:
#   Rscript RCT_data/midline/HFC/scripts/11.hfc_backcheck.R
#
# Author: Tanay, Yeji (using Claude) · Last modified: 2026-11-09
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

HFC_MODE <- "live"                     # same as 1.hfc_main.R: "pilot" or "live"
stopifnot(HFC_MODE %in% c("pilot", "live"))

FORM_ID <- "REP_readyboard_test"

TZ        <- "Africa/Kigali"
EXPORT_TZ <- Sys.timezone()            # time zone of the export (see 1.hfc_main.R)

# Same fieldwork start as MODE_CONFIG in 1.hfc_main.R
FIELDWORK_START <- switch(HFC_MODE, pilot = as.Date("2026-10-27"), live = as.Date("2026-10-31"))

MODE_DIR <- switch(HFC_MODE,
                   pilot = file.path(SHARED_EAQIP, "datawork/HFC_midline/pilot"),
                   live  = file.path(SHARED_EAQIP, "datawork/HFC_midline"))

raw_data_dir <- file.path(MODE_DIR, "data")
preload_dir  <- file.path(MODE_DIR, "deidentified/preload")
checks_dir   <- file.path(MODE_DIR, "output/checks")

# Baseline exports with the household GPS (coordinate-Latitude / -Longitude)
BASELINE_EXPORTS <- c(first  = file.path(DATA_CTO, "baseline-first-round", "REP_baseline_test_WIDE.csv"),
                      second = file.path(DATA_CTO, "baseline-second-round", "REP_baseline_test_WIDE.csv"))

# GPS issue: midline GPS further than GPS_MAX_KM + midline accuracy + baseline accuracy from every
# baseline point. Accuracy is the phone's own error estimate (m); a missing accuracy counts as 0.
GPS_MAX_KM     <- 1
GPS_SAMPLE_MAX <- 15     # cap on GPS issue households per list (about 10%; sampled at random, seeded)
HARD_HIGH_AT   <- 10     # enumerators with more hard-check submissions than this ...
HARD_N_HIGH    <- 5      # ... get this many households in the list (others get HARD_N)
HARD_N         <- 1
SEED <- 20261101

TODAY    <- Sys.Date()   # date of the computer running the script (names today's list)
out_path <- file.path(checks_dir, paste0("backcheck_list_", format(TODAY, "%Y%m%d"), ".xlsx"))


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

# The number to call, following the form: the preloaded number if the respondent confirmed it,
# the new number if they did not, or the number collected when nothing was preloaded
valid_phone <- function(pulled, confirm, revised, collected) {
  case_when(confirm %in% "1" ~ clean_phone(pulled),
            confirm %in% "0" ~ clean_phone(revised),
            is.na(clean_phone(pulled)) ~ clean_phone(collected),
            .default = NA_character_)
}

# Great-circle distance in km
haversine_km <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * 6371 * asin(pmin(1, sqrt(a)))
}

# Per enumerator: one column per survey day plus a total, named <prefix>_total, <prefix>_<date>
count_by_day <- function(df, prefix, days) {
  df |>
    count(enumerator, survey_date) |>
    mutate(survey_date = factor(format(survey_date), levels = format(days))) |>
    pivot_wider(names_from = survey_date, values_from = n, values_fill = 0,
                names_prefix = paste0(prefix, "_"), names_expand = TRUE) |>
    mutate(!!paste0(prefix, "_total") := rowSums(across(-enumerator)), .after = enumerator)
}


# ---- 4. Inputs ---------------------------------------------------------------

active_exports <- list.files(raw_data_dir, pattern = paste0("^", FORM_ID, "_WIDE_[0-9]{6}\\.csv$"), full.names = TRUE)
if (length(active_exports) != 1) {
  stop("Expected exactly one ", FORM_ID, "_WIDE_YYMMDD.csv in:\n", raw_data_dir, "\nFound: ", length(active_exports))
}
message("      source export: ", basename(active_exports))

flags_path <- file.path(checks_dir, "hfc_flags.csv")
if (!file.exists(flags_path)) stop("Run 1.hfc_main.R first (", flags_path, " not found).")

raw      <- read_chr(active_exports)
enums    <- read_chr(file.path(preload_dir, "enumerators.csv"))
villages <- read_chr(file.path(preload_dir, "scope_villages.csv"))

# Hard checks of the latest run of script 1 (one row per submission x check)
hard_flags <- read_chr(flags_path) |>
  filter(!is.na(KEY), KEY != "", str_starts(check, "5\\.hfc_logic: hard_"))

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

# Households in earlier lists (files dated before today; today's file is replaced on a re-run)
prev_files <- list.files(checks_dir, pattern = "^backcheck_list_[0-9]{8}\\.xlsx$", full.names = TRUE)
prev_files <- prev_files[as.Date(str_extract(basename(prev_files), "[0-9]{8}"), "%Y%m%d") < TODAY]
already_checked <- map(prev_files, \(f) {
  tryCatch(read_excel(f, sheet = "backcheck", col_types = "text")$hh_id,
           error = \(e) stop("Cannot read earlier list ", basename(f), " (", conditionMessage(e),
                             "). Close it in Excel, or save it without a sensitivity label, and re-run."))
}) |>
  unlist() |> discard(is.na) |> unique()
message(sprintf("      %d earlier list(s); %d households already back-checked", length(prev_files), length(already_checked)))


# ---- 5. Submissions ------------------------------------------------------------

# One row per midline submission in fieldwork, with names, valid phones and location
subs <- raw |>
  mutate(start = parse_export_datetime(starttime), survey_date = as_date(start)) |>
  filter(!is.na(survey_date), survey_date >= FIELDWORK_START) |>
  transmute(KEY, hh_id, enumerator, village, start, survey_date,
            survey_min = round(as.numeric(survey_duration) / 60, 1),
            hh_head_name,
            respondent_name = na_if(str_squish(paste(coalesce(first_name, ""), coalesce(last_name, ""))), ""),
            primary_phone = valid_phone(pull_phonenumber, confirm_phonenumber, phonenumber_revise, phonenumber),
            secondary_phone = valid_phone(pull_second_phonenumber, confirm_second_phonenumber,
                                          second_phonenumber_revise, second_phonenumber),
            ml_lat = as.numeric(`obs_gps-Latitude`), ml_lon = as.numeric(`obs_gps-Longitude`),
            ml_accuracy_m = as.numeric(`obs_gps-Accuracy`)) |>
  left_join(select(enums, enumerator = enumeratorid_key, enumerator_name = enumerator_key), by = "enumerator") |>
  left_join(select(villages, village = villageid_key, district = district_key, sector = sector_key,
                   cell = cell_key, village_name = village_key), by = "village")

fieldwork_days <- seq(FIELDWORK_START, max(subs$survey_date), by = "day")


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


# ---- 7. Hard checks with names -------------------------------------------------

hard_subs <- hard_flags |>
  summarise(n_hard_checks = n_distinct(check), hard_checks = paste(unique(detail), collapse = "; "), .by = KEY) |>
  inner_join(subs, by = "KEY") |>
  left_join(select(gps_issues, KEY, distance_km), by = "KEY")


# ---- 8. Per enumerator ---------------------------------------------------------

enum_counts <- subs |>
  distinct(enumerator, enumerator_name) |>
  left_join(count_by_day(gps_issues, "gps_issues", fieldwork_days), by = "enumerator") |>
  left_join(count_by_day(hard_subs, "hard_checks", fieldwork_days), by = "enumerator") |>
  mutate(across(-c(enumerator, enumerator_name), \(x) coalesce(x, 0)),
         hard_quota = if_else(hard_checks_total > HARD_HIGH_AT, HARD_N_HIGH, HARD_N))


# ---- 9. Back-check sample ------------------------------------------------------

# Callable: a household ID, not in an earlier list, and at least one valid phone number
callable <- \(df) df |>
  filter(!is.na(hh_id), !hh_id %in% already_checked, !is.na(primary_phone) | !is.na(secondary_phone))

set.seed(SEED)

# Every GPS issue household (one submission per household: the furthest)
gps_pool <- gps_issues |> callable() |> slice_max(distance_km, n = 1, with_ties = FALSE, by = hh_id)
gps_pick <- if (nrow(gps_pool) > GPS_SAMPLE_MAX) slice_sample(gps_pool, n = GPS_SAMPLE_MAX) else gps_pool

# Hard checks: HARD_N or HARD_N_HIGH households per enumerator; most checks first, then at random
hard_pick <- hard_subs |>
  callable() |>
  filter(!hh_id %in% gps_pick$hh_id) |>
  left_join(select(enum_counts, enumerator, hard_quota), by = "enumerator") |>
  mutate(tiebreak = runif(n())) |>
  arrange(enumerator, desc(n_hard_checks), tiebreak) |>
  distinct(hh_id, .keep_all = TRUE) |>
  filter(row_number() <= first(hard_quota), .by = enumerator)

short_enums <- hard_pick |> count(enumerator, name = "picked") |>
  right_join(filter(enum_counts, hard_checks_total > 0), by = "enumerator") |>
  filter(coalesce(picked, 0L) < pmin(hard_quota, hard_checks_total))

backcheck <- bind_rows(
  mutate(gps_pick, reason = "GPS far from baseline"),
  mutate(hard_pick, reason = "Hard check")
) |>
  select(-any_of(c("hard_checks", "n_hard_checks"))) |>
  left_join(select(hard_subs, KEY, hard_checks), by = "KEY")


# ---- 10. Write -----------------------------------------------------------------

id_cols <- \(df) df |>
  transmute(survey_date = format(survey_date), survey_start_rwanda = format(start, "%H:%M"), survey_min,
            enumerator_id = as.integer(enumerator), enumerator_name, district, sector, cell, village = village_name,
            hh_id, hh_head_name, respondent_name, KEY, pick(any_of(c("primary_phone", "secondary_phone"))))

sheets <- list(
  gps_issues = gps_issues |>
    arrange(desc(distance_km)) |>
    (\(d) bind_cols(id_cols(d), transmute(d, distance_km, threshold_km, ml_lat, ml_lon, ml_accuracy_m,
                                          baseline_round, bl_lat, bl_lon, bl_accuracy_m)))(),
  hard_checks = hard_subs |>
    arrange(survey_date, enumerator) |>
    (\(d) bind_cols(id_cols(d), transmute(d, n_hard_checks, hard_checks, gps_distance_km = distance_km)))(),
  enumerators = enum_counts |>
    mutate(enumerator_id = as.integer(enumerator), .before = 1) |>
    select(-enumerator) |>
    arrange(desc(hard_checks_total + gps_issues_total), enumerator_id),
  backcheck = backcheck |>
    arrange(reason != "GPS far from baseline", enumerator, survey_date) |>
    (\(d) bind_cols(transmute(d, reason), id_cols(d),
                    transmute(d, hard_checks, gps_distance_km = distance_km,
                              call_date = "", reached = "", respondent_confirms_interview = "", notes = "")))()
)

wb <- createWorkbook()
head_style <- createStyle(textDecoration = "bold", fgFill = "#DDEBF7")
for (nm in names(sheets)) {
  df <- sheets[[nm]]
  addWorksheet(wb, nm)
  writeData(wb, nm, df, headerStyle = head_style)
  # Keep the leading 0 of phone numbers and IDs in Excel
  txt <- match(intersect(c("hh_id", "primary_phone", "secondary_phone"), names(df)), names(df))
  if (length(txt) > 0 && nrow(df) > 0) {
    addStyle(wb, nm, createStyle(numFmt = "@"), rows = seq_len(nrow(df)) + 1, cols = txt, gridExpand = TRUE)
  }
  setColWidths(wb, nm, seq_along(df), "auto")
  freezePane(wb, nm, firstRow = TRUE)
}
dir.create(checks_dir, recursive = TRUE, showWarnings = FALSE)
saveWorkbook(wb, out_path, overwrite = TRUE)

message(sprintf("[11.hfc_backcheck] gps_issues %d | hard_checks %d submissions | backcheck %d households (%d GPS, %d hard check; %d enumerators at %d) -> %s",
                nrow(sheets$gps_issues), nrow(sheets$hard_checks), nrow(backcheck), nrow(gps_pick), nrow(hard_pick),
                sum(enum_counts$hard_quota == HARD_N_HIGH & enum_counts$hard_checks_total > 0), HARD_N_HIGH, out_path))
if (nrow(gps_pool) > nrow(gps_pick)) message(sprintf("      GPS issue households sampled: %d of %d", nrow(gps_pick), nrow(gps_pool)))
if (nrow(short_enums) > 0) {
  warning(nrow(short_enums), " enumerator(s) got fewer hard-check households than their quota ",
          "(already back-checked or no valid phone): enumerator ", paste(short_enums$enumerator, collapse = ", "), ".")
}
