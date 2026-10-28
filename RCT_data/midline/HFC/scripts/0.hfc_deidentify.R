# ==============================================================================
# 0.hfc_deidentify.R
# Midline HFC: builds the deidentified inputs that 1.hfc_main.R reads. Run it
# first, and again whenever the export or preload changes.
#
# Allowlist: a survey column is kept only if the form defines it as a number,
# date, choice or calculate field, or as an "other (specify)" text field
# (`*_other`, kept on purpose), and its name is not on the privacy block list.
# Names, phones, GPS, other free text (e.g. comment), audio, photos, text audits
# and device ids are therefore dropped. hh_id and the treatment arm are kept.
# The raw files are only read, never edited.
#
# Run from the repo root:
#   Rscript RCT_data/midline/HFC/scripts/0.hfc_deidentify.R
#
# Author: Yeji (using Claude) · Last modified: 2026-10-27
# ==============================================================================


# ---- 1. Setup ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
})
if (!file.exists("PATHS.R")) stop("Run this script from the repo root (PATHS.R not found).")
source("PATHS.R")
source("RCT_data/midline/HFC/scripts/hfc_privacy.R")
t_start <- Sys.time()


# ---- 2. Config / constants ---------------------------------------------------

HFC_MODE <- "live"                               # "test" or "live" (same as in 1.hfc_main.R)
stopifnot(HFC_MODE %in% c("test", "live"))

FORM_ID   <- "REP_readyboard_test"               # TBD: production form id
ENUM_FILE <- if (HFC_MODE == "test") "test_enumerators.csv" else "enumerators.csv"
form_path <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/0_REP_SurveyCTO_Test_Form.xlsx")

if (HFC_MODE == "test") {
  raw_data_dir    <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/synthetic_data")
  raw_preload_dir <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/synthetic_data/preload")
  deid_dir        <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/deidentified")
} else {
  raw_data_dir    <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/data")
  raw_preload_dir <- file.path(DROPBOX_EAQIP, "questionnaires/02-midline/02-CTO-preload")
  deid_dir        <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/deidentified")
}
deid_preload_dir <- file.path(deid_dir, "preload")

# Preload columns the HFC needs (file written -> source file and columns kept)
PRELOAD_KEEP <- list(
  household_frame.csv = list(from = "contact_info.csv", cols = c("householdid_key", "villageid_key", "treat")),
  shs_eligible.csv    = list(from = "shs_eligible.csv", cols = "householdid_key"),
  scope_villages.csv  = list(from = "scope_villages.csv",
                             cols = c("villageid_key", "districtid_key", "district_key", "sector_key", "cell_key", "village_key")),
  enumerators.csv     = list(from = ENUM_FILE, cols = c("enumeratorid_key", "enumerator_key"))
)


# ---- 3. Survey export --------------------------------------------------------

form <- read_excel(form_path, "survey", col_types = "text", .name_repair = "unique_quiet") |>
  filter(!is.na(type)) |>
  mutate(type = str_squish(type), name = str_squish(name), base_type = word(type, 1))
in_repeat <- cumsum(form$type == "begin repeat") - cumsum(form$type == "end repeat") > 0

form_keep   <- form |>
  filter(base_type %in% HFC_KEEP_TYPES | (base_type == "text" & str_detect(name, HFC_OTHER_REGEX)),
         !str_detect(name, regex(HFC_BLOCK_REGEX, ignore_case = TRUE)))
keep_fields <- c(HFC_META_COLS, form_keep$name)
repeat_keep <- form_keep$name[form_keep$name %in% form$name[in_repeat]]

export_path <- file.path(raw_data_dir, paste0(FORM_ID, "_WIDE.csv"))
header <- names(read_csv(export_path, n_max = 0, col_types = cols(.default = col_character()),
                         show_col_types = FALSE, name_repair = "minimal"))
# Repeat fields appear as <field>_1, <field>_2, ... in the wide export
keep_cols <- header[header %in% keep_fields | str_remove(header, "_[0-9]+$") %in% repeat_keep]

required <- c("KEY", "hh_id", "enumerator", "starttime", "endtime")
if (!all(required %in% keep_cols)) stop("Export is missing: ", paste(setdiff(required, keep_cols), collapse = ", "))
assert_no_identifying_cols(keep_cols, "Deidentified survey export")

survey <- read_csv(export_path, col_select = all_of(keep_cols), col_types = cols(.default = col_character()),
                   show_col_types = FALSE, name_repair = "minimal")
if (anyNA(survey$hh_id) || any(survey$hh_id == "")) stop("Some submissions have no hh_id.")

dir.create(deid_preload_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(survey, file.path(deid_dir, paste0(FORM_ID, "_WIDE.csv")), na = "")
message(sprintf("[1/2] survey export: kept %d of %d columns, %d submissions; dropped %d",
                length(keep_cols), length(header), nrow(survey), length(header) - length(keep_cols)))


# ---- 4. Preload files --------------------------------------------------------

iwalk(PRELOAD_KEEP, \(spec, out_file) {
  x <- read_csv(file.path(raw_preload_dir, spec$from), col_select = all_of(spec$cols),
                col_types = cols(.default = col_character()), show_col_types = FALSE, name_repair = "unique_quiet")
  assert_no_identifying_cols(names(x), out_file)
  write_csv(x, file.path(deid_preload_dir, out_file), na = "")
  message(sprintf("      preload %s: %s", out_file, paste(spec$cols, collapse = ", ")))
})
message(sprintf("[2/2] deidentified inputs in %s (%.1fs)", deid_dir,
                as.numeric(difftime(Sys.time(), t_start, units = "secs"))))
