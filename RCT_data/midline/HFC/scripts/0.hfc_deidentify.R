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
# Author: Yeji (using Claude) · Last modified: 2026-10-29
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

HFC_MODE <- "live"                   # "test", "pilot (Oct 27, 28)", or "live (Oct 29 -) "
stopifnot(HFC_MODE %in% c("test", "pilot", "live"))

FORM_ID <- "REP_readyboard_test"

MODE_CONFIG <- switch(
  HFC_MODE,
  
  test = list(
    enum_file = "test_enumerators.csv",
    
    form_path = file.path(
      DROPBOX_EAQIP,
      "datawork/HFC_midline/test/0_REP_SurveyCTO_Test_Form.xlsx"
    ),
    
    raw_data_dir = file.path(
      DROPBOX_EAQIP,
      "datawork/HFC_midline/test/synthetic_data"
    ),
    
    raw_preload_dir = file.path(
      DROPBOX_EAQIP,
      "datawork/HFC_midline/test/synthetic_data/preload"
    ),
    
    deid_dir = file.path(
      DROPBOX_EAQIP,
      "datawork/HFC_midline/test/deidentified"
    ),
    
    versioned_export = FALSE
  ),
  
  pilot = list(
    enum_file = "test_enumerators.csv",
    
    form_path = file.path(
      DROPBOX_EAQIP,
      paste0(
        "questionnaires/02-midline/!-archived-survey/",
        "REP_questionnaire_SurveyCTO_26102027.xlsx"
      )
    ),
    
    raw_data_dir = file.path(
      DROPBOX_EAQIP,
      "datawork/HFC_midline/pilot/data"
    ),
    
    raw_preload_dir = file.path(
      DROPBOX_EAQIP,
      "questionnaires/02-midline/02-CTO-preload/pilot"
    ),
    
    deid_dir = file.path(
      DROPBOX_EAQIP,
      "datawork/HFC_midline/pilot/deidentified"
    ),
    
    versioned_export = TRUE
  ),
  
  live = list(
    enum_file = "enumerators.csv",
    
    # Update after the final live form definition is cleaned
    form_path = file.path(
      DROPBOX_EAQIP,
      paste0(
        "questionnaires/02-midline/",
        "REP_questionnaire_SurveyCTO_LIVE.xlsx"
      )
    ),
    
    raw_data_dir = file.path(
      DROPBOX_EAQIP,
      "datawork/HFC_midline/data"
    ),
    
    raw_preload_dir = file.path(
      DROPBOX_EAQIP,
      "questionnaires/02-midline/02-CTO-preload"
    ),
    
    deid_dir = file.path(
      DROPBOX_EAQIP,
      "datawork/HFC_midline/deidentified"
    ),
    
    versioned_export = TRUE
  )
)

ENUM_FILE      <- MODE_CONFIG$enum_file
form_path      <- MODE_CONFIG$form_path
raw_data_dir   <- MODE_CONFIG$raw_data_dir
raw_preload_dir <- MODE_CONFIG$raw_preload_dir
deid_dir       <- MODE_CONFIG$deid_dir

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

# Read the form definition used for the selected mode
if (!file.exists(form_path)) {
  stop("Form definition not found:\n", form_path)
}

message("      form definition: ", basename(form_path))

form <- read_excel(
  form_path,
  sheet = "survey",
  col_types = "text",
  .name_repair = "unique_quiet"
) |>
  filter(!is.na(type)) |>
  mutate(
    type = str_squish(type),
    name = str_squish(name),
    base_type = word(type, 1)
  )

# Identify fields inside repeat groups
in_repeat <- (
  cumsum(form$type == "begin repeat") -
    cumsum(form$type == "end repeat")
) > 0

# Keep only allowed field types and permitted "other specify" text fields
form_keep <- form |>
  filter(
    base_type %in% HFC_KEEP_TYPES |
      (
        base_type == "text" &
          str_detect(name, HFC_OTHER_REGEX)
      ),
    !str_detect(
      name,
      regex(HFC_BLOCK_REGEX, ignore_case = TRUE)
    )
  )

keep_fields <- unique(c(
  HFC_META_COLS,
  form_keep$name
))

# Repeat fields appear as <field>_1, <field>_2, etc. in a WIDE export
repeat_keep <- form_keep$name[
  form_keep$name %in% form$name[in_repeat]
]


# ---- 3.1 Locate active SurveyCTO WIDE export ---------------------------------

if (!MODE_CONFIG$versioned_export) {
  
  # Synthetic test export keeps its unversioned filename
  export_path <- file.path(
    raw_data_dir,
    paste0(FORM_ID, "_WIDE.csv")
  )
  
  if (!file.exists(export_path)) {
    stop(
      "Test WIDE export not found:\n",
      export_path
    )
  }
  
} else {
  
  # Pilot/live export naming:
  # REP_readyboard_test_WIDE_YYMMDD.csv
  export_pattern <- paste0(
    "^",
    FORM_ID,
    "_WIDE_[0-9]{6}\\.csv$"
  )
  
  active_exports <- list.files(
    raw_data_dir,
    pattern = export_pattern,
    full.names = TRUE,
    recursive = FALSE
  )
  
  if (length(active_exports) == 0) {
    stop(
      "No active WIDE export found in:\n",
      raw_data_dir,
      "\nExpected filename like:\n",
      FORM_ID,
      "_WIDE_261028.csv"
    )
  }
  
  if (length(active_exports) > 1) {
    stop(
      "Multiple active WIDE exports found in:\n",
      raw_data_dir,
      "\nMove all older exports to !-archived:\n",
      paste(
        paste0("  - ", basename(active_exports)),
        collapse = "\n"
      )
    )
  }
  
  export_path <- active_exports[[1]]
}

message("      source export: ", basename(export_path))


# ---- 3.2 Select deidentified survey columns ----------------------------------

header <- names(
  read_csv(
    export_path,
    n_max = 0,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    name_repair = "minimal"
  )
)

keep_cols <- header[
  header %in% keep_fields |
    str_remove(header, "_[0-9]+$") %in% repeat_keep
]

# Variables required by downstream HFC scripts
required <- c(
  "KEY",
  "hh_id",
  "enumerator",
  "SubmissionDate",
  "starttime",
  "endtime"
)

missing_required <- setdiff(required, keep_cols)

if (length(missing_required) > 0) {
  stop(
    "Survey export is missing required columns:\n",
    paste(
      paste0("  - ", missing_required),
      collapse = "\n"
    )
  )
}

assert_no_identifying_cols(
  keep_cols,
  "Deidentified survey export"
)


# ---- 3.3 Read and write deidentified survey ----------------------------------

survey <- read_csv(
  export_path,
  col_select = all_of(keep_cols),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  name_repair = "minimal"
) |>
  mutate(
    # Keep submissions without hh_id, but standardize blank IDs as missing
    hh_id = na_if(str_squish(hh_id), "")
  )

n_missing_hh_id <- sum(is.na(survey$hh_id))

if (n_missing_hh_id > 0) {
  warning(
    n_missing_hh_id,
    " submission(s) have no hh_id; retained with hh_id = NA."
  )
}

if (nrow(survey) == 0) {
  stop("The active WIDE export contains no submissions.")
}

if (anyDuplicated(survey$KEY)) {
  warning(
    sum(duplicated(survey$KEY)),
    " duplicate SurveyCTO KEY value(s) found in the export."
  )
}

dir.create(
  deid_preload_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

deid_export_path <- file.path(
  deid_dir,
  paste0(FORM_ID, "_WIDE.csv")
)

write_csv(
  survey,
  deid_export_path,
  na = ""
)

message(
  sprintf(
    paste0(
      "[1/2] survey export: kept %d of %d columns, ",
      "%d submissions; dropped %d"
    ),
    length(keep_cols),
    length(header),
    nrow(survey),
    length(header) - length(keep_cols)
  )
)

message(
  "      deidentified export: ",
  basename(deid_export_path)
)

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
