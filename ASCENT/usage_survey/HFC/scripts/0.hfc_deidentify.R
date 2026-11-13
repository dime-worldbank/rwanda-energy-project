# ==============================================================================
# 0.hfc_deidentify.R
# Clean cooking HFC: builds the deidentified inputs that 1.hfc_main.R reads.
# Run it first, and again whenever the export or preload changes.
#
# Allowlist (as in the midline HFC): a survey column is kept only if the form
# defines it as a number, date, choice or calculate field, or as an "other
# (specify)" text field (`*_other`), and its name is not on the privacy block
# list. Free text (member names, usage_comment), dates of birth and device ids
# are dropped. Repeat fields are kept at their nesting depth only: <field>_i in a
# repeat, <field>_i_j in a nested repeat (stove_fuel_loop inside stove_loop).
# The raw files are only read, never edited.
#
# Run from the repo root:
#   Rscript ASCENT/usage_survey/HFC/scripts/0.hfc_deidentify.R
#
# Author: Tanay (using Claude) ?? Last modified: 2026-11-05 (Yeji: new form structure)
# ==============================================================================


# ---- 1. Setup ----------------------------------------------------------------

t_start <- Sys.time()
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
})
if (!file.exists("PATHS.R")) stop("Run this script from the repo root (PATHS.R not found).")
source("PATHS.R")
source("ASCENT/usage_survey/HFC/scripts/hfc_config.R")
source("ASCENT/usage_survey/HFC/scripts/hfc_privacy.R")
message("[0.hfc_deidentify] mode: ", HFC_MODE, " | form: ", basename(form_path))


# ---- 2. Config / constants ---------------------------------------------------

REQUIRED <- c("KEY", "hh_id", "enumerator", "SubmissionDate", "starttime", "endtime")

read_chr <- function(path, ...) {
  read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE,
           name_repair = "unique_quiet", ...)
}


# ---- 3. Survey export --------------------------------------------------------

form <- read_excel(form_path, "survey", col_types = "text", .name_repair = "unique_quiet") |>
  filter(!is.na(type)) |>
  mutate(type = str_squish(type), name = str_squish(name), base_type = word(type, 1))

form$depth <- cumsum(form$type == "begin repeat") - cumsum(form$type == "end repeat")   # repeat nesting

form_keep <- form |>
  filter(base_type %in% HFC_KEEP_TYPES | (base_type == "text" & str_detect(name, HFC_OTHER_REGEX)),
         !str_detect(name, regex(HFC_BLOCK_REGEX, ignore_case = TRUE)))
keep_fields <- unique(c(HFC_META_COLS, form_keep$name[form_keep$depth == 0]))
repeat_keep <- form_keep |> filter(depth > 0) |> distinct(name, depth)

export_path <- find_raw_export()
if (!file.exists(export_path)) stop("Export not found:\n", export_path)
message("      source export: ", basename(export_path))

header <- names(read_csv(export_path, n_max = 0, col_types = cols(.default = col_character()),
                         show_col_types = FALSE, name_repair = "minimal"))
# Repeat fields appear as <field>_1, <field>_2, ... (nested: <field>_1_2) in a WIDE export. Matching the
# exact depth leaves out the 0/1 columns SurveyCTO adds per choice of a select_multiple in a repeat.
repeat_pattern <- paste0("^(", paste0(repeat_keep$name, strrep("_[0-9]+", repeat_keep$depth), collapse = "|"), ")$")
keep_cols <- header[header %in% keep_fields | str_detect(header, repeat_pattern)]

missing_required <- setdiff(REQUIRED, keep_cols)
if (length(missing_required) > 0) stop("Export is missing required columns: ", paste(missing_required, collapse = ", "))
assert_no_identifying_cols(keep_cols, "Deidentified survey export")

survey <- read_chr(export_path, col_select = all_of(keep_cols)) |>
  mutate(hh_id = na_if(str_squish(hh_id), ""))
if (nrow(survey) == 0) stop("The export contains no submissions.")
if (any(is.na(survey$hh_id))) warning(sum(is.na(survey$hh_id)), " submission(s) have no hh_id.")
if (anyDuplicated(survey$KEY)) warning(sum(duplicated(survey$KEY)), " duplicate KEY value(s) in the export.")

dir.create(preload_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(survey, file.path(deid_dir, paste0(FORM_ID, "_WIDE.csv")), na = "")
message(sprintf("[1/2] survey export: kept %d of %d columns, %d submissions",
                length(keep_cols), length(header), nrow(survey)))


# ---- 4. Preload files --------------------------------------------------------

raw_pre <- function(what, cols) {
  path <- file.path(raw_preload_dir, PRELOAD_RAW[[what]])
  if (!file.exists(path)) stop("Preload file not found:\n", path)
  x <- read_chr(path)
  miss <- setdiff(cols, names(x))
  if (length(miss) > 0) stop(basename(path), " is missing columns: ", paste(miss, collapse = ", "))
  x |> select(all_of(cols))
}

# Sample frame: every household, its village and its preload stove type (the stratum)
household_frame <- raw_pre("stoves", c("householdid_key", "stove_type_code")) |>
  rename(stove_type = stove_type_code) |>
  left_join(raw_pre("households", c("householdid_key", "villageid_key")), by = "householdid_key")
if (any(is.na(household_frame$villageid_key))) {
  warning(sum(is.na(household_frame$villageid_key)), " household(s) in ", PRELOAD_RAW[["stoves"]],
          " are not in ", PRELOAD_RAW[["households"]], " (no village).")
}

deid_preload <- list(
  household_frame.csv = household_frame,
  scope_villages.csv  = raw_pre("villages", c("districtid_key", "sectorid_key", "cellid_key", "villageid_key",
                                              "district_key", "sector_key", "cell_key", "village_key")),
  enumerators.csv     = raw_pre("enumerators", c("enumeratorid_key", "enumerator_key"))
)
iwalk(deid_preload, \(x, f) {
  assert_no_identifying_cols(names(x), f)
  write_csv(x, file.path(preload_dir, f), na = "")
  message(sprintf("      preload %s: %d rows (%s)", f, nrow(x), paste(names(x), collapse = ", ")))
})

message(sprintf("[2/2] deidentified inputs in %s (%.1fs)", deid_dir,
                as.numeric(difftime(Sys.time(), t_start, units = "secs"))))



