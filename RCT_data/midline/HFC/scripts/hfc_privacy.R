# Privacy rules shared by 0.hfc_deidentify.R and 1.hfc_main.R. Defines objects only; reads no data.

# Form field types the HFC may use. Free text, geopoints, audio, images, text audits and device ids
# are not in this list, so they are dropped by default.
HFC_KEEP_TYPES <- c("integer", "decimal", "date", "select_one", "select_multiple", "calculate", "calculate_here")

# SurveyCTO columns that are not form fields
HFC_META_COLS <- c("KEY", "SubmissionDate", "starttime", "endtime", "formdef_version")

# Column-name patterns that must never reach the HFC (names, phones, GPS, device ids, media, free-text comments)
HFC_BLOCK_REGEX <- paste(c("phone", "(^|_)name$", "latitude", "longitude", "altitude", "accuracy", "gps",
                           "(^|_)nid$", "audio", "photo", "text_audit", "^simid$", "subscriber", "deviceid",
                           "meter_number", "comment"), collapse = "|")

# "Other (specify)" text fields are kept (not deidentified), including repeat instances (F2_2_other_1, ...)
HFC_OTHER_REGEX <- "_other$"

# Stops the run if an input holds a column that looks identifying
assert_no_identifying_cols <- function(cols, label) {
  bad <- grep(HFC_BLOCK_REGEX, cols, value = TRUE, ignore.case = TRUE)
  if (length(bad) > 0) {
    stop(label, " has identifying columns: ", paste(bad, collapse = ", "),
         ". Run 0.hfc_deidentify.R and use its output.", call. = FALSE)
  }
}
