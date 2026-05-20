# 01_load.R ---------------------------------------------------------------
# Read the three main wave CSVs. One row per aggregator interview.
# guess_max is set high so readr does not mis-type sparsely-populated
# multi-select columns. All columns default to character; specific
# type coercion lives in 02_clean.R.

read_wave <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    guess_max = 10000,
    show_col_types = FALSE,
    progress = FALSE
  )
}

waves_raw <- list(
  baseline = read_wave(paths$baseline),
  midline  = read_wave(paths$midline),
  endline  = read_wave(paths$endline)
)

message(sprintf(
  "Loaded raw waves: baseline=%d rows, midline=%d rows, endline=%d rows",
  nrow(waves_raw$baseline),
  nrow(waves_raw$midline),
  nrow(waves_raw$endline)
))

# TODO (follow-up): also load the repeat-group child CSVs
#   - "*-last_trip_crops.csv"
#   - "*-survey-aggregator-crop_sales.csv" / "*-survey-crop_sales.csv"
#   - "*-survey-aggregator-last_trips.csv"
#   - "*-survey-aggregator-transported_crops-crops_transport.csv"
# and join them to the parent submission via the KEY / PARENT_KEY columns
# for trip-level and crop-level descriptive statistics.
#
# TODO (follow-up): link to Sampling/forms/aggregator_assignment.csv
# via aggregator_id to recover feeder-level assignment / treatment arms.
