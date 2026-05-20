# 02_clean.R --------------------------------------------------------------
# Light cleaning only: coerce types where needed, tag each row with the
# wave label, normalise the consent flag. Nothing substantive is recoded
# here — raw values remain inspectable for downstream sanity checks.

# Helper: safely coerce a character column to numeric (returns NA on "" or
# values like "n/a"). Used for variables ODK exports as text.
to_num <- function(x) suppressWarnings(as.numeric(x))

clean_wave <- function(df, wave_label) {
  df %>%
    dplyr::mutate(
      wave           = factor(wave_label, levels = c("baseline", "midline", "endline")),
      aggregator_id  = as.character(aggregator_id),
      village_id     = as.character(village_id),
      market_id      = as.character(market_id),
      sector_id      = as.character(sector_id),
      district_id    = as.character(district_id),
      province_id    = as.character(province_id),
      sampled_village = to_num(sampled_village),
      consent        = to_num(consent),
      agg_size       = to_num(agg_size),
      num_trips      = to_num(num_trips),
      last_trips_count = to_num(last_trips_count),
      number_trucker = to_num(number_trucker),
      duration       = to_num(duration),
      date           = suppressWarnings(lubridate::as_date(date))
    )
}

waves <- list(
  baseline = clean_wave(waves_raw$baseline, "baseline"),
  midline  = clean_wave(waves_raw$midline,  "midline"),
  endline  = clean_wave(waves_raw$endline,  "endline")
)

# Drop rows with missing aggregator_id (test submissions) and de-duplicate
# on aggregator_id within each wave (keep the latest by SubmissionDate).
dedupe_wave <- function(df) {
  df %>%
    dplyr::filter(!is.na(aggregator_id), aggregator_id != "") %>%
    dplyr::group_by(aggregator_id) %>%
    dplyr::slice_max(order_by = SubmissionDate, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
}

waves <- purrr::map(waves, dedupe_wave)

message(sprintf(
  "Cleaned waves: baseline=%d, midline=%d, endline=%d unique aggregator_ids",
  nrow(waves$baseline), nrow(waves$midline), nrow(waves$endline)
))

# Long-form stack for cross-wave summaries. Only the columns common to all
# three waves survive the bind; everything else stays on the per-wave
# tibbles.
common_cols <- Reduce(intersect, lapply(waves, names))

waves_long <- purrr::map2_dfr(
  waves, names(waves),
  ~ dplyr::select(.x, dplyr::all_of(common_cols)) %>% dplyr::mutate(wave = .y)
) %>%
  dplyr::mutate(wave = factor(wave, levels = c("baseline", "midline", "endline")))
