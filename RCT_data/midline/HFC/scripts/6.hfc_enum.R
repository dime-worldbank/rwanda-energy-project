# ==============================================================================
# 6.hfc_enum.R
# Midline HFC: enumerator checks. Submissions per day, locate / consent /
# completion rates, durations, and counts of duplicates, logic errors,
# outliers and low-accuracy GPS points. Flags low consent rate and many short
# surveys.
#
# Sourced by 1.hfc_main.R after scripts 2-5 (uses hfc_constr, duplicates,
# logic_data, outlier_data).
# Outputs: enumerator_check
#
# Author: Yeji (using Claude) · Last modified: 2026-10-21
# ==============================================================================


# ---- 1. Checks: enumerator -----------------------------------------------------

enumerator_day <- hfc_constr |>
  count(enumerator, submissiondate) |>
  arrange(submissiondate) |>
  mutate(submissiondate = format(submissiondate)) |>
  pivot_wider(names_from = submissiondate, values_from = n, values_fill = 0)

# GPS accuracy (information only): observation point if consented, refusal point otherwise
gps_low <- hfc_constr |>
  mutate(accuracy = coalesce(`obs_gps-Accuracy`, `location-Accuracy`)) |>
  filter(accuracy > GPS_ACCURACY_MAX)

count_by_enum <- function(df, name) df |> count(enumerator, name = name)

# Summaries get new names (n_*) so they never mask the submission-level columns they use
enumerator_check <- hfc_constr |>
  summarise(enumerator_key = first(enumerator_key),
            n_attempts = n(),
            n_located = sum(located),
            n_consented = sum(consented),
            n_complete = sum(complete),
            days_worked = n_distinct(submissiondate),
            mean_survey_min = round(mean(survey_min[consented]), 1),
            n_short = sum(consented & survey_min < DURATION_MIN),
            n_long = sum(consented & survey_min > DURATION_MAX),
            .by = enumerator) |>
  mutate(locate_rate = round(n_located / n_attempts, 3),
         consent_rate = round(n_consented / n_located, 3),
         completion_rate = round(n_complete / n_attempts, 3),
         completes_per_day = round(n_complete / days_worked, 1)) |>
  # Bug 1 fixed: compare to the enumerator column, not to a data frame
  left_join(count_by_enum(distinct(duplicates, enumerator, KEY), "n_duplicates"), by = "enumerator") |>
  left_join(count_by_enum(distinct(filter(logic_data, type == "error"), enumerator, KEY), "n_logic_error"), by = "enumerator") |>
  left_join(count_by_enum(distinct(outlier_data, enumerator, KEY), "n_outlier"), by = "enumerator") |>
  left_join(count_by_enum(gps_low, "n_gps_low_accuracy"), by = "enumerator") |>
  mutate(across(starts_with("n_"), \(x) coalesce(x, 0L)),
         flag_low_consent = consent_rate < CONSENT_TARGET,
         flag_many_short = n_short >= SHORT_SURVEYS_ENUM_MIN) |>
  left_join(enumerator_day, by = "enumerator") |>
  arrange(as.numeric(enumerator))

add_flags(enumerator_check |> filter(flag_low_consent), "6.hfc_enum: low_consent_rate",
          detail = sprintf("consent rate %.1f%%", 100 * filter(enumerator_check, flag_low_consent)$consent_rate))
add_flags(enumerator_check |> filter(flag_many_short), "6.hfc_enum: many_short_surveys",
          detail = sprintf("%d consented surveys < %g min", filter(enumerator_check, flag_many_short)$n_short, DURATION_MIN))


# ---- 2. Export ---------------------------------------------------------------

hfc_outputs$enumerator_check <- enumerator_check

message(sprintf("      %d enumerators; %d with low consent rate, %d with many short surveys",
                nrow(enumerator_check), sum(enumerator_check$flag_low_consent), sum(enumerator_check$flag_many_short)))
