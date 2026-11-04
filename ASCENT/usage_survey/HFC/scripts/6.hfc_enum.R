# ==============================================================================
# 6.hfc_enum.R
# Clean cooking HFC: enumerator checks. Submissions per day, locate / consent /
# completion rates, durations, and counts of duplicates, inconsistencies,
# answers to verify and outliers. Flags a consent rate below CONSENT_TARGET and
# many short surveys. (Same as the midline HFC, with logic split by kind.)
#
# Sourced by 1.hfc_main.R after scripts 2-5.
# Outputs: enumerator_check
#
# Author: Tanay (using Claude) · Last modified: 2026-11-04
# ==============================================================================


# ---- 1. Config / constants ---------------------------------------------------

COL_SHORT       <- sprintf("n_short_under_%gmin", DURATION_MIN)
COL_LONG        <- sprintf("n_long_over_%gmin", DURATION_MAX)
COL_LOW_CONSENT <- sprintf("consent_rate_below_%g", 100 * CONSENT_TARGET)
COL_MANY_SHORT  <- sprintf("short_surveys_%d_or_more", SHORT_SURVEYS_ENUM_MIN)


# ---- 2. Checks: enumerator -----------------------------------------------------

enumerator_day <- hfc_constr |>
  count(enumerator, submissiondate) |>
  arrange(submissiondate) |>
  mutate(submissiondate = format(submissiondate)) |>
  pivot_wider(names_from = submissiondate, values_from = n, values_fill = 0)

count_by_enum <- function(df, name) df |> count(enumerator, name = name)

enumerator_check <- hfc_constr |>
  summarise(enumerator_key = first(enumerator_key),
            n_submissions = n(),
            n_located = sum(located),
            n_consented = sum(consented),
            n_complete = sum(complete),
            days_worked = n_distinct(submissiondate),
            mean_survey_min = round(mean(survey_min[consented]), 1),
            n_short = sum(consented & survey_min < DURATION_MIN, na.rm = TRUE),
            n_long = sum(consented & survey_min > DURATION_MAX, na.rm = TRUE),
            .by = enumerator) |>
  mutate(locate_pct = round(100 * n_located / n_submissions, 1),
         consent_pct = round(100 * n_consented / n_located, 1),
         completion_pct = round(100 * n_complete / n_submissions, 1),
         completes_per_day = round(n_complete / days_worked, 1)) |>
  left_join(count_by_enum(distinct(duplicates, enumerator, KEY), "n_duplicates"), by = "enumerator") |>
  left_join(count_by_enum(distinct(filter(logic_data, kind == "inconsistent"), enumerator, KEY), "n_inconsistent"),
            by = "enumerator") |>
  left_join(count_by_enum(distinct(filter(logic_data, kind == "verify"), enumerator, KEY), "n_verify"),
            by = "enumerator") |>
  left_join(count_by_enum(distinct(outlier_data, enumerator, KEY), "n_outlier"), by = "enumerator") |>
  mutate(across(c(n_duplicates, n_inconsistent, n_verify, n_outlier), \(x) coalesce(x, 0L)),
         flag_low_consent = coalesce(consent_pct < 100 * CONSENT_TARGET, FALSE),
         flag_many_short = n_short >= SHORT_SURVEYS_ENUM_MIN) |>
  left_join(enumerator_day, by = "enumerator") |>
  arrange(suppressWarnings(as.numeric(enumerator)))

add_flags(enumerator_check |> filter(flag_low_consent), "6.hfc_enum: low_consent_rate",
          detail = sprintf("consent rate %.1f%%", filter(enumerator_check, flag_low_consent)$consent_pct))
add_flags(enumerator_check |> filter(flag_many_short), "6.hfc_enum: many_short_surveys",
          detail = sprintf("%d consented surveys < %g min", filter(enumerator_check, flag_many_short)$n_short, DURATION_MIN))

enumerator_check <- enumerator_check |>
  rename(!!COL_SHORT := n_short, !!COL_LONG := n_long,
         !!COL_LOW_CONSENT := flag_low_consent, !!COL_MANY_SHORT := flag_many_short)


# ---- 3. Export ---------------------------------------------------------------

hfc_outputs$enumerator_check <- enumerator_check

message(sprintf("      %d enumerators; %d with %s, %d with %s",
                nrow(enumerator_check), sum(enumerator_check[[COL_LOW_CONSENT]]), COL_LOW_CONSENT,
                sum(enumerator_check[[COL_MANY_SHORT]]), COL_MANY_SHORT))
