# ==============================================================================
# 5.hfc_logic.R
# Midline HFC: consistency checks, one row per submission x check.
#   Hard checks (consented submissions): electricity devices vs. grid electricity use.
#   Flags (soft checks): dates and durations, treatment compliance, enumerator
#     observation vs. reported devices, SHS consistency. Summary by date.
# Counts per enumerator: n_hard_checks and n_soft_flags in enumerator_check.
#
# Sourced by 1.hfc_main.R after 2.hfc_construct.R (uses hfc_constr).
# Outputs: hard_checks, hard_checks_summary, flags_data, flags_summary
#
# Author: Yeji (using Claude) · Last modified: 2026-11-09
# ==============================================================================


# ---- 1. Config / constants ---------------------------------------------------

SECTION_USAGE <- "Electricity devices vs. usage"

# One row per hard check, in dashboard order. A devices vs. photos section is
# tentative: it needs photo-submitted indicators from 0.hfc_deidentify.R.
HARD_CHECKS <- tribble(
  ~section,        ~check,                          ~issue,
  SECTION_USAGE,   "hard_usage_no_device",          "Uses grid electricity, has no readyboard or wiring",
  SECTION_USAGE,   "hard_usage_device_not_working", "Uses grid electricity, readyboard or wiring not working",
  SECTION_USAGE,   "hard_working_no_usage",         "Readyboard or wiring working, does not use grid electricity",
  SECTION_USAGE,   "hard_home_charge_no_usage",     "Charges phone at home, does not use grid electricity, no SHS",
  SECTION_USAGE,   "hard_home_charge_no_meter",     "Charges phone at home, no meter, no SHS"
)


# ---- 2. Helper functions -----------------------------------------------------

# Rows of `data` meeting `cond` (NA counts as not flagged), in the flags_data layout
flag_rows <- function(check, issue, cond, data = hfc_constr) {
  data |>
    filter({{ cond }}) |>
    transmute(KEY, hh_id, enumerator, enumerator_key, district_key, village_key, submissiondate, startdate,
              treat, check = paste0("5.hfc_logic: ", check), issue)
}

# Rows of hard_base meeting `cond`, with the section and label from HARD_CHECKS
hard_rows <- function(check_name, cond) {
  spec <- HARD_CHECKS |> filter(check == check_name)
  hard_base |>
    filter({{ cond }}) |>
    transmute(KEY, hh_id, enumerator, enumerator_key, district_key, village_key, submissiondate, startdate,
              treat, section = spec$section, check = paste0("5.hfc_logic: ", check_name), issue = spec$issue)
}


# ---- 3. Checks: hard ---------------------------------------------------------

# Readyboard, wiring and grid use are asked only if the household has a meter, so a blank counts as no.
# Phone charging: any phone of the household charged at home (F2_1 = 1).
hard_base <- hfc_constr |>
  filter(consented) |>
  mutate(
    has_meter = meter_confirm %in% 1,
    has_shs = shs_confirm %in% 1,
    has_rb_wire = readyboard_confirm %in% 1 | additional_wire_confirm %in% 1,
    any_device = has_rb_wire | has_shs,
    rb_wire_working = readyboard_confirm_working %in% 1 | additional_wire_confirm_working %in% 1,
    uses_grid = electricity_usage_confirm %in% 1,
    no_grid_use = electricity_usage_confirm %in% 0,
    observed_yes = observe_electricity_connection %in% 1,
    observed_no = observe_electricity_connection %in% 0,
    # "No" is the correct observation if SHS is the only device and the enumerator reports it is not visible
    shs_only_not_visible = has_shs & !has_rb_wire & obs_shs %in% 0,
    home_charge = if_any(matches("^F2_1_\\d+$"), \(x) x %in% 1)
  )

hard_checks <- bind_rows(
  # Electricity devices vs. usage
  hard_rows("hard_usage_no_device", uses_grid & !has_rb_wire),
  hard_rows("hard_usage_device_not_working", uses_grid & has_rb_wire & !rb_wire_working),
  hard_rows("hard_working_no_usage", rb_wire_working & no_grid_use),
  hard_rows("hard_home_charge_no_usage", home_charge & no_grid_use & !has_shs),
  hard_rows("hard_home_charge_no_meter", home_charge & !has_meter & !has_shs)
) |>
  arrange(check, submissiondate)

add_flags(hard_checks, detail = hard_checks$issue)

# Every check is listed, including those with no flagged submission
hard_checks_summary <- HARD_CHECKS |>
  mutate(check = paste0("5.hfc_logic: ", check)) |>
  left_join(count(hard_checks, check, name = "total"), by = "check") |>
  mutate(total = coalesce(total, 0L),
         pct_of_consented = round(100 * total / nrow(hard_base), 1))


# ---- 4. Checks: flags (soft) -------------------------------------------------

flags_data <- bind_rows(
  # Dates and durations
  flag_rows("start_end_date", "End date differs from start date", startdate != enddate),
  flag_rows("end_submission_date", sprintf("Uploaded %g or more days after the survey ended", UPLOAD_DELAY_DAYS),
            as.numeric(submissiondate - enddate) >= UPLOAD_DELAY_DAYS),
  flag_rows("duration_short", sprintf("Consented survey shorter than %g min", DURATION_MIN),
            consented & survey_min < DURATION_MIN),
  flag_rows("duration_long", sprintf("Consented survey longer than %g min", DURATION_MAX),
            consented & survey_min > DURATION_MAX),

  # Treatment compliance
  flag_rows("readyboard_in_C_T2", "Readyboard reported in a C or T2 village",
            readyboard_confirm == 1 & !treat %in% RB_ARMS),
  flag_rows("nudge_outside_T2T3", "SHS nudge answered outside T2/T3",
            (!is.na(nosolar_confirm) | !is.na(shs_willing)) & !treat %in% SHS_ARMS),

  # Grid consistency
  flag_rows("usage_without_meter", "Grid electricity use reported without a meter",
            electricity_usage_confirm == 1 & meter_confirm == 0),

  # Observation vs. answers (consented submissions)
  flag_rows("observe_device_yes_observed_no", "Readyboard, wiring or SHS = yes, observed NO",
            any_device & observed_no & !shs_only_not_visible, data = hard_base),
  flag_rows("observe_all_no_meter_observed", "All no, has meter, observed YES",
            !any_device & has_meter & observed_yes, data = hard_base),
  flag_rows("observe_all_no_no_meter_observed", "All no, no meter, observed YES",
            !any_device & !has_meter & observed_yes, data = hard_base),

  # SHS consistency
  flag_rows("obs_shs_without_shs_confirm", "SHS observed but none reported", obs_shs == 1 & shs_confirm == 0)
) |>
  arrange(check, submissiondate)

add_flags(flags_data, detail = flags_data$issue)


# ---- 5. Summaries ------------------------------------------------------------

n_flag_sub <- n_distinct(flags_data$KEY)

# Flag x submission date (+ total and share of all submissions)
flags_summary <- flags_data |>
  count(check, issue, submissiondate) |>
  arrange(submissiondate) |>
  mutate(submissiondate = format(submissiondate)) |>
  pivot_wider(names_from = submissiondate, values_from = n, values_fill = 0) |>
  mutate(total = rowSums(across(-c(check, issue))),
         pct_of_submissions = round(100 * total / nrow(hfc_constr), 1), .after = issue) |>
  add_row(check = "5.hfc_logic: any_flag",
          issue = "Submissions with at least one soft flag",
          total = n_flag_sub, pct_of_submissions = round(100 * n_flag_sub / nrow(hfc_constr), 1))


# ---- 6. Export ---------------------------------------------------------------

hfc_outputs$hard_checks         <- hard_checks
hfc_outputs$hard_checks_summary <- hard_checks_summary
hfc_outputs$flags_data          <- flags_data
hfc_outputs$flags_summary       <- flags_summary

message(sprintf("      hard checks: %d on %d of %d consented submissions; soft flags: %d on %d submissions",
                nrow(hard_checks), n_distinct(hard_checks$KEY), nrow(hard_base), nrow(flags_data), n_flag_sub))
