# ==============================================================================
# 5.hfc_logic.R
# Clean cooking HFC: logic checks, one row per submission x check. Two kinds:
#   inconsistent  answers that contradict each other: count toward LOGIC_TARGET
#   verify        plausible answers worth a back-check call (soft flags, e.g. a
#                 program stove not used in the last 30 days); reported, not targeted
# All checks are soft: nothing here is a form constraint.
#
# Sourced by 1.hfc_main.R after 2.hfc_construct.R (uses hfc_constr and the
# derived prog_* / stove / fuel / roster / time-use variables).
# Outputs: logic_data, logic_summary (check x date)
#
# Author: Tanay (using Claude) · Last modified: 2026-11-05 (Yeji: new form structure)
# ==============================================================================


# ---- 1. Helper functions -----------------------------------------------------

flag_rows <- function(check, kind, issue, cond) {
  hfc_constr |>
    filter({{ cond }}) |>
    transmute(KEY, hh_id, enumerator, enumerator_key, district_key, village_key, submissiondate,
              stove_group, check = paste0("5.hfc_logic: ", check), kind, issue)
}


# ---- 2. Checks: logic ----------------------------------------------------------

logic_data <- bind_rows(
  # Dates and durations
  flag_rows("start_end_date", "inconsistent", "End date differs from start date", startdate != enddate),
  flag_rows("end_submission_date", "inconsistent",
            sprintf("Uploaded %g or more days after the survey ended", UPLOAD_DELAY_DAYS),
            as.numeric(submissiondate - enddate) >= UPLOAD_DELAY_DAYS),
  flag_rows("duration_short", "inconsistent", sprintf("Consented survey shorter than %g min", DURATION_MIN),
            consented & survey_min < DURATION_MIN),
  flag_rows("duration_long", "inconsistent", sprintf("Consented survey longer than %g min", DURATION_MAX),
            consented & survey_min > DURATION_MAX),

  # Stove loop
  flag_rows("share_sum_off", "inconsistent",
            sprintf("Cooking shares across stoves used add up to <%g%% or >%g%%", SHARE_SUM_RANGE[1], SHARE_SUM_RANGE[2]),
            !is.na(share_sum) & (share_sum < SHARE_SUM_RANGE[1] | share_sum > SHARE_SUM_RANGE[2])),
  flag_rows("share_all_with_other_stove", "inconsistent", "One stove does 'all' cooking but another stove was used",
            any_share_all & n_stoves_used_7d > 1),
  flag_rows("events_below_days", "inconsistent", "A stove has fewer cooking events than days used in the last 7 days",
            n_events_below_days > 0),
  flag_rows("stove_fuel_mismatch", "inconsistent",
            "LPG / PAYG LPG stove used without gas, or EPC used without electricity",
            n_stove_fuel_mismatch > 0),
  flag_rows("used_but_not_working", "verify", "A stove used in the last 7 days is reported as not working",
            n_used_not_working > 0),

  # Fuel loop
  flag_rows("fuel_unit_kwh_not_electricity", "inconsistent", "Kilowatt-hours reported for a fuel other than electricity",
            kwh_nonelec > 0),
  flag_rows("fuel_unit_cylinder_not_gas", "inconsistent", "Cylinder reported for a fuel other than gas", cyl_nongas > 0),
  flag_rows("fuel_own_above_qty", "inconsistent", "Own production of a fuel is larger than the quantity used",
            n_own_above_qty > 0),

  # Roster and time use
  flag_rows("heads_not_one", "inconsistent", "Household roster does not have exactly one household head",
            consented & n_roster > 0 & n_heads != 1),
  flag_rows("nobody_cooks", "inconsistent", "A stove was used in the last 7 days but no member cooked (complete surveys)",
            complete & n_stoves_used_7d > 0 & !any_cook),
  flag_rows("cook_hours_high", "verify", sprintf("A member cooked more than %g hours in the last 7 days", COOK_HOURS_MAX),
            coalesce(max_cook_hours > COOK_HOURS_MAX, FALSE)),
  flag_rows("child_cook", "verify", sprintf("A member younger than %g cooked in the last 7 days", CHILD_COOK_AGE),
            n_child_cook > 0),
  flag_rows("firewood_not_source", "verify", "A member collected firewood but no stove uses firewood",
            any_firewood & !firewood_source),

  # Program stove verification (soft: these are results too, but worth a call)
  flag_rows("program_stove_not_confirmed", "verify", "Household does not confirm the stove type on the beneficiary list",
            ascent_confirm %in% "0"),
  flag_rows("program_stove_not_used_30d", "verify", "Program stove not among the stoves used in the last 30 days",
            !is.na(program_stove_type) & !prog_in_loop),
  flag_rows("program_stove_not_observed", "verify", "Program stove not observed, or observed but not usable",
            program_stove_seen %in% c("2", "3"))
) |>
  arrange(kind, check, submissiondate)

add_flags(logic_data, detail = logic_data$issue)


# ---- 3. Summaries ------------------------------------------------------------

n_inconsistent_sub <- n_distinct(logic_data$KEY[logic_data$kind == "inconsistent"])
n_verify_sub       <- n_distinct(logic_data$KEY[logic_data$kind == "verify"])

logic_summary <- logic_data |>
  count(kind, check, issue, submissiondate) |>
  arrange(submissiondate) |>
  mutate(submissiondate = format(submissiondate)) |>
  pivot_wider(names_from = submissiondate, values_from = n, values_fill = 0) |>
  mutate(total = rowSums(across(-c(kind, check, issue))),
         pct_of_submissions = round(100 * total / nrow(hfc_constr), 1), .after = issue) |>
  add_row(kind = "inconsistent", check = "5.hfc_logic: any_inconsistent",
          issue = sprintf("Submissions with at least one inconsistency (target <%.0f%%)", 100 * LOGIC_TARGET),
          total = n_inconsistent_sub, pct_of_submissions = round(100 * n_inconsistent_sub / nrow(hfc_constr), 1)) |>
  add_row(kind = "verify", check = "5.hfc_logic: any_verify",
          issue = "Submissions with at least one answer to verify (no target; back-check list)",
          total = n_verify_sub, pct_of_submissions = round(100 * n_verify_sub / nrow(hfc_constr), 1))


# ---- 4. Export ---------------------------------------------------------------

hfc_outputs$logic_data    <- logic_data
hfc_outputs$logic_summary <- logic_summary

message(sprintf("      %d logic flags; %d submissions inconsistent (%.1f%%), %d to verify (%.1f%%)",
                nrow(logic_data), n_inconsistent_sub, 100 * n_inconsistent_sub / nrow(hfc_constr),
                n_verify_sub, 100 * n_verify_sub / nrow(hfc_constr)))
