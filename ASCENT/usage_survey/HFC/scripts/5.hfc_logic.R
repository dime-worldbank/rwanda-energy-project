# ==============================================================================
# 5.hfc_logic.R
# Clean cooking HFC: logic checks, one row per submission x check. Two kinds:
#   inconsistent  answers that contradict each other: count toward LOGIC_TARGET
#   verify        plausible answers worth a back-check call (soft flags, e.g. a
#                 program stove that was never used); reported, not targeted
# All checks are soft: nothing here is a form constraint.
#
# Sourced by 1.hfc_main.R after 2.hfc_construct.R (uses hfc_constr and the
# derived prog_* / roster / fuel variables).
# Outputs: logic_data, logic_summary (check x date)
#
# Author: Tanay (using Claude) · Last modified: 2026-11-04
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
  flag_rows("multiple_primary", "inconsistent", "More than one stove reported as primary", n_primary > 1),
  flag_rows("share_sum_off", "inconsistent",
            sprintf("Cooking shares across stoves used add up to <%g%% or >%g%%", SHARE_SUM_RANGE[1], SHARE_SUM_RANGE[2]),
            !is.na(share_sum) & (share_sum < SHARE_SUM_RANGE[1] | share_sum > SHARE_SUM_RANGE[2])),
  flag_rows("share_all_with_other_stove", "inconsistent", "One stove does 'all' cooking but another stove was used",
            any_share_all & n_stoves_used_7d > 1),
  flag_rows("primary_stove_not_used", "inconsistent", "Primary stove (Section C) not among stoves used in last 30 days",
            consented & !is.na(primary_stove) & !primary_in_loop),

  # Seasonality vs last 7 days (program stove)
  flag_rows("no_regular_months_but_used", "inconsistent", "0 months of regular use, but program stove used in last 7 days",
            months_regular_use %in% 0 & prog_days_7d > 0),
  flag_rows("typical_vs_7d_days", "verify",
            sprintf("Typical days/week and days used last 7 days differ by %g+ with no seasonal change", TYPICAL_VS_7D_GAP),
            seasonal_change %in% "0" & abs(days_week_typical - prog_days_7d) >= TYPICAL_VS_7D_GAP),

  # Time-use roster
  flag_rows("heads_not_one", "inconsistent", "Time-use roster does not have exactly one household head",
            consented & n_roster > 0 & n_heads != 1),
  flag_rows("children_mismatch", "inconsistent", "Members under 18 in the roster differ from hh_child_count",
            consented & n_roster > 0 & n_child_roster != hh_child_count),
  flag_rows("nobody_cooks", "inconsistent", "A stove was used in the last 7 days but no member cooked",
            n_stoves_used_7d > 0 & n_roster > 0 & !any_cook),
  flag_rows("cook_hours_high", "verify", sprintf("A member cooked more than %g hours in the last 7 days", COOK_HOURS_MAX),
            max_cook_hours > COOK_HOURS_MAX),
  flag_rows("firewood_not_source", "verify", "A member collected firewood but firewood is not a cooking energy source",
            any_firewood & !firewood_source),

  # Fuel units
  flag_rows("fuel_unit_kwh_not_electricity", "inconsistent", "Kilowatt-hours reported for a fuel other than electricity",
            kwh_nonelec > 0),
  flag_rows("fuel_unit_cylinder_not_gas", "inconsistent", "Cylinder reported for a fuel other than gas", cyl_nongas > 0),

  # Program stove verification (soft: these are results too, but worth a call)
  flag_rows("program_stove_not_confirmed", "verify", "Household does not confirm the stove type on the beneficiary list",
            ascent_confirm %in% "0"),
  flag_rows("program_stove_never_used", "verify", "Household never started using the program stove",
            started_program_stove %in% "0"),
  flag_rows("program_stove_not_used_30d", "verify", "Program stove started but not among stoves used in last 30 days",
            started_program_stove %in% "1" & !prog_in_loop),
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
