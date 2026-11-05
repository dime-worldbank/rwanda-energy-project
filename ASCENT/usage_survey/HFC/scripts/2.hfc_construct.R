# ==============================================================================
# 2.hfc_construct.R
# Clean cooking HFC: load the deidentified export, parse dates, filter fieldwork
# start, special codes -> NA (continuous only), join enumerator / admin names and
# the preload stove type (stratum), derive household-level variables from the
# repeat groups, and find duplicates.
#
# All survey columns are read as text; continuous fields are converted to
# numbers. Choice codes stay text ("1", "0", "-77"), so checks compare to strings.
#
# Repeat groups in the WIDE export: <field>_i (stove_loop, hh_roster,
# cooking_time_loop, firewood_time_loop) and <field>_i_j for the nested
# stove_fuel_loop (i = stove instance, j = fuel instance). rep_long() reads a
# field at its exact depth into a long table (KEY, i1[, i2], value).
#
# Derived variables (one row per submission):
#   complete          located, consented and observe_stoves answered (last required question)
#   prog_*            answers for the program stove, taken from the stove loop
#                     instance whose stove_type = program_stove_type
#   prog_top_share    program stove used and its cooking share is the highest
#                     among the stoves used in the last 7 days (ties count)
#   op_days_year      program stove days used in the last 7 days x 365/7
#   n_stoves_used_7d  stoves with days_used_7d > 0
#   share_sum         sum of cooking_share midpoints (SHARE_MID) over those stoves
#   trad_used_7d      traditional stoves (TRADITIONAL_STOVES) used in the last 7 days
#   n_events_below_days, n_used_not_working, n_stove_fuel_mismatch   (stove loop)
#   kwh_nonelec, cyl_nongas, n_own_above_qty                         (fuel loop)
#   n_roster, n_heads                                                (hh_roster)
#   any_cook, max_cook_hours, n_child_cook, any_firewood             (time use)
#
# Sourced by 1.hfc_main.R. Outputs: hfc_constr, stoves_long, duration_data, duplicates
#
# Author: Tanay (using Claude) · Last modified: 2026-11-05 (Yeji: new form structure)
# ==============================================================================


# ---- 1. Helper functions -----------------------------------------------------

read_preload <- function(file) {
  df <- read_csv(file.path(preload_dir, file), col_types = cols(.default = col_character()),
                 show_col_types = FALSE, name_repair = "unique_quiet")
  assert_no_identifying_cols(names(df), file)
  df
}

# Export columns of a form field: the field itself, or its repeat instances at its exact depth
rep_cols <- function(cols, field) {
  d <- field_depth[field]
  d <- if (is.na(d)) 0L else d
  str_subset(cols, paste0("^", field, strrep("_\\d+", d), "$"))
}

# Long table of a repeat field: KEY, i1 (, i2), <field>; answered instances only
rep_long <- function(df, field) {
  d <- field_depth[[field]]
  idx <- paste0("i", seq_len(d))
  cols <- rep_cols(names(df), field)
  if (length(cols) == 0) {
    return(tibble(KEY = character(), !!!set_names(rep(list(integer()), d), idx), !!field := character()))
  }
  long <- df |>
    select(KEY, all_of(cols)) |>
    pivot_longer(all_of(cols), names_to = "column", values_to = field) |>
    filter(!is.na(.data[[field]]), as.character(.data[[field]]) != "")
  sfx <- str_split_fixed(str_sub(long$column, nchar(field) + 2), "_", d)
  for (k in seq_len(d)) long[[idx[k]]] <- as.integer(sfx[, k])
  long |> select(KEY, all_of(idx), all_of(field))
}

# Several fields of the same repeat group side by side
rep_wide <- function(df, fields) {
  idx <- paste0("i", seq_len(field_depth[[fields[1]]]))
  map(fields, \(f) rep_long(df, f)) |> reduce(full_join, by = c("KEY", idx))
}

has_code <- function(multi, code) map_lgl(str_split(coalesce(multi, ""), " "), \(x) code %in% x)
n_inst   <- function(long, i = "i1") if (nrow(long) == 0) 0L else max(long[[i]])


# ---- 2. Data loading ---------------------------------------------------------

form_survey <- read_excel(form_path, "survey", col_types = "text", .name_repair = "unique_quiet") |>
  filter(!is.na(type)) |>
  mutate(type = str_squish(type), name = str_squish(name),
         depth = cumsum(type == "begin repeat") - cumsum(type == "end repeat"))

form_fields_only  <- form_survey |> filter(!type %in% c("begin repeat", "end repeat", "begin group", "end group", "note"))
field_depth       <- set_names(as.integer(form_fields_only$depth), form_fields_only$name)
continuous_fields <- form_survey$name[word(form_survey$type, 1) %in% c("integer", "decimal")]
repeat_fields     <- names(field_depth)[field_depth > 0]

export_path <- file.path(data_dir, paste0(FORM_ID, "_WIDE.csv"))
raw <- read_csv(export_path, col_types = cols(.default = col_character()), show_col_types = FALSE)
assert_no_identifying_cols(names(raw), "Survey export")

enumerators     <- read_preload("enumerators.csv") |> select(enumeratorid_key, enumerator_key)
villages        <- read_preload("scope_villages.csv") |>
  select(villageid_key, district_key, sector_key, cell_key, village_key)
household_frame <- read_preload("household_frame.csv") |> select(householdid_key, stove_type)

message(sprintf("      export: %d submissions x %d columns", nrow(raw), ncol(raw)))


# ---- 3. Construct ------------------------------------------------------------

hfc_constr <- raw |>
  mutate(across(c(SubmissionDate, starttime, endtime), \(x) with_tz(mdy_hms(x, tz = EXPORT_TZ), TZ)),
         submissiondate = as_date(SubmissionDate),
         startdate = as_date(starttime),
         enddate = as_date(endtime)) |>
  filter(startdate >= FIELDWORK_START)

# Continuous fields (and their repeat instances) -> numbers; special codes -> NA
cont_cols <- unlist(map(continuous_fields, \(f) rep_cols(names(hfc_constr), f)))
hfc_constr <- hfc_constr |>
  mutate(across(all_of(cont_cols), \(x) { v <- suppressWarnings(as.numeric(x)); if_else(v %in% SPECIAL_CODES, NA_real_, v) }),
         survey_duration = suppressWarnings(as.numeric(survey_duration)),
         formdef_version = suppressWarnings(as.numeric(formdef_version)))

hfc_constr <- hfc_constr |>
  left_join(enumerators, by = c("enumerator" = "enumeratorid_key")) |>
  left_join(villages, by = c("village" = "villageid_key")) |>
  left_join(household_frame, by = c("hh_id" = "householdid_key")) |>
  mutate(stove_group = unname(STOVE_TYPES[stove_type]),
         located = locate %in% "1",
         consented = consent %in% "1",
         # The form has no finish field: observe_stoves is the last required question
         complete = located & consented & !is.na(observe_stoves),
         survey_min = survey_duration / 60)

n_no_stratum <- sum(is.na(hfc_constr$stove_group))
if (n_no_stratum > 0) warning(n_no_stratum, " submission(s) have an hh_id not in household_frame.csv (no stove type).")


# ---- 4. Derived variables from the repeat groups ------------------------------

## 4.1 Stove loop (one row per submission x stove)
stoves_long <- rep_wide(hfc_constr, c("stove_type", "stove_working", "days_used_7d", "cooking_events_7d",
                                      "cooking_share", "stove_fuels")) |>
  filter(!is.na(stove_type)) |>
  left_join(hfc_constr |> select(KEY, program_stove_type), by = "KEY") |>
  mutate(days = as.numeric(days_used_7d), events = as.numeric(cooking_events_7d),
         used = coalesce(days, 0) > 0,
         share_mid = unname(SHARE_MID[cooking_share]),
         is_prog = stove_type == coalesce(program_stove_type, ""),
         fuel_mismatch = used & !is.na(stove_fuels) &
           ((stove_type %in% c("1", "2") & !has_code(stove_fuels, "3")) |
              (stove_type == "3" & !has_code(stove_fuels, "6"))))

stove_hh <- stoves_long |>
  summarise(n_stoves_used_7d      = sum(used),
            share_sum             = if (any(used)) sum(share_mid[used], na.rm = TRUE) else NA_real_,
            max_share_used        = if (any(used)) max(share_mid[used], na.rm = TRUE) else NA_real_,
            any_share_all         = any(cooking_share %in% "7" & used),
            trad_used_7d          = any(stove_type %in% TRADITIONAL_STOVES & used),
            firewood_source       = any(has_code(stove_fuels, "1")),
            n_events_below_days   = sum(used & !is.na(events) & events < days),
            n_used_not_working    = sum(used & stove_working %in% "0"),
            n_stove_fuel_mismatch = sum(fuel_mismatch),
            .by = KEY)

prog_hh <- stoves_long |>
  filter(is_prog) |>
  arrange(KEY, i1) |>
  distinct(KEY, .keep_all = TRUE) |>
  transmute(KEY, prog_in_loop = TRUE, prog_working = stove_working, prog_days_7d = days,
            prog_events_7d = events, prog_share = share_mid)

## 4.2 Fuel loop, nested in the stove loop (one row per submission x stove x fuel)
fuels_long <- rep_wide(hfc_constr, c("fuel_type", "fuel_unit", "fuel_qty_7d", "fuel_spend_7d", "fuel_own_7d")) |>
  mutate(qty = as.numeric(fuel_qty_7d), own = as.numeric(fuel_own_7d))
fuel_hh <- fuels_long |>
  summarise(kwh_nonelec     = sum(fuel_unit %in% "6" & !fuel_type %in% "6"),
            cyl_nongas      = sum(fuel_unit %in% "5" & !fuel_type %in% "3"),
            n_own_above_qty = sum(!is.na(own) & !is.na(qty) & own > qty),
            .by = KEY)

## 4.3 Household roster and time use
roster_long <- rep_wide(hfc_constr, c("rel_to_hhh", "member_gender", "member_age"))
roster_hh <- roster_long |>
  summarise(n_roster = n(), n_heads = sum(rel_to_hhh %in% "1"), .by = KEY)

cook_long <- rep_wide(hfc_constr, c("cooking_member_index", "cooking_member_age", "cooking_hours_7d"))
cook_hh <- cook_long |>
  summarise(max_cook_hours = suppressWarnings(max(as.numeric(cooking_hours_7d), na.rm = TRUE)),
            n_child_cook = sum(as.numeric(cooking_member_age) < CHILD_COOK_AGE, na.rm = TRUE),
            .by = KEY) |>
  mutate(max_cook_hours = if_else(is.finite(max_cook_hours), max_cook_hours, NA_real_))

## 4.4 Back to one row per submission
hfc_constr <- hfc_constr |>
  left_join(stove_hh, by = "KEY") |>
  left_join(prog_hh, by = "KEY") |>
  left_join(fuel_hh, by = "KEY") |>
  left_join(roster_hh, by = "KEY") |>
  left_join(cook_hh, by = "KEY") |>
  mutate(across(c(n_stoves_used_7d, n_events_below_days, n_used_not_working, n_stove_fuel_mismatch,
                  kwh_nonelec, cyl_nongas, n_own_above_qty, n_roster, n_heads, n_child_cook), \(x) coalesce(x, 0L)),
         across(c(any_share_all, trad_used_7d, firewood_source, prog_in_loop), \(x) coalesce(x, FALSE)),
         prog_top_share = coalesce(prog_days_7d > 0 & prog_share >= max_share_used, FALSE),
         any_cook       = coalesce(members_cooking, "") != "",
         any_firewood   = coalesce(members_firewood, "") != "",
         # Operational days per year, annualised from the last 7 days (no seasonality in the form)
         op_days_year   = coalesce(prog_days_7d, 0) * 365 / 7)


# ---- 5. Durations and duplicates ---------------------------------------------

duration_data <- hfc_constr |>
  select(KEY, hh_id, submissiondate, district_key, sector_key, cell_key, village_key,
         enumerator, enumerator_key, stove_group, consent, survey_min) |>
  arrange(submissiondate)

duplicates <- hfc_constr |>
  filter(consented) |>
  mutate(n = n(), .by = hh_id) |>
  filter(n > 1) |>
  select(KEY, hh_id, n, enumerator, enumerator_key, district_key, sector_key, cell_key, village_key,
         starttime, submissiondate) |>
  arrange(hh_id, starttime)

add_flags(duplicates, "2.hfc_construct: duplicate", detail = "multiple consented submissions for hh_id")

hfc_outputs$duration_data <- duration_data
hfc_outputs$duplicates    <- duplicates

message(sprintf("      %d submissions after %s; %d duplicate submissions (%d households); repeats: %d stoves, %d fuels per stove, %d members",
                nrow(hfc_constr), FIELDWORK_START, nrow(duplicates), n_distinct(duplicates$hh_id),
                n_inst(stoves_long), n_inst(fuels_long, "i2"), n_inst(roster_long)))
