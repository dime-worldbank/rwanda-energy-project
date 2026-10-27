# ==============================================================================
# 2.hfc_construct.R
# Midline HFC: load the SurveyCTO export, parse dates, filter fieldwork start,
# special codes -> NA (continuous only), join enumerator / admin names and
# treatment arm, module durations, duplicates.
#
# Sourced by 1.hfc_main.R (uses its paths, constants, add_flags() and hfc_outputs).
# Outputs: hfc_constr, duration_data, duplicates
#
# Author: Yeji (using Claude) · Last modified: 2026-10-21
# ==============================================================================


# ---- 1. Config / constants ---------------------------------------------------

# Deidentified HFC: retain household IDs and survey metadata only; drop names, phones, GPS and media.
ID_COLS <- c("KEY", "hh_id", "district", "sector", "cell", "village", "enumerator")
DROP_IDENTIFYING_COLS <- c("first_name", "last_name", "hh_head_name", "deviceid",
                          "devicephonenum", "pull_phonenumber", "phonenumber", "phonenumber_revise",
                          "pull_second_phonenumber", "second_phonenumber", "second_phonenumber_revise",
                          "meter_number", "meter_photo", "readyboard_photo", "shs_photo",
                          "audio", "text_audit", "location", "obs_gps", "location-Accuracy", "obs_gps-Accuracy")
MODULES <- c(a = "stamp_a", b = "stamp_b", c = "stamp_c", nudge = "stamp_nudge")


# ---- 2. Helper functions -----------------------------------------------------

read_preload <- function(file) {
  df <- read_csv(file.path(preload_dir, file), col_types = cols(.default = col_character()),
                 show_col_types = FALSE, name_repair = "unique_quiet") |>
    select(-starts_with("..."))
  df |> select(-any_of(DROP_IDENTIFYING_COLS))
}


# ---- 3. Data loading ---------------------------------------------------------

form_survey <- read_excel(form_path, "survey", col_types = "text", .name_repair = "unique_quiet") |>
  filter(!is.na(type)) |>
  mutate(type = str_squish(type), name = str_squish(name))

# Continuous fields (integer / decimal) and repeat fields, from the form
rep_rows <- cumsum(form_survey$type == "begin repeat") - cumsum(form_survey$type == "end repeat") > 0
continuous_fields <- form_survey$name[word(form_survey$type, 1) %in% c("integer", "decimal")]
repeat_fields     <- form_survey$name[rep_rows & !form_survey$type %in% c("begin repeat", "note")]

# Yes/no follow-up asked right after each device question (e.g. meter_confirm_working); names read from the form
FOLLOWUP <- map_chr(c(meter = "meter_confirm", readyboard = "readyboard_confirm",
                      wiring = "additional_wire_confirm", shs = "shs_confirm"), \(p) {
  x <- form_survey$name[form_survey$type == "select_one yesno" &
                          str_detect(str_squish(coalesce(form_survey$relevance, "")), sprintf("^\\$\\{%s\\}\\s*=\\s*1$", p))]
  if (length(x) == 0) NA_character_ else x[1]
})

raw <- read_csv(file.path(data_dir, paste0(FORM_ID, "_WIDE.csv")),
                col_types = cols(.default = col_guess(), !!!setNames(rep(list(col_character()), length(ID_COLS)), ID_COLS)),
                show_col_types = FALSE, guess_max = 10000) |>
  select(-any_of(DROP_IDENTIFYING_COLS))

enumerators  <- read_preload(ENUM_FILE) |> select(enumeratorid_key, enumerator_key)
villages     <- read_preload("scope_villages.csv") |>
  select(villageid_key, district_key, sector_key, cell_key, village_key)
contact_info <- read_preload("contact_info.csv") |> select(householdid_key, treat)
shs_eligible <- read_preload("shs_eligible.csv") |> select(householdid_key)

message(sprintf("      export: %d submissions x %d columns", nrow(raw), ncol(raw)))

# One-time preload check: SHS-eligible households must be in an SHS arm
elig_wrong_arm <- shs_eligible |>
  left_join(contact_info, by = "householdid_key") |>
  filter(!treat %in% SHS_ARMS)
if (nrow(elig_wrong_arm) > 0) {
  warning(nrow(elig_wrong_arm), " household(s) in shs_eligible.csv are not in T2/T3: ",
          paste(elig_wrong_arm$householdid_key, collapse = ", "), ". Check the preload.")
}


# ---- 4. Construct ------------------------------------------------------------

# Dates (bug 3 fixed: one documented format; TBD confirm on a real midline export)
hfc_constr <- raw |>
  mutate(across(c(SubmissionDate, starttime, endtime), \(x) mdy_hms(x, tz = TZ)),
         submissiondate = as_date(SubmissionDate),
         startdate = as_date(starttime),
         enddate = as_date(endtime)) |>
  filter(startdate >= FIELDWORK_START)

# Special codes -> NA in continuous variables only (including repeat instances)
cont_cols <- names(hfc_constr)[names(hfc_constr) %in% continuous_fields |
                                 str_remove(names(hfc_constr), "_\\d+$") %in% intersect(continuous_fields, repeat_fields)]
hfc_constr <- hfc_constr |>
  mutate(across(all_of(cont_cols), \(x) if_else(as.numeric(x) %in% SPECIAL_CODES, NA_real_, as.numeric(x))))

# Names, treatment arm, preload SHS eligibility
hfc_constr <- hfc_constr |>
  left_join(enumerators, by = c("enumerator" = "enumeratorid_key")) |>
  left_join(villages, by = c("village" = "villageid_key")) |>
  left_join(contact_info, by = c("hh_id" = "householdid_key")) |>
  mutate(preload_shs_eligible = hh_id %in% shs_eligible$householdid_key,
         # SHS nudge asked (relevance of the form's nosolar group)
         nudge_eligible = pull_shs_eligible %in% 1 & shs_confirm %in% 0 & electrified %in% 0,
         located = locate %in% 1,
         consented = consent %in% 1,
         complete = located & consented & finish %in% 1,
         survey_min = survey_duration / 60)

n_no_arm <- sum(is.na(hfc_constr$treat))
if (n_no_arm > 0) warning(n_no_arm, " submissions have an hh_id not found in contact_info.csv (no treatment arm).")


# ---- 5. Module durations -----------------------------------------------------

# Minutes from stamp pairs (seconds since form start); negative durations are flagged, then set to NA
for (m in names(MODULES)) {
  s1 <- paste0(MODULES[[m]], "_1"); s2 <- paste0(MODULES[[m]], "_2")
  hfc_constr[[paste0("dur_", m)]] <- (hfc_constr[[s2]] - hfc_constr[[s1]]) / 60
}
dur_cols <- paste0("dur_", names(MODULES))
hfc_constr <- hfc_constr |>
  mutate(negative_mods = pmap_chr(across(all_of(dur_cols)), \(...) {
    d <- c(...)
    neg <- names(d)[!is.na(d) & d < 0]
    if (length(neg) == 0) NA_character_ else paste(str_remove(neg, "^dur_"), collapse = ", ")
  }),
  across(all_of(dur_cols), \(x) if_else(x < 0, NA_real_, x)))

duration_data <- hfc_constr |>
  select(KEY, hh_id, submissiondate, district_key, sector_key, cell_key, village_key,
         enumerator, enumerator_key, treat, consent, preload_shs_eligible, nudge_eligible,
         survey_min, all_of(dur_cols), negative_mods) |>
  arrange(submissiondate)


# ---- 6. Duplicates -----------------------------------------------------------

# Same hh_id with more than one consented submission. name_differs: the head name typed at the
# door differs between submissions (likely a wrong hh_id) vs. a true repeat interview.
duplicates <- hfc_constr |>
  filter(consented) |>
  mutate(n = n(), .by = hh_id) |>
  filter(n > 1) |>
  select(KEY, hh_id, n, enumerator, enumerator_key,
         district_key, sector_key, cell_key, village_key, starttime, submissiondate) |>
  arrange(hh_id, starttime)

add_flags(duplicates, "2.hfc_construct: duplicate",
          detail = "multiple consented submissions for hh_id")

hfc_outputs$duration_data <- duration_data
hfc_outputs$duplicates    <- duplicates

message(sprintf("      %d submissions after %s; %d duplicate submissions (%d households)",
                nrow(hfc_constr), FIELDWORK_START, nrow(duplicates), n_distinct(duplicates$hh_id)))
