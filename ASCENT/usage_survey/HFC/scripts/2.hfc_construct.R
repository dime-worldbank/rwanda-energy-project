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
# Derived variables (one row per submission):
#   prog_*            answers for the program stove, taken from the stove loop
#                     instance whose stove_type = program_stove_type
#   n_stoves_used_7d  stoves with days_used_7d > 0
#   share_sum         sum of cooking_share midpoints (SHARE_MID) over those stoves
#   n_primary         stoves reported as primary
#   trad_used_7d      traditional stoves (TRADITIONAL_STOVES) used in the last 7 days
#   n_heads, n_child_roster, any_cook, max_cook_hours, any_firewood   (time-use roster)
#   kwh_nonelec, cyl_nongas   fuel instances with a unit that does not fit the fuel
#
# Sourced by 1.hfc_main.R. Outputs: hfc_constr, duration_data, duplicates
#
# Author: Tanay (using Claude) · Last modified: 2026-11-04
# ==============================================================================


# ---- 1. Helper functions -----------------------------------------------------

read_preload <- function(file) {
  df <- read_csv(file.path(preload_dir, file), col_types = cols(.default = col_character()),
                 show_col_types = FALSE, name_repair = "unique_quiet")
  assert_no_identifying_cols(names(df), file)
  df
}

# Number of instances of a repeat field in the wide export (<field>_1 .. <field>_k)
n_inst <- function(df, field) {
  k <- as.integer(str_extract(str_subset(names(df), paste0("^", field, "_\\d+$")), "\\d+$"))
  max(c(1L, k))
}

# n x k text matrix of a repeat field; instances missing from the export are NA
rep_mat <- function(df, field, k) {
  out <- matrix(NA_character_, nrow(df), k)
  for (j in seq_len(k)) {
    col <- paste0(field, "_", j)
    if (col %in% names(df)) out[, j] <- as.character(df[[col]])
  }
  out
}
num <- function(m) { storage.mode(m) <- "double"; m }
row_max <- function(m) { x <- suppressWarnings(apply(m, 1, max, na.rm = TRUE)); x[!is.finite(x)] <- NA; x }
has_code <- function(multi, code) map_lgl(str_split(coalesce(multi, ""), " "), \(x) code %in% x)


# ---- 2. Data loading ---------------------------------------------------------

form_survey <- read_excel(form_path, "survey", col_types = "text", .name_repair = "unique_quiet") |>
  filter(!is.na(type)) |>
  mutate(type = str_squish(type), name = str_squish(name))

rep_rows <- cumsum(form_survey$type == "begin repeat") - cumsum(form_survey$type == "end repeat") > 0
continuous_fields <- form_survey$name[word(form_survey$type, 1) %in% c("integer", "decimal")]
repeat_fields     <- form_survey$name[rep_rows & !form_survey$type %in% c("begin repeat", "note")]

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
cont_cols <- names(hfc_constr)[names(hfc_constr) %in% continuous_fields |
                                 str_remove(names(hfc_constr), "_\\d+$") %in% intersect(continuous_fields, repeat_fields)]
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
         complete = located & consented & finish %in% "1",
         survey_min = survey_duration / 60)

n_no_stratum <- sum(is.na(hfc_constr$stove_group))
if (n_no_stratum > 0) warning(n_no_stratum, " submission(s) have an hh_id not in household_frame.csv (no stove type).")


# ---- 4. Derived variables from the repeat groups ------------------------------

n <- nrow(hfc_constr)
K_STOVE <- n_inst(hfc_constr, "stove_type")
K_FUEL  <- n_inst(hfc_constr, "fuel_type")
K_TU    <- n_inst(hfc_constr, "tu_rel")

S  <- rep_mat(hfc_constr, "stove_type", K_STOVE)
D  <- num(rep_mat(hfc_constr, "days_used_7d", K_STOVE))
E  <- num(rep_mat(hfc_constr, "cooking_events_7d", K_STOVE))
SH <- rep_mat(hfc_constr, "cooking_share", K_STOVE)
R  <- rep_mat(hfc_constr, "stove_role", K_STOVE)
W  <- rep_mat(hfc_constr, "stove_working", K_STOVE)

# Loop instance of the program stove (first match)
prog <- hfc_constr$program_stove_type
prog_j <- rep(NA_integer_, n)
for (j in seq_len(K_STOVE)) prog_j[is.na(prog_j) & !is.na(prog) & !is.na(S[, j]) & S[, j] == prog] <- j
prog_val <- function(m) m[cbind(seq_len(n), prog_j)]

SHN <- matrix(unname(SHARE_MID[SH]), n, K_STOVE)
used <- !is.na(D) & D > 0

REL  <- rep_mat(hfc_constr, "tu_rel", K_TU)
AGE  <- num(rep_mat(hfc_constr, "tu_age", K_TU))
COOK <- rep_mat(hfc_constr, "tu_cook", K_TU)
CH   <- num(rep_mat(hfc_constr, "tu_cook_hours", K_TU))
FW   <- rep_mat(hfc_constr, "tu_firewood", K_TU)

FT <- rep_mat(hfc_constr, "fuel_type", K_FUEL)
FU <- rep_mat(hfc_constr, "fuel_unit", K_FUEL)

hfc_constr <- hfc_constr |>
  mutate(prog_in_loop     = !is.na(prog_j),
         prog_working     = prog_val(W),
         prog_days_7d     = prog_val(D),
         prog_events_7d   = prog_val(E),
         prog_share       = unname(SHARE_MID[prog_val(SH)]),
         prog_role        = prog_val(R),
         n_stoves_used_7d = rowSums(used),
         share_sum        = if_else(n_stoves_used_7d > 0, rowSums(SHN * used, na.rm = TRUE), NA_real_),
         any_share_all    = rowSums(SH == "7" & used, na.rm = TRUE) > 0,
         n_primary        = rowSums(R == "1", na.rm = TRUE),
         trad_used_7d     = rowSums(matrix(S %in% TRADITIONAL_STOVES, n, K_STOVE) & used) > 0,
         primary_in_loop  = map2_lgl(primary_stove, str_split(coalesce(stoves_used, ""), " "), \(p, s) p %in% s),
         firewood_source  = has_code(energy_sources, "1"),
         n_roster         = rowSums(!is.na(REL)),
         n_heads          = rowSums(REL == "1", na.rm = TRUE),
         n_child_roster   = rowSums(AGE < 18, na.rm = TRUE),
         any_cook         = rowSums(COOK == "1", na.rm = TRUE) > 0,
         max_cook_hours   = row_max(CH),
         any_firewood     = rowSums(FW == "1", na.rm = TRUE) > 0,
         kwh_nonelec      = rowSums(FU == "6" & FT != "6", na.rm = TRUE),
         cyl_nongas       = rowSums(FU == "5" & FT != "3", na.rm = TRUE),
         # Operational days per year from the seasonality block (0 if the stove was never used)
         op_days_year     = case_when(started_program_stove %in% "0" ~ 0,
                                      months_regular_use %in% 0 ~ 0,
                                      .default = months_regular_use * days_week_typical * 52 / 12))


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

message(sprintf("      %d submissions after %s; %d duplicate submissions (%d households); repeats: %d stoves, %d fuels, %d members",
                nrow(hfc_constr), FIELDWORK_START, nrow(duplicates), n_distinct(duplicates$hh_id),
                K_STOVE, K_FUEL, K_TU))
