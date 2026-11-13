# ==============================================================================
# 0.generate_synthetic_data.R
# Synthetic SurveyCTO export, preload files and answer key for testing the clean
# cooking HFC (ASCENT/usage_survey/HFC/scripts).
#
# SYNTHETIC DATA ONLY: every name, date of birth, phone number and ID is invented
# here. No real household data is read; only the form definition (choice codes,
# field names, version).
#
# Follows the form structure of clean_cooking_questionnaire (2026-11-04 revision):
#   hh_roster (A1_1 members), stove_loop with a nested stove_fuel_loop (WIDE
#   columns <field>_<stove>_<fuel>), cooking_time_loop and firewood_time_loop.
#
# Every clean household is internally consistent, so it raises no flag. Problems
# are planted one per household (scenario table below) and written to the answer
# key. Expected outliers are computed here with the same rule as 4.hfc_outliers.R
# (|z| > 3 on log(1 + x)), as an independent second implementation.
#
# Run from the repo root (writes to the test folders of SHARED_ASCENT_CC):
#   Rscript ASCENT/usage_survey/HFC/test/0.generate_synthetic_data.R
#
# Author: Tanay (using Claude) ?? Last modified: 2026-11-05 (Yeji: new form structure)
# ==============================================================================


# ---- 1. Setup ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
})
options(scipen = 999)
if (!file.exists("PATHS.R")) stop("Run this script from the repo root (PATHS.R not found).")
source("PATHS.R")
HFC_MODE <- "test"
source("ASCENT/usage_survey/HFC/scripts/hfc_config.R")
set.seed(20261104)


# ---- 2. Config / constants ---------------------------------------------------

FIELDWORK_DAYS <- as.Date("2026-11-09") + 0:4      # static: synthetic fieldwork week (Mon-Fri)
stopifnot(min(FIELDWORK_DAYS) == FIELDWORK_START)
TZ <- "Africa/Kigali"
OUTLIER_SD <- 3; MIN_GROUP_N <- 10                 # as in 4.hfc_outliers.R
CHILD_COOK_AGE <- 10                               # as in 1.hfc_main.R
N_PER_STOVE <- 40                                  # households per preload stove type (1 LPG, 2 PAYG, 3 EPC)
N_ENUM <- 6
ENUM_LOW_CONSENT <- "6"                            # gets every refusal
ENUM_SHORT <- "3"                                  # gets the 'many short surveys'

form_version <- read_excel(form_path, "settings", col_types = "text")$version[1]   # as in the form (should be numeric)
choices <- read_excel(form_path, "choices", col_types = "text") |> filter(!is.na(list_name))
form_survey <- read_excel(form_path, "survey", col_types = "text") |> filter(!is.na(type))

districts <- tibble(districtid_key = c("31", "36"), district_key = c("Karongi", "Rusizi"))
villages <- expand_grid(districts, v = 1:6) |>
  mutate(sectorid_key = paste0(districtid_key, "0", ceiling(v / 3)),
         cellid_key = paste0(sectorid_key, "0", v),
         villageid_key = paste0(cellid_key, "01"),
         sector_key = paste0(district_key, " sector ", ceiling(v / 3)),
         cell_key = paste0("Cell ", v), village_key = paste0(district_key, " village ", v)) |>
  select(-v)
enums <- tibble(enumeratorid_key = as.character(1:N_ENUM), enumerator_key = paste("Enumerator", LETTERS[1:N_ENUM]),
                districtid_key = rep(districts$districtid_key, each = N_ENUM / 2))

# Scenarios: one household each unless n > 1. stove = preload stove type (NA = any)
scenarios <- tribble(
  ~scenario,                         ~n, ~stove,
  "not_attempted",                    6, NA,
  "not_located",                      3, NA,
  "refused",                          3, NA,
  "partial",                          1, NA,     # consented, stopped after the roster (not complete)
  "duplicate",                        1, "1",
  "many_short",                       4, NA,
  "duration_short",                   1, NA,
  "duration_long",                    1, NA,
  "start_end_date",                   1, NA,
  "end_submission_date",              1, NA,
  "outlier_members",                  1, NA,
  "outlier_events",                   1, NA,
  "outlier_gas_qty",                  1, "1",
  "outlier_gas_spend",                1, "2",
  "share_sum_off",                    1, NA,
  "share_all_with_other_stove",       1, NA,
  "events_below_days",                1, NA,
  "stove_fuel_mismatch",              1, "3",    # EPC used with gas only
  "used_but_not_working",             1, NA,
  "fuel_unit_kwh_not_electricity",    1, "1",
  "fuel_unit_cylinder_not_gas",       1, "3",
  "fuel_own_above_qty",               1, NA,
  "heads_not_one",                    1, NA,
  "nobody_cooks",                     1, NA,
  "cook_hours_high",                  1, NA,
  "child_cook",                       1, NA,
  "firewood_not_source",              1, NA,
  "program_stove_not_confirmed",      1, "1",
  "program_stove_not_used_30d",       1, NA,
  "program_stove_not_observed",       1, NA
)
# Logic flags each scenario must raise (besides outliers, computed later)
EXPECTED_LOGIC <- c(
  duration_short = "duration_short", many_short = "duration_short", duration_long = "duration_long",
  start_end_date = "start_end_date", end_submission_date = "end_submission_date",
  share_sum_off = "share_sum_off", share_all_with_other_stove = "share_all_with_other_stove",
  events_below_days = "events_below_days", stove_fuel_mismatch = "stove_fuel_mismatch",
  used_but_not_working = "used_but_not_working",
  fuel_unit_kwh_not_electricity = "fuel_unit_kwh_not_electricity",
  fuel_unit_cylinder_not_gas = "fuel_unit_cylinder_not_gas", fuel_own_above_qty = "fuel_own_above_qty",
  heads_not_one = "heads_not_one", nobody_cooks = "nobody_cooks", cook_hours_high = "cook_hours_high",
  child_cook = "child_cook", firewood_not_source = "firewood_not_source",
  program_stove_not_confirmed = "program_stove_not_confirmed",
  program_stove_not_used_30d = "program_stove_not_used_30d", program_stove_not_observed = "program_stove_not_observed"
)
FIRST <- c("Alice", "Jean", "Marie", "Eric", "Claudine", "Patrick", "Diane", "Olivier", "Grace", "Emmanuel")
LAST  <- c("Testname", "Sample", "Fictive", "Example", "Placeholder")


# ---- 3. Households and preload -------------------------------------------------

hh <- tibble(stove_type = rep(c("1", "2", "3"), each = N_PER_STOVE)) |>
  mutate(villageid_key = sample(villages$villageid_key, n(), replace = TRUE)) |>
  mutate(householdid_key = paste0(villageid_key, sprintf("%03d", row_number())), .by = villageid_key) |>
  left_join(villages |> select(villageid_key, districtid_key), by = "villageid_key") |>
  mutate(scenario = "clean")

# Assign scenarios to distinct households (matching the stove type where one is required)
for (i in seq_len(nrow(scenarios))) {
  free <- which(hh$scenario == "clean" & (is.na(scenarios$stove[i]) | hh$stove_type == scenarios$stove[i]))
  hh$scenario[sample(free, scenarios$n[i])] <- scenarios$scenario[i]
}

# Enumerator: one of the district's enumerators; planted cases go to the planted enumerator
hh <- hh |>
  mutate(enumerator = map_chr(districtid_key, \(d) sample(enums$enumeratorid_key[enums$districtid_key == d], 1)),
         enumerator = case_when(scenario == "refused" ~ ENUM_LOW_CONSENT,
                                scenario == "many_short" ~ ENUM_SHORT,
                                scenario == "duration_short" ~ "1",
                                enumerator %in% c(ENUM_LOW_CONSENT, ENUM_SHORT) & scenario != "clean" ~ "1",
                                .default = enumerator))
# ENUM_LOW_CONSENT needs enough located surveys for a clear rate (< 95%) and must work in its district
stopifnot(sum(hh$enumerator == ENUM_LOW_CONSENT & hh$scenario != "not_attempted") >= 8)

preload <- list(
  test_enumerators.csv = enums |> left_join(districts, by = "districtid_key"),
  scope_villages.csv   = villages,
  household_head.csv   = hh |> transmute(householdid_key, household_id = householdid_key, villageid_key,
                                         hh_head_name = paste(sample(FIRST, n(), TRUE), sample(LAST, n(), TRUE)),
                                         phone_clean = sprintf("0700%06d", sample(1e5:999999, n())),
                                         phone2_clean = if_else(runif(n()) < 0.3, sprintf("0700%06d", sample(1e5:999999, n())), NA_character_)),
  stove_type.csv = hh |> transmute(householdid_key, stove_type = unname(c("1" = "LPG", "2" = "PAYG LPG", "3" = "EPC")[stove_type]), stove_type_code = stove_type, product_name = if_else(stove_type == "3", "Panda EPC", "Hello Gas II"), product_name_code = if_else(stove_type == "3", "P003", "P014"), stove_year = sample(c(2025, 2026), n(), TRUE))
)


# ---- 4. One submission -------------------------------------------------------

u <- function(a, b) round(runif(1, a, b))
FUEL_OF <- c("1" = "3", "2" = "3", "3" = "6")       # program stove -> energy source (gas / electricity)
UNIT_OF <- c("3" = "1", "6" = "6", "1" = "3")       # energy source -> usual unit (kg / kWh / bundle)

make_sub <- function(h, day, start_hm, dur_min, k = 1) {
  sc <- h$scenario; p <- h$stove_type
  start <- as.POSIXct(paste(day, start_hm), tz = TZ)
  if (sc == "start_end_date") start <- as.POSIXct(paste(day, "23:40"), tz = TZ)
  end <- start + dur_min * 60
  sub_delay <- if (sc == "end_submission_date") 3 * 86400 else 3600 * u(1, 5)
  s <- list(KEY = paste0("uuid:", h$householdid_key, "-", k), SubmissionDate = end + sub_delay,
            starttime = start, endtime = end, formdef_version = form_version,
            deviceid = paste0("dev", u(1e5, 9e5)), subscriberid = "fake", simid = "fake", devicephonenum = "0700000000",
            survey_duration = dur_min * 60, enumerator = h$enumerator, district = h$districtid_key,
            sector = substr(h$villageid_key, 1, 4), cell = substr(h$villageid_key, 1, 6), village = h$villageid_key,
            hh_id = h$householdid_key)
  if (sc == "not_located") { s$locate <- "0"; return(s) }
  s$locate <- "1"
  if (sc == "refused") { s$consent <- "0"; return(s) }
  s$consent <- "1"

  # Section A: roster. Head first, then children, then adults (spouse first)
  m <- if (sc == "outlier_members") 19 else u(2, 7)
  c_n <- u(0, m - 1)
  if (sc == "child_cook") c_n <- max(1, c_n)
  n_ad <- m - 1 - c_n
  rel <- c("1", rep("3", c_n), rep(c("2", "8"), length.out = n_ad)[seq_len(n_ad)])
  if (sc == "heads_not_one") rel[m] <- "1"
  age <- c(u(30, 70), round(runif(c_n, 3, 17)), round(runif(n_ad, 18, 60)))
  if (sc == "child_cook") age[2] <- CHILD_COOK_AGE - 2
  dob <- as.Date(day) - round(age * 365.25 + runif(m, 10, 300))
  age <- floor(as.numeric(as.Date(day) - dob) / 365.25)       # as the form's member_age calculate
  sex <- c("2", as.character(sample(1:2, m - 1, TRUE)))
  first <- sample(FIRST, m, TRUE); last <- sample(LAST, 1)

  s$A1_1 <- m
  s$respondent_selection <- "1"
  for (j in seq_len(m)) {
    s[[paste0("member_firstname_", j)]] <- first[j]
    s[[paste0("member_lastname_", j)]] <- last
    s[[paste0("member_name_", j)]] <- paste(first[j], last)
    s[[paste0("rel_to_hhh_", j)]] <- rel[j]
    s[[paste0("member_gender_", j)]] <- sex[j]
    s[[paste0("member_dob_", j)]] <- format(dob[j], "%b %d, %Y")
    s[[paste0("member_age_", j)]] <- age[j]
  }
  if (sc == "partial") return(s)

  # Section B: program stove, stacking, stove loop with nested fuel loop
  confirm <- sc != "program_stove_not_confirmed"
  prog <- if (confirm) p else setdiff(c("1", "2", "3"), p)[1]
  in_loop <- sc != "program_stove_not_used_30d"
  stack <- in_loop && (runif(1) < 0.5 || sc == "share_all_with_other_stove") &&
    !sc %in% c("share_sum_off", "firewood_not_source")
  uses_firewood <- stack || !in_loop

  s$pull_stovetype <- p
  s$ascent_confirm <- if (confirm) "1" else "0"
  if (!confirm) s$received_stove_type <- prog
  s$program_stove_type <- prog

  stoves <- c(if (in_loop) prog, if (uses_firewood) "5")
  s$stoves_used <- paste(stoves, collapse = " ")
  for (j in seq_along(stoves)) {
    st <- stoves[j]; is_prog <- st == prog
    sx <- \(f) paste0(f, "_", j)
    days <- if (is_prog) u(3, 7) else u(1, 3)
    events <- if (sc == "outlier_events" && is_prog) 95 else if (sc == "events_below_days" && is_prog) days - 1 else days * u(1, 2)
    share <- if (is_prog) {
      if (sc == "share_sum_off") "4" else if (sc == "share_all_with_other_stove") "7" else if (stack) "5" else "7"
    } else if (!in_loop) "7" else if (sc == "share_all_with_other_stove") "2" else "3"
    fuel <- if (st == "5") "1" else if (sc == "stove_fuel_mismatch" && is_prog) "3" else FUEL_OF[[prog]]

    s[[sx("stove_type")]] <- st
    s[[sx("stove_working")]] <- if (sc == "used_but_not_working" && is_prog) "0" else "1"
    s[[sx("days_used_7d")]] <- days
    s[[sx("cooking_events_7d")]] <- events
    s[[sx("cooking_share")]] <- share
    s[[sx("cooking_purpose")]] <- if (is_prog) "1 2 3" else "5"
    s[[sx("stove_fuels")]] <- fuel

    # stove_fuel_loop: one fuel per stove (instance <stove>_1)
    fx <- \(f) paste0(f, "_", j, "_1")
    unit <- UNIT_OF[[fuel]]
    if (sc == "fuel_unit_kwh_not_electricity" && fuel == "3") unit <- "6"
    if (sc == "fuel_unit_cylinder_not_gas" && fuel == "6") unit <- "5"
    qty <- switch(fuel, "3" = u(2, 4), "6" = u(15, 30), "1" = u(2, 4))
    if (sc == "outlier_gas_qty" && fuel == "3") qty <- 60
    spend <- switch(fuel, "3" = u(4000, 7000), "6" = u(3000, 5000), "1" = u(0, 500))
    if (sc == "outlier_gas_spend" && fuel == "3") spend <- 400000
    own <- if (fuel == "1") qty else 0
    if (sc == "fuel_own_above_qty" && is_prog) own <- qty + 1
    s[[fx("fuel_type")]] <- fuel
    s[[fx("fuel_qty_7d")]] <- qty
    s[[fx("fuel_unit")]] <- unit
    s[[fx("fuel_spend_7d")]] <- spend
    s[[fx("fuel_own_7d")]] <- own
  }

  # Section D: time use (members picked from the roster)
  cook_j <- if (sc == "nobody_cooks") integer() else c(1L, if (n_ad > 0) c_n + 2L, if (sc == "child_cook") 2L)
  fw_j <- if (uses_firewood || sc == "firewood_not_source") 1L else integer()
  s$members_cooking <- paste(cook_j, collapse = " ")
  for (k2 in seq_along(cook_j)) {
    j <- cook_j[k2]; cx <- \(f) paste0(f, "_", k2)
    s[[cx("cooking_member_index")]] <- j
    s[[cx("cooking_member_name")]] <- paste(first[j], last)
    s[[cx("cooking_member_gender")]] <- sex[j]
    s[[cx("cooking_member_age")]] <- age[j]
    s[[cx("cooking_member_rel")]] <- rel[j]
    s[[cx("cooking_hours_7d")]] <- if (sc == "cook_hours_high" && k2 == 1) 84 else u(5, 20)
  }
  s$members_firewood <- paste(fw_j, collapse = " ")
  for (k2 in seq_along(fw_j)) {
    j <- fw_j[k2]; wx <- \(f) paste0(f, "_", k2)
    s[[wx("firewood_member_index")]] <- j
    s[[wx("firewood_member_name")]] <- paste(first[j], last)
    s[[wx("firewood_member_gender")]] <- sex[j]
    s[[wx("firewood_member_age")]] <- age[j]
    s[[wx("firewood_member_rel")]] <- rel[j]
    s[[wx("firewood_hours_7d")]] <- u(2, 10)
  }

  # Section E: validation
  s$observe_stoves <- "1"
  s$program_stove_seen <- if (sc == "program_stove_not_observed") "3" else "1"
  s$usage_comment <- "Free text that the HFC must drop"
  s
}


# ---- 5. Build the export -----------------------------------------------------

attempted <- hh |> filter(scenario != "not_attempted")
days <- sample(FIELDWORK_DAYS, nrow(attempted), replace = TRUE)
subs <- map(seq_len(nrow(attempted)), \(i) {
  h <- attempted[i, ]
  dur <- case_when(h$scenario == "many_short" ~ u(7, 9), h$scenario == "duration_short" ~ 6,
                   h$scenario == "duration_long" ~ 120, .default = u(20, 45))
  out <- list(make_sub(h, days[i], sprintf("%02d:%02d", u(8, 15), u(0, 59)), dur))
  if (h$scenario == "duplicate") {
    out[[2]] <- make_sub(h, min(days[i] + 1, max(FIELDWORK_DAYS)), "16:10", u(20, 45), k = 2)
  }
  out
}) |> list_flatten()

# SurveyCTO export format, in the time zone of the computer that downloads it (read back with EXPORT_TZ)
fmt_cto <- function(x) {
  x <- with_tz(x, Sys.timezone())
  sprintf("%s %d, %d %d:%s %s", month.abb[month(x)], mday(x), year(x),
          (hour(x) + 11) %% 12 + 1, format(x, "%M:%S"), if_else(hour(x) < 12, "AM", "PM"))
}
export <- map_dfr(subs, \(s) {
  for (f in c("SubmissionDate", "starttime", "endtime")) s[[f]] <- fmt_cto(s[[f]])
  as_tibble(map(s, as.character))
})


# ---- 6. Validate against the form --------------------------------------------

# Form field of an export column: strip repeat suffixes (_i, _i_j) until a form name matches.
# (A1_1 is itself a form name, so it is not stripped.)
known <- c("KEY", "SubmissionDate", form_survey$name, "formdef_version")
base_of <- function(cols) map_chr(cols, \(x) {
  b <- x
  while (!b %in% known && str_detect(b, "_\\d+$")) b <- str_remove(b, "_\\d+$")
  b
})
col_base <- base_of(names(export))

unknown <- setdiff(col_base, known)
if (length(unknown) > 0) stop("Synthetic columns not in the form: ", paste(unknown, collapse = ", "))

sel <- form_survey |> filter(str_starts(type, "select_one")) |> transmute(name, list = word(type, 2))
bad <- map_dfr(seq_len(nrow(sel)), \(i) {
  vals <- unique(na.omit(unlist(export[names(export)[col_base == sel$name[i]]])))
  ok <- choices$value[choices$list_name == sel$list[i]]
  if (length(vals) == 0 || sel$list[i] %in% c("enumerator", "district", "sector", "cell", "village", "hh_id")) return(NULL)
  tibble(field = sel$name[i], value = as.character(setdiff(vals, ok)))
})
if (nrow(bad) > 0) { print(bad); stop("Synthetic values not in the form's choice lists.") }

# Column order: form order, then repeat instance
sfx_key <- map_chr(str_sub(names(export), nchar(col_base) + 1), \(x) {
  p <- str_split_1(x, "_")[-1]
  paste(sprintf("%03d", as.integer(p)), collapse = "_")
})
export <- export[, order(match(col_base, known), sfx_key)]
col_base <- base_of(names(export))


# ---- 7. Answer key -------------------------------------------------------------

key_of <- \(sc) export |> filter(hh_id %in% hh$householdid_key[hh$scenario == sc]) |> select(KEY, hh_id)
logic_exp <- imap_dfr(EXPECTED_LOGIC, \(chk, sc) key_of(sc) |>
  mutate(check = paste0("5.hfc_logic: ", chk), description = paste("scenario", sc)))
dup_exp <- key_of("duplicate") |> mutate(check = "2.hfc_construct: duplicate", description = "scenario duplicate")

# Outliers: same rule as 4.hfc_outliers.R, computed independently
field_long <- function(field) {
  cols <- names(export)[col_base == field]
  export |> select(KEY, hh_id, all_of(cols)) |>
    pivot_longer(all_of(cols), names_to = "col", values_to = "value") |>
    mutate(instance = str_sub(col, nchar(field) + 1)) |>
    filter(!is.na(value)) |>
    select(-col)
}
num_long <- \(field) field_long(field) |> mutate(value = as.numeric(value)) |> filter(!is.na(value))
zflag <- \(d) d |> mutate(lv = log1p(value), z = (lv - mean(lv)) / sd(lv)) |> filter(abs(z) > OUTLIER_SD)
pooled <- c("A1_1", "cooking_events_7d", "cooking_hours_7d", "firewood_hours_7d")
out_exp <- bind_rows(
  map_dfr(pooled, \(v) zflag(num_long(v)) |> mutate(var = v)),
  {
    ft <- field_long("fuel_type") |> transmute(KEY, instance, fuel = value)
    fu <- field_long("fuel_unit") |> transmute(KEY, instance, unit = value)
    q <- num_long("fuel_qty_7d") |> left_join(ft, by = c("KEY", "instance")) |> left_join(fu, by = c("KEY", "instance")) |>
      filter(n() >= MIN_GROUP_N, .by = c(fuel, unit)) |> group_by(fuel, unit) |> group_modify(\(d, g) zflag(d)) |>
      ungroup() |> mutate(var = "fuel_qty_7d")
    sp <- num_long("fuel_spend_7d") |> left_join(ft, by = c("KEY", "instance")) |>
      filter(n() >= MIN_GROUP_N, .by = fuel) |> group_by(fuel) |> group_modify(\(d, g) zflag(d)) |>
      ungroup() |> mutate(var = "fuel_spend_7d")
    bind_rows(q, sp)
  }
) |>
  distinct(KEY, hh_id, var) |>
  transmute(KEY, hh_id, check = paste0("4.hfc_outliers: outlier_", var), description = "computed outlier")
planted_out <- c(outlier_members = "A1_1", outlier_events = "cooking_events_7d",
                 outlier_gas_qty = "fuel_qty_7d", outlier_gas_spend = "fuel_spend_7d")
for (sc in names(planted_out)) {
  if (!any(out_exp$hh_id %in% key_of(sc)$hh_id & str_ends(out_exp$check, planted_out[[sc]]))) {
    stop("Planted outlier '", sc, "' is not an outlier under the rule: increase it.")
  }
}

enum_exp <- tibble(KEY = NA_character_, hh_id = NA_character_,
                   check = c("6.hfc_enum: low_consent_rate", "6.hfc_enum: many_short_surveys"),
                   description = c(paste0("enumerator=", ENUM_LOW_CONSENT), paste0("enumerator=", ENUM_SHORT)))

expected <- bind_rows(dup_exp, out_exp, logic_exp, enum_exp) |> mutate(expected_flag = "1") |>
  select(KEY, hh_id, check, expected_flag, description)


# ---- 8. Write ----------------------------------------------------------------

dir.create(raw_preload_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(export, file.path(raw_data_dir, paste0(FORM_ID, "_WIDE.csv")), na = "")
iwalk(preload, \(x, f) write_csv(x, file.path(raw_preload_dir, f), na = ""))
write_csv(expected, file.path(raw_data_dir, "expected_flags.csv"), na = "")

message(sprintf("[0.generate_synthetic_data] %d submissions (%d households in preload), %d expected flags -> %s",
                nrow(export), nrow(hh), nrow(expected), raw_data_dir))


