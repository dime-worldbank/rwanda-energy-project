# ==============================================================================
# 0.generate_synthetic_data.R
# Synthetic midline SurveyCTO export, preload files and answer key for testing
# the midline HFC pipeline (RCT_data/midline/HFC/scripts/).
#
# SYNTHETIC DATA ONLY: every name, phone number, NID, ID and coordinate is
# invented here. No real household data is read; only the form definition
# (variable names, types, choices, relevance, constraints) is read.
#
# Run from the repo root:
#   Rscript RCT_data/midline/HFC/test/0.generate_synthetic_data.R
# Outputs and the test settings the answer key assumes: see README.md in this folder.
#
# Author: Yeji (using Claude) · Last modified: 2026-10-21
# ==============================================================================


# ---- 1. Setup ----------------------------------------------------------------

t_start <- Sys.time()
message("[0.generate_synthetic_data] start: ", format(t_start, "%Y-%m-%d %H:%M:%S"))

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
})
options(scipen = 999)

if (!file.exists("PATHS.R")) stop("Run this script from the repo root (PATHS.R not found).")
source("PATHS.R")

set.seed(20261021)

N_STEPS <- 9
step <- function(i, msg) {
  message(sprintf("[%d/%d] %s (%.1fs elapsed)", i, N_STEPS, msg,
                  as.numeric(difftime(Sys.time(), t_start, units = "secs"))))
}


# ---- 2. Config / constants ---------------------------------------------------

# Paths (only DROPBOX_EAQIP from PATHS.R)
form_path   <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/0_REP_SurveyCTO_Test_Form.xlsx")
data_dir    <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/synthetic_data")
preload_dir <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/synthetic_data/preload")

FORM_ID         <- "REP_readyboard_test"         # TBD: production form id
FIELDWORK_DAYS  <- as.Date("2026-10-12") + 0:4   # static on purpose: fixed test fieldwork week (Mon-Fri)
FIELDWORK_START <- min(FIELDWORK_DAYS)
OLD_FORMDEF     <- 2610150900                    # planted "old form version"
DURATION_MIN    <- 15                            # TBD: test value (minutes); use the same in 1.hfc_main.R
DURATION_MAX    <- 90                            # TBD: test value (minutes); use the same in 1.hfc_main.R
OUTLIER_SD      <- 3                             # +-3 SD on log(1 + x), as in script 4
CONSENT_TARGET  <- 0.95                          # enumerator consent rate target (among located)
SHORT_SURVEYS_ENUM_MIN <- 3                      # TBD: "many short surveys" = at least this many per enumerator

ENUM_LOW_CONSENT <- 7                            # enumerator with planted low consent rate
ENUM_SHORT       <- 4                            # enumerator with planted many short surveys
N_SHORT_ENUM     <- 6                            # number of short surveys by ENUM_SHORT (10-14 min)
DUR_SHORT_PLANT  <- 8                            # planted single too-short survey (minutes)
DUR_LONG_PLANT   <- 150                          # planted single too-long survey (minutes)

districts <- tribble(
  ~district,  ~districtid_key, ~code, ~provinceid_key, ~province_key, ~extra_arm, ~lat,  ~lon,
  "Karongi",  31,              "KAR", 3,               "Western",     "C",        -2.06, 29.35,
  "Rulindo",  41,              "RUL", 4,               "Northern",    "T1",       -1.73, 29.99,
  "Rusizi",   36,              "RUS", 3,               "Western",     "T2",       -2.48, 29.00,
  "Rutsiro",  32,              "RUT", 3,               "Western",     "T3",       -1.93, 29.33
)

# Households in preload per arm (attempted = preload - not attempted); shares ~ 530/537/483/423
arms <- tibble(
  treat       = c("C", "T1", "T2", "T3"),
  treat_label = c("Control", "Readyboard", "Solar off-grid (SHS)", "Readyboard + SHS"),
  n_preload   = c(56, 56, 52, 46)
)

# Scenario table: households per arm (see plan)
scenarios <- tribble(
  ~scenario,          ~C, ~T1, ~T2, ~T3,
  "not_attempted",     2,   2,   3,   3,
  "not_located",       2,   2,   2,   2,
  "refused",           3,   3,   2,   2,
  "partial",           1,   1,   1,   1,
  "none",             16,  10,   4,   3,
  "meter_only",       10,   6,   2,   0,
  "meter_use",        12,   3,   6,   0,
  "meter_rb",          1,  24,   0,   6,
  "shs",               9,   5,   4,   3,
  "elig_nudge_none",   0,   0,  13,  10,
  "elig_nudge_meter",  0,   0,   6,   5,
  "elig_meter_rb",     0,   0,   1,   6,
  "elig_electrified",  0,   0,   5,   3,
  "elig_shs",          0,   0,   3,   2
)
NON_COMPLETE <- c("not_attempted", "not_located", "refused", "partial")

# Preload-eligible households among T2/T3 households that never reach the nudge
elig_noncomplete <- tribble(
  ~scenario,       ~T2, ~T3,
  "not_attempted",   2,   2,
  "not_located",     1,   1,
  "refused",         1,   1
)

# Nudge outcomes among nudged households (yes / no / not sure; data consent = 0 among yes)
willing_counts <- tribble(
  ~treat, ~yes, ~no, ~notsure, ~data_consent_no,
  "T2",   11,   5,   3,        2,
  "T3",    9,   4,   2,        2
)

# Refusal quotas: ENUM_LOW_CONSENT gets most refusals; the rest go to distinct enumerators
refusal_low_enum <- c(C = 2, T1 = 2, T2 = 1, T3 = 1)

# Amounts in Rwf: lognormal around median (clamped at +-2 sdlog); planted outlier value
amounts <- tribble(
  ~var,    ~median, ~sdlog, ~outlier,
  "mt_1",    56000,  0.35,  1500000,
  "mt_6",     5000,  0.40,       NA,
  "rb_1",    18000,  0.35,   600000,
  "rb_6",     2000,  0.40,       NA,
  "iw_1",    40000,  0.45,  1800000,
  "iw_6",     5000,  0.40,       NA,
  "eu_1",     2000,  0.60,   250000,
  "eu_6",     2000,  0.60,   300000,
  "shs_1",  150000,  0.45,  6000000,
  "shs_6",    6000,  0.40,   250000,
  "F2_4",     1500,  0.50,    60000
)
COUNT_OUTLIERS <- c(hh_member_count = 19, F1_1 = 12)
OUTLIER_VARS   <- c("mt_1", "rb_1", "iw_1", "eu_1", "eu_6", "shs_1", "shs_6", "F2_4", "hh_member_count", "F1_1")

# Share paying partially (fullpartial = 2) by block
P_PARTIAL <- c(mt = 0.3, rb = 0.3, iw = 0.3, shs = 0.9)

# Invented name pools
FIRST_M <- c("Jean", "Eric", "Patrick", "Emmanuel", "Innocent", "Olivier", "Fabrice", "Claude",
             "Theogene", "Celestin", "Damascene", "Faustin")
FIRST_F <- c("Marie", "Alice", "Grace", "Diane", "Josiane", "Solange", "Aline", "Clarisse",
             "Esperance", "Jeanne", "Vestine", "Chantal")
SYL_1 <- c("Ka", "Mu", "Ni", "Ha", "Bi", "Nya", "Twa", "Ru", "Ga", "Se")
SYL_2 <- c("ze", "ri", "bo", "ta", "gi", "mo", "ne", "yi", "lu", "fa")
SYL_3 <- c("mana", "rimo", "yezu", "teka", "bizi", "gora", "nani", "huza")

# "Other (specify)" texts by choice list
SPECIFY_TEXT <- list(
  nolocate        = c("Household not known by village leader", "House abandoned"),
  meter_location  = c("On a pole near the road", "At the shop of a relative"),
  borrowed        = c("Church group", "Employer"),
  payment         = c("Paid in kind (beans)", "Paid by a family member abroad"),
  solartype       = c("Unbranded panel kit from market", "Second-hand system"),
  solarinstall    = c("Local electrician from church"),
  chargefrequency = c("Twice a month", "Only when travelling to town"),
  nofinish        = c("Respondent fell ill", "Heavy rain interrupted the interview")
)
COMMENTS <- c("Respondent was very cooperative", "Interview briefly interrupted by visitors",
              "Neighbour helped to locate the household")


# ---- 3. Helper functions -----------------------------------------------------

digit_sum <- function(x) map_int(as.character(x), \(s) sum(as.integer(strsplit(s, "")[[1]])))

# household_id = villageid_key * 10000 + hh number * 10 + check digit (digit sum mod 10)
make_hh_id <- function(villageid_key, hh_no) {
  base <- villageid_key * 1000 + hh_no
  sprintf("%.0f", base * 10 + digit_sum(sprintf("%.0f", base)) %% 10)
}

make_uuid <- function(n) {
  hex <- c(0:9, letters[1:6])
  map_chr(seq_len(n), \(i) {
    h <- sample(hex, 32, replace = TRUE)
    paste0("uuid:", paste(h[1:8], collapse = ""), "-", paste(h[9:12], collapse = ""), "-4",
           paste(h[13:15], collapse = ""), "-", sample(c("8", "9", "a", "b"), 1),
           paste(h[16:18], collapse = ""), "-", paste(h[19:30], collapse = ""))
  })
}

invent_last_name <- function(n) paste0(sample(SYL_1, n, TRUE), sample(SYL_2, n, TRUE), sample(SYL_3, n, TRUE))

# SurveyCTO export formats: "Oct 12, 2026 8:03:11 AM" and "Nov 1, 2024"
fmt_dt <- function(x) {
  h <- as.integer(format(x, "%H"))
  paste0(month.abb[as.integer(format(x, "%m"))], " ", as.integer(format(x, "%d")), ", ",
         format(x, "%Y"), " ", (h + 11) %% 12 + 1, format(x, ":%M:%S"), " ",
         if_else(h < 12, "AM", "PM"))
}
fmt_date <- function(x) paste0(month.abb[as.integer(format(x, "%m"))], " ", as.integer(format(x, "%d")), ", ", format(x, "%Y"))

chr <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(NA_character_)
  if (is.numeric(x)) return(format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE))
  as.character(x)
}

yn <- function(p) if (runif(1) < p) 1 else 0

new_phone <- function() sprintf("0700%06d", sample(0:999999, 1))

rand_amount <- function(var) {
  a <- amounts[amounts$var == var, ]
  z <- max(min(rnorm(1, 0, a$sdlog), 2 * a$sdlog), -2 * a$sdlog)
  max(100, round(a$median * exp(z), -2))
}
amount_outlier <- function(var) amounts$outlier[amounts$var == var]

# Choice sampling (codes come from the form; -77 / -88 only where the list allows them)
pick1 <- function(field, p_other = 0.03, p_dk = 0, allowed = NULL) {
  codes <- field_codes[[field]]
  reg <- setdiff(codes, c("-77", "-88"))
  if (!is.null(allowed)) reg <- intersect(reg, allowed)
  u <- runif(1)
  if ("-77" %in% codes && u < p_other) return("-77")
  if ("-88" %in% codes && u > 1 - p_dk) return("-88")
  sample(reg, 1)
}
pickm <- function(field, p_other = 0.03) {
  codes <- field_codes[[field]]
  reg <- setdiff(codes, c("-77", "-88"))
  x <- sample(reg, sample(1:2, 1, prob = c(0.8, 0.2)))
  if ("-77" %in% codes && runif(1) < p_other) x <- c(x, "-77")
  paste(codes[codes %in% x], collapse = " ")
}

has_code <- function(x, code) str_detect(coalesce(x, ""), paste0("(^| )", code, "( |$)"))

# Payment block shared by meter (mt), readyboard (rb), internal wiring (iw) and SHS (shs)
pay_block <- function(prefix, outlier_1 = FALSE, outlier_6 = FALSE) {
  f <- function(k) paste0(prefix, "_", k)
  v <- list()
  v[[f(1)]] <- if (outlier_1) amount_outlier(f(1)) else rand_amount(f(1))
  v[[f(2)]] <- yn(0.2)
  if (v[[f(2)]] == 1) v[[f(3)]] <- pickm(f(3))
  v[[f(4)]] <- pickm(f(4))
  v[[f(5)]] <- if (outlier_6 || runif(1) < P_PARTIAL[[prefix]]) 2 else 1
  if (v[[f(5)]] == 2) {
    v[[f(6)]] <- if (outlier_6) amount_outlier(f(6)) else rand_amount(f(6))
    v[[f(7)]] <- yn(0.5)
  }
  v
}

gps <- function(h, prefix) {
  setNames(list(round(h$lat + rnorm(1, 0, 3e-4), 6), round(h$lon + rnorm(1, 0, 3e-4), 6),
                round(h$alt + rnorm(1, 0, 5), 1), round(runif(1, 3, 15), 1)),
           paste0(prefix, c("-Latitude", "-Longitude", "-Altitude", "-Accuracy")))
}

image_name <- function(h, k) sprintf("%.0f.jpg", as.numeric(h$end) * 1000 + k)

# Module timestamps in seconds since form start (calculate_here once(duration()))
make_stamps <- function(h) {
  s <- list(all_1 = round(runif(1, 20, 60)))
  if (h$status == "not_located") return(s)
  s$consent <- s$all_1 + round(runif(1, 60, 150))
  if (h$status == "refused") return(s)
  R <- h$dur_sec - s$consent
  s$a_1 <- s$consent + round(runif(1, 5, 20)); s$a_2 <- s$a_1 + round(runif(1, 0.12, 0.18) * R)
  s$b_1 <- s$a_2 + round(runif(1, 2, 10));     s$b_2 <- s$b_1 + round(runif(1, 0.45, 0.55) * R)
  s$n_1 <- s$b_1 + round(0.6 * (s$b_2 - s$b_1))
  s$n_2 <- s$n_1 + round(min(runif(1, 120, 240), 0.3 * (s$b_2 - s$b_1)))
  s$c_1 <- s$b_2 + round(runif(1, 2, 10));     s$c_2 <- s$c_1 + round(runif(1, 0.12, 0.18) * R)
  if ("negative_duration" %in% h$plants) s$c_2 <- s$c_1 - round(runif(1, 60, 180))
  s
}

# One submission -> named list of field values, following the form order and relevance
fill_submission <- function(h) {
  P <- function(tag) tag %in% h$plants
  st <- make_stamps(h)
  v <- list(starttime = fmt_dt(h$start), endtime = fmt_dt(h$end), deviceid = h$deviceid,
            survey_duration = h$dur_sec, enumerator = h$enumerator,
            district = h$districtid_key, sector = h$sectorid_key, cell = h$cellid_key,
            village = h$villageid_key, hh_id = h$hh_id, stamp_all_1 = st$all_1)

  # Tracking & consent
  if (h$status == "not_located") {
    v$locate <- 0
    v$nolocate <- pick1("nolocate", p_other = 0.15)
    return(v)
  }
  v$locate <- 1
  v$audio <- paste0("AA_", str_remove(h$KEY, "uuid:"), ".m4a")
  v$consent <- if (h$status == "refused") 0 else 1
  v$stamp_consent <- st$consent
  v$text_audit <- paste0("TA_", str_remove(h$KEY, "uuid:"), ".csv")
  v$interview_duration <- round((h$dur_sec - st$consent) / 60, 3)
  if (v$consent == 0) {
    v$no_consent_reason <- pick1("no_consent_reason", p_dk = 0.1)
    return(c(v, gps(h, "location")))
  }

  meter <- h$meter; rb <- h$rb; wire <- h$wire; use <- h$use; shs <- h$shs
  v$observe_electricity_connection <- if (P("grid_observe_mismatch")) 1 - meter else meter

  # Module A: roster and contact
  v$stamp_a_1 <- st$a_1
  v$hh_member_count <- if (P("outlier_hh_member_count")) COUNT_OUTLIERS[["hh_member_count"]] else min(9, 2 + rpois(1, 3))
  v$first_name <- h$first_name
  v$last_name <- h$last_name
  v$hh_head_name <- paste(h$first_name, h$last_name)
  if (h$phone_clean != "") {
    v$pull_phonenumber <- h$phone_clean
    v$confirm_phonenumber <- if (P("phone_changed")) 0 else 1
    if (v$confirm_phonenumber == 0) v$phonenumber_revise <- new_phone()
  } else {
    v$phonenumber <- new_phone()
  }
  if (h$phone2_clean != "") {
    v$pull_second_phonenumber <- h$phone2_clean
    v$confirm_second_phonenumber <- 1
  } else {
    v$second_phonenumber <- new_phone()
  }
  v$stamp_a_2 <- st$a_2
  v$a_duration_min <- round((st$a_2 - st$a_1) / 60, 3)

  # Module B: grid connection, SHS, nudge, observation
  v$stamp_b_1 <- st$b_1
  v$meter_confirm <- meter
  if (meter == 1) {
    v[[FOLLOWUP[["meter"]]]] <- yn(0.95)
    meter_month <- sample(MONTHS, 1)
    v$meter_date <- fmt_date(meter_month)
    v$meter_location <- pick1("meter_location")
    v$meter_payment <- if (P("outlier_mt_1")) 1 else yn(0.85)
    if (v$meter_payment == 1) v <- c(v, pay_block("mt", outlier_1 = P("outlier_mt_1")))

    v$readyboard_confirm <- rb
    if (rb == 1) {
      v[[FOLLOWUP[["rb"]]]] <- yn(0.95)
      v$readyboard_date <- fmt_date(sample(MONTHS[MONTHS >= meter_month], 1))
      v$readyboard_payment <- if (P("outlier_rb_1")) 1 else yn(0.8)
      if (v$readyboard_payment == 1) v <- c(v, pay_block("rb", outlier_1 = P("outlier_rb_1")))
    }
    v$additional_wire_confirm <- wire
    if (wire == 1) {
      v[[FOLLOWUP[["iw"]]]] <- yn(0.95)
      v$additional_wire_date <- fmt_date(sample(MONTHS[MONTHS >= meter_month], 1))
      v$additional_wire_payment <- if (P("outlier_iw_1")) 1 else yn(0.85)
      if (v$additional_wire_payment == 1) v <- c(v, pay_block("iw", outlier_1 = P("outlier_iw_1")))
    }
    v$electricity_usage_confirm <- use
    if (use == 1) {
      v$electricity_usage_payment <- if (P("outlier_eu_1") || P("outlier_eu_6")) 1 else yn(0.9)
      if (v$electricity_usage_payment == 1) {
        v$eu_1 <- if (P("outlier_eu_1")) amount_outlier("eu_1") else rand_amount("eu_1")
        v$eu_2 <- yn(0.1)
        if (v$eu_2 == 1) v$eu_3 <- pickm("eu_3")
        v$eu_4 <- pickm("eu_4")
        v$eu_5 <- pick1("eu_5")
        v$eu_6 <- if (P("outlier_eu_6")) amount_outlier("eu_6") else rand_amount("eu_6")
      }
    }
  } else if (P("grid_usage_no_meter")) {
    # Planted: grid use reported without a meter (outside the form's relevance)
    v$electricity_usage_confirm <- 1
    v$electricity_usage_payment <- 0
  }

  v$shs_confirm <- shs
  if (shs == 1) {
    v[[FOLLOWUP[["shs"]]]] <- yn(0.95)
    v$shs_number <- if (P("shs_number_gt3")) 5 else sample(1:2, 1, prob = c(0.9, 0.1))
    v$shs_date <- fmt_date(sample(MONTHS, 1))
    v$shs_type <- pick1("shs_type", p_dk = 0.03)
    v$shs_install <- pick1("shs_install")
    v$shs_own <- pick1("shs_own")
    v$shs_payment <- if (P("outlier_shs_1") || P("outlier_shs_6")) 1 else yn(0.95)
    if (v$shs_payment == 1) v <- c(v, pay_block("shs", outlier_1 = P("outlier_shs_1"), outlier_6 = P("outlier_shs_6")))
  }

  v$pull_shs_eligible <- if (h$elig) 1 else NA
  electrified <- if (meter == 0 || (rb == 0 && wire == 0 && use == 0)) 0 else 1
  v$electrified <- electrified
  if (h$elig && shs == 0 && electrified == 0) {
    v$nosolar_confirm <- 1
    v$stamp_nudge_1 <- st$n_1
    v$shs_willing <- h$willing
    if (h$willing == 1) v$shs_data_consent <- h$data_consent
    v$stamp_nudge_2 <- st$n_2
    v$nudge_duration_min <- round((st$n_2 - st$n_1) / 60, 3)
  }

  v <- c(v, gps(h, "obs_gps"))
  if (shs == 1) v$obs_shs <- yn(0.85)
  if (P("shs_obs_mismatch")) v$obs_shs <- 1          # planted: SHS observed but none reported
  if (meter == 1 || shs == 1) {
    v$obs_consent <- yn(0.9)
    if (v$obs_consent == 1) {
      if (meter == 1) {
        v$meter_photo <- image_name(h, 1)
        v$meter_number <- paste0("9", paste(sample(0:9, 12, TRUE), collapse = ""))
      }
      if (rb %in% 1) v$readyboard_photo <- image_name(h, 2)
      if (shs == 1 && v$obs_shs == 1) v$shs_photo <- image_name(h, 3)
    }
  }
  v$stamp_b_2 <- st$b_2
  v$b_duration_min <- round((st$b_2 - st$b_1) / 60, 3)

  # Module C: mobile phone charging (repeat)
  v$stamp_c_1 <- st$c_1
  v$F1_0 <- if (P("outlier_F1_1") || P("outlier_F2_4")) 1 else yn(0.92)
  if (v$F1_0 == 1) {
    n <- if (P("outlier_F1_1")) COUNT_OUTLIERS[["F1_1"]] else 1 + rbinom(1, 3, 0.35)
    v$F1_1 <- n
    v$mobile_repeat_count <- n
    for (i in seq_len(n)) {
      loc <- pick1("F2_1")
      v[[paste0("F2_1_", i)]] <- loc
      v[[paste0("F2_2_", i)]] <- pick1("F2_2")
      pays <- if (P("outlier_F2_4") && i == 1) 1 else if (loc == "1") yn(0.05) else yn(0.8)
      v[[paste0("F2_3_", i)]] <- pays
      if (pays == 1) v[[paste0("F2_4_", i)]] <- if (P("outlier_F2_4") && i == 1) amount_outlier("F2_4") else rand_amount("F2_4")
    }
  }
  v$stamp_c_2 <- st$c_2
  v$c_duration_min <- round((st$c_2 - st$c_1) / 60, 3)

  # Conclusion
  v$finish <- if (h$status == "partial") 0 else 1
  if (v$finish == 0) v$conclusion_nofinish <- pick1("conclusion_nofinish", allowed = c("1", "2", "3"), p_other = 0.3)
  if (runif(1) < 0.1) v$comment <- sample(COMMENTS, 1)
  v
}

# Relevance / constraint translation (XLSForm -> R)
rel_to_r <- function(x) {
  x <- str_squish(coalesce(x, ""))
  if (x == "") return("TRUE")
  x |>
    str_replace_all("string-length\\(", "string_length(") |>
    str_replace_all("selected\\(\\s*\\$\\{(\\w+)\\}\\s*,\\s*'([^']*)'\\s*\\)",
                    "str_detect(coalesce(as.character(\\1), ''), '(^| )\\2( |$)')") |>
    str_replace_all("\\$\\{(\\w+)\\}", "\\1") |>
    str_replace_all("(?<![<>!=])=(?!=)", "==") |>
    str_replace_all("\\band\\b", "&") |>
    str_replace_all("\\bor\\b", "|")
}
con_to_r <- function(x) {
  x |>
    str_squish() |>
    str_replace_all("format-date-time\\(", "format_date_time(") |>
    str_replace_all("today\\(\\)", ".today") |>
    str_replace_all("(?<![\\w])\\.(?![\\w])", ".v") |>
    str_replace_all("\\bregex\\(", "str_detect(") |>
    str_replace_all("(?<![<>!=])=(?!=)", "==") |>
    str_replace_all("\\band\\b", "&") |>
    str_replace_all("\\bor\\b", "|")
}
eval_env <- rlang::new_environment(
  list(string_length = \(x) nchar(ifelse(is.na(x), "", as.character(x))),
       # Dates in the export look like "Nov 1, 2024"; .today is a Date
       format_date_time = \(x, fmt) format(if (inherits(x, "Date")) x else as.Date(mdy(x, quiet = TRUE)), fmt),
       str_detect = str_detect, coalesce = coalesce),
  parent = baseenv()
)
eval_lgl <- function(expr, data) {
  out <- rlang::eval_tidy(rlang::parse_expr(expr), data = data, env = eval_env)
  rep_len(out %in% TRUE, nrow(data))
}

num <- function(x) suppressWarnings(as.numeric(x))


# ---- 4. Data loading (form definition) ---------------------------------------

step(1, "Loading form definition")

survey <- read_excel(form_path, "survey", col_types = "text", .name_repair = "unique_quiet") |>
  filter(!is.na(type)) |>
  mutate(type = str_squish(type), name = str_squish(name),
         across(c(relevance, constraint, required, repeat_count), \(x) str_squish(replace_na(x, ""))))
choices  <- read_excel(form_path, "choices", col_types = "text", .name_repair = "unique_quiet") |>
  filter(!is.na(list_name), !is.na(value)) |>
  mutate(list_name = str_squish(list_name), value = str_squish(value))
settings <- read_excel(form_path, "settings", col_types = "text")

CURRENT_FORMDEF <- num(settings$version[1])
stopifnot(settings$form_id[1] == FORM_ID, !is.na(CURRENT_FORMDEF))
if (OLD_FORMDEF >= CURRENT_FORMDEF) {
  stop(sprintf("Form version %.0f is not higher than OLD_FORMDEF %.0f: check settings$version (yymmddHHMM).",
               CURRENT_FORMDEF, OLD_FORMDEF))
}

META_TYPES  <- c("start", "end", "deviceid", "subscriberid", "simserial", "phonenumber")
GEO_FIELDS  <- c("district", "sector", "cell", "village")

# Field table with full relevance (enclosing groups AND own), in form order
fields <- local({
  stack <- character(); in_rep <- FALSE; out <- list()
  for (i in seq_len(nrow(survey))) {
    r <- survey[i, ]
    if (r$type %in% c("begin group", "begin repeat")) {
      stack <- c(stack, rel_to_r(r$relevance))
      if (r$type == "begin repeat") {
        in_rep <- TRUE; REP_NAME <<- r$name; REP_COUNT <<- rel_to_r(r$repeat_count)
      }
      next
    }
    if (r$type %in% c("end group", "end repeat")) {
      stack <- head(stack, -1)
      if (r$type == "end repeat") in_rep <- FALSE
      next
    }
    if (r$type == "note") next
    out[[length(out) + 1]] <- tibble(
      name = r$name, type = r$type, base = word(r$type, 1), list = word(r$type, 2),
      relevance_raw = r$relevance,
      rel_full = paste0("(", c(stack, rel_to_r(r$relevance)), ")", collapse = " & "),
      required = tolower(r$required) == "yes", constraint = r$constraint, in_repeat = in_rep
    )
  }
  bind_rows(out)
})
rep_fields <- fields$name[fields$in_repeat]

# Choice codes for static lists (dynamic lists such as hh_id come from the preload)
static_lists <- choices |>
  group_by(list_name) |>
  filter(!any(str_detect(value, "_key$"))) |>
  summarise(codes = list(value), .groups = "drop")
field_codes <- fields |>
  filter(base %in% c("select_one", "select_multiple")) |>
  inner_join(static_lists, by = c("list" = "list_name")) |>
  select(name, codes) |>
  deframe()

# Specify fields: text fields whose relevance needs parent = -77 (names read from the form)
spec_map <- fields |>
  filter(base == "text") |>
  mutate(parent = coalesce(str_match(relevance_raw, "selected\\(\\$\\{(\\w+)\\}\\s*,\\s*'-77'\\)")[, 2],
                           str_match(relevance_raw, "\\$\\{(\\w+)\\}\\s*=\\s*-77")[, 2])) |>
  filter(!is.na(parent), !parent %in% GEO_FIELDS) |>
  left_join(fields |> select(parent = name, parent_list = list), by = "parent") |>
  select(spec = name, parent, parent_list, in_repeat)

# Yes/no follow-up asked right after each device question (e.g. meter_confirm_working); names read from the form
followup_of <- function(parent) {
  x <- fields$name[fields$list %in% "yesno" &
                     str_detect(fields$relevance_raw, sprintf("^\\$\\{%s\\}\\s*=\\s*1$", parent))]
  if (length(x) == 0) stop("No yes/no follow-up found for ", parent)
  x[1]
}
FOLLOWUP <- map_chr(c(meter = "meter_confirm", rb = "readyboard_confirm",
                      iw = "additional_wire_confirm", shs = "shs_confirm"), followup_of)

# Months available for meter/readyboard/wiring/SHS dates (last 2 years, 1st of month)
MONTHS <- seq(floor_date(FIELDWORK_START %m-% years(2), "month") %m+% months(1),
              floor_date(FIELDWORK_START, "month") %m-% months(1), by = "month")

message(sprintf("      form %s v%.0f: %d fields, %d static choice lists, %d specify fields",
                FORM_ID, CURRENT_FORMDEF, nrow(fields), nrow(static_lists), nrow(spec_map)))


# ---- 5. Build synthetic data -------------------------------------------------

## 5.1 Villages, households, enumerators ---------------------------------------
step(2, "Building villages, households and enumerators")

villages <- districts |>
  mutate(arm = map(extra_arm, \(x) c("C", "T1", "T2", "T3", x))) |>
  unnest(arm) |>
  group_by(district) |>
  mutate(v_no = row_number()) |>
  ungroup() |>
  mutate(
    sector_no   = if_else(v_no <= 3, 1L, 2L),
    sectorid_key  = districtid_key * 100 + sector_no,
    cellid_key    = sectorid_key * 100 + v_no,
    villageid_key = cellid_key * 100 + 1,
    sector_key = sprintf("%s-S%d", code, sector_no),
    cell_key   = sprintf("%s-C%d", code, v_no),
    village_key = sprintf("%s-V%d", code, v_no),
    v_lat = lat + runif(n(), -0.08, 0.08), v_lon = lon + runif(n(), -0.08, 0.08),
    v_alt = round(runif(n(), 1500, 2200))
  )

# Village sizes per arm sum to the preload targets
villages <- villages |>
  group_by(arm) |>
  mutate(n_hh = {
    tot <- arms$n_preload[arms$treat == arm[1]]
    k <- n(); s <- rep(tot %/% k, k)
    r <- tot %% k
    if (r > 0) { i <- sample(k, r); s[i] <- s[i] + 1 }
    s + sample(c(-2, 2, -1, 1, 0))            # zero-sum jitter (5 villages per arm)
  }) |>
  ungroup()
stopifnot(all(count(villages, arm, wt = n_hh)$n == arms$n_preload))

enumerators <- districts |>
  mutate(e = map(seq_len(n()), \(d) c(2 * d - 1, 2 * d))) |>
  unnest(e) |>
  transmute(enumeratorid_key = e, enumerator_key = paste(sample(c(FIRST_M, FIRST_F), n()), invent_last_name(n())),
            district_key = district, districtid_key,
            deviceid = map_chr(e, \(i) paste0("35", paste(sample(0:9, 13, TRUE), collapse = ""))))

hh <- villages |>
  mutate(hh_no = map(n_hh, seq_len)) |>
  unnest(hh_no) |>
  mutate(
    hh_id = make_hh_id(villageid_key, hh_no),
    female = runif(n()) < 0.3,
    first_name = if_else(female, sample(FIRST_F, n(), TRUE), sample(FIRST_M, n(), TRUE)),
    last_name = invent_last_name(n()),
    gender = if_else(female, "Female", "Male"),
    nid = map_chr(seq_len(n()), \(i) paste0("9", paste(sample(0:9, 15, TRUE), collapse = ""))),
    phone_clean  = sprintf("0700%06d", sample(0:999999, n())),
    phone_clean  = if_else(runif(n()) < 0.05, "", phone_clean),
    phone2_clean = if_else(runif(n()) < 0.35, sprintf("0700%06d", sample(0:999999, n())), ""),
    lat = round(v_lat + rnorm(n(), 0, 0.003), 6), lon = round(v_lon + rnorm(n(), 0, 0.003), 6),
    alt = v_alt + round(rnorm(n(), 0, 20))
  ) |>
  rename(treat = arm) |>
  left_join(arms |> select(treat, treat_label), by = "treat") |>
  group_by(districtid_key) |>
  mutate(enumerator = 2 * match(districtid_key, districts$districtid_key) - 1 + (row_number() %% 2 == 0)) |>
  ungroup() |>
  mutate(scenario = NA_character_, elig = FALSE)

stopifnot(!anyDuplicated(hh$hh_id), all(count(hh, treat)$n == arms$n_preload))

## 5.2 Scenarios and planted problems ------------------------------------------
step(3, "Assigning scenarios and planted problems")

assign_scen <- function(hh, cond, scen, n = 1) {
  cand <- which(is.na(hh$scenario) & cond)
  if (length(cand) < n) stop("Not enough households for scenario ", scen)
  hh$scenario[cand[sample.int(length(cand), n)]] <- scen
  hh
}

# Not attempted (preload only)
for (a in arms$treat) hh <- assign_scen(hh, hh$treat == a, "not_attempted", scenarios[[a]][scenarios$scenario == "not_attempted"])

# Not located: one per enumerator, two per arm
nl_arms <- sample(rep(arms$treat, 2))
for (e in 1:8) hh <- assign_scen(hh, hh$enumerator == e & hh$treat == nl_arms[e], "not_located")

# Refusals: low-consent enumerator first, then one each to four other distinct enumerators
for (a in names(refusal_low_enum)) hh <- assign_scen(hh, hh$enumerator == ENUM_LOW_CONSENT & hh$treat == a, "refused", refusal_low_enum[[a]])
other_enums <- sample(setdiff(1:8, ENUM_LOW_CONSENT), 5)
ref_rest <- map_int(arms$treat, \(a) scenarios[[a]][scenarios$scenario == "refused"] - refusal_low_enum[[a]])
for (k in seq_along(arms$treat)) hh <- assign_scen(hh, hh$enumerator == other_enums[k] & hh$treat == arms$treat[k], "refused", ref_rest[k])
ENUM_REVISIT <- other_enums[5]   # refuses first, completes on revisit; no other refusal

# Partial: one per arm, distinct enumerators
part_enums <- sample(1:8, 4)
for (k in seq_along(arms$treat)) hh <- assign_scen(hh, hh$enumerator == part_enums[k] & hh$treat == arms$treat[k], "partial")

# Complete scenarios: shuffle within arm
for (a in arms$treat) {
  todo <- scenarios |> filter(!scenario %in% NON_COMPLETE)
  vec <- rep(todo$scenario, todo[[a]])
  idx <- which(is.na(hh$scenario) & hh$treat == a)
  stopifnot(length(idx) == length(vec))
  hh$scenario[idx] <- sample(vec)
}

# Preload eligibility
hh <- hh |> mutate(elig = str_starts(scenario, "elig_"))
for (a in c("T2", "T3")) for (s in elig_noncomplete$scenario) {
  idx <- which(hh$treat == a & hh$scenario == s)
  hh$elig[idx[sample.int(length(idx), elig_noncomplete[[a]][elig_noncomplete$scenario == s])]] <- TRUE
}

# Household situation flags
hh <- hh |>
  mutate(
    status = case_when(scenario %in% NON_COMPLETE ~ scenario, TRUE ~ "complete"),
    base = case_when(
      scenario == "partial" ~ sample(c("none", "meter_only"), n(), TRUE),
      scenario %in% c("elig_nudge_none") ~ "none",
      scenario %in% c("elig_nudge_meter") ~ "meter_only",
      scenario %in% c("elig_meter_rb") ~ "meter_rb",
      scenario %in% c("elig_electrified") ~ "meter_use",
      scenario %in% c("elig_shs") ~ "shs_nometer",
      scenario == "shs" ~ if_else(runif(n()) < 0.3, "shs_meter", "shs_nometer"),
      TRUE ~ scenario),
    meter = as.integer(base %in% c("meter_only", "meter_use", "meter_rb", "shs_meter")),
    rb    = as.integer(base == "meter_rb"),
    wire  = as.integer(base == "meter_use" | (base == "meter_rb" & runif(n()) < 0.6)),
    use   = as.integer(base == "meter_use" | (base == "meter_rb" & runif(n()) < 0.6)),
    shs   = as.integer(base %in% c("shs_meter", "shs_nometer"))
  )

# Planted problems on distinct complete households
used <- character()
take <- function(cond, n = 1) {
  cand <- setdiff(hh$hh_id[hh$status == "complete" & cond], used)
  if (length(cand) < n) stop("Not enough candidate households for a planted problem")
  x <- cand[sample.int(length(cand), n)]
  used <<- c(used, x)
  x
}
CT1 <- hh$treat %in% c("C", "T1")

plants <- list()
plants$rb_compliance <- hh$hh_id[hh$status == "complete" & hh$rb == 1 & hh$treat %in% c("C", "T2")]
used <- c(used, plants$rb_compliance)
plants$e04_short <- take(hh$enumerator == ENUM_SHORT, N_SHORT_ENUM)
plants$revisit <- take(CT1 & hh$enumerator == ENUM_REVISIT)
plants$dup_same_enum <- take(CT1)
plants$dup_diff_enum <- take(CT1)
plants$outlier_mt_1 <- take(hh$meter == 1)
plants$outlier_rb_1 <- take(hh$rb == 1 & hh$treat %in% c("T1", "T3"))
plants$outlier_iw_1 <- take(hh$wire == 1)
plants$outlier_eu_1 <- take(hh$use == 1)
plants$outlier_eu_6 <- take(hh$use == 1)
plants$outlier_shs_1 <- take(hh$shs == 1)
plants$outlier_shs_6 <- take(hh$shs == 1)
plants$outlier_F2_4 <- take(TRUE)
plants$outlier_hh_member_count <- take(TRUE)
plants$outlier_F1_1 <- take(TRUE)
plants$old_formdef <- take(TRUE)
plants$start_end_date <- take(TRUE)
plants$end_submission_date <- take(TRUE)
plants$negative_duration <- take(TRUE)
plants$duration_short <- take(hh$enumerator != ENUM_SHORT)
plants$duration_long <- take(hh$enumerator != ENUM_SHORT)
plants$nudge_outside <- take(hh$treat == "C" & hh$scenario == "none")
plants$grid_usage_no_meter <- take(CT1 & hh$scenario == "none")
plants$grid_observe_mismatch <- take(CT1 & hh$scenario == "none")
plants$shs_obs_mismatch <- take(CT1 & hh$scenario == "meter_only")
plants$shs_number_gt3 <- take(hh$shs == 1)
plants$phone_changed <- take(hh$phone_clean != "", 2)

plant_long <- enframe(plants, name = "plant", value = "hh_id") |> unnest(hh_id)

# C household planted into shs_eligible.csv -> nudge asked outside T2/T3
hh$elig[hh$hh_id %in% plants$nudge_outside] <- TRUE

# Nudge outcomes
hh <- hh |>
  mutate(nudged = status %in% c("complete", "partial") & elig & shs == 0 &
           (meter == 0 | (rb == 0 & wire == 0 & use == 0)),
         willing = NA_real_, data_consent = NA_real_)
for (a in willing_counts$treat) {
  w <- willing_counts[willing_counts$treat == a, ]
  idx <- which(hh$nudged & hh$treat == a)
  stopifnot(length(idx) == w$yes + w$no + w$notsure)
  hh$willing[idx] <- sample(rep(c(1, 0, -88), c(w$yes, w$no, w$notsure)))
  yes <- idx[hh$willing[idx] == 1]
  hh$data_consent[yes] <- sample(rep(c(0, 1), c(w$data_consent_no, w$yes - w$data_consent_no)))
}
idx <- which(hh$nudged & !hh$treat %in% willing_counts$treat)
hh$willing[idx] <- 1
hh$data_consent[idx] <- 1

## 5.3 Submissions and schedule ------------------------------------------------
step(4, "Scheduling submissions")

subs <- bind_rows(
  hh |> filter(status != "not_attempted") |>
    transmute(hh_id, role = "main", enumerator, status,
              ord = if_else(hh_id %in% plants$revisit, Inf, row_number())),
  hh |> filter(hh_id %in% plants$revisit) |>
    transmute(hh_id, role = "revisit_refused", enumerator, status = "refused", ord = -Inf),
  hh |> filter(hh_id %in% plants$dup_same_enum) |>
    transmute(hh_id, role = "duplicate", enumerator, status = "complete", ord = Inf),
  hh |> filter(hh_id %in% plants$dup_diff_enum) |>
    transmute(hh_id, role = "duplicate", enumerator = if_else(enumerator %% 2 == 1, enumerator + 1, enumerator - 1),
              status = "complete", ord = Inf)
)

# Planted problems apply to the household's main submission only
subs <- subs |>
  left_join(plant_long |> summarise(plants = list(plant), .by = hh_id), by = "hh_id") |>
  mutate(plants = map2(plants, role, \(p, r) if (r == "main" && !is.null(p)) p else character()))

subs <- subs |>
  mutate(
    dur_min = case_when(
      status == "not_located" ~ runif(n(), 2, 5),
      status == "refused" ~ runif(n(), 4, 9),
      map_lgl(plants, \(p) "duration_short" %in% p) ~ DUR_SHORT_PLANT,
      map_lgl(plants, \(p) "duration_long" %in% p) ~ DUR_LONG_PLANT,
      map_lgl(plants, \(p) "e04_short" %in% p) ~ runif(n(), 10, 14),
      map_lgl(plants, \(p) "start_end_date" %in% p) ~ 45,
      TRUE ~ runif(n(), 20, 60)),
    dur_sec = round(dur_min * 60)
  ) |>
  arrange(enumerator, ord, hh_id) |>
  group_by(enumerator) |>
  mutate(day = FIELDWORK_DAYS[ceiling(row_number() * length(FIELDWORK_DAYS) / n())]) |>
  group_by(enumerator, day) |>
  mutate(gap = c(runif(1, 7.75 * 3600, 8.5 * 3600), runif(n() - 1, 600, 2400)),
         start = as.POSIXct(day, tz = "UTC") + cumsum(gap + lag(dur_sec, default = 0))) |>
  ungroup() |>
  mutate(
    start = if_else(map_lgl(plants, \(p) "start_end_date" %in% p),
                    as.POSIXct(day, tz = "UTC") + 23 * 3600 + 35 * 60, start),
    end = start + dur_sec,
    submitted = end + runif(n(), 300, 5400),
    submitted = if_else(as.Date(submitted) != as.Date(end), end + 60, submitted),
    submitted = if_else(map_lgl(plants, \(p) "end_submission_date" %in% p),
                        as.POSIXct(as.Date(end) + 1, tz = "UTC") + 7.5 * 3600 + runif(n(), 0, 1800), submitted),
    formdef_version = if_else(map_lgl(plants, \(p) "old_formdef" %in% p), OLD_FORMDEF, CURRENT_FORMDEF),
    KEY = make_uuid(n())
  ) |>
  arrange(submitted) |>
  left_join(enumerators |> select(enumerator = enumeratorid_key, deviceid), by = "enumerator") |>
  left_join(hh |> select(hh_id, districtid_key, sectorid_key, cellid_key, villageid_key, treat,
                         first_name, last_name, phone_clean, phone2_clean, lat, lon, alt,
                         elig, meter, rb, wire, use, shs, willing, data_consent),
            by = "hh_id")

message(sprintf("      %d submissions for %d attempted households", nrow(subs), n_distinct(subs$hh_id)))

## 5.4 Fill fields ---------------------------------------------------------------
step(5, sprintf("Filling %d submissions", nrow(subs)))

rows <- map(seq_len(nrow(subs)), \(i) {
  h <- as.list(subs[i, ])
  h$plants <- h$plants[[1]]
  v <- fill_submission(h)
  tibble::as_tibble_row(map(v, chr), .name_repair = "minimal")
})
wide <- bind_rows(rows) |>
  mutate(SubmissionDate = fmt_dt(subs$submitted), instanceID = subs$KEY,
         formdef_version = sprintf("%.0f", subs$formdef_version), KEY = subs$KEY)

# Column order: SurveyCTO wide (metadata, form order, repeat fields by instance, KEY last)
N_REP <- max(num(wide$F1_1), na.rm = TRUE)
wide_cols <- local({
  out <- "SubmissionDate"; rep_done <- FALSE
  for (i in seq_len(nrow(fields))) {
    f <- fields[i, ]
    if (f$in_repeat) {
      if (!rep_done) {
        out <- c(out, paste0(REP_NAME, "_count"),
                 as.vector(t(outer(seq_len(N_REP), rep_fields, \(k, nm) paste0(nm, "_", k)))))
        rep_done <- TRUE
      }
      next
    }
    out <- c(out, if (f$base == "geopoint") paste0(f$name, c("-Latitude", "-Longitude", "-Altitude", "-Accuracy")) else f$name)
  }
  c(out, "instanceID", "formdef_version", "KEY")
})
extra <- setdiff(names(wide), wide_cols)
if (length(extra) > 0) stop("Generated columns not in the form: ", paste(extra, collapse = ", "))
for (cl in setdiff(wide_cols, names(wide))) wide[[cl]] <- NA_character_
wide <- wide |> select(all_of(wide_cols))

## 5.5 Special codes and specify texts -------------------------------------------
step(6, "Ensuring -77 / -88 coverage and specify texts")

field_cols <- function(f) if (f %in% rep_fields) paste0(f, "_", seq_len(N_REP)) else f
planted_keys <- subs$KEY[lengths(subs$plants) > 0]
special <- fields |>
  filter(name %in% names(field_codes), !name %in% GEO_FIELDS) |>
  mutate(codes = field_codes[name]) |>
  mutate(code = map(codes, \(x) intersect(c("-77", "-88"), x))) |>
  unnest(code) |>
  select(name, base, code)

forced <- list()
for (j in seq_len(nrow(special))) {
  f <- special$name[j]; code <- special$code[j]
  cols <- field_cols(f)
  if (any(map_lgl(cols, \(cl) any(has_code(wide[[cl]], code))))) next
  if (f == "shs_willing") stop("shs_willing must contain ", code, " from the scenario assignment")
  cand <- map_dfr(cols, \(cl) tibble(row = which(!is.na(wide[[cl]]) & !has_code(wide[[cl]], "-77") &
                                                   !has_code(wide[[cl]], "-88") & !wide$KEY %in% planted_keys),
                                     col = cl))
  if (nrow(cand) == 0) stop("No candidate submission to plant ", code, " in ", f)
  pick <- cand[sample.int(nrow(cand), 1), ]
  wide[[pick$col]][pick$row] <- if (special$base[j] == "select_multiple") paste(wide[[pick$col]][pick$row], code) else code
  forced[[length(forced) + 1]] <- tibble(field = f, code = code, KEY = wide$KEY[pick$row])
}
forced <- bind_rows(forced)

for (j in seq_len(nrow(spec_map))) {
  s <- spec_map[j, ]
  pairs <- if (s$in_repeat) tibble(p = paste0(s$parent, "_", seq_len(N_REP)), sp = paste0(s$spec, "_", seq_len(N_REP))) else tibble(p = s$parent, sp = s$spec)
  pool <- SPECIFY_TEXT[[s$parent_list]] %||% "Other (synthetic)"
  for (k in seq_len(nrow(pairs))) {
    hit <- has_code(wide[[pairs$p[k]]], "-77")
    wide[[pairs$sp[k]]] <- if_else(hit, sample(pool, nrow(wide), TRUE), NA_character_)
  }
}
message(sprintf("      %d special codes forced; %d specify fields filled", nrow(forced), nrow(spec_map)))


# ---- 6. Answer key ---------------------------------------------------------------
step(7, "Building answer key")

plant_checks <- bind_rows(
  tribble(
    ~plant,                  ~check,                                      ~description,
    "dup_same_enum",         "2.hfc_construct: duplicate",                "Same hh_id, two consented submissions by the same enumerator",
    "dup_diff_enum",         "2.hfc_construct: duplicate",                "Same hh_id, two consented submissions by different enumerators",
    "start_end_date",        "5.hfc_logic: start_end_date",               "starttime 23:35, endtime next day",
    "end_submission_date",   "5.hfc_logic: end_submission_date",          "SubmissionDate the day after endtime",
    "negative_duration",     "5.hfc_logic: negative_module_duration",     "stamp_c_2 < stamp_c_1 (c_duration_min < 0)",
    "duration_short",        "5.hfc_logic: duration_short",               sprintf("Consented survey of %d min (< DURATION_MIN = %d)", DUR_SHORT_PLANT, DURATION_MIN),
    "e04_short",             "5.hfc_logic: duration_short",               sprintf("Short survey (10-14 min) by enumerator %d", ENUM_SHORT),
    "duration_long",         "5.hfc_logic: duration_long",                sprintf("Consented survey of %d min (> DURATION_MAX = %d)", DUR_LONG_PLANT, DURATION_MAX),
    "rb_compliance",         "5.hfc_logic: readyboard_in_C_T2",           "readyboard_confirm = 1 in a C or T2 village",
    "nudge_outside",         "5.hfc_logic: nudge_outside_T2T3",           "SHS nudge answered in a C household",
    "grid_usage_no_meter",   "5.hfc_logic: usage_without_meter",          "electricity_usage_confirm = 1 with meter_confirm = 0",
    "grid_observe_mismatch", "5.hfc_logic: observe_vs_meter_confirm",     "observe_electricity_connection != meter_confirm",
    "shs_obs_mismatch",      "5.hfc_logic: obs_shs_without_shs_confirm",  "obs_shs = 1 with shs_confirm = 0"
  ),
  # Planted in the data but not HFC logic checks (removed 2026-10-22): old_formdef, shs_number_gt3,
  # phone_changed, and the C household in shs_eligible.csv (a one-time preload warning in script 2).
  tibble(plant = paste0("outlier_", OUTLIER_VARS), check = paste0("4.hfc_outliers: outlier_", OUTLIER_VARS),
         description = map_chr(OUTLIER_VARS, \(x) sprintf("High %s = %s", x,
                                                          chr(coalesce(amounts$outlier[match(x, amounts$var)], COUNT_OUTLIERS[x])))))
)

key_main <- subs |> select(KEY, hh_id, role, submitted)
exp_records <- plant_long |>
  inner_join(plant_checks, by = "plant", relationship = "many-to-many") |>
  inner_join(key_main, by = "hh_id", relationship = "many-to-many") |>
  filter(role == "main" | str_starts(plant, "dup_")) |>
  arrange(check, submitted) |>
  transmute(KEY, hh_id, check, expected_flag = 1L, description)

# Enumerator-level
enum_stats <- wide |>
  mutate(enumerator = num(enumerator), dur = num(survey_duration) / 60) |>
  summarise(located = sum(locate == "1"), consented = sum(consent == "1", na.rm = TRUE),
            n_short = sum(consent == "1" & dur < DURATION_MIN, na.rm = TRUE), .by = enumerator) |>
  mutate(consent_rate = consented / located)
exp_enum <- bind_rows(
  enum_stats |> filter(enumerator == ENUM_LOW_CONSENT) |>
    transmute(check = "6.hfc_enum: low_consent_rate",
              description = sprintf("enumerator=%d: consent rate %.0f%% among located (target %.0f%%)",
                                    enumerator, 100 * consent_rate, 100 * CONSENT_TARGET)),
  enum_stats |> filter(enumerator == ENUM_SHORT) |>
    transmute(check = "6.hfc_enum: many_short_surveys",
              description = sprintf("enumerator=%d: %d consented surveys < %d min", enumerator, n_short, DURATION_MIN))
) |> mutate(KEY = NA_character_, hh_id = NA_character_, expected_flag = 1L)

# SHS funnel (T2/T3, households)
by_hh <- wide |>
  summarise(located = any(locate == "1"), consented = any(consent == "1", na.rm = TRUE),
            door = any(pull_shs_eligible == "1" & shs_confirm == "0" & electrified == "0", na.rm = TRUE),
            yes = any(shs_willing == "1", na.rm = TRUE), no = any(shs_willing == "0", na.rm = TRUE),
            notsure = any(shs_willing == "-88", na.rm = TRUE),
            dconsent = any(shs_data_consent == "1", na.rm = TRUE), .by = hh_id)
shs_funnel <- hh |>
  filter(treat %in% c("T2", "T3")) |>
  left_join(by_hh, by = "hh_id") |>
  mutate(across(c(located, consented, door, yes, no, notsure, dconsent), \(x) coalesce(x, FALSE)),
         attempted = hh_id %in% wide$hh_id, consented_elig = consented & elig) |>
  summarise(in_arm = n(), attempted = sum(attempted), located = sum(located), consented = sum(consented),
            preload_eligible = sum(consented_elig), door_confirmed = sum(door),
            willing_yes = sum(yes), willing_no = sum(no), willing_notsure = sum(notsure),
            data_consent = sum(dconsent), .by = treat) |>
  arrange(treat)
exp_funnel <- shs_funnel |>
  pivot_longer(-treat, names_to = "step", values_to = "n") |>
  mutate(prev = case_when(str_starts(step, "willing_") ~ "door_confirmed",
                          step == "data_consent" ~ "willing_yes",
                          TRUE ~ lag(step)), .by = treat) |>
  mutate(prev_n = n[match(prev, step)], .by = treat) |>
  transmute(KEY = NA_character_, hh_id = NA_character_, check = paste0("9.hfc_shs: funnel_", step),
            expected_flag = as.integer(n),
            description = if_else(is.na(prev), paste0("arm=", treat),
                                  sprintf("arm=%s; %.1f%% of %s", treat, 100 * n / prev_n, prev)))

# Special-code coverage (not errors: categories that must appear in script 7 descriptives)
exp_codes <- special |>
  mutate(KEY = map2_chr(name, code, \(f, cd) {
    hit <- map(field_cols(f), \(cl) wide$KEY[has_code(wide[[cl]], cd)]) |> unlist()
    hit[1]
  })) |>
  left_join(wide |> select(KEY, hh_id), by = "KEY") |>
  left_join(spec_map |> select(name = parent, spec), by = "name") |>
  transmute(KEY, hh_id, check = paste0("7.hfc_stats: code_", code), expected_flag = 1L,
            description = if_else(code == "-77" & !is.na(spec), sprintf("%s = -77 with %s filled", name, spec),
                                  sprintf("%s = %s", name, code)))

expected_flags <- bind_rows(exp_records, exp_enum, exp_funnel, exp_codes)
message(sprintf("      %d answer-key rows (%d record flags)", nrow(expected_flags), nrow(exp_records)))


# ---- 7. Checks ---------------------------------------------------------------------
step(8, "Validating against form rules and answer key")

## 7.1 Form rules: relevance, required, constraints, choice codes ------------------
conv <- wide |> mutate(across(everything(), \(x) type.convert(x, as.is = TRUE)))
check_fields <- fields |> filter(!base %in% META_TYPES)

validation <- map_dfr(seq_len(nrow(check_fields)), \(j) {
  f <- check_fields[j, ]
  inst <- if (f$in_repeat) seq_len(N_REP) else NA_integer_
  map_dfr(inst, \(i) {
    d <- conv
    col <- if (f$base == "geopoint") paste0(f$name, "-Latitude") else f$name
    in_inst <- rep(TRUE, nrow(d))
    if (!is.na(i)) {
      for (rf in rep_fields) d[[rf]] <- conv[[paste0(rf, "_", i)]]
      col <- paste0(col, "_", i)
      in_inst <- i <= coalesce(num(eval(rlang::parse_expr(REP_COUNT), conv)), 0)
    }
    rel <- eval_lgl(f$rel_full, d) & in_inst
    val <- wide[[col]]
    filled <- !is.na(val) & val != ""
    con_ok <- rep(TRUE, nrow(d))
    if (f$constraint != "") {
      d$.v <- if (f$base %in% c("integer", "decimal")) num(val) else val
      d$.today <- as.Date(mdy_hms(wide$starttime))   # today() = the day the form was filled in
      con_ok <- !(filled & rel) | eval_lgl(con_to_r(f$constraint), d)
    }
    choice_ok <- rep(TRUE, nrow(d))
    if (f$name %in% names(field_codes)) {
      choice_ok <- !filled | map_lgl(str_split(coalesce(val, ""), " "), \(x) all(x %in% field_codes[[f$name]]))
    }
    tibble(KEY = wide$KEY, field = col,
           problem = case_when(filled & !rel ~ "filled_not_relevant",
                               rel & !filled & f$required & !str_starts(f$base, "calculate") ~ "missing_required",
                               !con_ok ~ "constraint_failed",
                               !choice_ok ~ "invalid_choice")) |>
      filter(!is.na(problem))
  })
})

allowed <- bind_rows(
  tibble(plant = "grid_usage_no_meter", field = c("electricity_usage_confirm", "electricity_usage_payment")),
  tibble(plant = "shs_obs_mismatch", field = "obs_shs")
) |>
  inner_join(plant_long, by = "plant") |>
  inner_join(subs |> filter(role == "main") |> select(hh_id, KEY), by = "hh_id") |>
  select(KEY, field)
stopifnot(all(allowed$KEY %in% expected_flags$KEY))

validation <- validation |>
  left_join(allowed |> mutate(allowed = TRUE), by = c("KEY", "field")) |>
  mutate(allowed = coalesce(allowed, FALSE))
validation_summary <- validation |>
  mutate(field = if_else(str_detect(field, paste0("^(", paste(rep_fields, collapse = "|"), ")_\\d+$")),
                         str_remove(field, "_\\d+$"), field)) |>
  count(problem, field, allowed, name = "n_records") |>
  arrange(allowed, problem, field)

## 7.2 Answer key self-check: reference implementation of record-level checks ------
ref <- wide |>
  left_join(hh |> select(hh_id, treat, elig), by = "hh_id") |>
  mutate(start_d = as.Date(mdy_hms(starttime)), end_d = as.Date(mdy_hms(endtime)),
         sub_d = as.Date(mdy_hms(SubmissionDate)), dur = num(survey_duration) / 60)

outlier_hits <- map_dfr(OUTLIER_VARS, \(x) {
  cols <- if (x %in% rep_fields) paste0(x, "_", seq_len(N_REP)) else x
  wide |>
    select(KEY, all_of(cols)) |>
    pivot_longer(-KEY, values_to = "val") |>
    mutate(val = num(val)) |>
    filter(!is.na(val), !val %in% c(-77, -88)) |>
    mutate(z = (log1p(val) - mean(log1p(val))) / sd(log1p(val))) |>
    filter(abs(z) > OUTLIER_SD) |>
    distinct(KEY) |>
    mutate(check = paste0("4.hfc_outliers: outlier_", x))
})
neg_mod <- function(a, b) num(ref[[b]]) - num(ref[[a]]) < 0
observed <- bind_rows(
  ref |> filter(consent == "1") |> filter(n() > 1, .by = hh_id) |> transmute(KEY, check = "2.hfc_construct: duplicate"),
  outlier_hits,
  ref |> filter(start_d != end_d) |> transmute(KEY, check = "5.hfc_logic: start_end_date"),
  ref |> filter(end_d != sub_d) |> transmute(KEY, check = "5.hfc_logic: end_submission_date"),
  ref |> filter((neg_mod("stamp_a_1", "stamp_a_2") | neg_mod("stamp_b_1", "stamp_b_2") |
                   neg_mod("stamp_c_1", "stamp_c_2") | neg_mod("stamp_nudge_1", "stamp_nudge_2")) %in% TRUE) |>
    transmute(KEY, check = "5.hfc_logic: negative_module_duration"),
  ref |> filter(consent == "1", dur < DURATION_MIN) |> transmute(KEY, check = "5.hfc_logic: duration_short"),
  ref |> filter(consent == "1", dur > DURATION_MAX) |> transmute(KEY, check = "5.hfc_logic: duration_long"),
  ref |> filter(readyboard_confirm == "1", treat %in% c("C", "T2")) |> transmute(KEY, check = "5.hfc_logic: readyboard_in_C_T2"),
  ref |> filter(!is.na(shs_willing), !treat %in% c("T2", "T3")) |> transmute(KEY, check = "5.hfc_logic: nudge_outside_T2T3"),
  ref |> filter(electricity_usage_confirm == "1", meter_confirm == "0") |> transmute(KEY, check = "5.hfc_logic: usage_without_meter"),
  ref |> filter(consent == "1", observe_electricity_connection != meter_confirm) |> transmute(KEY, check = "5.hfc_logic: observe_vs_meter_confirm"),
  ref |> filter(obs_shs == "1", shs_confirm == "0") |> transmute(KEY, check = "5.hfc_logic: obs_shs_without_shs_confirm")
) |> distinct()

missed     <- anti_join(exp_records |> select(KEY, check), observed, by = c("KEY", "check"))
unexpected <- anti_join(observed, exp_records |> select(KEY, check), by = c("KEY", "check"))

obs_enum <- c(enum_stats$enumerator[enum_stats$consent_rate < CONSENT_TARGET],
              enum_stats$enumerator[enum_stats$n_short >= SHORT_SURVEYS_ENUM_MIN])
enum_ok <- setequal(obs_enum, c(ENUM_LOW_CONSENT, ENUM_SHORT)) && length(obs_enum) == 2

## 7.3 Summary ------------------------------------------------------------------------
message("\n==== Households by arm and scenario ====")
hh |> count(scenario, treat) |> pivot_wider(names_from = treat, values_from = n, values_fill = 0) |>
  mutate(total = C + T1 + T2 + T3) |>
  arrange(match(scenario, scenarios$scenario)) |>
  print(n = Inf)

message("\n==== Submissions by arm and role/status ====")
subs |> count(role, status, treat) |> pivot_wider(names_from = treat, values_from = n, values_fill = 0) |> print(n = Inf)

message("\n==== SHS funnel (T2/T3) ====")
print(shs_funnel)

message("\n==== Enumerators ====")
print(enum_stats |> arrange(enumerator))

message("\n==== Validation summary (form rules) ====")
if (nrow(validation_summary) == 0) message("No relevance/required/constraint/choice problems.") else print(validation_summary, n = Inf)

message("\n==== Answer-key self-check ====")
message(sprintf("Record-level expected flags: %d | missed: %d | unexpected: %d | enumerator flags ok: %s",
                nrow(exp_records), nrow(missed), nrow(unexpected), enum_ok))

bad_validation <- validation |> filter(!allowed)
if (nrow(bad_validation) > 0 || nrow(missed) > 0 || nrow(unexpected) > 0 || !enum_ok) {
  if (nrow(bad_validation) > 0) print(bad_validation |> count(problem, field), n = Inf)
  if (nrow(missed) > 0) print(missed, n = Inf)
  if (nrow(unexpected) > 0) print(unexpected, n = Inf)
  stop("Validation failed: see problems above. Nothing was written.")
}
message("Validation passed: only planted problems fail.")


# ---- 8. Export ------------------------------------------------------------------------
step(9, "Writing files to Dropbox")

dir.create(preload_dir, recursive = TRUE, showWarnings = FALSE)

geo_names <- hh |> transmute(villageid_key = sprintf("%.0f", villageid_key), village = village_key,
                             cell = cell_key, sector = sector_key, district, householdid_key = hh_id)

household_head <- geo_names |>
  bind_cols(hh |> select(first_name, last_name, nid)) |>
  mutate(household_id = householdid_key)

raw_phone <- function(x) if_else(x == "", "", sub("^0", "", x))
contact_info <- geo_names |>
  bind_cols(hh |> transmute(first_name, last_name, gender, nid,
                            phonenumber = raw_phone(phone_clean), second_phonenumber = raw_phone(phone2_clean),
                            coordinate.Latitude = lat, coordinate.Longitude = lon,
                            coordinate.Altitude = alt, coordinate.Accuracy = round(runif(n(), 3, 15), 1),
                            phone_clean, phone2_clean, treat, treat_label))

shs_eligible <- geo_names |>
  bind_cols(hh |> select(treat, treat_label, elig)) |>
  filter(elig) |>
  transmute(villageid_key, village, cell, sector, district, householdid_key, treat, treat_label, eligible = 1)

scope_villages <- villages |>
  transmute(provinceid_key, province_key, districtid_key, district_key = district,
            sectorid_key, sector_key, cellid_key, cell_key,
            villageid_key = sprintf("%.0f", villageid_key), village_key)

test_enumerators <- enumerators |> select(enumeratorid_key, enumerator_key, district_key, districtid_key)

# Same layout as the real preload files (unnamed row-number first column)
write_preload <- function(df, file) write.csv(df, file.path(preload_dir, file), na = "")
write_preload(household_head, "household_head.csv")
write_preload(contact_info, "contact_info.csv")
write_preload(shs_eligible, "shs_eligible.csv")
write_preload(scope_villages, "scope_villages.csv")
write_csv(test_enumerators, file.path(preload_dir, "test_enumerators.csv"), na = "")

write_csv(wide, file.path(data_dir, paste0(FORM_ID, "_WIDE.csv")), na = "")
write_csv(expected_flags, file.path(data_dir, "expected_flags.csv"), na = "")

message(sprintf("\nFiles written to %s:", data_dir))
walk(c(paste0(FORM_ID, "_WIDE.csv"), "expected_flags.csv",
       file.path("preload", c("household_head.csv", "contact_info.csv", "shs_eligible.csv", "scope_villages.csv", "test_enumerators.csv"))),
     \(f) message("  ", f))
message(sprintf("[0.generate_synthetic_data] done in %.1fs", as.numeric(difftime(Sys.time(), t_start, units = "secs"))))
