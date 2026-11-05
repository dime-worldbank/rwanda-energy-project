# ==============================================================================
# compare_expected_flags.R
# Compares a test-mode run of the clean cooking HFC with the synthetic answer
# key expected_flags.csv. Only checks from scripts that ran are compared.
# Stops with an error if any flag is missed or unexpected.
#
# Run from the repo root, after 0.hfc_deidentify.R and 1.hfc_main.R in test mode
# (or run test/run_test.R, which does all steps):
#   Rscript ASCENT/usage_survey/HFC/test/compare_expected_flags.R
#
# Author: Tanay (using Claude) · Last modified: 2026-11-04
# ==============================================================================


# ---- 1. Setup ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
})
if (!file.exists("PATHS.R")) stop("Run this script from the repo root (PATHS.R not found).")
source("PATHS.R")
HFC_MODE <- "test"
source("ASCENT/usage_survey/HFC/scripts/hfc_config.R")

outputs_file <- file.path(output_dir, "HFC_cleancooking_dashboard.xlsx")
flags_file   <- file.path(checks_dir, "hfc_flags.csv")
script_of <- function(check) str_extract(check, "^\\d+\\.hfc_\\w+")


# ---- 2. Data loading ---------------------------------------------------------

if (!all(file.exists(c(outputs_file, flags_file)))) stop("HFC outputs not found: run 1.hfc_main.R in test mode first.")
expected <- read_csv(file.path(raw_data_dir, "expected_flags.csv"), col_types = cols(.default = col_character()))
flags    <- read_csv(flags_file, col_types = cols(.default = col_character()))
run_info <- read_excel(outputs_file, "run_info")

if (run_info$mode != "test") stop("Latest HFC run is not in test mode.")
scripts_run <- str_remove(str_split_1(run_info$scripts_run, ", "), "\\.R$")
message("Comparing run of ", run_info$run_time, " (scripts: ", run_info$scripts_run, ")")

expected <- expected |> mutate(script = script_of(check)) |> filter(script %in% scripts_run)


# ---- 3. Checks ---------------------------------------------------------------

## Submission-level flags (scripts 2, 4, 5): KEY + check
exp_rec <- expected |> filter(!is.na(KEY)) |> distinct(KEY, check)
obs_rec <- flags |> filter(!is.na(KEY)) |> distinct(KEY, check)
rec <- bind_rows(
  inner_join(exp_rec, obs_rec, by = c("KEY", "check")) |> mutate(result = "matched"),
  anti_join(exp_rec, obs_rec, by = c("KEY", "check")) |> mutate(result = "missed"),
  anti_join(obs_rec, exp_rec, by = c("KEY", "check")) |> mutate(result = "unexpected")
)

## Enumerator flags (script 6): enumerator id + check
exp_enum <- expected |> filter(str_starts(check, "6\\.")) |>
  transmute(enumerator = str_match(description, "enumerator=(\\d+)")[, 2], check)
obs_enum <- flags |> filter(str_starts(check, "6\\."), is.na(KEY)) |> distinct(enumerator, check)
enum <- bind_rows(
  inner_join(exp_enum, obs_enum, by = c("enumerator", "check")) |> mutate(result = "matched"),
  anti_join(exp_enum, obs_enum, by = c("enumerator", "check")) |> mutate(result = "missed"),
  anti_join(obs_enum, exp_enum, by = c("enumerator", "check")) |> mutate(result = "unexpected")
)

## Privacy: the deidentified export must not hold the planted free text / device columns
deid_cols <- names(read_csv(file.path(data_dir, paste0(FORM_ID, "_WIDE.csv")), n_max = 0, show_col_types = FALSE))
PRIVATE_FIELDS <- c("deviceid", "subscriberid", "simid", "devicephonenum", "audio", "usage_comment", "comment",
                    "member_firstname", "member_lastname", "member_name", "member_dob",
                    "cooking_member_name", "firewood_member_name")
leaked <- str_subset(deid_cols, paste0("^(", paste(PRIVATE_FIELDS, collapse = "|"), ")(_\\d+)*$"))
privacy <- tibble(check = "0.hfc_deidentify: dropped_columns", result = if (length(leaked)) "missed" else "matched")


# ---- 4. Report ---------------------------------------------------------------

all_results <- bind_rows(rec, enum, privacy)
summary_tbl <- all_results |>
  count(check, result) |>
  pivot_wider(names_from = result, values_from = n, values_fill = 0)
for (cl in setdiff(c("matched", "missed", "unexpected"), names(summary_tbl))) summary_tbl[[cl]] <- 0L
summary_tbl <- summary_tbl |> select(check, matched, missed, unexpected) |> arrange(check)

not_covered <- setdiff(unique(expected$check), all_results$check)

message("\n==== Expected vs flagged, by check ====")
print(summary_tbl, n = Inf)

bad <- all_results |> filter(result != "matched")
if (length(leaked) > 0) message("Columns that reached the HFC: ", paste(leaked, collapse = ", "))
if (length(not_covered) > 0) message("\nExpected checks with no output: ", paste(not_covered, collapse = ", "))
if (nrow(bad) > 0 || length(not_covered) > 0) {
  message("\n==== Problems ====")
  print(bad, n = Inf)
  stop(sprintf("Test failed: %d missed, %d unexpected, %d checks without output.",
               sum(bad$result == "missed"), sum(bad$result == "unexpected"), length(not_covered)))
}
message(sprintf("\nTest passed: %d expected results matched for %s.", nrow(all_results), paste(scripts_run, collapse = ", ")))
