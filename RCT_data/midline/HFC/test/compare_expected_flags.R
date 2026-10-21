# ==============================================================================
# compare_expected_flags.R
# Compares the outputs of a test-mode HFC run (1.hfc_main.R, HFC_MODE = "test")
# with the synthetic answer key expected_flags.csv. Only checks from scripts
# that ran are compared. Stops with an error if any flag is missed or unexpected.
#
# Run from the repo root, after the HFC run:
#   Rscript RCT_data/midline/HFC/test/compare_expected_flags.R
#
# Author: Yeji (using Claude) · Last modified: 2026-10-21
# ==============================================================================


# ---- 1. Setup ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
})

if (!file.exists("PATHS.R")) stop("Run this script from the repo root (PATHS.R not found).")
source("PATHS.R")


# ---- 2. Config / constants ---------------------------------------------------

data_dir   <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/synthetic_data")
output_dir <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/test/output")


# ---- 3. Helper functions -----------------------------------------------------

latest <- function(pattern) {
  f <- sort(list.files(output_dir, pattern = pattern, full.names = TRUE), decreasing = TRUE)
  if (length(f) == 0) stop("No file matching ", pattern, " in ", output_dir, ": run 1.hfc_main.R first.")
  f[1]
}
script_of <- function(check) str_extract(check, "^\\d+\\.hfc_\\w+")


# ---- 4. Data loading ---------------------------------------------------------

expected <- read_csv(file.path(data_dir, "expected_flags.csv"), col_types = cols(.default = col_character()))
flags    <- read_csv(latest("^hfc_flags_\\d{8}\\.csv$"), col_types = cols(.default = col_character()))
outputs_file <- latest("^hfc_outputs_\\d{8}\\.xlsx$")
run_info <- read_excel(outputs_file, "run_info")

scripts_run <- str_remove(str_split_1(run_info$scripts_run, ", "), "\\.R$")
if (run_info$mode != "test") stop("Latest HFC run is not in test mode.")
message("Comparing run of ", run_info$run_time, " (scripts: ", run_info$scripts_run, ")")

expected <- expected |> mutate(script = script_of(check)) |> filter(script %in% scripts_run)
flags    <- flags |> mutate(script = script_of(check))


# ---- 5. Checks ---------------------------------------------------------------

## 5.1 Submission-level flags (scripts 2, 4, 5): join on KEY + check
exp_rec <- expected |> filter(!is.na(KEY), !str_starts(check, "7\\.")) |> distinct(KEY, check)
obs_rec <- flags |> filter(!is.na(KEY)) |> distinct(KEY, check)
rec <- bind_rows(
  inner_join(exp_rec, obs_rec, by = c("KEY", "check")) |> mutate(result = "matched"),
  anti_join(exp_rec, obs_rec, by = c("KEY", "check")) |> mutate(result = "missed"),
  anti_join(obs_rec, exp_rec, by = c("KEY", "check")) |> mutate(result = "unexpected")
)

## 5.2 Enumerator flags (script 6): join on enumerator id + check
exp_enum <- expected |>
  filter(str_starts(check, "6\\.")) |>
  transmute(enumerator = str_match(description, "enumerator=(\\d+)")[, 2], check)
obs_enum <- flags |> filter(str_starts(check, "6\\."), is.na(KEY)) |> distinct(enumerator, check)
enum <- bind_rows(
  inner_join(exp_enum, obs_enum, by = c("enumerator", "check")) |> mutate(result = "matched"),
  anti_join(exp_enum, obs_enum, by = c("enumerator", "check")) |> mutate(result = "missed"),
  anti_join(obs_enum, exp_enum, by = c("enumerator", "check")) |> mutate(result = "unexpected")
)

## 5.3 Special-code coverage (script 7): each expected field/code appears in desc_cat_data
codes <- tibble(check = character(), var = character(), result = character())
if ("7.hfc_stats" %in% scripts_run) {
  cat_data <- read_excel(outputs_file, "desc_cat_data", col_types = "text")
  exp_codes <- expected |>
    filter(str_starts(check, "7\\.")) |>
    transmute(check, var = str_extract(description, "^\\w+"), code = str_remove(check, "^.*code_"))
  codes <- exp_codes |>
    left_join(cat_data |> distinct(var, code) |> mutate(found = TRUE), by = c("var", "code")) |>
    transmute(check, var, result = if_else(coalesce(found, FALSE), "matched", "missed"))
}

## 5.4 SHS funnel counts (script 9): join on arm + step, compare counts
funnel <- tibble(check = character(), result = character())
if ("9.hfc_shs" %in% scripts_run) {
  shs_funnel <- read_excel(outputs_file, "shs_funnel")
  exp_funnel <- expected |>
    filter(str_starts(check, "9\\.")) |>
    transmute(treat = str_match(description, "arm=(T\\d)")[, 2], step = str_remove(check, "^.*funnel_"),
              expected_n = as.integer(expected_flag))
  funnel <- exp_funnel |>
    left_join(shs_funnel |> transmute(treat, step, observed_n = as.integer(n)), by = c("treat", "step")) |>
    mutate(check = paste0("9.hfc_shs: funnel_", step),
           result = if_else(expected_n %in% observed_n, "matched", "missed"))
}


# ---- 6. Report ---------------------------------------------------------------

all_results <- bind_rows(rec, enum, codes, funnel |> select(check, result))
summary_tbl <- all_results |>
  count(check, result) |>
  pivot_wider(names_from = result, values_from = n, values_fill = 0)
for (cl in setdiff(c("matched", "missed", "unexpected"), names(summary_tbl))) summary_tbl[[cl]] <- 0L
summary_tbl <- summary_tbl |> select(check, matched, missed, unexpected) |> arrange(check)

not_covered <- setdiff(unique(expected$check), all_results$check)

message("\n==== Expected vs flagged, by check ====")
print(summary_tbl, n = Inf)
if (nrow(funnel) > 0) {
  message("\n==== SHS funnel ====")
  print(funnel |> select(treat, step, expected_n, observed_n, result), n = Inf)
}

bad <- all_results |> filter(result != "matched")
if (length(not_covered) > 0) message("\nExpected checks with no output: ", paste(not_covered, collapse = ", "))
if (nrow(bad) > 0 || length(not_covered) > 0) {
  message("\n==== Problems ====")
  print(bad, n = Inf)
  stop(sprintf("Test failed: %d missed, %d unexpected, %d checks without output.",
               sum(bad$result == "missed"), sum(bad$result == "unexpected"), length(not_covered)))
}
message(sprintf("\nTest passed: %d expected results matched for %s.", nrow(all_results), paste(scripts_run, collapse = ", ")))
