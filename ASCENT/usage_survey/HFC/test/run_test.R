# ==============================================================================
# run_test.R
# Clean cooking HFC end-to-end test: generate synthetic data, deidentify, run
# the HFC, back-check list, and compare the flags with the answer key. Each step
# runs in its own Rscript process with HFC_MODE=test (environment variable), so
# it never touches live data whatever hfc_config.R says.
#
# Run from the repo root:
#   Rscript ASCENT/usage_survey/HFC/test/run_test.R
#
# Author: Tanay (using Claude) · Last modified: 2026-11-04
# ==============================================================================

steps <- c("ASCENT/usage_survey/HFC/test/0.generate_synthetic_data.R",
           "ASCENT/usage_survey/HFC/scripts/0.hfc_deidentify.R",
           "ASCENT/usage_survey/HFC/scripts/1.hfc_main.R",
           "ASCENT/usage_survey/HFC/scripts/11.hfc_backcheck.R",
           "ASCENT/usage_survey/HFC/test/compare_expected_flags.R")

rscript <- file.path(R.home("bin"), "Rscript")
Sys.setenv(HFC_MODE = "test")
for (s in steps) {
  message("\n======== ", basename(s), " ========")
  status <- system2(rscript, shQuote(s))
  if (status != 0) stop("Step failed: ", s)
}
message("\nAll test steps finished.")
