# ==============================================================================
# 10.hfc_dashboard.R
# Clean cooking HFC: Excel dashboard workbook. First sheet "Dashboard" (key rates
# vs targets, progress by stove type and district, logic flags, usage parameter
# preview, key variables, daily submissions chart), then every output as its own
# formatted tab. Each dashboard table has a definition row under its column names.
#
# Sourced by 1.hfc_main.R after scripts 2-9 (uses hfc_outputs, hfc_constr,
# DASH_FILE, save_current() and the targets in the config).
# Output: DASH_FILE (output_dir/HFC_cleancooking_dashboard.xlsx; the previous
# version is moved to archive_dir)
#
# Author: Tanay (using Claude) · Last modified: 2026-11-04
# ==============================================================================


# ---- 1. Config / constants ---------------------------------------------------

TAB_FIRST  <- c("enumerator_check", "stove_data", "district_data", "village_data", "duplicates",
                "logic_summary", "logic_data", "usage_params")
DASH_SHEET <- "Dashboard"
CHART_COL  <- 15

st_title   <- openxlsx::createStyle(fontSize = 16, textDecoration = "bold", fontColour = "#1F4E79")
st_section <- openxlsx::createStyle(fontSize = 12, textDecoration = "bold", fontColour = "#1F4E79")
st_note    <- openxlsx::createStyle(fontColour = "#595959", textDecoration = "italic")
st_header  <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DDEBF7", border = "bottom",
                                    wrapText = TRUE, valign = "top")
st_def     <- openxlsx::createStyle(fontColour = "#595959", textDecoration = "italic", fontSize = 9,
                                    fgFill = "#F2F2F2", wrapText = TRUE, valign = "top")
st_good    <- openxlsx::createStyle(fgFill = "#C6EFCE", fontColour = "#006100")
st_bad     <- openxlsx::createStyle(fgFill = "#FFC7CE", fontColour = "#9C0006")

DEF_PROGRESS <- c(
  stove_group = "Stove type on the beneficiary list", district_key = "District",
  target = "Households in the preload (sample)",
  hh_attempted = "Households with at least one submitted form",
  hh_complete = "Households located, consented and reaching the last question, observe_stoves (counted once)",
  complete_of_target_pct = "Progress: hh_complete / target",
  located_of_attempted_pct = "Households located / hh_attempted",
  consented_of_located_pct = "Households consented / households located",
  complete_of_attempted_pct = "hh_complete / hh_attempted"
)
DEF_KEYVARS <- c(
  var = "Form variable", type = "% yes: share answering yes; value: a number",
  n = "Consented submissions answering", mean_or_pct = "Mean, or % yes",
  median = "Median (numbers only)", asked_if = "Who is asked (form relevance)"
)


# ---- 2. Helper functions -----------------------------------------------------

share_pct <- function(n, d) round(if_else(d > 0, 100 * n / d, NA_real_), 1)

write_block <- function(wb, title, df, row, notes = NULL, defs = NULL) {
  openxlsx::writeData(wb, DASH_SHEET, title, startRow = row, startCol = 1)
  openxlsx::addStyle(wb, DASH_SHEET, st_section, rows = row, cols = 1)
  for (nt in notes) {
    row <- row + 1
    openxlsx::writeData(wb, DASH_SHEET, nt, startRow = row, startCol = 1)
    openxlsx::addStyle(wb, DASH_SHEET, st_note, rows = row, cols = 1)
  }
  header_row <- row + 1
  openxlsx::writeData(wb, DASH_SHEET, df[0, ], startRow = header_row, startCol = 1, headerStyle = st_header)
  data_row <- header_row + 1
  if (!is.null(defs)) {
    def_line <- as.data.frame(t(unname(coalesce(defs[names(df)], ""))))
    openxlsx::writeData(wb, DASH_SHEET, def_line, startRow = data_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, DASH_SHEET, st_def, rows = data_row, cols = seq_len(ncol(df)), gridExpand = TRUE)
    openxlsx::setRowHeights(wb, DASH_SHEET, rows = data_row, heights = 48)
    data_row <- data_row + 1
  }
  openxlsx::writeData(wb, DASH_SHEET, df, startRow = data_row, startCol = 1, colNames = FALSE, keepNA = FALSE)
  list(next_row = data_row + nrow(df) + 2, data_row = data_row)
}

write_tab <- function(wb, name, df) {
  openxlsx::addWorksheet(wb, name)
  openxlsx::writeData(wb, name, df, headerStyle = st_header, keepNA = FALSE, withFilter = TRUE)
  openxlsx::freezePane(wb, name, firstRow = TRUE)
  openxlsx::setColWidths(wb, name, cols = seq_len(ncol(df)), widths = pmin(pmax(nchar(names(df)), 8) + 2, 40))
}


# ---- 3. Dashboard tables -----------------------------------------------------

run_info <- make_run_info()
hfc_outputs$run_info <- run_info
totals <- hfc_outputs$stove_data |>
  summarise(across(c(target, hh_attempted, hh_located, hh_consented, hh_complete), sum))
logic_rate <- \(chk) hfc_outputs$logic_summary |> filter(check == chk) |> pull(pct_of_submissions)

kpi <- tibble(
  indicator = c("Locate rate", "Consent rate", "Completion rate", "Submissions with an inconsistency",
                "Submissions with an answer to verify"),
  definition = c("Households located / households attempted",
                 "Households consented / households located",
                 "Households complete / households attempted",
                 "Submissions with at least one 'inconsistent' logic flag / all submissions",
                 "Submissions with at least one 'verify' flag / all submissions (back-check list; no target)"),
  value_pct = c(share_pct(totals$hh_located, totals$hh_attempted),
                share_pct(totals$hh_consented, totals$hh_located),
                share_pct(totals$hh_complete, totals$hh_attempted),
                logic_rate("5.hfc_logic: any_inconsistent"),
                logic_rate("5.hfc_logic: any_verify")),
  target = c(sprintf(c("> %g%%", "> %g%%", "> %g%%", "< %g%%"),
                     100 * c(LOCATE_TARGET, CONSENT_TARGET, COMPLETION_TARGET, LOGIC_TARGET)), "none")
) |>
  mutate(on_target = c(value_pct[1:3] >= 100 * c(LOCATE_TARGET, CONSENT_TARGET, COMPLETION_TARGET),
                       value_pct[4] < 100 * LOGIC_TARGET, NA),
         status = case_when(is.na(on_target) ~ "info", on_target ~ "on target", .default = "off target"))

prog_cols <- c("target", "hh_attempted", "hh_complete", "complete_of_target_pct", "located_of_attempted_pct",
               "consented_of_located_pct", "complete_of_attempted_pct")
progress_stove    <- hfc_outputs$stove_data |> select(stove_group, all_of(prog_cols))
progress_district <- hfc_outputs$district_data |> select(district_key, all_of(prog_cols))

last_day <- max(hfc_constr$submissiondate)
logic_block <- hfc_outputs$logic_summary |>
  filter(!str_detect(check, "any_")) |>
  select(kind, issue, total, pct_of_submissions) |>
  left_join(hfc_outputs$logic_data |> filter(submissiondate == last_day) |> count(issue, name = "latest_day"),
            by = "issue") |>
  mutate(latest_day = coalesce(latest_day, 0L)) |>
  arrange(kind, desc(total))
DEF_LOGIC <- c(kind = "inconsistent: counts toward the target; verify: call back to confirm",
               issue = "Logic flag (detail: logic_data tab)", total = "Submissions with this flag",
               pct_of_submissions = "total / all submissions", latest_day = paste("On submissions of", last_day))

usage_block <- hfc_outputs$usage_params |>
  select(parameter, definition, stove_group, mean) |>
  pivot_wider(names_from = stove_group, values_from = mean) |>
  left_join(hfc_outputs$usage_params |> filter(stove_group == "All") |> select(parameter, n_all = n, ci_low, ci_high),
            by = "parameter")

key_vars <- hfc_outputs$desc_stats_data |>
  select(var, type, n, mean_or_pct, median, any_of(unname(STOVE_TYPES)), asked_if)

chart_file <- tempfile(fileext = ".png")
chart <- hfc_constr |>
  count(submissiondate, district_key) |>
  ggplot(aes(submissiondate, n, fill = district_key)) +
  geom_col() +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  labs(title = "Submissions per day, by district", x = NULL, y = "Submissions", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
ggsave(chart_file, chart, width = 7, height = 4, dpi = 150)


# ---- 4. Build workbook -------------------------------------------------------

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, DASH_SHEET, gridLines = FALSE)

openxlsx::writeData(wb, DASH_SHEET, "ASCENT clean cooking usage survey: high-frequency checks", startRow = 1, startCol = 1)
openxlsx::addStyle(wb, DASH_SHEET, st_title, rows = 1, cols = 1)
openxlsx::writeData(wb, DASH_SHEET, startRow = 2, startCol = 1, x = sprintf(
  "%s mode | updated %s | form version %.0f | %d submissions (%s to %s) | %d of %d households complete (%.1f%%)",
  run_info$mode, run_info$run_time, run_info$current_formdef, run_info$submissions,
  run_info$first_submission, run_info$last_submission,
  totals$hh_complete, totals$target, share_pct(totals$hh_complete, totals$target)))
openxlsx::addStyle(wb, DASH_SHEET, st_note, rows = 2, cols = 1)

b <- write_block(wb, "Key rates vs targets", kpi |> select(-on_target), 4,
                 notes = "Targets are set in the config section of 1.hfc_main.R.")
openxlsx::addStyle(wb, DASH_SHEET, st_good, rows = b$data_row - 1 + which(kpi$on_target %in% TRUE), cols = 5, gridExpand = TRUE)
openxlsx::addStyle(wb, DASH_SHEET, st_bad, rows = b$data_row - 1 + which(kpi$on_target %in% FALSE), cols = 5, gridExpand = TRUE)

b <- write_block(wb, "Progress by stove type", progress_stove, b$next_row, defs = DEF_PROGRESS,
                 notes = "Households. Submissions per day: stove_data tab.")
b <- write_block(wb, "Progress by district", progress_district, b$next_row, defs = DEF_PROGRESS,
                 notes = "Households. Villages: village_data tab.")
b <- write_block(wb, "Logic flags", logic_block, b$next_row, defs = DEF_LOGIC)
b <- write_block(wb, "Usage parameters (preliminary, unweighted)", usage_block, b$next_row,
                 notes = c("Consented households, first submission. Means by stove type; CI is for All.",
                           "Monitoring only: report values come from the analysis scripts."))
b <- write_block(wb, "Key variables (consented submissions)", key_vars, b$next_row, defs = DEF_KEYVARS,
                 notes = "Choice questions (all answers): desc_cat_data tab. Stove stacking: stove_use tab.")

openxlsx::insertImage(wb, DASH_SHEET, chart_file, startRow = 4, startCol = CHART_COL, width = 7, height = 4)
openxlsx::setColWidths(wb, DASH_SHEET, cols = 1, widths = 34)
openxlsx::setColWidths(wb, DASH_SHEET, cols = 2:13, widths = 16)

tab_order <- c(intersect(TAB_FIRST, names(hfc_outputs)), setdiff(names(hfc_outputs), c(TAB_FIRST, "run_info")))
for (nm in tab_order) write_tab(wb, nm, hfc_outputs[[nm]])
write_tab(wb, "run_info", run_info)


# ---- 5. Export ---------------------------------------------------------------

saved_dash <- save_current(DASH_FILE, \(p) openxlsx::saveWorkbook(wb, p, overwrite = TRUE))

message(sprintf("      dashboard: %s (%d tabs)", basename(saved_dash), length(tab_order) + 2))
