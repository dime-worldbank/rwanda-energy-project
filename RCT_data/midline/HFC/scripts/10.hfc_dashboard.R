# ==============================================================================
# 10.hfc_dashboard.R
# Midline HFC: Excel dashboard workbook. First sheet "Dashboard" (key rates vs
# targets, progress by arm with the SHS nudge, progress by district, logic
# issues, energy access, key variables, daily submissions chart), then every
# output as its own formatted tab. Each dashboard table has a definition row
# under its column names.
#
# Tab order: Dashboard, enumerator_check, district_data, village_data,
# duplicates, logic tabs, then the remaining outputs, run_info last.
#
# Sourced by 1.hfc_main.R after scripts 2-9 (uses hfc_outputs, hfc_constr,
# DASH_FILE, save_current() and the targets in the config).
# Output: DASH_FILE (output_dir/HFC_midline_dashboard.xlsx; the previous version
# is moved to archive_dir)
#
# Author: Yeji (using Claude) · Last modified: 2026-10-22
# ==============================================================================


# ---- 1. Config / constants ---------------------------------------------------

TAB_FIRST  <- c("enumerator_check", "district_data", "village_data", "duplicates", "logic_summary", "logic_data")
DASH_SHEET <- "Dashboard"
CHART_COL  <- 15                                 # dashboard column where the chart starts

st_title   <- openxlsx::createStyle(fontSize = 16, textDecoration = "bold", fontColour = "#1F4E79")
st_section <- openxlsx::createStyle(fontSize = 12, textDecoration = "bold", fontColour = "#1F4E79")
st_note    <- openxlsx::createStyle(fontColour = "#595959", textDecoration = "italic")
st_header  <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DDEBF7", border = "bottom",
                                    wrapText = TRUE, valign = "top")
st_def     <- openxlsx::createStyle(fontColour = "#595959", textDecoration = "italic", fontSize = 9,
                                    fgFill = "#F2F2F2", wrapText = TRUE, valign = "top")
st_good    <- openxlsx::createStyle(fgFill = "#C6EFCE", fontColour = "#006100")
st_bad     <- openxlsx::createStyle(fgFill = "#FFC7CE", fontColour = "#9C0006")

# Column definitions shown under the column names
DEF_PROGRESS <- c(
  treat = "Treatment arm", district_key = "District",
  target = "Households in the preload (sample)",
  hh_attempted = "Households with at least one submitted form",
  hh_complete = "Households located, consented and finished (counted once; duplicates removed)",
  complete_of_target_pct = "Progress: hh_complete / target",
  located_of_attempted_pct = "Households located / hh_attempted",
  consented_of_located_pct = "Households consented / households located",
  complete_of_attempted_pct = "hh_complete / hh_attempted",
  nudge_eligible = "T2/T3: asked the SHS nudge (preload-eligible, no SHS, not electrified)",
  nudge_agreed = "shs_willing = yes (agreed to receive the subsidy)",
  nudge_agreed_of_eligible_pct = "nudge_agreed / nudge_eligible",
  nudge_data_consent = "shs_data_consent = yes (asked only if agreed)"
)
DEF_ENERGY <- c(
  treat = "Treatment arm (All = total)",
  households = "Consented households",
  meter_pct = "meter_confirm = 1",
  readyboard_pct = "readyboard_confirm = 1",
  internal_wiring_pct = "additional_wire_confirm = 1",
  grid_use_pct = "electricity_usage_confirm = 1",
  shs_pct = "shs_confirm = 1",
  meter_and_shs_pct = "meter_confirm = 1 and shs_confirm = 1",
  none_pct = "meter_confirm = 0 and shs_confirm = 0"
)
DEF_KEYVARS <- c(
  var = "Form variable", type = "% yes: share answering yes; value: a number",
  n = "Consented submissions answering", mean_or_pct = "Mean, or % yes",
  median = "Median (numbers only)", C = "Arm: mean or % yes", T1 = "Arm: mean or % yes",
  T2 = "Arm: mean or % yes", T3 = "Arm: mean or % yes", asked_if = "Who is asked (form relevance)"
)


# ---- 2. Helper functions -----------------------------------------------------

share_pct <- function(n, d) round(if_else(d > 0, 100 * n / d, NA_real_), 1)

# Section title, notes, table with a definition row under the column names; returns the next free row
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

# One output as a formatted tab: header style, frozen header row, filters, column widths
write_tab <- function(wb, name, df) {
  openxlsx::addWorksheet(wb, name)
  openxlsx::writeData(wb, name, df, headerStyle = st_header, keepNA = FALSE, withFilter = TRUE)
  openxlsx::freezePane(wb, name, firstRow = TRUE)
  openxlsx::setColWidths(wb, name, cols = seq_len(ncol(df)),
                         widths = pmin(pmax(nchar(names(df)), 8) + 2, 40))
}


# ---- 3. Dashboard tables -----------------------------------------------------

run_info  <- make_run_info()
treatment <- hfc_outputs$treatment_data
totals    <- treatment |> summarise(across(c(target, hh_attempted, hh_located, hh_consented, hh_complete), sum))
logic_any <- hfc_outputs$logic_summary |> filter(check == "5.hfc_logic: any_issue")

kpi <- tibble(
  indicator = c("Locate rate", "Consent rate", "Completion rate", "Submissions with a logic issue"),
  definition = c("Households located / households attempted",
                 "Households consented / households located",
                 "Households complete / households attempted",
                 "Submissions with at least one logic issue / all submissions"),
  value_pct = c(share_pct(totals$hh_located, totals$hh_attempted),
                share_pct(totals$hh_consented, totals$hh_located),
                share_pct(totals$hh_complete, totals$hh_attempted),
                logic_any$pct_of_submissions),
  target = sprintf(c("> %g%%", "> %g%%", "> %g%%", "< %g%%"),
                   100 * c(LOCATE_TARGET, CONSENT_TARGET, COMPLETION_TARGET, LOGIC_TARGET))
) |>
  mutate(on_target = c(value_pct[1:3] >= 100 * c(LOCATE_TARGET, CONSENT_TARGET, COMPLETION_TARGET),
                       value_pct[4] < 100 * LOGIC_TARGET),
         status = if_else(on_target, "on target", "off target"))

progress_arm <- treatment |>
  select(treat, target, hh_attempted, hh_complete, complete_of_target_pct, located_of_attempted_pct,
         consented_of_located_pct, complete_of_attempted_pct,
         nudge_eligible, nudge_agreed, nudge_agreed_of_eligible_pct, nudge_data_consent)

progress_district <- hfc_outputs$district_data |>
  select(district_key, target, hh_attempted, hh_complete, complete_of_target_pct, located_of_attempted_pct,
         consented_of_located_pct, complete_of_attempted_pct)

last_day <- max(hfc_constr$submissiondate)
logic_block <- hfc_outputs$logic_summary |>
  filter(check != "5.hfc_logic: any_issue") |>
  select(issue, total, pct_of_submissions) |>
  left_join(hfc_outputs$logic_data |> filter(submissiondate == last_day) |> count(issue, name = "latest_day"),
            by = "issue") |>
  mutate(latest_day = coalesce(latest_day, 0L)) |>
  arrange(desc(total))
DEF_LOGIC <- c(issue = "Logic issue (detail: logic_data tab)", total = "Submissions with this issue",
               pct_of_submissions = "total / all submissions", latest_day = paste("On submissions of", last_day))

key_vars <- hfc_outputs$desc_stats_data |>
  select(var, type, n, mean_or_pct, median, any_of(ARMS), asked_if)

# Daily submissions chart (image: openxlsx cannot create native Excel charts)
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

openxlsx::writeData(wb, DASH_SHEET, "R-EAQIP midline: high-frequency checks", startRow = 1, startCol = 1)
openxlsx::addStyle(wb, DASH_SHEET, st_title, rows = 1, cols = 1)
openxlsx::writeData(wb, DASH_SHEET, startRow = 2, startCol = 1, x = sprintf(
  "%s mode | updated %s | form version %.0f | %d submissions (%s to %s) | %d of %d households complete (%.1f%%)",
  run_info$mode, run_info$run_time, run_info$current_formdef, run_info$submissions,
  run_info$first_submission, run_info$last_submission,
  totals$hh_complete, totals$target, share_pct(totals$hh_complete, totals$target)))
openxlsx::addStyle(wb, DASH_SHEET, st_note, rows = 2, cols = 1)

b <- write_block(wb, "Key rates vs targets", kpi |> select(-on_target), 4,
                 notes = "Targets are set in the config section of 1.hfc_main.R.")
openxlsx::addStyle(wb, DASH_SHEET, st_good, rows = b$data_row - 1 + which(kpi$on_target), cols = 5, gridExpand = TRUE)
openxlsx::addStyle(wb, DASH_SHEET, st_bad, rows = b$data_row - 1 + which(!kpi$on_target), cols = 5, gridExpand = TRUE)

b <- write_block(wb, "Progress by treatment arm", progress_arm, b$next_row, defs = DEF_PROGRESS,
                 notes = "Households. Submissions per day and more columns: treatment_data tab.")
b <- write_block(wb, "Progress by district", progress_district, b$next_row, defs = DEF_PROGRESS,
                 notes = "Households. Submissions per day: district_data tab (villages: village_data tab).")
b <- write_block(wb, "Logic issues", logic_block, b$next_row, defs = DEF_LOGIC)
b <- write_block(wb, "Energy access (% of consented households)", hfc_outputs$energy_access, b$next_row,
                 defs = DEF_ENERGY,
                 notes = c("Has the device, whether or not it works (working: X_confirm_working in Key variables).",
                           "Readyboard, wiring and grid use are asked only if the household has a meter; others count as no."))
b <- write_block(wb, "Key variables (consented submissions)", key_vars, b$next_row, defs = DEF_KEYVARS,
                 notes = "Amounts in Rwf. Choice questions (all answers): desc_cat_data tab.")

openxlsx::insertImage(wb, DASH_SHEET, chart_file, startRow = 4, startCol = CHART_COL, width = 7, height = 4)
openxlsx::setColWidths(wb, DASH_SHEET, cols = 1, widths = 34)
openxlsx::setColWidths(wb, DASH_SHEET, cols = 2:13, widths = 16)

# Other tabs: the agreed order first, then the remaining outputs, run_info last
tab_order <- c(intersect(TAB_FIRST, names(hfc_outputs)),
               setdiff(names(hfc_outputs), c(TAB_FIRST, "run_info")))
for (nm in tab_order) write_tab(wb, nm, hfc_outputs[[nm]])
write_tab(wb, "run_info", run_info)


# ---- 5. Export ---------------------------------------------------------------

saved_dash <- save_current(DASH_FILE, \(p) openxlsx::saveWorkbook(wb, p, overwrite = TRUE))

message(sprintf("      dashboard: %s (%d tabs)", basename(saved_dash), length(tab_order) + 2))
