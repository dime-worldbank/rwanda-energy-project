# 04_export.R -------------------------------------------------------------
# Persist every entry in `tables` and `figures` to disk under output/.

# Tables: CSV for tibbles, HTML for gtsummary objects.
purrr::iwalk(tables, function(obj, name) {
  if (inherits(obj, "gtsummary")) {
    out_html <- file.path(paths$out_tab, paste0(name, ".html"))
    gt_obj <- gtsummary::as_gt(obj)
    gt::gtsave(gt_obj, filename = out_html)
    message("Wrote ", out_html)
  } else {
    out_csv <- file.path(paths$out_tab, paste0(name, ".csv"))
    readr::write_csv(tibble::as_tibble(obj), out_csv)
    message("Wrote ", out_csv)
  }
})

# Figures
purrr::iwalk(figures, function(plt, name) {
  out_png <- file.path(paths$out_fig, paste0(name, ".png"))
  ggplot2::ggsave(
    filename = out_png,
    plot     = plt,
    width    = 9,
    height   = 6,
    dpi      = 150
  )
  message("Wrote ", out_png)
})

message("\nAll outputs written under: ", here::here("output"))
