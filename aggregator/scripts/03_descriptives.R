# 03_descriptives.R -------------------------------------------------------
# Build descriptive tables and figures from the cleaned waves. Results are
# accumulated in two lists (`tables`, `figures`) and saved in 04_export.R.

tables  <- list()
figures <- list()

# -- Helper: turn a per-wave list into a long tibble with one row per
# wave/variable/statistic, for tidy CSV export.
summarise_numeric <- function(df, var) {
  x <- df[[var]]
  tibble::tibble(
    variable = var,
    n_nonmiss = sum(!is.na(x)),
    n_missing = sum(is.na(x)),
    mean   = mean(x, na.rm = TRUE),
    sd     = sd(x, na.rm = TRUE),
    min    = suppressWarnings(min(x, na.rm = TRUE)),
    p25    = stats::quantile(x, 0.25, na.rm = TRUE),
    median = stats::median(x, na.rm = TRUE),
    p75    = stats::quantile(x, 0.75, na.rm = TRUE),
    max    = suppressWarnings(max(x, na.rm = TRUE))
  )
}

# 1. Fieldwork ------------------------------------------------------------
fieldwork <- purrr::imap_dfr(waves, function(df, w) {
  tibble::tibble(
    wave              = w,
    n_submitted       = nrow(df),
    n_consented       = sum(df$consent == 1, na.rm = TRUE),
    consent_rate      = mean(df$consent == 1, na.rm = TRUE),
    date_min          = suppressWarnings(min(df$date, na.rm = TRUE)),
    date_max          = suppressWarnings(max(df$date, na.rm = TRUE)),
    date_median       = suppressWarnings(stats::median(df$date, na.rm = TRUE)),
    median_duration_min = stats::median(df$duration, na.rm = TRUE),
    n_enumerators     = dplyr::n_distinct(df$enumerator),
    n_supervisors     = dplyr::n_distinct(df$supervisor),
    n_provinces       = dplyr::n_distinct(df$province_id),
    n_districts       = dplyr::n_distinct(df$district_id),
    n_sectors         = dplyr::n_distinct(df$sector_id),
    n_cells           = dplyr::n_distinct(df$cell_id),
    n_villages        = dplyr::n_distinct(df$village_id),
    n_markets         = dplyr::n_distinct(df$market_id),
    n_unique_aggregators = dplyr::n_distinct(df$aggregator_id)
  )
})
tables$`01_fieldwork` <- fieldwork

# Submissions per enumerator (one tibble; wave as a column)
enumerator_load <- purrr::imap_dfr(waves, function(df, w) {
  df %>%
    dplyr::group_by(enumerator) %>%
    dplyr::summarise(
      wave = w,
      n_submissions = dplyr::n(),
      median_duration_min = stats::median(duration, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(n_submissions))
})
tables$`01b_enumerator_load` <- enumerator_load

# Panel attrition: aggregator_id overlap across the three waves
ids <- purrr::map(waves, ~ unique(.x$aggregator_id))
panel_overlap <- tibble::tribble(
  ~set,                          ~n,
  "baseline only",               length(setdiff(ids$baseline, union(ids$midline, ids$endline))),
  "midline only",                length(setdiff(ids$midline,  union(ids$baseline, ids$endline))),
  "endline only",                length(setdiff(ids$endline,  union(ids$baseline, ids$midline))),
  "baseline & midline",          length(intersect(ids$baseline, ids$midline)),
  "baseline & endline",          length(intersect(ids$baseline, ids$endline)),
  "midline & endline",           length(intersect(ids$midline,  ids$endline)),
  "all three waves",             length(Reduce(intersect, ids)),
  "any wave (union)",            length(Reduce(union, ids))
)
tables$`01c_panel_overlap` <- panel_overlap

# 2. Aggregator characteristics -------------------------------------------
agg_chars <- purrr::imap_dfr(waves, function(df, w) {
  own_tab <- df %>%
    dplyr::count(aggown_rent) %>%
    dplyr::mutate(share = n / sum(n)) %>%
    dplyr::transmute(
      wave      = w,
      variable  = "aggown_rent",
      category  = ifelse(is.na(aggown_rent) | aggown_rent == "", "(missing)", aggown_rent),
      n         = n,
      share     = share
    )
  size_tab <- summarise_numeric(df, "agg_size") %>%
    dplyr::mutate(wave = w, category = NA_character_) %>%
    dplyr::select(wave, variable, category, n = n_nonmiss,
                  share = mean, sd, median, min, max)
  sv_tab <- df %>%
    dplyr::summarise(
      n      = sum(!is.na(sampled_village)),
      share  = mean(sampled_village == 1, na.rm = TRUE)
    ) %>%
    dplyr::mutate(wave = w, variable = "sampled_village (=1 in sampled vlg)",
                  category = "share == 1") %>%
    dplyr::select(wave, variable, category, n, share)
  dplyr::bind_rows(own_tab, sv_tab, size_tab)
})
tables$`02_aggregator_chars` <- agg_chars

# 3. Crop portfolio -------------------------------------------------------
# Each wave has a block of `agg_crops_N` 0/1 columns (N = 1..44, plus 0
# and 77 as special codes). Endline also has agg_crops repeated; midline
# carries bought_crops_*; endline has bought_crops_23B_* and _23C_*.
# We summarise every such block we can find.

crop_blocks <- list(
  list(wave = "baseline", prefix = "agg_crops_",         label = "agg_crops"),
  list(wave = "midline",  prefix = "agg_crops_",         label = "agg_crops"),
  list(wave = "midline",  prefix = "bought_crops_",      label = "bought_crops_23A"),
  list(wave = "endline",  prefix = "agg_crops_",         label = "agg_crops"),
  list(wave = "endline",  prefix = "bought_crops_",      label = "bought_crops_23B"),
  list(wave = "endline",  prefix = "bought_crops_23C_",  label = "bought_crops_23C")
)

crop_code_cols <- function(df, prefix) {
  # Columns named <prefix><N> where N is a digit string (excludes labels
  # like agg_crops_other or *__77 which carry "_" before the code).
  pat <- paste0("^", prefix, "[0-9]+$")
  grep(pat, names(df), value = TRUE)
}

crop_summaries <- purrr::map_dfr(crop_blocks, function(spec) {
  df <- waves[[spec$wave]]
  cols <- crop_code_cols(df, spec$prefix)
  if (length(cols) == 0) return(NULL)
  mat <- as.matrix(dplyr::mutate(dplyr::select(df, dplyr::all_of(cols)),
                                 dplyr::across(dplyr::everything(), to_num)))
  any_reported <- rowSums(!is.na(mat) & mat == 1) > 0
  n_crops_per_agg <- rowSums(mat == 1, na.rm = TRUE)
  per_crop_share <- colMeans(mat == 1, na.rm = TRUE)

  share_tbl <- tibble::tibble(
    wave        = spec$wave,
    block       = spec$label,
    crop_code   = sub(paste0("^", spec$prefix), "", cols),
    share_aggs  = per_crop_share,
    n_aggs_yes  = colSums(mat == 1, na.rm = TRUE)
  ) %>% dplyr::arrange(dplyr::desc(share_aggs))

  summary_tbl <- tibble::tibble(
    wave  = spec$wave,
    block = spec$label,
    crop_code = "(summary)",
    share_aggs = mean(any_reported, na.rm = TRUE),
    n_aggs_yes = sum(any_reported, na.rm = TRUE),
    mean_n_crops = mean(n_crops_per_agg, na.rm = TRUE),
    median_n_crops = stats::median(n_crops_per_agg, na.rm = TRUE)
  )

  dplyr::bind_rows(summary_tbl, share_tbl)
})
tables$`03_crops` <- crop_summaries

# Top-15 crops figure (agg_crops block only, comparable across waves)
top_crops_plot_data <- crop_summaries %>%
  dplyr::filter(block == "agg_crops", crop_code != "(summary)") %>%
  dplyr::group_by(wave) %>%
  dplyr::slice_max(share_aggs, n = 15, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    wave = factor(wave, levels = c("baseline", "midline", "endline")),
    crop_code = stats::reorder(crop_code, share_aggs)
  )

figures$top_crops_by_wave <- ggplot2::ggplot(
  top_crops_plot_data,
  ggplot2::aes(x = crop_code, y = share_aggs)
) +
  ggplot2::geom_col(fill = "steelblue") +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ wave, scales = "free_y") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(
    title = "Top 15 crops handled by aggregators, by wave",
    subtitle = "Share of aggregators reporting each crop (agg_crops_* block)",
    x = "Crop code (see survey codebook)",
    y = "Share of aggregators"
  ) +
  ggplot2::theme_minimal(base_size = 11)

# 4. Purchase activity (Midline + Endline) --------------------------------
purchase <- dplyr::bind_rows(
  tibble::tibble(
    wave   = "midline",
    period = "23A",
    n      = sum(!is.na(to_num(waves$midline$buy_23A))),
    share_yes = mean(to_num(waves$midline$buy_23A) == 1, na.rm = TRUE),
    crop_bought_count_mean   = mean(to_num(waves$midline$crop_bought_count), na.rm = TRUE),
    crop_bought_count_median = stats::median(to_num(waves$midline$crop_bought_count), na.rm = TRUE)
  ),
  tibble::tibble(
    wave   = "endline",
    period = "23B",
    n      = sum(!is.na(to_num(waves$endline$buy_23B))),
    share_yes = mean(to_num(waves$endline$buy_23B) == 1, na.rm = TRUE),
    crop_bought_count_mean   = mean(to_num(waves$endline$crop_bought_count), na.rm = TRUE),
    crop_bought_count_median = stats::median(to_num(waves$endline$crop_bought_count), na.rm = TRUE)
  ),
  tibble::tibble(
    wave   = "endline",
    period = "23C",
    n      = sum(!is.na(to_num(waves$endline$buy_23C))),
    share_yes = mean(to_num(waves$endline$buy_23C) == 1, na.rm = TRUE),
    crop_bought_count_mean   = mean(to_num(waves$endline$`crop_bought_23C_count`), na.rm = TRUE),
    crop_bought_count_median = stats::median(to_num(waves$endline$`crop_bought_23C_count`), na.rm = TRUE)
  )
)
tables$`04_purchase` <- purchase

# 5. Transport ------------------------------------------------------------
transport <- purrr::imap_dfr(waves, function(df, w) {
  pieces <- list()

  # Numeric summaries common to all waves
  for (v in c("num_trips", "last_trips_count", "number_trucker")) {
    if (v %in% names(df)) {
      pieces[[v]] <- summarise_numeric(df, v) %>%
        dplyr::mutate(wave = w)
    }
  }

  # pay_transp may be numeric (Baseline) or categorical (Midline/Endline)
  if ("pay_transp" %in% names(df)) {
    pt_num <- to_num(df$pay_transp)
    pt <- if (any(!is.na(pt_num))) {
      tibble::tibble(
        variable = "pay_transp",
        n_nonmiss = sum(!is.na(pt_num)),
        n_missing = sum(is.na(pt_num)),
        mean = mean(pt_num, na.rm = TRUE),
        sd   = sd(pt_num, na.rm = TRUE),
        min  = suppressWarnings(min(pt_num, na.rm = TRUE)),
        p25  = stats::quantile(pt_num, 0.25, na.rm = TRUE),
        median = stats::median(pt_num, na.rm = TRUE),
        p75  = stats::quantile(pt_num, 0.75, na.rm = TRUE),
        max  = suppressWarnings(max(pt_num, na.rm = TRUE))
      )
    } else NULL
    if (!is.null(pt)) pieces$pay_transp <- pt %>% dplyr::mutate(wave = w)
  }

  dplyr::bind_rows(pieces)
}) %>%
  dplyr::select(wave, dplyr::everything())
tables$`05_transport_numeric` <- transport

# Transport categorical: transp_means (Baseline), transport / transport_offer
# / service_provider (Midline+)
transport_cat <- purrr::imap_dfr(waves, function(df, w) {
  cat_vars <- intersect(
    c("transp_means", "transport_offer", "transport", "service_provider",
      "last_trip_any_truck"),
    names(df)
  )
  purrr::map_dfr(cat_vars, function(v) {
    df %>%
      dplyr::count(.data[[v]]) %>%
      dplyr::mutate(
        wave     = w,
        variable = v,
        category = ifelse(is.na(.data[[v]]) | .data[[v]] == "", "(missing)",
                          as.character(.data[[v]])),
        share    = n / sum(n)
      ) %>%
      dplyr::select(wave, variable, category, n, share)
  })
})
tables$`05b_transport_categorical` <- transport_cat

# Trips distribution figure
trips_plot_data <- waves_long %>%
  dplyr::filter(!is.na(num_trips))

figures$trips_distribution <- ggplot2::ggplot(
  trips_plot_data,
  ggplot2::aes(x = wave, y = num_trips, fill = wave)
) +
  ggplot2::geom_boxplot(outlier.alpha = 0.3) +
  ggplot2::scale_y_continuous(limits = c(0, stats::quantile(trips_plot_data$num_trips, 0.99, na.rm = TRUE))) +
  ggplot2::labs(
    title = "Number of trips reported per aggregator, by wave",
    subtitle = "Boxplot truncated at the 99th percentile to remove extreme outliers",
    x = NULL,
    y = "Number of trips"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "none")

# 6. Cross-wave gtsummary -------------------------------------------------
# Variables that exist in all three waves and are meaningful to compare.
cross_vars <- intersect(
  c("aggown_rent", "agg_size", "sampled_village",
    "num_trips", "last_trips_count", "number_trucker"),
  common_cols
)

cross_tbl_data <- waves_long %>%
  dplyr::select(wave, dplyr::all_of(cross_vars))

cross_tbl <- gtsummary::tbl_summary(
  cross_tbl_data,
  by = wave,
  missing = "ifany",
  statistic = list(
    gtsummary::all_continuous()  ~ "{median} ({p25}, {p75})",
    gtsummary::all_categorical() ~ "{n} ({p}%)"
  )
) %>%
  gtsummary::add_n() %>%
  gtsummary::add_p() %>%
  gtsummary::modify_header(label = "**Variable**") %>%
  gtsummary::modify_caption("Aggregator characteristics across survey waves")

tables$`06_cross_wave` <- cross_tbl
