# ==============================================================================
# 8.hfc_audio.R
# Midline HFC: daily audio audit sample. Draws up to AUDIO_SAMPLE_N consented
# submissions from the audit day that have an audio file, excluding households
# audited on earlier days, and copies their audio files into a dated folder for
# listening. (No backcheck in midline.)
#
# Audit day: live = yesterday (Sys.Date() - 1); test = last fieldwork day in
# the synthetic data (static dates).
#
# Sourced by 1.hfc_main.R after 2.hfc_construct.R (uses hfc_constr, media_dir, audio_dir).
# Outputs: audio_sample (+ audio_dir/<YYYYMMDD>/audiocheck_list.xlsx and audio files)
#
# Author: Yeji (using Claude) · Last modified: 2026-10-21
# ==============================================================================


# ---- 1. Config / constants ---------------------------------------------------

# Bug 6 fixed: audit day relative to the run date, not hard-coded
AUDIO_DATE <- if (HFC_MODE == "live") Sys.Date() - 1 else max(hfc_constr$startdate)
audio_day_dir <- file.path(audio_dir, format(AUDIO_DATE, "%Y%m%d"))


# ---- 2. Data loading ---------------------------------------------------------

# Households audited on earlier days (lists from previous runs)
earlier_lists <- list.files(audio_dir, pattern = "^audiocheck_list\\.xlsx$", recursive = TRUE, full.names = TRUE)
earlier_lists <- earlier_lists[basename(dirname(earlier_lists)) < format(AUDIO_DATE, "%Y%m%d")]
already_audited <- map(earlier_lists, \(f) read_excel(f, col_types = "text")$hh_id) |> unlist() |> unique()


# ---- 3. Sample -------------------------------------------------------------------

set.seed(as.integer(format(AUDIO_DATE, "%Y%m%d")))   # same day -> same sample
audio_sample <- hfc_constr |>
  filter(consented, submissiondate == AUDIO_DATE, !is.na(audio), audio != "",
         !hh_id %in% already_audited) |>
  slice_sample(n = AUDIO_SAMPLE_N) |>
  transmute(KEY, hh_id, submissiondate, enumerator, enumerator_key, district_key, sector_key,
            cell_key, village_key, treat, survey_min = round(survey_min, 1), audio,
            audio_file_found = file.exists(file.path(media_dir, basename(audio)))) |>
  arrange(enumerator, hh_id)


# ---- 4. Export ---------------------------------------------------------------

dir.create(audio_day_dir, recursive = TRUE, showWarnings = FALSE)
copied <- file.copy(file.path(media_dir, basename(audio_sample$audio[audio_sample$audio_file_found])),
                    audio_day_dir, overwrite = TRUE)
write_xlsx(audio_sample, file.path(audio_day_dir, "audiocheck_list.xlsx"))

hfc_outputs$audio_sample <- audio_sample

message(sprintf("      audit day %s: %d submissions sampled (%d excluded as already audited), %d audio files copied",
                AUDIO_DATE, nrow(audio_sample), length(already_audited), sum(copied)))
