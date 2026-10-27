# ==============================================================================
# 8.hfc_audio.R
# Disabled in the deidentified HFC pipeline. Audio paths and files are not used
# in the dashboard or checks because they are identifiable.
#
# This script is intentionally no-op to avoid copying audio files or sampling by
# household identity. The deidentified HFC pipeline uses hh_id only.
# ============================================================================== 


# Audio is identifiable and is excluded from deidentified HFC outputs.
stop("Audio audit sampling is disabled for deidentified HFCs.")
