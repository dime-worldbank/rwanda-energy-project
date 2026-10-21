# ==============================================================================
# 9.hfc_shs.R
# Midline HFC: SHS nudge funnel for T2 and T3, by arm and by village.
# Households in arm -> attempted -> located -> consented -> preload-eligible ->
# door-confirmed eligible -> shs_willing (yes / no / not sure) -> data consent.
# Counts are households (a household counts once); % is of the previous step.
#
# Sourced by 1.hfc_main.R after scripts 2-3 (uses hfc_constr, hh_frame).
# Outputs: shs_funnel, shs_funnel_village
#
# Author: Yeji (using Claude) · Last modified: 2026-10-21
# ==============================================================================


# ---- 1. Config / constants ---------------------------------------------------

FUNNEL_STEPS <- c("in_arm", "attempted", "located", "consented", "preload_eligible", "door_confirmed",
                  "willing_yes", "willing_no", "willing_notsure", "data_consent")
# Previous step for the % column (the three willingness outcomes split door_confirmed)
FUNNEL_PREV <- c(attempted = "in_arm", located = "attempted", consented = "located",
                 preload_eligible = "consented", door_confirmed = "preload_eligible",
                 willing_yes = "door_confirmed", willing_no = "door_confirmed",
                 willing_notsure = "door_confirmed", data_consent = "willing_yes")


# ---- 2. Checks: funnel ---------------------------------------------------------

# Household-level funnel status (door-confirmed = relevance of the nosolar group)
shs_hh <- hfc_constr |>
  summarise(located = any(located),
            consented = any(consented),
            preload_eligible = any(consented & preload_shs_eligible),
            door_confirmed = any(pull_shs_eligible %in% 1 & shs_confirm %in% 0 & electrified %in% 0),
            willing_yes = any(shs_willing %in% 1),
            willing_no = any(shs_willing %in% 0),
            willing_notsure = any(shs_willing %in% -88),
            data_consent = any(shs_data_consent %in% 1),
            .by = hh_id)

funnel_frame <- hh_frame |>
  filter(treat %in% SHS_ARMS) |>
  left_join(shs_hh, by = "hh_id") |>
  mutate(in_arm = TRUE, attempted = hh_id %in% hfc_constr$hh_id,
         across(c(located:data_consent), \(x) coalesce(x, FALSE)))

funnel_long <- function(by) {
  funnel_frame |>
    summarise(across(all_of(FUNNEL_STEPS), sum), .by = all_of(by)) |>
    pivot_longer(all_of(FUNNEL_STEPS), names_to = "step", values_to = "n") |>
    mutate(pct_of_prev = round(100 * n / n[match(FUNNEL_PREV[step], step)], 1), .by = all_of(by))
}

shs_funnel <- funnel_long("treat") |>
  arrange(treat, match(step, FUNNEL_STEPS))

shs_funnel_village <- funnel_long(c("treat", "district_key", "villageid_key", "village_key")) |>
  select(-pct_of_prev) |>
  pivot_wider(names_from = step, values_from = n) |>
  arrange(treat, district_key, village_key)


# ---- 3. Export ---------------------------------------------------------------

hfc_outputs$shs_funnel         <- shs_funnel
hfc_outputs$shs_funnel_village <- shs_funnel_village

funnel_n <- function(s) paste(shs_funnel$n[shs_funnel$step == s], collapse = "/")
message(sprintf("      funnel (%s): in arm %s, door-confirmed %s, willing %s",
                paste(unique(shs_funnel$treat), collapse = "/"),
                funnel_n("in_arm"), funnel_n("door_confirmed"), funnel_n("willing_yes")))
