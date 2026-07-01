# Simulated daily diary study: entertainment media use and wellbeing
#
# Design
# ------
# N = 100 persons, T = 14 days (1,400 rows total).
#
# Variables
# ---------
#   person        integer person ID
#   wellbeing     daily positive wellbeing (1-7)
#   screen_time   daily entertainment media use in minutes
#   stress        daily perceived stress (1-7)
#   enjoyment     daily enjoyment of media watched (1-7)
#   self_control  trait self-control, measured once (1-7, ICC ~ 1)
#
# Intended correlation structure
# ------------------------------
# Key divergence — screen_time x wellbeing:
#   within-person:   r ~ +.30  (more TV today -> mood boost, escapism works)
#   between-person:  r ~ -.35  (chronic heavy viewers have lower wellbeing)
#
# Mechanism: self_control is a common cause at the between-person level —
#   higher self-control -> higher wellbeing AND lower screen_time, inducing
#   a negative between-person association despite a positive within-person one.
#
# Within-person, stress is a common cause in the opposite direction:
#   more stress today -> more screen_time (escapism) AND lower wellbeing;
#   but screen_time has an additional direct positive effect on wellbeing,
#   making the net within-person correlation screen_time x wellbeing positive.
#
# Other correlations (same direction within and between, no divergence):
#   stress x wellbeing:    r ~ -.40 within, r ~ -.35 between
#   stress x screen_time:  r ~ +.28 within, r ~ +.35 between
#   enjoyment x wellbeing: r ~ +.35 within, r ~ +.25 between
#   enjoyment x screen_time: r ~ +.30 within, r ~ +.20 between
#
# self_control has ICC ~ 1 (between-person only); no within-person correlations.

set.seed(42)
n_persons <- 100
n_days    <- 14

# ---- Between-person level ------------------------------------------------

# Trait self-control (measured once per person, between-person only)
self_control_b <- rnorm(n_persons, mean = 4, sd = 0.8)

# Person means: self-control positively predicts wellbeing, negatively
# predicts screen_time and stress.
person_wellbeing   <- 4.5 + 0.45 * (self_control_b - 4) + rnorm(n_persons, 0, 0.50)
person_screen_time <- 130 - 20   * (self_control_b - 4) + rnorm(n_persons, 0, 20)
person_stress      <- 3.8 - 0.40 * (self_control_b - 4) + rnorm(n_persons, 0, 0.50)
person_enjoyment   <- 4.5 - 0.15 * (self_control_b - 4) + rnorm(n_persons, 0, 0.50)

# ---- Within-person level -------------------------------------------------

n_total <- n_persons * n_days

# Daily stress deviations
stress_w <- rnorm(n_total, 0, 0.75)

# Daily screen_time deviations: stressed days -> more screen_time (escapism)
screen_time_w <- 12 * stress_w + rnorm(n_total, 0, 30)

# Daily wellbeing deviations:
#   - stress hurts wellbeing (direct negative effect)
#   - extra screen_time helps wellbeing (direct positive escapism effect)
#   The escapism effect (0.012 per minute) is chosen to outweigh the
#   indirect negative path through stress, giving a net positive
#   within-person correlation between screen_time and wellbeing.
wellbeing_w <- -0.50 * stress_w + 0.012 * screen_time_w + rnorm(n_total, 0, 0.45)

# Daily enjoyment deviations: better wellbeing days and more screen_time
# both predict higher enjoyment
enjoyment_w <- 0.30 * wellbeing_w + 0.008 * screen_time_w + rnorm(n_total, 0, 0.45)

# ---- Assemble and clip to valid ranges -----------------------------------

media_diary <- dplyr::tibble(
  person       = rep(seq_len(n_persons), each = n_days),
  self_control = round(
    pmin(pmax(rep(self_control_b, each = n_days), 1), 7), 1
  ),
  wellbeing    = round(
    pmin(pmax(rep(person_wellbeing,   each = n_days) + wellbeing_w,   1), 7), 1
  ),
  screen_time  = pmax(
    round(rep(person_screen_time, each = n_days) + screen_time_w), 0
  ),
  stress       = round(
    pmin(pmax(rep(person_stress,      each = n_days) + stress_w,      1), 7), 1
  ),
  enjoyment    = round(
    pmin(pmax(rep(person_enjoyment,   each = n_days) + enjoyment_w,   1), 7), 1
  )
)

# ---- Save ----------------------------------------------------------------

save(media_diary, file = "data/media_diary.rda", compress = "bzip2")
message("Saved data/media_diary.rda (", nrow(media_diary), " rows)")
