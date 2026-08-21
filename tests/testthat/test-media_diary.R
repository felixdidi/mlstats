# The bundled media_diary example dataset (data-raw/media_diary.R) has a
# variable number of diary days per person (simulated non-response), rather
# than a fixed T. These tests pin the properties the package's own
# documentation, README, and vignettes rely on: the sign and significance of
# every documented within-/between-person correlation (see the "Intended
# correlation structure" comment at the top of data-raw/media_diary.R), and
# the trait-like nature of self_control. They intentionally do NOT pin exact
# coefficient values, or correlations that are not part of the documented
# structure (e.g., stress x enjoyment).

data("media_diary")

# Extracts the numeric correlation estimate from a formatted mlstats_stat
# cell (e.g. "-.34*" or "0.42*"), stripping the significance star.
extract_est <- function(x) {
  as.numeric(gsub("\\*", "", vctrs::vec_data(x)))
}

is_starred <- function(x) {
  grepl("\\*", vctrs::vec_data(x))
}

test_that("media_diary has a variable number of observations per person", {
  group_sizes <- table(media_diary$person)

  expect_equal(length(group_sizes), 100)
  expect_true(min(group_sizes) >= 5)
  expect_true(max(group_sizes) <= 14)
  # Genuinely variable, not accidentally balanced
  expect_true(min(group_sizes) < max(group_sizes))
})

test_that("self_control is a between-person-only trait (ICC ~ 1)", {
  result <- mldesc(media_diary, group = "person", vars = c("self_control", "wellbeing"))
  icc_self_control <- as.numeric(vctrs::vec_data(result$icc)[result$variable == "Self control"])
  expect_gt(icc_self_control, 0.95)
})

test_that("the documented screen_time x wellbeing divergence keeps its sign and significance", {
  result <- within_between_correlations(
    media_diary,
    group = "person",
    vars = c("wellbeing", "screen_time")
  )

  within_est <- extract_est(result$`2`[result$variable == "wellbeing"])
  between_est <- extract_est(result$`1`[result$variable == "screen_time"])

  expect_gt(within_est, 0)
  expect_true(is_starred(result$`2`[result$variable == "wellbeing"]))

  expect_lt(between_est, 0)
  expect_true(is_starred(result$`1`[result$variable == "screen_time"]))
})

test_that("self_control-driven between-person correlations keep their documented sign and significance", {
  result <- within_between_correlations(
    media_diary,
    group = "person",
    vars = c("self_control", "wellbeing", "screen_time", "stress")
  )

  # self_control is between-only, so these are all in the "between" (lower
  # triangle, i > j) position relative to self_control (column 1).
  sc_wellbeing <- result$`1`[result$variable == "wellbeing"]
  sc_screen_time <- result$`1`[result$variable == "screen_time"]
  sc_stress <- result$`1`[result$variable == "stress"]

  expect_gt(extract_est(sc_wellbeing), 0)
  expect_true(is_starred(sc_wellbeing))

  expect_lt(extract_est(sc_screen_time), 0)
  expect_true(is_starred(sc_screen_time))

  expect_lt(extract_est(sc_stress), 0)
  expect_true(is_starred(sc_stress))
})

test_that("wellbeing x stress and screen_time x stress keep their documented sign and significance", {
  result <- within_between_correlations(
    media_diary,
    group = "person",
    vars = c("wellbeing", "screen_time", "stress")
  )

  # wellbeing x stress: negative at both levels
  expect_lt(extract_est(result$`3`[result$variable == "wellbeing"]), 0)
  expect_true(is_starred(result$`3`[result$variable == "wellbeing"]))
  expect_lt(extract_est(result$`1`[result$variable == "stress"]), 0)
  expect_true(is_starred(result$`1`[result$variable == "stress"]))

  # screen_time x stress: positive at both levels
  expect_gt(extract_est(result$`3`[result$variable == "screen_time"]), 0)
  expect_true(is_starred(result$`3`[result$variable == "screen_time"]))
  expect_gt(extract_est(result$`2`[result$variable == "stress"]), 0)
  expect_true(is_starred(result$`2`[result$variable == "stress"]))
})

test_that("enjoyment x wellbeing and enjoyment x screen_time keep their documented sign and significance", {
  result <- within_between_correlations(
    media_diary,
    group = "person",
    vars = c("wellbeing", "screen_time", "enjoyment")
  )

  # enjoyment x wellbeing: positive at both levels
  expect_gt(extract_est(result$`3`[result$variable == "wellbeing"]), 0)
  expect_true(is_starred(result$`3`[result$variable == "wellbeing"]))
  expect_gt(extract_est(result$`1`[result$variable == "enjoyment"]), 0)
  expect_true(is_starred(result$`1`[result$variable == "enjoyment"]))

  # enjoyment x screen_time: positive at both levels
  expect_gt(extract_est(result$`3`[result$variable == "screen_time"]), 0)
  expect_true(is_starred(result$`3`[result$variable == "screen_time"]))
  expect_gt(extract_est(result$`2`[result$variable == "enjoyment"]), 0)
  expect_true(is_starred(result$`2`[result$variable == "enjoyment"]))
})
