# See helper-bayes-fixtures.R for the shared cache folder/fixture data and
# why both bayes test files use them. bayes_mldesc() calls
# bayes_within_between_correlations() internally with the same data/vars/
# folder it receives, so the correlation models fit in
# test-bayes_within_between_correlations.R are reused here rather than
# refit -- only the ICC models below are unique to this file.

test_that("bayes_mldesc requires folder argument", {
  expect_error(
    bayes_mldesc(
      data = bayes_fixture_basic,
      group = "group",
      vars = c("x", "y")
    ),
    "folder.*must be specified"
  )
})

test_that("bayes_mldesc validates ci argument", {
  expect_error(
    bayes_mldesc(
      data = bayes_fixture_basic,
      group = "group",
      vars = c("x", "y"),
      ci = 0,
      folder = bayes_cache_folder
    ),
    "ci.*must be between 0 and 1"
  )

  expect_error(
    bayes_mldesc(
      data = bayes_fixture_basic,
      group = "group",
      vars = c("x", "y"),
      ci = 1.5,
      folder = bayes_cache_folder
    ),
    "ci.*must be between 0 and 1"
  )
})

test_that("bayes_mldesc creates folder if it doesn't exist", {
  # Single variable: still fits one ICC model (ICC is always computed), but
  # no correlation models, keeping this cheap.
  temp_folder <- file.path(tempdir(), "test_brms_mldesc_folder")
  on.exit(unlink(temp_folder, recursive = TRUE), add = TRUE)

  expect_false(dir.exists(temp_folder))

  result <- bayes_mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = "x",
    folder = temp_folder
  )

  expect_true(dir.exists(temp_folder))
})

test_that("bayes_mldesc handles basic input correctly", {
  result <- bayes_mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    folder = bayes_cache_folder
  )

  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "mlstats_desc_tibble")
  expect_equal(nrow(result), 2)
  expect_equal(result$variable, c("X", "Y"))
  expect_equal(colnames(result), c("variable", "n_obs", "m", "sd", "range", "1", "2", "icc"))

  expect_match(result$n_obs, "^[0-9]+$")
  expect_match(result$m, "^[0-9]+\\.[0-9]{2}$")
  expect_match(result$sd, "^[0-9]+\\.[0-9]{2}$")
  expect_match(result$range, "^-?[0-9]+–-?[0-9]+$")
  expect_match(result$icc, "^\\.[0-9]{2}$")
  expect_equal(as.character(result$`1`[1]), "–")

  expect_true(all(sapply(result[, -1], function(col) inherits(col, "mlstats_stat"))))

  expect_true(attr(result, "bayesian"))
  expect_true(!is.null(attr(result, "table_title")))
  expect_true(!is.null(attr(result, "correlation_note")))
})

test_that("bayes_mldesc handles single variable", {
  result <- bayes_mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = "x",
    folder = bayes_cache_folder
  )

  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 7)
  expect_equal(as.character(result$`1`[1]), "–")
})

test_that("bayes_mldesc handles missing values", {
  result <- bayes_mldesc(
    data = bayes_fixture_na,
    group = "group",
    vars = c("x", "y"),
    folder = bayes_cache_folder
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("bayes_mldesc remove_leading_zero parameter works", {
  result_with_removal <- bayes_mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    remove_leading_zero = TRUE,
    folder = bayes_cache_folder
  )

  result_without_removal <- bayes_mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    remove_leading_zero = FALSE,
    folder = bayes_cache_folder
  )

  expect_true(any(grepl("^\\.", result_with_removal$icc)))
  expect_true(any(grepl("^0\\.", result_without_removal$icc)))
})

test_that("bayes_mldesc weight=FALSE calculates mean and SD of group means", {
  result_weighted <- bayes_mldesc(
    data = bayes_fixture_unbalanced,
    group = "group",
    vars = "x",
    weight = TRUE,
    folder = bayes_cache_folder
  )

  result_unweighted <- bayes_mldesc(
    data = bayes_fixture_unbalanced,
    group = "group",
    vars = "x",
    weight = FALSE,
    folder = bayes_cache_folder
  )

  group_means <- tapply(bayes_fixture_unbalanced$x, bayes_fixture_unbalanced$group, mean)

  expect_equal(as.character(result_weighted$m), sprintf("%.2f", mean(bayes_fixture_unbalanced$x)))
  expect_equal(as.character(result_unweighted$m), sprintf("%.2f", mean(group_means)))
  expect_equal(as.character(result_weighted$sd), sprintf("%.2f", sd(bayes_fixture_unbalanced$x)))
  expect_equal(as.character(result_unweighted$sd), sprintf("%.2f", sd(group_means)))

  # n_obs, range, and ICC don't depend on weighting
  expect_equal(as.character(result_weighted$n_obs), as.character(result_unweighted$n_obs))
  expect_equal(as.character(result_weighted$icc), as.character(result_unweighted$icc))
})

test_that("bayes_mldesc weight argument defaults to TRUE", {
  result_default <- bayes_mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    folder = bayes_cache_folder
  )

  result_explicit <- bayes_mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE,
    folder = bayes_cache_folder
  )

  expect_identical(result_default, result_explicit)
})

test_that("bayes_mldesc handles different ci levels without refitting", {
  result_90 <- bayes_mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    ci = 0.9,
    folder = bayes_cache_folder
  )

  result_95 <- bayes_mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    ci = 0.95,
    folder = bayes_cache_folder
  )

  expect_equal(dim(result_90), dim(result_95))
})

test_that("bayes_mldesc reuses cached models", {
  result1 <- bayes_mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    folder = bayes_cache_folder
  )

  result2 <- bayes_mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    folder = bayes_cache_folder
  )

  expect_identical(result1, result2)
})

test_that("bayes_mldesc print method supports gt, tt, and default formats", {
  result <- bayes_mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    folder = bayes_cache_folder
  )

  result_gt <- print(result, "gt")
  expect_s3_class(result_gt, "gt_tbl")

  result_tt <- print(result, "tt")
  expect_s4_class(result_tt, "tinytable")

  output <- capture.output(print(result))
  expect_true(any(grepl("Multilevel Descriptive Statistics", output)))
  expect_true(any(grepl("credible intervals", output)))
})

test_that("bayes_mldesc print method accepts custom title and notes", {
  result <- bayes_mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    folder = bayes_cache_folder
  )

  result_gt <- print(
    result,
    "gt",
    table_title = "Custom Bayesian Descriptive Statistics Table",
    correlation_note = "Custom correlation interpretation",
    note_text = "Custom footer note"
  )
  expect_s3_class(result_gt, "gt_tbl")
})

test_that("bayes_mldesc flip = TRUE forwards correctly", {
  result_normal <- bayes_mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    folder = bayes_cache_folder,
    flip = FALSE
  )

  result_flipped <- bayes_mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    folder = bayes_cache_folder,
    flip = TRUE
  )

  expect_false(attr(result_normal, "flipped"))
  expect_true(attr(result_flipped, "flipped"))

  # Upper triangle of normal = lower triangle of flipped
  expect_equal(
    vctrs::vec_data(result_normal$`2`)[1],
    vctrs::vec_data(result_flipped$`1`)[2]
  )

  # Descriptive columns and ICC should be identical
  expect_equal(
    vctrs::vec_data(result_normal$n_obs),
    vctrs::vec_data(result_flipped$n_obs)
  )
  expect_equal(
    vctrs::vec_data(result_normal$icc),
    vctrs::vec_data(result_flipped$icc)
  )
})
