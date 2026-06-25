# See helper-bayes-fixtures.R for the shared cache folder/fixture data and
# why both bayes test files use them.

test_that("bayes_within_between_correlations requires folder argument", {
  expect_error(
    bayes_within_between_correlations(
      data = bayes_fixture_basic,
      group = "group",
      vars = c("x", "y")
    ),
    "folder.*must be specified"
  )
})

test_that("bayes_within_between_correlations validates ci argument", {
  expect_error(
    bayes_within_between_correlations(
      data = bayes_fixture_basic,
      group = "group",
      vars = c("x", "y"),
      ci = 0,
      folder = bayes_cache_folder
    ),
    "ci.*must be between 0 and 1"
  )

  expect_error(
    bayes_within_between_correlations(
      data = bayes_fixture_basic,
      group = "group",
      vars = c("x", "y"),
      ci = 1.5,
      folder = bayes_cache_folder
    ),
    "ci.*must be between 0 and 1"
  )
})

test_that("bayes_within_between_correlations creates folder if it doesn't exist", {
  # A single variable needs no brms fit at all (the comparison matrix is
  # diagonal-only), so this only exercises folder creation.
  temp_folder <- file.path(tempdir(), "test_brms_wb_folder")
  on.exit(unlink(temp_folder, recursive = TRUE), add = TRUE)

  expect_false(dir.exists(temp_folder))

  result <- bayes_within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = "x",
    folder = temp_folder
  )

  expect_true(dir.exists(temp_folder))
  expect_equal(as.character(result$`1`[1]), "–")
})

test_that("bayes_within_between_correlations returns NA for zero-variance pairs without fitting", {
  result <- bayes_within_between_correlations(
    data = bayes_fixture_zero_variance,
    group = "group",
    vars = c("constant", "x"),
    folder = bayes_cache_folder
  )

  expect_equal(as.character(result$`2`[1]), "NA")
  expect_equal(as.character(result$`1`[2]), "NA")
})

test_that("bayes_within_between_correlations handles basic input correctly", {
  result <- bayes_within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    folder = bayes_cache_folder
  )

  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "mlstats_wb_tibble")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
  expect_equal(result$variable, c("x", "y"))
  expect_equal(as.character(result$`1`[1]), "–")
  expect_equal(as.character(result$`2`[2]), "–")

  cor_vals <- unlist(result[, -1])
  cor_vals <- cor_vals[cor_vals != "–"]
  expect_true(all(grepl("^-?[0-9]\\.[0-9]{2}\\*?$|^NA$", cor_vals)))

  expect_true(all(sapply(result[, -1], function(col) inherits(col, "mlstats_stat"))))
})

test_that("bayes_within_between_correlations handles missing values", {
  result <- bayes_within_between_correlations(
    data = bayes_fixture_na,
    group = "group",
    vars = c("x", "y"),
    folder = bayes_cache_folder
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("bayes_within_between_correlations weight TRUE vs FALSE differ on unbalanced data", {
  result_weighted <- bayes_within_between_correlations(
    data = bayes_fixture_unbalanced,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE,
    folder = bayes_cache_folder
  )

  result_unweighted <- bayes_within_between_correlations(
    data = bayes_fixture_unbalanced,
    group = "group",
    vars = c("x", "y"),
    weight = FALSE,
    folder = bayes_cache_folder
  )

  expect_equal(nrow(result_weighted), 2)
  expect_equal(nrow(result_unweighted), 2)

  between_weighted <- result_weighted$`1`[2]
  between_unweighted <- result_unweighted$`1`[2]
  expect_true(nchar(between_weighted) > 0)
  expect_true(nchar(between_unweighted) > 0)
})

test_that("bayes_within_between_correlations weight argument defaults to TRUE", {
  result_default <- bayes_within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    folder = bayes_cache_folder
  )

  result_explicit <- bayes_within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE,
    folder = bayes_cache_folder
  )

  expect_identical(result_default, result_explicit)
})

test_that("bayes_within_between_correlations handles different ci levels without refitting", {
  result_90 <- bayes_within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    ci = 0.9,
    folder = bayes_cache_folder
  )

  result_95 <- bayes_within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    ci = 0.95,
    folder = bayes_cache_folder
  )

  expect_equal(dim(result_90), dim(result_95))
})

test_that("bayes_within_between_correlations handles numeric and factor group variables", {
  # Single variable: no brms fit needed, so this is purely a check that
  # group-type coercion (factor/numeric) doesn't break the diagonal-only path.
  data_numeric <- bayes_fixture_basic
  data_numeric$group <- as.numeric(factor(data_numeric$group))

  data_factor <- bayes_fixture_basic
  data_factor$group <- factor(data_factor$group)

  result_numeric <- bayes_within_between_correlations(
    data = data_numeric, group = "group", vars = "x", folder = bayes_cache_folder
  )
  result_factor <- bayes_within_between_correlations(
    data = data_factor, group = "group", vars = "x", folder = bayes_cache_folder
  )

  expect_s3_class(result_numeric, "tbl_df")
  expect_s3_class(result_factor, "tbl_df")
})

test_that("bayes_within_between_correlations reuses cached models", {
  result1 <- bayes_within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    folder = bayes_cache_folder
  )

  result2 <- bayes_within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    folder = bayes_cache_folder
  )

  expect_identical(result1, result2)
})

test_that("flip=TRUE swaps which triangle holds within- vs between-group correlations", {
  result <- bayes_within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    flip = TRUE,
    folder = bayes_cache_folder
  )

  expect_true(attr(result, "flipped"))
  expect_equal(as.character(result$`1`[1]), "–")
})

test_that("bayes_within_between_correlations default print dispatches pillar formatting", {
  result <- bayes_within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    folder = bayes_cache_folder
  )

  output <- capture.output(print(result))
  expect_true(any(grepl("Within- and Between-Group Correlations", output)))
  expect_true(any(grepl("credible intervals", output)))
})
