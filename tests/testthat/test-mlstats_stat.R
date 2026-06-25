# Direct tests for the mlstats_stat vctrs coercion methods and for the
# pillar/print methods on mlstats_wb_tibble, which are only exercised when a
# result is actually printed in the default (non-gt/tt) format.

test_that("as.numeric/as.double/as.integer strip stars and commas", {
  x <- vctrs::new_vctr(c("0.45*", "1,234", "–", "NA"), class = "mlstats_stat")

  expect_equal(as.numeric(x), c(0.45, 1234, NA, NA))
  expect_equal(as.double(x), c(0.45, 1234, NA, NA))
  expect_equal(as.integer(x), c(0L, 1234L, NA, NA))
})

test_that("as.character returns the raw underlying strings", {
  x <- vctrs::new_vctr(c("0.45*", "–"), class = "mlstats_stat")
  expect_equal(as.character(x), c("0.45*", "–"))
})

test_that("vec_ptype_abbr labels mlstats_stat columns as 'mls'", {
  x <- vctrs::new_vctr("0.45", class = "mlstats_stat")
  expect_equal(vctrs::vec_ptype_abbr(x), "mls")
})

test_that("within_between_correlations default print dispatches pillar methods", {
  set.seed(1)
  data <- data.frame(
    g = rep(1:5, each = 10),
    x = rnorm(50),
    y = rnorm(50)
  )
  result <- within_between_correlations(data, "g", c("x", "y"))

  output <- capture.output(print(result))
  expect_true(any(grepl("Within- and Between-Group Correlations", output)))
  expect_true(any(grepl("Within-group correlations above", output)))
  expect_true(any(grepl("variance decomposition", output)))

  result_flipped <- within_between_correlations(data, "g", c("x", "y"), flip = TRUE)
  output_flipped <- capture.output(print(result_flipped))
  expect_true(any(grepl("Between-group correlations above", output_flipped)))
})

test_that("within_between_correlations sem method note appears in default print", {
  set.seed(2)
  data <- data.frame(
    g = rep(1:10, each = 10),
    x = rnorm(100),
    y = rnorm(100)
  )
  result <- suppressWarnings(within_between_correlations(data, "g", c("x", "y"), method = "sem"))

  output <- capture.output(print(result))
  expect_true(any(grepl("two-level SEM", output)))
})
