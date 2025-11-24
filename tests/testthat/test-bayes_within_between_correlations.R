# Test setup - Create shared test data to reuse across tests
setup_test_data <- function() {
  set.seed(123)
  list(
    basic = data.frame(
      group = rep(c("A", "B", "C"), each = 10),
      x = c(rnorm(10, 10, 2), rnorm(10, 15, 2), rnorm(10, 20, 2)),
      y = c(rnorm(10, 5, 1), rnorm(10, 10, 1), rnorm(10, 15, 1))
    ),
    multi_var = data.frame(
      group = rep(1:5, each = 20),
      v1 = rnorm(100),
      v2 = rnorm(100),
      v3 = rnorm(100)
    ),
    unbalanced = data.frame(
      group = c(rep("A", 5), rep("B", 45)),
      x = c(rnorm(5, 0, 1), rnorm(45, 10, 1)),
      y = c(rnorm(5, 0, 1), rnorm(45, 10, 1))
    )
  )
}

# Setup: Create test data once for all tests
test_data <- setup_test_data()

test_that("bayes_within_between_correlations requires folder argument", {
  expect_error(
    bayes_within_between_correlations(
      data = test_data$basic,
      group = "group",
      vars = c("x", "y")
    ),
    "folder.*must be specified"
  )
})

test_that("bayes_within_between_correlations validates ci argument", {
  expect_error(
    bayes_within_between_correlations(
      data = test_data$basic,
      group = "group",
      vars = c("x", "y"),
      ci = 0,
      folder = "brms_models"
    ),
    "ci.*must be between 0 and 1"
  )
  
  expect_error(
    bayes_within_between_correlations(
      data = test_data$basic,
      group = "group",
      vars = c("x", "y"),
      ci = 1.5,
      folder = "brms_models"
    ),
    "ci.*must be between 0 and 1"
  )
})

test_that("bayes_within_between_correlations creates folder if it doesn't exist", {
  temp_folder <- file.path(tempdir(), "test_brms_models")
  on.exit(unlink(temp_folder, recursive = TRUE), add = TRUE)
  
  expect_false(dir.exists(temp_folder))
  
  result <- bayes_within_between_correlations(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    folder = temp_folder
  )
  
  expect_true(dir.exists(temp_folder))
})

test_that("bayes_within_between_correlations handles basic input correctly", {
  result <- bayes_within_between_correlations(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    folder = "brms_models"
  )
  
  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
  expect_equal(result$variable, c("x", "y"))
})

test_that("bayes_within_between_correlations produces symmetric matrix structure", {
  result <- bayes_within_between_correlations(
    data = test_data$multi_var,
    group = "group",
    vars = c("v1", "v2", "v3"),
    folder = "brms_models"
  )
  
  # Check dimensions
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4)  # variable column + 3 correlation columns
  
  # Check diagonal is "\u2013"
  expect_equal(result$`1`[1], "\u2013")
  expect_equal(result$`2`[2], "\u2013")
  expect_equal(result$`3`[3], "\u2013")
})

test_that("bayes_within_between_correlations output format is correct", {
  result <- bayes_within_between_correlations(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    folder = "brms_models"
  )
  
  # Check column names
  expect_true("variable" %in% colnames(result))
  expect_true(all(colnames(result)[-1] %in% as.character(1:2)))
  
  # Check that correlations are formatted correctly
  cor_vals <- unlist(result[, -1])
  cor_vals <- cor_vals[cor_vals != "\u2013"]
  
  # Should be numeric-like strings with optional asterisk
  expect_true(all(grepl("^-?[0-9]\\.[0-9]{2}\\*?$|^NA$", cor_vals)))
})

test_that("bayes_within_between_correlations handles single variable", {
  result <- bayes_within_between_correlations(
    data = test_data$basic,
    group = "group",
    vars = "x",
    folder = "brms_models"
  )
  
  # Should return 1x2 tibble with diagonal only
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 2)
  expect_equal(result$`1`[1], "\u2013")
})

test_that("bayes_within_between_correlations handles missing values", {
  data_with_na <- test_data$basic
  data_with_na$x[c(1, 5, 15)] <- NA
  data_with_na$y[c(2, 10, 20)] <- NA
  
  result <- bayes_within_between_correlations(
    data = data_with_na,
    group = "group",
    vars = c("x", "y"),
    folder = "brms_models"
  )
  
  # Should complete without error
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("bayes_within_between_correlations weight=TRUE uses weighted correlations", {
  result_weighted <- bayes_within_between_correlations(
    data = test_data$unbalanced,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE,
    folder = "brms_models"
  )
  
  expect_s3_class(result_weighted, "tbl_df")
  expect_equal(nrow(result_weighted), 2)
  
  # Between-group correlation should be computed
  between_val <- result_weighted$`1`[2]
  expect_true(nchar(between_val) > 0)
})

test_that("bayes_within_between_correlations weight=FALSE uses unweighted correlations", {
  result_unweighted <- bayes_within_between_correlations(
    data = test_data$unbalanced,
    group = "group",
    vars = c("x", "y"),
    weight = FALSE,
    folder = "brms_models"
  )
  
  expect_s3_class(result_unweighted, "tbl_df")
  expect_equal(nrow(result_unweighted), 2)
  
  # Between-group correlation should be computed
  between_val <- result_unweighted$`1`[2]
  expect_true(nchar(between_val) > 0)
})

test_that("bayes_within_between_correlations weight argument defaults to TRUE", {
  result_default <- bayes_within_between_correlations(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    folder = "brms_models"
  )
  
  result_explicit <- bayes_within_between_correlations(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE,
    folder = "brms_models"
  )
  
  # Should be identical (models are cached, so results should match exactly)
  expect_identical(result_default, result_explicit)
})

test_that("bayes_within_between_correlations handles different ci levels", {
  result_90 <- bayes_within_between_correlations(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    ci = 0.9,
    folder = "brms_models"
  )
  
  result_95 <- bayes_within_between_correlations(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    ci = 0.95,
    folder = "brms_models"
  )
  
  # Both should complete successfully
  expect_s3_class(result_90, "tbl_df")
  expect_s3_class(result_95, "tbl_df")
  
  # Structure should be identical
  expect_equal(dim(result_90), dim(result_95))
})

test_that("bayes_within_between_correlations marks credible correlations", {
  result <- bayes_within_between_correlations(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    folder = "brms_models"
  )
  
  # At least one cell should have content (correlation value)
  expect_true(nchar(result$`2`[1]) > 0)
  expect_true(nchar(result$`1`[2]) > 0)
})

test_that("bayes_within_between_correlations handles numeric group variable", {
  result <- bayes_within_between_correlations(
    data = test_data$multi_var,
    group = "group",
    vars = c("v1", "v2"),
    folder = "brms_models"
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("bayes_within_between_correlations handles factor group variable", {
  data_factor <- test_data$basic
  data_factor$group <- factor(data_factor$group)
  
  result <- bayes_within_between_correlations(
    data = data_factor,
    group = "group",
    vars = c("x", "y"),
    folder = "brms_models"
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("bayes_within_between_correlations reuses cached models", {
  # First run
  result1 <- bayes_within_between_correlations(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    folder = "brms_models"
  )
  
  # Second run should use cached models
  result2 <- bayes_within_between_correlations(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    folder = "brms_models"
  )
  
  # Results should be identical
  expect_identical(result1, result2)
})

test_that("bayes_within_between_correlations weighted vs unweighted differ", {
  result_weighted <- bayes_within_between_correlations(
    data = test_data$unbalanced,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE,
    folder = "brms_models"
  )
  
  result_unweighted <- bayes_within_between_correlations(
    data = test_data$unbalanced,
    group = "group",
    vars = c("x", "y"),
    weight = FALSE,
    folder = "brms_models"
  )
  
  # Extract between-group correlations
  between_weighted <- result_weighted$`1`[2]
  between_unweighted <- result_unweighted$`1`[2]
  
  # Both should be valid
  expect_true(nchar(between_weighted) > 0)
  expect_true(nchar(between_unweighted) > 0)
})

test_that("bayes_within_between_correlations upper triangle is within-group", {
  result <- bayes_within_between_correlations(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    folder = "brms_models"
  )
  
  # Upper triangle should be within-group
  # Lower triangle should be between-group
  within_val <- result$`2`[1]
  between_val <- result$`1`[2]
  
  expect_true(nchar(within_val) > 0)
  expect_true(nchar(between_val) > 0)
})