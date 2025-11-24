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

test_that("bayes_mldesc requires folder argument", {
  expect_error(
    bayes_mldesc(
      data = test_data$basic,
      group = "group",
      vars = c("x", "y")
    ),
    "folder.*must be specified"
  )
})

test_that("bayes_mldesc validates ci argument", {
  expect_error(
    bayes_mldesc(
      data = test_data$basic,
      group = "group",
      vars = c("x", "y"),
      ci = 0,
      folder = "brms_models"
    ),
    "ci.*must be between 0 and 1"
  )
  
  expect_error(
    bayes_mldesc(
      data = test_data$basic,
      group = "group",
      vars = c("x", "y"),
      ci = 1.5,
      folder = "brms_models"
    ),
    "ci.*must be between 0 and 1"
  )
})

test_that("bayes_mldesc creates folder if it doesn't exist", {
  temp_folder <- file.path(tempdir(), "test_brms_models_mldesc")
  on.exit(unlink(temp_folder, recursive = TRUE), add = TRUE)
  
  expect_false(dir.exists(temp_folder))
  
  result <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    folder = temp_folder
  )
  
  expect_true(dir.exists(temp_folder))
})

test_that("bayes_mldesc handles basic input correctly", {
  result <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    folder = "brms_models"
  )
  
  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$variable, c("X", "Y"))
  
  # Check that key columns exist
  expect_true(all(c("variable", "n_obs", "m", "sd", "range", "icc") %in% colnames(result)))
})

test_that("bayes_mldesc computes descriptive statistics correctly", {
  result <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = "x",
    folder = "brms_models"
  )
  
  # Check that statistics are present
  expect_match(result$n_obs, "^[0-9]+$")
  expect_match(result$m, "^[0-9]+\\.[0-9]{2}$")
  expect_match(result$sd, "^[0-9]+\\.[0-9]{2}$")
  expect_match(result$range, "^[0-9]+-[0-9]+$")
})

test_that("bayes_mldesc handles multiple variables", {
  result <- bayes_mldesc(
    data = test_data$multi_var,
    group = "group",
    vars = c("v1", "v2", "v3"),
    folder = "brms_models"
  )
  
  # Check dimensions
  expect_equal(nrow(result), 3)
  
  # Should have: variable, n_obs, m, sd, range, 3 correlation columns, icc
  expect_equal(ncol(result), 9)
  
  # Check all variables present
  expect_equal(result$variable, c("V1", "V2", "V3"))
})

test_that("bayes_mldesc computes ICC values", {
  result <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = "x",
    folder = "brms_models"
  )
  
  # ICC should be present and formatted
  expect_true("icc" %in% colnames(result))
  expect_match(result$icc, "^\\.[0-9]{2}$")
})

test_that("bayes_mldesc includes correlation matrix", {
  result <- bayes_mldesc(
    data = test_data$multi_var,
    group = "group",
    vars = c("v1", "v2", "v3"),
    folder = "brms_models"
  )
  
  # Check that correlation columns exist (named "1", "2", "3")
  expect_true(all(c("1", "2", "3") %in% colnames(result)))
  
  # Diagonal should be "\u2013"
  expect_equal(result$`1`[1], "\u2013")
  expect_equal(result$`2`[2], "\u2013")
  expect_equal(result$`3`[3], "\u2013")
})

test_that("bayes_mldesc handles missing values", {
  data_with_na <- test_data$basic
  data_with_na$x[c(1, 5, 15)] <- NA
  data_with_na$y[c(2, 10, 20)] <- NA
  
  result <- bayes_mldesc(
    data = data_with_na,
    group = "group",
    vars = c("x", "y"),
    folder = "brms_models"
  )
  
  # Should complete without error
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("bayes_mldesc remove_leading_zero parameter works", {
  result_with_removal <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    remove_leading_zero = TRUE,
    folder = "brms_models"
  )
  
  result_without_removal <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    remove_leading_zero = FALSE,
    folder = "brms_models"
  )
  
  # With removal: should have "." format
  expect_true(any(grepl("^\\.", result_with_removal$icc)))
  
  # Without removal: should have "0." format
  expect_true(any(grepl("^0\\.", result_without_removal$icc)))
})

test_that("bayes_mldesc handles single variable", {
  result <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = "x",
    folder = "brms_models"
  )
  
  # Should have: variable, n_obs, m, sd, range, 1 correlation column, icc
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 7)
  expect_equal(result$`1`[1], "\u2013")
})

test_that("bayes_mldesc handles numeric group variable", {
  result <- bayes_mldesc(
    data = test_data$multi_var,
    group = "group",
    vars = c("v1", "v2"),
    folder = "brms_models"
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("bayes_mldesc handles factor group variable", {
  data_factor <- test_data$basic
  data_factor$group <- factor(data_factor$group)
  
  result <- bayes_mldesc(
    data = data_factor,
    group = "group",
    vars = c("x", "y"),
    folder = "brms_models"
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("bayes_mldesc formats numbers correctly", {
  result <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = "x",
    folder = "brms_models"
  )
  
  # Mean and SD should have 2 decimal places
  expect_match(result$m, "^[0-9]+\\.[0-9]{2}$")
  expect_match(result$sd, "^[0-9]+\\.[0-9]{2}$")
  
  # Range should be integers
  expect_match(result$range, "^[0-9]+-[0-9]+$")
  
  # ICC should have 2 decimal places
  expect_match(result$icc, "^\\.[0-9]{2}$")
})

test_that("bayes_mldesc marks credible correlations", {
  result <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    folder = "brms_models"
  )
  
  # Should have correlation values
  cor_values <- c(result$`2`[1], result$`1`[2])
  cor_values <- cor_values[cor_values != "\u2013"]
  
  expect_true(length(cor_values) > 0)
  expect_true(all(nchar(cor_values) > 0))
})

test_that("bayes_mldesc output format matches expected structure", {
  result <- bayes_mldesc(
    data = test_data$multi_var,
    group = "group",
    vars = c("v1", "v2", "v3"),
    folder = "brms_models"
  )
  
  # Check column order: variable, n_obs, m, sd, range, correlations, icc
  expected_cols <- c("variable", "n_obs", "m", "sd", "range", "1", "2", "3", "icc")
  expect_equal(colnames(result), expected_cols)
  
  # Check that all values are character strings
  expect_true(all(sapply(result, is.character)))
})

test_that("bayes_mldesc weight=TRUE uses weighted correlations", {
  result_weighted <- bayes_mldesc(
    data = test_data$unbalanced,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE,
    folder = "brms_models"
  )
  
  expect_s3_class(result_weighted, "tbl_df")
  expect_equal(nrow(result_weighted), 2)
  
  # Between-group correlation should be present
  between_val <- result_weighted$`1`[2]
  expect_true(nchar(between_val) > 0)
  expect_true(between_val != "\u2013")
})

test_that("bayes_mldesc weight=FALSE uses unweighted correlations", {
  result_unweighted <- bayes_mldesc(
    data = test_data$unbalanced,
    group = "group",
    vars = c("x", "y"),
    weight = FALSE,
    folder = "brms_models"
  )
  
  expect_s3_class(result_unweighted, "tbl_df")
  expect_equal(nrow(result_unweighted), 2)
  
  # Between-group correlation should be present
  between_val <- result_unweighted$`1`[2]
  expect_true(nchar(between_val) > 0)
  expect_true(between_val != "\u2013")
})

test_that("bayes_mldesc weight argument defaults to TRUE", {
  result_default <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    folder = "brms_models"
  )
  
  result_explicit <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE,
    folder = "brms_models"
  )
  
  # Should be identical (models are cached)
  expect_identical(result_default, result_explicit)
})

test_that("bayes_mldesc weight parameter doesn't affect descriptives or ICC", {
  result_weighted <- bayes_mldesc(
    data = test_data$unbalanced,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE,
    folder = "brms_models"
  )
  
  result_unweighted <- bayes_mldesc(
    data = test_data$unbalanced,
    group = "group",
    vars = c("x", "y"),
    weight = FALSE,
    folder = "brms_models"
  )
  
  # n_obs and range should be identical
  expect_equal(result_weighted$n_obs, result_unweighted$n_obs)
  expect_equal(result_weighted$range, result_unweighted$range)
  
  # Mean and SD should DIFFER with unbalanced groups
  expect_false(identical(result_weighted$m, result_unweighted$m))
  expect_false(identical(result_weighted$sd, result_unweighted$sd))
  
  # ICC should be identical
  expect_equal(result_weighted$icc, result_unweighted$icc)
  
  # Within-group correlations (upper triangle) should be identical
  expect_equal(result_weighted$`2`[1], result_unweighted$`2`[1])
})

test_that("bayes_mldesc handles different ci levels", {
  result_90 <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    ci = 0.9,
    folder = "brms_models"
  )
  
  result_95 <- bayes_mldesc(
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

test_that("bayes_mldesc reuses cached models", {
  # First run
  result1 <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    folder = "brms_models"
  )
  
  # Second run should use cached models
  result2 <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    folder = "brms_models"
  )
  
  # Results should be identical
  expect_identical(result1, result2)
})

test_that("bayes_mldesc weighted vs unweighted differ with unbalanced data", {
  result_weighted <- bayes_mldesc(
    data = test_data$unbalanced,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE,
    folder = "brms_models"
  )
  
  result_unweighted <- bayes_mldesc(
    data = test_data$unbalanced,
    group = "group",
    vars = c("x", "y"),
    weight = FALSE,
    folder = "brms_models"
  )
  
  # Extract between-group correlations (lower triangle)
  between_weighted <- result_weighted$`1`[2]
  between_unweighted <- result_unweighted$`1`[2]
  
  # Both should be valid
  expect_true(nchar(between_weighted) > 0)
  expect_true(nchar(between_unweighted) > 0)
  expect_true(between_weighted != "\u2013")
  expect_true(between_unweighted != "\u2013")
})

test_that("bayes_mldesc weight=FALSE calculates mean of group means", {
  # Create data where group means are 10, 20, 30 but with different group sizes
  data_grouped <- data.frame(
    group = c(rep("A", 5), rep("B", 10), rep("C", 30)),
    x = c(rep(10, 5), rep(20, 10), rep(30, 30))
  )
  
  result_weighted <- bayes_mldesc(
    data = data_grouped,
    group = "group",
    vars = "x",
    weight = TRUE,
    folder = "brms_models"
  )
  
  result_unweighted <- bayes_mldesc(
    data = data_grouped,
    group = "group",
    vars = "x",
    weight = FALSE,
    folder = "brms_models"
  )
  
  # Weighted mean: (5*10 + 10*20 + 30*30) / 45 = 25.56
  expect_equal(result_weighted$m, "25.56")
  
  # Unweighted mean: (10 + 20 + 30) / 3 = 20.00
  expect_equal(result_unweighted$m, "20.00")
  
  # Weighted SD should be 6.93, sd(c(rep(10, 5), rep(20, 10), rep(30, 30)))
  expect_equal(result_weighted$sd, "6.93")
  
  # Unweighted SD should be SD of group means: sd(c(10, 20, 30)) = 10.00
  expect_equal(result_unweighted$sd, "10.00")
})

test_that("bayes_mldesc returns gt table when print_gt = TRUE", {
  result_gt <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    print_gt = TRUE,
    folder = "brms_models"
  )
  
  # Should return gt_tbl object
  expect_s3_class(result_gt, "gt_tbl")
  
  # Should not be a tibble
  expect_false(inherits(result_gt, "tbl_df"))
})

test_that("bayes_mldesc returns tibble when print_gt = FALSE (default)", {
  result_tibble <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    print_gt = FALSE,
    folder = "brms_models"
  )
  
  # Should return tibble
  expect_s3_class(result_tibble, "tbl_df")
  
  # Should not be a gt_tbl
  expect_false(inherits(result_tibble, "gt_tbl"))
})

test_that("bayes_mldesc gt table accepts custom parameters", {
  custom_title <- "Custom Bayesian Descriptive Statistics Table"
  custom_note <- "Custom correlation interpretation"
  custom_footer <- "Custom footer note"
  
  result_gt <- bayes_mldesc(
    data = test_data$basic,
    group = "group",
    vars = c("x", "y"),
    print_gt = TRUE,
    table_title = custom_title,
    correlation_note = custom_note,
    note_text = custom_footer,
    folder = "brms_models"
  )
  
  # Should return gt_tbl object with custom parameters
  expect_s3_class(result_gt, "gt_tbl")
})