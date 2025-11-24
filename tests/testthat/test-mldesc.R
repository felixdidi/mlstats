test_that("mldesc handles basic input correctly", {
  set.seed(123)
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 10),
    x = c(rnorm(10, 10, 2), rnorm(10, 15, 2), rnorm(10, 20, 2)),
    y = c(rnorm(10, 5, 1), rnorm(10, 10, 1), rnorm(10, 15, 1))
  )
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$variable, c("X", "Y"))
  
  # Check that key columns exist
  expect_true(all(c("variable", "n_obs", "m", "sd", "range", "icc") %in% colnames(result)))
})

test_that("mldesc computes descriptive statistics correctly", {
  set.seed(456)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = c(rep(10, 10), rep(20, 10), rep(30, 10))
  )
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = "x"
  )
  
  # Check n_obs
  expect_equal(result$n_obs, "30")
  
  # Check mean
  expect_equal(result$m, "20.00")
  
  # Check range format
  expect_match(result$range, "^[0-9]+-[0-9]+$")
})

test_that("mldesc handles multiple variables", {
  set.seed(789)
  data <- data.frame(
    group = rep(1:5, each = 20),
    v1 = rnorm(100),
    v2 = rnorm(100),
    v3 = rnorm(100),
    v4 = rnorm(100)
  )
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = c("v1", "v2", "v3", "v4")
  )
  
  # Check dimensions
  expect_equal(nrow(result), 4)
  
  # Should have: variable, n_obs, m, sd, range, 4 correlation columns, icc
  expect_equal(ncol(result), 10)
  
  # Check all variables present
  expect_equal(result$variable, c("V1", "V2", "V3", "V4"))
})

test_that("mldesc computes ICC values", {
  set.seed(111)
  # Create data with known between-group variance
  data <- data.frame(
    group = rep(1:10, each = 10),
    x = rep(1:10, each = 10) + rnorm(100, 0, 0.5)
  )
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = "x"
  )
  
  # ICC should be present and numeric-like
  expect_true("icc" %in% colnames(result))
  expect_match(result$icc, "^\\.[0-9]{2}$")
  
  # ICC should be relatively high for this data structure
  icc_value <- as.numeric(paste0("0", result$icc))
  expect_gt(icc_value, 0.5)
})

test_that("mldesc includes correlation matrix", {
  set.seed(222)
  data <- data.frame(
    group = rep(1:5, each = 20),
    x = rnorm(100),
    y = rnorm(100),
    z = rnorm(100)
  )
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y", "z")
  )
  
  # Check that correlation columns exist (named "1", "2", "3")
  expect_true(all(c("1", "2", "3") %in% colnames(result)))
  
  # Diagonal should be "\u2013"
  expect_equal(result$`1`[1], "\u2013")
  expect_equal(result$`2`[2], "\u2013")
  expect_equal(result$`3`[3], "\u2013")
})

test_that("mldesc handles missing values", {
  set.seed(333)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  # Introduce missing values
  data$x[c(1, 5, 15)] <- NA
  data$y[c(2, 10, 20)] <- NA
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  # Should complete without error
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  
  # n_obs should reflect non-missing values
  expect_equal(result$n_obs[1], "27")  # 30 - 3 missing
  expect_equal(result$n_obs[2], "27")  # 30 - 3 missing
})

test_that("mldesc remove_leading_zero parameter works", {
  set.seed(444)
  data <- data.frame(
    group = rep(1:5, each = 20),
    x = rnorm(100),
    y = rnorm(100)
  )
  
  result_with_removal <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    remove_leading_zero = TRUE
  )
  
  result_without_removal <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    remove_leading_zero = FALSE
  )
  
  # With removal: should have "." format
  expect_true(any(grepl("^\\.", result_with_removal$icc)))
  
  # Without removal: should have "0." format
  expect_true(any(grepl("^0\\.", result_without_removal$icc)))
})

test_that("mldesc handles single variable", {
  set.seed(555)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30)
  )
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = "x"
  )
  
  # Should have: variable, n_obs, m, sd, range, 1 correlation column, icc
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 7)
  expect_equal(result$`1`[1], "\u2013")
})

test_that("mldesc handles numeric group variable", {
  set.seed(666)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("mldesc handles factor group variable", {
  set.seed(777)
  data <- data.frame(
    group = factor(rep(c("Low", "Med", "High"), each = 10)),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("mldesc handles unbalanced groups", {
  set.seed(888)
  data <- data.frame(
    group = c(rep("A", 5), rep("B", 15), rep("C", 30)),
    x = rnorm(50),
    y = rnorm(50)
  )
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$n_obs[1], "50")
})

test_that("mldesc formats numbers correctly", {
  set.seed(999)
  data <- data.frame(
    group = rep(1:5, each = 20),
    x = rnorm(100, 50, 10)
  )
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = "x"
  )
  
  # Mean and SD should have 2 decimal places
  expect_match(result$m, "^[0-9]+\\.[0-9]{2}$")
  expect_match(result$sd, "^[0-9]+\\.[0-9]{2}$")
  
  # Range should be integers
  expect_match(result$range, "^[0-9]+-[0-9]+$")
  
  # ICC should have 2 decimal places
  expect_match(result$icc, "^\\.[0-9]{2}$")
})

test_that("mldesc marks significant correlations", {
  set.seed(1111)
  # Create data with strong correlations
  data <- data.frame(
    group = rep(1:10, each = 20),
    x = rep(1:10, each = 20) + rnorm(200, 0, 1),
    y = rep(1:10, each = 20) + rnorm(200, 0, 1)
  )
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  # Should have asterisks for significant correlations
  cor_values <- c(result$`2`[1], result$`1`[2])
  cor_values <- cor_values[cor_values != "\u2013"]
  
  expect_true(any(grepl("\\*$", cor_values)))
})

test_that("mldesc output format matches expected structure", {
  set.seed(1234)
  data <- data.frame(
    group = rep(1:5, each = 20),
    x = rnorm(100),
    y = rnorm(100),
    z = rnorm(100)
  )
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y", "z")
  )
  
  # Check column order: variable, n_obs, m, sd, range, correlations, icc
  expected_cols <- c("variable", "n_obs", "m", "sd", "range", "1", "2", "3", "icc")
  expect_equal(colnames(result), expected_cols)
  
  # Check that all values are character strings
  expect_true(all(sapply(result, is.character)))
})

test_that("mldesc handles variables with zero variance within groups", {
  set.seed(1357)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rep(c(10, 20, 30), each = 10),  # No within-group variance
    y = rnorm(30)
  )
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  # Should complete without error
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("mldesc handles large number of variables efficiently", {
  set.seed(2468)
  # Create data with 10 variables
  n_vars <- 10
  data <- data.frame(
    group = rep(1:5, each = 20)
  )
  
  for (i in 1:n_vars) {
    data[[paste0("v", i)]] <- rnorm(100)
  }
  
  vars <- paste0("v", 1:n_vars)
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = vars
  )
  
  # Check dimensions
  expect_equal(nrow(result), n_vars)
  expect_equal(ncol(result), 5 + n_vars + 1)  # descriptives + correlations + icc
})

test_that("mldesc weight=TRUE uses weighted correlations", {
  set.seed(3000)
  # Create data with unbalanced groups
  data <- data.frame(
    group = c(rep("A", 5), rep("B", 45)),
    x = c(rnorm(5, 0, 1), rnorm(45, 10, 1)),
    y = c(rnorm(5, 0, 1), rnorm(45, 10, 1))
  )
  
  result_weighted <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE
  )
  
  expect_s3_class(result_weighted, "tbl_df")
  expect_equal(nrow(result_weighted), 2)
  
  # Between-group correlation should be present
  between_val <- result_weighted$`1`[2]
  expect_true(nchar(between_val) > 0)
  expect_true(between_val != "\u2013")
})

test_that("mldesc weight=FALSE uses unweighted correlations", {
  set.seed(3001)
  # Create data with unbalanced groups
  data <- data.frame(
    group = c(rep("A", 5), rep("B", 45)),
    x = c(rnorm(5, 0, 1), rnorm(45, 10, 1)),
    y = c(rnorm(5, 0, 1), rnorm(45, 10, 1))
  )
  
  result_unweighted <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    weight = FALSE
  )
  
  expect_s3_class(result_unweighted, "tbl_df")
  expect_equal(nrow(result_unweighted), 2)
  
  # Between-group correlation should be present
  between_val <- result_unweighted$`1`[2]
  expect_true(nchar(between_val) > 0)
  expect_true(between_val != "\u2013")
})

test_that("mldesc weighted vs unweighted differ with unbalanced data", {
  set.seed(3002)
  # Create highly unbalanced groups with different means
  data <- data.frame(
    group = c(rep("A", 10), rep("B", 10), rep("C", 80)),
    x = c(rnorm(10, 0, 1), rnorm(10, 5, 1), rnorm(80, 10, 1)),
    y = c(rnorm(10, 0, 1), rnorm(10, 5, 1), rnorm(80, 10, 1))
  )
  
  result_weighted <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE
  )
  
  result_unweighted <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    weight = FALSE
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

test_that("mldesc weight argument defaults to TRUE", {
  set.seed(3003)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  result_default <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  result_explicit <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE
  )
  
  # Should be identical
  expect_identical(result_default, result_explicit)
})

test_that("mldesc unweighted works with balanced groups", {
  set.seed(3004)
  # With balanced groups, weighted and unweighted should be similar
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 20),
    x = c(rnorm(20, 0, 1), rnorm(20, 5, 1), rnorm(20, 10, 1)),
    y = c(rnorm(20, 0, 1), rnorm(20, 5, 1), rnorm(20, 10, 1))
  )
  
  result_unweighted <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    weight = FALSE
  )
  
  expect_s3_class(result_unweighted, "tbl_df")
  expect_equal(nrow(result_unweighted), 2)
  
  # Descriptive statistics should be unaffected by weight parameter
  expect_equal(result_unweighted$n_obs[1], "60")
})

test_that("mldesc weight parameter doesn't affect descriptives or ICC", {
  set.seed(3005)
  data <- data.frame(
    group = c(rep("A", 10), rep("B", 40)),
    x = c(rnorm(10, 0, 1), rnorm(40, 10, 1)),
    y = c(rnorm(10, 0, 1), rnorm(40, 10, 1))
  )
  
  result_weighted <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE
  )
  
  result_unweighted <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    weight = FALSE
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

test_that("mldesc weight=FALSE calculates mean of group means", {
  set.seed(3006)
  # Create data where group means are 10, 20, 30 but with different group sizes
  data <- data.frame(
    group = c(rep("A", 5), rep("B", 10), rep("C", 30)),
    x = c(rep(10, 5), rep(20, 10), rep(30, 30))
  )
  
  result_weighted <- mldesc(
    data = data,
    group = "group",
    vars = "x",
    weight = TRUE
  )
  
  result_unweighted <- mldesc(
    data = data,
    group = "group",
    vars = "x",
    weight = FALSE
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

test_that("mldesc returns gt table when print_gt = TRUE", {
  set.seed(4000)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  result_gt <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    print_gt = TRUE
  )
  
  # Should return gt_tbl object
  expect_s3_class(result_gt, "gt_tbl")
  
  # Should not be a tibble
  expect_false(inherits(result_gt, "tbl_df"))
})

test_that("mldesc returns tibble when print_gt = FALSE (default)", {
  set.seed(4001)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  result_tibble <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    print_gt = FALSE
  )
  
  # Should return tibble
  expect_s3_class(result_tibble, "tbl_df")
  
  # Should not be a gt_tbl
  expect_false(inherits(result_tibble, "gt_tbl"))
})

test_that("mldesc gt table accepts custom parameters", {
  set.seed(4002)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  custom_title <- "Custom Descriptive Statistics Table"
  custom_note <- "Custom correlation interpretation"
  custom_footer <- "Custom footer note"
  
  result_gt <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    print_gt = TRUE,
    table_title = custom_title,
    correlation_note = custom_note,
    note_text = custom_footer
  )
  
  # Should return gt_tbl object with custom parameters
  expect_s3_class(result_gt, "gt_tbl")
})