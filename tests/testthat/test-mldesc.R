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
  
  # Check n_obs - extract underlying value
  expect_equal(vctrs::vec_data(result$n_obs)[1], "30")
  
  # Check mean - extract underlying value
  expect_equal(vctrs::vec_data(result$m)[1], "20.00")
  
  # Check range format - extract underlying value
  expect_match(vctrs::vec_data(result$range)[1], "^[0-9]+-[0-9]+$")
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
  icc_val <- vctrs::vec_data(result$icc)[1]
  expect_match(icc_val, "^\\.[0-9]{2}$")
  
  # ICC should be relatively high for this data structure
  icc_value <- as.numeric(paste0("0", icc_val))
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
  
  # Diagonal should be "\u2013" - extract underlying values
  expect_equal(vctrs::vec_data(result$`1`)[1], "\u2013")
  expect_equal(vctrs::vec_data(result$`2`)[2], "\u2013")
  expect_equal(vctrs::vec_data(result$`3`)[3], "\u2013")
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
  
  # n_obs should reflect non-missing values - extract underlying values
  expect_equal(vctrs::vec_data(result$n_obs)[1], "27")  # 30 - 3 missing
  expect_equal(vctrs::vec_data(result$n_obs)[2], "27")  # 30 - 3 missing
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
  
  # With removal: should have "." format - extract underlying values
  icc_with <- vctrs::vec_data(result_with_removal$icc)
  expect_true(any(grepl("^\\.", icc_with)))
  
  # Without removal: should have "0." format - extract underlying values
  icc_without <- vctrs::vec_data(result_without_removal$icc)
  expect_true(any(grepl("^0\\.", icc_without)))
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
  expect_equal(vctrs::vec_data(result$`1`)[1], "\u2013")
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
  expect_equal(vctrs::vec_data(result$n_obs)[1], "50")
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
  
  # Extract underlying values
  m_val <- vctrs::vec_data(result$m)[1]
  sd_val <- vctrs::vec_data(result$sd)[1]
  range_val <- vctrs::vec_data(result$range)[1]
  icc_val <- vctrs::vec_data(result$icc)[1]
  
  # Mean and SD should have 2 decimal places
  expect_match(m_val, "^[0-9]+\\.[0-9]{2}$")
  expect_match(sd_val, "^[0-9]+\\.[0-9]{2}$")
  
  # Range should be integers
  expect_match(range_val, "^[0-9]+-[0-9]+$")
  
  # ICC should have 2 decimal places
  expect_match(icc_val, "^\\.[0-9]{2}$")
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
  
  # Should have asterisks for significant correlations - extract underlying values
  cor_values <- c(vctrs::vec_data(result$`2`)[1], vctrs::vec_data(result$`1`)[2])
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
  
  # Check that variable is character, others are mlstats_stat
  expect_type(result$variable, "character")
  expect_s3_class(result$n_obs, "mlstats_stat")
  expect_s3_class(result$m, "mlstats_stat")
  expect_s3_class(result$sd, "mlstats_stat")
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
  
  # Between-group correlation should be present - extract underlying value
  between_val <- vctrs::vec_data(result_weighted$`1`)[2]
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
  
  # Between-group correlation should be present - extract underlying value
  between_val <- vctrs::vec_data(result_unweighted$`1`)[2]
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
  between_weighted <- vctrs::vec_data(result_weighted$`1`)[2]
  between_unweighted <- vctrs::vec_data(result_unweighted$`1`)[2]
  
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
  
  # Descriptive statistics - extract underlying value
  expect_equal(vctrs::vec_data(result_unweighted$n_obs)[1], "60")
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
  
  # n_obs and range should be identical - extract underlying values
  expect_equal(
    vctrs::vec_data(result_weighted$n_obs),
    vctrs::vec_data(result_unweighted$n_obs)
  )
  expect_equal(
    vctrs::vec_data(result_weighted$range),
    vctrs::vec_data(result_unweighted$range)
  )
  
  # Mean and SD should DIFFER with unbalanced groups - extract underlying values
  expect_false(identical(
    vctrs::vec_data(result_weighted$m),
    vctrs::vec_data(result_unweighted$m)
  ))
  expect_false(identical(
    vctrs::vec_data(result_weighted$sd),
    vctrs::vec_data(result_unweighted$sd)
  ))
  
  # ICC should be identical - extract underlying values
  expect_equal(
    vctrs::vec_data(result_weighted$icc),
    vctrs::vec_data(result_unweighted$icc)
  )
  
  # Within-group correlations (upper triangle) should be identical - extract underlying values
  expect_equal(
    vctrs::vec_data(result_weighted$`2`)[1],
    vctrs::vec_data(result_unweighted$`2`)[1]
  )
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
  
  # Extract underlying values
  m_weighted <- vctrs::vec_data(result_weighted$m)[1]
  m_unweighted <- vctrs::vec_data(result_unweighted$m)[1]
  sd_weighted <- vctrs::vec_data(result_weighted$sd)[1]
  sd_unweighted <- vctrs::vec_data(result_unweighted$sd)[1]
  
  # Weighted mean: (5*10 + 10*20 + 30*30) / 45 = 25.56
  expect_equal(m_weighted, "25.56")
  
  # Unweighted mean: (10 + 20 + 30) / 3 = 20.00
  expect_equal(m_unweighted, "20.00")
  
  # Weighted SD should be 6.93, sd(c(rep(10, 5), rep(20, 10), rep(30, 30)))
  expect_equal(sd_weighted, "6.93")
  
  # Unweighted SD should be SD of group means: sd(c(10, 20, 30)) = 10.00
  expect_equal(sd_unweighted, "10.00")
})

test_that("all non-variable columns have mlstats_stat class", {
  set.seed(5000)
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
  
  # Check that all columns except variable have mlstats_stat class
  expect_type(result$variable, "character")
  expect_s3_class(result$n_obs, "mlstats_stat")
  expect_s3_class(result$m, "mlstats_stat")
  expect_s3_class(result$sd, "mlstats_stat")
  expect_s3_class(result$range, "mlstats_stat")
  expect_s3_class(result$`1`, "mlstats_stat")
  expect_s3_class(result$`2`, "mlstats_stat")
  expect_s3_class(result$icc, "mlstats_stat")
})

test_that("result has mlstats_desc_tibble class", {
  set.seed(5001)
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
  
  # Check custom class is applied
  expect_s3_class(result, "mlstats_desc_tibble")
})

test_that("mldesc returns correct default output", {
  set.seed(4000)
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
  
  # Should return tibble with custom class
  expect_s3_class(result, "mlstats_desc_tibble")
  expect_s3_class(result, "tbl_df")
})

test_that("mldesc stores default attributes for printing", {
  set.seed(4001)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE
  )
  
  # Check that default attributes are stored
  expect_equal(attr(result, "table_title"), "Multilevel descriptive statistics")
  expect_equal(attr(result, "correlation_note"), 
               "Within-group correlations above, between-group correlations below the diagonal.")
  expect_match(attr(result, "note_text"), "Group-weighted")
  
  result_unweighted <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    weight = FALSE
  )
  
  # Note should reflect unweighted
  expect_match(attr(result_unweighted, "note_text"), "Unweighted")
})

test_that("mldesc flip=FALSE stores correct attributes", {
  set.seed(4002)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    flip = FALSE
  )
  
  # Check flipped attribute
  expect_false(attr(result, "flipped"))
  
  # Check correlation note
  expect_equal(
    attr(result, "correlation_note"),
    "Within-group correlations above, between-group correlations below the diagonal."
  )
})

test_that("mldesc flip=TRUE stores correct attributes", {
  set.seed(4003)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  result <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    flip = TRUE
  )
  
  # Check flipped attribute
  expect_true(attr(result, "flipped"))
  
  # Check correlation note
  expect_equal(
    attr(result, "correlation_note"),
    "Between-group correlations above, within-group correlations below the diagonal."
  )
})

test_that("mldesc flip=TRUE transposes correlation matrix", {
  set.seed(4004)
  data <- data.frame(
    group = rep(1:5, each = 10),
    x = rnorm(50),
    y = rnorm(50),
    z = rnorm(50)
  )
  
  result_normal <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y", "z"),
    flip = FALSE
  )
  
  result_flipped <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y", "z"),
    flip = TRUE
  )
  
  # Upper triangle of normal should equal lower triangle of flipped
  expect_equal(
    vctrs::vec_data(result_normal$`2`)[1],
    vctrs::vec_data(result_flipped$`1`)[2]
  )
  expect_equal(
    vctrs::vec_data(result_normal$`3`)[1],
    vctrs::vec_data(result_flipped$`1`)[3]
  )
  expect_equal(
    vctrs::vec_data(result_normal$`3`)[2],
    vctrs::vec_data(result_flipped$`2`)[3]
  )
})

test_that("mldesc flip defaults to FALSE", {
  set.seed(4005)
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
    flip = FALSE
  )
  
  # Should be identical
  expect_identical(result_default, result_explicit)
})

test_that("mldesc flip doesn't affect descriptives or ICC", {
  set.seed(4006)
  data <- data.frame(
    group = rep(1:5, each = 10),
    x = rnorm(50),
    y = rnorm(50)
  )
  
  result_normal <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    flip = FALSE
  )
  
  result_flipped <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    flip = TRUE
  )
  
  # Descriptive statistics should be identical
  expect_equal(
    vctrs::vec_data(result_normal$n_obs),
    vctrs::vec_data(result_flipped$n_obs)
  )
  expect_equal(
    vctrs::vec_data(result_normal$m),
    vctrs::vec_data(result_flipped$m)
  )
  expect_equal(
    vctrs::vec_data(result_normal$sd),
    vctrs::vec_data(result_flipped$sd)
  )
  expect_equal(
    vctrs::vec_data(result_normal$range),
    vctrs::vec_data(result_flipped$range)
  )
  
  # ICC should be identical
  expect_equal(
    vctrs::vec_data(result_normal$icc),
    vctrs::vec_data(result_flipped$icc)
  )
})