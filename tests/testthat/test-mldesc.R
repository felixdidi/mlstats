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
  # x varies within groups (else it would be counted once per group in
  # n_obs, like a trait), with an exact overall mean of 20
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = c(rep(c(9, 11), 5), rep(c(19, 21), 5), rep(c(29, 31), 5))
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
  
  # Check range format - extract underlying value (whole-number data, so
  # decimals are dropped)
  expect_equal(vctrs::vec_data(result$range)[1], "9–31")
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
  
  # Range should have 2 decimal places
  expect_match(range_val, "^-?[0-9]+\\.[0-9]{2}–-?[0-9]+\\.[0-9]{2}$")
  
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
  
  # Check that default attributes are stored (table_title defaults to "" so
  # that gt/tt output has no title unless the user supplies one via print())
  expect_equal(attr(result, "table_title"), "")
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

test_that("mldesc print method accepts format parameter", {
  set.seed(6000)
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

  result_gt <- print(result, "gt")
  # Should return a gt table
  expect_s3_class(result_gt, "gt_tbl")

  result_tt <- print(result, "tt")
  # Should return a tinytable
  expect_s4_class(result_tt, "tinytable")
})

test_that("mldesc print method accepts custom parameters", {
  set.seed(6000)
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
  
  custom_title <- "Custom Bayesian Descriptive Statistics Table"
  custom_note <- "Custom correlation interpretation"
  custom_footer <- "Custom footer note"
  
  result_gt <- print(
    result,
    "gt",
    table_title = custom_title,
    correlation_note = custom_note,
    note_text = custom_footer
  )

  result_tt <- print(
    result,
    "tt",
    table_title = custom_title,
    correlation_note = custom_note,
    note_text = custom_footer
  )
  
  # Should return correct classes
  expect_s3_class(result_gt, "gt_tbl")
  expect_s4_class(result_tt, "tinytable")
})

# ---- Tests for method = "sem" ----

test_that("mldesc method='sem' handles basic input correctly", {
  set.seed(7001)
  data <- data.frame(
    group = rep(1:10, each = 20),
    x = rnorm(200, 50, 10),
    y = rnorm(200, 50, 10)
  )

  result <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    method = "sem"
  )

  # Check structure
  expect_s3_class(result, "mlstats_desc_tibble")
  expect_equal(nrow(result), 2)
  expect_true(all(c("variable", "n_obs", "m", "sd", "range", "1", "2", "icc") %in% colnames(result)))
})

test_that("mldesc method='sem' does not leak the weight-ignored message", {
  set.seed(7001)
  data <- data.frame(
    group = rep(1:10, each = 20),
    x = rnorm(200, 50, 10),
    y = rnorm(200, 50, 10)
  )

  # within_between_correlations() informs when `weight` is explicitly
  # specified under method = "sem" (it has no effect on the correlations),
  # but mldesc() should not surface that message: `weight` still controls
  # the mean/SD calculation here regardless of `method`.
  expect_no_message(
    mldesc(
      data = data,
      group = "group",
      vars = c("x", "y"),
      method = "sem",
      weight = FALSE
    )
  )
})

test_that("mldesc method='sem' still uses weight for mean/SD with unequal group sizes", {
  set.seed(7002)
  data <- data.frame(
    group = rep(1:5, times = c(2, 4, 6, 8, 10)),
    x = rnorm(30, 50, 10)
  )

  result_weighted <- suppressWarnings(mldesc(
    data = data,
    group = "group",
    vars = "x",
    method = "sem",
    weight = TRUE
  ))
  result_unweighted <- suppressWarnings(mldesc(
    data = data,
    group = "group",
    vars = "x",
    method = "sem",
    weight = FALSE
  ))

  expect_false(identical(result_weighted$m, result_unweighted$m))
})

test_that("mldesc method='sem' stores correct method attribute", {
  set.seed(7002)
  data <- data.frame(
    group = rep(1:5, each = 20),
    x = rnorm(100),
    y = rnorm(100)
  )

  result_sem <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    method = "sem"
  )

  expect_equal(attr(result_sem, "method"), "sem")
  expect_match(attr(result_sem, "note_text"), "SEM-based")
})

test_that("mldesc method='sem' still computes descriptives and ICC", {
  set.seed(7003)
  data <- data.frame(
    group = rep(1:10, each = 20),
    x = rep(1:10, each = 20) + rnorm(200, 0, 0.5)
  )

  result <- mldesc(
    data = data,
    group = "group",
    vars = "x",
    method = "sem"
  )

  # ICC should be present and high
  icc_val <- vctrs::vec_data(result$icc)[1]
  expect_match(icc_val, "^\\.[0-9]{2}$")
  icc_value <- as.numeric(paste0("0", icc_val))
  expect_gt(icc_value, 0.5)

  # n_obs should be correct
  expect_equal(vctrs::vec_data(result$n_obs)[1], "200")
})

test_that("mldesc method defaults to decomposition", {
  set.seed(7004)
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
    method = "decomposition"
  )

  expect_identical(result_default, result_explicit)
})

test_that("mldesc method='sem' works with flip=TRUE", {
  set.seed(7005)
  data <- data.frame(
    group = rep(1:10, each = 20),
    x = rnorm(200),
    y = rnorm(200),
    z = rnorm(200)
  )

  # Whether this particular model yields an out-of-range standardized
  # solution (and hence the "out-of-range" warning) depends on the lavaan
  # version: older versions converge on an inadmissible between-level
  # solution here, newer ones do not. That is incidental to what this test
  # checks -- the flip= symmetry -- so any warning is simply suppressed.
  result_normal <- suppressWarnings(
    mldesc(
      data = data,
      group = "group",
      vars = c("x", "y", "z"),
      method = "sem",
      flip = FALSE
    )
  )

  result_flipped <- suppressWarnings(
    mldesc(
      data = data,
      group = "group",
      vars = c("x", "y", "z"),
      method = "sem",
      flip = TRUE
    )
  )

  # Upper triangle of normal should equal lower triangle of flipped
  expect_equal(
    vctrs::vec_data(result_normal$`2`)[1],
    vctrs::vec_data(result_flipped$`1`)[2]
  )

  # Descriptives and ICC should be identical
  expect_equal(
    vctrs::vec_data(result_normal$icc),
    vctrs::vec_data(result_flipped$icc)
  )
})

test_that("mldesc method='sem' print methods work", {
  set.seed(7006)
  data <- data.frame(
    group = rep(1:5, each = 20),
    x = rnorm(100),
    y = rnorm(100)
  )

  result <- mldesc(
    data = data,
    group = "group",
    vars = c("x", "y"),
    method = "sem"
  )

  result_gt <- print(result, "gt")
  expect_s3_class(result_gt, "gt_tbl")

  result_tt <- print(result, "tt")
  expect_s4_class(result_tt, "tinytable")
})

test_that("mldesc method='sem' handles between-only variables correctly", {
  set.seed(7007)
  # Create a between-only variable (constant within groups, like a trait)
  data <- data.frame(
    group = rep(1:20, each = 25)
  )
  data$trait <- rep(rnorm(20, 10, 3), each = 25)
  data$x <- rnorm(500, 5, 2)
  data$y <- rnorm(500, 5, 2)

  # As above, an "out-of-range" warning here is lavaan-version dependent and
  # incidental: this test checks the handling of the between-only variable.
  result <- suppressWarnings(
    mldesc(
      data = data,
      group = "group",
      vars = c("trait", "x", "y"),
      method = "sem"
    )
  )

  # Check structure
  expect_s3_class(result, "mlstats_desc_tibble")
  expect_equal(nrow(result), 3)

  # Descriptives should be computed for all variables; the trait is constant
  # within groups, so its n_obs counts groups (20), not rows (500)
  expect_equal(vctrs::vec_data(result$n_obs)[1], "20")
  expect_equal(vctrs::vec_data(result$n_obs)[2], "500")

  # Within-group correlations involving the trait should be NA
  expect_equal(vctrs::vec_data(result$`2`)[1], "NA")
  expect_equal(vctrs::vec_data(result$`3`)[1], "NA")

  # ICC for the between-only variable should be very high (close to 1)
  icc_trait <- as.numeric(paste0("0", vctrs::vec_data(result$icc)[1]))
  expect_gt(icc_trait, 0.9)
})

test_that("mldesc errors on unknown group variable", {
  data <- data.frame(group = rep(1:3, each = 5), x = rnorm(15))
  expect_error(
    mldesc(data, group = "nope", vars = "x"),
    "not found"
  )
})

test_that("mldesc errors on unknown vars", {
  data <- data.frame(group = rep(1:3, each = 5), x = rnorm(15))
  expect_error(
    mldesc(data, group = "group", vars = c("x", "missing")),
    "not found"
  )
})

test_that("mldesc errors for non-numeric variables", {
  set.seed(11)
  data <- data.frame(
    group = rep(1:5, each = 10),
    x = rnorm(50),
    flag = sample(c(TRUE, FALSE), 50, replace = TRUE)
  )

  expect_error(mldesc(data, "group", c("x", "flag")), "numeric")
})

test_that("mldesc default print method dispatches to pillar formatting", {
  set.seed(12)
  data <- data.frame(
    group = rep(1:5, each = 10),
    x = rnorm(50),
    y = rnorm(50)
  )
  result <- mldesc(data, "group", c("x", "y"))

  output <- capture.output(print(result))
  expect_true(any(grepl("Multilevel Descriptive Statistics", output)))
  expect_true(any(grepl("Within-group correlations", output)))
  expect_true(any(grepl("variance decomposition", output)))
})

test_that("mldesc print accepts a custom significance_note", {
  set.seed(13)
  data <- data.frame(
    group = rep(1:5, each = 10),
    x = rnorm(50),
    y = rnorm(50)
  )
  result <- mldesc(data, "group", c("x", "y"))

  output <- capture.output(print(result, significance_note = "Custom significance note."))
  expect_true(any(grepl("Custom significance note", output)))
})

# ---- Tests for `ci`/`folder` being no-ops outside method = "bayes" ----

test_that("ci argument is ignored with a message unless method = 'bayes'", {
  set.seed(3012)
  data <- data.frame(group = rep(1:3, each = 10), x = rnorm(30), y = rnorm(30))

  expect_message(
    mldesc(data, "group", c("x", "y"), ci = 0.8),
    "no effect"
  )
})

test_that("folder argument is ignored with a message unless method = 'bayes'", {
  set.seed(3013)
  data <- data.frame(group = rep(1:3, each = 10), x = rnorm(30), y = rnorm(30))

  expect_message(
    mldesc(data, "group", c("x", "y"), folder = tempdir()),
    "no effect"
  )
})

# ---- Tests for method = "bayes" ----
# See helper-bayes-fixtures.R for the shared cache folder/fixture data/
# skip_if_no_bayes() and why these tests are structured to reuse cached brms
# fits (both the ICC fits here and the correlation fits from
# test-within_between_correlations.R's "method = 'bayes'" section, since
# mldesc(method = "bayes") calls within_between_correlations(method =
# "bayes") internally with the same data/vars/folder) as much as possible.

test_that("mldesc method='bayes' requires folder argument", {
  skip_if_no_bayes()
  expect_error(
    mldesc(
      data = bayes_fixture_basic,
      group = "group",
      vars = c("x", "y"),
      method = "bayes"
    ),
    "folder.*must be specified"
  )
})

test_that("mldesc method='bayes' validates ci argument", {
  skip_if_no_bayes()
  expect_error(
    mldesc(
      data = bayes_fixture_basic,
      group = "group",
      vars = c("x", "y"),
      method = "bayes",
      ci = 0,
      folder = bayes_cache_folder
    ),
    "ci.*must be between 0 and 1"
  )

  expect_error(
    mldesc(
      data = bayes_fixture_basic,
      group = "group",
      vars = c("x", "y"),
      method = "bayes",
      ci = 1.5,
      folder = bayes_cache_folder
    ),
    "ci.*must be between 0 and 1"
  )
})

test_that("mldesc method='bayes' creates folder if it doesn't exist", {
  skip_if_no_bayes()
  # Single variable: still fits one ICC model (ICC is always computed), but
  # no correlation models, keeping this cheap.
  temp_folder <- file.path(tempdir(), "test_brms_mldesc_folder")
  on.exit(unlink(temp_folder, recursive = TRUE), add = TRUE)

  expect_false(dir.exists(temp_folder))

  result <- mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = "x",
    method = "bayes",
    folder = temp_folder
  )

  expect_true(dir.exists(temp_folder))
})

test_that("mldesc method='bayes' handles basic input correctly", {
  skip_if_no_bayes()
  result <- mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    method = "bayes",
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
  expect_match(result$range, "^-?[0-9]+\\.[0-9]{2}–-?[0-9]+\\.[0-9]{2}$")
  expect_match(result$icc, "^\\.[0-9]{2}$")
  expect_equal(as.character(result$`1`[1]), "\u2013")

  expect_true(all(sapply(result[, -1], function(col) inherits(col, "mlstats_stat"))))

  expect_equal(attr(result, "method"), "bayes")
  expect_true(attr(result, "bayesian"))
  expect_true(!is.null(attr(result, "table_title")))
  expect_true(!is.null(attr(result, "correlation_note")))
})

test_that("mldesc method='bayes' weight=FALSE affects descriptives but not ICC", {
  skip_if_no_bayes()
  result_weighted <- mldesc(
    data = bayes_fixture_unbalanced,
    group = "group",
    vars = "x",
    method = "bayes",
    weight = TRUE,
    folder = bayes_cache_folder
  )

  result_unweighted <- mldesc(
    data = bayes_fixture_unbalanced,
    group = "group",
    vars = "x",
    method = "bayes",
    weight = FALSE,
    folder = bayes_cache_folder
  )

  group_means <- tapply(bayes_fixture_unbalanced$x, bayes_fixture_unbalanced$group, mean)

  expect_equal(as.character(result_weighted$m), sprintf("%.2f", mean(bayes_fixture_unbalanced$x)))
  expect_equal(as.character(result_unweighted$m), sprintf("%.2f", mean(group_means)))

  # ICC doesn't depend on weighting
  expect_equal(as.character(result_weighted$icc), as.character(result_unweighted$icc))

  expect_match(attr(result_weighted, "note_text"), "Bayesian group-weighted")
  expect_match(attr(result_unweighted, "note_text"), "Bayesian unweighted")
})

test_that("mldesc method='bayes' handles different ci levels without refitting", {
  skip_if_no_bayes()
  result_90 <- mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    method = "bayes",
    ci = 0.9,
    folder = bayes_cache_folder
  )

  result_95 <- mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    method = "bayes",
    ci = 0.95,
    folder = bayes_cache_folder
  )

  expect_equal(dim(result_90), dim(result_95))
})

test_that("mldesc method='bayes' reuses cached models", {
  skip_if_no_bayes()
  result1 <- mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    method = "bayes",
    folder = bayes_cache_folder
  )

  result2 <- mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    method = "bayes",
    folder = bayes_cache_folder
  )

  expect_identical(result1, result2)
})

test_that("mldesc method='bayes' flip=TRUE forwards correctly", {
  skip_if_no_bayes()
  result_normal <- mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    method = "bayes",
    folder = bayes_cache_folder,
    flip = FALSE
  )

  result_flipped <- mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    method = "bayes",
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

  # ICC should be identical regardless of flip
  expect_equal(
    vctrs::vec_data(result_normal$icc),
    vctrs::vec_data(result_flipped$icc)
  )
})

test_that("mldesc method='bayes' print method supports gt, tt, and default formats", {
  skip_if_no_bayes()
  result <- mldesc(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    method = "bayes",
    folder = bayes_cache_folder
  )

  result_gt <- print(result, "gt")
  expect_s3_class(result_gt, "gt_tbl")

  result_tt <- print(result, "tt")
  expect_s4_class(result_tt, "tinytable")

  output <- capture.output(print(result))
  expect_true(any(grepl("Multilevel Descriptive Statistics", output)))
  expect_true(any(grepl("credible intervals", output)))
  expect_true(any(grepl("Bayesian multilevel models", output)))
})

test_that("mldesc method='bayes' ignores significance with a message", {
  skip_if_no_bayes()
  expect_message(
    mldesc(
      data = bayes_fixture_basic,
      group = "group",
      vars = "x",
      method = "bayes",
      significance = "detailed",
      folder = bayes_cache_folder
    ),
    "no effect"
  )
})
test_that("mldesc counts trait variables once per group in n_obs", {
  set.seed(8101)
  data <- data.frame(
    group = rep(1:10, each = 10)
  )
  data$trait <- rep(rnorm(10), each = 10)
  data$x <- rnorm(100)

  result <- mldesc(data, group = "group", vars = c("trait", "x"))

  # trait is constant within every group: n_obs counts groups, not rows
  expect_equal(vctrs::vec_data(result$n_obs)[1], "10")
  expect_equal(vctrs::vec_data(result$n_obs)[2], "100")
})

test_that("mldesc trait n_obs counts only groups that provided a value", {
  set.seed(8102)
  data <- data.frame(
    group = rep(1:10, each = 10)
  )
  data$trait <- rep(rnorm(10), each = 10)
  data$trait[data$group %in% 1:3] <- NA  # 3 groups never measured
  data$x <- rnorm(100)

  result <- mldesc(data, group = "group", vars = c("trait", "x"))

  expect_equal(vctrs::vec_data(result$n_obs)[1], "7")
})

test_that("mldesc formats range with two decimals", {
  data <- data.frame(
    group = rep(1:5, each = 4),
    x = c(1.512, rep(3, 18), 6.897)
  )

  result <- mldesc(data, group = "group", vars = "x")

  expect_equal(vctrs::vec_data(result$range)[1], "1.51–6.90")
})

test_that("mldesc drops range decimals when min and max are whole numbers", {
  set.seed(8106)
  data <- data.frame(
    group = rep(1:5, each = 4),
    x = c(1, sample(c(2.5, 3, 4.5), 18, replace = TRUE), 7)
  )

  # Interior values have decimals, but min (1) and max (7) are whole
  result <- mldesc(data, group = "group", vars = "x")

  expect_equal(vctrs::vec_data(result$range)[1], "1–7")
})

test_that("mldesc errors informatively on all-NA variables", {
  data <- data.frame(
    group = rep(1:5, each = 4),
    x = rnorm(20),
    y = NA_real_
  )

  expect_error(
    mldesc(data, group = "group", vars = c("x", "y")),
    "only missing values"
  )
})

test_that("mldesc warns on and excludes observations with a missing group", {
  set.seed(8103)
  data <- data.frame(
    group = rep(1:10, each = 10),
    x = rnorm(100),
    y = rnorm(100)
  )
  data_na <- data
  data_na$group[1:10] <- NA  # all of group 1 plus nothing else

  result_na <- expect_warning_value(
    mldesc(data_na, group = "group", vars = c("x", "y")),
    "missing value on the grouping variable"
  )
  result_filtered <- mldesc(
    data[data$group != 1, ],
    group = "group",
    vars = c("x", "y")
  )

  expect_equal(
    vctrs::vec_data(result_na$n_obs),
    vctrs::vec_data(result_filtered$n_obs)
  )
  expect_equal(vctrs::vec_data(result_na$`2`), vctrs::vec_data(result_filtered$`2`))
  expect_equal(vctrs::vec_data(result_na$icc), vctrs::vec_data(result_filtered$icc))
})
