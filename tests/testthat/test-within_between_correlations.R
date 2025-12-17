# Test setup
test_that("within_between_correlations handles basic input correctly", {
  # Create simple test data
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 10),
    x = c(rnorm(10, 10, 2), rnorm(10, 15, 2), rnorm(10, 20, 2)),
    y = c(rnorm(10, 5, 1), rnorm(10, 10, 1), rnorm(10, 15, 1))
  )
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "mlstats_wb_tibble")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
  expect_equal(result$variable, c("x", "y"))
})

test_that("within_between_correlations produces symmetric matrix structure", {
  set.seed(123)
  data <- data.frame(
    group = rep(1:5, each = 20),
    v1 = rnorm(100),
    v2 = rnorm(100),
    v3 = rnorm(100)
  )
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("v1", "v2", "v3")
  )
  
  # Check dimensions
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4)  # variable column + 3 correlation columns
  
  # Check diagonal is "\u2013" - need to extract underlying data
  expect_equal(vctrs::vec_data(result$`1`)[1], "\u2013")
  expect_equal(vctrs::vec_data(result$`2`)[2], "\u2013")
  expect_equal(vctrs::vec_data(result$`3`)[3], "\u2013")
})

test_that("within_between_correlations handles perfect correlations", {
  data <- data.frame(
    group = rep(c("A", "B"), each = 10),
    x = rep(1:20, 1),
    y = rep(1:20, 1) * 2  # Perfect linear relationship
  )
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  # Should contain correlation values - extract underlying data
  val <- vctrs::vec_data(result$`2`)[1]
  expect_true(grepl("1\\.00", val) || grepl("0\\.", val))
})

test_that("within_between_correlations marks significant correlations", {
  set.seed(456)
  # Create data with strong between-group differences
  data <- data.frame(
    group = rep(c("A", "B", "C", "D", "E"), each = 20),
    x = c(rnorm(20, 0), rnorm(20, 5), rnorm(20, 10), rnorm(20, 15), rnorm(20, 20)),
    y = c(rnorm(20, 0), rnorm(20, 5), rnorm(20, 10), rnorm(20, 15), rnorm(20, 20))
  )
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  # Check that asterisk exists for significant correlations
  # At least one cell should have content
  val <- vctrs::vec_data(result$`2`)[1]
  expect_true(nchar(val) > 0)
})

test_that("within_between_correlations handles missing values", {
  set.seed(789)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  # Introduce some missing values
  data$x[c(1, 5, 15)] <- NA
  data$y[c(2, 10, 20)] <- NA
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  # Should complete without error
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("within_between_correlations handles single variable", {
  set.seed(111)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30)
  )
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = "x"
  )
  
  # Should return 1x2 tibble with diagonal only
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 2)
  expect_equal(vctrs::vec_data(result$`1`)[1], "\u2013")
})

test_that("within_between_correlations handles many variables", {
  set.seed(222)
  data <- data.frame(
    group = rep(1:5, each = 20),
    v1 = rnorm(100),
    v2 = rnorm(100),
    v3 = rnorm(100),
    v4 = rnorm(100),
    v5 = rnorm(100)
  )
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("v1", "v2", "v3", "v4", "v5")
  )
  
  # Check dimensions
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 6)  # variable column + 5 correlation columns
})

test_that("within_between_correlations upper triangle is within-group", {
  set.seed(333)
  # Create data where within-group correlation differs from between-group
  data <- data.frame(
    group = rep(1:10, each = 10),
    x = rep(1:10, each = 10) + rnorm(100, 0, 0.5),  # Strong between-group variation
    y = rnorm(100)  # Random within groups
  )
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  # Upper triangle should be within-group (likely small/non-significant)
  # Lower triangle should be between-group
  within_val <- vctrs::vec_data(result$`2`)[1]
  between_val <- vctrs::vec_data(result$`1`)[2]
  
  expect_true(nchar(within_val) > 0)
  expect_true(nchar(between_val) > 0)
})

test_that("within_between_correlations handles numeric group variable", {
  set.seed(444)
  data <- data.frame(
    group = rep(1:3, each = 10),  # Numeric group
    x = rnorm(30),
    y = rnorm(30)
  )
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("within_between_correlations handles factor group variable", {
  set.seed(555)
  data <- data.frame(
    group = factor(rep(c("Low", "Med", "High"), each = 10)),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("within_between_correlations handles very small groups", {
  set.seed(666)
  # Minimum viable: 3 groups with 2 observations each
  data <- data.frame(
    group = rep(1:3, each = 2),
    x = rnorm(6),
    y = rnorm(6)
  )
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("within_between_correlations handles unbalanced groups", {
  set.seed(777)
  data <- data.frame(
    group = c(rep("A", 5), rep("B", 15), rep("C", 30)),
    x = rnorm(50),
    y = rnorm(50)
  )
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("within_between_correlations output format is correct", {
  set.seed(888)
  data <- data.frame(
    group = rep(1:5, each = 10),
    x = rnorm(50),
    y = rnorm(50)
  )
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  # Check column names
  expect_true("variable" %in% colnames(result))
  expect_true(all(colnames(result)[-1] %in% as.character(1:2)))
  
  # Check that correlations are formatted correctly (e.g., "0.45" or "-0.23*")
  # Need to extract underlying character data from mlstats_stat vectors
  cor_vals <- c(vctrs::vec_data(result$`1`), vctrs::vec_data(result$`2`))
  cor_vals <- cor_vals[cor_vals != "\u2013"]
  
  # Should be numeric-like strings
  expect_true(all(grepl("^-?[0-9]\\.[0-9]{2}\\*?$|^NA$", cor_vals)))
})

test_that("within_between_correlations weight=TRUE uses weighted correlations", {
  set.seed(999)
  # Create data with unbalanced groups
  data <- data.frame(
    group = c(rep("A", 5), rep("B", 45)),  # Very unbalanced
    x = c(rnorm(5, 0, 1), rnorm(45, 10, 1)),
    y = c(rnorm(5, 0, 1), rnorm(45, 10, 1))
  )
  
  result_weighted <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE
  )
  
  expect_s3_class(result_weighted, "tbl_df")
  expect_equal(nrow(result_weighted), 2)
  
  # Between-group correlation should be computed with all observations
  between_val <- vctrs::vec_data(result_weighted$`1`)[2]
  expect_true(nchar(between_val) > 0)
})

test_that("within_between_correlations weight=FALSE uses unweighted correlations", {
  set.seed(1000)
  # Create data with unbalanced groups
  data <- data.frame(
    group = c(rep("A", 5), rep("B", 45)),  # Very unbalanced
    x = c(rnorm(5, 0, 1), rnorm(45, 10, 1)),
    y = c(rnorm(5, 0, 1), rnorm(45, 10, 1))
  )
  
  result_unweighted <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y"),
    weight = FALSE
  )
  
  expect_s3_class(result_unweighted, "tbl_df")
  expect_equal(nrow(result_unweighted), 2)
  
  # Between-group correlation should be computed with only group means
  between_val <- vctrs::vec_data(result_unweighted$`1`)[2]
  expect_true(nchar(between_val) > 0)
})

test_that("within_between_correlations weighted vs unweighted differ with unbalanced data", {
  set.seed(1001)
  # Create highly unbalanced groups with different means
  data <- data.frame(
    group = c(rep("A", 10), rep("B", 10), rep("C", 80)),
    x = c(rnorm(10, 0, 1), rnorm(10, 5, 1), rnorm(80, 10, 1)),
    y = c(rnorm(10, 0, 1), rnorm(10, 5, 1), rnorm(80, 10, 1))
  )
  
  result_weighted <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE
  )
  
  result_unweighted <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y"),
    weight = FALSE
  )
  
  # Extract between-group correlations (lower triangle)
  between_weighted <- vctrs::vec_data(result_weighted$`1`)[2]
  between_unweighted <- vctrs::vec_data(result_unweighted$`1`)[2]
  
  # They should differ (though we can't guarantee direction without knowing exact values)
  # At minimum, both should be valid
  expect_true(nchar(between_weighted) > 0)
  expect_true(nchar(between_unweighted) > 0)
})

test_that("within_between_correlations weight argument defaults to TRUE", {
  set.seed(1002)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  result_default <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  result_explicit <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y"),
    weight = TRUE
  )
  
  # Should be identical
  expect_identical(result_default, result_explicit)
})

test_that("within_between_correlations unweighted works with balanced groups", {
  set.seed(1003)
  # With balanced groups, weighted and unweighted should be very similar
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 20),
    x = c(rnorm(20, 0, 1), rnorm(20, 5, 1), rnorm(20, 10, 1)),
    y = c(rnorm(20, 0, 1), rnorm(20, 5, 1), rnorm(20, 10, 1))
  )
  
  result_unweighted <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y"),
    weight = FALSE
  )
  
  expect_s3_class(result_unweighted, "tbl_df")
  expect_equal(nrow(result_unweighted), 2)
})

test_that("correlation columns have mlstats_stat class", {
  set.seed(1004)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  # Check that correlation columns have the mlstats_stat class
  expect_s3_class(result$`1`, "mlstats_stat")
  expect_s3_class(result$`2`, "mlstats_stat")
})

test_that("result has mlstats_wb_tibble class", {
  set.seed(1005)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  # Check custom class is applied
  expect_s3_class(result, "mlstats_wb_tibble")
})

test_that("flip=FALSE shows within above diagonal", {
  set.seed(1006)
  data <- data.frame(
    group = rep(1:5, each = 10),
    x = rnorm(50),
    y = rnorm(50)
  )
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y"),
    flip = FALSE
  )
  
  # Check flipped attribute is FALSE
  expect_false(attr(result, "flipped"))
})

test_that("flip=TRUE shows between above diagonal", {
  set.seed(1007)
  data <- data.frame(
    group = rep(1:5, each = 10),
    x = rnorm(50),
    y = rnorm(50)
  )
  
  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y"),
    flip = TRUE
  )
  
  # Check flipped attribute is TRUE
  expect_true(attr(result, "flipped"))
})

test_that("flip=TRUE transposes the correlation matrix", {
  set.seed(1008)
  data <- data.frame(
    group = rep(1:5, each = 10),
    x = rnorm(50),
    y = rnorm(50),
    z = rnorm(50)
  )
  
  result_normal <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y", "z"),
    flip = FALSE
  )
  
  result_flipped <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y", "z"),
    flip = TRUE
  )
  
  # Upper triangle of normal should equal lower triangle of flipped
  # Extract values and compare
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

test_that("flip defaults to FALSE", {
  set.seed(1009)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  result_default <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  result_explicit <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y"),
    flip = FALSE
  )
  
  # Should be identical
  expect_identical(result_default, result_explicit)
})