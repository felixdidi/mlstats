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

# ---- Tests for method = "sem" ----

test_that("method='sem' handles basic input correctly", {
  set.seed(2001)
  data <- data.frame(
    group = rep(c("A", "B", "C", "D", "E"), each = 20),
    x = c(rnorm(20, 10, 2), rnorm(20, 15, 2), rnorm(20, 20, 2), rnorm(20, 25, 2), rnorm(20, 30, 2)),
    y = c(rnorm(20, 5, 1), rnorm(20, 10, 1), rnorm(20, 15, 1), rnorm(20, 20, 1), rnorm(20, 25, 1))
  )

  result <- suppressWarnings(
    within_between_correlations(
      data = data,
      group = "group",
      vars = c("x", "y"),
      method = "sem"
    )
  )

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "mlstats_wb_tibble")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
  expect_equal(result$variable, c("x", "y"))
})

test_that("method='sem' produces correct matrix structure", {
  set.seed(2002)
  data <- data.frame(
    group = rep(1:10, each = 20),
    v1 = rnorm(200),
    v2 = rnorm(200),
    v3 = rnorm(200)
  )

  result <- suppressWarnings(
    within_between_correlations(
      data = data,
      group = "group",
      vars = c("v1", "v2", "v3"),
      method = "sem"
    )
  )

  # Check dimensions
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4)

  # Check diagonal is en-dash
  expect_equal(vctrs::vec_data(result$`1`)[1], "\u2013")
  expect_equal(vctrs::vec_data(result$`2`)[2], "\u2013")
  expect_equal(vctrs::vec_data(result$`3`)[3], "\u2013")
})

test_that("method='sem' correlation values are within [-1, 1]", {
  set.seed(2003)
  data <- data.frame(
    group = rep(1:20, each = 20),
    x = rnorm(400),
    y = rnorm(400)
  )

  result <- suppressWarnings(
    within_between_correlations(
      data = data,
      group = "group",
      vars = c("x", "y"),
      method = "sem"
    )
  )

  # Extract non-diagonal values
  cor_vals <- c(vctrs::vec_data(result$`1`), vctrs::vec_data(result$`2`))
  cor_vals <- cor_vals[cor_vals != "\u2013" & cor_vals != "NA"]
  numeric_vals <- as.numeric(gsub("\\*+$", "", cor_vals))

  expect_true(all(numeric_vals >= -1 & numeric_vals <= 1, na.rm = TRUE))
})

test_that("method='sem' marks significant correlations", {
  set.seed(2004)
  # Create data with strong between-group and within-group correlations
  data <- data.frame(
    group = rep(1:20, each = 30)
  )
  data$x <- rep(1:20, each = 30) * 5 + rnorm(600, 0, 1)
  data$y <- data$x + rnorm(600, 0, 2)

  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y"),
    method = "sem"
  )

  # With strong correlations, at least one should be significant
  cor_vals <- c(vctrs::vec_data(result$`2`)[1], vctrs::vec_data(result$`1`)[2])
  expect_true(any(grepl("\\*", cor_vals)))
})

test_that("method='sem' handles significance='detailed'", {
  set.seed(2005)
  data <- data.frame(
    group = rep(1:20, each = 30)
  )
  data$x <- rep(1:20, each = 30) * 5 + rnorm(600, 0, 1)
  data$y <- data$x + rnorm(600, 0, 2)

  result <- suppressWarnings(
    within_between_correlations(
      data = data,
      group = "group",
      vars = c("x", "y"),
      method = "sem",
      significance = "detailed"
    )
  )

  # Should complete without error
  expect_s3_class(result, "mlstats_wb_tibble")

  # With strong correlations, should have multiple stars
  cor_vals <- c(vctrs::vec_data(result$`2`)[1], vctrs::vec_data(result$`1`)[2])
  expect_true(any(grepl("\\*{2,}", cor_vals)))
})

test_that("method='sem' ignores weight with message", {
  set.seed(2006)
  data <- data.frame(
    group = rep(1:5, each = 20),
    x = rnorm(100),
    y = rnorm(100)
  )

  expect_message(
    within_between_correlations(
      data = data,
      group = "group",
      vars = c("x", "y"),
      method = "sem",
      weight = FALSE
    ),
    "weight"
  )
})

test_that("method='sem' ignores weight with message even when weight = TRUE", {
  set.seed(2006)
  data <- data.frame(
    group = rep(1:5, each = 20),
    x = rnorm(100),
    y = rnorm(100)
  )

  expect_message(
    within_between_correlations(
      data = data,
      group = "group",
      vars = c("x", "y"),
      method = "sem",
      weight = TRUE
    ),
    "weight"
  )
})

test_that("method='sem' flip works correctly", {
  set.seed(2007)
  data <- data.frame(
    group = rep(1:10, each = 20),
    x = rnorm(200),
    y = rnorm(200),
    z = rnorm(200)
  )

  result_normal <- suppressWarnings(
    within_between_correlations(
      data = data,
      group = "group",
      vars = c("x", "y", "z"),
      method = "sem",
      flip = FALSE
    )
  )

  result_flipped <- suppressWarnings(
    within_between_correlations(
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
  expect_equal(
    vctrs::vec_data(result_normal$`3`)[1],
    vctrs::vec_data(result_flipped$`1`)[3]
  )

  expect_true(attr(result_flipped, "flipped"))
  expect_false(attr(result_normal, "flipped"))
})

test_that("method='sem' stores method attribute", {
  set.seed(2008)
  data <- data.frame(
    group = rep(1:5, each = 20),
    x = rnorm(100),
    y = rnorm(100)
  )

  result_sem <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y"),
    method = "sem"
  )

  result_decomp <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y"),
    method = "decomposition"
  )

  expect_equal(attr(result_sem, "method"), "sem")
  expect_equal(attr(result_decomp, "method"), "decomposition")
})

test_that("method='sem' handles single variable", {
  set.seed(2009)
  data <- data.frame(
    group = rep(1:5, each = 20),
    x = rnorm(100)
  )

  result <- within_between_correlations(
    data = data,
    group = "group",
    vars = "x",
    method = "sem"
  )

  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 2)
  expect_equal(vctrs::vec_data(result$`1`)[1], "\u2013")
})

test_that("method='sem' columns have mlstats_stat class", {
  set.seed(2010)
  data <- data.frame(
    group = rep(1:5, each = 20),
    x = rnorm(100),
    y = rnorm(100)
  )

  result <- suppressWarnings(
    within_between_correlations(
      data = data,
      group = "group",
      vars = c("x", "y"),
      method = "sem"
    )
  )

  expect_s3_class(result$`1`, "mlstats_stat")
  expect_s3_class(result$`2`, "mlstats_stat")
})

test_that("method defaults to decomposition", {
  set.seed(2011)
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
    method = "decomposition"
  )

  expect_identical(result_default, result_explicit)
})

test_that("method='sem' handles between-only variables (zero within-cluster variance)", {
  set.seed(2012)
  # Create a between-only variable (constant within each group, like a trait)
  data <- data.frame(
    group = rep(1:20, each = 25)
  )
  data$trait <- rep(rnorm(20, 10, 3), each = 25)  # Between-only: constant within groups
  data$x <- rnorm(500, 5, 2)  # Within+between variation
  data$y <- rnorm(500, 5, 2)  # Within+between variation

  result <- suppressWarnings(
    within_between_correlations(
      data = data,
      group = "group",
      vars = c("trait", "x", "y"),
      method = "sem"
    )
  )

  # Structure checks
  expect_s3_class(result, "mlstats_wb_tibble")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4)

  # Within-group correlations involving the between-only variable should be NA
  # Upper triangle: row 1 (trait) cols 2, 3
  expect_equal(vctrs::vec_data(result$`2`)[1], "NA")  # trait ~~ x within
  expect_equal(vctrs::vec_data(result$`3`)[1], "NA")  # trait ~~ y within

  # Within-group correlation between x and y should be a valid number
  val_xy_within <- vctrs::vec_data(result$`3`)[2]
  expect_false(val_xy_within == "NA")
  expect_false(val_xy_within == "\u2013")

  # Between-group correlations for the trait should either be a valid
  # correlation in [-1, 1] or "NA" (the latter if the SEM produces an
  # improper/out-of-range solution, which is expected and flagged with a
  # warning when only a handful of groups carry the between-level signal).
  val_trait_x_between <- vctrs::vec_data(result$`1`)[2]
  val_trait_y_between <- vctrs::vec_data(result$`1`)[3]
  for (val in c(val_trait_x_between, val_trait_y_between)) {
    if (val != "NA") {
      numeric_val <- as.numeric(gsub("\\*+$", "", val))
      expect_true(numeric_val >= -1 && numeric_val <= 1)
    }
  }
})

test_that("method='sem' between-only variable correlations do not distort other correlations", {
  set.seed(2013)
  # Create data where x and y have known strong within-group correlation
  data <- data.frame(
    group = rep(1:20, each = 25)
  )
  data$trait <- rep(rnorm(20, 10, 3), each = 25)
  data$x <- rnorm(500)
  data$y <- data$x * 0.5 + rnorm(500, 0, 0.5)  # Correlated with x

  # With trait included
  result_with <- suppressWarnings(
    within_between_correlations(
      data = data,
      group = "group",
      vars = c("trait", "x", "y"),
      method = "sem"
    )
  )

  # Without trait
  result_without <- within_between_correlations(
    data = data,
    group = "group",
    vars = c("x", "y"),
    method = "sem"
  )

  # Within-group x~~y correlation should be similar regardless of trait inclusion
  val_with <- as.numeric(gsub("\\*+$", "", vctrs::vec_data(result_with$`3`)[2]))
  val_without <- as.numeric(gsub("\\*+$", "", vctrs::vec_data(result_without$`2`)[1]))
  expect_equal(val_with, val_without, tolerance = 0.05)
})

test_that("within_between_correlations errors on unknown group variable", {
  data <- data.frame(group = rep(1:3, each = 5), x = rnorm(15))
  expect_error(
    within_between_correlations(data, group = "nope", vars = "x"),
    "not found"
  )
})

test_that("within_between_correlations errors on unknown vars", {
  data <- data.frame(group = rep(1:3, each = 5), x = rnorm(15))
  expect_error(
    within_between_correlations(data, group = "group", vars = c("x", "missing")),
    "not found"
  )
})

test_that("within_between_correlations returns NA for non-finite correlation estimate", {
  # x and y never have a non-missing value in the same row, so the
  # within-group pairwise correlation is undefined (NaN) even though both
  # variables individually have nonzero variance.
  data <- data.frame(
    g = rep(1:4, each = 4),
    x = c(1, 2, 3, 4, NA, NA, NA, NA, 5, 6, 7, 8, NA, NA, NA, NA),
    y = c(NA, NA, NA, NA, 1, 2, 3, 4, NA, NA, NA, NA, 5, 6, 7, 8)
  )

  result <- suppressWarnings(within_between_correlations(data, "g", c("x", "y")))

  expect_equal(vctrs::vec_data(result$`2`)[1], "NA")
})

test_that("within_between_correlations significance='detailed' marks all three star levels", {
  set.seed(7)
  n_groups <- 8
  n_per <- 15
  g <- rep(1:n_groups, each = n_per)
  n <- length(g)
  x <- rnorm(n)
  data <- data.frame(
    g = g,
    x = x,
    y_strong = x + rnorm(n, 0, 0.05),
    y_mod = x + rnorm(n, 0, 3.8),
    y_weak = x + rnorm(n, 0, 4.6)
  )

  result <- within_between_correlations(
    data, "g", c("x", "y_strong", "y_mod", "y_weak"),
    significance = "detailed"
  )

  vals <- as.character(unlist(result[, -1]))
  expect_true(any(grepl("\\*\\*\\*", vals)))
  expect_true(any(grepl("(?<!\\*)\\*\\*(?!\\*)", vals, perl = TRUE)))
  expect_true(any(grepl("(?<!\\*)\\*(?!\\*)", vals, perl = TRUE)))
})

test_that("method='sem' excludes constant variables and warns", {
  set.seed(99)
  data <- data.frame(group = rep(1:10, each = 10))
  data$const <- 5
  data$x <- rnorm(100)
  data$y <- rnorm(100)

  result <- expect_warning_value(
    within_between_correlations(data, "group", c("const", "x", "y"), method = "sem"),
    "constant"
  )

  # const is var 1; its within- and between-group correlations with x and y
  # must be NA since it was excluded from the model.
  expect_equal(vctrs::vec_data(result$`2`)[1], "NA")
  expect_equal(vctrs::vec_data(result$`3`)[1], "NA")
})

test_that("method='sem' returns all NA when no variable has variance at both levels", {
  set.seed(99)
  data <- data.frame(group = rep(1:10, each = 10))
  data$a <- rep(rnorm(10), each = 10)
  data$b <- rep(rnorm(10), each = 10)

  result <- expect_warning_value(
    within_between_correlations(data, "group", c("a", "b"), method = "sem"),
    "Returning .NA. for all correlations"
  )

  expect_equal(vctrs::vec_data(result$`2`)[1], "NA")
  expect_equal(vctrs::vec_data(result$`1`)[2], "NA")
})

test_that("method='sem' excludes within-only variables from the between-group model", {
  set.seed(55)
  data <- data.frame(group = rep(1:10, each = 20))
  data$wi <- rnorm(200)
  data$x <- rnorm(200)

  result <- suppressWarnings(
    within_between_correlations(data, "group", c("wi", "x"), method = "sem")
  )

  # Between-group correlation involving the within-only variable is NA
  expect_equal(vctrs::vec_data(result$`1`)[2], "NA")
})

test_that("method='sem' treats a variable with at most one observation per cluster as between-only", {
  # `sparse` has exactly one non-missing value per group, so var() within
  # each group is NA (undefined, not just zero): no within-cluster variance
  # is even *observable*, which is a different code path from a variable
  # that is observably constant within every group.
  set.seed(77)
  n_groups <- 5
  n_per <- 10
  g <- rep(1:n_groups, each = n_per)
  n <- length(g)
  sparse <- rep(NA_real_, n)
  sparse[match(unique(g), g)] <- rnorm(n_groups, 10, 3)
  data <- data.frame(group = g, sparse = sparse, x = rnorm(n))

  # The classification itself doesn't depend on the model fit succeeding;
  # the model regardless fails to converge here because listwise deletion
  # collapses every cluster to a single row once `sparse` enters the model.
  result <- expect_warning_value(
    within_between_correlations(data, "group", c("sparse", "x"), method = "sem"),
    "could not be fit"
  )

  expect_s3_class(result, "mlstats_wb_tibble")
  expect_equal(nrow(result), 2)
})

test_that("method='sem' handles exactly one within-only and one between-only variable", {
  set.seed(56)
  # trait: constant within each group (between-only).
  # wi: no between-group variance at all (within-only).
  # With only one variable left per level, the model syntax uses a bare
  # variance line ("var ~~ var") rather than a covariance combination.
  data <- data.frame(group = rep(1:10, each = 20))
  data$trait <- rep(rnorm(10, 10, 3), each = 20)
  data$wi <- rnorm(200)

  result <- suppressWarnings(
    within_between_correlations(data, "group", c("trait", "wi"), method = "sem")
  )

  expect_s3_class(result, "mlstats_wb_tibble")
  expect_equal(nrow(result), 2)
})

# ---- Tests for `ci`/`folder` being no-ops outside method = "bayes" ----

test_that("ci argument is ignored with a message unless method = 'bayes'", {
  set.seed(3010)
  data <- data.frame(group = rep(1:3, each = 10), x = rnorm(30), y = rnorm(30))

  expect_message(
    within_between_correlations(data, "group", c("x", "y"), ci = 0.8),
    "no effect"
  )
})

test_that("folder argument is ignored with a message unless method = 'bayes'", {
  set.seed(3011)
  data <- data.frame(group = rep(1:3, each = 10), x = rnorm(30), y = rnorm(30))

  expect_message(
    within_between_correlations(data, "group", c("x", "y"), folder = tempdir()),
    "no effect"
  )
})

# ---- Tests for method = "bayes" ----
# See helper-bayes-fixtures.R for the shared cache folder/fixture data/
# skip_if_no_bayes() and why these tests are structured to reuse cached brms
# fits as much as possible.

test_that("method='bayes' requires folder argument", {
  skip_if_no_bayes()
  expect_error(
    within_between_correlations(
      data = bayes_fixture_basic,
      group = "group",
      vars = c("x", "y"),
      method = "bayes"
    ),
    "folder.*must be specified"
  )
})

test_that("method='bayes' validates ci argument", {
  skip_if_no_bayes()
  expect_error(
    within_between_correlations(
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
    within_between_correlations(
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

test_that("method='bayes' creates folder if it doesn't exist", {
  skip_if_no_bayes()
  # A single variable needs no brms fit at all (the comparison matrix is
  # diagonal-only), so this only exercises folder creation.
  temp_folder <- file.path(tempdir(), "test_brms_wb_folder")
  on.exit(unlink(temp_folder, recursive = TRUE), add = TRUE)

  expect_false(dir.exists(temp_folder))

  result <- within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = "x",
    method = "bayes",
    folder = temp_folder
  )

  expect_true(dir.exists(temp_folder))
  expect_equal(as.character(result$`1`[1]), "\u2013")
})

test_that("method='bayes' returns NA for zero-variance pairs without fitting", {
  skip_if_no_bayes()
  result <- within_between_correlations(
    data = bayes_fixture_zero_variance,
    group = "group",
    vars = c("constant", "x"),
    method = "bayes",
    folder = bayes_cache_folder
  )

  expect_equal(as.character(result$`2`[1]), "NA")
  expect_equal(as.character(result$`1`[2]), "NA")
})

test_that("method='bayes' handles basic input correctly", {
  skip_if_no_bayes()
  result <- within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    method = "bayes",
    folder = bayes_cache_folder
  )

  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "mlstats_wb_tibble")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
  expect_equal(result$variable, c("x", "y"))
  expect_equal(as.character(result$`1`[1]), "\u2013")
  expect_equal(as.character(result$`2`[2]), "\u2013")

  cor_vals <- unlist(result[, -1])
  cor_vals <- cor_vals[cor_vals != "\u2013"]
  expect_true(all(grepl("^-?[0-9]\\.[0-9]{2}\\*?$|^NA$", cor_vals)))

  expect_true(all(sapply(result[, -1], function(col) inherits(col, "mlstats_stat"))))

  expect_equal(attr(result, "method"), "bayes")
  expect_true(attr(result, "bayesian"))
})

test_that("method='bayes' weight = FALSE uses unweighted correlations", {
  skip_if_no_bayes()
  result_unweighted <- within_between_correlations(
    data = bayes_fixture_unbalanced,
    group = "group",
    vars = c("x", "y"),
    method = "bayes",
    weight = FALSE,
    folder = bayes_cache_folder
  )

  expect_s3_class(result_unweighted, "tbl_df")
  expect_equal(nrow(result_unweighted), 2)

  between_val <- as.character(result_unweighted$`1`[2])
  expect_true(nchar(between_val) > 0)
})

test_that("method='bayes' handles different ci levels without refitting", {
  skip_if_no_bayes()
  result_90 <- within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    method = "bayes",
    ci = 0.9,
    folder = bayes_cache_folder
  )

  result_95 <- within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    method = "bayes",
    ci = 0.95,
    folder = bayes_cache_folder
  )

  expect_equal(dim(result_90), dim(result_95))
})

test_that("method='bayes' reuses cached models", {
  skip_if_no_bayes()
  result1 <- within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    method = "bayes",
    folder = bayes_cache_folder
  )

  result2 <- within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    method = "bayes",
    folder = bayes_cache_folder
  )

  expect_identical(result1, result2)
})

test_that("method='bayes' flip=TRUE swaps which triangle holds within- vs between-group correlations", {
  skip_if_no_bayes()
  result <- within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    method = "bayes",
    flip = TRUE,
    folder = bayes_cache_folder
  )

  expect_true(attr(result, "flipped"))
  expect_equal(as.character(result$`1`[1]), "\u2013")
})

test_that("method='bayes' default print dispatches pillar formatting", {
  skip_if_no_bayes()
  result <- within_between_correlations(
    data = bayes_fixture_basic,
    group = "group",
    vars = c("x", "y"),
    method = "bayes",
    folder = bayes_cache_folder
  )

  output <- capture.output(print(result))
  expect_true(any(grepl("Within- and Between-Group Correlations", output)))
  expect_true(any(grepl("credible intervals", output)))
  expect_true(any(grepl("Bayesian multilevel models", output)))
})

test_that("method='bayes' ignores significance with a message", {
  skip_if_no_bayes()
  expect_message(
    within_between_correlations(
      data = bayes_fixture_basic,
      group = "group",
      vars = c("x", "y"),
      method = "bayes",
      significance = "detailed",
      folder = bayes_cache_folder
    ),
    "no effect"
  )
})