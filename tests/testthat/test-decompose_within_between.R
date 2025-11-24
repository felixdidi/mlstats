test_that("decompose_within_between handles basic input correctly", {
  set.seed(123)
  data <- data.frame(
    participant = rep(1:5, each = 10),
    x = rnorm(50, 10, 2),
    y = rnorm(50, 15, 3)
  )
  
  result <- decompose_within_between(
    data = data,
    group = "participant",
    vars = c("x", "y")
  )
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 50)
  
  # Check that original columns are preserved
  expect_true(all(c("participant", "x", "y") %in% colnames(result)))
  
  # Check that decomposed columns exist
  expect_true("x_grand_mean_centered" %in% colnames(result))
  expect_true("x_between_participant" %in% colnames(result))
  expect_true("x_within_participant" %in% colnames(result))
  expect_true("y_grand_mean_centered" %in% colnames(result))
  expect_true("y_between_participant" %in% colnames(result))
  expect_true("y_within_participant" %in% colnames(result))
})

test_that("decompose_within_between computes grand mean centering correctly", {
  set.seed(456)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = c(rep(10, 10), rep(20, 10), rep(30, 10))
  )
  
  result <- decompose_within_between(
    data = data,
    group = "group",
    vars = "x"
  )
  
  # Grand mean should be 20
  # Grand mean centered values should be: -10, 0, 10
  expect_equal(mean(result$x_grand_mean_centered), 0, tolerance = 1e-10)
  expect_equal(result$x_grand_mean_centered[1], -10)
  expect_equal(result$x_grand_mean_centered[11], 0)
  expect_equal(result$x_grand_mean_centered[21], 10)
})

test_that("decompose_within_between computes between-group component correctly", {
  set.seed(789)
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 10),
    x = c(rep(5, 10), rep(15, 10), rep(25, 10))
  )
  
  result <- decompose_within_between(
    data = data,
    group = "group",
    vars = "x"
  )
  
  # Between-group values should be group means
  expect_equal(unique(result$x_between_group[result$group == "A"]), 5)
  expect_equal(unique(result$x_between_group[result$group == "B"]), 15)
  expect_equal(unique(result$x_between_group[result$group == "C"]), 25)
  
  # Between-group values should be constant within groups
  expect_equal(length(unique(result$x_between_group[result$group == "A"])), 1)
  expect_equal(length(unique(result$x_between_group[result$group == "B"])), 1)
  expect_equal(length(unique(result$x_between_group[result$group == "C"])), 1)
})

test_that("decompose_within_between computes within-group component correctly", {
  set.seed(111)
  data <- data.frame(
    group = rep(1:3, each = 5),
    x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
  )
  
  result <- decompose_within_between(
    data = data,
    group = "group",
    vars = "x"
  )
  
  # Within-group mean should be zero for each group
  within_means <- tapply(result$x_within_group, result$group, mean)
  expect_equal(as.numeric(within_means), c(0, 0, 0), tolerance = 1e-10)
  
  # Within-group variance should match original within-group variance
  expect_equal(var(result$x_within_group[result$group == 1]), var(data$x[data$group == 1]))
})

test_that("decompose_within_between handles multiple variables", {
  set.seed(222)
  data <- data.frame(
    school = rep(1:5, each = 20),
    math = rnorm(100),
    reading = rnorm(100),
    science = rnorm(100)
  )
  
  result <- decompose_within_between(
    data = data,
    group = "school",
    vars = c("math", "reading", "science")
  )
  
  # Check all decomposed columns exist for all variables
  expect_true(all(c(
    "math_grand_mean_centered", "math_between_school", "math_within_school",
    "reading_grand_mean_centered", "reading_between_school", "reading_within_school",
    "science_grand_mean_centered", "science_between_school", "science_within_school"
  ) %in% colnames(result)))
  
  # Check dimensions
  expect_equal(nrow(result), 100)
  expect_equal(ncol(result), 4 + 9)  # original 4 + 3 variables * 3 decompositions
})

test_that("decompose_within_between preserves original data", {
  set.seed(333)
  data <- data.frame(
    id = rep(1:10, each = 5),
    x = rnorm(50),
    y = rnorm(50),
    z = letters[1:50]
  )
  
  result <- decompose_within_between(
    data = data,
    group = "id",
    vars = c("x", "y")
  )
  
  # Original columns should be unchanged
  expect_equal(result$id, data$id)
  expect_equal(result$x, data$x)
  expect_equal(result$y, data$y)
  expect_equal(result$z, data$z)
})

test_that("decompose_within_between handles missing values", {
  set.seed(444)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30),
    y = rnorm(30)
  )
  
  # Introduce missing values
  data$x[c(1, 5, 15)] <- NA
  data$y[c(2, 10, 20)] <- NA
  
  result <- decompose_within_between(
    data = data,
    group = "group",
    vars = c("x", "y")
  )
  
  # Should complete without error
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 30)
  
  # Missing values should propagate appropriately
  expect_true(is.na(result$x_grand_mean_centered[1]))
  expect_true(is.na(result$y_grand_mean_centered[2]))
})

test_that("decompose_within_between throws error for missing group variable", {
  data <- data.frame(
    id = rep(1:5, each = 10),
    x = rnorm(50)
  )
  
  expect_error(
    decompose_within_between(data = data, group = "nonexistent", vars = "x"),
    "Group variable 'nonexistent' not found in data"
  )
})

test_that("decompose_within_between throws error for missing variables", {
  data <- data.frame(
    id = rep(1:5, each = 10),
    x = rnorm(50)
  )
  
  expect_error(
    decompose_within_between(data = data, group = "id", vars = c("x", "y", "z")),
    "Variables not found in data: y, z"
  )
})

test_that("decompose_within_between handles single variable", {
  set.seed(555)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30)
  )
  
  result <- decompose_within_between(
    data = data,
    group = "group",
    vars = "x"
  )
  
  # Check structure
  expect_equal(nrow(result), 30)
  expect_equal(ncol(result), 5)  # original 2 + 3 decompositions
  
  # Check decomposed columns exist
  expect_true(all(c(
    "x_grand_mean_centered",
    "x_between_group",
    "x_within_group"
  ) %in% colnames(result)))
})

test_that("decompose_within_between handles numeric group variable", {
  set.seed(666)
  data <- data.frame(
    group = rep(1:5, each = 10),
    x = rnorm(50)
  )
  
  result <- decompose_within_between(
    data = data,
    group = "group",
    vars = "x"
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 50)
})

test_that("decompose_within_between handles factor group variable", {
  set.seed(777)
  data <- data.frame(
    group = factor(rep(c("Low", "Med", "High"), each = 10)),
    x = rnorm(30)
  )
  
  result <- decompose_within_between(
    data = data,
    group = "group",
    vars = "x"
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 30)
})

test_that("decompose_within_between handles unbalanced groups", {
  set.seed(888)
  data <- data.frame(
    group = c(rep("A", 5), rep("B", 15), rep("C", 30)),
    x = rnorm(50)
  )
  
  result <- decompose_within_between(
    data = data,
    group = "group",
    vars = "x"
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 50)
  
  # Check that between-group values are correct for unbalanced groups
  expect_equal(unique(result$x_between_group[result$group == "A"]), mean(data$x[data$group == "A"]))
  expect_equal(unique(result$x_between_group[result$group == "B"]), mean(data$x[data$group == "B"]))
  expect_equal(unique(result$x_between_group[result$group == "C"]), mean(data$x[data$group == "C"]))
})

test_that("decompose_within_between maintains mathematical relationships", {
  set.seed(999)
  data <- data.frame(
    group = rep(1:10, each = 10),
    x = rnorm(100, 50, 10)
  )
  
  result <- decompose_within_between(
    data = data,
    group = "group",
    vars = "x"
  )
  
  # Original value = between + within
  reconstructed <- result$x_between_group + result$x_within_group
  expect_equal(reconstructed, result$x, tolerance = 1e-10)
  
})

test_that("decompose_within_between handles variables with zero variance within groups", {
  set.seed(1111)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rep(c(10, 20, 30), each = 10)  # No within-group variance
  )
  
  result <- decompose_within_between(
    data = data,
    group = "group",
    vars = "x"
  )
  
  # Within-group component should be zero
  expect_equal(result$x_within_group, rep(0, 30))
  
  # Between-group component should equal original values
  expect_equal(result$x_between_group, data$x)
})

test_that("decompose_within_between handles variables with zero variance between groups", {
  set.seed(1212)
  data <- data.frame(
    group = rep(1:3, each = 10),
    x = rnorm(30, 50, 5)
  )
  # Make all group means equal
  data$x <- data$x - ave(data$x, data$group, FUN = mean) + 50
  
  result <- decompose_within_between(
    data = data,
    group = "group",
    vars = "x"
  )
  
  # Between-group component should be constant (approximately 50)
  expect_equal(unique(result$x_between_group), 50, tolerance = 1e-10)
})

test_that("decompose_within_between handles character group variable", {
  set.seed(1313)
  data <- data.frame(
    group = rep(c("alpha", "beta", "gamma"), each = 10),
    x = rnorm(30)
  )
  
  result <- decompose_within_between(
    data = data,
    group = "group",
    vars = "x"
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 30)
})

test_that("decompose_within_between handles single observation per group", {
  set.seed(1414)
  data <- data.frame(
    group = 1:20,
    x = rnorm(20)
  )
  
  result <- decompose_within_between(
    data = data,
    group = "group",
    vars = "x"
  )
  
  # With single observations, within-group component should be zero
  expect_equal(result$x_within_group, rep(0, 20))
  
  # Between-group component should equal original values
  expect_equal(result$x_between_group, data$x)
})

test_that("decompose_within_between returns ungrouped data frame", {
  set.seed(1515)
  data <- data.frame(
    group = rep(1:5, each = 10),
    x = rnorm(50)
  )
  
  result <- decompose_within_between(
    data = data,
    group = "group",
    vars = "x"
  )
  
  # Should not be a grouped data frame
  expect_false(dplyr::is_grouped_df(result))
})

test_that("decompose_within_between column naming is consistent", {
  set.seed(1616)
  data <- data.frame(
    my_group = rep(1:3, each = 10),
    var1 = rnorm(30),
    var2 = rnorm(30)
  )
  
  result <- decompose_within_between(
    data = data,
    group = "my_group",
    vars = c("var1", "var2")
  )
  
  # Check naming pattern
  expect_true("var1_grand_mean_centered" %in% colnames(result))
  expect_true("var1_between_my_group" %in% colnames(result))
  expect_true("var1_within_my_group" %in% colnames(result))
  expect_true("var2_grand_mean_centered" %in% colnames(result))
  expect_true("var2_between_my_group" %in% colnames(result))
  expect_true("var2_within_my_group" %in% colnames(result))
})