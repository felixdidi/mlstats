# Evaluate `expr`, assert it emits a warning matching `regexp`, muffle that
# warning so it doesn't leak into test output, and return the value of `expr`.
# Used for the SEM-method tests, where lavaan models that fail to fit cleanly
# or produce out-of-range standardized solutions emit an intentional warning
# (see within_between_correlations.R's .wb_cor_sem()) rather than silently
# returning a bad value.
expect_warning_value <- function(expr, regexp) {
  matched <- FALSE
  result <- withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl(regexp, conditionMessage(w))) {
        matched <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )
  testthat::expect_true(
    matched,
    label = paste0("a warning matching '", regexp, "'")
  )
  result
}
