#' @keywords internal
#'
#' @description
#' The **mlstats** package provides tools for multilevel descriptive statistics
#' and data preparation (e.g., repeated measurements per person, or students
#' nested within schools). It supports:
#'
#' * Computing within-group and between-group correlations
#'   ([within_between_correlations()])
#' * Creating publication-ready descriptive statistics tables with ICCs and
#'   within-/between-group correlations ([mldesc()])
#' * Decomposing variables into within-group and between-group components for
#'   Random Effects Within-Between (REWB) models
#'   ([decompose_within_between()])
#' * Three estimation methods, selectable via the `method` argument of
#'   [mldesc()] and [within_between_correlations()]: variance decomposition
#'   (default), two-level structural equation modeling (via lavaan), and
#'   Bayesian multilevel modeling (via brms)
#' * Exporting result tables as 'gt' or 'tinytable' objects via
#'   `print(result, format = "gt")` or `print(result, format = "tt")`
#'
#' See `vignette("correlation-methods")` for the statistical background, and
#' [media_diary] for an example dataset.
#'
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
