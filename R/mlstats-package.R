#' @keywords internal
#'
#' @description
#' The **mlstats** package provides tools for multilevel descriptive statistics
#' and data preparation. It supports:
#'
#' * Decomposing variables into within-group and between-group components for
#'   Random Effects Within-Between (REWB) models
#'   ([decompose_within_between()])
#' * Computing within-group and between-group correlations
#'   ([within_between_correlations()])
#' * Creating publication-ready descriptive statistics tables with ICCs
#'   ([mldesc()])
#' * Frequentist (via lme4 or lavaan) and Bayesian (via brms) estimation,
#'   selectable via the `method` argument of [mldesc()] and
#'   [within_between_correlations()]
#'
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
