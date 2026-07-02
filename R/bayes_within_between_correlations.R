#' Compute Bayesian Within-Group and Between-Group Correlations
#'
#' In data with a grouping structure (e.g., repeated measurements per person, or
#' students nested within schools), a single correlation between two variables can
#' be misleading, because it mixes two different relationships: how the variables
#' relate \emph{within} each group (e.g., do a person's good days also tend to be
#' their productive days?), and how they relate \emph{between} groups (e.g., do
#' people who are generally happier also tend to be generally more productive?).
#' This function estimates both relationships separately using Bayesian
#' multivariate models fit via \code{brms}, reporting credible intervals instead
#' of p-values. See \code{\link{within_between_correlations}} for the frequentist
#' equivalent, and Details and \code{vignette("correlation-methods")} for the
#' statistical background shared by both.
#'
#' @param data A data frame containing the variables to analyze.
#' @param group A character string specifying the name of the grouping variable.
#' @param vars A character vector specifying the names of variables to correlate.
#' @param weight Logical. If TRUE (default), the between-group correlation gives
#'   more weight to larger groups. If FALSE, every group counts equally
#'   regardless of size. See Details.
#' @param ci Numeric value strictly between 0 and 1 specifying the credible
#'   interval width. Default is 0.9 (90% CI).
#' @param folder Character string specifying the directory path where brms models
#'   should be saved. No default; must be specified.
#' @param flip Logical. If TRUE, between-group correlations are shown in the upper
#'   triangle and within-group correlations in the lower triangle. Default is FALSE.
#'
#' @return A tibble containing a correlation matrix where:
#' \itemize{
#'   \item The upper triangle contains within-group correlations
#'   \item The lower triangle contains between-group correlations
#'   \item Diagonal elements are marked with "–"
#'   \item Correlations whose credible interval excludes zero are marked with an asterisk
#' }
#'
#' @details
#' This function computes the within-group correlation by first subtracting each
#' group's mean from every observation, then correlating the resulting deviation
#' scores via a Bayesian multivariate model (\code{brms::brm()} with
#' \code{set_rescor(TRUE)}); this mirrors \code{method = "decomposition"} in
#' \code{\link{within_between_correlations}}, but with credible intervals
#' (whether they exclude zero) in place of p-values.
#'
#' The between-group correlation is handled differently depending on
#' \code{weight}:
#' \itemize{
#'   \item If \code{weight = TRUE} (default): the correlation point estimate
#'     (posterior median) is obtained from a model fit on group means replicated
#'     for each observation (implicitly weighting by group size). The credible
#'     interval, however, is always obtained from a model fit on unique group
#'     means only, so that uncertainty reflects the actual number of groups
#'     rather than the total sample size.
#'   \item If \code{weight = FALSE}: both the correlation estimate and the
#'     credible interval are obtained from a model fit on unique group means.
#'     Each group contributes equally regardless of size.
#' }
#'
#' Unlike \code{method = "sem"} in \code{\link{within_between_correlations}},
#' this function has no automatic variable classification: every variable in
#' \code{vars} is modeled at both levels.
#'
#' Models are saved to \code{folder} for caching. Each pair of variables
#' requires its own \code{brms::brm} fit, and when \code{weight = TRUE} each
#' between-group pair requires two fits (one for the point estimate, one for
#' the credible interval). For \code{p} variables this means
#' \code{p * (p - 1) / 2} within-group fits plus \code{p * (p - 1)} (or
#' \code{p * (p - 1) / 2} if \code{weight = FALSE}) between-group fits — e.g.
#' 4 variables means 6 within-group and 12 (or 6) between-group fits. This can
#' take a long time for larger numbers of variables.
#'
#' @examples
#' \donttest{
#' data("media_diary")
#'
#' # Compute Bayesian within and between correlations (weighted)
#' result <- bayes_within_between_correlations(
#'   data = media_diary,
#'   group = "person",
#'   vars = c("wellbeing", "screen_time"),
#'   ci = 0.9,
#'   folder = tempdir()
#' )
#'
#' # Compute unweighted between-group correlations
#' result_unweighted <- bayes_within_between_correlations(
#'   data = media_diary,
#'   group = "person",
#'   vars = c("wellbeing", "screen_time"),
#'   weight = FALSE,
#'   ci = 0.9,
#'   folder = tempdir()
#' )
#' }
#'
#' @seealso \code{\link{within_between_correlations}} for the frequentist version
#'
#' @export
bayes_within_between_correlations <- function(
  data,
  group,
  vars,
  weight = TRUE,
  ci = 0.9,
  folder,
  flip = FALSE
) {
  # Check that brms is installed
  rlang::check_installed(
    "brms",
    reason = "to fit Bayesian within and between group correlations."
  )

  # Validate inputs
  .validate_group_vars(data, group, vars)
  if (base::missing(folder)) {
    base::stop("Argument 'folder' must be specified to save brms models.")
  }
  if (!base::dir.exists(folder)) {
    base::dir.create(folder, recursive = TRUE)
  }
  if (ci <= 0 || ci >= 1) {
    base::stop("Argument 'ci' must be between 0 and 1.")
  }

  # brms::brm(file = ...) caches purely on filename, so a content hash of the
  # relevant data (plus the sampling settings, which also affect the fit) is
  # folded into every cache filename below. Otherwise, re-running with
  # different data, or different options(mlstats.brms_iter/chains = ...),
  # but the same `vars`/`group`/`folder` would silently reload a stale
  # cached fit instead of refitting.
  data_hash <- rlang::hash(base::list(data[base::c(group, vars)], .brms_iter(), .brms_chains()))

  # Calculate quantiles for CI
  alpha <- (1 - ci) / 2
  ci_low <- alpha
  ci_high <- 1 - alpha

  d_centered <- decompose_within_between(
    dplyr::select(data, dplyr::all_of(base::c(group, vars))),
    group = group,
    vars = vars,
    components = base::c("between", "within"),
    between_pattern = "{col}_between",
    within_pattern = "{col}_within"
  )

  # One row per group — used for unweighted CI and for the significance test
  d_between_unweighted <- d_centered |>
    dplyr::distinct(!!rlang::sym(group), .keep_all = TRUE)

  # Prepare data for between-group point estimates
  d_between <- if (weight) d_centered else d_between_unweighted

  # Initialize comparison matrix
  n <- base::length(vars)
  comparison_matrix <- base::matrix("", nrow = n, ncol = n)

  # Compute correlations
  for (i in base::seq_along(vars)) {
    for (j in base::seq_along(vars)) {
      if (i == j) {
        comparison_matrix[i, j] <- "\u2013"
      } else if (i < j) {
        # Within-group correlation
        within_x <- base::paste0(vars[i], "_within")
        within_y <- base::paste0(vars[j], "_within")

        # Check for zero variance
        if (
          stats::sd(d_centered[[within_x]], na.rm = TRUE) == 0 ||
            stats::sd(d_centered[[within_y]], na.rm = TRUE) == 0
        ) {
          comparison_matrix[i, j] <- "NA"
        } else {
          model_file <- base::file.path(
            folder,
            base::paste0("within_", vars[i], "__", vars[j], "_", data_hash)
          )

          fit <- base::suppressWarnings(
            brms::brm(
              brms::bf(stats::as.formula(base::paste0(
                "brms::mvbind(",
                within_x,
                ", ",
                within_y,
                ") ~ 1"
              ))) +
                brms::set_rescor(rescor = TRUE),
              seed = 42,
              iter = .brms_iter(),
              chains = .brms_chains(),
              data = d_centered,
              file = model_file,
              silent = 2,
              refresh = 0
            )
          )

          draws <- fit |>
            brms::as_draws_df() |>
            dplyr::summarise(
              dplyr::across(
                dplyr::starts_with("rescor"),
                base::list(
                  Median = ~ stats::median(.x, na.rm = TRUE),
                  CI_low = ~ stats::quantile(.x, ci_low, na.rm = TRUE),
                  CI_high = ~ stats::quantile(.x, ci_high, na.rm = TRUE)
                ),
                .names = "{.fn}"
              )
            )

          # Check if CI excludes zero
          is_credible <- base::sign(draws$CI_high) + base::sign(draws$CI_low) != 0

          label <- base::sprintf("%.2f", draws$Median)
          if (is_credible) {
            label <- base::paste0(label, "*")
          }
          comparison_matrix[i, j] <- label
        }
      } else {
        # Between-group correlation
        between_x <- base::paste0(vars[i], "_between")
        between_y <- base::paste0(vars[j], "_between")

        # Check for zero variance
        if (
          stats::sd(d_between_unweighted[[between_x]], na.rm = TRUE) == 0 ||
            stats::sd(d_between_unweighted[[between_y]], na.rm = TRUE) == 0
        ) {
          comparison_matrix[i, j] <- "NA"
        } else {
          if (weight) {
            # Fit unweighted model for credible intervals
            model_file_unweighted <- base::file.path(
              folder,
              base::paste0("between_", vars[i], "__", vars[j], "_unweighted_", data_hash)
            )

            fit_unweighted <- base::suppressWarnings(
              brms::brm(
                brms::bf(stats::as.formula(base::paste0(
                  "brms::mvbind(",
                  between_x,
                  ", ",
                  between_y,
                  ") ~ 1"
                ))) +
                  brms::set_rescor(rescor = TRUE),
                seed = 42,
                iter = .brms_iter(),
                chains = .brms_chains(),
                data = d_between_unweighted,
                file = model_file_unweighted,
                silent = 2,
                refresh = 0
              )
            )

            draws_unweighted <- fit_unweighted |>
              brms::as_draws_df() |>
              dplyr::summarise(
                dplyr::across(
                  dplyr::starts_with("rescor"),
                  base::list(
                    CI_low = ~ stats::quantile(.x, ci_low, na.rm = TRUE),
                    CI_high = ~ stats::quantile(.x, ci_high, na.rm = TRUE)
                  ),
                  .names = "{.fn}"
                )
              )

            # Fit weighted model for point estimate
            model_file_weighted <- base::file.path(
              folder,
              base::paste0("between_", vars[i], "__", vars[j], "_weighted_", data_hash)
            )

            fit_weighted <- base::suppressWarnings(
              brms::brm(
                brms::bf(stats::as.formula(base::paste0(
                  "brms::mvbind(",
                  between_x,
                  ", ",
                  between_y,
                  ") ~ 1"
                ))) +
                  brms::set_rescor(rescor = TRUE),
                seed = 42,
                iter = .brms_iter(),
                chains = .brms_chains(),
                data = d_centered,
                file = model_file_weighted,
                silent = 2,
                refresh = 0
              )
            )

            draws_weighted <- fit_weighted |>
              brms::as_draws_df() |>
              dplyr::summarise(
                dplyr::across(
                  dplyr::starts_with("rescor"),
                  base::list(
                    Median = ~ stats::median(.x, na.rm = TRUE)
                  ),
                  .names = "{.fn}"
                )
              )

            # Use weighted estimate but unweighted CI
            is_credible <- base::sign(draws_unweighted$CI_high) +
              base::sign(draws_unweighted$CI_low) !=
              0
            label <- base::sprintf("%.2f", draws_weighted$Median)
          } else {
            # For unweighted, both estimate and CI from unweighted model
            model_file <- base::file.path(
              folder,
              base::paste0("between_", vars[i], "__", vars[j], "_unweighted_", data_hash)
            )

            fit <- base::suppressWarnings(
              brms::brm(
                brms::bf(stats::as.formula(base::paste0(
                  "brms::mvbind(",
                  between_x,
                  ", ",
                  between_y,
                  ") ~ 1"
                ))) +
                  brms::set_rescor(rescor = TRUE),
                seed = 42,
                iter = .brms_iter(),
                chains = .brms_chains(),
                data = d_between_unweighted,
                file = model_file,
                silent = 2,
                refresh = 0
              )
            )

            draws <- fit |>
              brms::as_draws_df() |>
              dplyr::summarise(
                dplyr::across(
                  dplyr::starts_with("rescor"),
                  base::list(
                    Median = ~ stats::median(.x, na.rm = TRUE),
                    CI_low = ~ stats::quantile(.x, ci_low, na.rm = TRUE),
                    CI_high = ~ stats::quantile(.x, ci_high, na.rm = TRUE)
                  ),
                  .names = "{.fn}"
                )
              )

            is_credible <- base::sign(draws$CI_high) + base::sign(draws$CI_low) != 0
            label <- base::sprintf("%.2f", draws$Median)
          }

          if (is_credible) {
            label <- base::paste0(label, "*")
          }
          comparison_matrix[i, j] <- label
        }
      }
    }
  }

  # Convert to tibble for output
  result_tibble <-
    dplyr::bind_cols(
      dplyr::tibble(variable = vars),
      dplyr::as_tibble(comparison_matrix, .name_repair = ~ base::as.character(base::c(1:n)))
    )

  # Flip matrix if requested
  if (flip) {
    comparison_matrix <- base::t(comparison_matrix)
    result_tibble <-
      dplyr::bind_cols(
        dplyr::tibble(variable = vars),
        dplyr::as_tibble(comparison_matrix, .name_repair = ~ base::as.character(base::c(1:n)))
      )
  }

  result_tibble <- result_tibble |>
    dplyr::mutate(
      dplyr::across(
        -variable,
        ~ vctrs::new_vctr(.x, class = "mlstats_stat", inherit_base_type = TRUE)
      )
    )

  class(result_tibble) <- c("mlstats_wb_tibble", class(result_tibble))
  base::attr(result_tibble, "flipped") <- flip

  ci_percent <- base::round(ci * 100)
  attr(result_tibble, "significance_note") <- base::paste0(
    "Correlations marked with a star have ",
    ci_percent,
    "% credible intervals that exclude zero."
  )

  attr(result_tibble, "bayesian") <- TRUE

  return(result_tibble)
}