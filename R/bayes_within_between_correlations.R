#' Compute Bayesian Within-Group and Between-Group Correlations
#'
#' This function decomposes observed correlations between variables into within-group
#' and between-group components using Bayesian estimation via brms. Similar to
#' \code{\link{within_between_correlations}}, but uses Bayesian inference with
#' credible intervals instead of frequentist p-values.
#'
#' @param data A data frame containing the variables to analyze.
#' @param group A character string specifying the name of the grouping variable.
#' @param vars A character vector specifying the names of variables to correlate.
#' @param weight Logical. If TRUE (default), between-group correlations are weighted by group size.
#'   If FALSE, each group contributes equally (unweighted group means).
#' @param ci Numeric value between 0 and 1 specifying the credible interval width
#'   (default = 0.9 for 90% CI).
#' @param folder Character string specifying the directory path where brms models
#'   should be saved. No default; must be specified.
#'
#' @return A tibble containing a correlation matrix where:
#' \itemize{
#'   \item The upper triangle contains within-group correlations
#'   \item The lower triangle contains between-group correlations
#'   \item Diagonal elements are marked with "-"
#'   \item Credible correlations (CI excludes zero) are marked with "*"
#' }
#'
#' @details
#' This function uses brms to estimate correlations via multivariate models with
#' \code{set_rescor(TRUE)}. For each pair of variables:
#'
#' \strong{Within-group correlations} are computed on deviation scores (individual values
#' minus group means).
#'
#' \strong{Between-group correlations} are handled differently based on the \code{weight} parameter:
#' \itemize{
#'   \item If \code{weight = TRUE}: The correlation point estimate (median) is obtained
#'     from a model fitted on group means replicated for each observation (implicitly
#'     weighting by group size). However, the credible interval is always obtained from
#'     a model fitted on unique group means only, ensuring that uncertainty reflects
#'     the actual number of groups rather than the total sample size.
#'   \item If \code{weight = FALSE}: Both the correlation estimate and credible interval
#'     are obtained from a model fitted on unique group means. Each group contributes
#'     equally regardless of size.
#' }
#'
#' Models are saved to the specified folder for caching. Significance is determined
#' by whether the specified credible interval excludes zero.
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' data <- data.frame(
#'   school = rep(1:5, each = 20),
#'   math_score = rnorm(100, 50, 10),
#'   reading_score = rnorm(100, 50, 10)
#' )
#'
#' # Compute Bayesian within and between correlations (weighted)
#' result <- bayes_within_between_correlations(
#'   data = data,
#'   group = "school",
#'   vars = c("math_score", "reading_score"),
#'   ci = 0.9,
#'   folder = "brms_models/"
#' )
#'
#' # Compute unweighted between-group correlations
#' result_unweighted <- bayes_within_between_correlations(
#'   data = data,
#'   group = "school",
#'   vars = c("math_score", "reading_score"),
#'   weight = FALSE,
#'   ci = 0.9,
#'   folder = "brms_models/"
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
  folder
) {
  # Check that brms is installed
  rlang::check_installed(
    "brms",
    reason = "to fit Bayesian within and between group correlations."
  )

  # Validate inputs
  if (base::missing(folder)) {
    base::stop("Argument 'folder' must be specified to save brms models.")
  }
  if (!base::dir.exists(folder)) {
    base::dir.create(folder, recursive = TRUE)
  }
  if (ci <= 0 || ci >= 1) {
    base::stop("Argument 'ci' must be between 0 and 1.")
  }

  # Calculate quantiles for CI
  alpha <- (1 - ci) / 2
  ci_low <- alpha
  ci_high <- 1 - alpha

  # Compute group means
  group_means <-
    data |>
    dplyr::group_by(!!rlang::sym(group)) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(vars),
        ~ base::mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  # Merge group means back to original data
  d_with_means <-
    data |>
    dplyr::select(dplyr::all_of(base::c(group, vars))) |>
    dplyr::left_join(
      group_means,
      by = group,
      suffix = base::c("", "_between")
    )

  # Compute within-group deviations
  d_centered <-
    d_with_means |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(vars),
        ~ .x - base::get(base::paste0(dplyr::cur_column(), "_between")),
        .names = "{col}_within"
      )
    )

  # Prepare data for between-group correlations
  if (weight) {
    # Use all observations (variance-weighted)
    d_between <- d_centered
  } else {
    # Use only one observation per group (unweighted)
    d_between <- d_centered |>
      dplyr::distinct(!!rlang::sym(group), .keep_all = TRUE)
  }

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
            base::paste0("within_", vars[i], "__", vars[j])
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
              iter = 5000,
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

        # Always prepare unweighted data for credible intervals
        d_between_unweighted <- d_centered |>
          dplyr::distinct(!!rlang::sym(group), .keep_all = TRUE)

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
              base::paste0("between_", vars[i], "__", vars[j], "_unweighted")
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
                iter = 5000,
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
              base::paste0("between_", vars[i], "__", vars[j], "_weighted")
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
                iter = 5000,
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
              base::paste0("between_", vars[i], "__", vars[j], "_unweighted")
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
                iter = 5000,
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

  return(result_tibble)
}