#' Compute Within-Group and Between-Group Correlations
#'
#' This function decomposes observed correlations between variables into within-group
#' and between-group components following the approach of Pedhazur (1997) as originally implemented
#' in \code{psych::statsBy}. The decomposition follows the formula:
#' \deqn{r_{xy} = \eta_{x,wg} * \eta_{y,wg} * r_{xy,wg} + \eta_{x,bg} * \eta_{y,bg} * r_{xy,bg}}
#'
#' @param data A data frame containing the variables to analyze.
#' @param group A character string specifying the name of the grouping variable.
#' @param vars A character vector specifying the names of variables to correlate.
#' @param weight Logical. If TRUE (default), between-group correlations are weighted by group size.
#'   If FALSE, each group contributes equally (unweighted group means).
#' @param flip Logical. If TRUE, between-group correlations are shown in the upper
#'   triangle and within-group correlations in the lower triangle. Default is FALSE.
#'
#' @return A tibble containing a correlation matrix where:
#' \itemize{
#'   \item The upper triangle contains within-group correlations
#'   \item The lower triangle contains between-group correlations
#'   \item Diagonal elements are marked with "-"
#'   \item Significant correlations (p < 0.05) are marked with "*"
#' }
#'
#' @details
#' \strong{Within-group correlations} are computed on deviation scores (individual values
#' minus group means).
#'
#' \strong{Between-group correlations} can be computed in two ways:
#' \itemize{
#'   \item If \code{weight = TRUE}: Computed on group means replicated for each observation.
#'     This implicitly weights groups by their sample size and matches the variance
#'     decomposition formula.
#'   \item If \code{weight = FALSE}: Computed on unique group means only. Each group
#'     contributes equally regardless of size.
#' }
#'
#' The significance tests account for the effective sample size:
#' \itemize{
#'   \item Within-group p-values use the total number of observations
#'   \item Between-group p-values use the number of groups
#' }
#'
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
#' # Compute weighted between-group correlations (default)
#' result_weighted <- within_between_correlations(
#'   data = data,
#'   group = "school",
#'   vars = c("math_score", "reading_score")
#' )
#'
#' # Compute unweighted between-group correlations
#' result_unweighted <- within_between_correlations(
#'   data = data,
#'   group = "school",
#'   vars = c("math_score", "reading_score"),
#'   weight = FALSE
#' )
#' }
#'
#' @references
#' Pedhazur, E. J. (1997). Multiple regression in behavioral research: explanation and prediction. Harcourt Brace.
#'
#' @seealso \code{\link[psych]{statsBy}} for the original implementation
#'
#' @export
within_between_correlations <- function(data, group, vars, weight = TRUE, flip = FALSE) {
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
    dplyr::select(dplyr::all_of(c(group, vars))) |>
    dplyr::left_join(
      group_means,
      by = group,
      suffix = c("", "_between")
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
  n_groups <- base::nrow(group_means)
  n <- base::length(vars)
  comparison_matrix <- base::matrix("", nrow = n, ncol = n)

  # Compute correlations
  for (i in base::seq_along(vars)) {
    for (j in base::seq_along(vars)) {
      if (i == j) {
        comparison_matrix[i, j] <- "\u2013"
      } else if (i < j) {
        # Within-group correlation (on all observations)
        within_x <- d_centered[[base::paste0(vars[i], "_within")]]
        within_y <- d_centered[[base::paste0(vars[j], "_within")]]

        # Check for zero variance
        if (
          stats::sd(within_x, na.rm = TRUE) == 0 ||
            stats::sd(within_y, na.rm = TRUE) == 0
        ) {
          comparison_matrix[i, j] <- "NA"
        } else {
          cor_within <- base::suppressWarnings(
            stats::cor.test(within_x, within_y)
          )
          est <- base::as.numeric(cor_within$estimate)
          pval <- cor_within$p.value
          label <- if (base::is.finite(est)) {
            base::sprintf("%.2f", est)
          } else {
            "NA"
          }
          if (!base::is.na(pval) && pval < 0.05) {
            label <- base::paste0(label, "*")
          }
          comparison_matrix[i, j] <- label
        }
      } else {
        # Between-group correlation
        between_x <- d_between[[base::paste0(vars[i], "_between")]]
        between_y <- d_between[[base::paste0(vars[j], "_between")]]

        # Check for zero variance
        if (
          stats::sd(between_x, na.rm = TRUE) == 0 ||
            stats::sd(between_y, na.rm = TRUE) == 0
        ) {
          comparison_matrix[i, j] <- "NA"
        } else {
          r_bg <- stats::cor(
            between_x,
            between_y,
            use = "pairwise.complete.obs"
          )
          # Compute p-value using number of groups (only if df > 0)
          if (n_groups > 2) {
            t_stat <- (r_bg * base::sqrt(n_groups - 2)) / base::sqrt(1 - r_bg^2)
            pval <- 2 * (1 - stats::pt(base::abs(t_stat), df = n_groups - 2))
          } else {
            # With 2 or fewer groups, p-value is undefined
            pval <- NA
          }

          est <- r_bg
          label <- if (base::is.finite(est)) {
            base::sprintf("%.2f", est)
          } else {
            "NA"
          }
          if (!base::is.na(pval) && pval < 0.05) {
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
      dplyr::as_tibble(comparison_matrix, .name_repair = ~ as.character(c(1:n)))
    )

  # Flip matrix if requested
  if (flip) {
    comparison_matrix <- base::t(comparison_matrix)
    result_tibble <-
      dplyr::bind_cols(
        dplyr::tibble(variable = vars),
        dplyr::as_tibble(comparison_matrix, .name_repair = ~ base::as.character(c(1:n)))
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
  return(result_tibble)
}