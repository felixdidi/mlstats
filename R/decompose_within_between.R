#' Decompose Variables into Within-Group and Between-Group Components
#'
#' This function performs a multilevel decomposition of variables by computing:
#' \itemize{
#'   \item Grand mean centered scores (deviations from overall mean)
#'   \item Between-group scores (group means)
#'   \item Within-group scores (deviations from group means)
#' }
#'
#' This decomposition is commonly used in multilevel modeling to separate within-group
#' and between-group variance components (Enders & Tofighi, 2007). The decomposed
#' variables are particularly useful for Random Effects Within-Between (REWB) models
#' (Bell et al., 2019), which allow the estimation of distinct within-group and
#' between-group effects.
#'
#' @param data A data frame containing the variables to decompose.
#' @param group A character string specifying the name of the grouping variable.
#' @param vars A character vector specifying the names of variables to decompose.
#' @param components A character vector specifying which components to compute.
#'   Any subset of \code{c("gmc", "between", "within")} (default: all three).
#'   \code{"gmc"} = grand mean centering, \code{"between"} = group means,
#'   \code{"within"} = within-group deviations. If \code{"within"} is requested
#'   without \code{"between"}, the between component is computed internally as an
#'   intermediate step and not included in the output.
#' @param gmc_pattern A glue-style naming pattern for grand-mean-centered columns.
#'   Use \code{{col}} for the variable name. Default: \code{"{col}_grand_mean_centered"}.
#' @param between_pattern A glue-style naming pattern for between-group (group mean)
#'   columns. Use \code{{col}} for the variable name and \code{{group}} for the
#'   grouping variable name. Default: \code{"{col}_between_{group}"}.
#' @param within_pattern A glue-style naming pattern for within-group deviation
#'   columns. Use \code{{col}} for the variable name and \code{{group}} for the
#'   grouping variable name. Default: \code{"{col}_within_{group}"}.
#'
#' @return A data frame containing:
#' \itemize{
#'   \item All original variables from \code{data}
#'   \item Grand mean centered versions (named by \code{gmc_pattern}), if \code{"gmc"} in \code{components}
#'   \item Between-group means (named by \code{between_pattern}), if \code{"between"} in \code{components}
#'   \item Within-group deviations (named by \code{within_pattern}), if \code{"within"} in \code{components}
#' }
#'
#' @details
#' The function performs three centering operations:
#'
#' \strong{1. Grand mean centering:} Each value is expressed as a deviation from the
#' overall sample mean. This centers the entire distribution at zero.
#'
#' \strong{2. Between-group component:} For each observation, this equals the mean of
#' their group. These values are constant within groups and vary between groups.
#' In REWB models, this represents the between-group effect of the predictor.
#'
#' \strong{3. Within-group component:} Each value is expressed as a deviation from
#' their group mean. This removes all between-group variance and represents the
#' within-group effect of the predictor in REWB models.
#'
#' @examples
#' data("media_diary")
#'
#' # Decompose all three components (default)
#' result <- decompose_within_between(
#'   data = media_diary,
#'   group = "person",
#'   vars = c("stress", "screen_time")
#' )
#'
#' # Only between and within (no grand mean centering)
#' result_wb <- decompose_within_between(
#'   data = media_diary,
#'   group = "person",
#'   vars = c("stress", "screen_time"),
#'   components = c("between", "within")
#' )
#'
#' # Custom column naming: flat suffixes without the group name
#' result_flat <- decompose_within_between(
#'   data = media_diary,
#'   group = "person",
#'   vars = c("stress", "screen_time"),
#'   components = c("between", "within"),
#'   between_pattern = "{col}_between",
#'   within_pattern = "{col}_within"
#' )
#'
#' @references
#' Bell, A., Fairbrother, M., & Jones, K. (2019). Fixed and random effects models:
#' making an informed choice. \emph{Quality & Quantity}, 53(2), 1051-1074.
#'
#' Enders, C. K., & Tofighi, D. (2007). Centering predictor variables in cross-sectional
#' multilevel models: A new look at an old issue. \emph{Psychological Methods}, 12(2), 121-138.
#'
#' @seealso \code{\link{within_between_correlations}} and
#'   \code{\link{bayes_within_between_correlations}}, which use this function
#'   internally to perform the within/between decomposition.
#'
#' @export
decompose_within_between <- function(
  data,
  group,
  vars,
  components = c("gmc", "between", "within"),
  gmc_pattern = "{col}_grand_mean_centered",
  between_pattern = "{col}_between_{group}",
  within_pattern = "{col}_within_{group}"
) {
  components <- base::match.arg(components, several.ok = TRUE)

  # Validate inputs
  .validate_group_vars(data, group, vars)

  non_numeric_vars <- vars[!base::vapply(data[vars], base::is.numeric, base::logical(1))]
  if (base::length(non_numeric_vars) > 0) {
    cli::cli_abort(c(
      "Variable{?s} {.val {non_numeric_vars}} must be numeric.",
      "i" = "All variables passed to {.arg vars} must be numeric columns."
    ))
  }

  result <- data

  # Grand mean centering
  if ("gmc" %in% components) {
    result <- result |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(vars),
          ~ .x - base::mean(.x, na.rm = TRUE),
          .names = gmc_pattern
        )
      )
  }

  # Between and/or within components
  if ("between" %in% components || "within" %in% components) {
    # Between: group means (always needed when within is requested, even if not
    # in final output, because within is defined as x - group_mean)
    result <- result |>
      dplyr::group_by(!!rlang::sym(group)) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(vars),
          ~ base::mean(.x, na.rm = TRUE),
          .names = between_pattern
        )
      ) |>
      dplyr::ungroup()

    # Within: deviations from group means
    if ("within" %in% components) {
      grp <- group  # capture for use inside the across lambda
      result <- result |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(vars),
            function(x) {
              between_col <- .eval_name_pattern(between_pattern, dplyr::cur_column(), grp)
              x - dplyr::pick(dplyr::everything())[[between_col]]
            },
            .names = within_pattern
          )
        )
    }

    # Drop the intermediate between columns if they were only needed to
    # compute within and were not requested in the output
    if (!"between" %in% components) {
      between_col_names <- base::vapply(
        vars,
        function(v) .eval_name_pattern(between_pattern, v, group),
        base::character(1)
      )
      result <- dplyr::select(result, -dplyr::all_of(between_col_names))
    }
  }

  return(result)
}
