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
#'
#' @return A data frame containing:
#' \itemize{
#'   \item All original variables from \code{data}
#'   \item Grand mean centered versions (suffix: \code{_grand_mean_centered})
#'   \item Between-group means (suffix: \code{_between_[group]})
#'   \item Within-group deviations (suffix: \code{_within_[group]})
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
#' \dontrun{
#' # Create sample data
#' data <- data.frame(
#'   participant = rep(1:10, each = 5),
#'   stress = rnorm(50, 50, 10),
#'   mood = rnorm(50, 50, 10)
#' )
#'
#' # Decompose variables for REWB modeling
#' result <- decompose_within_between(
#'   data = data,
#'   group = "participant",
#'   vars = c("stress", "mood")
#' )
#' }
#'
#' @references
#' Bell, A., Fairbrother, M., & Jones, K. (2019). Fixed and random effects models: 
#' making an informed choice. \emph{Quality & Quantity}, 53(2), 1051-1074.
#' 
#' Enders, C. K., & Tofighi, D. (2007). Centering predictor variables in cross-sectional
#' multilevel models: A new look at an old issue. \emph{Psychological Methods}, 12(2), 121-138.
#'
#' @seealso \code{\link{within_between_correlations}} for computing correlations on decomposed components
#'
#' @export
decompose_within_between <- function(data, group, vars) {
  
  # Validate inputs
  if (!group %in% base::names(data)) {
    base::stop("Group variable '", group, "' not found in data")
  }
  
  missing_vars <- base::setdiff(vars, base::names(data))
  if (base::length(missing_vars) > 0) {
    base::stop("Variables not found in data: ", base::paste(missing_vars, collapse = ", "))
  }
  
  # Step 1: Grand mean centering
  result <- 
    data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(vars),
        ~ .x - base::mean(.x, na.rm = TRUE),
        .names = "{col}_grand_mean_centered"
      )
    )
  
  # Step 2: Compute between-group means and within-group deviations
  result <- 
    result |>
    dplyr::group_by(!!rlang::sym(group)) |>
    dplyr::mutate(
      # Between-group component (group means)
      dplyr::across(
        dplyr::all_of(vars),
        ~ base::mean(.x, na.rm = TRUE),
        .names = "{col}_between_{group}"
      ),
      # Within-group component (deviations from group means)
      dplyr::across(
        dplyr::all_of(vars),
        ~ .x - base::get(glue::glue("{dplyr::cur_column()}_between_{.env$group}")),
        .names = "{col}_within_{group}"
      )
    ) |>
    dplyr::ungroup()
  
  return(result)
}