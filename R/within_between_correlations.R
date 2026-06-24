#' Compute Within-Group and Between-Group Correlations
#'
#' This function computes within-group and between-group correlations using one of
#' two estimation methods. The \code{"decomposition"} method (default) follows the
#' approach of Pedhazur (1997) as originally implemented in \code{psych::statsBy},
#' explicitly decomposing scores into within and between components. The \code{"sem"}
#' method uses a two-level structural equation model via \code{lavaan::sem} to
#' simultaneously estimate within-group and between-group covariance matrices, similar
#' to the approach in \code{misty::multilevel.cor}.
#'
#' @param data A data frame containing the variables to analyze.
#' @param group A character string specifying the name of the grouping variable.
#' @param vars A character vector specifying the names of variables to correlate.
#' @param method Character string specifying the estimation method. Either
#'   \code{"decomposition"} (default) for explicit variance decomposition following
#'   Pedhazur (1997), or \code{"sem"} for a two-level structural equation model
#'   estimated via \code{lavaan::sem}.
#' @param weight Logical. If TRUE (default), between-group correlations are weighted by group size.
#'   If FALSE, each group contributes equally (unweighted group means).
#'   Only used when \code{method = "decomposition"}; ignored (with a message) when
#'   \code{method = "sem"} because ML estimation handles unbalanced groups natively.
#' @param flip Logical. If TRUE, between-group correlations are shown in the upper
#'   triangle and within-group correlations in the lower triangle. Default is FALSE.
#' @param significance Character string specifying the significance marking style.
#'   Either "basic" (default) or "detailed". If "basic", correlations with p < .05
#'   are marked with a star. If "detailed", correlations are marked with 1-3 stars 
#'   for p < .05, p < .01, or p < .001, respectively.
#'
#' @return A tibble containing a correlation matrix where:
#' \itemize{
#'   \item The upper triangle contains within-group correlations
#'   \item The lower triangle contains between-group correlations
#'   \item Diagonal elements are marked with "-"
#'   \item Significant correlations are marked with asterisks (see \code{significance} parameter)
#' }
#'
#' @details
#' \strong{Method \code{"decomposition"} (Pedhazur, 1997):}
#'
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
#' \strong{Method \code{"sem"} (two-level SEM):}
#'
#' This method fits a two-level structural equation model using \code{lavaan::sem} with
#' \code{cluster = group}. Within-group and between-group covariance matrices are
#' estimated simultaneously via maximum likelihood. The standardized solution provides
#' correlations at each level, and significance is based on z-tests from lavaan's
#' parameter estimates (using MLR for robust standard errors). The \code{weight}
#' parameter is not applicable for this method because ML estimation naturally handles
#' unbalanced group sizes.
#'
#' The function automatically classifies variables by their level of variation,
#' following the same approach as \code{misty::multilevel.cor}:
#' \itemize{
#'   \item \strong{Between-only variables} have zero variance within all clusters
#'     (e.g., time-invariant traits). These are modeled only at the between level
#'     (level 2), and within-group correlations involving these variables are reported
#'     as \code{NA}.
#'   \item \strong{Within-only variables} have an ICC of approximately zero, meaning
#'     virtually all variance is within clusters. These are modeled only at the within
#'     level (level 1), and between-group correlations involving these variables are
#'     reported as \code{NA}.
#'   \item All other variables are modeled at both levels.
#' }
#'
#' The optimizer follows a fallback chain: the quasi-Newton method (\code{nlminb}) is
#' tried first, then the EM algorithm if \code{nlminb} does not converge, and finally
#' \code{estimator = "ML"} if robust standard errors (MLR) cannot be computed.
#'
#' @examples
#' set.seed(123)
#' # Create sample data
#' data <- data.frame(
#'   school = rep(1:5, each = 20),
#'   math_score = rnorm(100, 50, 10),
#'   reading_score = rnorm(100, 50, 10)
#' )
#'
#' # Compute weighted between-group correlations (default, decomposition method)
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
#'
#' # Use SEM-based estimation
#' \donttest{
#' result_sem <- within_between_correlations(
#'   data = data,
#'   group = "school",
#'   vars = c("math_score", "reading_score"),
#'   method = "sem"
#' )
#' }
#'
#' # Use detailed significance marking
#' result_detailed <- within_between_correlations(
#'   data = data,
#'   group = "school",
#'   vars = c("math_score", "reading_score"),
#'   significance = "detailed"
#' )
#'
#' @references
#' Hox, J., Moerbeek, M., & van de Schoot, R. (2018). \emph{Multilevel analysis:
#' Techniques and applications} (3rd ed.). Routledge.
#'
#' Pedhazur, E. J. (1997). \emph{Multiple regression in behavioral research:
#' Explanation and prediction}. Harcourt Brace.
#'
#' Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: An
#' introduction to basic and advanced multilevel modeling} (2nd ed.). Sage Publishers.
#'
#' @seealso \code{\link[psych]{statsBy}} for the decomposition approach,
#'   \code{\link[misty]{multilevel.cor}} for the SEM-based approach
#'
#' @export
within_between_correlations <- function(data, group, vars, method = c("decomposition", "sem"), weight = TRUE, flip = FALSE, significance = c("basic", "detailed")) {
  method <- base::match.arg(method)
  significance <- base::match.arg(significance)
  .validate_group_vars(data, group, vars)

  # Warn if weight is specified with SEM method
  if (method == "sem" && !base::missing(weight) && !weight) {
    cli::cli_inform(c(
      "i" = "The {.arg weight} argument is ignored when {.code method = \"sem\"}.",
      "i" = "ML estimation handles unbalanced group sizes natively."
    ))
  }
  
  # Helper function to add significance stars
  add_stars <- function(est, pval, style) {
    if (!base::is.finite(est)) {
      return("NA")
    }
    label <- base::sprintf("%.2f", est)

    if (!base::is.na(pval)) {
      if (style == "detailed") {
        if (pval < 0.001) {
          label <- base::paste0(label, "***")
        } else if (pval < 0.01) {
          label <- base::paste0(label, "**")
        } else if (pval < 0.05) {
          label <- base::paste0(label, "*")
        }
      } else {  # basic
        if (pval < 0.05) {
          label <- base::paste0(label, "*")
        }
      }
    }
    
    return(label)
  }

  if (method == "sem") {
    comparison_matrix <- .wb_cor_sem(data, group, vars, significance, add_stars)
  } else {
    comparison_matrix <- .wb_cor_decomposition(data, group, vars, weight, significance, add_stars)
  }

  n <- base::length(vars)

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

  # Set significance note based on style
  if (significance == "detailed") {
    significance_note <- "Correlations marked with * are significant at p < .05, ** at p < .01, and *** at p < .001."
  } else {
    significance_note <- "All correlations marked with a star are significant at p < .05."
  }

  class(result_tibble) <- c("mlstats_wb_tibble", class(result_tibble))
  base::attr(result_tibble, "flipped") <- flip
  base::attr(result_tibble, "significance_note") <- significance_note
  base::attr(result_tibble, "method") <- method
  return(result_tibble)
}

# --- Internal: decomposition method (Pedhazur, 1997) ---
.wb_cor_decomposition <- function(data, group, vars, weight, significance, add_stars) {
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
          comparison_matrix[i, j] <- add_stars(est, pval, significance)
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
          comparison_matrix[i, j] <- add_stars(est, pval, significance)
        }
      }
    }
  }

  comparison_matrix
}

# --- Internal: SEM method (two-level SEM via lavaan) ---
.wb_cor_sem <- function(data, group, vars, significance, add_stars) {
  rlang::check_installed("lavaan", reason = "to compute SEM-based multilevel correlations.")

  n <- base::length(vars)
  comparison_matrix <- base::matrix("", nrow = n, ncol = n)

  if (n < 2) {
    comparison_matrix[1, 1] <- "\u2013"
    return(comparison_matrix)
  }

  cluster_vec <- data[[group]]

  # --- Detect between-only variables (zero within-cluster variance) ---
  # These are cluster-level variables (e.g., traits) that have no within-group

  # variation and must be excluded from the within (level 1) model.
  is_between_only <- base::vapply(vars, function(v) {
    grp_var <- base::tapply(data[[v]], cluster_vec, stats::var, na.rm = TRUE)
    base::all(grp_var < .Machine$double.eps^0.5, na.rm = TRUE)
  }, FUN.VALUE = base::logical(1))
  between_only_vars <- vars[is_between_only]

  # --- Detect within-only variables (ICC ~ 0) among remaining variables ---
  # These variables have no between-group variation and must be excluded from
  # the between (level 2) model.
  remaining_vars <- base::setdiff(vars, between_only_vars)
  if (base::length(remaining_vars) > 0) {
    is_within_only <- base::vapply(remaining_vars, function(v) {
      sigma2_total <- stats::var(data[[v]], na.rm = TRUE)
      if (sigma2_total < .Machine$double.eps^0.5) return(FALSE)
      grp_means <- base::tapply(data[[v]], cluster_vec, base::mean, na.rm = TRUE)
      sigma2_between <- stats::var(grp_means, na.rm = TRUE)
      (sigma2_between / sigma2_total) < .Machine$double.eps^0.5
    }, FUN.VALUE = base::logical(1))
    within_only_vars <- remaining_vars[is_within_only]
  } else {
    within_only_vars <- base::character(0)
  }

  # --- Variable sets per level ---
  var_within <- base::setdiff(vars, between_only_vars)
  var_between <- base::setdiff(vars, within_only_vars)

  # --- Build lavaan model syntax (covariances only, as in misty) ---
  within_lines <- base::character(0)
  between_lines <- base::character(0)

  if (base::length(var_within) >= 2) {
    combos_w <- utils::combn(var_within, 2)
    for (k in base::seq_len(base::ncol(combos_w))) {
      within_lines <- base::c(
        within_lines,
        base::paste0(combos_w[1, k], " ~~ ", combos_w[2, k])
      )
    }
  } else if (base::length(var_within) == 1) {
    within_lines <- base::paste0(var_within, " ~~ ", var_within)
  }

  if (base::length(var_between) >= 2) {
    combos_b <- utils::combn(var_between, 2)
    for (k in base::seq_len(base::ncol(combos_b))) {
      between_lines <- base::c(
        between_lines,
        base::paste0(combos_b[1, k], " ~~ ", combos_b[2, k])
      )
    }
  } else if (base::length(var_between) == 1) {
    between_lines <- base::paste0(var_between, " ~~ ", var_between)
  }

  model_syntax <- base::paste0(
    "level: 1\n",
    base::paste(within_lines, collapse = "\n"),
    "\n\nlevel: 2\n",
    base::paste(between_lines, collapse = "\n")
  )

  # --- Fit the model (MLR with nlminb -> EM fallback -> ML fallback) ---
  .try_sem <- function(...) {
    base::tryCatch(
      base::suppressWarnings(lavaan::sem(...)),
      error = function(e) NULL
    )
  }
  .converged <- function(fit) {
    !base::is.null(fit) && base::isTRUE(lavaan::lavInspect(fit, "converged"))
  }

  fit <- .try_sem(
    model_syntax,
    data = data,
    cluster = group,
    estimator = "MLR",
    missing = "listwise",
    optim.method = "nlminb",
    check.gradient = FALSE,
    check.post = FALSE,
    check.vcov = FALSE
  )

  if (!.converged(fit)) {
    fit <- .try_sem(
      model_syntax,
      data = data,
      cluster = group,
      estimator = "MLR",
      missing = "listwise",
      optim.method = "em",
      se = "robust.huber.white",
      check.gradient = FALSE,
      check.post = FALSE,
      check.vcov = FALSE
    )
  }

  se_ok <- base::tryCatch(
    {
      pe <- lavaan::parameterEstimates(fit)
      !base::all(base::is.na(pe$se))
    },
    error = function(e) FALSE
  )

  if (!.converged(fit) || !se_ok) {
    fit <- .try_sem(
      model_syntax,
      data = data,
      cluster = group,
      estimator = "ML",
      missing = "listwise",
      check.gradient = FALSE,
      check.post = FALSE,
      check.vcov = FALSE
    )
  }

  if (!.converged(fit)) {
    cli::cli_warn(c(
      "The two-level SEM model could not be fit for variables {.val {vars}}.",
      "i" = "Returning {.val NA} for all correlations.",
      "i" = "Try {.code method = \"decomposition\"} instead, or check your data for collinearity or small group sizes."
    ))
    comparison_matrix[] <- "NA"
    base::diag(comparison_matrix) <- "–"
    return(comparison_matrix)
  }

  # --- Extract results via lavMatrixRepresentation ---
  std_sol <- lavaan::lavMatrixRepresentation(
    lavaan::standardizedSolution(fit)
  )
  param_est <- lavaan::lavMatrixRepresentation(
    lavaan::parameterEstimates(fit)
  )

  # Within: filter level 1 parameter IDs, then theta matrix off-diagonal
  within_ids <- base::unlist(
    base::subset(param_est, param_est$level == 1, select = "id")
  )
  if (base::length(within_ids) > 0) {
    within_std <- std_sol[within_ids, , drop = FALSE]
    within_theta <- within_std[
      within_std$mat == "theta" & within_std$row != within_std$col,
      ,
      drop = FALSE
    ]
  } else {
    within_theta <- std_sol[0, , drop = FALSE]
  }

  # Between: filter level 2 parameter IDs, then theta matrix off-diagonal
  between_ids <- base::unlist(
    base::subset(param_est, param_est$level == 2, select = "id")
  )
  if (base::length(between_ids) > 0) {
    between_std <- std_sol[between_ids, , drop = FALSE]
    between_theta <- between_std[
      between_std$mat == "theta" & between_std$row != between_std$col,
      ,
      drop = FALSE
    ]
  } else {
    between_theta <- std_sol[0, , drop = FALSE]
  }

  # --- Fill comparison matrix ---
  improper_pairs <- base::character(0)

  for (i in base::seq_along(vars)) {
    for (j in base::seq_along(vars)) {
      if (i == j) {
        comparison_matrix[i, j] <- "\u2013"
      } else if (i < j) {
        # Within-group correlation (upper triangle by default)
        vi <- vars[i]
        vj <- vars[j]
        if (vi %in% var_within && vj %in% var_within) {
          idx <- base::which(
            (within_theta$lhs == vi & within_theta$rhs == vj) |
              (within_theta$lhs == vj & within_theta$rhs == vi)
          )
          if (base::length(idx) > 0) {
            est <- within_theta$est.std[idx[1]]
            pval <- within_theta$pvalue[idx[1]]
            if (!base::is.finite(est) || base::abs(est) > 1) {
              improper_pairs <- base::c(
                improper_pairs,
                base::paste0(vi, "-", vj, " (within)")
              )
              comparison_matrix[i, j] <- "NA"
            } else {
              comparison_matrix[i, j] <- add_stars(est, pval, significance)
            }
          } else {
            comparison_matrix[i, j] <- "NA"
          }
        } else {
          comparison_matrix[i, j] <- "NA"
        }
      } else {
        # Between-group correlation (lower triangle by default)
        vi <- vars[i]
        vj <- vars[j]
        if (vi %in% var_between && vj %in% var_between) {
          idx <- base::which(
            (between_theta$lhs == vi & between_theta$rhs == vj) |
              (between_theta$lhs == vj & between_theta$rhs == vi)
          )
          if (base::length(idx) > 0) {
            est <- between_theta$est.std[idx[1]]
            pval <- between_theta$pvalue[idx[1]]
            if (!base::is.finite(est) || base::abs(est) > 1) {
              improper_pairs <- base::c(
                improper_pairs,
                base::paste0(vi, "-", vj, " (between)")
              )
              comparison_matrix[i, j] <- "NA"
            } else {
              comparison_matrix[i, j] <- add_stars(est, pval, significance)
            }
          } else {
            comparison_matrix[i, j] <- "NA"
          }
        } else {
          comparison_matrix[i, j] <- "NA"
        }
      }
    }
  }

  if (base::length(improper_pairs) > 0) {
    n_improper <- base::length(improper_pairs)
    cli::cli_warn(c(
      "The two-level SEM model produced {n_improper} out-of-range standardized correlation{?s} (outside [-1, 1]) for: {.val {improper_pairs}}.",
      "i" = "This usually indicates a non-positive-definite residual covariance matrix at that level.",
      "i" = "Returning {.val NA} for the affected {n_improper} correlation{?s}."
    ))
  }

  comparison_matrix
}