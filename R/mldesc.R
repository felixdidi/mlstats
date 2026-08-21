#' Compute Multilevel Descriptive Statistics
#'
#' Creates a publication-ready descriptive statistics table for multilevel data
#' (e.g., repeated measurements per person, or students nested within schools).
#' For each variable, the table reports basic descriptives, the proportion of
#' variance that lies between groups (the intraclass correlation, ICC), and how
#' each pair of variables relates both within and between groups (see
#' \code{\link{within_between_correlations}} and \code{vignette("correlation-methods")}
#' for the statistical background on the latter).
#'
#' @param data A data frame containing the variables to analyze.
#' @param group A character string specifying the name of the grouping variable.
#' @param vars A character vector specifying the names of variables to describe.
#' @param method Character string specifying the estimation method for correlations
#'   and the ICC: \code{"decomposition"} (default), \code{"sem"}, or \code{"bayes"}.
#'   See \code{\link{within_between_correlations}} for details on the correlation
#'   methods. With \code{method = "bayes"}, the ICC is also estimated with a
#'   Bayesian intercept-only model (via \code{brms::brm}) instead of
#'   \code{lme4::lmer}, reporting the posterior median.
#' @param weight Logical. If TRUE (default), the mean and SD are calculated across all
#'   observations (so larger groups contribute more), and the between-group correlation
#'   gives more weight to larger groups. If FALSE, every group counts equally: the mean
#'   and SD are calculated on group means, and the between-group correlation is
#'   unweighted. For correlations, this is only used when \code{method = "decomposition"}
#'   or \code{method = "bayes"}.
#' @param flip Logical. If TRUE, between-group correlations are shown in the upper
#'   triangle and within-group correlations in the lower triangle. Default is FALSE.
#' @param significance Character string specifying the significance marking style.
#'   Either "basic" (default) or "detailed". If "basic", correlations with p < .05
#'   are marked with a star. If "detailed", correlations are marked with 1-3 stars
#'   for p < .05, p < .01, or p < .001, respectively. Ignored (with a message) when
#'   \code{method = "bayes"}, which always marks correlations whose credible interval
#'   excludes zero with a single star.
#' @param ci Numeric value strictly between 0 and 1 specifying the credible interval
#'   width used for the within-group and between-group correlations when \code{method
#'   = "bayes"}. Default is 0.9 (90% CI). The ICC always reports the posterior median
#'   only and is not affected by this argument. Ignored (with a message) for other
#'   methods.
#' @param folder Character string specifying the directory path where \code{brms}
#'   models should be saved. Required when \code{method = "bayes"}; ignored (with a
#'   message) otherwise. Default is \code{NULL}.
#' @param remove_leading_zero Logical. If TRUE (default), removes leading zeros from
#'   decimal values in correlation and ICC columns according to APA standards.
#'
#' @return A tibble of class \code{mlstats_desc_tibble} containing:
#' \itemize{
#'   \item \code{variable}: Variable name
#'   \item \code{n_obs}: Number of observations. For variables that are
#'     constant within every group (e.g., a trait measured once per person
#'     but repeated across that person's rows), this is the number of groups
#'     that provided a value, not the number of rows it was replicated across.
#'   \item \code{m}: Mean (rounded to two decimals)
#'   \item \code{sd}: Standard deviation (rounded to two decimals)
#'   \item \code{range}: Range from observed minimum to maximum, rounded to
#'     two decimals unless both are whole numbers (e.g., integer scales), in
#'     which case decimals are dropped
#'   \item One column per variable in \code{vars} containing correlations
#'   \item \code{icc}: Intraclass correlation coefficient
#' }
#'
#' The tibble can be returned as a gt object using \code{print(result, format = "gt")}
#' and as a tinytable object using \code{print(result, format = "tt")}.
#'
#' @details
#' The function combines three types of information:
#'
#' \strong{Descriptive statistics:} Basic summary statistics for each variable. When
#' \code{weight = TRUE} (default), statistics are calculated across all observations.
#' When \code{weight = FALSE}, the mean is the mean of group means, and the SD is the
#' standard deviation of group means, representing between-group variability.
#'
#' \strong{Correlations:} Within-group correlations (upper triangle) and between-group
#' correlations (lower triangle), computed using \code{\link{within_between_correlations}}.
#' See that function's documentation and the package vignette for how each method
#' estimates these correlations and tests them for significance.
#'
#' \strong{ICC:} The intraclass correlation coefficient, computed from an unconditional
#' (intercept-only) multilevel model using \code{lme4::lmer} (or \code{brms::brm} when
#' \code{method = "bayes"}). The ICC represents the proportion of variance in each
#' variable that lies between groups, with values close to 1 indicating a variable
#' that barely varies within groups (e.g., a stable trait), and values close to 0
#' indicating a variable that barely varies between groups (e.g., a fast-changing
#' state).
#'
#' The ICC is always computed from a linear (Gaussian) model, regardless of a
#' variable's measurement scale. For binary, ordinal, or count variables this
#' yields a linear-probability-style ICC rather than a latent-scale ICC from a
#' generalized linear mixed model. A warning is emitted if any \code{vars}
#' look binary, ordinal, or count-like (few, whole-number values).
#'
#' With \code{method = "bayes"}, the function fits one \code{brms} model per variable
#' for the ICCs, plus all the models described in
#' \code{\link{within_between_correlations}} for the correlations — for \code{p}
#' variables, \code{p} ICC fits in addition to the within/between-group correlation
#' fits. This can take a long time for larger numbers of variables; see
#' \code{vignette("correlation-methods")} for details.
#'
#' @examples
#' data("media_diary")
#' vars <- c("self_control", "wellbeing", "screen_time", "stress")
#'
#' # Compute multilevel descriptives (default: decomposition method)
#' result <- mldesc(
#'   data = media_diary,
#'   group = "person",
#'   vars = vars
#' )
#'
#' result
#'
#' # Compute with unweighted between-group correlations
#' result_unweighted <- mldesc(
#'   data = media_diary,
#'   group = "person",
#'   vars = vars,
#'   weight = FALSE
#' )
#'
#' # Use SEM-based estimation for correlations (on similarly-scaled variables;
#' # SEM is sensitive to large scale differences, unlike "decomposition")
#' \donttest{
#' result_sem <- mldesc(
#'   data = media_diary,
#'   group = "person",
#'   vars = c("self_control", "wellbeing", "stress"),
#'   method = "sem"
#' )
#' }
#'
#' # Use detailed significance marking
#' result_detailed <- mldesc(
#'   data = media_diary,
#'   group = "person",
#'   vars = vars,
#'   significance = "detailed"
#' )
#'
#' # Use Bayesian estimation for correlations and the ICC (requires brms)
#' \donttest{
#' result_bayes <- mldesc(
#'   data = media_diary,
#'   group = "person",
#'   vars = c("self_control", "wellbeing", "screen_time"),
#'   method = "bayes",
#'   folder = tempdir()
#' )
#' }
#'
#' @references
#' Bürkner, P.-C. (2017). brms: An R package for Bayesian multilevel models using
#' Stan. \emph{Journal of Statistical Software, 80}(1), 1–28.
#' \doi{10.18637/jss.v080.i01}
#'
#' Pedhazur, E. J. (1997). \emph{Multiple regression in behavioral research:
#' Explanation and prediction}. Harcourt Brace.
#'
#' Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: An
#' introduction to basic and advanced multilevel modeling} (2nd ed.). Sage Publishers.
#'
#' @seealso \code{\link{within_between_correlations}} for details on how within-group
#'   and between-group correlations are estimated and tested.
#'
#' @export
mldesc <- function(
  data,
  group,
  vars,
  method = c("decomposition", "sem", "bayes"),
  weight = TRUE,
  flip = FALSE,
  significance = c("basic", "detailed"),
  ci = 0.9,
  folder = NULL,
  remove_leading_zero = TRUE
) {
  # Captured before match.arg() reassigns `significance` below: once a
  # formal argument has been assigned to, missing() unconditionally returns
  # FALSE for it, regardless of whether the caller actually supplied it.
  significance_missing <- base::missing(significance)

  method <- base::match.arg(method)
  significance <- base::match.arg(significance)
  .validate_group_vars(data, group, vars)
  data <- .drop_na_group(data, group)

  # `ci`/`folder` only matter for method = "bayes"; `significance` only
  # matters for the other two methods (bayes marks credible intervals
  # instead of p-values). Only fire when the caller explicitly supplied the
  # inapplicable argument, mirroring within_between_correlations().
  if (method != "bayes") {
    if (!base::missing(ci)) {
      cli::cli_inform(c(
        "i" = "The {.arg ci} argument has no effect unless {.code method = \"bayes\"}."
      ))
    }
    if (!base::is.null(folder)) {
      cli::cli_inform(c(
        "i" = "The {.arg folder} argument has no effect unless {.code method = \"bayes\"}."
      ))
    }
  } else if (!significance_missing) {
    cli::cli_inform(c(
      "i" = "The {.arg significance} argument has no effect when {.code method = \"bayes\"}.",
      "i" = "Credible-interval-based marking is controlled by {.arg ci} instead."
    ))
  }

  if (method == "bayes") {
    rlang::check_installed(
      "brms",
      reason = "to fit Bayesian multilevel models"
    )
    if (base::is.null(folder)) {
      base::stop("Argument 'folder' must be specified to save brms models when method = \"bayes\".")
    }
    if (ci <= 0 || ci >= 1) {
      cli::cli_abort("{.arg ci} must be between 0 and 1.")
    }
    if (!base::dir.exists(folder)) {
      base::dir.create(folder, recursive = TRUE)
    }

    # brms::brm(file = ...) caches purely on filename, so a content hash of
    # the relevant data (plus the sampling settings, which also affect the
    # fit) is folded into the ICC cache filenames below (the correlation
    # models get the same treatment inside within_between_correlations()).
    # Otherwise, re-running with different data, or different
    # options(mlstats.brms_iter/chains = ...), but the same
    # `vars`/`group`/`folder` would silently reload a stale cached fit
    # instead of refitting.
    data_hash <- rlang::hash(base::list(data[base::c(group, vars)], .brms_iter(), .brms_chains()))
  }

  # Internal function to remove leading zeros from decimal strings
  remove_zero <- function(x) {
    if (!remove_leading_zero) {
      return(x)
    }
    # Replace "0." with "." and "-0." with "-."
    x <- base::gsub("^0\\.", ".", x)
    x <- base::gsub("^-0\\.", "-.", x)
    return(x)
  }

  # Internal function to compute the ICC (frequentist via lme4, or Bayesian
  # via brms when method = "bayes")
  get_icc <- function(data, group, vars) {
    .warn_discrete_icc_vars(data, vars)

    if (method == "bayes") {
      icc_values <- base::sapply(vars, function(var) {
        formula_str <- base::paste0(var, " ~ 1 + (1 | ", group, ")")
        model_file <- base::file.path(folder, base::paste0("icc_", var, "_", data_hash))

        fit <- base::suppressWarnings(
          brms::brm(
            stats::as.formula(formula_str),
            data = data,
            seed = 42,
            iter = .brms_iter(),
            chains = .brms_chains(),
            file = model_file,
            silent = 2,
            refresh = 0
          )
        )

        # Get posterior draws of variance components
        draws <- brms::as_draws_df(fit)

        # Calculate ICC for each draw, then take the posterior median
        icc_draws <- draws[[base::paste0("sd_", group, "__Intercept")]]^2 /
          (draws[[base::paste0("sd_", group, "__Intercept")]]^2 + draws$sigma^2)
        stats::median(icc_draws)
      })
    } else {
      icc_values <- base::sapply(vars, function(var) {
        # Fit intercept-only multilevel model
        formula_str <- base::paste0(var, " ~ 1 + (1 | ", group, ")")

        # Suppress convergence warnings and messages for ICC calculation
        m0 <- base::suppressMessages(
          base::suppressWarnings(
            lme4::lmer(
              stats::as.formula(formula_str),
              data = data,
              control = lme4::lmerControl(
                optimizer = "bobyqa",
                optCtrl = base::list(maxfun = 2e5),
                check.conv.singular = lme4::.makeCC(action = "ignore", tol = 1e-4)
              )
            )
          )
        )

        # Extract variance components, matching on `grp` rather than relying on
        # row order (the random-intercept variance is always the row where
        # `grp` equals the grouping variable, the residual is always "Residual").
        variance <- base::as.data.frame(lme4::VarCorr(m0))
        var_between <- variance$vcov[variance$grp == group]
        var_residual <- variance$vcov[variance$grp == "Residual"]
        var_between / (var_between + var_residual)
      })
    }

    tibble::tibble(
      variable = vars,
      icc = remove_zero(base::sprintf("%.2f", icc_values))
    )
  }

  # Internal function to compute descriptive statistics
  get_desc <- function(data, vars, group) {
    desc_list <- base::lapply(vars, function(var) {
      var_data <- data[[var]]
      var_data_clean <- var_data[!base::is.na(var_data)]

      # Variables that are constant within every group (e.g., a trait
      # measured once per person but attached to every observation) were
      # measured once per group, not once per row: count the groups that
      # provided a value, not the rows the value was replicated across.
      group_values <- base::tapply(
        var_data,
        data[[group]],
        function(v) base::length(base::unique(v[!base::is.na(v)]))
      )
      is_trait <- base::all(group_values <= 1) && base::any(group_values == 1)
      n_val <- if (is_trait) {
        base::sum(group_values == 1)
      } else {
        base::length(var_data_clean)
      }

      if (weight) {
        # Weighted: each observation contributes equally
        m_val <- base::mean(var_data_clean, na.rm = TRUE)
        sd_val <- stats::sd(var_data_clean, na.rm = TRUE)
      } else {
        # Unweighted: each group contributes equally
        group_data <- data[!base::is.na(data[[var]]), ]
        group_means <- group_data |>
          dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
          dplyr::summarise(
            group_mean = base::mean(!!rlang::sym(var), na.rm = TRUE),
            .groups = "drop"
          )

        m_val <- base::mean(group_means$group_mean, na.rm = TRUE)
        sd_val <- stats::sd(group_means$group_mean, na.rm = TRUE)
      }

      # Two decimals for the observed minimum/maximum, unless both are whole
      # numbers (e.g., integer scales), in which case decimals are dropped
      range_min <- base::min(var_data_clean, na.rm = TRUE)
      range_max <- base::max(var_data_clean, na.rm = TRUE)
      range_fmt <- if (
        range_min == base::round(range_min) && range_max == base::round(range_max)
      ) {
        "%.0f"
      } else {
        "%.2f"
      }

      tibble::tibble(
        variable = var,
        n_obs = base::as.character(scales::comma(n_val)),
        m = base::sprintf("%.2f", m_val),
        sd = base::sprintf("%.2f", sd_val),
        range = base::paste0(
          base::sprintf(range_fmt, range_min),
          "\u2013",
          base::sprintf(range_fmt, range_max)
        )
      )
    })

    dplyr::bind_rows(desc_list)
  }

  # Compute descriptive statistics
  desc_stats <- get_desc(data, vars, group)

  # Compute within-between correlations. Two kinds of "argument has no
  # effect" cli_inform() messages from within_between_correlations() are
  # suppressed here because, unlike there, these arguments are NOT no-ops in
  # mldesc(): `weight` still controls the mean/SD calculation above
  # regardless of `method`, and `ci`/`folder`/`significance` are handled (and
  # messaged about, if applicable) by mldesc() itself above. Only messages
  # matching "no effect" are caught; cli_warn() calls (convergence issues,
  # etc.) pass through unaffected.
  corr_matrix <- base::withCallingHandlers(
    within_between_correlations(
      data,
      group,
      vars,
      method = method,
      weight = weight,
      flip = flip,
      significance = significance,
      ci = ci,
      folder = folder
    ),
    message = function(m) {
      if (base::grepl("no effect", base::conditionMessage(m), ignore.case = TRUE)) {
        rlang::cnd_muffle(m)
      }
    }
  )

  # Remove first column (variable names) from correlation matrix
  corr_values <- corr_matrix[, -1]

  # Apply remove_zero to correlation columns
  if (remove_leading_zero) {
    corr_values <- dplyr::mutate(
      corr_values,
      dplyr::across(dplyr::everything(), remove_zero)
    )
  }

  # Compute ICCs
  icc_stats <- get_icc(data, group, vars)

  # Combine all components
  result <- dplyr::bind_cols(
    desc_stats,
    corr_values,
    dplyr::select(icc_stats, dplyr::all_of("icc"))
  ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of("variable"),
        ~ stringr::str_replace_all(.x, "_", " ") |>
          stringr::str_to_sentence()
      ),
      dplyr::across(
        -variable,
        ~ vctrs::new_vctr(.x, class = "mlstats_stat", inherit_base_type = TRUE)
      )
    )

  # Add custom class for tibble printing
  class(result) <- c("mlstats_desc_tibble", class(result))

  # Store default values as attributes
  group_label <- .group_note_label(group)
  attr(result, "table_title") <- ""
  attr(result, "flipped") <- flip
  attr(result, "group") <- group
  attr(result, "correlation_note") <- if (flip) {
    base::paste0(
      "Between-", group_label, " correlations above, within-", group_label,
      " correlations below the diagonal."
    )
  } else {
    base::paste0(
      "Within-", group_label, " correlations above, between-", group_label,
      " correlations below the diagonal."
    )
  }

  # Get significance note and group size note from correlation matrix
  attr(result, "significance_note") <- base::attr(corr_matrix, "significance_note")
  attr(result, "group_size_note") <- base::attr(corr_matrix, "group_size_note")

  if (method == "bayes") {
    attr(result, "note_text") <- if (weight) {
      "Bayesian group-weighted multilevel descriptive statistics computed with mlstats."
    } else {
      "Bayesian unweighted multilevel descriptive statistics computed with mlstats."
    }
    attr(result, "bayesian") <- TRUE
  } else {
    method_label <- if (method == "sem") "SEM-based" else if (weight) "group-weighted" else "unweighted"
    attr(result, "note_text") <- base::paste0(
      base::toupper(base::substr(method_label, 1, 1)),
      base::substr(method_label, 2, base::nchar(method_label)),
      " multilevel descriptive statistics computed with mlstats."
    )
  }
  attr(result, "method") <- method

  return(result)
}
