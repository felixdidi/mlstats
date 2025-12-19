#' Compute Bayesian Multilevel Descriptive Statistics
#'
#' This function creates a comprehensive descriptive statistics table for multilevel data,
#' including basic descriptives, Bayesian within-group and between-group correlations, and
#' Bayesian intraclass correlation coefficients (ICCs).
#'
#' @param data A data frame containing the variables to analyze.
#' @param group A character string specifying the name of the grouping variable.
#' @param vars A character vector specifying the names of variables to describe.
#' @param weight Logical. If TRUE (default), statistics are weighted by group size so that
#'   each observation contributes equally. If FALSE, statistics are unweighted by group
#'   size (each group contributes equally).
#' @param ci Numeric value between 0 and 1 specifying the credible interval width
#'   (default = 0.9 for 90% CI).
#' @param folder Character string specifying the directory path where brms models
#'   should be saved. No default; must be specified.
#' @param flip Logical. If TRUE, between-group correlations are shown in the upper
#'   triangle and within-group correlations in the lower triangle. Default is FALSE.
#' @param remove_leading_zero Logical. If TRUE (default), removes leading zeros from
#'   decimal values in correlation and ICC columns according to APA standards.
#'
#' @return A tibble of class \code{mlstats_desc_tibble} containing:
#' \itemize{
#'   \item \code{variable}: Variable name
#'   \item \code{n_obs}: Number of observations
#'   \item \code{m}: Mean
#'   \item \code{sd}: Standard deviation
#'   \item \code{range}: Range from minimum to maximum
#'   \item One column per variable in \code{vars} containing correlations
#'   \item \code{icc}: Intraclass correlation coefficient
#' }
#' 
#' The tibble can be returned as a gt object using \code{print("gt")}
#' and as a tinytable object using \code{print("tt")}.
#'
#' @details
#' The function combines three types of information:
#'
#' \strong{Descriptive Statistics:} Basic summary statistics for each variable.
#' When \code{weight = TRUE} (default), statistics are calculated across all observations.
#' When \code{weight = FALSE}, the mean is the mean of group means (unweighted), and
#' the SD is the standard deviation of group means, representing between-group variability.
#'
#' \strong{Correlations:} Bayesian within-group correlations (upper triangle) and between-group
#' correlations (lower triangle) computed using \code{\link{bayes_within_between_correlations}}.
#' The \code{weight} parameter controls whether between-group correlations are weighted
#' by group size (default) or unweighted. Credible correlations are marked with "*".
#'
#' \strong{ICC:} The intraclass correlation coefficient computed from an unconditional
#' (intercept-only) multilevel model using \code{brms::brm}. The ICC represents
#' the proportion of variance in each variable that exists between groups.
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' data <- data.frame(
#'   school = rep(1:5, each = 20),
#'   math_score = rnorm(100, 50, 10),
#'   reading_score = rnorm(100, 50, 10),
#'   motivation = rnorm(100, 5, 2)
#' )
#'
#' # Compute Bayesian multilevel descriptives with weighted between-group correlations
#' result <- bayes_mldesc(
#'   data = data,
#'   group = "school",
#'   vars = c("math_score", "reading_score", "motivation"),
#'   folder = "brms_models/"
#' )
#'
#' # Print as default tibble
#' print(result)
#'
#' # Print as formatted gt table
#' print(result, "gt")
#'
#' # Print as gt table with custom title and notes
#' print(result, "gt",
#'       table_title = "Custom Table Title",
#'       correlation_note = "Custom correlation note",
#'       note_text = "Data collected from 5 schools.")
#'
#' # Compute with unweighted between-group correlations
#' result_unweighted <- bayes_mldesc(
#'   data = data,
#'   group = "school",
#'   vars = c("math_score", "reading_score", "motivation"),
#'   weight = FALSE,
#'   folder = "brms_models/"
#' )
#' }
#'
#' @seealso \code{\link{bayes_within_between_correlations}}, \code{\link{mldesc}}
#'
#' @export
bayes_mldesc <- function(
  data,
  group,
  vars,
  weight = TRUE,
  ci = 0.9,
  folder,
  flip = FALSE,
  remove_leading_zero = TRUE
) {
  # Check that brms is installed
  rlang::check_installed(
    "brms",
    reason = "to fit Bayesian multilevel models"
  )

  # Validate folder argument
  if (base::missing(folder)) {
    base::stop("Argument 'folder' must be specified to save brms models.")
  }
  if (!base::dir.exists(folder)) {
    base::dir.create(folder, recursive = TRUE)
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

  # Internal function to compute Bayesian ICC
  get_bayes_icc <- function(data, group, vars, ci, folder) {
    # Calculate quantiles for CI
    alpha <- (1 - ci) / 2
    ci_low <- alpha
    ci_high <- 1 - alpha

    icc_values <- base::sapply(vars, function(var) {
      # Fit intercept-only multilevel model
      formula_str <- base::paste0(var, " ~ 1 + (1 | ", group, ")")
      model_file <- base::file.path(folder, base::paste0("icc_", var))

      fit <- base::suppressWarnings(
        brms::brm(
          stats::as.formula(formula_str),
          data = data,
          seed = 42,
          iter = 5000,
          file = model_file,
          silent = 2,
          refresh = 0
        )
      )

      # Extract variance components
      vc <- brms::VarCorr(fit)

      # Get posterior draws of variance components
      draws <- brms::as_draws_df(fit)

      # Calculate ICC for each draw
      icc_draws <- draws[[base::paste0("sd_", group, "__Intercept")]]^2 /
        (draws[[base::paste0("sd_", group, "__Intercept")]]^2 + draws$sigma^2)

      # Get median and credible interval
      icc_median <- stats::median(icc_draws)

      label <- remove_zero(base::sprintf("%.2f", icc_median))

      return(label)
    })

    tibble::tibble(
      variable = vars,
      icc = icc_values
    )
  }

  # Internal function to compute descriptive statistics
  get_desc <- function(data, vars, group) {
    desc_list <- base::lapply(vars, function(var) {
      var_data <- data[[var]]
      var_data_clean <- var_data[!base::is.na(var_data)]

      # Calculate weighted (default) or unweighted statistics
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
        
        # Mean of group means
        m_val <- base::mean(group_means$group_mean, na.rm = TRUE)
        # SD of group means (not mean of group SDs)
        sd_val <- stats::sd(group_means$group_mean, na.rm = TRUE)
      }

      tibble::tibble(
        variable = var,
        n_obs = base::as.character(scales::comma(base::length(var_data_clean))),
        m = base::sprintf("%.2f", m_val),
        sd = base::sprintf("%.2f", sd_val),
        range = base::paste0(
          base::sprintf("%.0f", base::min(var_data_clean, na.rm = TRUE)),
          "-",
          base::sprintf("%.0f", base::max(var_data_clean, na.rm = TRUE))
        )
      )
    })

    dplyr::bind_rows(desc_list)
  }

  # Compute descriptive statistics
  desc_stats <- get_desc(data, vars, group)

  # Compute Bayesian within-between correlations
  corr_matrix <- bayes_within_between_correlations(
    data = data,
    group = group,
    vars = vars,
    weight = weight,
    ci = ci,
    folder = folder,
    flip = flip
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

  # Compute Bayesian ICCs
  icc_stats <- get_bayes_icc(data, group, vars, ci, folder)

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
  ci_percent <- base::round(ci * 100)
  attr(result, "table_title") <- "Bayesian multilevel descriptive statistics"
  attr(result, "flipped") <- flip
  attr(result, "correlation_note") <- if (flip) {
    "Between-group correlations above, within-group correlations below the diagonal."
  } else {
    "Within-group correlations above, between-group correlations below the diagonal."
  }
  attr(result, "note_text") <- if (weight) {
      "Bayesian group-weighted multilevel descriptive statistics computed with mlstats. "
  } else {
      "Bayesian unweighted multilevel descriptive statistics computed with mlstats. "
  }

  attr(result, "significance_note") <- base::paste0(
    "Correlations marked with a star have ",
    ci_percent,
    "% credible intervals that exclude zero."
  )

  attr(result, "bayesian") <- TRUE

  return(result)
}