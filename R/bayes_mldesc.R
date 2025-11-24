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
#' @param remove_leading_zero Logical. If TRUE (default), removes leading zeros from
#'   decimal values in correlation and ICC columns according to APA standards.
#' @param print_gt Logical. If TRUE, returns a formatted gt table instead of a tibble.
#'   Default is FALSE.
#' @param table_title Character string. Custom title for the gt table when print_gt = TRUE.
#'   Default describes descriptive statistics with within-group and between-group correlations.
#' @param correlation_note Character string. Custom note text for correlation interpretation
#'   when print_gt = TRUE. Default describes within-group and between-group correlations.
#' @param note_text Character string. Custom note text to append to the table footer
#'   when print_gt = TRUE.
#'
#' @return A tibble containing descriptive statistics (default), or a gt table object
#'   if print_gt = TRUE. The tibble contains:
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
#' # Compute with unweighted between-group correlations
#' result_unweighted <- bayes_mldesc(
#'   data = data,
#'   group = "school",
#'   vars = c("math_score", "reading_score", "motivation"),
#'   weight = FALSE,
#'   folder = "brms_models/"
#' )
#'
#' # Return as formatted gt table with custom title
#' gt_result <- bayes_mldesc(
#'   data = data,
#'   group = "school",
#'   vars = c("math_score", "reading_score", "motivation"),
#'   print_gt = TRUE,
#'   table_title = "Custom Table Title",
#'   correlation_note = "Custom correlation note",
#'   note_text = "Data collected from 5 schools.",
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
  remove_leading_zero = TRUE,
  print_gt = FALSE,
  table_title = NULL,
  correlation_note = NULL,
  note_text = NULL
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

  # Set default table title, correlation note, and note text if not provided
  if (base::is.null(table_title)) {
    table_title <- "Bayesian descriptive statistics, within-group correlations, between-group correlations, and intraclass correlations (ICCs)"
  }
  if (base::is.null(correlation_note)) {
    correlation_note <- "Within-group correlations depicted above, between-group correlations below the diagonal."
  }
  if (base::is.null(note_text)) {
    ci_percent <- base::round(ci * 100)
    note_text <- base::paste0(
      "Bayesian multilevel descriptive statistics computed with mlstats. ",
      "Correlations marked with a star have ",
      ci_percent,
      "% credible intervals that exclude zero."
    )
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
        n_obs = base::as.character(base::length(var_data_clean)),
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
    folder = folder
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
      )
    )

  # Return gt table if requested
  if (print_gt) {
    x <- result

    # Detect correlation columns (numeric column names like "1", "2", "3", etc.)
    all_cols <- base::names(x)
    correlation_cols <- all_cols[base::grepl("^[0-9]+$", all_cols)]

    # Build the gt table
    gt_result <- x |>
      tibble::rowid_to_column(var = "id") |>
      gt::gt(rowname_col = "id") |>
      gt::tab_options(quarto.disable_processing = TRUE) |>
      gt::cols_align(
        align = "center",
        columns = dplyr::everything()
      ) |>
      gt::cols_align(
        align = "left",
        columns = dplyr::any_of("variable")
      ) |>
      gt::tab_options(
        heading.title.font.size = gt::px(16),
        table.border.top.color = "white",
        table.border.top.width = gt::px(1),
        table_body.border.top.color = "white",
        table_body.border.top.width = gt::px(1),
        column_labels.border.top.width = gt::px(1),
        column_labels.border.top.color = "black",
        column_labels.border.bottom.width = gt::px(1),
        column_labels.border.bottom.color = "black",
        table_body.border.bottom.color = "black",
        table_body.border.bottom.width = gt::px(1),
        table.width = gt::pct(99),
        table.background.color = "white"
      ) |>
      gt::tab_style(
        style = base::list(
          gt::cell_borders(
            sides = base::c("top", "bottom", "left", "right"),
            color = "white",
            weight = gt::px(1)
          ),
          gt::cell_fill(color = "white", alpha = NULL)
        ),
        locations = base::list(
          gt::cells_stub(rows = dplyr::everything()),
          gt::cells_body(
            columns = dplyr::everything(),
            rows = dplyr::everything()
          )
        )
      ) |>
      gt::tab_style(
        style = base::list(
          gt::cell_text(weight = "bold")
        ),
        locations = gt::cells_row_groups(groups = dplyr::everything())
      ) |>
      gt::tab_style(
        style = base::list(
          gt::cell_borders(
            sides = base::c("top", "bottom"),
            color = "white",
            weight = gt::px(1)
          )
        ),
        locations = gt::cells_row_groups(groups = dplyr::everything())
      ) |>
      gt::cols_label(
        variable = "Variable",
        n_obs = gt::html("<i>N</i><sub>obs</sub>"),
        m = gt::html("<i>M</i>"),
        sd = gt::html("<i>SD</i>"),
        range = "Range",
        icc = " "
      ) |>
      gt::tab_spanner(
        label = "Descriptives",
        columns = dplyr::any_of(base::c("n_obs", "m", "sd", "range"))
      )

    # Add correlations spanner if correlation columns exist
    if (base::length(correlation_cols) > 0) {
      gt_result <- gt_result |>
        gt::tab_spanner(
          label = gt::html("Correlations<sup>a,b</sup>"),
          columns = dplyr::any_of(correlation_cols)
        )
    }

    # Add ICC spanner
    ci_percent <- base::round(ci * 100)
    gt_result <- gt_result |>
      gt::tab_spanner(
        label = "ICC ",
        columns = dplyr::any_of("icc")
      ) |>
      gt::tab_header(
        title = gt::html(
          base::paste0("<b>Table.</b> ", table_title)
        )
      ) |>
      gt::tab_source_note(
        source_note = gt::html(
          base::paste0(
            "<sup>a</sup> ",
            correlation_note,
            "<br>",
            "<sup>b</sup> All correlations marked with a star have ",
            ci_percent,
            "% credible intervals that exclude zero.<br><br>",
            "<i>Note.</i> ",
            note_text
          )
        )
      ) |>
      gt::opt_align_table_header(align = "left")

    return(gt_result)
  }

  return(result)
}