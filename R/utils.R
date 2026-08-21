# export "variable" as global variable
utils::globalVariables("variable")

# Evaluate a naming pattern by substituting {col} and {group} placeholders.
# Mirrors the two tokens that dplyr's .names argument resolves via glue.
.eval_name_pattern <- function(pattern, col, group) {
  result <- base::gsub("{col}", col, pattern, fixed = TRUE)
  result <- base::gsub("{group}", group, result, fixed = TRUE)
  result
}

# Validate that `group` and `vars` are present in `data`, with helpful errors.
.validate_group_vars <- function(data, group, vars) {
  if (!group %in% base::names(data)) {
    cli::cli_abort(c(
      "Group variable {.val {group}} not found in {.arg data}.",
      "i" = "Available columns: {.val {base::names(data)}}."
    ))
  }

  missing_vars <- base::setdiff(vars, base::names(data))
  if (base::length(missing_vars) > 0) {
    cli::cli_abort(c(
      "Variable{?s} {.val {missing_vars}} not found in {.arg data}.",
      "i" = "Available columns: {.val {base::names(data)}}."
    ))
  }

  all_na_vars <- vars[base::vapply(
    vars,
    function(v) base::all(base::is.na(data[[v]])),
    base::logical(1)
  )]
  if (base::length(all_na_vars) > 0) {
    cli::cli_abort(c(
      "Variable{?s} {.val {all_na_vars}} contain{?s/} only missing values.",
      "i" = "All variables passed to {.arg vars} must have at least one non-missing value."
    ))
  }
}

# Warn about (and drop) observations with a missing value on the grouping
# variable. Left in the data, dplyr::group_by() would treat NA as a group of
# its own, so those rows would form a spurious extra group in the between-
# group statistics while lme4::lmer() silently drops them from the ICC
# models -- two different samples inside the same table. Used by mldesc()
# and within_between_correlations(); decompose_within_between() keeps the
# rows and NAs their components instead (see there).
.drop_na_group <- function(data, group) {
  na_group <- base::is.na(data[[group]])
  n_na <- base::sum(na_group)
  if (n_na > 0) {
    cli::cli_warn(c(
      "{n_na} observation{?s} {?has/have} a missing value on the grouping variable {.val {group}}.",
      "i" = "{cli::qty(n_na)}{?This observation is/These observations are} excluded from all computations."
    ))
    data <- data[!na_group, , drop = FALSE]
  }
  data
}

# Heuristic: does `x` look binary/ordinal/count-like (few, whole-number
# values) rather than continuous? Used to warn that ICC is always computed
# from a linear (Gaussian) model regardless of measurement scale.
.is_discrete_like <- function(x) {
  x <- x[!base::is.na(x)]
  if (base::length(x) == 0 || !base::is.numeric(x)) {
    return(FALSE)
  }
  is_whole <- base::all(x == base::round(x))
  n_unique <- base::length(base::unique(x))
  is_whole && n_unique <= 10
}

# Warn once if any of `vars` look discrete; used by mldesc() (for every
# `method`, including "bayes") so the ICC's Gaussian-only limitation isn't
# silently applied to non-continuous variables.
.warn_discrete_icc_vars <- function(data, vars) {
  discrete_vars <- vars[base::vapply(vars, function(v) .is_discrete_like(data[[v]]), base::logical(1))]
  if (base::length(discrete_vars) > 0) {
    cli::cli_warn(c(
      "{cli::qty(discrete_vars)}{.val {discrete_vars}} {?looks/look} binary, ordinal, or count-like (few, whole-number values).",
      "i" = "The ICC is always computed from a linear (Gaussian) model regardless of measurement scale, which may not reflect a latent-scale ICC for non-continuous variables."
    ))
  }
}

# Sampling settings for every brms::brm() fit under method = "bayes" in
# mldesc()/within_between_correlations(). Overridable via
# options(mlstats.brms_iter = ..., mlstats.brms_chains = ...) so tests (or
# advanced users) can fit much shorter chains; defaults match brms::brm()'s
# own defaults for chains, and the iter the package has always used.
.brms_iter <- function() {
  base::getOption("mlstats.brms_iter", 5000)
}

.brms_chains <- function() {
  base::getOption("mlstats.brms_chains", 4)
}

# Sentence-case label for the grouping variable, used in table titles (e.g.
# "Within- and Between-Person Correlations"). Falls back to "Group" when no
# group name is available (e.g. an object with the `group` attribute
# stripped).
.group_title_label <- function(group) {
  if (base::is.null(group) || !base::nzchar(group)) {
    return("Group")
  }
  base::paste0(
    base::toupper(base::substr(group, 1, 1)),
    base::substr(group, 2, base::nchar(group))
  )
}

# Lowercase label for the grouping variable, used inline in notes (e.g.
# "within-person correlations above..."). Falls back to "group".
.group_note_label <- function(group) {
  if (base::is.null(group) || !base::nzchar(group)) {
    return("group")
  }
  base::tolower(group)
}

# Reports the number of groups, the total number of observations, and
# observations per group.
.group_size_note <- function(data, group) {
  group_sizes <- base::table(data[[group]])
  n_groups <- base::length(group_sizes)
  n_total <- base::sum(group_sizes)
  group_label <- .group_note_label(group)
  group_label_plural <- base::paste0(group_label, if (n_groups == 1) "" else "s")
  n_total_fmt <- scales::comma(n_total)

  min_n <- base::min(group_sizes)
  max_n <- base::max(group_sizes)

  size_text <- if (min_n == max_n) {
    base::paste0(min_n, " per ", group_label)
  } else {
    med_n <- stats::median(group_sizes)
    med_fmt <- if (med_n == base::round(med_n)) {
      base::sprintf("%.0f", med_n)
    } else {
      base::sprintf("%.1f", med_n)
    }
    base::paste0(
      "median ", med_fmt, " per ", group_label,
      "; range: ", min_n, "\u2013", max_n
    )
  }

  base::paste0(
    "Based on ", n_groups, " ", group_label_plural, " and ", n_total_fmt,
    " observations (", size_text, ")."
  )
}

# Shared footer-note builder for tbl_format_footer.mlstats_wb_tibble and
# tbl_format_footer.mlstats_desc_tibble: the correlation-note, significance-
# note, and method-note lines are identical for both classes.
.mlstats_footer_notes <- function(x, setup) {
  correlation_note <- base::attr(x, "correlation_note", exact = TRUE)
  if (base::is.null(correlation_note)) {
    group_label <- .group_note_label(base::attr(x, "group", exact = TRUE))
    flipped <- base::isTRUE(base::attr(x, "flipped"))
    correlation_note <- if (flipped) {
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
  }
  correlation_note <- base::paste0("\u2139 ", correlation_note)

  significance_note_val <- base::attr(x, "significance_note", exact = TRUE)
  significance_note <- if (base::is.null(significance_note_val)) {
    NULL
  } else {
    base::paste0("\u2139 ", significance_note_val)
  }

  group_size_note_val <- base::attr(x, "group_size_note", exact = TRUE)
  group_size_note <- if (base::is.null(group_size_note_val)) {
    NULL
  } else {
    base::paste0("\u2139 ", group_size_note_val)
  }

  method <- base::attr(x, "method", exact = TRUE)
  method_note <- if (!base::is.null(method) && method == "sem") {
    "\u2139 Correlations estimated via two-level SEM (lavaan)."
  } else if (!base::is.null(method) && method == "decomposition") {
    "\u2139 Correlations estimated via variance decomposition."
  } else if (!base::is.null(method) && method == "bayes") {
    "\u2139 Correlations estimated via Bayesian multilevel models (brms)."
  } else {
    NULL
  }

  notes <- base::c(
    format_comment(correlation_note, width = setup$width),
    format_comment(significance_note, width = setup$width),
    format_comment(group_size_note, width = setup$width)
  )
  if (!base::is.null(method_note)) {
    notes <- base::c(notes, format_comment(method_note, width = setup$width))
  }
  notes
}

# Shared body for ctl_new_pillar.mlstats_wb_tibble and
# ctl_new_pillar.mlstats_desc_tibble; `out` is the result of `NextMethod()`,
# which must be called from within the actual dispatched S3 method.
.mlstats_new_pillar_from_out <- function(out) {
  width <- base::ifelse(base::attr(out$data, "width") > 5, base::attr(out$data, "width"), 5)
  rule_char <- pillar::style_subtle(base::strrep("=", width))
  mid_rule_char <- pillar::style_subtle(base::strrep("-", width))

  pillar::new_pillar(list(
    top_rule = pillar::new_pillar_component(list(rule_char), width = width),
    title = out$title,
    mid_rule = pillar::new_pillar_component(list(mid_rule_char), width = width),
    data = out$data,
    bottom_rule = pillar::new_pillar_component(list(rule_char), width = width)
  ))
}

# Implement functionality from pillar:::format_comment
format_comment <- function(x, width) {
  if (length(x) == 0L) {
    return(character())
  }
  vapply(x, wrap, character(1), prefix = "# ", width = min(width, cli::console_width()))
}

NBSP <- "\U00A0"

wrap <- function(..., indent = 0, prefix = "", width) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap2(x, width - pillar::get_extent(prefix), indent)
  wrapped <- paste0(prefix, wrapped)
  wrapped <- gsub(NBSP, " ", wrapped)

  paste0(wrapped, collapse = "\n")
}

strwrap2 <- function(x, width, indent) {
  cli::ansi_strwrap(x, width = max(width, 0), indent = indent, exdent = indent + 2, simplify = FALSE)
}