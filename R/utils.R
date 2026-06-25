# export "variable" as global variable
utils::globalVariables("variable")

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

# Warn once if any of `vars` look discrete; shared by mldesc()/bayes_mldesc()
# so the ICC's Gaussian-only limitation isn't silently applied to non-
# continuous variables.
.warn_discrete_icc_vars <- function(data, vars) {
  discrete_vars <- vars[base::vapply(vars, function(v) .is_discrete_like(data[[v]]), base::logical(1))]
  if (base::length(discrete_vars) > 0) {
    cli::cli_warn(c(
      "{cli::qty(discrete_vars)}{.val {discrete_vars}} {?looks/look} binary, ordinal, or count-like (few, whole-number values).",
      "i" = "The ICC is always computed from a linear (Gaussian) model regardless of measurement scale, which may not reflect a latent-scale ICC for non-continuous variables."
    ))
  }
}

# Shared footer-note builder for tbl_format_footer.mlstats_wb_tibble and
# tbl_format_footer.mlstats_desc_tibble: the correlation-note, significance-
# note, and method-note lines are identical for both classes.
.mlstats_footer_notes <- function(x, setup) {
  correlation_note <- base::attr(x, "correlation_note", exact = TRUE)
  if (base::is.null(correlation_note)) {
    flipped <- base::isTRUE(base::attr(x, "flipped"))
    correlation_note <- if (flipped) {
      "Between-group correlations above, within-group correlations below the diagonal."
    } else {
      "Within-group correlations above, between-group correlations below the diagonal."
    }
  }
  correlation_note <- base::paste0("ℹ ", correlation_note)

  significance_note <- base::paste0("ℹ ", base::attr(x, "significance_note", exact = TRUE))

  method <- base::attr(x, "method", exact = TRUE)
  method_note <- if (!base::is.null(method) && method == "sem") {
    "ℹ Correlations estimated via two-level SEM (lavaan)."
  } else if (!base::is.null(method) && method == "decomposition") {
    "ℹ Correlations estimated via variance decomposition."
  } else {
    NULL
  }

  notes <- base::c(
    format_comment(correlation_note, width = setup$width),
    format_comment(significance_note, width = setup$width)
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