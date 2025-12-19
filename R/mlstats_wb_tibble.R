#' @exportS3Method pillar::tbl_sum
tbl_sum.mlstats_wb_tibble <- function(x, ...) {
  pillar::style_subtle("Within- and Between-Group Correlations")
}

#' @exportS3Method pillar::tbl_format_footer
tbl_format_footer.mlstats_wb_tibble <- function(x, setup, ...) {
default_footer <- base::NextMethod()
  
  # Get correlation note from attribute
  correlation_note <- base::attr(x, "correlation_note", exact = TRUE)
  if (base::is.null(correlation_note)) {
    # Fallback if not set
    flipped <- base::isTRUE(base::attr(x, "flipped"))
    correlation_note <- if (flipped) {
      "Between-group correlations above, within-group correlations below the diagonal."
    } else {
      "Within-group correlations above, between-group correlations below the diagonal."
    }
  }

  correlation_note <- base::paste0("\u2139 ", correlation_note)

  significance_note <- base::paste0("\u2139 ", base::attr(x, "significance_note", exact = TRUE))

  base::c(
    default_footer,
    format_comment(correlation_note, width = setup$width),
    format_comment(significance_note, width = setup$width)
  ) |> 
  pillar::style_subtle()
}

#' @exportS3Method pillar::ctl_new_pillar
ctl_new_pillar.mlstats_wb_tibble <- function(controller, x, width, ..., title = NULL) {
  out <- base::NextMethod()
  width <- ifelse(base::attr(out$data, "width") > 5, base::attr(out$data, "width"), 5)
  rule_char <- pillar::style_subtle(strrep("=", width))
  mid_rule_char <- pillar::style_subtle(strrep("-", width))
  
  pillar::new_pillar(list(
    top_rule = pillar::new_pillar_component(list(rule_char), width = width),
    title = out$title,
    # type = out$type,
    mid_rule = pillar::new_pillar_component(list(mid_rule_char), width = width),
    data = out$data,
    bottom_rule = pillar::new_pillar_component(list(rule_char), width = width)
  ))
}