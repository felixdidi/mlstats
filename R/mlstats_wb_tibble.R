#' @exportS3Method pillar::tbl_sum
tbl_sum.mlstats_wb_tibble <- function(x, ...) {
  pillar::style_subtle("Within- and Between-Group Correlations")
}

#' @exportS3Method pillar::tbl_format_footer
tbl_format_footer.mlstats_wb_tibble <- function(x, setup, ...) {
  default_footer <- base::NextMethod()

  bayesian <- base::isTRUE(base::attr(x, "bayesian"))

  significance_note <- if (bayesian) {
    attr(x, "significance_note", exact = TRUE)
  } else {
    "All correlations marked with a star are significant at p < .05."
  }
  
  # Check if matrix was flipped by looking for flip attribute
  flipped <- base::isTRUE(base::attr(x, "flipped"))
  
  if (flipped) {
    extra_footer <- pillar::style_subtle(base::paste0("# Between-group correlations above, within-group correlations below the diagonal.\n# ", significance_note))
  } else {
    extra_footer <- pillar::style_subtle(base::paste0("# Within-group correlations above, between-group correlations below the diagonal.\n# ", significance_note))
  }
  
  base::c(default_footer, extra_footer)
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