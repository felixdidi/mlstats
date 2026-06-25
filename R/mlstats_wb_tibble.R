#' @exportS3Method pillar::tbl_sum
tbl_sum.mlstats_wb_tibble <- function(x, ...) {
  pillar::style_subtle("Within- and Between-Group Correlations")
}

#' @exportS3Method pillar::tbl_format_footer
tbl_format_footer.mlstats_wb_tibble <- function(x, setup, ...) {
  default_footer <- base::NextMethod()
  base::c(default_footer, .mlstats_footer_notes(x, setup)) |> pillar::style_subtle()
}

#' @exportS3Method pillar::ctl_new_pillar
ctl_new_pillar.mlstats_wb_tibble <- function(controller, x, width, ..., title = NULL) {
  out <- base::NextMethod()
  .mlstats_new_pillar_from_out(out)
}