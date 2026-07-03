#' @exportS3Method pillar::tbl_sum
tbl_sum.mlstats_wb_tibble <- function(x, ...) {
  group_label <- .group_title_label(base::attr(x, "group", exact = TRUE))
  pillar::style_subtle(base::paste0("Within- and Between-", group_label, " Correlations"))
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

#' @export
print.mlstats_wb_tibble <- function(
  x,
  format = "default",
  table_title = NULL,
  correlation_note = NULL,
  significance_note = NULL,
  note_text = NULL,
  ...
) {
  # Update attributes if custom values provided
  if (!base::is.null(table_title)) {
    attr(x, "table_title") <- table_title
  }
  if (!base::is.null(correlation_note)) {
    attr(x, "correlation_note") <- correlation_note
  }
  if (!base::is.null(significance_note)) {
    attr(x, "significance_note") <- significance_note
  }
  if (!base::is.null(note_text)) {
    attr(x, "note_text") <- note_text
  }

  group_label <- .group_title_label(base::attr(x, "group", exact = TRUE))
  table_title <- base::attr(x, "table_title", exact = TRUE)
  if (base::is.null(table_title) || !base::nzchar(table_title)) {
    table_title <- base::paste0("Within- and Between-", group_label, " Correlations")
  }
  correlation_note <- base::attr(x, "correlation_note", exact = TRUE)
  significance_note <- base::attr(x, "significance_note", exact = TRUE)
  note_text <- base::attr(x, "note_text", exact = TRUE)

  if (format == "gt") {
    rlang::check_installed("gt", reason = "to render tables in gt format")
    all_cols <- base::names(x)
    correlation_cols <- all_cols[base::grepl("^[0-9]+$", all_cols)]

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
        table_body.border.bottom.width = gt::px(1),
        table_body.border.bottom.color = "black",
        table.width = gt::pct(99),
        table.background.color = "white"
      ) |>
      gt::tab_style(
        style = base::list(
          gt::cell_borders(
            sides = base::c("top", "bottom", "left", "right"),
            weight = gt::px(0)
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
      gt::cols_label(variable = "Variable")

    # Add correlations spanner if correlation columns exist
    if (base::length(correlation_cols) > 0) {
      gt_result <- gt_result |>
        gt::tab_spanner(
          label = gt::html("Correlations<sup>a,b</sup>"),
          columns = dplyr::any_of(correlation_cols)
        )
    }

    gt_result <- gt_result |>
      gt::tab_header(
        title = gt::html(table_title)
      ) |>
      gt::tab_source_note(
        source_note = gt::html(note_text)
      ) |>
      gt::tab_source_note(
        source_note = gt::html(
          base::paste0(
            "<sup>a</sup> ",
            correlation_note
          )
        )
      ) |>
      gt::tab_source_note(
        source_note = gt::html(
          base::paste0(
            "<sup>b</sup> ",
            significance_note
          )
        )
      ) |>
      gt::opt_align_table_header(align = "left")

    return(gt_result)
  } else if (format == "tt") {

    all_cols <- base::names(x)
    correlation_cols <- all_cols[base::grepl("^[0-9]+$", all_cols)]

    tt_data <-
      x |>
      dplyr::rename_with(
        ~ dplyr::case_when(
          .x == "variable" ~ "Variable",
          .x %in% correlation_cols ~ stringr::str_c("Correlations^a,b^__", .x),
          TRUE ~ .x
        )
      ) |>
      tibble::rowid_to_column(var = "id")
    base::names(tt_data)[1] <- ""

    tt_result <-
      tt_data |>
      tinytable::tt(
        caption = table_title,
        notes = list(
          stringr::str_c("*Note.* ", note_text),
          a = correlation_note,
          b = significance_note
        )
      ) |>
      tinytable::group_tt(j = "__") |>
      tinytable::format_tt(markdown = TRUE)

    return(tt_result)

  } else {
    base::NextMethod()
  }
}
