# Add pillar methods for the custom tibble class
#' @exportS3Method pillar::tbl_sum
tbl_sum.mlstats_desc_tibble <- function(x, ...) {
  table_title <- attr(x, "table_title", exact = TRUE)
  if (is.null(table_title) || !base::nzchar(table_title)) {
    table_title <- "Multilevel Descriptive Statistics"
  }
  pillar::style_subtle(table_title)
}

#' @exportS3Method pillar::tbl_format_footer
tbl_format_footer.mlstats_desc_tibble <- function(x, setup, ...) {
  default_footer <- base::NextMethod()

  note_text <- base::paste0("\u2139 ", base::attr(x, "note_text", exact = TRUE))

  base::c(
    default_footer,
    .mlstats_footer_notes(x, setup),
    format_comment(note_text, width = setup$width)
  ) |>
    pillar::style_subtle()
}

#' @exportS3Method pillar::ctl_new_pillar
ctl_new_pillar.mlstats_desc_tibble <- function(controller, x, width, ..., title = NULL) {
  out <- base::NextMethod()
  .mlstats_new_pillar_from_out(out)
}

#' @export
print.mlstats_desc_tibble <- function(
  x,
  format = "default",
  table_title = NULL,
  correlation_note = NULL,
  significance_note = NULL,
  note_text = NULL,
  ...
) {
  # Update attributes if custom values provided
  if (!is.null(table_title)) {
    attr(x, "table_title") <- table_title
  }
  if (!is.null(correlation_note)) {
    attr(x, "correlation_note") <- correlation_note
  }
  if (!is.null(significance_note)) {
    attr(x, "significance_note") <- significance_note
  }
  if (!is.null(note_text)) {
    attr(x, "note_text") <- note_text
  }

  table_title <- attr(x, "table_title", exact = TRUE)
  if (base::is.null(table_title) || !base::nzchar(table_title)) {
    table_title <- "Multilevel Descriptive Statistics"
  }
  correlation_note <- attr(x, "correlation_note", exact = TRUE)
  significance_note <- attr(x, "significance_note", exact = TRUE)
  note_text <- attr(x, "note_text", exact = TRUE)

  if (format == "gt") {
    rlang::check_installed("gt", reason = "to render tables in gt format")
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
      )

    # Only set labels for columns that exist
    col_labels <- base::list()
    if ("variable" %in% all_cols) {
      col_labels$variable <- "Variable"
    }
    if ("n_obs" %in% all_cols) {
      col_labels$n_obs <- gt::html("<i>N</i><sub>obs</sub>")
    }
    if ("m" %in% all_cols) {
      col_labels$m <- gt::html("<i>M</i>")
    }
    if ("sd" %in% all_cols) {
      col_labels$sd <- gt::html("<i>SD</i>")
    }
    if ("range" %in% all_cols) {
      col_labels$range <- "Range"
    }
    if ("icc" %in% all_cols) {
      col_labels$icc <- " "
    }

    if (base::length(col_labels) > 0) {
      gt_result <- rlang::exec(gt::cols_label, gt_result, !!!col_labels)
    }

    gt_result <- gt_result |>
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
    gt_result <- gt_result |>
      gt::tab_spanner(
        label = "ICC ",
        columns = dplyr::any_of("icc")
      ) |>
      gt::tab_header(
        title = gt::html(table_title)
      ) |>
      gt::tab_source_note(
        source_note = gt::md(note_text)
      ) |>
      gt::tab_source_note(
        source_note = gt::md(
          base::paste0(
            "<sup>a</sup> ",
            correlation_note
          )
        )
      ) |>
      gt::tab_source_note(
        source_note = gt::md(
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
          .x == "n_obs" ~ "Descriptives__*N*<sub>obs</sub>",
          .x == "m" ~ "Descriptives__*M*",
          .x == "sd" ~ "Descriptives__*SD*",
          .x == "range" ~ "Descriptives__Range",
          .x == "icc" ~ "ICC__",
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