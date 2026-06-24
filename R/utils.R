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