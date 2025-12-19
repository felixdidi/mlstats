# export "variable" as global variable
utils::globalVariables("variable")

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