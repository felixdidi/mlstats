# Add abbreviiation method for the custom vector class
#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.mlstats_stat <- function(x) {
  "mls"
}

# Add pillar method for the custom vector class
#' @exportS3Method pillar::pillar_shaft
pillar_shaft.mlstats_stat <- function(x, ...) {
  values <- vctrs::vec_data(x)
  
  # Apply subtle style to "–" characters
  styled_values <- ifelse(
    values == "\u2013",
    pillar::style_subtle(values),
    values
  )
  
  pillar::new_pillar_shaft_simple(
    styled_values,
    align = "right"
  )
}

# Method for as.numeric()
#' @export
as.numeric.mlstats_stat <- function(x, ...) {
  vctrs::vec_data(x) |> base::as.character() |> stringr::str_remove_all("\\*") |> stringr::str_remove_all(",") |> base::as.numeric()
}

# Method for as.double()
#' @export
as.double.mlstats_stat <- function(x, ...) {
  vctrs::vec_data(x) |> base::as.character() |> stringr::str_remove_all("\\*") |> stringr::str_remove_all(",") |> base::as.double()
}

# Method for as.integer()
#' @export
as.integer.mlstats_stat <- function(x, ...) {
  vctrs::vec_data(x) |> base::as.character() |> stringr::str_remove_all("\\*") |> stringr::str_remove_all(",") |> base::as.integer()
}

# Method for as.character()
#' @export
as.character.mlstats_stat <- function(x, ...) {
  vctrs::vec_data(x) |> base::as.character()
}