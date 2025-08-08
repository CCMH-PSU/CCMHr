#' Extract the mode value from a vector.
#'
#' @name get_mode
#'
#' @description The function extracts the mode value(s) from a vector. `NA` responses are omitted.
#'
#' @param x A vector.
#'
#' @return The mode value(s) within a vector.
#'
#' @export

get_mode <- function(x) {

  # Removing NA values from x
  x <- x[!is.na(x)]

  # Getting unique values from x
  u <- unique(x)

  # Tabulating the number of times each unique value appears in x
  tab <- tabulate(match(x, u))

  # Returning the value(s) that appear most frequently
  u[tab == max(tab)]

}

