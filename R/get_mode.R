#' Get the mode of a vector
#'
#' @param x a vector
#'
#' @return Mode of x
#' @export


get_mode <- function(x) {
  values <- unique(na.omit(x))
  values[which.max(tabulate(match(x, values)))]
}

