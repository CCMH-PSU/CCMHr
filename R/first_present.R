#' Extract first non-NA value
#'
#' @param x A vector
#' @param order_by Another vector to sort `x` by.
#'
#' @return The first non-NA value in `x`.
#' @export
#'
#' @examples
#'first_present(x = c(NA, 1, 2, NA), order_by = c(1, 2, 3, 4))
#'first_present(x = c(NA, 1, 2, NA), order_by = c(4, 3, 2, 1))
#'
first_present <- function(x, order_by) {
  na.omit(x[order(order_by)])[1L]
}
