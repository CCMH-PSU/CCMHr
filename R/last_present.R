#' Extract the last non-NA value
#'
#' @param x A vector
#' @param order_by Another vector to sort `x` by
#'
#' @return The last non-NA value in `x`.
#' @export
#'
#' @examples
#'last_present(x = c(NA, 1, 2, NA), order_by = c(1, 2, 3, 4))
#'last_present(x = c(NA, 1, 2, NA), order_by = c(4, 3, 2, 1))
#'
last_present <- function(x, order_by) {
  stats::na.omit(x[order(order_by, decreasing = T)])[1L]
}
