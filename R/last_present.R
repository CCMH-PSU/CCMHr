#' Extract the last non-NA value
#'
#' @description Returns the last non-NA value of `x` when sorted by `order_by`.
#'
#'
#' @param x A vector
#' @param order_by Another vector to sort `x` by
#'
#' @return The last non-NA value in `x`.
#' @note If there are no non-NA values, NA is returned.
#' @export
#'
#' @examples
#'last_present(x = c(NA, 1, 2, NA), order_by = c(1, 2, 3, 4))
#'last_present(x = c(NA, 1, 2, NA), order_by = c(4, 3, 2, 1))
#'
last_present <- function(x, order_by = Date) {
  stats::na.omit(x[order(order_by, decreasing = T)])[1L]
}
