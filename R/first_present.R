#' Extract first or last non-NA value from a vector
#'
#' @description Returns the first or last non-NA value of `x` when sorted by `order_by`.
#'
#' @param x A vector
#' @param order_by Another vector to sort `x` by.
#' @param default A default value to use if the position does not exist in the input.
#'
#' @return The first or last non-NA value in `x`.
#' @note If there are no non-NA values, NA is returned.
#' @export
#'
#' @examples
#'first_present(x = c(NA, 1, 2, NA), order_by = c(4, 3, 2, 1))
#'last_present(x = c(NA, 1, 2, NA), order_by = c(4, 3, 2, 1))
#'
#'first_present(x = c(NA, 1, 2, NA), order_by = c(1, 2, 3, 4))
#'last_present(x = c(NA, 1, 2, NA), order_by = c(1, 2, 3, 4))

first_present <- function(x, order_by = NULL, default = NA) {
  nth(x, 1L, order_by = order_by, default = default)
}

#' @export
#' @rdname first_present

last_present <- function(x, order_by = NULL, default = NA) {
  nth(x, -1L, order_by = order_by, default = default)
}

