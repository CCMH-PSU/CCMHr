abort_if_not <- function(...) {
  tryCatch(stopifnot(...), simpleError = function(e) rlang::abort(e))
}

nth <- function(x, n, order_by = NULL, default = NA) {
  abort_if_not(length(n) == 1, is.numeric(n))
  n <- trunc(n)

  if (!is.null(order_by)) {
    x <- x[order(order_by)]
    x <- stats::na.omit(x)
  } else {
    x <- stats::na.omit(x)
  }


  if (n == 0 || n > length(x) || n < -length(x)) {
    return(default)
  }

  # Negative values index from RHS
  if (n < 0) {
    n <- length(x) + n + 1
    x[[n]]
  } else {
    x[[n]]
  }
}
