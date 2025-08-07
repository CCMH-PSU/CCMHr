#' Extract the first value from a vector.
#'
#' @description The function extracts the first non-NA value from a vector.
#'
#' @param x A vector.
#' @param order_by Another vector to sort `x` by.
#' @param default A numeric value to use if the `x` position does not exist in the input.
#'
#' @return The first non-NA value in a vector.
#'
#' @importFrom dplyr nth
#'
#' @export

first_present <- function(x,
                          order_by = NULL,
                          default = NULL){

  dplyr::nth(x,
             1L,
             order_by = order_by,
             default = default,
             na_rm = TRUE)

}

#' Extract the last value from a vector.
#'
#' @description The function extracts the last non-NA value from a vector.
#'
#' @param x A vector.
#' @param order_by Another vector to sort `x` by.
#' @param default A numeric value to use if the `x` position does not exist in the input.
#'
#' @return The last non-NA value in a vector.
#'
#' @importFrom dplyr nth
#'
#' @export

last_present <- function(x,
                         order_by = NULL,
                         default = NULL){

  dplyr::nth(x,
             -1L,
             order_by = order_by,
             default = default,
             na_rm = TRUE)

}

