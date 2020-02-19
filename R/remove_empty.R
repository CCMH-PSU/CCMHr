#' Remove empty variables
#' @description Used in data request cleaning to remove variables that don't contain any data
#'
#' @param data A data object
#'
#' @return The data object without any variables that were completely empty
#' @export
#'

remove_empty <- function(data) {
  dplyr::select_if(data, ~!all(is.na(.)))
}
