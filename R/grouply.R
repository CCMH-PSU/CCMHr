#' A function that combines grouping and a function.
#'
#' @name grouply
#'
#' @description A function that combines grouping of a data frame by a variable and conducting analysis of the data frame by a function.
#'
#' @param f A function.
#' @param ... A unquoted string to specify a grouping variable.
#'
#' @return A data object.
#'
#' @importFrom lazyeval lazy_dots
#' @importFrom dplyr group_by_
#' @importFrom dplyr ungroup
#'
#' @export

grouply <- function(f, ...){

  groups <- lazyeval::lazy_dots(...)

  function(tbl, ...){

    dplyr::group_by_(tbl, .dots = groups) |>
      f(...) |>
      dplyr::ungroup()

  }

}
