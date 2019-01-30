#' grouply
#'
#' @param f A function
#' @param ... A grouping variable
#'
#' @export
#'
#' @examples
#' mtcars %>% grouply(dplyr::mutate, cyl)(M = mean(wt))
#'
grouply <- function(f, ...) {
  groups <- lazyeval::lazy_dots(...)

  function(tbl, ...) {
    dplyr::group_by_(tbl, .dots = groups) %>%
      f(...) %>%
      dplyr::ungroup()
  }
}
