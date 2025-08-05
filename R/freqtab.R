#' Create frequency table.
#'
#' @description The function creates a frequency table based on the values within a vector. The frequency table outputs the percentage and cumulative percentage based on the vector values.
#'
#' @param x A vector.
#'
#' @return A data frame or table containing details from the frequency table.
#'
#' @importFrom descr freq
#' @importFrom tibble rownames_to_column
#'
#' @export

freqtab <- function(x){

  sym <- deparse(substitute(x))

  descr::freq(ordered(x), plot = F) |>
    as.data.frame() |>
    tibble::rownames_to_column(var = sym)

}

