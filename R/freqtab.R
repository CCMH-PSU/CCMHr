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
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#'
#' @export

freqtab <- function(x){

  sym <- deparse(substitute(x))

  data <- descr::freq(ordered(x),
                      plot = FALSE) |>
    as.data.frame() |>
    tibble::rownames_to_column(var = sym)

  # Detecting if Valid Percent is in data
  if (!"Valid Percent" %in% colnames(data)) {

    data <- data |>
      dplyr::mutate(`Valid Percent` = Percent) |>
      dplyr::relocate(`Valid Percent`, .after = Percent)

  } else {

  }

  return(data)

}

