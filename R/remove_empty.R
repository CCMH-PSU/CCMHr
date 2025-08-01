#' Exclude columns that have complete missing data.
#'
#' @description Some variables/columns could have complete missing data. This function excludes variables/columns that are completely missing.
#'
#' @param data A data frame.
#'
#' @return A data frame without variables/columns that are completely missing.
#'
#' @import dplyr
#'
#' @export

remove_empty <- function(data){

  # Exclude columns that have complete missing data
  data <- data |>
    dplyr::select_if(~!all(is.na(.)))

  # Return
  return(data)

}
