#' Selecting only centers who contributed data for all years.
#'
#' @param data A data frame with multiple years of CCMH data.
#'
#' @return A data frame with only centers contributing data for all data years included in the original data set.
#' @export
#'
#'
center_data_each_year <- function(data) {
  if(!"Data_year" %in% names(data) | !"CcmhID" %in% names(data)) {
    stop("Data_year or CcmhID columns missing.")
  } else{
    if(sum(is.na(data$Data_year) | is.na(data$CcmhID)) >0) {
      stop("Data_year variable contains missing data.")
    } else{
      dplyr::group_by(data, .data$CcmhID) %>%
        dplyr::filter(dplyr::n_distinct(data$Data_year) == length(unique(.data$Data_year))) %>%
        dplyr::ungroup() %>%
        return()
    }
  }
}
