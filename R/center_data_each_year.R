#' Selecting only centers who contributed data for all years.
#'
#' @param data A data frame with multiple years of CCMH data.
#'
#' @return A data frame with only centers contributing data for all data years included in the original data set.
#' @export
#'
#' @examples
#'

center_data_each_year <- function(data) {
  if(!"Data_year" %in% names(data) | !"CenterID" %in% names(data)) {
    print("Data_year or CenterID columns missing.")
  } else{
    years <- length(unique(data$Data_year))
    group_by(data, Data_year, CenterID) %>%
      summarize(n()) %>%
      group_by(CenterID) %>%
      summarize(N = n()) %>%
      filter(N == years) %>%
      semi_join(data, ., by = "CenterID")
  }
}
