#' Selecting only centers who contributed data for all years.
#'
#' @param data A data frame with multiple years of CCMH data.
#'
#' @return A data frame with only centers contributing data for all data years included in the original data set.
#' @export
#'
#'
center_data_each_year <- function(data) {
  if(!"Data_year" %in% names(data) | !"CenterID" %in% names(data)) {
    print("Data_year or CenterID columns missing.")
  } else{
    if(sum(is.na(data$Data_year)) >0) {
      print("Data_year variable contains missing data.")
    } else{
      group_by(data, CenterID) %>%
        mutate(N = n_distinct(Data_year)) %>%
        filter(N == length(unique(data$Data_year))) %>%
        select(-N)
    }
  }
}
