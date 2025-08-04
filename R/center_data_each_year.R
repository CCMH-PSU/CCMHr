#' Filter for centers that contributed data for all years within a data frame.
#'
#' @description A center may contribute data for research each year, some years, or a single year. For some analyses, it could be critical to include only centers that contribute data annually within a data frame. This function excludes centers that did not contribute research data every year. A valid "Data_year" and "CcmhID" variables must exist in the data frame to run the function.
#'
#' @param data A data frame with multiple years of data.
#'
#' @return A data frame with only centers contributing data for all years included in the original data frame.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr n_distinct
#' @importFrom dplyr ungroup
#'
#' @export
#'

center_data_each_year <- function(data){

  # Check for Data_year and CcmhID variables
  if(!"Data_year" %in% names(data) |
     !"CcmhID" %in% names(data)){

    # Message
    stop("Data_year or CcmhID columns missing.")

  } else{

    # Check for missing data in Data_year or CcmhID variables
    if(sum(is.na(data$Data_year) |
           is.na(data$CcmhID)) > 0) {

      stop("Data_year variable contains missing data.")

    } else{

      # Detect max number of years
      max_years <- length(unique(data$Data_year))

      # Filter for centers with data for all years
      data <- data |>
        dplyr::group_by(CcmhID) |>
        dplyr::filter(dplyr::n_distinct(Data_year) == max_years) |>
        dplyr::ungroup()

      # Return data frame
      return(data)

    }

  }

}
