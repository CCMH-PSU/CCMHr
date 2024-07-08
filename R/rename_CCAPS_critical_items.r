#' Rename CCAPS critical items
#'
#' @param data  A data file.
#' @param keep_all A logical statement to indicate if all columns should be keeped. If `TRUE` will keep all columns of the orginal data frame and add CCAPS_SI and CCAPS_THO. If S`FALSE` will keep columns UniqueClientID, CcmhID, Date, Data_year, CCAPS_SI, and CCAPS_THO critical. Default is `TRUE`.
#'
#' @export
#'
#' 

rename_CCAPS_critical_items <- function(data, 
                                        keep_all = TRUE) {

    # Check if the correct variables are present
    if (!any(names(data) == "CCAPS_51") | !any(names(data) == "CCAPS_68")) {
      stop("CCAPS_51 or CCAPS_68 are not present in data")
    }

    # Check if the UniqueClientID, CcmhID, Date, and Data_year are present
    if (!any(names(data) == "UniqueClientID") | !any(names(data) == "CcmhID") | !any(names(data) == "Date") | !any(names(data) == "Data_year") & keep_all == FALSE) {
      stop("UniqueClientID, CcmhID, Date, or Data_year are not present in data")
    }

    # Convert to data frame
    data <- as.data.frame(data)

    # Rename 
    data$CCAPS_SI <- data$CCAPS_51
    data$CCAPS_THO <- data$CCAPS_68

    # Keep all is false
    if(keep_all == FALSE) {

      data <- data %>% 
        dplyr::select(UniqueClientID, CcmhID, 
                      Date, Data_year, 
                      CCAPS_SI, CCAPS_THO)

    } else {
      # Skip
    }

    # Return
    return(data)
    
}