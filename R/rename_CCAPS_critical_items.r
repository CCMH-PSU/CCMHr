#' Rename the CCAPS critical items.
#'
#' @description The CCAPS critical items include CCAPS_51 (Suicide Ideation) and CCAPS_68 (Threat-to-others). To make these variables easier to identify, this function creates duplicates of the variables and makes the names of the duplicates more salient. Specifically, the duplicate of CCAPS_51 was named "CCAPS_SI", while "CCAPS_68" was named CCAPS_THO. The keep_all argument allows the programmer to specify if all variables in the original data frame should be returned (keep_all = `TRUE`) or if UniqueClientID, CcmhID, Date, Data_year, CCAPS_SI, and CCAPS_THO are returned (keep_all = `FALSE`).
#'
#' @param data  A data frame that contains the following variables: UniqueClientID, CcmhID, Date, Data_year, CCAPS_51, and CCAPS_68.
#' @param keep_all A logical argument to indicate if all columns of the original data frame should be kept. If `TRUE`, all columns of the original data frame are kept, and the new variables are added (i.e., CCAPS_SI, CCAPS_THO). If `FALSE`, the following columns are kept: UniqueClientID, CcmhID, Date, Data_year, CCAPS_SI, and CCAPS_THO. By default, `TRUE`.
#'
#' @return A data frame with new variables CCAPS_SI and CCAPS_THO. If keep_all is `TRUE`, all columns of the original data frame are kept. If keep_all is `FALSE`, UniqueClientID, CcmhID, Date, and Data_year are kept along with the new variables.
#'
#' @importFrom dplyr select
#'
#' @export

rename_CCAPS_critical_items <- function(data,
                                        keep_all = TRUE) {

  # Check if the correct variables are present
  if (!any(names(data) == "CCAPS_51") |
      !any(names(data) == "CCAPS_68")){

    stop("CCAPS_51 or CCAPS_68 are not present in data.")

  } else{

  }

  # Check if the UniqueClientID, CcmhID, Date, and Data_year are present
  if (!any(names(data) == "UniqueClientID") |
      !any(names(data) == "CcmhID") |
      !any(names(data) == "Date") |
      !any(names(data) == "Data_year") &
      keep_all == FALSE){

    stop("UniqueClientID, CcmhID, Date, or Data_year are not present in data.")

  }

  # Convert to data frame
  data <- as.data.frame(data)

  # Rename
  data$CCAPS_SI <- data$CCAPS_51
  data$CCAPS_THO <- data$CCAPS_68

  # Keep all is false
  if(keep_all == FALSE){

    data <- data |>
      dplyr::select(UniqueClientID, CcmhID,
                    Date, Data_year,
                    CCAPS_SI, CCAPS_THO)

  } else {

  }

  # Return
  return(data)

}
