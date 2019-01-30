#' Check CCAPS item values
#'
#' @description A function used to check the validity of the CCAPS items. All CCAPS
#' items should have values between 0 and 4, and possibly NA for missing values. This
#' function checks whether these constraints are satisfied, and if not, returns a vector
#' containing the CCAPS item(s) which violated these rules.
#'
#' @param dat The data frame containing the new year of (combined) data.
#' @note \code{CCAPSvalue_check} calls on first and last CCAPS items--CCAPS_01 and CCAPS_70
#' --by name, so they must be named properly in \code{dat}. Variable naming convention is
#' \code{CCAPS_01}, \code{CCAPS_70}. If \code{dat} does not contain the proper CCAPS
#' variable names (or the naming convention was changed in the new year's data), the
#' function will return an error. In addition, it is assumed that all CCAPS variables
#' exist between \code{CCAPS_01} and \code{CCAPS_70}.
#' @return A vector of strings containing the names of the CCAPS variables that have
#' at least one value out of bounds. If no variables have values out of bounds, the
#' returned value will be \code{NULL}.
#' @export


check_CCAPS <- function(dat){
  violate <- dat %>% dplyr::select(.data$CCAPS_01:.data$CCAPS_70) %>%
    dplyr::mutate_all(dplyr::funs(viol = !dplyr::between(.data,left = 0,right = 4)))

  sum_violate <- violate %>% dplyr::select(.data$CCAPS_01_viol:.data$CCAPS_70_viol) %>%
    dplyr::summarise_all(dplyr::funs(sum = sum(.data,na.rm = T)))

  total <- sum(sum_violate)

  if(total==0){
    message("CCAPS looks good!")
  } else{
    violations <- colnames(dplyr::select(dat,.data$CCAPS_01:.data$CCAPS_70))[which(sum_violate>0)]
    return(violations)
  }
}
