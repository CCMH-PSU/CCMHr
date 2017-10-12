#' A Check for the Values of CCAPS Items
#'
#' @description A function used to check the validity of the CCAPS items. All CCAPS
#' items should have values between 0 and 4, and possibly NA for missing values. This
#' function checks whether these constraints are satisfied, and if not, returns a list
#' containing the CCAPS item(s) which violated these rules, and their associated
#' offending value(s).
#'
#' @param dat The data frame containing the new year of (combined) data.
#' @note \code{CCAPSvalue_check} calls on CCAPS variables by name, so they must be named
#' properly in \code{dat}. Variable naming convention is \code{CCAPS_01}, \code{CCAPS_02},
#' etc. If \code{dat} does not contain the proper CCAPS variable names (or the naming
#' convention was changed in the new year's data), the function will return an error.
#' @return A list with two elements. The first, \code{violations}, is a boolean indicating
#' whether or not any violations were found. The second, \code{CCAPS_viol}, is a vector of
#' strings containing the names of the CCAPS variables that have at least one value out of
#' bounds.
#' @example


CCAPSvalue_check <- function(dat){
  library(dplyr)
  violate <- dat %>% select(CCAPS_01:CCAPS_70) %>%
    mutate_all(funs(viol = !between(.,left = 0,right = 4)))

  sum_violate <- violate %>% select(CCAPS_01_viol:CCAPS_70_viol) %>%
    summarise_all(funs(sum = sum(.,na.rm = T)))

  total <- sum(sum_violate)

  if(total==0){
    violations <- FALSE
    CCAPS_viol <- NULL
  } else{
    violations <- TRUE
    CCAPS_viol <- colnames(select(dat,CCAPS_01:CCAPS_70))[which(sum_violate>0)]
  }
  out <- list("violations" = violations,"CCAPS_viol" = CCAPS_viol)
  return(out)
}
