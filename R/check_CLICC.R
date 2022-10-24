#' CLICC Variable Check
#'
#' @description A function used to check the validity of the CLICC items. For CLICC_01_01
#' through CLICC_01_44, all values should be either 1 or NA. For CLICC_03, values between
#' 0 and 44, as well as NA for missing values, are tolerated. This function checks
#' whether these constraints are satisfied, and if not, returns a vector
#' containing the names of the CLICC item(s) which violated these rules.
#'
#' @param dat The data frame containing the new year of (combined) data.
#' @note \code{CLICCvalue_check} calls on CLICC_01_01, CLICC_01_44, and CLICC_03
#' by name, so they must be named properly in \code{dat}. All other CLICC items
#' follow the same naming convention as \code{CLICC_01_01}
#' (e.g. \code{CLICC_01_02}, ...). If \code{dat} does not contain the proper CLICC
#' variable names (or the naming convention was changed in the new year's data), the
#' function will return an error. In addition, it is assumed that all CLICC variables
#' exist between \code{CLICC_01_01} and \code{CLICC_01_44}, with the exception of
#' \code{CLICC_03}.
#' @export


check_CLICC <- function(dat){

  one_or_na <- function(x){
    ifelse(is.na(x),T,ifelse(x==1,T,F))
  }

  sum_violate_01 <- dat %>%
    dplyr::select(.data$CLICC_01_01:.data$CLICC_01_44) %>%
    dplyr::mutate_all(dplyr::funs(viol = !one_or_na(.))) %>%
    dplyr::select(.data$CLICC_01_01_viol:.data$CLICC_01_44_viol) %>%
    dplyr::summarise_all(dplyr::funs(sum = sum(.,na.rm = T)))

  sum_violate_03 <- dat %>%
    dplyr::select(.data$CLICC_03) %>%
    dplyr::mutate_all(dplyr::funs(CLICC_03_viol = !dplyr::between(.,left = 0,right = 44))) %>%
    dplyr::select(.data$CLICC_03_viol) %>%
    dplyr::summarise_all(dplyr::funs(sum = sum(.,na.rm = T)))

  total <- sum(sum_violate_01) + sum(sum_violate_03)

  if(total==0){
    violations <- NULL
  } else{
    violations <- colnames(dplyr::select(dat,.data$CLICC_01_01:.data$CLICC_01_44,.data$CLICC_03))[which(c(.data$sum_violate_01,.data$sum_violate_03)>0)]
  }
  return(violations)
}
