#' CLICC Variable Check
#'
#' @description A function used to check the validity of the CLICC items. For CLICC_01_01
#' through CLICC_01_44, all values should be either 1 or NA. For CLICC_03, values between
#' 0 and 44, as well as NA for missing values, are tolerated. This function checks
#' whether these constraints are satisfied, and if not, returns a vector
#' containing the names of the CLICC item(s) which violated these rules.
#'
#' @param dat The data frame containing the new year of (combined) data.
#' @return
#' @note \code{CLICCvalue_check} calls on CLICC_01_01, CLICC_01_44, and CLICC_03
#' by name, so they must be named properly in \code{dat}. All other CLICC items
#' follow the same naming convention as \code{CLICC_01_01}
#' (e.g. \code{CLICC_01_02}, ...). If \code{dat} does not contain the proper CLICC
#' variable names (or the naming convention was changed in the new year's data), the
#' function will return an error. In addition, it is assumed that all CLICC variables
#' exist between \code{CLICC_01_01} and \code{CLICC_01_44}, with the exception of
#' \code{CLICC_03}.
#' @examples
#' @export


CLICCvalue_check <- function(dat){
  library(dplyr)

  one_or_na <- function(x){
    ifelse(is.na(x),T,ifelse(x==1,T,F))
  }

  sum_violate_01 <- dat %>%
    rlang::select(CLICC_01_01:CLICC_01_44) %>%
    rlang::mutate_all(funs(viol = !one_or_na(.))) %>%
    rlang::select(CLICC_01_01_viol:CLICC_01_44_viol) %>%
    rlang::summarise_all(funs(sum = sum(.,na.rm = T)))

  sum_violate_03 <- dat %>%
    rlang::select(CLICC_03) %>%
    rlang::mutate_all(funs(CLICC_03_viol = !between(.,left = 0,right = 44))) %>%
    rlang::select(CLICC_03_viol) %>%
    rlang::summarise_all(funs(sum = sum(.,na.rm = T)))

  total <- sum(sum_violate_01) + sum(sum_violate_03)

  if(total==0){
    violations <- NULL
  } else{
    violations <- colnames(select(dat,CLICC_01_01:CLICC_01_44,CLICC_03))[which(c(sum_violate_01,sum_violate_03)>0)]
  }
  return(violations)
}
