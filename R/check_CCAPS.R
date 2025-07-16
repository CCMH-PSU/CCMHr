#' Check the response validity of the CCAPS items.
#'
#' @description Check a data frame to ensure all CCAPS items have 0, 1, 2, 3, 4, or NA values. If these constraints are not satisfied, the function returns a vector of the CCAPS item(s) that violated the constraints. To run this function, CCAPS variable names must be correctly named.
#'
#' @param dat A data file containing CCAPS items.
#'
#' @note \code{CCAPSvalue_check} calls on first and last CCAPS items--CCAPS_01 and CCAPS_70 --by name, so they must be named properly in \code{dat}. Variable naming convention is \code{CCAPS_01}, \code{CCAPS_70}. If \code{dat} does not contain the proper CCAPS variable names (or the naming convention was changed in the new year's data), the function will return an error. In addition, it is assumed that all CCAPS variables exist between \code{CCAPS_01} and \code{CCAPS_70}.
#'
#' @return If CCAPS item responses are valid, the function returns the message, "CCAPS looks good!" If CCAPS item responses are invalid, the function returns a vector of the CCAPS item(s) that violated the constraints.
#'
#' @export

check_CCAPS <- function(dat){

  # Detect violations
  violate <- dat |>
    dplyr::select(.data$CCAPS_01:.data$CCAPS_70) |>
    dplyr::mutate_all(list(viol = function(x) !dplyr::between(x, left = 0, right = 4)))

  # Sum violations
  sum_violate <- violate |>
    dplyr::select(.data$CCAPS_01_viol:.data$CCAPS_70_viol) |>
    dplyr::summarise_all(list(sum = function(x) sum(x, na.rm = TRUE)))

  # Total violations
  total <- sum(sum_violate)

  # Return message
  if(total==0){

    message("CCAPS looks good!")

  } else{

    violations <- colnames(dplyr::select(dat,.data$CCAPS_01:.data$CCAPS_70))[which(sum_violate>0)]

    return(violations)

  }
}
