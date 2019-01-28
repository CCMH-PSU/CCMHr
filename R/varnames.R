#' Check variable Names
#'
#' @description
#' This function is used to check the consistency of variable names for the new yearly data. Checks will be run against
#' the expected variable names (e.g. based on the previous year's variable names), and any inconsistencies
#' will be returned.
#'
#' @param old_names A vector of strings indicating the old (or expected) variable names in the data set.
#' The default is a vector of variable names coming from the 2016-2017 combined data, stored as an
#' Rdata file in the package, called `CCMHvars`.
#' @param dat The data frame containing the new year of (combined) data, with the variable names stored in `colnames(dat)`.
#' @return A list containing `Is_same`, `Omitted`, and `Extra`. `Is_same` is boolean-valued,
#' indicating whether or not the new variable names were consistent with the expected variable names. `Omitted`
#' is a vector of strings corresponding to the variables from `old_names` not appearing in `dat`.
#' `Extra` is a vector of strings corresponding to the variables in \code{dat} not appearing in `old_names`.
#' @examples
#' old_names <- letters[1:3]
#' dat <- as.data.frame(matrix(rnorm(12),3,4))
#' colnames(dat) <- c("a","B","c","x")
#' varnames(old_names,dat)
#' @export


varnames <- function(dat, old_names=NULL){
  if(is.null(old_names)){
    old_names <- CCMHvars
  }
  new_names <- colnames(dat)
  if(all(old_names %in% new_names) & all(new_names %in% old_names)){
    Is_same <- TRUE
    Omitted <- NULL
    Extra <- NULL
  } else{
    Is_same <- FALSE
    Omitted <- old_names[which((old_names %in% colnames(dat))==F)]
    Extra <- new_names[which((new_names %in% old_names)==F)]
  }
  out <- list("Is_same"= Is_same, "Omitted"= Omitted, "Extra"= Extra)
  return(out)
}
