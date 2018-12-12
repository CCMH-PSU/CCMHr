#' A Check for the Values of SDS Items
#'
#' @description A function used to check the validity of the SDS items. Each SDS item has
#' a specific range of accepted values, and takes NA for missing values. This
#' function checks whether these constraints are satisfied, and if not, returns a list
#' containing the SDS item(s) which violated its/their constraints.
#'
#' @param dat The data frame containing the new year of (combined) data.
#' @note \code{SDSvalue_check} ...
#' @return A list with two elements. The first, \code{violations}, is a boolean indicating
#' whether or not any violations were found. The second, \code{SDS_viol}, is a vector of
#' strings containing the names of the SDS variables that have at least one value out of
#' bounds.
#' @example
#' @export

SDSvalue_check <- function(dat){

}
