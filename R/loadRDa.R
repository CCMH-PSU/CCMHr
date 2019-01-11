#' Title loadRDa
#'
#' @description Loads .rda and .rdata files with the ability to assign them to a new object name instead of the one they were originally saved with.
#'
#' @param fileName The file path to a .rda or .rdata file to be loaded into a new object name
#'
#' @return
#' @export
#'
#' @examples new_name <- loadRDa("filepath/file.rda")
#'
loadRDa <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
