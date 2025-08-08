#' Load an RDA file into a new object name.
#'
#' @name loadRDa
#'
#' @description A function to load .rda and .rdata files with the ability to assign them a new object name instead of the original name.
#'
#' @param fileName The file path to a .rda or .rdata file to be loaded into a new object name.
#'
#' @return A data object.
#'
#' @export

loadRDa <- function(fileName){

  # loads an RData file, and returns it
  load(fileName)

  # Change name of the data object
  get(ls()[ls() != "fileName"])

}

#' @rdname loadRDa
#' @export
loadrda <- loadRDa
