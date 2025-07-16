#' Check variable names.
#'
#' @description This function is used to check the consistency of variable names for the new yearly data. Checks will be run against the expected variable names (e.g., based on the variable names from the previous year), and any inconsistencies will be identified and returned.
#'
#' @param data A regular data frame or a data frame that contains a new year of CCMH data.
#' @param old_names A vector of strings indicating the old (or expected) variable names in the data frame. If `NULL`, a vector of variable names coming from the 2016-2017 combined data, stored as an Rdata file in the package, called "CCMHvars". By default, `NULL`.
#'
#' @return A list containing "Is_same", "Omitted", and "Extra". "Is_same" is a logical value, indicating whether or not the new variable names were consistent with the expected variable names. If "Is_same" equals FALSE, some variable names of the data frame do not match the vector of names specified in old_names. "Omitted" is a vector of quoted strings corresponding to the variables from the vector of names specified in old_names that do not appear in the data frame. "Extra" is a vector of quoted strings corresponding to the variables in the data frame not appearing in the vector of names specified in old_names.
#'
#' @export

check_varnames <- function(data,
                           old_names=NULL){

  # Specify if old_names is NULL
  if(is.null(old_names)){

    old_names <- CCMHvars

  } else{

  }

  # Specify column name of data
  new_names <- colnames(data)

  # Check if new and old names are the same
  if(all(old_names %in% new_names) &
     all(new_names %in% old_names)){

    Is_same <- TRUE
    Omitted <- NULL
    Extra <- NULL

  } else{

    Is_same <- FALSE
    Omitted <- old_names[which((old_names %in% colnames(data)) == FALSE)]
    Extra <- new_names[which((new_names %in% old_names) == FALSE)]

  }

  # Create and return output
  out <- list("Is_same"= Is_same,
              "Omitted"= Omitted,
              "Extra"= Extra)

  return(out)

}
