#' Setup data cleaning file for data request
#'
#' @param cleaning_folder The folder where the data cleaning file should be saved. Should be formatted "{Requester last name} data cleaning".
#' @param requester_name The last name of the data requester.
#'
#' @return An R syntax file with the skeleton sections needed to clean data for a data request
#' @export
#'
#' @examples
#' \dontrun{
#' setup_data_request(cleaning_folder = "Janis data cleaning", requester_name = "Janis")
#' }

setup_data_request <- function(cleaning_folder, requester_name) {
  if (!cleaning_folder %in% list.files()) {
    stop("The cleaning_folder provided does not exist. Make sure you have created a new folder for data cleaning and spelled it correctly when calling the function.")
    }

  usethis::use_template(template = "data_request_cleaning.R",
                        save_as = paste0(cleaning_folder, "/", requester_name, "-data-cleaning.R"),
                        package = "CCMHr",
                        data = list(requester_name = requester_name,
                                    cleaning_folder = cleaning_folder),
                        open = TRUE)

}
