#' Setup data cleaning file for data request
#'
#' @param requester_last_name The last name of the data requester.
#'
#' @return An R syntax file with the skeleton sections needed to clean data for a data request
#' @export
#'
#' @examples
#' \dontrun{
#' setup_data_request(requester_last_name = "Janis")
#' }

setup_data_request <- function(requester_last_name = NULL) {

  if (is.null(requester_last_name)) {
    requester_last_name <- stringr::str_extract(basename(getwd()), "[^-]+")
  } else {
    requester_last_name
  }

  cleaning_folder <- glue::glue("{requester_last_name} data cleaning")
  final_folder <- glue::glue("Data for {requester_last_name}")

  if (!cleaning_folder %in% list.files()) {
    dir.create(cleaning_folder)
  }

  if (!final_folder %in% list.files()) {
    dir.create(final_folder)
  }

  usethis::use_template(template = "data_request_cleaning.R",
                        save_as = paste0(cleaning_folder, "/", requester_last_name, "-data-cleaning.R"),
                        package = "CCMHr",
                        data = list(requester_last_name = requester_last_name,
                                    cleaning_folder = cleaning_folder,
                                    final_folder = final_folder),
                        open = TRUE)

}
