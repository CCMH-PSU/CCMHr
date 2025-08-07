#' Setup data cleaning file for data request
#'
#' @description The function creates folders and R files associated with a data request. Primarily, this function is used to create a data request folder, its corresponding subfolders, and an R file. To run the function and create a new data request folder, the programmer must first set their working directory to a specified folder where the data request folder will be saved. Typically, the working directory is located under the following path: "CCMH - Documents/Data Requests/####/Author Last Name-Author First Name", where #### represents the data year and Author Last Name-Author First Name is formatted similarly to "Janis-Rebecca". The R file ("Author Last Name-data-cleaning.R") is also added to standardize some of the cleaning processes.
#'
#' @param requester_last_name A quoted string to indicate the last name of the data requester.
#'
#' @return The function will create a new data request folder that contains subfolders "LAST NAME data cleaning" and "Data for LAST NAME". The R file is also added to standardize some of the cleaning processes.
#'
#' @importFrom stringr str_extract
#' @importFrom glue glue
#' @importFrom usethis use_template
#'
#' @export

setup_data_request <- function(requester_last_name = NULL){

  # Specify the working directory
  if(is.null(requester_last_name)){

    requester_last_name <- stringr::str_extract(basename(getwd()), "[^-]+")

  } else {

    requester_last_name

  }

  # Specify sub folder names
  cleaning_folder <- glue::glue("{requester_last_name} data cleaning")
  final_folder <- glue::glue("Data for {requester_last_name}")

  # Create the sub folders
  if (!cleaning_folder %in% list.files()){

    dir.create(cleaning_folder)

  } else{

  }

  if (!final_folder %in% list.files()) {

    dir.create(final_folder)

  } else{

  }

  # Create the R file
  usethis::use_template(template = "data_request_cleaning.R",
                        save_as = paste0(cleaning_folder, "/", requester_last_name, "-data-cleaning.R"),
                        package = "CCMHr",
                        data = list(requester_last_name = requester_last_name,
                                    cleaning_folder = cleaning_folder,
                                    final_folder = final_folder),
                        open = TRUE)

}
