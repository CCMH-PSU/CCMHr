#' Check column class equality across two data frames.
#'
#' @description A function to check that columns named the same across two data frames have the same classes. To be used before merging two data frames to ensure that the columns with the same names will merge cleanly.
#'
#' @param dataframe1 A data frame.
#' @param dataframe2 Another data frame.
#'
#' @importFrom tibble tibble
#' @importFrom purrr map_chr
#' @importFrom dplyr full_join
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @return If column classes are equal across two data frames, this message is returned: "No class mismatches detected." If column classes are not equal across two data frames, then a message is returned (i.e., "Class mismatches detected.") along with a printed data frame that details the column, column class for the first data frame, and column class for the second data frame.
#'
#' @export

check_column_classes <- function(dataframe1 = survey,
                                 dataframe2 = appts){

  # Detecting common variables
  common_vars <- intersect(names(dataframe1), names(dataframe2))

  # Detecting if variables have multiple classes and stripping to primary class
  strip_to_primary_class <- function(x){

    class(x) <- class(x)[1]

    x

  }

  dataframe2[] <- lapply(dataframe2, strip_to_primary_class)
  dataframe1[] <- lapply(dataframe1, strip_to_primary_class)

  # Extracting classes of common variables
  dataframe2_classes <- tibble::tibble(class_dataframe2 = purrr::map_chr(dataframe2[,colnames(dataframe2) %in% common_vars], class), Column = common_vars)
  dataframe1_classes <- tibble::tibble(class_dataframe1 = purrr::map_chr(dataframe1[,colnames(dataframe1) %in% common_vars], class), Column = common_vars)

  # Joining and filtering for mismatches
  mismatches <- dplyr::full_join(dataframe2_classes, dataframe1_classes) |>
    dplyr::filter(class_dataframe2 != class_dataframe1) |>
    dplyr::select(Column, class_dataframe1, class_dataframe2)

  # Output
  if(nrow(mismatches) == 0){

    message("No class mismatches detected.")

  } else {

    message(paste0("Class mismatches detected."))

    return(mismatches)

  }

}
