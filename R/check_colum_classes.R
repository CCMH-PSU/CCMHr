#' Check column class equality across two data frames
#'
#' @description A function to check that columns named the same across two data frames have the same classes.
#' To be used before merging two data frames to ensure that the columns with the same names will merge cleanly.
#'
#' @param dataframe1 A data frame.
#' @param dataframe2 Another data frame.
#' @return A data frame with columns for each data frame and rows for each column in the data frames that has unequal column classes.
#' The observations indicate the column classes in each data frame.
#' @export

check_column_classes <- function(dataframe1 = survey, dataframe2 = appts){
  common_vars <- intersect(names(dataframe1), names(dataframe2))
  dataframe2_classes <- data.frame(class_dataframe2 = sapply(dataframe2[,colnames(dataframe2) %in% common_vars], class), Column = common_vars)
  dataframe1_classes <- data.frame(class_dataframe1 = sapply(dataframe1[,colnames(dataframe1) %in% common_vars], class), Column = common_vars)
  dataframe1_classes$class_dataframe1 <- as.character(dataframe1_classes$class_dataframe1)
  dataframe2_classes$class_dataframe2 <- as.character(dataframe2_classes$class_dataframe2)
  df <- dplyr::full_join(dataframe2_classes, dataframe1_classes) %>%
    dplyr::filter(class_dataframe2 != class_dataframe1) %>%
    dplyr::select(Column, class_dataframe1, class_dataframe2)
  names(df) <- c("Column", rlang::expr_text(substitute(dataframe1)), rlang::expr_text(substitute(dataframe2)))
  df
}

