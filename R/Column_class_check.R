require(tidyverse)
require(rlang)

#' Check for Column Class Equality Across Two Data Frames
#'
#' @description A function to check that columns named the same across two data frames have the same classes.
#' To be used before merging two data frames to ensure that the columns with the same names will merge cleanly.
#'
#' @param dataframe1 A data frame.
#' @param dataframe2 Another data frame.
#' @note
#' @return A data frame with columns for each data frame and rows for each column in the data frames that has unequal column classes.
#' The observations indicate the column classes in each data frame.
#' @example
#' @export

class_check <- function(dataframe1 = survey, dataframe2 = appts){
  common_vars <- intersect(names(dataframe1), names(dataframe2))
  dataframe2_classes <- data.frame(class_dataframe2 = sapply(dataframe2[,colnames(dataframe2) %in% common_vars], class), Column = common_vars)
  dataframe1_classes <- data.frame(class_dataframe1 = sapply(dataframe1[,colnames(dataframe1) %in% common_vars], class), Column = common_vars)
  dataframe1_classes$class_dataframe1 <- as.character(dataframe1_classes$class_dataframe1)
  dataframe2_classes$class_dataframe2 <- as.character(dataframe2_classes$class_dataframe2)
  df <- full_join(dataframe2_classes, dataframe1_classes) %>%
    filter(class_dataframe2 != class_dataframe1) %>%
    select(Column, class_dataframe1, class_dataframe2)
  names(df) <- c("Column", expr_text(substitute(dataframe1)), expr_text(substitute(dataframe2)))
  df
}

