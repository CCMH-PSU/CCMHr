require(tidyverse)
require(tidy)
require(rlang)

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

