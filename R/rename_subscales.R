#' Rename CCAPS subscales within a wide format.
#'
#' @description The CCAPS subscale variable names are "Depression34", "Anxiety34", "Social_Anxiety34", "Academics34", "Eating34", "Hostility34", "Alcohol34", "DI", "Depression62", "Anxiety62", "Social_Anxiety62", "Academics62", "Eating62", "Hostility62", "Substance62", and "Family62". When creating visualizations, we often rename the subscales to make them more salient (e.g., "DI" corresponds to "Distress Index"). This function renames the subscales when CCAPS subscale variable names are stored in a data frame in a wide format (i.e., the subscale variables are stored across multiple columns).
#'
#' @param data A data frame that contains columns pertaining to CCAPS subscale variables. If CCAPS62 equals `FALSE`, the data frame must have the following variable names: "Depression34", "Anxiety34", "Social_Anxiety34", "Academics34", "Eating34", "Hostility34", "Alcohol34", and "DI". If CCAPS62 equals `TRUE`, the data frame must have the following variable names: "Depression62", "Anxiety62", "Social_Anxiety62", "Academics62", "Eating62", "Hostility62", "Substance62", "Family62", and "DI".
#' @param CCAPS62 A logical argument to indicate if the CCAPS34 or CCAPS62 subscale names should be renamed. If `FALSE`, the CCAPS34 subscale names are renamed. If `TRUE`, the CCAPS62 subscale names are renamed. By default, `FALSE`.
#'
#' @return A data frame with the CCAPS subscale column names recoded to the formal names.
#'
#' @import dplyr
#'
#' @export

rename_subscales_wide <- function(data,
                                  CCAPS62 = FALSE){

  # Check if CCAPS62 is a logical value
  if(CCAPS62 == TRUE){

    # Rename the CCAPS62 subscales
    data <- data |>
      dplyr::rename("Depression" = "Depression62",
                    "Generalized Anxiety" = "Anxiety62",
                    "Social Anxiety" = "Social_Anxiety62",
                    "Academic Distress" = "Academics62",
                    "Eating Concerns" = "Eating62",
                    "Frustration/Anger" = "Hostility62",
                    "Substance Use" = "Substance62",
                    "Family Distress" = "Family62",
                    "Distress Index" = "DI")

  } else{

    # Rename the CCAPS62 subscales
    data <- data |>
      dplyr::rename("Depression" = "Depression34",
                    "Generalized Anxiety" = "Anxiety34",
                    "Social Anxiety" = "Social_Anxiety34",
                    "Academic Distress" = "Academics34",
                    "Eating Concerns" = "Eating34",
                    "Frustration/Anger" = "Hostility34",
                    "Alcohol Use" = "Alcohol34",
                    "Distress Index" = "DI")

  }

  # Return the modified data
  return(data)

}

#' Rename CCAPS subscales within a long format.
#'
#' @description The CCAPS subscale variable names are "Depression34", "Anxiety34", "Social_Anxiety34", "Academics34", "Eating34", "Hostility34", "Alcohol34", "DI", "Depression62", "Eating62", "Substance62", "Anxiety62", "Hostility62", "Social_Anxiety62", "Family62", and "Academics62". When creating visualizations, we often rename the subscales to make them more salient (e.g., "DI" corresponds to "Distress Index"). This function renames the subscales when CCAPS subscale variable names are stored in a data frame in a long format (i.e., the subscale variables are stored in a single column, and multiple rows correspond to a single client, date, and subscale). CCAPS34 and CCAPS62 subscales variable names can be in the same column.
#'
#' @param data A data frame that contains a column with CCAPS subscale variable names. The column must have valid CCAPS names such as "Depression34", "Anxiety34", "Social_Anxiety34", "Academics34", "Eating34", "Hostility34", "Alcohol34", "DI", "Depression62", "Eating62", "Substance62", "Anxiety62", "Hostility62", "Social_Anxiety62", "Family62", and "Academics62".
#' @param column An unquoted string to indicate the name of the column that contains the CCAPS subscale variable names.
#' @param formal A logical argument to determine whether CCAPS subscale names should be in a formal or informal format. If `FALSE`, the CCAPS subscale names are renamed in an informal format (i.e., "Depression", "Anxiety", "Social Anxiety", "Academics", "Eating", "Frustration/Anger", "Alcohol Use", "Substance Use", "Family Distress", "Distress Index"). If `TRUE`, the CCAPS subscale names are renamed in an informal format (i.e., "Depression", "Generalized Anxiety", "Social Anxiety", "Academic Distress", "Eating Concerns", "Frustration/Anger", "Alcohol Use", "Substance Use", "Family Distress", "Distress Index"). By default, `FALSE`.
#'
#' @return A data frame with the column containing CCAPS subscale names is recoded to the formal or informal names.
#'
#' @import glue
#' @import dplyr
#' @import stringr
#' @import forcats
#'
#' @export

rename_subscales_long <- function(data,
                                  column,
                                  formal = FALSE){

  # Specify all possible items within the column
  col_names <- c("Depression34", "Anxiety34",
                 "Social_Anxiety34", "Academics34",
                 "Eating34", "Hostility34",
                 "Alcohol34", "DI",
                 "Depression62", "Eating62",
                 "Substance62", "Anxiety62",
                 "Hostility62", "Social_Anxiety62",
                 "Family62", "Academics62")

  # Ensure data is ungrouped
  data <- dplyr::ungroup(data)

  # Check if column exists as a variable
  if(!deparse(substitute(column)) %in% names(data)){

    stop(glue::glue("Unquoted column name was not found as a variable in data."))

  } else{

  }

  # Check if at least one of the items exist in the column
  if(!any(col_names %in% unique(dplyr::pull(data, {{column}})))){

    # Stop message
    stop(glue::glue("
                    One of the following should be present in the column:
                    Depression34
                    Anxiety34
                    Social_Anxiety34
                    Academics34
                    Eating34
                    Hostility34
                    Alcohol34
                    DI
                    Depression62
                    Eating62
                    Substance62
                    Anxiety62
                    Hostility62
                    Social_Anxiety62
                    Family62
                    Academics62"))

  } else{

  }

  # Rename the subscales
  column_str <- deparse(substitute(column))

  # Rename the subscales
  if(formal == FALSE){

    # Remove specific numbers/symbols and rename Hostility
    data <- data |>
      dplyr::mutate(dplyr::across(c({{column}}), stringr::str_remove, "34"),
                    dplyr::across(c({{column}}), stringr::str_remove, "62"),
                    dplyr::across(c({{column}}), stringr::str_replace, "_", " "),
                    dplyr::across(c({{column}}), ~ifelse(.x == "Hostility", "Frustration/Anger", .x)))

    # Define the order of the subscales
    order <- intersect(c("Depression", "Anxiety",
                         "Social Anxiety", "Academics",
                         "Eating", "Frustration/Anger",
                         "Alcohol Use", "Substance Use",
                         "Family Distress", "Distress Index"),
                       unique(dplyr::pull(data, {{column}})))

    # Rename the subscales and arrange the data
    data <- data |>
      dplyr::mutate(dplyr::across(c({{column}}), forcats::fct_relevel, order)) |>
      dplyr::arrange({{column}})

  } else{

    # Remove numbers and recode subscales
    data <- data |>
      dplyr::mutate(dplyr::across(c({{column}}), stringr::str_remove, "34"),
                    dplyr::across(c({{column}}), stringr::str_remove, "62"),
                    dplyr::across(c({{column}}), ~dplyr::recode(.x,
                                                                "Academics"= "Academic Distress",
                                                                "Alcohol" = "Alcohol Use",
                                                                "Substance" = "Substance Use",
                                                                "Hostility" = "Frustration/Anger",
                                                                "Family" = "Family Distress",
                                                                "Eating" = "Eating Concerns",
                                                                "Anxiety" = "Generalized Anxiety",
                                                                "DI" = "Distress Index",
                                                                "Social_Anxiety" = "Social Anxiety")))

    # Define the order of the subscales
    order <- intersect(c("Depression", "Generalized Anxiety",
                         "Social Anxiety", "Academic Distress",
                         "Eating Concerns", "Frustration/Anger",
                         "Alcohol Use", "Substance Use",
                         "Family Distress", "Distress Index"),
                       unique(dplyr::pull(data, {{column}})))

    # Rename the subscales and arrange the data
    data <- data |>
      dplyr::mutate(dplyr::across(c({{column}}), forcats::fct_relevel, order)) |>
      dplyr::arrange({{column}})

  }

  # Return the modified data
  return(data)

}
