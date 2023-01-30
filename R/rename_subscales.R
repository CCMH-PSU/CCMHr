#' Rename CCAPS subscales
#'
#' The `rename_subscales` family of functions renames the CCAPS subscales for better presentation (e.g. removing underscores, removing 34 or 62 from subscale names)
#' `rename_subscales_wide` takes a wide data set where each subscale is its own column.
#' `rename_subscales_long` takes a long data set where with a single column of CCAPS subscale names
#'
#' @param data Data frame
#' @param column Column with subscale names
#' @param formal If FALSE (default), returns informal versions of names (e.g. Anxiety, Eating). If TRUE, returns formal versions of names (e.g. Generalized Anxiety, Eating Concerns).
#' @param CCAPS62 If FALSE (default), CCAPS-34 subscale columns are renamed.
#'
#' @return A data frame with renamed subscale column names or single column of subscale names.
#' @export
#'
#' @examples \dontrun{
#' format_subscales(data, subscale, formal = F)
#' }

rename_subscales_wide <- function(data, CCAPS62 = F) {
  if (CCAPS62 == T) {
    dplyr::rename(data, "Depression" = "Depression62",
                  "Generalized Anxiety" = "Anxiety62",
                  "Social Anxiety" = "Social_Anxiety62",
                  "Academic Distress" = "Academics62",
                  "Eating Concerns" = "Eating62",
                  "Frustration/Anger" = "Hostility62",
                  "Substance Use" = "Substance62",
                  "Family Distress" = "Family62",
                  "Distress Index" = "DI")
  } else {
    dplyr::rename(data, "Depression" = "Depression34",
                  "Generalized Anxiety" = "Anxiety34",
                  "Social Anxiety" = "Social_Anxiety34",
                  "Academic Distress" = "Academics34",
                  "Eating Concerns" = "Eating34",
                  "Frustration/Anger" = "Hostility34",
                  "Alcohol Use" = "Alcohol34",
                  "Distress Index" = "DI")
  }

}

#' @export
#' @rdname rename_subscales_wide

rename_subscales_long <- function(data, column, formal = F) {
  col_names <- c("Depression34", "Anxiety34", "Social_Anxiety34", "Academics34", "Eating34", "Hostility34", "Alcohol34", "DI")

  data <- dplyr::ungroup(data)


  if(!deparse(substitute(column)) %in% names(data)) {
    stop(glue::glue("Column: {deparse(substitute(column))} not found in data: {deparse(substitute(data))}."))
  }
  if (!all(unique(dplyr::pull(data, {{column}})) %in% col_names)) {
    stop(glue::glue('CCAPS cols not named correctly.

    Expected {paste(col_names, collapse=", ")}.

    Found {paste(unique(data[,deparse(substitute(column))]), collapse=", ")}.'))
  }

  column_str <- deparse(substitute(column))

  if (formal == F) {
    data <- dplyr::mutate(data,
                          dplyr::across(c({{column}}), stringr::str_remove, "34"),
                          dplyr::across(c({{column}}), stringr::str_remove, "62"),
                          dplyr::across(c({{column}}), stringr::str_replace, "_", " "),
                          dplyr::across(c({{column}}), ~ifelse(.x == "Hostility", "Frustration/Anger", .x)))

    order <- intersect(c("Depression", "Anxiety", "Social Anxiety", "Academics",
                         "Eating", "Frustration/Anger", "Alcohol Use",
                         "Substance Use", "Family Distress", "Distress Index"),
                       unique(dplyr::pull(data, {{column}})))

    dplyr::mutate(data, dplyr::across(c({{column}}), forcats::fct_relevel, order)) %>%
      dplyr::arrange({{column}})

  } else {
    data <- dplyr::mutate(data, dplyr::across(c({{column}}), stringr::str_remove, "34"),
                          dplyr::across(c({{column}}), stringr::str_remove, "62"),
                          dplyr::across(c({{column}}), ~dplyr::recode(.x, "Academics"= "Academic Distress",
                                                               "Alcohol" = "Alcohol Use",
                                                               "Substance" = "Substance Use",
                                                               "Hostility" = "Frustration/Anger",
                                                               "Family" = "Family Distress",
                                                               "Eating" = "Eating Concerns",
                                                               "Anxiety" = "Generalized Anxiety",
                                                               "DI" = "Distress Index",
                                                               "Social_Anxiety" = "Social Anxiety")))

    order <- intersect(c("Depression", "Generalized Anxiety", "Social Anxiety", "Academic Distress", "Eating Concerns", "Frustration/Anger", "Alcohol Use", "Substance Use", "Family Distress", "Distress Index"),
                       dplyr::pull(data[, column_str]))

    dplyr::mutate(data, dplyr::across(c({{column}}), forcats::fct_relevel, order)) %>%
      dplyr::arrange({{column}})
  }
}
