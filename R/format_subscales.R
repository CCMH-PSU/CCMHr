#' Format subscale names
#'
#' @description Renames a column of CCAPS subscales to remove 34 or 62 and underscores in subscale name for use in making plots, tables, etc.
#'
#' @param data Data frame
#' @param col Column with subscale names
#' @param formal If FALSE (default), returns informal versions of names (e.g. Anxiety, Eating). If TRUE, returns formal versions of names (e.g. Generalized Anxiety, Eating Concerns).
#'
#' @return A data frame with renamed subscales.
#' @export
#' @importFrom rlang :=
#'
#' @examples \dontrun{
#' format_subscales(data, subscale, formal = F)
#' }
#'
format_subscales <- function(data, col, formal = F) {
  col_names <- c("Depression34", "Anxiety34", "Social_Anxiety34", "Academics34", "Eating34", "Hostility34", "Alcohol34", "DI")


  if(!deparse(substitute(col)) %in% names(data)) {
    stop(glue::glue("Column: {deparse(substitute(col))} not found in data: {deparse(substitute(data))}."))
  }
  if (!all(unique(data[,deparse(substitute(col))]) %in% col_names)) {
    stop(glue::glue('CCAPS cols not named correctly.

    Expected {paste(col_names, collapse=", ")}.

    Found {paste(unique(data[,deparse(substitute(col))]), collapse=", ")}.'))
  }

  if (formal == F) {
    suppressWarnings(data %>% dplyr::mutate({{col}} := stringr::str_remove({{col}}, "34"),
                           {{col}} := stringr::str_replace({{col}}, "_", " "),
                           {{col}} := forcats::fct_relevel({{col}}, "Depression",
                                                      "Anxiety",
                                                      "Social Anxiety",
                                                      "Academics",
                                                      "Eating",
                                                      "Hostility",
                                                      "Alcohol",
                                                      "Substance",
                                                      "Family",
                                                      "DI")))
  } else {
    suppressWarnings(data %>% dplyr::mutate({{col}} := stringr::str_remove({{col}}, "34"),
                           {{col}} := stringr::str_replace({{col}}, "_", " "),
                           {{col}} := dplyr::case_when({{col}} == "DI" ~"Distress Index",
                                                      {{col}} == "Anxiety" ~ "Generalized Anxiety",
                                                      {{col}} == "Social_Anxiety" ~ "Social Anxiety",
                                                      {{col}} == "Academics" ~ "Academic Distress",
                                                      {{col}} == "Eating" ~ "Eating Concerns",
                                                      {{col}} == "Alcohol" ~ "Alcohol Use",
                                                      {{col}} == "Family" ~ "Family Distress",
                                                      {{col}} == "Depression" ~ "Depression",
                                                      {{col}} == "Hostility" ~ "Hostility",
                                                      {{col}} == "Substance" ~ "Substance Use",
                                                  TRUE ~ {{col}}),
                           {{col}} := forcats::fct_relevel({{col}}, "Depression",
                                                      "Generalized Anxiety",
                                                      "Social Anxiety",
                                                      "Academic Distress",
                                                      "Eating Concerns",
                                                      "Hostility",
                                                      "Alcohol Use",
                                                      "Substance Use",
                                                      "Family Distress",
                                                      "Distress Index")))
  }
}
