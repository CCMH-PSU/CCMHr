#' Find CCAPS scores on the boundaries of the cut points.
#'
#' @name find_cutpoint_boundaries
#'
#' @description This function finds and returns CCAPS scores just above and below the cut point, as well as the item responses required to produce those scores.
#'
#' @param items The number of items in the scale.
#' @param cutpoint The scale's cut point.
#'
#' @return CCAPS scores just above and below the cut point, as well as the item responses necessary to produce that score.
#'
#' @importFrom gtools combinations
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr slice
#' @importFrom dplyr mutate
#' @importFrom dplyr summarize_all
#' @importFrom dplyr filter
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr first
#' @importFrom dplyr desc
#' @importFrom purrr map_df
#'
#' @export

find_cutpoint_boundaries <- function(items,
                                     cutpoint) {

  # For all administrations missing < 33% of items, calculate all possible scores
  # Find the possible scores closest to the cut point, above and below it
  # If there's a score == cut point, return that, as well as the ones above and below

  unique_scores <- function(items) {

    subscale <- as.data.frame(gtools::combinations(5, items, v = c(0,1,2,3,4), set = FALSE, repeats.allowed = TRUE))

    subscale$Score <- round(apply(subscale, 1, mean), 2)

    subscale <- dplyr::arrange(subscale, .data$Score) |>
      dplyr::group_by(.data$Score) |>
      dplyr::slice(1) |>
      dplyr::mutate(Items = items)

    return(subscale)

  }

  admins <- Filter(function(x) round(1-(x/items),2) < .33, seq(1, items, by = 1))

  purrr::map_df(admins, unique_scores) |>
    dplyr::arrange(dplyr::desc(.data$Items)) |>
    dplyr::group_by(.data$Score) |>
    dplyr::summarize_all(dplyr::first) |>
    # dplyr::filter(Score == max(Score < cutpoint)|Score == max(Score > cutpoint))
    dplyr::filter(.data$Score == max(.data$Score[.data$Score < cutpoint])|
                  .data$Score == min(.data$Score[.data$Score >= cutpoint])|
                  .data$Score == cutpoint) |>
    dplyr::mutate(result = dplyr::case_when(.data$Score < cutpoint ~ "Below",
                                            .data$Score > cutpoint ~ "Above",
                                            .data$Score == cutpoint ~ "Equal (above)")) |>
    dplyr::select(-.data$Items) |>
    dplyr::mutate(cutpoint = cutpoint) |>
    dplyr::select(.data$Score, .data$cutpoint, .data$result, dplyr::everything())

}
