#' Find CCAPS scores on the boundaries of the cut points
#'
#' @param items The number of items in the scale
#' @param cutpoint The scale's cut point
#'
#' @return CCAPS scores just above and below the cut point, as well as the item responses necessary to produce that score
#' @export
#'
find_cutpoint_boundaries <- function(items, cutpoint) {
# For all administrations missing < 33% of items, calculate all possible scores
# Find the possible scores closest to the cut point, above and below it
# If there's a score == cut point, return that, as well as the ones above and below
  unique_scores <- function(items) {
    subscale <- as.data.frame(arrangements::combinations(x = c(0,1,2,3,4), k = items, replace=TRUE))
    subscale$Score <- round(apply(subscale, 1, mean), 4)
    subscale <- dplyr::arrange(subscale, Score) %>%
      dplyr::group_by(Score) %>%
      dplyr::slice(1) %>%
      dplyr::mutate(Items = items)
    return(subscale)
  }

  admins <- Filter(function(x) round(1-(x/items),2) < .33, seq(1, items, by = 1))

  purrr::map_df(admins, unique_scores) %>%
      dplyr::arrange(desc(Items)) %>%
      dplyr::group_by(Score) %>%
      dplyr::summarize_all(dplyr::first) %>%
      # dplyr::filter(Score == max(Score < cutpoint)|Score == max(Score > cutpoint))
      dplyr::filter(Score == max(Score[Score < cutpoint])|
                      Score == min(Score[Score >= cutpoint])|
                      Score == cutpoint) %>%
    dplyr::mutate(result = case_when(Score < cutpoint ~ "Below",
                                       Score > cutpoint ~ "Above",
                                       Score == cutpoint ~ "Equal (above)")) %>%
    dplyr::select(-Items) %>%
    dplyr::mutate(cutpoint = cutpoint) %>%
    dplyr::select(Score, cutpoint, result, dplyr::everything())
}
