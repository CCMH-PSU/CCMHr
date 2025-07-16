#' Create CCAPS bins based on the first CCAPS scores.
#'
#' @description The function is used for profile curves. To run this function, the data frame must contain details regarding the client's first CCAPS administration (e.g., use the CCAPS_change function and set the include_first argument to `TRUE`).
#'
#' @param data A data file with CCAPS subscale variables for the first CCAPS administration and named "subscale_first".
#'
#' @return A data frame with variables for each CCAPS subscale, categorizing the client into the corresponding bins.
#'
#' @export

create_CCAPS_bins <- function(data){

  # Create cut function
  cut_mod <- function(x, breaks){

    cut(x,
        breaks,
        include.lowest = TRUE,
        labels = FALSE,
        right = FALSE)

  }

  # Check CCAPS columns are named correctly
  if (!all(c("Depression34_first", "Anxiety34_first",
             "Social_Anxiety34_first", "Academics34_first",
             "Eating34_first", "Hostility34_first",
             "Alcohol34_first", "DI_first") %in% colnames(data))) {

    # Message if columns not named correctly
    stop('CCAPS columns not named correctly.')

  } else {

  }

  # Conduct cuts
  data <- dplyr::mutate(data,
                        depression_bin = cut_mod(.data$Depression34_first,
                                                 breaks = c(0.00, 0.16,
                                                            0.33, 0.40,
                                                            0.60, 0.80,
                                                            1.00, 1.16,
                                                            1.25, 1.50,
                                                            1.60, 1.75,
                                                            2.00, 2.16,
                                                            2.33, 2.50,
                                                            2.60, 2.80,
                                                            3.00, 3.16,
                                                            3.33, 3.50,
                                                            3.60, 4.00)),
                        anxiety_bin = cut_mod(.data$Anxiety34_first,
                                              breaks = c(0.00, 0.16,
                                                         0.33, 0.50,
                                                         0.60, 0.80,
                                                         1.00, 1.16,
                                                         1.33, 1.50,
                                                         1.60, 1.80,
                                                         2.00, 2.16,
                                                         2.33, 2.50,
                                                         2.60, 2.80,
                                                         3.00, 3.16,
                                                         3.33, 3.50,
                                                         3.60, 3.80,
                                                         4.00, 4.10)),
                        social_anxiety_bin = cut_mod(.data$Social_Anxiety34_first,
                                                     breaks = c(0.00, 0.20,
                                                                0.33, 0.60,
                                                                0.75, 1.00,
                                                                1.20, 1.33,
                                                                1.60, 1.75,
                                                                2.00, 2.20,
                                                                2.33, 2.60,
                                                                2.75, 3.00,
                                                                3.20, 3.33,
                                                                3.60, 3.75,
                                                                4.00, 4.10)),
                        academics_bin = cut_mod(.data$Academics34_first,
                                                breaks = c(0.00, 0.25,
                                                           0.50, 0.66,
                                                           1.00, 1.25,
                                                           1.50, 1.66,
                                                           2.00, 2.25,
                                                           2.50, 2.66,
                                                           3.00, 3.25,
                                                           3.50, 3.66,
                                                           4.00, 4.1)),
                        eating_bin = cut_mod(.data$Eating34_first,
                                             breaks = c(0.00, 0.33,
                                                        0.66, 1.00,
                                                        1.33, 1.66,
                                                        2.00, 2.33,
                                                        2.66, 3.00,
                                                        3.33, 3.66,
                                                        4.00, 4.1)),
                        hostility_bin = cut_mod(.data$Hostility34_first,
                                                breaks = c(0.00, 0.16,
                                                           0.33, 0.50,
                                                           0.60, 0.75,
                                                           1.00, 1.16,
                                                           1.33, 1.50,
                                                           1.60, 1.75,
                                                           2.00, 2.16,
                                                           2.33, 2.50,
                                                           2.60, 2.75,
                                                           4.00)),
                        alcohol_bin = cut_mod(.data$Alcohol34_first,
                                              breaks = c(0.00, 0.25,
                                                         0.50, 0.66,
                                                         1.00, 1.25,
                                                         1.50, 1.66,
                                                         2.00, 2.25,
                                                         2.50, 2.66,
                                                         3.00, 4.00)),
                        DI_bin = cut_mod(.data$DI_first,
                                         breaks = c(0.00, 0.60,
                                                    0.63, 0.68,
                                                    0.73, 0.78,
                                                    0.83, 0.88,
                                                    0.94, 1.00,
                                                    1.05, 1.10,
                                                    1.15, 1.20,
                                                    1.25, 1.30,
                                                    1.35, 1.38,
                                                    1.44, 1.50,
                                                    1.55, 1.60,
                                                    1.63, 1.68,
                                                    1.73, 1.78,
                                                    1.83, 1.87,
                                                    1.94, 2.00,
                                                    2.05, 2.10,
                                                    2.15, 2.20,
                                                    2.25, 2.29,
                                                    2.33, 2.38,
                                                    2.44, 2.50,
                                                    2.55, 2.60,
                                                    2.63, 2.68,
                                                    2.73, 2.78,
                                                    2.84, 2.89,
                                                    2.90, 4.00)))

  # Return data frame
  return(data)

}
