#' Create CCAPS bins for profile curves based on starting CCAPS scores
#'
#' @param data A data file with variables for first CCAPS administration named subscale_first
#'
#' @return A data frame with variables for each subscale indicating which bin the client falls in.
#' @export

create_CCAPS_bins <- function(data) {
  cut_mod <- function(x, breaks) {cut(x, breaks, include.lowest = T, labels = F, right = F)}

  if (!all(c("Depression34_first", "Anxiety34_first", "Social_Anxiety34_first", "Academics34_first", "Eating34_first", "Hostility34_first", "Alcohol34_first", "DI_first") %in% colnames(data))) {
    stop('CCAPS columns not named correctly.')
  }
  data <- dplyr::mutate(data,
                        depression_bin = cut_mod(.data$Depression34_first, breaks = c(0, .16, .33, .4, .6, .8, 1, 1.16, 1.25, 1.50, 1.60, 1.75, 2.00, 2.16, 2.33, 2.50, 2.60, 2.80, 3.00, 3.16, 3.33, 3.50, 3.60, 4.00)),
                        anxiety_bin = cut_mod(.data$Anxiety34_first, breaks = c(0, 0.16, 0.33, 0.5, 0.6, 0.8, 1, 1.16, 1.33, 1.5, 1.6, 1.8, 2, 2.16, 2.33, 2.5, 2.6, 2.8, 3, 3.16, 3.33, 3.5, 3.6, 3.8, 4, 4.1)),
                        social_anxiety_bin = cut_mod(.data$Social_Anxiety34_first, breaks = c(0, 0.2, 0.33, 0.6, 0.75, 1, 1.2, 1.33, 1.6, 1.75, 2, 2.2, 2.33, 2.6, 2.75, 3, 3.2, 3.33, 3.6, 3.75, 4, 4.1)),
                        academics_bin = cut_mod(.data$Academics34_first, breaks = c(0, 0.25, 0.5, 0.66, 1, 1.25, 1.5, 1.66, 2, 2.25, 2.5, 2.66, 3, 3.25, 3.5, 3.66, 4, 4.1)),
                        eating_bin = cut_mod(.data$Eating34_first, breaks = c(0, 0.33, 0.66, 1, 1.33, 1.66, 2, 2.33, 2.66, 3, 3.33, 3.66, 4, 4.1)),
                        hostility_bin = cut_mod(.data$Hostility34_first, breaks = c(0, 0.16, 0.33, 0.5, 0.6, 0.75, 1, 1.16, 1.33, 1.5, 1.6, 1.75, 2, 2.16, 2.33, 2.5, 2.6, 2.75, 4)),
                        alcohol_bin = cut_mod(.data$Alcohol34_first, breaks = c(0, 0.25, 0.5, 0.66, 1, 1.25, 1.5, 1.66, 2, 2.25, 2.5, 2.66, 3, 4)),
                        DI_bin = cut_mod(.data$DI_first, breaks = c(0, 0.6, 0.63, 0.68, 0.73, 0.78, 0.83, 0.88, 0.94, 1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.38, 1.44, 1.5, 1.55, 1.6, 1.63, 1.68, 1.73, 1.78, 1.83, 1.87, 1.94, 2, 2.05, 2.1, 2.15, 2.2, 2.25, 2.29, 2.33, 2.38, 2.44, 2.5, 2.55, 2.6, 2.63, 2.68, 2.73, 2.78, 2.84, 2.89, 2.9, 4))
)

}






