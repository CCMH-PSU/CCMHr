#' Create CCAPS-62 cut points
#'
#' @description Creates binary variables for CCAPS-62 subscales indicating whether the score is above or below the low and high cut respectively, using the 2018 cut points.
#' @param data A data file containing CCAPS-62 subscales
#' @note If `data` does not contain the proper CCAPS subscale names, it will return an error.
#' @return A data frame with all the original data in `data`, and several additional columns of cut scores coded 1 for above cut score and 0 for below cut score.
#' @examples \dontrun{
#' TI1718 <- ccaps62_lowcut(TI1718)
#' }
#' @export

ccaps62_cuts <- function(data){
  if(!all(c("Depression62", "Anxiety62", "Social_Anxiety62", "Academics62", "Eating62", "Hostility62", "Substance62", "Family62", "DI") %in% colnames(data))) stop('CCAPS columns not named correctly.')
    data %>%
      dplyr::mutate(depression_low_cut62 = ifelse(round(.data$Depression62, 2) >= 1.09, 1, 0),
                    depression_hi_cut62 = ifelse(round(.data$Depression62, 2) >= 1.92, 1, 0),
                    anxiety_low_cut62 = ifelse(round(.data$Anxiety62, 2) >= 1.25, 1, 0),
                    anxiety_hi_cut62 = ifelse(round(.data$Anxiety62,2) >= 1.89, 1, 0),
                    social_anxiety_low_cut62 = ifelse(round(.data$Social_Anxiety62, 2) >= 1.72, 1, 0),
                    social_anxiety_hi_cut62 = ifelse(round(.data$Social_Anxiety62,2) >= 2.57, 1, 0),
                    academics_low_cut62 = ifelse(round(.data$Academics62,2) >= 1.42, 1, 0),
                    academics_hi_cut62 = ifelse(round(.data$Academics62,2) >= 2.40, 1, 0),
                    eating_low_cut62 = ifelse(round(.data$Eating62,2) >= 1.09, 1, 0),
                    eating_hi_cut62 = ifelse(round(.data$Eating62,2) >= 1.80, 1, 0),
                    hostility_low_cut62 = ifelse(round(.data$Hostility62,2) >= .82, 1, 0),
                    hostility_hi_cut62 = ifelse(round(.data$Hostility62,2) >= 1.43, 1, 0),
                    substance_low_cut62 = ifelse(round(.data$Substance62,2) >= .7, 1, 0),
                    substance_hi_cut62 = ifelse(round(.data$Substance62,2) >= 1, 1, 0),
                    family_low_cut62 = ifelse(round(.data$Family62,2) >= .98, 1, 0),
                    family_hi_cut62 = ifelse(round(.data$Family62,2) >= 1.83, 1, 0)
      )
  message("CCAPS-62 low cut scores based on original 2012 norms and high cut scores based on 2018 norms.")
}