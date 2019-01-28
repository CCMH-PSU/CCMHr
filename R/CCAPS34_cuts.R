#' Create CCAPS-34 cut points
#'
#' @description Creates binary variables for CCAPS-34 subscales indicating whether the score is above or below the low and high cut respectively, using the 2018 cut points.
#' @param data A data file containing CCAPS-34 subscales
#' @note If `data` does not contain the proper CCAPS subscale names, it will return an error.
#' @return A data frame with all the original data in `data`, and several additional columns of cut scores coded 1 for above cut score and 0 for below cut score.
#' @examples \dontrun{
#' TI1718 <- ccaps34_cuts(TI1718)
#' }
#' @export

ccaps34_cuts <- function(data){
  if(!all(c("Depression34", "Anxiety34", "Social_Anxiety34", "Academics34", "Eating34", "Hostility34", "Alcohol34", "DI") %in% colnames(data))) stop('CCAPS columns not named correctly.')
    data %>%
      dplyr::mutate(depression_low_cut34 = ifelse(round(Depression34, 2) >= 1, 1, 0),
             depression_hi_cut34 = ifelse(round(Depression34, 2) >= 1.83, 1, 0),
             anxiety_low_cut34 = ifelse(round(Anxiety34, 2) >= 1.3, 1, 0),
             anxiety_hi_cut34 = ifelse(round(Anxiety34,2) >= 2.1, 1, 0),
             social_anxiety_low_cut34 = ifelse(round(Social_Anxiety34, 2) >= 1.65, 1, 0),
             social_anxiety_hi_cut34 = ifelse(round(Social_Anxiety34,2) >= 2.5, 1, 0),
             academics_low_cut34 = ifelse(round(Academics34,2) >= 1.45, 1, 0),
             academics_hi_cut34 = ifelse(round(Academics34,2) >= 2.50, 1, 0),
             eating_low_cut34 = ifelse(round(Eating34,2) >= 1.07, 1, 0),
             eating_hi_cut34 = ifelse(round(Eating34,2) >= 1.50, 1, 0),
             hostility_low_cut34 = ifelse(round(Hostility34,2) >= .74, 1, 0),
             hostility_hi_cut34 = ifelse(round(Hostility34,2) >= 1.17, 1, 0),
             alcohol_low_cut34 = ifelse(round(Alcohol34,2) >= .64, 1, 0),
             alcohol_hi_cut34 = ifelse(round(Alcohol34,2) >= 1.10, 1, 0),
             DI_low_cut = ifelse(round(DI,2) >= 1.21, 1, 0),
             DI_hi_cut = ifelse(round(DI,2) >= 2.25, 1, 0)
      )
}
