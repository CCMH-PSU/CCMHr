#' Create CCAPS-62 cut points
#'
#' @description Creates binary variables for CCAPS-62 subscales indicating whether the score is equal to or above (1) or below (0) the low and high cut respectively.
#' @param data A data file containing CCAPS-62 subscales
#' @param version The version of CCAPS cut points to use. Currently, 2018 & 2019 are available.
#' @param first True indicates the cut points should be based on variabels with _first.
#' @note If `data` does not contain the proper CCAPS subscale names, it will return an error.
#' @return A data frame with all the original data in `data`, and several additional columns of cut scores coded 1 for above cut score and 0 for below cut score.
#' @examples \dontrun{
#' TI1718 <- ccaps62_lowcut(TI1718)
#' }
#' @export

ccaps62_cuts <- function(data, version = 2019, first = F){
  if(first == F & !all(c("Depression62", "Anxiety62", "Social_Anxiety62", "Academics62", "Eating62", "Hostility62", "Substance62", "DI") %in% colnames(data))) {
    stop('CCAPS columns not named correctly.')
    } else if(first == T & !all(c("Depression62_first", "Anxiety62_first", "Social_Anxiety62_first", "Academics62_first", "Eating62_first", "Hostility62_first", "Substance62_first", "DI_first") %in% colnames(data))) {
    stop('CCAPS columns not named correctly.')
    } else {
      # First CCAPS
      if(first == F) {
        if(version == "2018") {
          data <- data %>%
            dplyr::mutate(Depression62_low_cut = ifelse(round(.data$Depression62, 2) >= 1.09, 1, 0),
                          Depression62_hi_cut = ifelse(round(.data$Depression62, 2) >= 1.92, 1, 0),
                          Anxiety62_low_cut = ifelse(round(.data$Anxiety62, 2) >= 1.25, 1, 0),
                          Anxiety62_hi_cut = ifelse(round(.data$Anxiety62,2) >= 1.89, 1, 0),
                          Social_Anxiety62_low_cut = ifelse(round(.data$Social_Anxiety62, 2) >= 1.72, 1, 0),
                          Social_Anxiety62_hi_cut = ifelse(round(.data$Social_Anxiety62,2) >= 2.57, 1, 0),
                          Academics62_low_cut = ifelse(round(.data$Academics62,2) >= 1.42, 1, 0),
                          Academics62_hi_cut = ifelse(round(.data$Academics62,2) >= 2.40, 1, 0),
                          Eating62_low_cut = ifelse(round(.data$Eating62,2) >= 1.09, 1, 0),
                          Eating62_hi_cut = ifelse(round(.data$Eating62,2) >= 1.80, 1, 0),
                          Hostility62_low_cut = ifelse(round(.data$Hostility62,2) >= .82, 1, 0),
                          Hostility62_hi_cut = ifelse(round(.data$Hostility62,2) >= 1.43, 1, 0),
                          Substance62_low_cut = ifelse(round(.data$Substance62,2) >= .7, 1, 0),
                          Substance62_hi_cut = ifelse(round(.data$Substance62,2) >= 1, 1, 0),
                          Family62_low_cut = ifelse(round(.data$Family62,2) >= .98, 1, 0),
                          Family62_hi_cut = ifelse(round(.data$Family62,2) >= 1.83, 1, 0)
            )
          return(data)
          message("CCAPS-62 low cut scores based on original 2012 norms and high cut scores based on 2018 norms.")
        } else if (version == "2019") {
          data <- data %>%
            dplyr::mutate(Depression62_low_cut = ifelse(round(.data$Depression62, 2) >= 1.23, 1, 0),
                          Depression62_hi_cut = ifelse(round(.data$Depression62, 2) >= 1.92, 1, 0),
                          Anxiety62_low_cut = ifelse(round(.data$Anxiety62, 2) >= 1.22, 1, 0),
                          Anxiety62_hi_cut = ifelse(round(.data$Anxiety62,2) >= 1.89, 1, 0),
                          Social_Anxiety62_low_cut = ifelse(round(.data$Social_Anxiety62, 2) >= 1.43, 1, 0),
                          Social_Anxiety62_hi_cut = ifelse(round(.data$Social_Anxiety62,2) >= 2.57, 1, 0),
                          Academics62_low_cut = ifelse(round(.data$Academics62,2) >= 1.2, 1, 0),
                          Academics62_hi_cut = ifelse(round(.data$Academics62,2) >= 2.40, 1, 0),
                          Eating62_low_cut = ifelse(round(.data$Eating62,2) >= 1.02, 1, 0),
                          Eating62_hi_cut = ifelse(round(.data$Eating62,2) >= 1.80, 1, 0),
                          Hostility62_low_cut = ifelse(round(.data$Hostility62,2) >= 1, 1, 0),
                          Hostility62_hi_cut = ifelse(round(.data$Hostility62,2) >= 1.43, 1, 0),
                          Substance62_low_cut = ifelse(round(.data$Substance62,2) >= .69, 1, 0),
                          Substance62_hi_cut = ifelse(round(.data$Substance62,2) >= 1, 1, 0),
                          Family62_low_cut = ifelse(round(.data$Family62,2) >= 1.31, 1, 0),
                          Family62_hi_cut = ifelse(round(.data$Family62,2) >= 1.83, 1, 0)
              )
            return(data)
            message("CCAPS-62 low cut scores based on 2019 norms and high cut scores based on 2018 norms.")
        }

      } else if (first == T) {
        if(version == "2018") {
          data <- data %>%
            dplyr::mutate(Depression62_low_cut = ifelse(round(.data$Depression62_first, 2) >= 1.09, 1, 0),
                          Depression62_hi_cut = ifelse(round(.data$Depression62_first, 2) >= 1.92, 1, 0),
                          Anxiety62_low_cut = ifelse(round(.data$Anxiety62_first, 2) >= 1.25, 1, 0),
                          Anxiety62_hi_cut = ifelse(round(.data$Anxiety62_first,2) >= 1.89, 1, 0),
                          Social_Anxiety62_low_cut = ifelse(round(.data$Social_Anxiety62_first, 2) >= 1.72, 1, 0),
                          Social_Anxiety62_hi_cut = ifelse(round(.data$Social_Anxiety62_first,2) >= 2.57, 1, 0),
                          Academics62_low_cut = ifelse(round(.data$Academics62_first,2) >= 1.42, 1, 0),
                          Academics62_hi_cut = ifelse(round(.data$Academics62_first,2) >= 2.40, 1, 0),
                          Eating62_low_cut = ifelse(round(.data$Eating62_first,2) >= 1.09, 1, 0),
                          Eating62_hi_cut = ifelse(round(.data$Eating62_first,2) >= 1.80, 1, 0),
                          Hostility62_low_cut = ifelse(round(.data$Hostility62_first,2) >= .82, 1, 0),
                          Hostility62_hi_cut = ifelse(round(.data$Hostility62_first,2) >= 1.43, 1, 0),
                          Substance62_low_cut = ifelse(round(.data$Substance62_first,2) >= .7, 1, 0),
                          Substance62_hi_cut = ifelse(round(.data$Substance62_first,2) >= 1, 1, 0),
                          Family62_low_cut = ifelse(round(.data$Family62_first,2) >= .98, 1, 0),
                          Family62_hi_cut = ifelse(round(.data$Family62_first,2) >= 1.83, 1, 0)
            )
          return(data)
          message("CCAPS-62 low cut scores based on original 2012 norms and high cut scores based on 2018 norms.")
        } else if (version == "2019") {
          data <- data %>%
            dplyr::mutate(Depression62_low_cut = ifelse(round(.data$Depression62_first, 2) >= 1.23, 1, 0),
                          Depression62_hi_cut = ifelse(round(.data$Depression62_first, 2) >= 1.92, 1, 0),
                          Anxiety62_low_cut = ifelse(round(.data$Anxiety62_first, 2) >= 1.22, 1, 0),
                          Anxiety62_hi_cut = ifelse(round(.data$Anxiety62_first,2) >= 1.89, 1, 0),
                          Social_Anxiety62_low_cut = ifelse(round(.data$Social_Anxiety62_first, 2) >= 1.43, 1, 0),
                          Social_Anxiety62_hi_cut = ifelse(round(.data$Social_Anxiety62_first,2) >= 2.57, 1, 0),
                          Academics62_low_cut = ifelse(round(.data$Academics62_first,2) >= 1.2, 1, 0),
                          Academics62_hi_cut = ifelse(round(.data$Academics62_first,2) >= 2.40, 1, 0),
                          Eating62_low_cut = ifelse(round(.data$Eating62_first,2) >= 1.02, 1, 0),
                          Eating62_hi_cut = ifelse(round(.data$Eating62_first,2) >= 1.80, 1, 0),
                          Hostility62_low_cut = ifelse(round(.data$Hostility62_first,2) >= 1, 1, 0),
                          Hostility62_hi_cut = ifelse(round(.data$Hostility62_first,2) >= 1.43, 1, 0),
                          Substance62_low_cut = ifelse(round(.data$Substance62_first,2) >= .69, 1, 0),
                          Substance62_hi_cut = ifelse(round(.data$Substance62_first,2) >= 1, 1, 0),
                          Family62_low_cut = ifelse(round(.data$Family62_first,2) >= 1.31, 1, 0),
                          Family62_hi_cut = ifelse(round(.data$Family62_first,2) >= 1.83, 1, 0)
            )
          return(data)
          message("CCAPS-62 low cut scores based on 2019 norms and high cut scores based on 2018 norms.")
        }
      }
    }
}
