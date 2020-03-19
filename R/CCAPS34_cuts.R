#' Create CCAPS-34 cut points
#'
#' @description Creates binary variables for CCAPS-34 subscales indicating whether the score is equal to or above (1) or below (0) the low and high cut respectively.
#' @param data A data file containing CCAPS-34 subscales
#' @param version The version of CCAPS cut points to use. Currently, 2018 & 2019 are available.
#' @param first True indicates the cut points should be based on variabels with _first.
#' @note If `data` does not contain the proper CCAPS subscale names, it will return an error.
#' @return A data frame with all the original data in `data`, and several additional columns of cut scores coded 1 for above cut score and 0 for below cut score.
#' @examples \dontrun{
#' TI1718 <- ccaps34_cuts(TI1718)
#' }
#' @export

ccaps34_cuts <- function(data, version = c("2018", "2019"), first = F){
  if(first == F & !all(c("Depression34", "Anxiety34", "Social_Anxiety34", "Academics34", "Eating34", "Hostility34", "Alcohol34", "DI") %in% colnames(data))) {
    stop('CCAPS columns not named correctly.')

    } else if(first == T & !all(c("Depression34_first", "Anxiety34_first", "Social_Anxiety34_first", "Academics34_first", "Eating34_first", "Hostility34_first", "Alcohol34_first", "DI_first") %in% colnames(data))) {
      stop('CCAPS columns not named correctly.')
    } else {
      # First CCAPS
      if(first == F ) {
        if(version == "2018") {
          data <- data %>%
            dplyr::mutate(Depression34_low_cut     = ifelse(round(.data$Depression34, 2) >= 1, 1, 0),
                          Depression34_hi_cut      = ifelse(round(.data$Depression34, 2) >= 1.83, 1, 0),
                          Anxiety34_low_cut        = ifelse(round(.data$Anxiety34, 2) >= 1.3, 1, 0),
                          Anxiety34_hi_cut         = ifelse(round(.data$Anxiety34,2) >= 2.1, 1, 0),
                          Social_Anxiety34_low_cut = ifelse(round(.data$Social_Anxiety34, 2) >= 1.65, 1, 0),
                          Social_Anxiety34_hi_cut  = ifelse(round(.data$Social_Anxiety34,2) >= 2.5, 1, 0),
                          Academics34_low_cut      = ifelse(round(.data$Academics34,2) >= 1.45, 1, 0),
                          Academics34_hi_cut       = ifelse(round(.data$Academics34,2) >= 2.50, 1, 0),
                          Eating34_low_cut         = ifelse(round(.data$Eating34,2) >= 1.07, 1, 0),
                          Eating34_hi_cut          = ifelse(round(.data$Eating34,2) >= 1.50, 1, 0),
                          Hostility34_low_cut      = ifelse(round(.data$Hostility34,2) >= .74, 1, 0),
                          Hostility34_hi_cut       = ifelse(round(.data$Hostility34,2) >= 1.17, 1, 0),
                          Alcohol34_low_cut        = ifelse(round(.data$Alcohol34,2) >= .64, 1, 0),
                          Alcohol34_hi_cut         = ifelse(round(.data$Alcohol34,2) >= 1.10, 1, 0),
                          DI_low_cut               = ifelse(round(.data$DI,2) >= 1.21, 1, 0),
                          DI_hi_cut                = ifelse(round(.data$DI,2) >= 2.25, 1, 0)
            )
          return(data)
          message("CCAPS-34 low cut scores based on original 2012 norms and high cut scores based on 2018 norms.")
        } else if (version == "2019") {
          data <- data %>%
            dplyr::mutate(Depression34_low_cut     = ifelse(round(.data$Depression34, 2) >= 1, 1, 0),
                          Depression34_hi_cut      = ifelse(round(.data$Depression34, 2) >= 1.83, 1, 0),
                          Anxiety34_low_cut        = ifelse(round(.data$Anxiety34, 2) >= 1.33, 1, 0),
                          Anxiety34_hi_cut         = ifelse(round(.data$Anxiety34,2) >= 2.1, 1, 0),
                          Social_Anxiety34_low_cut = ifelse(round(.data$Social_Anxiety34, 2) >= 1.4, 1, 0),
                          Social_Anxiety34_hi_cut  = ifelse(round(.data$Social_Anxiety34,2) >= 2.5, 1, 0),
                          Academics34_low_cut      = ifelse(round(.data$Academics34,2) >= 1.25, 1, 0),
                          Academics34_hi_cut       = ifelse(round(.data$Academics34,2) >= 2.50, 1, 0),
                          Eating34_low_cut         = ifelse(round(.data$Eating34,2) >= .96, 1, 0),
                          Eating34_hi_cut          = ifelse(round(.data$Eating34,2) >= 1.50, 1, 0),
                          Hostility34_low_cut      = ifelse(round(.data$Hostility34,2) >= .84, 1, 0),
                          Hostility34_hi_cut       = ifelse(round(.data$Hostility34,2) >= 1.17, 1, 0),
                          Alcohol34_low_cut        = ifelse(round(.data$Alcohol34,2) >= .6, 1, 0),
                          Alcohol34_hi_cut         = ifelse(round(.data$Alcohol34,2) >= 1.10, 1, 0),
                          DI_low_cut               = ifelse(round(.data$DI,2) >= 1.3, 1, 0),
                          DI_hi_cut                = ifelse(round(.data$DI,2) >= 2.25, 1, 0)
            )
          return(data)
          message("CCAPS-34 low cut scores based on 2019 norms and high cut scores based on 2018 norms.")
        }

      } else if (first == T) {
        if(version == "2018") {
          data <- data %>%
            dplyr::mutate(Depression34_low_cut     = ifelse(round(.data$Depression34_first, 2) >= 1.00, 1, 0),
                          Depression34_hi_cut      = ifelse(round(.data$Depression34_first, 2) >= 1.83, 1, 0),
                          Anxiety34_low_cut        = ifelse(round(.data$Anxiety34_first, 2) >= 1.30, 1, 0),
                          Anxiety34_hi_cut         = ifelse(round(.data$Anxiety34_first,2) >= 2.10, 1, 0),
                          Social_Anxiety34_low_cut = ifelse(round(.data$Social_Anxiety34_first, 2) >= 1.65, 1, 0),
                          Social_Anxiety34_hi_cut  = ifelse(round(.data$Social_Anxiety34_first,2) >= 2.50, 1, 0),
                          Academics34_low_cut      = ifelse(round(.data$Academics34_first,2) >= 1.45, 1, 0),
                          Academics34_hi_cut       = ifelse(round(.data$Academics34_first,2) >= 2.50, 1, 0),
                          Eating34_low_cut         = ifelse(round(.data$Eating34_first,2) >= 1.07, 1, 0),
                          Eating34_hi_cut          = ifelse(round(.data$Eating34_first,2) >= 1.50, 1, 0),
                          Hostility34_low_cut      = ifelse(round(.data$Hostility34_first,2) >= .74, 1, 0),
                          Hostility34_hi_cut       = ifelse(round(.data$Hostility34_first,2) >= 1.17, 1, 0),
                          Alcohol34_low_cut        = ifelse(round(.data$Alcohol34_first,2) >= .64, 1, 0),
                          Alcohol34_hi_cut         = ifelse(round(.data$Alcohol34_first,2) >= 1.10, 1, 0),
                          DI_low_cut               = ifelse(round(.data$DI_first,2) >= 1.21, 1, 0),
                          DI_hi_cut                = ifelse(round(.data$DI_first,2) >= 2.25, 1, 0)
            )
          return(data)
          message("CCAPS-34 low cut scores based on original 2012 norms and high cut scores based on 2018 norms.")
        } else if (version == "2019") {
          data <- data %>%
            dplyr::mutate(Depression34_low_cut     = ifelse(round(.data$Depression34_first, 2) >= 1.00, 1, 0),
                          Depression34_hi_cut      = ifelse(round(.data$Depression34_first, 2) >= 1.83, 1, 0),
                          Anxiety34_low_cut        = ifelse(round(.data$Anxiety34_first, 2) >= 1.33, 1, 0),
                          Anxiety34_hi_cut         = ifelse(round(.data$Anxiety34_first,2) >= 2.10, 1, 0),
                          Social_Anxiety34_low_cut = ifelse(round(.data$Social_Anxiety34_first, 2) >= 1.4, 1, 0),
                          Social_Anxiety34_hi_cut  = ifelse(round(.data$Social_Anxiety34_first,2) >= 2.50, 1, 0),
                          Academics34_low_cut      = ifelse(round(.data$Academics34_first,2) >= 1.25, 1, 0),
                          Academics34_hi_cut       = ifelse(round(.data$Academics34_first,2) >= 2.50, 1, 0),
                          Eating34_low_cut         = ifelse(round(.data$Eating34_first,2) >= .96, 1, 0),
                          Eating34_hi_cut          = ifelse(round(.data$Eating34_first,2) >= 1.50, 1, 0),
                          Hostility34_low_cut      = ifelse(round(.data$Hostility34_first,2) >= .84, 1, 0),
                          Hostility34_hi_cut       = ifelse(round(.data$Hostility34_first,2) >= 1.17, 1, 0),
                          Alcohol34_low_cut        = ifelse(round(.data$Alcohol34_first,2) >= .6, 1, 0),
                          Alcohol34_hi_cut         = ifelse(round(.data$Alcohol34_first,2) >= 1.10, 1, 0),
                          DI_low_cut               = ifelse(round(.data$DI_first,2) >= 1.3, 1, 0),
                          DI_hi_cut                = ifelse(round(.data$DI_first,2) >= 2.25, 1, 0)
            )
          return(data)
          message("CCAPS-34 low cut scores based on 2019 norms and high cut scores based on 2018 norms.")
        }
      }
    }
  }
