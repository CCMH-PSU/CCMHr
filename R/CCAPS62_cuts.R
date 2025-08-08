#' Create CCAPS-62 cut points
#'
#' @name ccaps62_cuts
#'
#' @description Creates binary variables for CCAPS-62 subscales indicating whether each subscale score is equal to or below (0) or above (1) the low and high cut scores, respectively.
#'
#' @param data A data file containing CCAPS-62 subscales scores.
#' @param version A numeric value to indicate the version of CCAPS cut point scores to use. Options include 2018, 2019, and 2025. By default, `2025`.
#' @param first A logical argument that indicates if the cut points should be based on variables ending with "\_first" (e.g., Depression62_first; variables created by the CCAPS_change function). If `TRUE`, the cut points should be based on variable names ending with "\_first". By default, `FALSE`.
#'
#' @note If the data frame does not contain the proper CCAPS subscale names, it will return an error. Two different variables are created. Variables that end "\_low_cut" (e.g., "Depression62_low_cut") indicate whether or not the client's score was below (0) or above (1) the low cut. Variables that end "\_hi_cut" (e.g., "Depression62_hi_cut") indicate whether or not the client's score was below (0) or above (1) the high cut. The Distress Index (DI) cut scores are not included. To add DI cut scores, a programmer must create a data frame with CCAPS34 cut scores using the ccaps34_cuts function and merge that data frame with a data frame that contains CCAPS62 subscale cut scores.
#'
#' @return A data frame with all the original data and several additional columns of cut scores.
#'
#' @importFrom dplyr mutate
#'
#' @export

ccaps62_cuts <- function(data,
                         version = 2025,
                         first = FALSE){

  # Checking if columns are named correctly when first equals FALSE
  if(first == FALSE &
     !all(c("Depression62", "Anxiety62", "Social_Anxiety62", "Academics62", "Eating62", "Hostility62", "Substance62", "DI") %in% colnames(data))) {

    stop('CCAPS columns not named correctly.')

    # Checking if columns are named correctly when first equals TRUE
    } else if(first == TRUE &
              !all(c("Depression62_first", "Anxiety62_first", "Social_Anxiety62_first", "Academics62_first", "Eating62_first", "Hostility62_first", "Substance62_first", "DI_first") %in% colnames(data))) {

      stop('CCAPS columns not named correctly.')

      } else {

      # First CCAPS cut scores if first equals FALSE
      if(first == FALSE) {

        # 2018 version
        if(version == "2018") {

          data <- data |>
            dplyr::mutate(Depression62_low_cut     = ifelse(round(Depression62, 2) >= 1.09, 1, 0),
                          Depression62_hi_cut      = ifelse(round(Depression62, 2) >= 1.92, 1, 0),
                          Anxiety62_low_cut        = ifelse(round(Anxiety62, 2) >= 1.25, 1, 0),
                          Anxiety62_hi_cut         = ifelse(round(Anxiety62, 2) >= 1.89, 1, 0),
                          Social_Anxiety62_low_cut = ifelse(round(Social_Anxiety62, 2) >= 1.72, 1, 0),
                          Social_Anxiety62_hi_cut  = ifelse(round(Social_Anxiety62, 2) >= 2.57, 1, 0),
                          Academics62_low_cut      = ifelse(round(Academics62, 2) >= 1.42, 1, 0),
                          Academics62_hi_cut       = ifelse(round(Academics62, 2) >= 2.40, 1, 0),
                          Eating62_low_cut         = ifelse(round(Eating62, 2) >= 1.09, 1, 0),
                          Eating62_hi_cut          = ifelse(round(Eating62, 2) >= 1.80, 1, 0),
                          Hostility62_low_cut      = ifelse(round(Hostility62, 2) >= 0.82, 1, 0),
                          Hostility62_hi_cut       = ifelse(round(Hostility62, 2) >= 1.43, 1, 0),
                          Substance62_low_cut      = ifelse(round(Substance62, 2) >= 0.70, 1, 0),
                          Substance62_hi_cut       = ifelse(round(Substance62, 2) >= 1.00, 1, 0),
                          Family62_low_cut         = ifelse(round(Family62, 2) >= 0.98, 1, 0),
                          Family62_hi_cut          = ifelse(round(Family62, 2) >= 1.83, 1, 0))

          return(data)

          message("CCAPS-62 low cut scores are based on the original 2012 norms, while high cut scores are based on the 2018 norms.")

        # 2019 version
        } else if (version == "2019") {

          data <- data |>
            dplyr::mutate(Depression62_low_cut     = ifelse(round(Depression62, 2) >= 1.23, 1, 0),
                          Depression62_hi_cut      = ifelse(round(Depression62, 2) >= 1.92, 1, 0),
                          Anxiety62_low_cut        = ifelse(round(Anxiety62, 2) >= 1.22, 1, 0),
                          Anxiety62_hi_cut         = ifelse(round(Anxiety62, 2) >= 1.89, 1, 0),
                          Social_Anxiety62_low_cut = ifelse(round(Social_Anxiety62, 2) >= 1.43, 1, 0),
                          Social_Anxiety62_hi_cut  = ifelse(round(Social_Anxiety62, 2) >= 2.57, 1, 0),
                          Academics62_low_cut      = ifelse(round(Academics62, 2) >= 1.20, 1, 0),
                          Academics62_hi_cut       = ifelse(round(Academics62, 2) >= 2.40, 1, 0),
                          Eating62_low_cut         = ifelse(round(Eating62, 2) >= 1.02, 1, 0),
                          Eating62_hi_cut          = ifelse(round(Eating62, 2) >= 1.80, 1, 0),
                          Hostility62_low_cut      = ifelse(round(Hostility62, 2) >= 1.00, 1, 0),
                          Hostility62_hi_cut       = ifelse(round(Hostility62, 2) >= 1.43, 1, 0),
                          Substance62_low_cut      = ifelse(round(Substance62, 2) >= 0.69, 1, 0),
                          Substance62_hi_cut       = ifelse(round(Substance62, 2) >= 1.00, 1, 0),
                          Family62_low_cut         = ifelse(round(Family62, 2) >= 1.31, 1, 0),
                          Family62_hi_cut          = ifelse(round(Family62, 2) >= 1.83, 1, 0))

            return(data)

            message("CCAPS-62 low cut scores are based on 2019 norms, while high cut scores are based on 2018 norms.")

        # 2025 version
        } else if (version == "2025") {

          data <- data |>
            dplyr::mutate(Depression62_low_cut     = ifelse(round(Depression62, 2) >= 1.23, 1, 0),
                          Depression62_hi_cut      = ifelse(round(Depression62, 2) >= 1.90, 1, 0),
                          Anxiety62_low_cut        = ifelse(round(Anxiety62, 2) >= 1.33, 1, 0),
                          Anxiety62_hi_cut         = ifelse(round(Anxiety62, 2) >= 1.86, 1, 0),
                          Social_Anxiety62_low_cut = ifelse(round(Social_Anxiety62, 2) >= 1.57, 1, 0),
                          Social_Anxiety62_hi_cut  = ifelse(round(Social_Anxiety62, 2) >= 2.57, 1, 0),
                          Academics62_low_cut      = ifelse(round(Academics62, 2) >= 1.40, 1, 0),
                          Academics62_hi_cut       = ifelse(round(Academics62, 2) >= 2.40, 1, 0),
                          Eating62_low_cut         = ifelse(round(Eating62, 2) >= 1.10, 1, 0),
                          Eating62_hi_cut          = ifelse(round(Eating62, 2) >= 1.62, 1, 0),
                          Hostility62_low_cut      = ifelse(round(Hostility62, 2) >= 0.97, 1, 0),
                          Hostility62_hi_cut       = ifelse(round(Hostility62, 2) >= 1.29, 1, 0),
                          Substance62_low_cut      = ifelse(round(Substance62, 2) >= 0.54, 1, 0),
                          Substance62_hi_cut       = ifelse(round(Substance62, 2) >= 0.83, 1, 0),
                          Family62_low_cut         = ifelse(round(Family62, 2) >= 1.42, 1, 0),
                          Family62_hi_cut          = ifelse(round(Family62, 2) >= 2.00, 1, 0))

          return(data)

          message("CCAPS-62 low and high cut scores are based on 2025 norms.")

        }

      # First CCAPS cut scores if first equals TRUE
      } else if (first == TRUE) {

        # 2018 version
        if(version == "2018") {

          data <- data |>
            dplyr::mutate(Depression62_low_cut     = ifelse(round(Depression62_first, 2) >= 1.09, 1, 0),
                          Depression62_hi_cut      = ifelse(round(Depression62_first, 2) >= 1.92, 1, 0),
                          Anxiety62_low_cut        = ifelse(round(Anxiety62_first, 2) >= 1.25, 1, 0),
                          Anxiety62_hi_cut         = ifelse(round(Anxiety62_first, 2) >= 1.89, 1, 0),
                          Social_Anxiety62_low_cut = ifelse(round(Social_Anxiety62_first, 2) >= 1.72, 1, 0),
                          Social_Anxiety62_hi_cut  = ifelse(round(Social_Anxiety62_first, 2) >= 2.57, 1, 0),
                          Academics62_low_cut      = ifelse(round(Academics62_first, 2) >= 1.42, 1, 0),
                          Academics62_hi_cut       = ifelse(round(Academics62_first, 2) >= 2.40, 1, 0),
                          Eating62_low_cut         = ifelse(round(Eating62_first, 2) >= 1.09, 1, 0),
                          Eating62_hi_cut          = ifelse(round(Eating62_first, 2) >= 1.80, 1, 0),
                          Hostility62_low_cut      = ifelse(round(Hostility62_first, 2) >= 0.82, 1, 0),
                          Hostility62_hi_cut       = ifelse(round(Hostility62_first, 2) >= 1.43, 1, 0),
                          Substance62_low_cut      = ifelse(round(Substance62_first, 2) >= 0.70, 1, 0),
                          Substance62_hi_cut       = ifelse(round(Substance62_first, 2) >= 1.00, 1, 0),
                          Family62_low_cut         = ifelse(round(Family62_first, 2) >= 0.98, 1, 0),
                          Family62_hi_cut          = ifelse(round(Family62_first, 2) >= 1.83, 1, 0))

          return(data)

          message("CCAPS-62 low cut scores are based on the original 2012 norms, while high cut scores are based on the 2018 norms.")

        # 2019 version
        } else if (version == "2019") {

          data <- data |>
            dplyr::mutate(Depression62_low_cut     = ifelse(round(Depression62_first, 2) >= 1.23, 1, 0),
                          Depression62_hi_cut      = ifelse(round(Depression62_first, 2) >= 1.92, 1, 0),
                          Anxiety62_low_cut        = ifelse(round(Anxiety62_first, 2) >= 1.22, 1, 0),
                          Anxiety62_hi_cut         = ifelse(round(Anxiety62_first, 2) >= 1.89, 1, 0),
                          Social_Anxiety62_low_cut = ifelse(round(Social_Anxiety62_first, 2) >= 1.43, 1, 0),
                          Social_Anxiety62_hi_cut  = ifelse(round(Social_Anxiety62_first, 2) >= 2.57, 1, 0),
                          Academics62_low_cut      = ifelse(round(Academics62_first, 2) >= 1.20, 1, 0),
                          Academics62_hi_cut       = ifelse(round(Academics62_first, 2) >= 2.40, 1, 0),
                          Eating62_low_cut         = ifelse(round(Eating62_first, 2) >= 1.02, 1, 0),
                          Eating62_hi_cut          = ifelse(round(Eating62_first, 2) >= 1.80, 1, 0),
                          Hostility62_low_cut      = ifelse(round(Hostility62_first, 2) >= 1.00, 1, 0),
                          Hostility62_hi_cut       = ifelse(round(Hostility62_first, 2) >= 1.43, 1, 0),
                          Substance62_low_cut      = ifelse(round(Substance62_first, 2) >= 0.69, 1, 0),
                          Substance62_hi_cut       = ifelse(round(Substance62_first, 2) >= 1.00, 1, 0),
                          Family62_low_cut         = ifelse(round(Family62_first, 2) >= 1.31, 1, 0),
                          Family62_hi_cut          = ifelse(round(Family62_first, 2) >= 1.83, 1, 0))

          return(data)

          message("CCAPS-62 low cut scores are based on 2019 norms, while high cut scores are based on 2018 norms.")

        # 2025 version
        } else if (version == "2025") {

          data <- data |>
            dplyr::mutate(Depression62_low_cut     = ifelse(round(Depression62_first, 2) >= 1.23, 1, 0),
                          Depression62_hi_cut      = ifelse(round(Depression62_first, 2) >= 1.90, 1, 0),
                          Anxiety62_low_cut        = ifelse(round(Anxiety62_first, 2) >= 1.33, 1, 0),
                          Anxiety62_hi_cut         = ifelse(round(Anxiety62_first, 2) >= 1.86, 1, 0),
                          Social_Anxiety62_low_cut = ifelse(round(Social_Anxiety62_first, 2) >= 1.57, 1, 0),
                          Social_Anxiety62_hi_cut  = ifelse(round(Social_Anxiety62_first, 2) >= 2.57, 1, 0),
                          Academics62_low_cut      = ifelse(round(Academics62_first, 2) >= 1.40, 1, 0),
                          Academics62_hi_cut       = ifelse(round(Academics62_first, 2) >= 2.40, 1, 0),
                          Eating62_low_cut         = ifelse(round(Eating62_first, 2) >= 1.10, 1, 0),
                          Eating62_hi_cut          = ifelse(round(Eating62_first, 2) >= 1.62, 1, 0),
                          Hostility62_low_cut      = ifelse(round(Hostility62_first, 2) >= 0.97, 1, 0),
                          Hostility62_hi_cut       = ifelse(round(Hostility62_first, 2) >= 1.29, 1, 0),
                          Substance62_low_cut      = ifelse(round(Substance62_first, 2) >= 0.54, 1, 0),
                          Substance62_hi_cut       = ifelse(round(Substance62_first, 2) >= 0.83, 1, 0),
                          Family62_low_cut         = ifelse(round(Family62_first, 2) >= 1.42, 1, 0),
                          Family62_hi_cut          = ifelse(round(Family62_first, 2) >= 2.00, 1, 0))

          return(data)

          message("CCAPS-62 low and high cut scores are based on 2025 norms.")

        }
      }
    }
  }

#' @rdname ccaps62_cuts
#' @export
CCAPS62_cuts <- ccaps62_cuts
