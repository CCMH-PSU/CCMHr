#' Create CCAPS-34 cut points
#'
#' @name ccaps34_cuts
#'
#' @description Creates binary variables for CCAPS-34 subscales indicating whether each subscale score is equal to, below (0), or above (1) the low and high cut scores, respectively.
#'
#' @param data A data file containing CCAPS-34 subscales scores.
#' @param version A numeric value to indicate the version of CCAPS cut point scores to use. Options include 2018, 2019, and 2025. By default, `2025`.
#' @param first A logical argument that indicates if the cut points should be based on variables ending with "\_first" (e.g., Depression34_first; variables created by the CCAPS_change function). If `TRUE`, the cut points should be based on variable names ending with "\_first". By default, `FALSE`.
#'
#' @note If the data frame does not contain the proper CCAPS subscale names, it will return an error. Two different variables are created. Variables that end "\_low_cut" (e.g., "Depression34_low_cut") indicate whether or not the client's score was below (0) or above (1) the low cut. Variables that end "\_hi_cut" (e.g., "Depression34_hi_cut") indicate whether or not the client's score was below (0) or above (1) the high cut.
#'
#' @return A data frame with all the original data and several additional columns of cut scores.
#'
#' @importFrom dplyr mutate
#'
#' @export

ccaps34_cuts <- function(data,
                         version = 2025,
                         first = FALSE){

  # Checking if columns are named correctly when first equals FALSE
  if(first == FALSE &
     !all(c("Depression34", "Anxiety34", "Social_Anxiety34", "Academics34", "Eating34", "Hostility34", "Alcohol34", "DI") %in% colnames(data))) {

      stop('CCAPS columns not named correctly.')

    # Checking if columns are named correctly when first equals TRUE
    } else if(first == TRUE &
              !all(c("Depression34_first", "Anxiety34_first", "Social_Anxiety34_first", "Academics34_first", "Eating34_first", "Hostility34_first", "Alcohol34_first", "DI_first") %in% colnames(data))) {

      stop('CCAPS columns not named correctly.')

      } else {

      # First CCAPS cut scores if first equals FALSE
      if(first == FALSE) {

        # 2018 version
        if(version == "2018") {

          data <- data |>
            dplyr::mutate(Depression34_low_cut     = ifelse(round(Depression34, 2) >= 1.00, 1, 0),
                          Depression34_hi_cut      = ifelse(round(Depression34, 2) >= 1.83, 1, 0),
                          Anxiety34_low_cut        = ifelse(round(Anxiety34, 2) >= 1.30, 1, 0),
                          Anxiety34_hi_cut         = ifelse(round(Anxiety34, 2) >= 2.10, 1, 0),
                          Social_Anxiety34_low_cut = ifelse(round(Social_Anxiety34, 2) >= 1.65, 1, 0),
                          Social_Anxiety34_hi_cut  = ifelse(round(Social_Anxiety34, 2) >= 2.50, 1, 0),
                          Academics34_low_cut      = ifelse(round(Academics34, 2) >= 1.45, 1, 0),
                          Academics34_hi_cut       = ifelse(round(Academics34, 2) >= 2.50, 1, 0),
                          Eating34_low_cut         = ifelse(round(Eating34, 2) >= 1.07, 1, 0),
                          Eating34_hi_cut          = ifelse(round(Eating34, 2) >= 1.50, 1, 0),
                          Hostility34_low_cut      = ifelse(round(Hostility34, 2) >= 0.74, 1, 0),
                          Hostility34_hi_cut       = ifelse(round(Hostility34, 2) >= 1.17, 1, 0),
                          Alcohol34_low_cut        = ifelse(round(Alcohol34, 2) >= 0.64, 1, 0),
                          Alcohol34_hi_cut         = ifelse(round(Alcohol34, 2) >= 1.10, 1, 0),
                          DI_low_cut               = ifelse(round(DI, 2) >= 1.21, 1, 0),
                          DI_hi_cut                = ifelse(round(DI, 2) >= 2.25, 1, 0))

          return(data)

          message("CCAPS-34 low cut scores are based on the original 2012 norms, while high cut scores are based on the 2018 norms.")

        # 2019 version
        } else if(version == "2019") {

          data <- data |>
            dplyr::mutate(Depression34_low_cut     = ifelse(round(Depression34, 2) >= 1.00, 1, 0),
                          Depression34_hi_cut      = ifelse(round(Depression34, 2) >= 1.83, 1, 0),
                          Anxiety34_low_cut        = ifelse(round(Anxiety34, 2) >= 1.33, 1, 0),
                          Anxiety34_hi_cut         = ifelse(round(Anxiety34, 2) >= 2.10, 1, 0),
                          Social_Anxiety34_low_cut = ifelse(round(Social_Anxiety34, 2) >= 1.40, 1, 0),
                          Social_Anxiety34_hi_cut  = ifelse(round(Social_Anxiety34, 2) >= 2.50, 1, 0),
                          Academics34_low_cut      = ifelse(round(Academics34, 2) >= 1.25, 1, 0),
                          Academics34_hi_cut       = ifelse(round(Academics34, 2) >= 2.50, 1, 0),
                          Eating34_low_cut         = ifelse(round(Eating34, 2) >= 0.96, 1, 0),
                          Eating34_hi_cut          = ifelse(round(Eating34, 2) >= 1.50, 1, 0),
                          Hostility34_low_cut      = ifelse(round(Hostility34, 2) >= 0.84, 1, 0),
                          Hostility34_hi_cut       = ifelse(round(Hostility34, 2) >= 1.17, 1, 0),
                          Alcohol34_low_cut        = ifelse(round(Alcohol34, 2) >= 0.60, 1, 0),
                          Alcohol34_hi_cut         = ifelse(round(Alcohol34, 2) >= 1.10, 1, 0),
                          DI_low_cut               = ifelse(round(DI, 2) >= 1.30, 1, 0),
                          DI_hi_cut                = ifelse(round(DI, 2) >= 2.25, 1, 0))

          return(data)

          message("CCAPS-34 low cut scores are based on 2019 norms, while high cut scores are based on 2018 norms.")

        # 2025 version
        } else if (version == "2025") {

          data <- data |>
            dplyr::mutate(Depression34_low_cut     = ifelse(round(Depression34, 2) >= 1.00, 1, 0),
                          Depression34_hi_cut      = ifelse(round(Depression34, 2) >= 1.80, 1, 0),
                          Anxiety34_low_cut        = ifelse(round(Anxiety34, 2) >= 1.33, 1, 0),
                          Anxiety34_hi_cut         = ifelse(round(Anxiety34, 2) >= 2.17, 1, 0),
                          Social_Anxiety34_low_cut = ifelse(round(Social_Anxiety34, 2) >= 1.60, 1, 0),
                          Social_Anxiety34_hi_cut  = ifelse(round(Social_Anxiety34, 2) >= 2.50, 1, 0),
                          Academics34_low_cut      = ifelse(round(Academics34, 2) >= 1.25, 1, 0),
                          Academics34_hi_cut       = ifelse(round(Academics34, 2) >= 2.50, 1, 0),
                          Eating34_low_cut         = ifelse(round(Eating34, 2) >= 1.03, 1, 0),
                          Eating34_hi_cut          = ifelse(round(Eating34, 2) >= 1.67, 1, 0),
                          Hostility34_low_cut      = ifelse(round(Hostility34, 2) >= 0.80, 1, 0),
                          Hostility34_hi_cut       = ifelse(round(Hostility34, 2) >= 1.17, 1, 0),
                          Alcohol34_low_cut        = ifelse(round(Alcohol34, 2) >= 0.44, 1, 0),
                          Alcohol34_hi_cut         = ifelse(round(Alcohol34, 2) >= 1.00, 1, 0),
                          DI_low_cut               = ifelse(round(DI, 2) >= 1.30, 1, 0),
                          DI_hi_cut                = ifelse(round(DI, 2) >= 2.20, 1, 0))

          return(data)

          message("CCAPS-34 low and high cut scores are based on 2025 norms.")

        }

      # First CCAPS cut scores if first equals TRUE
      } else if (first == TRUE) {

        # 2018 version
        if(version == "2018") {

          data <- data |>
            dplyr::mutate(Depression34_low_cut     = ifelse(round(Depression34_first, 2) >= 1.00, 1, 0),
                          Depression34_hi_cut      = ifelse(round(Depression34_first, 2) >= 1.83, 1, 0),
                          Anxiety34_low_cut        = ifelse(round(Anxiety34_first, 2) >= 1.30, 1, 0),
                          Anxiety34_hi_cut         = ifelse(round(Anxiety34_first, 2) >= 2.10, 1, 0),
                          Social_Anxiety34_low_cut = ifelse(round(Social_Anxiety34_first, 2) >= 1.65, 1, 0),
                          Social_Anxiety34_hi_cut  = ifelse(round(Social_Anxiety34_first, 2) >= 2.50, 1, 0),
                          Academics34_low_cut      = ifelse(round(Academics34_first, 2) >= 1.45, 1, 0),
                          Academics34_hi_cut       = ifelse(round(Academics34_first, 2) >= 2.50, 1, 0),
                          Eating34_low_cut         = ifelse(round(Eating34_first, 2) >= 1.07, 1, 0),
                          Eating34_hi_cut          = ifelse(round(Eating34_first, 2) >= 1.50, 1, 0),
                          Hostility34_low_cut      = ifelse(round(Hostility34_first, 2) >= 0.74, 1, 0),
                          Hostility34_hi_cut       = ifelse(round(Hostility34_first, 2) >= 1.17, 1, 0),
                          Alcohol34_low_cut        = ifelse(round(Alcohol34_first, 2) >= 0.64, 1, 0),
                          Alcohol34_hi_cut         = ifelse(round(Alcohol34_first, 2) >= 1.10, 1, 0),
                          DI_low_cut               = ifelse(round(DI_first, 2) >= 1.21, 1, 0),
                          DI_hi_cut                = ifelse(round(DI_first, 2) >= 2.25, 1, 0))

          return(data)

          message("CCAPS-34 low cut scores are based on the original 2012 norms, while high cut scores are based on the 2018 norms.")

        # 2019 version
        } else if (version == "2019") {

          data <- data |>
            dplyr::mutate(Depression34_low_cut     = ifelse(round(Depression34_first, 2) >= 1.00, 1, 0),
                          Depression34_hi_cut      = ifelse(round(Depression34_first, 2) >= 1.83, 1, 0),
                          Anxiety34_low_cut        = ifelse(round(Anxiety34_first, 2) >= 1.33, 1, 0),
                          Anxiety34_hi_cut         = ifelse(round(Anxiety34_first, 2) >= 2.10, 1, 0),
                          Social_Anxiety34_low_cut = ifelse(round(Social_Anxiety34_first, 2) >= 1.40, 1, 0),
                          Social_Anxiety34_hi_cut  = ifelse(round(Social_Anxiety34_first, 2) >= 2.50, 1, 0),
                          Academics34_low_cut      = ifelse(round(Academics34_first, 2) >= 1.25, 1, 0),
                          Academics34_hi_cut       = ifelse(round(Academics34_first, 2) >= 2.50, 1, 0),
                          Eating34_low_cut         = ifelse(round(Eating34_first, 2) >= 0.96, 1, 0),
                          Eating34_hi_cut          = ifelse(round(Eating34_first, 2) >= 1.50, 1, 0),
                          Hostility34_low_cut      = ifelse(round(Hostility34_first, 2) >= 0.84, 1, 0),
                          Hostility34_hi_cut       = ifelse(round(Hostility34_first, 2) >= 1.17, 1, 0),
                          Alcohol34_low_cut        = ifelse(round(Alcohol34_first, 2) >= 0.60, 1, 0),
                          Alcohol34_hi_cut         = ifelse(round(Alcohol34_first, 2) >= 1.10, 1, 0),
                          DI_low_cut               = ifelse(round(DI_first, 2) >= 1.30, 1, 0),
                          DI_hi_cut                = ifelse(round(DI_first, 2) >= 2.25, 1, 0))

          return(data)

          message("CCAPS-34 low cut scores are based on 2019 norms, while high cut scores are based on 2018 norms.")

        # 2025 version
        } else if (version == "2025") {

          data <- data |>
            dplyr::mutate(Depression34_low_cut     = ifelse(round(Depression34_first, 2) >= 1.00, 1, 0),
                          Depression34_hi_cut      = ifelse(round(Depression34_first, 2) >= 1.80, 1, 0),
                          Anxiety34_low_cut        = ifelse(round(Anxiety34_first, 2) >= 1.33, 1, 0),
                          Anxiety34_hi_cut         = ifelse(round(Anxiety34_first, 2) >= 2.17, 1, 0),
                          Social_Anxiety34_low_cut = ifelse(round(Social_Anxiety34_first, 2) >= 1.60, 1, 0),
                          Social_Anxiety34_hi_cut  = ifelse(round(Social_Anxiety34_first, 2) >= 2.50, 1, 0),
                          Academics34_low_cut      = ifelse(round(Academics34_first, 2) >= 1.25, 1, 0),
                          Academics34_hi_cut       = ifelse(round(Academics34_first, 2) >= 2.50, 1, 0),
                          Eating34_low_cut         = ifelse(round(Eating34_first, 2) >= 1.03, 1, 0),
                          Eating34_hi_cut          = ifelse(round(Eating34_first, 2) >= 1.67, 1, 0),
                          Hostility34_low_cut      = ifelse(round(Hostility34_first, 2) >= 0.80, 1, 0),
                          Hostility34_hi_cut       = ifelse(round(Hostility34_first, 2) >= 1.17, 1, 0),
                          Alcohol34_low_cut        = ifelse(round(Alcohol34_first, 2) >= 0.44, 1, 0),
                          Alcohol34_hi_cut         = ifelse(round(Alcohol34_first, 2) >= 1.00, 1, 0),
                          DI_low_cut               = ifelse(round(DI_first, 2) >= 1.30, 1, 0),
                          DI_hi_cut                = ifelse(round(DI_first, 2) >= 2.20, 1, 0))

          return(data)

          message("CCAPS-34 low and high cut scores are based on 2025 norms.")

        }
      }
    }
  }

#' @rdname ccaps34_cuts
#' @export
CCAPS34_cuts <- ccaps34_cuts
