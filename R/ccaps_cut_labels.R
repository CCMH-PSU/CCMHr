#' Creates label variables for CCAPS-62 and/or CCAPS-34 subscales cut scores.
#'
#' @description The functions create label variables for each CCAPS-62 and/or CCAPS-34 subscale cut score.
#'
#' @param data A data file containing CCAPS-62 and/or CCAPS-34 subscales cut scores.
#'
#' @note To run this function, cut scores must have been created using ccaps34_cuts and/or ccaps62_cuts functions. Three types of label variables are created. Variables ending with "_sum" (e.g., Depression34_cut_sum) represent the sum of the low and high cut variables (e.g., Depression34_low_cut + Depression34_hi_cut), with possible outputs of 0, 1, or 2. Variables that end with "_sum_label1" (e.g., Depression34_cut_sum_label1) are labels based on the "_sum" variables where the data was recoded: 0 = "Low", 1 = "Moderate", and 2 = "Elevated". Variables that end with "_sum_label2" (e.g., Depression34_cut_sum_label2) are labels based on the "_sum" variables where the data was recoded: 0 = "Not elevated", 1 = "Not elevated", and 2 = "Elevated".
#'
#' @return A data frame with all the original data and additional columns of cut score labels.
#'
#' @importFrom dplyr across
#' @importFrom dplyr mutate
#' @importFrom dplyr recode
#'
#' @export

ccaps_cut_labels <- function(data){

  # Extracting variable names from the data frame
  var.names <- names(data)

  # Required CCAPS62 names
  req.ccaps62 <- c("Depression62_low_cut", "Depression62_hi_cut",
                   "Anxiety62_low_cut", "Anxiety62_hi_cut",
                   "Social_Anxiety62_low_cut", "Social_Anxiety62_hi_cut",
                   "Academics62_low_cut", "Academics62_hi_cut",
                   "Eating62_low_cut", "Eating62_hi_cut",
                   "Hostility62_low_cut", "Hostility62_hi_cut",
                   "Substance62_low_cut", "Substance62_hi_cut",
                   "Family62_low_cut", "Family62_hi_cut")

  # Required CCAPS34 names
  req.ccaps34 <- c("Depression34_low_cut", "Depression34_hi_cut",
                   "Anxiety34_low_cut", "Anxiety34_hi_cut",
                   "Social_Anxiety34_low_cut", "Social_Anxiety34_hi_cut",
                   "Academics34_low_cut", "Academics34_hi_cut",
                   "Eating34_low_cut", "Eating34_hi_cut",
                   "Hostility34_low_cut", "Hostility34_hi_cut",
                   "Alcohol34_low_cut", "Alcohol34_hi_cut",
                   "DI_low_cut", "DI_hi_cut")

  # Checking if the required variables are present and determine if CCAPS variables pertain to a CCAPS34 and/or CCAPS62.
  if(all(req.ccaps62 %in% var.names) == TRUE &
     all(req.ccaps34 %in% var.names) == FALSE){

    # Creating a flag variable
    flag <- "CCAPS62"

  } else if(all(req.ccaps62 %in% var.names) == FALSE &
            all(req.ccaps34 %in% var.names) == TRUE){

    # Creating a flag variable
    flag <- "CCAPS34"

  } else if(all(req.ccaps62 %in% var.names) == TRUE &
            all(req.ccaps34 %in% var.names) == TRUE){

    # Creating a flag variable
    flag <- "Both"

  } else {

    stop("Data frame does not contain all the required CCAPS cut variables.")

  }

  # Mutating data frame based on flag variable
  if(flag == "CCAPS62"){

    # CCAPS62 only
    data <- data |>
      dplyr::mutate(Depression62_cut_sum = Depression62_low_cut + Depression62_hi_cut,
                    Anxiety62_cut_sum = Anxiety62_low_cut + Anxiety62_hi_cut,
                    Social_Anxiety62_cut_sum = Social_Anxiety62_low_cut + Social_Anxiety62_hi_cut,
                    Academics62_cut_sum = Academics62_low_cut + Academics62_hi_cut,
                    Eating62_cut_sum = Eating62_low_cut + Eating62_hi_cut,
                    Hostility62_cut_sum = Hostility62_low_cut + Hostility62_hi_cut,
                    Substance62_cut_sum = Substance62_low_cut + Substance62_hi_cut,
                    Family62_cut_sum = Family62_low_cut + Family62_hi_cut) |>
      dplyr::mutate(dplyr::across(c(Depression62_cut_sum:Family62_cut_sum),
                    ~ dplyr::recode(., `0` = "Low", `1` = "Moderate", `2` = "Elevated"),
                    .names = "{.col}_label1")) |>
      dplyr::mutate(dplyr::across(c(Depression62_cut_sum:Family62_cut_sum),
                                  ~ dplyr::recode(., `0` = "Not elevated", `1` = "Not elevated", `2` = "Elevated"),
                                  .names = "{.col}_label2"))

  } else if(flag == "CCAPS34"){

    # CCAPS34 only
    data <- data |>
      dplyr::mutate(Depression34_cut_sum = Depression34_low_cut + Depression34_hi_cut,
                    Anxiety34_cut_sum = Anxiety34_low_cut + Anxiety34_hi_cut,
                    Social_Anxiety34_cut_sum = Social_Anxiety34_low_cut + Social_Anxiety34_hi_cut,
                    Academics34_cut_sum = Academics34_low_cut + Academics34_hi_cut,
                    Eating34_cut_sum = Eating34_low_cut + Eating34_hi_cut,
                    Hostility34_cut_sum = Hostility34_low_cut + Hostility34_hi_cut,
                    Alcohol34_cut_sum = Alcohol34_low_cut + Alcohol34_hi_cut,
                    DI_cut_sum = DI_low_cut + DI_hi_cut) |>
      dplyr::mutate(dplyr::across(c(Depression34_cut_sum:DI_cut_sum),
                                  ~ dplyr::recode(., `0` = "Low", `1` = "Moderate", `2` = "Elevated"),
                                  .names = "{.col}_label1")) |>
      dplyr::mutate(dplyr::across(c(Depression34_cut_sum:DI_cut_sum),
                                  ~ dplyr::recode(., `0` = "Not elevated", `1` = "Not elevated", `2` = "Elevated"),
                                  .names = "{.col}_label2"))

  } else{

    # Both CCAPS62 and CCAPS34
    data <- data |>
      dplyr::mutate(Depression62_cut_sum = Depression62_low_cut + Depression62_hi_cut,
                    Anxiety62_cut_sum = Anxiety62_low_cut + Anxiety62_hi_cut,
                    Social_Anxiety62_cut_sum = Social_Anxiety62_low_cut + Social_Anxiety62_hi_cut,
                    Academics62_cut_sum = Academics62_low_cut + Academics62_hi_cut,
                    Eating62_cut_sum = Eating62_low_cut + Eating62_hi_cut,
                    Hostility62_cut_sum = Hostility62_low_cut + Hostility62_hi_cut,
                    Substance62_cut_sum = Substance62_low_cut + Substance62_hi_cut,
                    Family62_cut_sum = Family62_low_cut + Family62_hi_cut,
                    Depression34_cut_sum = Depression34_low_cut + Depression34_hi_cut,
                    Anxiety34_cut_sum = Anxiety34_low_cut + Anxiety34_hi_cut,
                    Social_Anxiety34_cut_sum = Social_Anxiety34_low_cut + Social_Anxiety34_hi_cut,
                    Academics34_cut_sum = Academics34_low_cut + Academics34_hi_cut,
                    Eating34_cut_sum = Eating34_low_cut + Eating34_hi_cut,
                    Hostility34_cut_sum = Hostility34_low_cut + Hostility34_hi_cut,
                    Alcohol34_cut_sum = Alcohol34_low_cut + Alcohol34_hi_cut,
                    DI_cut_sum = DI_low_cut + DI_hi_cut) |>
      dplyr::mutate(dplyr::across(c(Depression62_cut_sum:DI_cut_sum),
                                  ~ dplyr::recode(., `0` = "Low", `1` = "Moderate", `2` = "Elevated"),
                                  .names = "{.col}_label1")) |>
      dplyr::mutate(dplyr::across(c(Depression62_cut_sum:DI_cut_sum),
                                  ~ dplyr::recode(., `0` = "Not elevated", `1` = "Not elevated", `2` = "Elevated"),
                                  .names = "{.col}_label2"))

  }

  # Return data
  return(data)

}

