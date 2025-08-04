#' Remove free-response CCMH variables.
#'
#' @description This function removes variables that are free responses.
#'
#' @param data A CCMH data frame.
#' @param keep A quoted string or list of quoted strings that specify which free response variables should be kept. If `NULL`, all free response variables will be removed. By default, `NULL`.
#'
#' @return By default, a data frame without the following variables: "SDS_16", "SDS_18", "SDS_21", "SDS_29", "SDS_30", "SDS_35", "SDS_38", "SDS_40", "SDS_43", "SDS_45", "SDS_47", "SDS_49", "SDS_54", "SDS_55", "SDS_89", "SDS_92", "SDS_1047", "SDS_2047", "SDS_3047", "SDS_101", "SDS_129", "Services_Other_Description", "Psychiatric_Services_Other_Description", "ChargeFor_Other_Description", "TherEthnicity_Other", "Highest_Degree_Other", "Highest_Degree_Discipline_Other", "Position_Other", "Grading_Scale_Other_Description", "CCAPSFrequencyOther", "CLOSURE_02", "CLOSURE_05", "CIF_02".
#'
#' @importFrom dplyr select
#' @importFrom tidyselect any_of
#'
#' @export

remove_free_response <- function(data,
                                 keep = NULL){

  # List of free response variables
  free.response.list <- c("SDS_16", "SDS_18",
                          "SDS_21", "SDS_29",
                          "SDS_30", "SDS_35",
                          "SDS_38", "SDS_40",
                          "SDS_43", "SDS_45",
                          "SDS_47", "SDS_49",
                          "SDS_54", "SDS_55",
                          "SDS_89", "SDS_92",
                          "SDS_1047", "SDS_2047",
                          "SDS_3047", "SDS_101",
                          "SDS_129", "Services_Other_Description",
                          "Psychiatric_Services_Other_Description", "ChargeFor_Other_Description",
                          "TherEthnicity_Other", "Highest_Degree_Other",
                          "Highest_Degree_Discipline_Other", "Position_Other",
                          "Grading_Scale_Other_Description", "CCAPSFrequencyOther",
                          "CLOSURE_02", "CLOSURE_05",
                          "CIF_02")

  # Specify columns to keep
  if(!is.null(keep)){

    # Specify new list of free response variables
    free.response.list <- free.response.list[!free.response.list %in% keep]

  } else{

  }

  # Exclude columns that are free responses
  data <- data |>
    dplyr::select(-tidyselect::any_of(free.response.list))

  # Return
  return(data)

}
