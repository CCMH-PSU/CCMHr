#' Remove free-response CCMH variables.
#'
#' @description This function removes variables that are free responses.
#'
#' @param data A CCMH data frame.
#'
#' @return A data frame without the following variables: "SDS_16", "SDS_18", "SDS_21", "SDS_29", "SDS_30", "SDS_35", "SDS_38", "SDS_40", "SDS_43", "SDS_45", "SDS_47", "SDS_49", "SDS_54", "SDS_55", "SDS_89", "SDS_92", "SDS_1047", "SDS_101", "SDS_129", "Services_Other_Description", "Psychiatric_Services_Other_Description", "ChargeFor_Other_Description", "TherEthnicity_Other", "Highest_Degree_Other", "Highest_Degree_Discipline_Other", "Position_Other", "Grading_Scale_Other_Description", "CCAPSFrequencyOther", "CLOSURE_02", "CLOSURE_05", "CIF_02".
#'
#' @export

remove_free_response <- function(data){

  # Exclude columns that are free responses
  data <- data |>
    dplyr::select(data, -tidyselect::any_of(c("SDS_16", "SDS_18",
                                              "SDS_21", "SDS_29",
                                              "SDS_30", "SDS_35",
                                              "SDS_38", "SDS_40",
                                              "SDS_43", "SDS_45",
                                              "SDS_47", "SDS_49",
                                              "SDS_54", "SDS_55",
                                              "SDS_89", "SDS_92",
                                              "SDS_1047", "SDS_101",
                                              "SDS_129", "Services_Other_Description",
                                              "Psychiatric_Services_Other_Description", "ChargeFor_Other_Description",
                                              "TherEthnicity_Other", "Highest_Degree_Other",
                                              "Highest_Degree_Discipline_Other", "Position_Other",
                                              "Grading_Scale_Other_Description", "CCAPSFrequencyOther",
                                              "CLOSURE_02", "CLOSURE_05",
                                              "CIF_02")))

  # Return
  return(data)

}
