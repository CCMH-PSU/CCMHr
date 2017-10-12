#' A Check for Variable Names
#'
#' @description
#' This function is used to check the consistency of variable names for the new yearly data. Checks will be run against
#' the expected variable names (e.g. based on the previous year's variable names), and any inconsistencies
#' will be returned.
#'
#' @param old_names A vector of strings indicating the old (or expected) variable names in the data set.
#' @param dat The data frame containing the new year of (combined) data, with the variable names stored in \code{colnames(dat)}.
#' @return A list containing \code{Is_same}, \code{Omitted}, and \code{Extra}. \code{Is_same} is boolean-valued,
#' indicating whether or not the new variable names were consistent with the expected variable names. \code{Omitted}
#' is a vector of strings corresponding to the variables from \code{old_names} not appearing in \code{dat}.
#' \code{Extra} is a vector of strings corresponding to the variables in \code{dat} not appearing in \code{old_names}.
#' @examples
#' old_names <- letters[1:3]
#' dat <- as.data.frame(matrix(rnorm(12),3,4))
#' colnames(dat) <- c("a","B","c","x")
#' varnames(old_names,dat)




varnames <- function(old_names=NULL,dat){
  # if(is.null(old_names)){
    # old_names <-   c("UniqueClientID","CenterWideConsent","CenterID","CcmhMemberID",
    #                  "AppointID","Date","RecordType","ClientID","ClientAttendance",
    #                  "UserID","UserAttendance","State","ZipCode","Enrollment",
    #                  "Public_Private","Grading_Scale","Grading_Scale_Other_Description",
    #                  "Athletic_Division","EnrollmentCount","APA_Accredited_Training",
    #                  "IACS_Accredited","Services_Career","Services_Disability",
    #                  "Services_Drug_Alcohol","Services_EAP","Services_Learning","Services_Health",
    #                  "Services_Testing","Services_Other","Services_Other_Description",
    #                  "Psychiatric_Services","Psychiatric_Services_Other_Description","Session_Limit",
    #                  "Session_Limit_Amount","ChargeFor_Intake","ChargeFor_Individual","ChargeFor_Group",
    #                  "ChargeFor_PsychiatricEvaluation","ChargeFor_PsychiatricFollowup",
    #                  "ChargeFor_AssessmentPsychological","ChargeFor_AssessmentCareer",
    #                  "ChargeFor_AssessmentDisability","ChargeFor_Other","ChargeFor_Other_Description",
    #                  "CCAPSFrequency","CCAPSFrequencyX","CCAPSFrequencyOther","UserID.1","TherapistAge",
    #                  "Gender","SexualOrientation","Ethnicity","Ethnicity_Other","Ethnicity_More",
    #                  "CountryOfOrigin"                        "RelationshipStatus"                     "Religion"
    # [58] "Religion_Other"                         "Religion_Importance"                    "Highest_Degree"
    # [61] "Highest_Degree_Other"                   "Highest_Degree_Discipline"              "Highest_Degree_Discipline_Other"
    # [64] "Highest_Degree_YearReceived"            "Highest_Degree_Licensed"                "Year_Licensed"
    # [67] "Position"                               "Position_Other"                         "Theory_Analytic"
    # [70] "Theory_Behavioral"                      "Theory_Cognitive"                       "Theory_Humanistic"
    # [73] "Theory_Systems"                         "Year"                                   "Month"
    # [76] "Day"                                    "FirstAppt"                              "FirstApptIndiv"
    # [79] "FirstApptGroup"                         "LastAppt"                               "LastApptIndiv"
    # [82] "LastApptGroup"                          "FirstAttendedAppt"                      "FirstAttendedIndiv"
    # [85] "FirstAttendedGroup"                     "LastAttendedAppt"                       "LastAttendedIndiv"
    # [88] "LastAttendedGroup"                      "Seq"                                    "SeqIndiv"
    # [91] "SeqGroup"                               "SeqAttended"                            "SeqAttendedIndiv"
    # [94] "SeqAttendedGroup"                       "OnMultipleSchedules"                    "MultiSeq"
    # [97] "MultiSeqIndiv"                          "MultiSeqGroup"                          "MultiSeqAttended"
    # [100] "MultiSeqAttendedIndiv"                  "MultiSeqAttendedGroup"                  "FirstAttendedDate"
    # [103] "DaysInTreatment"                        "TreatmentCourse"                        "AppointmentCategory"
    # [106] "UniquePreviousTherapist"                "CcapsVer2012StartDate"                  "CcapsVer2015StartDate"
    # [109] "CaseNoteID"                             "ClientAge"                              "AUDIT_01"
    # [112] "AUDIT_02"                               "AUDIT_03"                               "AUDIT_04"
    # [115] "AUDIT_05"                               "AUDIT_06"                               "AUDIT_07"
    # [118] "AUDIT_08"                               "AUDIT_09"                               "AUDIT_10"
    # [121] "AUDIT_100"                              "CCAPS_01"                               "CCAPS_03"
    # [124] "CCAPS_04"                               "CCAPS_05"                               "CCAPS_06"
    # [127] "CCAPS_08"                               "CCAPS_09"                               "CCAPS_10"
    # [130] "CCAPS_11"                               "CCAPS_13"                               "CCAPS_14"
    # [133] "CCAPS_15"                               "CCAPS_16"                               "CCAPS_17"
    # [136] "CCAPS_18"                               "CCAPS_19"                               "CCAPS_21"
    # [139] "CCAPS_22"                               "CCAPS_23"                               "CCAPS_24"
    # [142] "CCAPS_25"                               "CCAPS_26"                               "CCAPS_27"
    # [145] "CCAPS_28"                               "CCAPS_29"                               "CCAPS_30"
    # [148] "CCAPS_31"                               "CCAPS_32"                               "CCAPS_33"
    # [151] "CCAPS_34"                               "CCAPS_35"                               "CCAPS_36"
    # [154] "CCAPS_37"                               "CCAPS_38"                               "CCAPS_39"
    # [157] "CCAPS_40"                               "CCAPS_41"                               "CCAPS_43"
    # [160] "CCAPS_44"                               "CCAPS_45"                               "CCAPS_46"
    # [163] "CCAPS_47"                               "CCAPS_48"                               "CCAPS_49"
    # [166] "CCAPS_50"                               "CCAPS_51"                               "CCAPS_52"
    # [169] "CCAPS_53"                               "CCAPS_54"                               "CCAPS_56"
    # [172] "CCAPS_57"                               "CCAPS_58"                               "CCAPS_59"
    # [175] "CCAPS_60"                               "CCAPS_61"                               "CCAPS_63"
    # [178] "CCAPS_64"                               "CCAPS_65"                               "CCAPS_66"
    # [181] "CCAPS_68"                               "CCAPS_69"                               "CCAPS_70"
    # [184] "SDS_01"                                 "SDS_02"                                 "SDS_19"
    # [187] "SDS_22"                                 "SDS_23"                                 "SDS_31"
    # [190] "SDS_32"                                 "SDS_33"                                 "SDS_36"
    # [193] "SDS_37"                                 "SDS_39"                                 "SDS_41"
    # [196] "SDS_42"                                 "SDS_44_01"                              "SDS_44_02"
    # [199] "SDS_44_03"                              "SDS_44_04"                              "SDS_44_05"
    # [202] "SDS_44_06"                              "SDS_44_07"                              "SDS_46"
    # [205] "SDS_47"                                 "SDS_48"                                 "SDS_1049"
    # [208] "SDS_50"                                 "SDS_51"                                 "SDS_53"
    # [211] "SDS_1055"                               "SDS_56"                                 "SDS_57"
    # [214] "SDS_58"                                 "SDS_60"                                 "SDS_1061_01"
    # [217] "SDS_1061_02"                            "SDS_1061_03"                            "SDS_1061_04"
    # [220] "SDS_1061_05"                            "SDS_1061_06"                            "SDS_1061_07"
    # [223] "SDS_1061_08"                            "SDS_1061_09"                            "SDS_1061_10"
    # [226] "SDS_1061_11"                            "SDS_1061_12"                            "SDS_64"
    # [229] "SDS_65"                                 "SDS_66"                                 "SDS_67"
    # [232] "SDS_68"                                 "SDS_69"                                 "SDS_70"
    # [235] "SDS_71"                                 "SDS_72"                                 "SDS_73"
    # [238] "SDS_74"                                 "SDS_75"                                 "SDS_76"
    # [241] "SDS_77"                                 "SDS_78"                                 "SDS_79"
    # [244] "SDS_80"                                 "SDS_81"                                 "SDS_82"
    # [247] "SDS_83"                                 "SDS_84"                                 "SDS_85"
    # [250] "SDS_86"                                 "SDS_87"                                 "SDS_88"
    # [253] "SDS_89"                                 "SDS_90"                                 "SDS_91"
    # [256] "SDS_92"                                 "SDS_93"                                 "SDS_94"
    # [259] "SDS_95"                                 "SDS_1096"                               "SDS_97"
    # [262] "SDS_98"                                 "SDS_99_01"                              "SDS_99_02"
    # [265] "SDS_99_03"                              "SDS_99_04"                              "SDS_99_05"
    # [268] "SDS_99_06"                              "SDS_99_07"                              "SDS_99_08"
    # [271] "SDS_99_09"                              "SDS_99_10"                              "SDS_99_11"
    # [274] "SDS_99_12"                              "SDS_99_13"                              "SDS_99_14"
    # [277] "SDS_99_23"                              "CIF_01_01"                              "CIF_01_02"
    # [280] "CIF_01_03"                              "CIF_01_04"                              "CIF_01_05"
    # [283] "CIF_01_06"                              "CIF_01_07"                              "CIF_01_08"
    # [286] "CIF_01_09"                              "CIF_01_10"                              "CIF_01_11"
    # [289] "CIF_01_12"                              "CIF_01_13"                              "CIF_01_14"
    # [292] "CIF_01_15"                              "CIF_01_16"                              "CIF_01_17"
    # [295] "CLICC_01_01"                            "CLICC_01_02"                            "CLICC_01_03"
    # [298] "CLICC_01_04"                            "CLICC_01_05"                            "CLICC_01_06"
    # [301] "CLICC_01_07"                            "CLICC_01_08"                            "CLICC_01_09"
    # [304] "CLICC_01_10"                            "CLICC_01_11"                            "CLICC_01_12"
    # [307] "CLICC_01_13"                            "CLICC_01_14"                            "CLICC_01_15"
    # [310] "CLICC_01_16"                            "CLICC_01_17"                            "CLICC_01_18"
    # [313] "CLICC_01_19"                            "CLICC_01_20"                            "CLICC_01_21"
    # [316] "CLICC_01_22"                            "CLICC_01_23"                            "CLICC_01_24"
    # [319] "CLICC_01_25"                            "CLICC_01_26"                            "CLICC_01_27"
    # [322] "CLICC_01_28"                            "CLICC_01_29"                            "CLICC_01_30"
    # [325] "CLICC_01_31"                            "CLICC_01_32"                            "CLICC_01_33"
    # [328] "CLICC_01_34"                            "CLICC_01_35"                            "CLICC_01_36"
    # [331] "CLICC_01_37"                            "CLICC_01_38"                            "CLICC_01_39"
    # [334] "CLICC_01_40"                            "CLICC_01_41"                            "CLICC_01_42"
    # [337] "CLICC_01_43"                            "CLICC_01_44"                            "CLICC_03"
    # [340] "Term_01_01"                             "Term_01_02"                             "Term_01_1003"
    # [343] "Term_01_04"                             "Term_01_05"                             "Term_01_06"
    # [346] "Term_01_07"                             "Term_01_08"                             "Term_01_09"
    # [349] "Term_01_10"                             "Term_01_11"                             "Term_01_12"
    # [352] "Term_01_13"                             "Term_01_14"                             "Term_01_15"
    # [355] "Term_01_16"                             "Term_01_17"                             "Term_02"
    # [358] "Term_04"                                "Term_05"                                "Term_06"
    # [361] "Term_07"                                "Term_08"                                "Term_09"
    # [364] "Term_10"                                "Term_11"                                "Term_12"
    # [367] "Term_13"                                "Term_14_AppointID"                      "Dx_ClassificationSystem"
    # [370] "Dx_StaticID_1"                          "Dx_ICD10_1"                             "Dx_Certainty_1"
    # [373] "Dx_ViewOrder_1"                         "Dx_StaticID_2"                          "Dx_ICD10_2"
    # [376] "Dx_Certainty_2"                         "Dx_ViewOrder_2"                         "Dx_StaticID_3"
    # [379] "Dx_ICD10_3"                             "Dx_Certainty_3"                         "Dx_ViewOrder_3"
    # [382] "Dx_StaticID_4"                          "Dx_ICD10_4"                             "Dx_Certainty_4"
    # [385] "Dx_ViewOrder_4"                         "Dx_StaticID_5"                          "Dx_ICD10_5"
    # [388] "Dx_Certainty_5"                         "Dx_ViewOrder_5"                         "HasSurvey"
    # [391] "HasDx"                                  "HasAUDIT"                               "HasCCAPS"
    # [394] "HasCCAPS34"                             "HasCCAPS62"                             "HasSDS"
    # [397] "HasCIF"                                 "HasCLICC"                               "HasTerm"
    # [400] "FirstSurvey"                            "FirstDx"                                "FirstAUDIT"
    # [403] "FirstCCAPS"                             "FirstCCAPS34"                           "FirstCCAPS62"
    # [406] "FirstSDS"                               "FirstCIF"                               "FirstCLICC"
    # [409] "FirstTerm"                              "LastSurvey"                             "LastDx"
    # [412] "LastAUDIT"                              "LastCCAPS"                              "LastCCAPS34"
    # [415] "LastCCAPS62"                            "LastSDS"                                "LastCIF"
    # [418] "LastCLICC"                              "LastTerm"                               "SeqSurvey"
    # [421] "SeqDx"                                  "SeqAUDIT"                               "SeqCCAPS"
    # [424] "SeqCCAPS34"                             "SeqCCAPS62"                             "SeqSDS"
    # [427] "SeqCIF"                                 "SeqCLICC"                               "SeqTerm"
    # [430] "FirstDxDate"                            "FirstAUDITDate"                         "FirstCCAPSDate"
    # [433] "FirstCCAPS34Date"                       "FirstCCAPS62Date"                       "FirstSDSDate"
    # [436] "FirstCIFDate"                           "FirstCLICCDate"                         "FirstTermDate"
    # [439] "UniqueScheduledTherapist")
  # }
    if(is.null(old_names)){
      data("old_names")
      old_names <- colnames(old_names)
    }
  new_names <- colnames(dat)
  if(all(old_names %in% new_names) & all(new_names %in% old_names)){
    Is_same <- TRUE
    Omitted <- NULL
    Extra <- NULL
  } else{
    Is_same <- FALSE
    Omitted <- old_names[which((old_names %in% colnames(dat))==F)]
    Extra <- new_names[which((new_names %in% old_names)==F)]
  }
  out <- list("Is_same"= Is_same, "Omitted"= Omitted, "Extra"= Extra)
  return(out)
}
