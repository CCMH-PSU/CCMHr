#' Convert SDS numeric labels to answer option factors
#'
#' @param data A CCMH data file with SDS variables
#'
#' @return data with all present SDS variables converted to factors
#' @export
#'
#' @examples sds_to_factor(data.frame(SDS_01 = c(3, 2, 4, 1), SDS_02 = c(4, 3, 2, 1)))
#'
sds_to_factor <- function(data){
  suppressWarnings({
  if ("SDS_01" %in% names(data)) {
    data$SDS_01 <- as.factor(data$SDS_01) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "Prior to college",
                    `3` = "After starting college",
                    `4` = "Both")
  }

  if ("SDS_02" %in% names(data)) {
    data$SDS_02 <- as.factor(data$SDS_02) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "Prior to college",
                    `3` = "After starting college",
                    `4` = "Both")
  }

  if ("SDS_64" %in% names(data)) {
    data$SDS_64 <- as.factor(data$SDS_64) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "1 time",
                    `3` = "2-3 times",
                    `4` = "4-5 times",
                    `5` = "More than 5 times")
  }

  if ("SDS_65" %in% names(data)) {
    data$SDS_65 <- as.factor(data$SDS_65) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "Within the last 2 weeks",
                    `3` = "Within the last month",
                    `4` = "Within the last year",
                    `5` = "Within the last 1-5 years",
                    `6` = "More than 5 years ago")
  }

  if ("SDS_66" %in% names(data)) {
    data$SDS_66 <- as.factor(data$SDS_66) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "1 time",
                    `3` = "2-3 times",
                    `4` = "4-5 times",
                    `5` = "More than 5 times")
  }

  if ("SDS_67" %in% names(data)) {
    data$SDS_67 <- as.factor(data$SDS_67) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "Within the last 2 weeks",
                    `3` = "Within the last month",
                    `4` = "Within the last year",
                    `5` = "Within the last 1-5 years",
                    `6` = "More than 5 years ago")
  }

  if ("SDS_68" %in% names(data)) {
    data$SDS_68 <- as.factor(data$SDS_68) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "1 time",
                    `3` = "2-3 times",
                    `4` = "4-5 times",
                    `5` = "More than 5 times")
  }

  if ("SDS_69" %in% names(data)) {
    data$SDS_69 <- as.factor(data$SDS_69) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "Within the last 2 weeks",
                    `3` = "Within the last month",
                    `4` = "Within the last year",
                    `5` = "Within the last 1-5 years",
                    `6` = "More than 5 years ago")
  }

  if ("SDS_70" %in% names(data)) {
    data$SDS_70 <- as.factor(data$SDS_70) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "1 time",
                    `3` = "2-3 times",
                    `4` = "4-5 times",
                    `5` = "More than 5 times")
  }

  if ("SDS_71" %in% names(data)) {
    data$SDS_71 <- as.factor(data$SDS_71) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "Within the last 2 weeks",
                    `3` = "Within the last month",
                    `4` = "Within the last year",
                    `5` = "Within the last 1-5 years",
                    `6` = "More than 5 years ago")
  }

  if ("SDS_72" %in% names(data)) {
    data$SDS_72 <- as.factor(data$SDS_72) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "1 time",
                    `3` = "2-3 times",
                    `4` = "4-5 times",
                    `5` = "More than 5 times")
  }

  if ("SDS_73" %in% names(data)) {
    data$SDS_73 <- as.factor(data$SDS_73) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "Within the last 2 weeks",
                    `3` = "Within the last month",
                    `4` = "Within the last year",
                    `5` = "Within the last 1-5 years",
                    `6` = "More than 5 years ago")
  }

  if ("SDS_74" %in% names(data)) {
    data$SDS_74 <- as.factor(data$SDS_74) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "1 time",
                    `3` = "2-3 times",
                    `4` = "4-5 times",
                    `5` = "More than 5 times")
  }

  if ("SDS_75" %in% names(data)) {
    data$SDS_75 <- as.factor(data$SDS_75) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "Within the last 2 weeks",
                    `3` = "Within the last month",
                    `4` = "Within the last year",
                    `5` = "Within the last 1-5 years",
                    `6` = "More than 5 years ago")
  }

  if ("SDS_76" %in% names(data)) {
    data$SDS_76 <- as.factor(data$SDS_76) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "1 time",
                    `3` = "2-3 times",
                    `4` = "4-5 times",
                    `5` = "More than 5 times")
  }

  if ("SDS_77" %in% names(data)) {
    data$SDS_77 <- as.factor(data$SDS_77) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "Within the last 2 weeks",
                    `3` = "Within the last month",
                    `4` = "Within the last year",
                    `5` = "Within the last 1-5 years",
                    `6` = "More than 5 years ago")
  }

  if ("SDS_78" %in% names(data)) {
    data$SDS_78 <- as.factor(data$SDS_78) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "1 time",
                    `3` = "2-3 times",
                    `4` = "4-5 times",
                    `5` = "More than 5 times")
  }

  if ("SDS_79" %in% names(data)) {
    data$SDS_79 <- as.factor(data$SDS_79) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "Within the last 2 weeks",
                    `3` = "Within the last month",
                    `4` = "Within the last year",
                    `5` = "Within the last 1-5 years",
                    `6` = "More than 5 years ago")
  }

  if ("SDS_80" %in% names(data)) {
    data$SDS_80 <- as.factor(data$SDS_80) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "1 time",
                    `3` = "2-3 times",
                    `4` = "4-5 times",
                    `5` = "More than 5 times")
  }

  if ("SDS_81" %in% names(data)) {
    data$SDS_81 <- as.factor(data$SDS_81) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "Within the last 2 weeks",
                    `3` = "Within the last month",
                    `4` = "Within the last year",
                    `5` = "Within the last 1-5 years",
                    `6` = "More than 5 years ago")
  }

  if ("SDS_82" %in% names(data)) {
    data$SDS_82 <- as.factor(data$SDS_82) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "1 time",
                    `3` = "2-3 times",
                    `4` = "4-5 times",
                    `5` = "More than 5 times")
  }

  if ("SDS_83" %in% names(data)) {
    data$SDS_83 <- as.factor(data$SDS_83) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "Within the last 2 weeks",
                    `3` = "Within the last month",
                    `4` = "Within the last year",
                    `5` = "Within the last 1-5 years",
                    `6` = "More than 5 years ago")
  }

  if ("SDS_84" %in% names(data)) {
    data$SDS_84 <- as.factor(data$SDS_84) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "1 time",
                    `3` = "2-3 times",
                    `4` = "4-5 times",
                    `5` = "More than 5 times")
  }

  if ("SDS_85" %in% names(data)) {
    data$SDS_85 <- as.factor(data$SDS_85) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "Within the last 2 weeks",
                    `3` = "Within the last month",
                    `4` = "Within the last year",
                    `5` = "Within the last 1-5 years",
                    `6` = "More than 5 years ago")
  }

  if ("SDS_86" %in% names(data)) {
    data$SDS_86 <- as.factor(data$SDS_86) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "1 time",
                    `3` = "2-3 times",
                    `4` = "4-5 times",
                    `5` = "More than 5 times")
  }

  if ("SDS_87" %in% names(data)) {
    data$SDS_87 <- as.factor(data$SDS_87) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Never",
                    `2` = "Within the last 2 weeks",
                    `3` = "Within the last month",
                    `4` = "Within the last year",
                    `5` = "Within the last 1-5 years",
                    `6` = "More than 5 years ago")
  }

  if ("SDS_19" %in% names(data)) {
    data$SDS_19 <- as.factor(data$SDS_19) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "None",
                    `2` = "Once",
                    `3` = "Twice",
                    `4` = "3 to 5 times",
                    `5` = "6 to 9 times",
                    `6` = "10 or more times")
  }

  if ("SDS_1096" %in% names(data)) {
    data$SDS_1096 <- as.factor(data$SDS_1096) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "None",
                    `2` = "Once",
                    `3` = "Twice",
                    `4` = "3 to 5 times",
                    `5` = "6 to 9 times",
                    `6` = "10 or more times")
  }

  if ("SDS_60" %in% names(data)) {
    data$SDS_60 <- as.factor(data$SDS_60) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Yes",
                    `0` = "No")
  }

  if ("SDS_22" %in% names(data)) {
    data$SDS_22 <- as.factor(data$SDS_22) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Strongly disagree",
                    `2` = "Somewhat disagree",
                    `3` = "Neutral",
                    `4` = "Somewhat agree",
                    `5` = "Strongly agree")
  }

  if ("SDS_23" %in% names(data)) {
    data$SDS_23 <- as.factor(data$SDS_23) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Strongly disagree",
                    `2` = "Somewhat disagree",
                    `3` = "Neutral",
                    `4` = "Somewhat agree",
                    `5` = "Strongly agree")
  }

  if ("SDS_88" %in% names(data)) {
    data$SDS_88 <- as.factor(data$SDS_88) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Woman",
                    `2` = "Man",
                    `3` = "Transgender",
                    `4` = "Self-identify",
                    `5` = "Transgender woman",
                    `6` = "Transgender man",
                    `7` = "Non-binary") %>%
      forcats::fct_relevel("Woman", "Transgender woman", "Man", "Transgender man", "Non-binary")
  }

  if ("SDS_90" %in% names(data)) {
    data$SDS_90 <- as.factor(data$SDS_90) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Female",
                    `2` = "Male",
                    `3` = "Intersex")
  }

  if ("SDS_91" %in% names(data)) {
    data$SDS_91 <- as.factor(data$SDS_91) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Heterosexual/Straight",
                    `1001` = "Heterosexual/Straight",
                    `2` = "Lesbian",
                    `3` = "Gay",
                    `4` = "Bisexual",
                    `5` = "Questioning",
                    `6` = "Self-identify",
                    `7` = "Asexual",
                    `8` = "Pansexual",
                    `9` = "Queer") %>%
      forcats::fct_relevel("Asexual", "Bisexual", "Gay", "Heterosexual/Straight", "Lesbian", "Pansexual", "Queer", "Questioning")
  }

  if ("SDS_93" %in% names(data)) {
    data$SDS_93 <- as.factor(data$SDS_93) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Only with men",
                    `2` = "Mostly with men",
                    `3` = "About the same number of men and women",
                    `4` = "Mostly with women",
                    `5` = "Only with women",
                    `6` = "I have not had sexual experiences")
  }

  if ("SDS_94" %in% names(data)) {
    data$SDS_94 <- as.factor(data$SDS_94) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Only attracted to women",
                    `2` = "Mostly attracted to women",
                    `3` = "Equally attracted to women and men",
                    `4` = "Mostly attracted to men",
                    `5` = "Only attracted to men",
                    `6` = "Not sure",
                    `7` = "I do not experience sexual attraction")
  }

  if ("SDS_95" %in% names(data)) {
    data$SDS_95 <- as.factor(data$SDS_95) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "African American/Black",
                    `2` = "American Indian or Alaskan Native",
                    `3` = "Asian American/Asian",
                    `4` = "Hispanic/Latino/a",
                    `5` = "Native Hawaiian or Pacific Islander",
                    `6` = "Multi-racial",
                    `7` = "White",
                    `8` = "Self-identify")
  }

  if ("SDS_32" %in% names(data)) {
    data$SDS_32 <- as.factor(data$SDS_32) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`0` = "No",
                    `1` = "Yes")
  }

  if ("SDS_33" %in% names(data)) {
    data$SDS_33 <- as.factor(data$SDS_33) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Single",
                    `2` = "Serious dating or committed relationships",
                    `3` = "Civil union, domestic partnership, or equivalent",
                    `4` = "Married",
                    `5` = "Divorced",
                    `6` = "Separated",
                    `7` = "Widowed")
  }

  if ("SDS_97" %in% names(data)) {
    data$SDS_97 <- as.factor(data$SDS_97) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Agnostic",
                    `2` = "Atheist",
                    `3` = "Buddhist",
                    `4` = "Catholic",
                    `5` = "Christian",
                    `6` = "Hindu",
                    `7` = "Jewish",
                    `8` = "Muslim",
                    `9` = "No preference",
                    `10` = "Self-identify")
  }

  if ("SDS_36" %in% names(data)) {
    data$SDS_36 <- as.factor(data$SDS_36) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Very important",
                    `2` = "Important",
                    `3` = "Neutral",
                    `4` = "Unimportant",
                    `5` = "Very unimportant")
  }

  if ("SDS_37" %in% names(data)) {
    data$SDS_37 <- as.factor(data$SDS_37) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Freshman/First-year",
                    `2` = "Sophomore",
                    `3` = "Junior",
                    `4` = "Senior",
                    `5` = "Graduate/Professional degree student",
                    `6` = "Non-student",
                    `7` = "High-school student taking college classes",
                    `8` = "Non-degree student",
                    `9` = "Faculty or staff",
                    `10` = "Other (please specify)")
  }

  if ("SDS_39" %in% names(data)) {
    data$SDS_39 <- as.factor(data$SDS_39) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Post-Baccalaureate",
                    `2` = "Masters",
                    `3` = "Doctoral degree",
                    `4` = "Law",
                    `5` = "Medical",
                    `6` = "Pharmacy",
                    `7` = "Dental",
                    `8` = "Veterinary Medicine",
                    `9` = "Not applicable",
                    `10` = "Other (please specify)")
  }

  if ("SDS_41" %in% names(data)) {
    data$SDS_41 <- as.factor(data$SDS_41) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "1",
                    `2` = "2",
                    `3` = "3",
                    `4` = "4",
                    `5` = "5+")
  }

  if ("SDS_42" %in% names(data)) {
    data$SDS_42 <- as.factor(data$SDS_42) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "On-campus residence hall/apartment",
                    `2` = "On/off campus fraternity/sorority house",
                    `3` = "On/off campus co-operative house",
                    `4` = "Off-campus apartment/house",
                    `5` = "Other (please specify)")
  }

  if ("SDS_46" %in% names(data)) {
    data$SDS_46 <- as.factor(data$SDS_46) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`0` = "No",
                    `1` = "Yes")
  }

  if ("SDS_48" %in% names(data)) {
    data$SDS_48 <- as.factor(data$SDS_48) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "None",
                    `2` = "Occasional participation",
                    `3` = "One regularly attended activity",
                    `4` = "Two regularly attended activities",
                    `5` = "Three or more regularly attended activities")
  }

  if ("SDS_1049" %in% names(data)) {
    data$SDS_1049 <- as.factor(data$SDS_1049) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "0",
                    `2` = "1-5",
                    `3` = "6-10",
                    `4` = "11-15",
                    `5` = "16-20",
                    `6` = "21-25",
                    `7` = "26-30",
                    `8` = "31-35",
                    `9` = "36-40",
                    `10` = "40+")
  }

  if ("SDS_1151" %in% names(data)) {
    data$SDS_1151 <- as.factor(data$SDS_1151) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`0` = "No",
                    `1` = "Yes")
  }

  if ("SDS_1152" %in% names(data)) {
    data$SDS_1152 <- as.factor(data$SDS_1152) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`0` = "No",
                    `1` = "Yes")
  }

  if ("SDS_1153" %in% names(data)) {
    data$SDS_1153 <- as.factor(data$SDS_1153) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`0` = "No",
                    `1` = "Yes")
  }

  if ("SDS_51" %in% names(data)) {
    data$SDS_51 <- as.factor(data$SDS_51) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`0` = "No",
                    `1` = "Yes")
  }

  if ("SDS_98" %in% names(data)) {
    data$SDS_98 <- as.factor(data$SDS_98) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`0` = "No",
                    `1` = "Yes")
  }

  if ("SDS_53" %in% names(data)) {
    data$SDS_53<- as.factor(data$SDS_53) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`0` = "No",
                    `1` = "Yes")
  }

  if ("SDS_1055" %in% names(data)) {
    data$SDS_1055 <- as.factor(data$SDS_1055) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "0",
                    `2` = "1-5",
                    `3` = "6-10",
                    `4` = "11-15",
                    `5` = "16-20",
                    `6` = "21-25",
                    `7` = "26-30",
                    `8` = "31-35",
                    `9` = "36-40",
                    `10` = "40+")
  }

  if ("SDS_56" %in% names(data)) {
    data$SDS_56<- as.factor(data$SDS_56) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`0` = "No",
                    `1` = "Yes")
  }

  if ("SDS_57" %in% names(data)) {
    data$SDS_57 <- as.factor(data$SDS_57) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Always stressful",
                    `2` = "Often stressful",
                    `3` = "Sometimes stressful",
                    `4` = "Rarely stressful",
                    `5` = "Never stressful")
  }

  if ("SDS_58" %in% names(data)) {
    data$SDS_58 <- as.factor(data$SDS_58) %>%
      forcats::fct_inseq() %>%
      dplyr::recode(`1` = "Always stressful",
                    `2` = "Often stressful",
                    `3` = "Sometimes stressful",
                    `4` = "Rarely stressful",
                    `5` = "Never stressful")
  }

  return(data)
  })
}



