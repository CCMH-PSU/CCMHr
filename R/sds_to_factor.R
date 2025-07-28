#' Convert SDS numeric labels into factors.
#'
#' @description When conducting analyses or creating visuals, the SDS items' factor labels are often used instead of the numeric labels to make the results more salient and comprehensible. This function converts the numeric labels of most SDS items into factors.
#'
#' @param data A data file containing SDS items.
#'
#' @return A data frame with all the original variables. Most SDS items will be converted to factors.
#'
#' @export

sds_to_factor <- function(data){

  # Recode
  suppressWarnings({

    if("SDS_01" %in% names(data)){

      data$SDS_01 <- as.factor(data$SDS_01) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "Prior to college",
                      `3` = "After starting college",
                      `4` = "Both")

    } else{

    }

    if("SDS_02" %in% names(data)){

      data$SDS_02 <- as.factor(data$SDS_02) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "Prior to college",
                      `3` = "After starting college",
                      `4` = "Both")

    } else{

    }

    if("SDS_64" %in% names(data)){

      data$SDS_64 <- as.factor(data$SDS_64) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "1 time",
                      `3` = "2-3 times",
                      `4` = "4-5 times",
                      `5` = "More than 5 times")

    } else{

    }

    if("SDS_65" %in% names(data)){

      data$SDS_65 <- as.factor(data$SDS_65) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "Within the last 2 weeks",
                      `3` = "Within the last month",
                      `4` = "Within the last year",
                      `5` = "Within the last 1-5 years",
                      `6` = "More than 5 years ago")

    } else{

    }

    if("SDS_66" %in% names(data)){

      data$SDS_66 <- as.factor(data$SDS_66) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "1 time",
                      `3` = "2-3 times",
                      `4` = "4-5 times",
                      `5` = "More than 5 times")

    } else{

    }

    if("SDS_67" %in% names(data)){

      data$SDS_67 <- as.factor(data$SDS_67) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "Within the last 2 weeks",
                      `3` = "Within the last month",
                      `4` = "Within the last year",
                      `5` = "Within the last 1-5 years",
                      `6` = "More than 5 years ago")

    } else{

    }

    if("SDS_68" %in% names(data)){

      data$SDS_68 <- as.factor(data$SDS_68) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "1 time",
                      `3` = "2-3 times",
                      `4` = "4-5 times",
                      `5` = "More than 5 times")

    } else{

    }

    if("SDS_69" %in% names(data)){

      data$SDS_69 <- as.factor(data$SDS_69) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "Within the last 2 weeks",
                      `3` = "Within the last month",
                      `4` = "Within the last year",
                      `5` = "Within the last 1-5 years",
                      `6` = "More than 5 years ago")

    } else{

    }

    if("SDS_70" %in% names(data)){

      data$SDS_70 <- as.factor(data$SDS_70) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "1 time",
                      `3` = "2-3 times",
                      `4` = "4-5 times",
                      `5` = "More than 5 times")

    } else{

    }

    if("SDS_71" %in% names(data)){

      data$SDS_71 <- as.factor(data$SDS_71) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "Within the last 2 weeks",
                      `3` = "Within the last month",
                      `4` = "Within the last year",
                      `5` = "Within the last 1-5 years",
                      `6` = "More than 5 years ago")

    } else{

    }

    if("SDS_72" %in% names(data)){

      data$SDS_72 <- as.factor(data$SDS_72) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "1 time",
                      `3` = "2-3 times",
                      `4` = "4-5 times",
                      `5` = "More than 5 times")

    } else{

    }

    if("SDS_73" %in% names(data)){

      data$SDS_73 <- as.factor(data$SDS_73) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "Within the last 2 weeks",
                      `3` = "Within the last month",
                      `4` = "Within the last year",
                      `5` = "Within the last 1-5 years",
                      `6` = "More than 5 years ago")

    } else{

    }

    if("SDS_74" %in% names(data)){

      data$SDS_74 <- as.factor(data$SDS_74) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "1 time",
                      `3` = "2-3 times",
                      `4` = "4-5 times",
                      `5` = "More than 5 times")

    } else{

    }

    if("SDS_75" %in% names(data)){

      data$SDS_75 <- as.factor(data$SDS_75) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "Within the last 2 weeks",
                      `3` = "Within the last month",
                      `4` = "Within the last year",
                      `5` = "Within the last 1-5 years",
                      `6` = "More than 5 years ago")

    } else{

    }

    if("SDS_76" %in% names(data)){

      data$SDS_76 <- as.factor(data$SDS_76) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "1 time",
                      `3` = "2-3 times",
                      `4` = "4-5 times",
                      `5` = "More than 5 times")

    } else{

    }

    if("SDS_77" %in% names(data)){

      data$SDS_77 <- as.factor(data$SDS_77) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "Within the last 2 weeks",
                      `3` = "Within the last month",
                      `4` = "Within the last year",
                      `5` = "Within the last 1-5 years",
                      `6` = "More than 5 years ago")

    } else{

    }

    if("SDS_78" %in% names(data)){

      data$SDS_78 <- as.factor(data$SDS_78) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "1 time",
                      `3` = "2-3 times",
                      `4` = "4-5 times",
                      `5` = "More than 5 times")

    } else{

    }

    if("SDS_79" %in% names(data)){

      data$SDS_79 <- as.factor(data$SDS_79) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "Within the last 2 weeks",
                      `3` = "Within the last month",
                      `4` = "Within the last year",
                      `5` = "Within the last 1-5 years",
                      `6` = "More than 5 years ago")

    } else{

    }

    if("SDS_80" %in% names(data)){

      data$SDS_80 <- as.factor(data$SDS_80) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "1 time",
                      `3` = "2-3 times",
                      `4` = "4-5 times",
                      `5` = "More than 5 times")

    } else{

    }

    if("SDS_81" %in% names(data)){

      data$SDS_81 <- as.factor(data$SDS_81) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "Within the last 2 weeks",
                      `3` = "Within the last month",
                      `4` = "Within the last year",
                      `5` = "Within the last 1-5 years",
                      `6` = "More than 5 years ago")

    } else{

    }

    if("SDS_82" %in% names(data)){

      data$SDS_82 <- as.factor(data$SDS_82) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "1 time",
                      `3` = "2-3 times",
                      `4` = "4-5 times",
                      `5` = "More than 5 times")

    } else{

    }

    if("SDS_83" %in% names(data)){

      data$SDS_83 <- as.factor(data$SDS_83) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "Within the last 2 weeks",
                      `3` = "Within the last month",
                      `4` = "Within the last year",
                      `5` = "Within the last 1-5 years",
                      `6` = "More than 5 years ago")

    } else{

    }

    if("SDS_84" %in% names(data)){

      data$SDS_84 <- as.factor(data$SDS_84) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "1 time",
                      `3` = "2-3 times",
                      `4` = "4-5 times",
                      `5` = "More than 5 times")

    } else{

    }

    if("SDS_85" %in% names(data)){

      data$SDS_85 <- as.factor(data$SDS_85) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "Within the last 2 weeks",
                      `3` = "Within the last month",
                      `4` = "Within the last year",
                      `5` = "Within the last 1-5 years",
                      `6` = "More than 5 years ago")

    } else{

    }

    if("SDS_86" %in% names(data)){

      data$SDS_86 <- as.factor(data$SDS_86) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "1 time",
                      `3` = "2-3 times",
                      `4` = "4-5 times",
                      `5` = "More than 5 times")

    } else{

    }

    if("SDS_87" %in% names(data)){

      data$SDS_87 <- as.factor(data$SDS_87) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Never",
                      `2` = "Within the last 2 weeks",
                      `3` = "Within the last month",
                      `4` = "Within the last year",
                      `5` = "Within the last 1-5 years",
                      `6` = "More than 5 years ago")

    } else{

    }

    if("SDS_19" %in% names(data)){

      data$SDS_19 <- as.factor(data$SDS_19) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "None",
                      `2` = "Once",
                      `3` = "Twice",
                      `4` = "3 to 5 times",
                      `5` = "6 to 9 times",
                      `6` = "10 or more times")

    } else{

    }

    if("SDS_1096" %in% names(data)){

      data$SDS_1096 <- as.factor(data$SDS_1096) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "None",
                      `2` = "Once",
                      `3` = "Twice",
                      `4` = "3 to 5 times",
                      `5` = "6 to 9 times",
                      `6` = "10 or more times")

    } else{

    }

    if("SDS_60" %in% names(data)){

      data$SDS_60 <- as.factor(data$SDS_60) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Yes",
                      `0` = "No")

    } else{

    }

    if("SDS_22" %in% names(data)){

      data$SDS_22 <- as.factor(data$SDS_22) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Strongly disagree",
                      `2` = "Somewhat disagree",
                      `3` = "Neutral",
                      `4` = "Somewhat agree",
                      `5` = "Strongly agree")

    } else{

    }

    if("SDS_23" %in% names(data)){

      data$SDS_23 <- as.factor(data$SDS_23) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Strongly disagree",
                      `2` = "Somewhat disagree",
                      `3` = "Neutral",
                      `4` = "Somewhat agree",
                      `5` = "Strongly agree")

    } else{

    }

    if("SDS_88" %in% names(data)){

      data$SDS_88 <- as.factor(data$SDS_88) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Woman",
                      `2` = "Man",
                      `3` = "Transgender",
                      `4` = "Self-identify",
                      `5` = "Transgender woman",
                      `6` = "Transgender man",
                      `7` = "Non-binary") |>
        forcats::fct_relevel("Woman", "Transgender woman",
                             "Man", "Transgender man",
                             "Non-binary")

    } else{

    }

    if("SDS_90" %in% names(data)){

      data$SDS_90 <- as.factor(data$SDS_90) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Female",
                      `2` = "Male",
                      `3` = "Intersex")

    } else{

    }

    if("SDS_91" %in% names(data)){

      data$SDS_91 <- as.factor(data$SDS_91) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Heterosexual/Straight",
                      `1001` = "Heterosexual/Straight",
                      `2` = "Lesbian",
                      `3` = "Gay",
                      `4` = "Bisexual",
                      `5` = "Questioning",
                      `6` = "Self-identify",
                      `7` = "Asexual",
                      `8` = "Pansexual",
                      `9` = "Queer") |>
        forcats::fct_relevel("Asexual", "Bisexual",
                             "Gay", "Heterosexual/Straight",
                             "Lesbian", "Pansexual",
                             "Queer", "Questioning")

    } else{

    }

    if("SDS_93" %in% names(data)){

      data$SDS_93 <- as.factor(data$SDS_93) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Only with men",
                      `2` = "Mostly with men",
                      `3` = "About the same number of men and women",
                      `4` = "Mostly with women",
                      `5` = "Only with women",
                      `6` = "I have not had sexual experiences")

    } else{

    }

    if("SDS_94" %in% names(data)){

      data$SDS_94 <- as.factor(data$SDS_94) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Only attracted to women",
                      `2` = "Mostly attracted to women",
                      `3` = "Equally attracted to women and men",
                      `4` = "Mostly attracted to men",
                      `5` = "Only attracted to men",
                      `6` = "Not sure",
                      `7` = "I do not experience sexual attraction")

    } else{

    }

    if("SDS_95" %in% names(data)){

      data$SDS_95 <- as.factor(data$SDS_95) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "African American/Black",
                      `2` = "American Indian or Alaskan Native",
                      `3` = "Asian American/Asian",
                      `4` = "Hispanic/Latino/a",
                      `5` = "Native Hawaiian or Pacific Islander",
                      `6` = "Multi-racial",
                      `7` = "White",
                      `8` = "Self-identify")

    } else{

    }

    if("SDS_32" %in% names(data)){

      data$SDS_32 <- as.factor(data$SDS_32) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_33" %in% names(data)){

      data$SDS_33 <- as.factor(data$SDS_33) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Single",
                      `2` = "Serious dating or committed relationship",
                      `3` = "Civil union, domestic partnership, or equivalent",
                      `4` = "Married",
                      `5` = "Divorced",
                      `6` = "Separated",
                      `7` = "Widowed")

    } else{

    }

    if("SDS_97" %in% names(data)){

      data$SDS_97 <- as.factor(data$SDS_97) |>
        forcats::fct_inseq() |>
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

    } else{

    }

    if("SDS_36" %in% names(data)){

      data$SDS_36 <- as.factor(data$SDS_36) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Very important",
                      `2` = "Important",
                      `3` = "Neutral",
                      `4` = "Unimportant",
                      `5` = "Very unimportant")

    } else{

    }

    if("SDS_37" %in% names(data)){

      data$SDS_37 <- as.factor(data$SDS_37) |>
        forcats::fct_inseq() |>
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

    } else{

    }

    if("SDS_39" %in% names(data)){

      data$SDS_39 <- as.factor(data$SDS_39) |>
        forcats::fct_inseq() |>
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

    } else{

    }

    if("SDS_41" %in% names(data)){

      data$SDS_41 <- as.factor(data$SDS_41) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "1",
                      `2` = "2",
                      `3` = "3",
                      `4` = "4",
                      `5` = "5+")

    } else{

    }

    if("SDS_42" %in% names(data)){

      data$SDS_42 <- as.factor(data$SDS_42) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "On-campus residence hall/apartment",
                      `2` = "On/off campus fraternity/sorority house",
                      `3` = "On/off campus co-operative house",
                      `4` = "Off-campus apartment/house",
                      `5` = "Other (please specify)")

    } else{

    }

    if("SDS_46" %in% names(data)){

      data$SDS_46 <- as.factor(data$SDS_46) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_48" %in% names(data)){

      data$SDS_48 <- as.factor(data$SDS_48) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "None",
                      `2` = "Occasional participation",
                      `3` = "One regularly attended activity",
                      `4` = "Two regularly attended activities",
                      `5` = "Three or more regularly attended activities")

    } else{

    }

    if("SDS_1049" %in% names(data)){

      data$SDS_1049 <- as.factor(data$SDS_1049) |>
        forcats::fct_inseq() |>
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

    } else{

    }

    if("SDS_1151" %in% names(data)){

      data$SDS_1151 <- as.factor(data$SDS_1151) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_1152" %in% names(data)){

      data$SDS_1152 <- as.factor(data$SDS_1152) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_1153" %in% names(data)){

      data$SDS_1153 <- as.factor(data$SDS_1153) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_51" %in% names(data)){

      data$SDS_51 <- as.factor(data$SDS_51) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_98" %in% names(data)){

      data$SDS_98 <- as.factor(data$SDS_98) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_53" %in% names(data)){

      data$SDS_53 <- as.factor(data$SDS_53) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_1055" %in% names(data)){

      data$SDS_1055 <- as.factor(data$SDS_1055) |>
        forcats::fct_inseq() |>
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

    } else{

    }

    if("SDS_56" %in% names(data)){

      data$SDS_56 <- as.factor(data$SDS_56) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_57" %in% names(data)){

      data$SDS_57 <- as.factor(data$SDS_57) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Always stressful",
                      `2` = "Often stressful",
                      `3` = "Sometimes stressful",
                      `4` = "Rarely stressful",
                      `5` = "Never stressful")

    } else{

    }

    if("SDS_58" %in% names(data)){

      data$SDS_58 <- as.factor(data$SDS_58) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Always stressful",
                      `2` = "Often stressful",
                      `3` = "Sometimes stressful",
                      `4` = "Rarely stressful",
                      `5` = "Never stressful")

    } else{

    }

    if("SDS_103" %in% names(data)){

      data$SDS_103 <- as.factor(data$SDS_103) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "1 time",
                      `2` = "2-3 times",
                      `3` = "4-5 times",
                      `4` = "More than 5 times",
                      `5` = "I don’t think I’ve had COVID-19")

    } else{

    }

    if("SDS_117" %in% names(data)){

      data$SDS_117 <- as.factor(data$SDS_117) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_1037" %in% names(data)){

      data$SDS_1037 <- as.factor(data$SDS_1037) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "1st year undergraduate",
                      `2` = "2nd year undergraduate",
                      `3` = "3rd year undergraduate",
                      `4` = "4th year undergraduate",
                      `5` = "5th year or more undergraduate",
                      `6` = "Graduate student",
                      `7` = "Professional degree student",
                      `8` = "Non-student",
                      `9` = "High-school student taking college classes",
                      `10` = "Non-degree student",
                      `11` = "Faculty or staff",
                      `12` = "Other (please specify)")

    } else{

    }

    if("SDS_111" %in% names(data)){

      data$SDS_111 <- as.factor(data$SDS_111) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_112" %in% names(data)){

      data$SDS_112 <- as.factor(data$SDS_112) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_113" %in% names(data)){

      data$SDS_113 <- as.factor(data$SDS_113) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_114" %in% names(data)){

      data$SDS_114 <- as.factor(data$SDS_114) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_115" %in% names(data)){

      data$SDS_115 <- as.factor(data$SDS_115) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_116" %in% names(data)){

      data$SDS_116 <- as.factor(data$SDS_116) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }


    if("SDS_119" %in% names(data)){

      data$SDS_119 <- as.factor(data$SDS_119) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_120" %in% names(data)){

      data$SDS_120 <- as.factor(data$SDS_120) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_121" %in% names(data)){

      data$SDS_121 <- as.factor(data$SDS_121) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_122" %in% names(data)){

      data$SDS_122 <- as.factor(data$SDS_122) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_123" %in% names(data)){

      data$SDS_123 <- as.factor(data$SDS_123) |>
        forcats::fct_inseq() |>
        dplyr::recode(`0` = "No",
                      `1` = "Yes")

    } else{

    }

    if("SDS_1042" %in% names(data)){

      data$SDS_1042 <- as.factor(data$SDS_1042) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "On-campus",
                      `2` = "Off-campus",
                      `3` = "I do not live in one stable, secure residence",
                      `4` = "Other (please specify)")

    } else{

    }

    if("SDS_1019" %in% names(data)){

      data$SDS_1019 <- as.factor(data$SDS_1019) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "None",
                      `2` = "Once",
                      `3` = "Twice",
                      `4` = "3 to 5 times",
                      `5` = "6 to 9 times",
                      `6` = "10 or more times")

    } else{

    }

    if("SDS_132" %in% names(data)){

      data$SDS_132 <- as.factor(data$SDS_132) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Yes",
                      `0` = "No",
                      `2` = "I did not use alcohol")

    } else{

    }

    if("SDS_2096" %in% names(data)){

      data$SDS_2096 <- as.factor(data$SDS_2096) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "None",
                      `2` = "Once",
                      `3` = "Twice",
                      `4` = "3 to 5 times",
                      `5` = "6 to 9 times",
                      `6` = "10 or more times")

    } else{

    }

    if("SDS_133" %in% names(data)){

      data$SDS_133 <- as.factor(data$SDS_133) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Yes",
                      `0` = "No",
                      `2` = "I did not use alcohol")

    } else{

    }

    if("SDS_134" %in% names(data)){

      data$SDS_134 <- as.factor(data$SDS_134) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Yes",
                      `0` = "No")

    } else{

    }

    if("SDS_135" %in% names(data)){

      data$SDS_135 <- as.factor(data$SDS_135) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Yes",
                      `0` = "No",
                      `2` = "I did not use any recreational drugs or non-prescribed medications")

    } else{

    }

    if("SDS_128" %in% names(data)){

      data$SDS_128 <- as.factor(data$SDS_128) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Yes",
                      `0` = "No")

    } else{

    }

    if("SDS_130" %in% names(data)){

      data$SDS_130 <- as.factor(data$SDS_130) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Yes",
                      `0` = "No")

    } else{

    }

    if("SDS_136" %in% names(data)){

      data$SDS_136 <- as.factor(data$SDS_136) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Yes",
                      `0` = "No",
                      `2` = "I did not gamble")

    } else{

    }

    if("SDS_1060" %in% names(data)){

      data$SDS_1060 <- as.factor(data$SDS_1060) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Yes",
                      `0` = "No")

    } else{

    }

    if("SDS_127" %in% names(data)){

      data$SDS_127 <- as.factor(data$SDS_127) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Yes",
                      `0` = "No")

    } else{

    }

    if("SDS_124" %in% names(data)){

      data$SDS_124 <- as.factor(data$SDS_124) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Hardly ever",
                      `2` = "Some of the time",
                      `3` = "Often")

    } else{

    }

    if("SDS_125" %in% names(data)){

      data$SDS_125 <- as.factor(data$SDS_125) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Hardly ever",
                      `2` = "Some of the time",
                      `3` = "Often")

    } else{

    }

    if("SDS_126" %in% names(data)){

      data$SDS_126 <- as.factor(data$SDS_126) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Hardly ever",
                      `2` = "Some of the time",
                      `3` = "Often")

    } else{

    }

    if("SDS_131" %in% names(data)){

      data$SDS_131 <- as.factor(data$SDS_131) |>
        forcats::fct_inseq() |>
        dplyr::recode(`1` = "Not at all",
                      `2` = "Slightly",
                      `3` = "Moderately",
                      `4` = "Very",
                      `5` = "Extremely")

    } else{

    }

  # Return data
  return(data)

  })

}
