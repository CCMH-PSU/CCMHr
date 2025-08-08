#' Convert SDS factor labels into numeric.
#'
#' @name sds_to_numeric
#'
#' @description When conducting analyses, the SDS items' factor labels may need to be converted into a numeric label to conduct the analysis. This function converts the factor labels of most SDS items into numeric values.
#'
#' @param data A data file containing SDS items.
#'
#' @return A data frame with all the original variables. Most SDS items will be converted to numeric.
#'
#' @importFrom dplyr recode
#'
#' @export

sds_to_numeric <- function(data){

  # Recode
  suppressWarnings({

    if("SDS_01" %in% names(data) &
       !is.numeric(data$SDS_01) &
       !is.integer(data$SDS_01)){

      data$SDS_01 <- data$SDS_01 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "Prior to college" = 2,
                      "After starting college" = 3,
                      "Both" = 4)

    } else{

    }

    if("SDS_02" %in% names(data) &
       !is.numeric(data$SDS_02) &
       !is.integer(data$SDS_02)){

      data$SDS_02 <- data$SDS_02 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "Prior to college" = 2,
                      "After starting college" = 3,
                      "Both" = 4)

    } else{

    }

    if("SDS_64" %in% names(data) &
       !is.numeric(data$SDS_64) &
       !is.integer(data$SDS_64)){

      data$SDS_64 <- data$SDS_64 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "1 time" = 2,
                      "2-3 times" = 3,
                      "4-5 times" = 4,
                      "More than 5 times" = 5)

    } else{

    }

    if("SDS_65" %in% names(data) &
       !is.numeric(data$SDS_65) &
       !is.integer(data$SDS_65)){

      data$SDS_65 <- data$SDS_65 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "Within the last 2 weeks" = 2,
                      "Within the last month" = 3,
                      "Within the last year" = 4,
                      "Within the last 1-5 years" = 5,
                      "More than 5 years ago" = 6)

    } else{

    }

    if("SDS_66" %in% names(data) &
       !is.numeric(data$SDS_66) &
       !is.integer(data$SDS_66)){

      data$SDS_66 <- data$SDS_66 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "1 time" = 2,
                      "2-3 times" = 3,
                      "4-5 times" = 4,
                      "More than 5 times" = 5)

    } else{

    }

    if("SDS_67" %in% names(data) &
       !is.numeric(data$SDS_67) &
       !is.integer(data$SDS_67)){

      data$SDS_67 <- data$SDS_67 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "Within the last 2 weeks" = 2,
                      "Within the last month" = 3,
                      "Within the last year" = 4,
                      "Within the last 1-5 years" = 5,
                      "More than 5 years ago" = 6)

    } else{

    }

    if("SDS_68" %in% names(data) &
       !is.numeric(data$SDS_68) &
       !is.integer(data$SDS_68)){

      data$SDS_68 <- data$SDS_68 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "1 time" = 2,
                      "2-3 times" = 3,
                      "4-5 times" = 4,
                      "More than 5 times" = 5)

    } else{

    }

    if("SDS_69" %in% names(data) &
       !is.numeric(data$SDS_69) &
       !is.integer(data$SDS_69)){

      data$SDS_69 <- data$SDS_69 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "Within the last 2 weeks" = 2,
                      "Within the last month" = 3,
                      "Within the last year" = 4,
                      "Within the last 1-5 years" = 5,
                      "More than 5 years ago" = 6)

    } else{

    }

    if("SDS_70" %in% names(data) &
       !is.numeric(data$SDS_70) &
       !is.integer(data$SDS_70)){

      data$SDS_70 <- data$SDS_70 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "1 time" = 2,
                      "2-3 times" = 3,
                      "4-5 times" = 4,
                      "More than 5 times" = 5)

    } else{

    }

    if("SDS_71" %in% names(data) &
       !is.numeric(data$SDS_71) &
       !is.integer(data$SDS_71)){

      data$SDS_71 <- data$SDS_71 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "Within the last 2 weeks" = 2,
                      "Within the last month" = 3,
                      "Within the last year" = 4,
                      "Within the last 1-5 years" = 5,
                      "More than 5 years ago" = 6)

    } else{

    }

    if("SDS_72" %in% names(data) &
       !is.numeric(data$SDS_72) &
       !is.integer(data$SDS_72)){

      data$SDS_72 <- data$SDS_72 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "1 time" = 2,
                      "2-3 times" = 3,
                      "4-5 times" = 4,
                      "More than 5 times" = 5)

    } else{

    }

    if("SDS_73" %in% names(data) &
       !is.numeric(data$SDS_73) &
       !is.integer(data$SDS_73)){

      data$SDS_73 <- data$SDS_73 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "Within the last 2 weeks" = 2,
                      "Within the last month" = 3,
                      "Within the last year" = 4,
                      "Within the last 1-5 years" = 5,
                      "More than 5 years ago" = 6)

    } else{

    }

    if("SDS_74" %in% names(data) &
       !is.numeric(data$SDS_74) &
       !is.integer(data$SDS_74)){

      data$SDS_74 <- data$SDS_74 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "1 time" = 2,
                      "2-3 times" = 3,
                      "4-5 times" = 4,
                      "More than 5 times" = 5)

    } else{

    }

    if("SDS_75" %in% names(data) &
       !is.numeric(data$SDS_75) &
       !is.integer(data$SDS_75)){

      data$SDS_75 <- data$SDS_75 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "Within the last 2 weeks" = 2,
                      "Within the last month" = 3,
                      "Within the last year" = 4,
                      "Within the last 1-5 years" = 5,
                      "More than 5 years ago" = 6)

    } else{

    }

    if("SDS_76" %in% names(data) &
       !is.numeric(data$SDS_76) &
       !is.integer(data$SDS_76)){

      data$SDS_76 <- data$SDS_76 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "1 time" = 2,
                      "2-3 times" = 3,
                      "4-5 times" = 4,
                      "More than 5 times" = 5)

    } else{

    }

    if("SDS_77" %in% names(data) &
       !is.numeric(data$SDS_77) &
       !is.integer(data$SDS_77)){

      data$SDS_77 <- data$SDS_77 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "Within the last 2 weeks" = 2,
                      "Within the last month" = 3,
                      "Within the last year" = 4,
                      "Within the last 1-5 years" = 5,
                      "More than 5 years ago" = 6)

    } else{

    }

    if("SDS_78" %in% names(data) &
       !is.numeric(data$SDS_78) &
       !is.integer(data$SDS_78)){

      data$SDS_78 <- data$SDS_78 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "1 time" = 2,
                      "2-3 times" = 3,
                      "4-5 times" = 4,
                      "More than 5 times" = 5)

    } else{

    }

    if("SDS_79" %in% names(data) &
       !is.numeric(data$SDS_79) &
       !is.integer(data$SDS_79)){

      data$SDS_79 <- data$SDS_79 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "Within the last 2 weeks" = 2,
                      "Within the last month" = 3,
                      "Within the last year" = 4,
                      "Within the last 1-5 years" = 5,
                      "More than 5 years ago" = 6)

    } else{

    }

    if("SDS_80" %in% names(data) &
       !is.numeric(data$SDS_80) &
       !is.integer(data$SDS_80)){

      data$SDS_80 <- data$SDS_80 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "1 time" = 2,
                      "2-3 times" = 3,
                      "4-5 times" = 4,
                      "More than 5 times" = 5)

    } else{

    }

    if("SDS_81" %in% names(data) &
       !is.numeric(data$SDS_81) &
       !is.integer(data$SDS_81)){

      data$SDS_81 <- data$SDS_81 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "Within the last 2 weeks" = 2,
                      "Within the last month" = 3,
                      "Within the last year" = 4,
                      "Within the last 1-5 years" = 5,
                      "More than 5 years ago" = 6)

    } else{

    }

    if("SDS_82" %in% names(data) &
       !is.numeric(data$SDS_82) &
       !is.integer(data$SDS_82)){

      data$SDS_82 <- data$SDS_82 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "1 time" = 2,
                      "2-3 times" = 3,
                      "4-5 times" = 4,
                      "More than 5 times" = 5)

    } else{

    }

    if("SDS_83" %in% names(data) &
       !is.numeric(data$SDS_83) &
       !is.integer(data$SDS_83)){

      data$SDS_83 <- data$SDS_83 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "Within the last 2 weeks" = 2,
                      "Within the last month" = 3,
                      "Within the last year" = 4,
                      "Within the last 1-5 years" = 5,
                      "More than 5 years ago" = 6)

    } else{

    }

    if("SDS_84" %in% names(data) &
       !is.numeric(data$SDS_84) &
       !is.integer(data$SDS_84)){

      data$SDS_84 <- data$SDS_84 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "1 time" = 2,
                      "2-3 times" = 3,
                      "4-5 times" = 4,
                      "More than 5 times" = 5)

    } else{

    }

    if("SDS_85" %in% names(data) &
       !is.numeric(data$SDS_85) &
       !is.integer(data$SDS_85)){

      data$SDS_85 <- data$SDS_85 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "Within the last 2 weeks" = 2,
                      "Within the last month" = 3,
                      "Within the last year" = 4,
                      "Within the last 1-5 years" = 5,
                      "More than 5 years ago" = 6)

    } else{

    }

    if("SDS_86" %in% names(data) &
       !is.numeric(data$SDS_86) &
       !is.integer(data$SDS_86)){

      data$SDS_86 <- data$SDS_86 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "1 time" = 2,
                      "2-3 times" = 3,
                      "4-5 times" = 4,
                      "More than 5 times" = 5)

    } else{

    }

    if("SDS_87" %in% names(data) &
       !is.numeric(data$SDS_87) &
       !is.integer(data$SDS_87)){

      data$SDS_87 <- data$SDS_87 |>
        as.character() |>
        dplyr::recode("Never" = 1,
                      "Within the last 2 weeks" = 2,
                      "Within the last month" = 3,
                      "Within the last year" = 4,
                      "Within the last 1-5 years" = 5,
                      "More than 5 years ago" = 6)

    } else{

    }

    if("SDS_19" %in% names(data) &
       !is.numeric(data$SDS_19) &
       !is.integer(data$SDS_19)){

      data$SDS_19 <- data$SDS_19 |>
        as.character() |>
        dplyr::recode("None" = 1,
                      "Once" = 2,
                      "Twice" = 3,
                      "3 to 5 times" = 4,
                      "6 to 9 times" = 5,
                      "10 or more times" = 6)

    } else{

    }

    if("SDS_1096" %in% names(data) &
       !is.numeric(data$SDS_1096) &
       !is.integer(data$SDS_1096)){

      data$SDS_1096 <- data$SDS_1096 |>
        as.character() |>
        dplyr::recode("None" = 1,
                      "Once" = 2,
                      "Twice" = 3,
                      "3 to 5 times" = 4,
                      "6 to 9 times" = 5,
                      "10 or more times" = 6)

    } else{

    }

    if("SDS_60" %in% names(data) &
       !is.numeric(data$SDS_60) &
       !is.integer(data$SDS_60)){

      data$SDS_60 <- data$SDS_60 |>
        as.character() |>
        dplyr::recode("Yes" = 1,
                      "No" = 0)

    } else{

    }

    if("SDS_22" %in% names(data) &
       !is.numeric(data$SDS_22) &
       !is.integer(data$SDS_22)){

      data$SDS_22 <- data$SDS_22 |>
        as.character() |>
        dplyr::recode("Strongly disagree" = 1,
                      "Somewhat disagree" = 2,
                      "Neutral" = 3,
                      "Somewhat agree" = 4,
                      "Strongly agree" = 5)

    } else{

    }

    if("SDS_23" %in% names(data) &
       !is.numeric(data$SDS_23) &
       !is.integer(data$SDS_23)){

      data$SDS_23 <- data$SDS_23 |>
        as.character() |>
        dplyr::recode("Strongly disagree" = 1,
                      "Somewhat disagree" = 2,
                      "Neutral" = 3,
                      "Somewhat agree" = 4,
                      "Strongly agree" = 5)

    } else{

    }

    if("SDS_88" %in% names(data) &
       !is.numeric(data$SDS_88) &
       !is.integer(data$SDS_88)){

      data$SDS_88 <- data$SDS_88 |>
        as.character() |>
        dplyr::recode("Woman" = 1,
                      "Man" = 2,
                      "Transgender" = 3,
                      "Self-identify (please specify):" = 4,
                      "Transgender woman" = 5,
                      "Transgender man" = 6,
                      "Non-binary" = 7)

    } else{

    }

    if("SDS_90" %in% names(data) &
       !is.numeric(data$SDS_90) &
       !is.integer(data$SDS_90)){

      data$SDS_90 <- data$SDS_90 |>
        as.character() |>
        dplyr::recode("Female" = 1,
                      "Male" = 2,
                      "Intersex" = 3)

    } else{

    }

    if("SDS_91" %in% names(data) &
       !is.numeric(data$SDS_91) &
       !is.integer(data$SDS_91)){

      data$SDS_91 <- data$SDS_91 |>
        as.character() |>
        dplyr::recode("Heterosexual" = "1",
                      "Heterosexual or straight" = "1001",
                      "Lesbian" = "2",
                      "Gay" = "3",
                      "Bisexual" = "4",
                      "Questioning" = "5",
                      "Self-identify (please specify):" = "6",
                      "Asexual" = "7",
                      "Pansexual" = "8",
                      "Queer" = "9")

    } else{

    }

    if("SDS_93" %in% names(data) &
       !is.numeric(data$SDS_93) &
       !is.integer(data$SDS_93)){

      data$SDS_93 <- data$SDS_93 |>
        as.character() |>
        dplyr::recode("Only with men" = 1,
                      "Mostly with men" = 2,
                      "About the same number of men and women" = 3,
                      "Mostly with women" = 4,
                      "Only with women" = 5,
                      "I have not had sexual experiences" = 6)

    } else{

    }

    if("SDS_94" %in% names(data) &
       !is.numeric(data$SDS_94) &
       !is.integer(data$SDS_94)){

      data$SDS_94 <- data$SDS_94 |>
        as.character() |>
        dplyr::recode("Only attracted to women" = 1,
                      "Mostly attracted to women" = 2,
                      "Equally attracted to women and men" = 3,
                      "Mostly attracted to men" = 4,
                      "Only attracted to men" = 5,
                      "Not sure" = 6,
                      "I do not experience sexual attraction" = 7)

    } else{

    }

    if("SDS_95" %in% names(data) &
       !is.numeric(data$SDS_95) &
       !is.integer(data$SDS_95)){

      data$SDS_95 <- data$SDS_95 |>
        as.character() |>
        dplyr::recode("African American / Black" = 1,
                      "American Indian or Alaskan Native" = 2,
                      "Asian American / Asian" = 3,
                      "Hispanic / Latino/a" = 4,
                      "Native Hawaiian or Pacific Islander" = 5,
                      "Multi-racial" = 6,
                      "White" = 7,
                      "Self-identify (please specify):" = 8)

    } else{

    }

    if("SDS_32" %in% names(data) &
       !is.numeric(data$SDS_32) &
       !is.integer(data$SDS_32)){

      data$SDS_32 <- data$SDS_32 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_33" %in% names(data) &
       !is.numeric(data$SDS_33) &
       !is.integer(data$SDS_33)){

      data$SDS_33 <- data$SDS_33 |>
        as.character() |>
        dplyr::recode("Single" = 1,
                      "Serious dating or committed relationship" = 2,
                      "Civil union, domestic partnership, or equivalent" = 3,
                      "Married" = 4,
                      "Divorced" = 5,
                      "Separated" = 6,
                      "Widowed" = 7)

    } else{

    }

    if("SDS_97" %in% names(data) &
       !is.numeric(data$SDS_97) &
       !is.integer(data$SDS_97)){

      data$SDS_97 <- data$SDS_97 |>
        as.character() |>
        dplyr::recode("Agnostic" = 1,
                      "Atheist" = 2,
                      "Buddhist" = 3,
                      "Catholic" = 4,
                      "Christian" = 5,
                      "Hindu" = 6,
                      "Jewish" = 7,
                      "Muslim" = 8,
                      "No preference" = 9,
                      "Self-identify (please specify):" = 10)

    } else{

    }

    if("SDS_36" %in% names(data) &
       !is.numeric(data$SDS_36) &
       !is.integer(data$SDS_36)){

      data$SDS_36 <- data$SDS_36 |>
        as.character() |>
        dplyr::recode("Very important" = 1,
                      "Important" = 2,
                      "Neutral" = 3,
                      "Unimportant" = 4,
                      "Very unimportant" = 5)

    } else{

    }

    if("SDS_37" %in% names(data) &
       !is.numeric(data$SDS_37) &
       !is.integer(data$SDS_37)){

      data$SDS_37 <- data$SDS_37 |>
        as.character() |>
        dplyr::recode("Freshman/First-year" = 1,
                      "Sophomore" = 2,
                      "Junior" = 3,
                      "Senior" = 4,
                      "Graduate/Professional degree student" = 5,
                      "Non-student" = 6,
                      "High-school student taking college classes" = 7,
                      "Non-degree student" = 8,
                      "Faculty or staff" = 9,
                      "Other (please specify)" = 10)

    } else{

    }

    if("SDS_39" %in% names(data) &
       !is.numeric(data$SDS_39) &
       !is.integer(data$SDS_39)){

      data$SDS_39 <- data$SDS_39 |>
        as.character() |>
        dplyr::recode("Post-Baccalaureate" = 1,
                      "Masters" = 2,
                      "Doctoral degree" = 3,
                      "Law" = 4,
                      "Medical" = 5,
                      "Pharmacy" = 6,
                      "Dental" = 7,
                      "Veterinary Medicine" = 8,
                      "Not applicable" = 9,
                      "Other (please specify)" = 10)

    } else{

    }

    if("SDS_41" %in% names(data) &
       !is.numeric(data$SDS_41) &
       !is.integer(data$SDS_41)){

      data$SDS_41 <- data$SDS_41 |>
        as.character() |>
        dplyr::recode("1" = 1,
                      "2" = 2,
                      "3" = 3,
                      "4" = 4,
                      "5+" = 5)

    } else{

    }

    if("SDS_42" %in% names(data) &
       !is.numeric(data$SDS_42) &
       !is.integer(data$SDS_42)){

      data$SDS_42 <- data$SDS_42 |>
        as.character() |>
        dplyr::recode("On-campus residence hall/apartment" = 1,
                      "On/off campus fraternity/sorority house" = 2,
                      "On/off campus co-operative house" = 3,
                      "Off-campus apartment/house" = 4,
                      "Other (please specify)" = 5)

    } else{

    }

    if("SDS_1042" %in% names(data) &
       !is.numeric(data$SDS_1042) &
       !is.integer(data$SDS_1042)){

      data$SDS_1042 <- data$SDS_1042 |>
        as.character() |>
        dplyr::recode("On-campus" = 1,
                      "Off-campus" = 2,
                      "I do not live in one stable, secure residence" = 3,
                      "Other (please specify)" = 4)

    } else{

    }

    if("SDS_46" %in% names(data) &
       !is.numeric(data$SDS_46) &
       !is.integer(data$SDS_46)){

      data$SDS_46 <- data$SDS_46 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_48" %in% names(data) &
       !is.numeric(data$SDS_48) &
       !is.integer(data$SDS_48)){

      data$SDS_48 <- data$SDS_48 |>
        as.character() |>
        dplyr::recode("None" = 1,
                      "Occasional participation" = 2,
                      "One regularly attended activity" = 3,
                      "Two regularly attended activities" = 4,
                      "Three or more regularly attended activities" = 5)

    } else{

    }

    if("SDS_1049" %in% names(data) &
       !is.numeric(data$SDS_1049) &
       !is.integer(data$SDS_1049)){

      data$SDS_1049 <- data$SDS_1049 |>
        as.character() |>
        dplyr::recode("0" = 1,
                      "1-5" = 2,
                      "5-Jan" = 2,
                      "6-10" = 3,
                      "10-Jun" = 3,
                      "11-15" = 4,
                      "15-Nov" = 4,
                      "16-20" = 5,
                      "21-25" = 6,
                      "26-30" = 7,
                      "31-35" = 8,
                      "36-40" = 9,
                      "40+" = 10)

    } else{

    }

    if("SDS_1151" %in% names(data) &
       !is.numeric(data$SDS_1151) &
       !is.integer(data$SDS_1151)){

      data$SDS_1151 <- data$SDS_1151 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_1152" %in% names(data) &
       !is.numeric(data$SDS_1152) &
       !is.integer(data$SDS_1152)){

      data$SDS_1152 <- data$SDS_1152 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_1153" %in% names(data) &
       !is.numeric(data$SDS_1153) &
       !is.integer(data$SDS_1153)){

      data$SDS_1153 <- data$SDS_1153 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_51" %in% names(data) &
       !is.numeric(data$SDS_51) &
       !is.integer(data$SDS_51)){

      data$SDS_51 <- data$SDS_51 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_98" %in% names(data) &
       !is.numeric(data$SDS_98) &
       !is.integer(data$SDS_98)){

      data$SDS_98 <- data$SDS_98 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_53" %in% names(data) &
       !is.numeric(data$SDS_53) &
       !is.integer(data$SDS_53)){

      data$SDS_53<- data$SDS_53 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_1055" %in% names(data) &
       !is.numeric(data$SDS_1055) &
       !is.integer(data$SDS_1055)){

      data$SDS_1055 <- data$SDS_1055 |>
        as.character() |>
        dplyr::recode("0" = 1,
                      "1-5" = 2,
                      "5-Jan" = 2,
                      "6-10" = 3,
                      "10-Jun" = 3,
                      "11-15" = 4,
                      "15-Nov" = 4,
                      "16-20" = 5,
                      "21-25" = 6,
                      "26-30" = 7,
                      "31-35" = 8,
                      "36-40" = 9,
                      "40+" = 10)

    } else{

    }

    if("SDS_56" %in% names(data) &
       !is.numeric(data$SDS_56) &
       !is.integer(data$SDS_56)){

      data$SDS_56 <- data$SDS_56 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_57" %in% names(data) &
       !is.numeric(data$SDS_57) &
       !is.integer(data$SDS_57)){

      data$SDS_57 <- data$SDS_57 |>
        as.character() |>
        dplyr::recode("Always stressful" = 1,
                      "Often stressful" = 2,
                      "Sometimes stressful" = 3,
                      "Rarely stressful" = 4,
                      "Never stressful" = 5)

    } else{

    }

    if("SDS_58" %in% names(data) &
       !is.numeric(data$SDS_58) &
       !is.integer(data$SDS_58)){

      data$SDS_58 <- data$SDS_58 |>
        as.character() |>
        dplyr::recode("Always stressful" = 1,
                      "Often stressful" = 2,
                      "Sometimes stressful" = 3,
                      "Rarely stressful" = 4,
                      "Never stressful" = 5)

    } else{

    }

    if("SDS_103" %in% names(data) &
       !is.numeric(data$SDS_103) &
       !is.integer(data$SDS_103)){

      data$SDS_103 <- data$SDS_103 |>
        as.character() |>
        dplyr::recode("1 time" = 1,
                      "2-3 times" = 2,
                      "4-5 times" = 3,
                      "More than 5 times" = 4,
                      "I don’t think I’ve had COVID-19" = 5)

    } else{

    }

    if("SDS_117" %in% names(data) &
       !is.numeric(data$SDS_117) &
       !is.integer(data$SDS_117)){

      data$SDS_117 <- data$SDS_117 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_1037" %in% names(data) &
       !is.numeric(data$SDS_1037) &
       !is.integer(data$SDS_1037)){

      data$SDS_1037 <- data$SDS_1037 |>
        as.character() |>
        dplyr::recode("1st year undergraduate" = 1,
                      "2nd year undergraduate" = 2,
                      "3rd year undergraduate" = 3,
                      "4th year undergraduate" = 4,
                      "5th year or more undergraduate" = 5,
                      "Graduate student" = 6,
                      "Professional degree student" = 7,
                      "Non-student" = 8,
                      "High-school student taking college classes" = 9,
                      "Non-degree student" = 10,
                      "Faculty or staff" = 11,
                      "Other (please specify)" = 12)

    } else{

    }

    if("SDS_111" %in% names(data) &
       !is.numeric(data$SDS_111) &
       !is.integer(data$SDS_111)){

      data$SDS_111 <- data$SDS_111 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_112" %in% names(data) &
       !is.numeric(data$SDS_112) &
       !is.integer(data$SDS_112)){

      data$SDS_112 <- data$SDS_112 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_113" %in% names(data) &
       !is.numeric(data$SDS_113) &
       !is.integer(data$SDS_113)){

      data$SDS_113 <- data$SDS_113 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_114" %in% names(data) &
       !is.numeric(data$SDS_114) &
       !is.integer(data$SDS_114)){

      data$SDS_114 <- data$SDS_114 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_115" %in% names(data) &
       !is.numeric(data$SDS_115) &
       !is.integer(data$SDS_115)){

      data$SDS_115 <- data$SDS_115 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_116" %in% names(data) &
       !is.numeric(data$SDS_116) &
       !is.integer(data$SDS_116)){

      data$SDS_116 <- data$SDS_116 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_119" %in% names(data) &
       !is.numeric(data$SDS_119) &
       !is.integer(data$SDS_119)){

      data$SDS_119 <- data$SDS_119 |>
        as.character() |>
        dplyr::recode("No" = 0,
                      "Yes" = 1)

    } else{

    }

    if("SDS_120" %in% names(data) &
       !is.numeric(data$SDS_120) &
       !is.integer(data$SDS_120)){

      data$SDS_120 <- data$SDS_120 |>
        as.character() |>
        dplyr::recode("Yes" = 1,
                      "No" = 0)

    } else{

    }

    if("SDS_121" %in% names(data) &
       !is.numeric(data$SDS_121) &
       !is.integer(data$SDS_121)){

      data$SDS_121 <- data$SDS_121 |>
        as.character() |>
        dplyr::recode("Yes" = 1,
                      "No" = 0)

    } else{

    }

    if("SDS_122" %in% names(data) &
       !is.numeric(data$SDS_122) &
       !is.integer(data$SDS_122)){

      data$SDS_122 <- data$SDS_122 |>
        as.character() |>
        dplyr::recode("Yes" = 1,
                      "No" = 0)

    } else{

    }

    if("SDS_123" %in% names(data) &
       !is.numeric(data$SDS_123) &
       !is.integer(data$SDS_123)){

      data$SDS_123 <- data$SDS_123 |>
        as.character() |>
        dplyr::recode("Yes" = 1,
                      "No" = 0)

    } else{

    }

    if("SDS_1042" %in% names(data) &
       !is.numeric(data$SDS_1042) &
       !is.integer(data$SDS_1042)){

      data$SDS_1042 <- data$SDS_1042 |>
        as.character() |>
        dplyr::recode("On-campus" = 1,
                      "Off-campus" = 2,
                      "I do not live in one stable, secure residence" = 3,
                      "Other (please specify)" = 4)

    } else{

    }

    if("SDS_1019" %in% names(data) &
       !is.numeric(data$SDS_1019) &
       !is.integer(data$SDS_1019)){

      data$SDS_1019 <- data$SDS_1019 |>
        as.character() |>
        dplyr::recode("None" = 1,
                      "Once" = 2,
                      "Twice" = 3,
                      "3 to 5 times" = 4,
                      "6 to 9 times" = 5,
                      "10 or more times" = 6)

    } else{

    }

    if("SDS_132" %in% names(data) &
       !is.numeric(data$SDS_132) &
       !is.integer(data$SDS_132)){

      data$SDS_132 <- data$SDS_132 |>
        as.character() |>
        dplyr::recode("Yes" = 1,
                      "No" = 0,
                      "I did not use alcohol" = 2)

    } else{

    }

    if("SDS_2096" %in% names(data) &
       !is.numeric(data$SDS_2096) &
       !is.integer(data$SDS_2096)){

      data$SDS_2096 <- data$SDS_2096 |>
        as.character() |>
        dplyr::recode("None" = 1,
                      "Once" = 2,
                      "Twice" = 3,
                      "3 to 5 times" = 4,
                      "6 to 9 times" = 5,
                      "10 or more times" = 6)

    } else{

    }

    if("SDS_133" %in% names(data) &
       !is.numeric(data$SDS_133) &
       !is.integer(data$SDS_133)){

      data$SDS_133 <- data$SDS_133 |>
        as.character() |>
        dplyr::recode("Yes" = 1,
                      "No" = 0,
                      "I did not use alcohol" = 2)

    } else{

    }

    if("SDS_134" %in% names(data) &
       !is.numeric(data$SDS_134) &
       !is.integer(data$SDS_134)){

      data$SDS_134 <- data$SDS_134 |>
        as.character() |>
        dplyr::recode("Yes" = 1,
                      "No" = 0)

    } else{

    }

    if("SDS_135" %in% names(data) &
       !is.numeric(data$SDS_135) &
       !is.integer(data$SDS_135)){

      data$SDS_135 <- data$SDS_135 |>
        as.character() |>
        dplyr::recode("Yes" = 1,
                      "No" = 0,
                      "I did not use any recreational drugs or non-prescribed medications" = 2)

    } else{

    }

    if("SDS_128" %in% names(data) &
       !is.numeric(data$SDS_128) &
       !is.integer(data$SDS_128)){

      data$SDS_128 <- data$SDS_128 |>
        as.character() |>
        dplyr::recode("Yes" = 1,
                      "No" = 0)

    } else{

    }

    if("SDS_130" %in% names(data) &
       !is.numeric(data$SDS_130) &
       !is.integer(data$SDS_130)){

      data$SDS_130 <- data$SDS_130 |>
        as.character() |>
        dplyr::recode("Yes" = 1,
                      "No" = 0)

    } else{

    }

    if("SDS_136" %in% names(data) &
       !is.numeric(data$SDS_136) &
       !is.integer(data$SDS_136)){

      data$SDS_136 <- data$SDS_136 |>
        as.character() |>
        dplyr::recode("Yes" = 1,
                      "No" = 0,
                      "I did not gamble" = 2)

    } else{

    }

    if("SDS_1060" %in% names(data) &
       !is.numeric(data$SDS_1060) &
       !is.integer(data$SDS_1060)){

      data$SDS_1060 <- data$SDS_1060 |>
        as.character() |>
        dplyr::recode("Yes" = 1,
                      "No" = 0)

    } else{

    }

    if("SDS_127" %in% names(data) &
       !is.numeric(data$SDS_127) &
       !is.integer(data$SDS_127)){

      data$SDS_127 <- data$SDS_127 |>
        as.character() |>
        dplyr::recode("Yes" = 1,
                      "No" = 0)

    } else{

    }

    if("SDS_124" %in% names(data) &
       !is.numeric(data$SDS_124) &
       !is.integer(data$SDS_124)){

      data$SDS_124 <- data$SDS_124 |>
        as.character() |>
        dplyr::recode("Hardly ever" = 1,
                      "Some of the time" = 2,
                      "Often" = 3)

    } else{

    }

    if("SDS_125" %in% names(data) &
       !is.numeric(data$SDS_125) &
       !is.integer(data$SDS_125)){

      data$SDS_125 <- data$SDS_125 |>
        as.character() |>
        dplyr::recode("Hardly ever" = 1,
                      "Some of the time" = 2,
                      "Often" = 3)

    } else{

    }

    if("SDS_126" %in% names(data) &
       !is.numeric(data$SDS_126) &
       !is.integer(data$SDS_126)){

      data$SDS_126 <- data$SDS_126 |>
        as.character() |>
        dplyr::recode("Hardly ever" = 1,
                      "Some of the time" = 2,
                      "Often" = 3)

    } else{

    }

    if("SDS_131" %in% names(data) &
       !is.numeric(data$SDS_131) &
       !is.integer(data$SDS_131)){

      data$SDS_131 <- data$SDS_131 |>
        as.character() |>
        dplyr::recode("Not at all" = 1,
                      "Slightly" = 2,
                      "Moderately" = 3,
                      "Very" = 4,
                      "Extremely" = 5)

    } else{

    }

  # Return
  return(data)

  })

}

#' @rdname sds_to_numeric
#' @export
SDS_to_numeric <- sds_to_numeric
