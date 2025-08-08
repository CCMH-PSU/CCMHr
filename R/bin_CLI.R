#' Create a vector that bins enrollment.
#'
#' @name bin_enrollment
#'
#' @description Enrollment is the number of students that are enrolled at an institution. This function is often used in relation to data frames that contain variables related to the Clinical Load Index (CLI). Enrollment bins include "under 1,501", "1,501-2,500", "2,501-5,000", "5,001-7,500", "7,501-10,000", "10,001-15,000", "15,001-20,000", "20,001-25,000", "25,001-30,000", "30,001-35,000", "35,001-45,000", and "45,001+".
#'
#' @param x A variable within a data frame that pertains to enrollment at institutions.
#'
#' @return A vector with enrollment bins.
#'
#' @export
#'

bin_enrollment <- function(x){

  # Create enrollment bins
  cut(x,
      c(0, 1500,
        2500, 5000,
        7500, 10000,
        15000, 20000,
        25000, 30000,
        35000, 45000,
        1000000),
      c("under 1,501", "1,501-2,500",
        "2,501-5,000", "5,001-7,500",
        "7,501-10,000", "10,001-15,000",
        "15,001-20,000", "20,001-25,000",
        "25,001-30,000", "30,001-35,000",
        "35,001-45,000", "45,001+"))

}

#' Create a vector that bins utilization.
#'
#' @name bin_utilization
#'
#' @description Utilization is the number of students seen by a center. This function is often used in relation to data frames that contain variables related to the Clinical Load Index (CLI). Utilization bins include "under 151", "151-200", "201-300", "301-350", "351-400", "401-500", "501-600", "601-700", "701-850", "851-1000", "1001-1200", "1201-1500", "1501-2000", "2001-3000", and "3001+".
#'
#' @param x A variable within a data frame that pertains to utilization at the center.
#'
#' @return A vector with utilization bins.
#'
#' @export
#'

bin_utilization <- function(x){

  # Create utilization bins
  cut(x,
      c(0, 150,
        200, 300,
        350, 400,
        500, 600,
        700, 850,
        1000, 1200,
        1500, 2000,
        3000, 10000),
      c("under 151", "151-200",
        "201-300", "301-350",
        "351-400", "401-500",
        "501-600", "601-700",
        "701-850", "851-1000",
        "1001-1200", "1201-1500",
        "1501-2000", "2001-3000",
        "3001+"))

}

#' Create a vector that bins capacity.
#'
#' @name bin_capacity
#'
#' @description Capacity is the total number of contracted/expected clinical hours for a typical/busy week when the center is fully staffed. This function is often used in relation to data frames that contain variables related to the Clinical Load Index (CLI). Capacity bins include "48 or less (0-2 Standardized Counselors)", "49-72 (2-3 Standardized Counselors)", "73-96 (3-4 Standardized Counselors)", "97-120 (4-5 Standardized Counselors)", "121-144 (5-6 Standardized Counselors)", "145-168 (6-7 Standardized Counselors)", "169-192 (7-8 Standardized Counselors)", "193-240 (7-9 Standardized Counselors)", "241-312 (9-13 Standardized Counselors)", "313-432 (13-18 Standardized Counselors)", and "over 433 (18+ Standardized Counselors)".
#'
#' @param x A variable within a data frame that pertains to capacity at the center.
#'
#' @return A vector with capacity bins.
#'
#' @export
#'

bin_capacity <- function(x){

  # Create capacity bins
  cut(x,
      c(0, 48,
        72, 96,
        120, 144,
        168, 192,
        240, 312,
        432, 1500),
      c("48 or less (0-2 Standardized Counselors)",
        "49-72 (2-3 Standardized Counselors)", "73-96 (3-4 Standardized Counselors)",
        "97-120 (4-5 Standardized Counselors)", "121-144 (5-6 Standardized Counselors)",
        "145-168 (6-7 Standardized Counselors)", "169-192 (7-8 Standardized Counselors)",
        "193-240 (7-9 Standardized Counselors)", "241-312 (9-13 Standardized Counselors)",
        "313-432 (13-18 Standardized Counselors)", "over 433 (18+ Standardized Counselors)"))

}

#' Create a variable that bins the percentage of students who utilize a center at the institution.
#'
#' @name bin_inst_utilization
#'
#' @description Institutional utilization refers to the percentage of students who utilize a center at the institution. This function is often used in relation to data frames that contain variables related to the Clinical Load Index (CLI). Institution utilization bins include "less than 5%", "5-7%", "7-10%", "10-12%", "12-15%", "15-20%", "20-30%", and "more than 30%".
#'
#' @param x A variable within a data frame pertains to the percentage of students who utilize a center at the institution.
#'
#' @return A vector with institution utilization bins.
#'
#' @export
#'

bin_inst_utilization <- function(x){

  # Create institutional utilization bins
  cut(x,
      c(0, 0.05,
        0.07, 0.1,
        0.12, 0.15,
        0.2, 0.3,
        1),
      c("less than 5%", "5-7%",
        "7-10%", "10-12%",
        "12-15%", "15-20%",
        "20-30%", "more than 30%"))

}
