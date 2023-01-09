#' Bin CLI variables (enrollment, utilization, clinical capacity)
#' @description Create categorical versions of CLI variables (enrollment, utilization, clinical capacity)
#'
#' @param x Variable to be binned
#'
#' @return Binned variable
#' @export
#'

bin_enrollment <- function(x) {
  cut(x,
       c(0, 1500, 2500, 5000, 7500, 10000, 15000, 20000, 25000, 30000, 35000, 45000, 1000000),
       c("under 1,501", "1,501-2,500", "2,501-5,000", "5,001-7,500", "7,501-10,000", "10,001-15,000", "15,001-20,000", "20,001-25,000", "25,001-30,000", "30,001-35,000", "35,001-45,000", "45,001+"))
}

#' @export
#' @rdname bin_enrollment

bin_utilization <- function(x) {
  cut(x,
      c(0, 150, 200, 300, 350, 400, 500, 600, 700, 850, 1000, 1200, 1500, 2000, 3000, 10000),
      c("under 151", "151-200", "201-300", "301-350", "351-400", "401-500", "501-600", "601-700", "701-850", "851-1000", "1001-1200", "1201-1500", "1501-2000", "2001-3000", "3001+"))
}

#' @export
#' @rdname bin_enrollment

bin_capacity <- function(x) {
  cut(x,
      c(0, 48, 72, 96, 120, 144, 168, 192, 240, 312, 432, 1500),
      c("48 or less (0-2 Standardized Counselors)", "49-72 (2-3 Standardized Counselors)", "73-96 (3-4 Standardized Counselors)", "97-120 (4-5 Standardized Counselors)", "121-144 (5-6 Standardized Counselors)", "145-168 (6-7 Standardized Counselors)", "169-192 (7-8 Standardized Counselors)", "193-240 (7-9 Standardized Counselors)", "241-312 (9-13 Standardized Counselors)", "313-432 (13-18 Standardized Counselors)", "over 433 (18+ Standardized Counselors)"))
}

#' @export
#' @rdname bin_enrollment

bin_inst_utilization <- function(x) {
  cut(x,
      c(0, .05, .07, .1, .12, .15, .2, .3, 1),
      c("less than 5%", "5-7%", "7-10", "10-12%", "12-15%", "15-20%", "20-30%", "more than 30%"))
}
