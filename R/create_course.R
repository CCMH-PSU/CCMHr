#' Create courses of therapy within data
#'
#' @description Create courses of therapy in CCMH data.
#'
#' @param data CCMH data files with UniqueClientID and Date variables
#' @param firstOnly If FALSE, will return the whole data set. If TRUE, will return only each client's first course.
#'
#' @return A data frame with added variables:
#' `UniqueClientID_byCourse`: an ID unique to each course of therapy. This is created by adding .1 to UniqueClientID for their first course (e.g. 100.1), .2 for their second course (100.2), etc.
#' `RankCourse`: The course number that the line of data belongs to, numbered 1 through N of courses.
#' `FirstCourse`: A dichotomous variable indicating whether the lines of data belong to a client's first course or not.
#'
#' @export
#'
create_course <- function(data, firstOnly = FALSE){
  if(!"UniqueClientID" %in% names(data)) stop('Data does not contain column named "UniqueClientID"')
  if(!"Date" %in% names(data)) stop('Data does not contain column named "Date"')
  ccmh_bycourse <- dplyr::arrange(data, UniqueClientID, Date) %>%
    dplyr::group_by(UniqueClientID) %>%
    dplyr::mutate(Date_1 = dplyr::lag(Date))
  ccmh_bycourse$Daysbetween <-
    difftime(ccmh_bycourse$Date , ccmh_bycourse$Date_1 , units = c("days"))

  # Creates variables indicating the start of a new course of treatment
  ccmh_bycourse <- dplyr::ungroup(ccmh_bycourse)
  ccmh_bycourse$NewCourse <- NA
  ccmh_bycourse$NewCourse[which(ccmh_bycourse$Daysbetween > 90)] <- 1

  # Ranks a client's new courses if there are multiple
  ccmh_bycourse <- dplyr::arrange(ccmh_bycourse, UniqueClientID, NewCourse, Date)
  ccmh_bycourse$RankCourse <-
    with(ccmh_bycourse,
         ave(UniqueClientID, UniqueClientID, NewCourse, FUN = seq_along))
  ccmh_bycourse$RankCourse[which(is.na(ccmh_bycourse$NewCourse))] = NA
  ccmh_bycourse <- dplyr::arrange(ccmh_bycourse, UniqueClientID, Date)

  # Add 1 to current RankCourse
  ccmh_bycourse$RankCourse <- ccmh_bycourse$RankCourse + 1

  # Need a 1 at the first observation of each new client
  ccmh_bycourse$RankCourse[which(is.na(ccmh_bycourse$Date_1))] <- 1

  # Carry last observation forward
  ccmh_bycourse$RankCourse <- zoo::na.locf(ccmh_bycourse$RankCourse, na.rm = F)

  # First course variable
  ccmh_bycourse$FirstCourse <- NA
  ccmh_bycourse$FirstCourse[which(ccmh_bycourse$RankCourse == 1)] <- 1

  if (firstOnly == FALSE) {
    return(ccmh_bycourse)
  } else if (firstOnly == TRUE) {
    # First course only
    ccmh_firstcourse <- dplyr::filter(ccmh_bycourse, FirstCourse == 1)
    return(ccmh_firstcourse)
  }

}
