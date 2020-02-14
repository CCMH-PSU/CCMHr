#' Create courses of therapy within data
#'
#' @description Create courses of therapy in CCMH data.
#'
#' @param data CCMH data files with UniqueClientID and Date variables
#' @param firstOnly If FALSE, will return the whole data set. If TRUE, will return only each client's first course.
#' @param daysbetween The number of days between records that triggers an additional course. By default, 90.
#'
#' @return A data frame with added variables indicating courses.
#'
#' If `firstOnly` = `FALSE`, all courses are retained, with the following variables added:
#'
#' * `UniqueClientID_byCourse`: an ID unique to each course of therapy. This is created by adding .1 to UniqueClientID for their first course (e.g. 100.1), .2 for their second course (100.2), etc.
#'
#' * `RankCourse`: The course number that the line of data belongs to, numbered 1 through N of courses.
#'
#' * `FirstCourse`: A dichotomous variable indicating whether the lines of data belong to a client's first course or not.
#'
#' If `firstOnly` = `TRUE`, only the first course is selected, and no variables are added:
#'
#'
#' @export
#'
create_courses <- function(data, firstOnly = FALSE, daysbetween = 90){
  if(!"UniqueClientID" %in% names(data)) stop('Data does not contain column: UniqueClientID')
  if(!"Date" %in% names(data)) stop('Data does not contain column: Date')
  if(!is.numeric(daysbetween) | daysbetween <= 0) stop('daysbetween appointments must be a numeric value greater than 0.')
  ccmh_bycourse <- dplyr::arrange(data, .data$UniqueClientID, .data$Date) %>%
    dplyr::group_by(.data$UniqueClientID) %>%
    dplyr::mutate(Date_1 = dplyr::lag(.data$Date))
  ccmh_bycourse$Daysbetween <-
    difftime(ccmh_bycourse$Date , ccmh_bycourse$Date_1 , units = c("days"))

  # Creates variables indicating the start of a new course of treatment
  ccmh_bycourse <- dplyr::ungroup(ccmh_bycourse)
  ccmh_bycourse$NewCourse <- NA
  ccmh_bycourse$NewCourse[which(ccmh_bycourse$Daysbetween > daysbetween)] <- 1

  # Ranks a client's new courses if there are multiple
  ccmh_bycourse <- dplyr::arrange(ccmh_bycourse, .data$UniqueClientID, .data$NewCourse, .data$Date)
  ccmh_bycourse$RankCourse <-
    with(ccmh_bycourse,
         ave(UniqueClientID, UniqueClientID, NewCourse, FUN = seq_along))
  ccmh_bycourse$RankCourse[which(is.na(ccmh_bycourse$NewCourse))] = NA
  ccmh_bycourse <- dplyr::arrange(ccmh_bycourse, .data$UniqueClientID, .data$Date)

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
    ccmh_bycourse$UniqueClientID_byCourse = ccmh_bycourse$RankCourse/10
    ccmh_bycourse$UniqueClientID_byCourse = ccmh_bycourse$UniqueClientID_byCourse + ccmh_bycourse$UniqueClientID
    ccmh_bycourse <- dplyr::select(ccmh_bycourse, -.data$Date_1, -.data$Daysbetween, -.data$NewCourse) %>%
    dplyr::select(.data$UniqueClientID, .data$UniqueClientID_byCourse, tidyselect::everything()) %>%
      as.data.frame()
    return(ccmh_bycourse)

  } else if (firstOnly == TRUE) {
    # First course only
    ccmh_firstcourse <- dplyr::filter(ccmh_bycourse, .data$FirstCourse == 1) %>%
    dplyr::select(-.data$Date_1, -.data$Daysbetween, -.data$NewCourse, -.data$RankCourse, -.data$FirstCourse) %>%
    as.data.frame()
    return(ccmh_firstcourse)
  }

}
