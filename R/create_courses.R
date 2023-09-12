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
#' If `firstOnly` = `TRUE`, only the first course is selected, and no variables are added.
#'
#' @export
#' @import data.table
#'
create_courses <- function(data,
                           firstOnly = FALSE,
                           daysbetween = 90){
  # Addressing "no visible binding for global variable" notes in R CMD check
  Date_1 = Date = UniqueClientID = Daysbetween = NewCourse = OrderVar = RankCourse = FirstCourse = UniqueClientID_byCourse = NULL

  #Error message for missing variables
    #UniqueClientID
      if(!"UniqueClientID" %in% names(data))
        stop('Data does not contain column: UniqueClientID')
    #Date
      if(!"Date" %in% names(data))
        stop('Data does not contain column: Date')
  #Error message for days between
    if(!is.numeric(daysbetween) | daysbetween <= 0)
      stop('daysbetween appointments must be a numeric value greater than 0.')

  #Creating a data table
    #data table
      datatable <- data.table::as.data.table(data)
    #Order list of variable to order by
      orderlist <- c("UniqueClientID", "Date")
    #Arrange
      ccmh_bycourse <- data.table::setorderv(datatable, orderlist)
    #Calculate lag between dates
      ccmh_bycourse <- datatable[,Date_1:=data.table::shift(Date), by=UniqueClientID]
    #Calculate data difference
      ccmh_bycourse <- ccmh_bycourse[,Daysbetween:=as.integer(Date)-as.integer(Date_1), by=UniqueClientID]
    #Creates variables indicating the start of a new course of treatment
      ccmh_bycourse <- ccmh_bycourse[,NewCourse:=NA,][, NewCourse:= ifelse(Daysbetween > daysbetween, 1, NewCourse)]
    #Create a variable to save data.table order
      ccmh_bycourse <- ccmh_bycourse[,OrderVar:=1:nrow(ccmh_bycourse),]
    #Order list of variable to order by
      orderlist <- c("UniqueClientID", "NewCourse", "Date")
    #Arrange
      ccmh_bycourse <- data.table::setorderv(ccmh_bycourse, orderlist)
    #Ranks a client's new courses if there are multiple
      ccmh_bycourse <- ccmh_bycourse[,RankCourse:=stats::ave(UniqueClientID, UniqueClientID, NewCourse, FUN = seq_along),][, RankCourse:= ifelse(is.na(NewCourse), NA, RankCourse)]
    #Order list of variable to order by
      orderlist <- c("OrderVar")
    #Arrange
      ccmh_bycourse <- data.table::setorderv(ccmh_bycourse, orderlist)
    #Add 1 to current RankCourse
      ccmh_bycourse <- ccmh_bycourse[,RankCourse:=RankCourse+1,]
    #Need a 1 at the first observation of each new client
      ccmh_bycourse <- ccmh_bycourse[, RankCourse:= ifelse(is.na(Date_1), 1, RankCourse)]
    #Carry last observation forward
      ccmh_bycourse <- ccmh_bycourse[, RankCourse:= zoo::na.locf(RankCourse, na.rm = F)]

  #First course variable
      ccmh_bycourse <- ccmh_bycourse[,FirstCourse:=NA,][, FirstCourse:= ifelse(RankCourse == 1, 1, FirstCourse)]

  #If else statments
  if (firstOnly == FALSE) {
    ccmh_bycourse <- ccmh_bycourse[,UniqueClientID_byCourse:=RankCourse/10,][, UniqueClientID_byCourse:= UniqueClientID_byCourse+UniqueClientID]

    #Removing unwanted vectors
    ccmh_bycourse <- ccmh_bycourse[,`:=`(Date_1 = NULL,
                                          Daysbetween = NULL,
                                          NewCourse = NULL,
                                         OrderVar = NULL)]
    ccmh_bycourse <- data.table::setcolorder(ccmh_bycourse, c("UniqueClientID", "UniqueClientID_byCourse"))

    data.table::setDF(ccmh_bycourse)

    return(ccmh_bycourse)

  } else if (firstOnly == TRUE) {
    #First course only
      ccmh_bycourse <- ccmh_bycourse[FirstCourse == 1, ]
    #Removing unwanted vectors
      ccmh_bycourse <- ccmh_bycourse[,`:=`(Date_1 = NULL,
                                           Daysbetween = NULL,
                                           NewCourse = NULL,
                                           RankCourse = NULL,
                                           FirstCourse = NULL,
                                           OrderVar = NULL)]

      data.table::setDF(ccmh_bycourse)

      return(ccmh_bycourse)
  }

}
