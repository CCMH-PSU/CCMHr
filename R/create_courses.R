#' Detect courses of therapy.
#'
#' @description Detect courses of therapy based on the number of days between records in CCMH data. The function can also extract data rows about a client's first treatment course or adjust the number of days between records required to indicate a new treatment course. The "UniqueClientID" and "Date" variables must be in the data frame to run this function.
#'
#' @param data A data frame that includes UniqueClientID and Date variables.
#' @param firstOnly A logical argument to indicate whether the returned data frame should include only data from a client's first administration. If `TRUE`, the returned data frame will only include data about a client's first administration, and no new variables are created. If `FALSE`, the original data frame is returned, and three new variables are created. By default, `FALSE`.
#' @param daysbetween A numeric value that indicates the threshold regarding the number of days between records that distinguish a separate treatment course. By default, `90`.
#'
#' @return A data frame will be returned. If the "firstOnly" argument is `TRUE`, the data frame will only contain data about a client's first treatment course, and no new variables will be created. If the "firstOnly" argument is `FALSE`, the entire data frame is returned, and three new variables are created. The new variables include: `UniqueClientID_byCourse` is an ID unique to each client and course of therapy (i.e., .1 is added to UniqueClientID for their first course (e.g. 100.1), .2 for their second course (100.2), etc); `RankCourse` is the course number that the row pertains to (i.e., 1 to N of courses); and `FirstCourse` is a dichotomous variable indicating whether the rows of data belong to a client's first course or not (i.e., 1 = First course, 0 = Not the first course).
#'
#' @export
#'

create_courses <- function(data,
                           firstOnly = FALSE,
                           daysbetween = 90){

  # Addressing "no visible binding for global variable" notes in R CMD check
  Date_1 = Date = UniqueClientID = Daysbetween = NewCourse = OrderVar = RankCourse = FirstCourse = UniqueClientID_byCourse = NULL

  # Error message for missing variables
    # UniqueClientID
    if(!"UniqueClientID" %in% names(data)){

      stop('Data does not contain column: UniqueClientID')

    } else{

    }

    # Date
    if(!"Date" %in% names(data)){

        stop('Data does not contain column: Date')

    } else{

    }

  # Error message for days between
  if(!is.numeric(daysbetween) |
     daysbetween <= 0){

    stop('daysbetween appointments must be a numeric value greater than 0.')

  } else{

  }

  # Format as a data table
  datatable <- data.table::as.data.table(data)

  # Order list of variable to order by
  orderlist <- c("UniqueClientID", "Date")

  # Arrange the data table
  ccmh_bycourse <- data.table::setorderv(datatable, orderlist)

  # Calculate lag between dates
  ccmh_bycourse <- datatable[,Date_1:=data.table::shift(Date), by=UniqueClientID]

  # Calculate data difference
  ccmh_bycourse <- ccmh_bycourse[,Daysbetween:=as.integer(Date)-as.integer(Date_1), by=UniqueClientID]

  # Create variables indicating the start of a new course of treatment
  ccmh_bycourse <- ccmh_bycourse[,NewCourse:=NA,][,NewCourse:= ifelse(Daysbetween > daysbetween, 1, NewCourse)]

  # Create a variable to save data.table order
  ccmh_bycourse <- ccmh_bycourse[,OrderVar:=1:nrow(ccmh_bycourse),]

  # Order list of variable to order by
  orderlist <- c("UniqueClientID", "NewCourse", "Date")

  # Arrange
  ccmh_bycourse <- data.table::setorderv(ccmh_bycourse, orderlist)

  # Ranks a client's new courses if there are multiple
  ccmh_bycourse <- ccmh_bycourse[,RankCourse:=stats::ave(UniqueClientID, UniqueClientID, NewCourse, FUN = seq_along),][, RankCourse:= ifelse(is.na(NewCourse), NA, RankCourse)]

  # Order list of variable to order by
  orderlist <- c("OrderVar")

  # Arrange by order list
  ccmh_bycourse <- data.table::setorderv(ccmh_bycourse, orderlist)

  # Add 1 to current RankCourse
  ccmh_bycourse <- ccmh_bycourse[,RankCourse:=RankCourse+1,]

  # Need a 1 at the first observation of each new client
  ccmh_bycourse <- ccmh_bycourse[,RankCourse:= ifelse(is.na(Date_1), 1, RankCourse)]

  # Carry last observation forward
  ccmh_bycourse <- ccmh_bycourse[,RankCourse:= zoo::na.locf(RankCourse, na.rm = F)]

  # First course variable
  ccmh_bycourse <- ccmh_bycourse[,FirstCourse:=NA,][,FirstCourse:= ifelse(RankCourse == 1, 1, FirstCourse)]

  # If else statements for firstOnly argument
  if (firstOnly == FALSE){

    # Creating UniqueClientID_byCourse variable
    ccmh_bycourse <- ccmh_bycourse[,UniqueClientID_byCourse:=RankCourse/10,][,UniqueClientID_byCourse:= UniqueClientID_byCourse+UniqueClientID]

    # Removing unwanted vectors
    ccmh_bycourse <- ccmh_bycourse[,`:=`(Date_1 = NULL,
                                         Daysbetween = NULL,
                                         NewCourse = NULL,
                                         OrderVar = NULL)]

    # Set column order
    ccmh_bycourse <- data.table::setcolorder(ccmh_bycourse, c("UniqueClientID", "UniqueClientID_byCourse"))

    # Convert back to data frame
    data.table::setDF(ccmh_bycourse)

    # Return data frame
    return(ccmh_bycourse)

  } else{

    # First course only
    ccmh_bycourse <- ccmh_bycourse[FirstCourse == 1, ]

    # Removing unwanted vectors
    ccmh_bycourse <- ccmh_bycourse[,`:=`(Date_1 = NULL,
                                         Daysbetween = NULL,
                                         NewCourse = NULL,
                                         RankCourse = NULL,
                                         FirstCourse = NULL,
                                         OrderVar = NULL)]

    # Convert back to data frame
    data.table::setDF(ccmh_bycourse)

    # Return data frame
    return(ccmh_bycourse)

  }

}
