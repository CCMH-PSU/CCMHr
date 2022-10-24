#' Select each client's first administration of a specified survey
#'
#' @param data data frame
#' @param order_by variable to order by when selecting the first administration.
#' @param keep_all Columns to keep. `TRUE` will keep all columns, while `FALSE` will keep only IDs and CCAPS subscales, SDS items, or CLICC items.
#' @param keep_columns A string list of column names to retain. If not specified, and keep_all = FALSE, defaults to the relevent columns for that data form.
#'
#' @return
#' @export
#'

select_first_CCAPS <- function(data,
                               order_by = "Date",
                               keep_all = FALSE,
                               keep_columns = c("UniqueClientID", "CcmhID",
                                                "Depression34", "Anxiety34",
                                                "Social_Anxiety34", "Academics34",
                                                "Eating34", "Hostility34",
                                                "Alcohol34", "DI", "Depression62",
                                                "Eating62", "Substance62",
                                                "Anxiety62", "Hostility62",
                                                "Social_Anxiety62", "Family62",
                                                "Academics62")) {
  #Required Packages
    library(data.table)

  #Error messages presented if specific variables are missing
    #if order_by specified as an argument (quoted) is missing
      if(!any(names(data) == order_by)) {
       stop("order_by variable not present in data")
      }
    #if Is_ValidCCAPS is missing
      if (!any(names(data) == "Is_ValidCCAPS")) {
       stop("Is_ValidCCAPS variable not present in data")
      }

  #Specify keep_all argument (what columns need to be kept)
    if (keep_all == TRUE) {
      keep_columns <- names(data)
    } else if (keep_all == FALSE) {
        if(!all(keep_columns %in% names(data))) {
           usethis::ui_stop("All columns specified in keep_columns were not present in the data.")
        }
      }

  #Setting data table
    data.table::setDT(data)

  #Only Valid CCAPS
    data <- data[Is_ValidCCAPS == 1,]

  #Order list of variables to be grouped
    orderlist<- c("UniqueClientID",
                  order_by)

  #Order data table by order list
    data <- data.table::setorderv(data,
                                  orderlist)

  #Obtain by first occurrences
    data <- data[, .SD[1], by=.(UniqueClientID)][, ..keep_columns]

  #Return data table as a data frame
    return(as.data.frame(data))
}


#' @export
#' @rdname select_first_CCAPS

select_first_SDS <- function(data,
                             order_by = "Date",
                             keep_all = FALSE,
                             keep_columns = "SDS") {

  #Required Packages
    library(data.table)

  #Error messages presented if specific variables are missing
    #if order_by specified as an argument (quoted) is missing
      if (!any(names(data) == order_by)) {
        stop("order_by variable not present in data")
      }
    #if Has_SDS is missing
      if (!"Has_SDS" %in% names(data)) {
        stop("Has_SDS variable not present in data")
      }

  #Setting data table
    data <- data.table::setDT(data)

  #Only Valid SDS
    data <- data[Has_SDS == 1,]

  #Order list of variables to be grouped
    orderlist<- c("UniqueClientID",
                  order_by)

  #Order data table by order list
    data <- data.table::setorderv(data,
                                  orderlist)

  #Obtain by first occurrences
    data <- data[, .SD[1], by=.(UniqueClientID)]

  #Specify keep_all and keep_columns argument (what columns need to be kept)
    if(keep_all == TRUE) {
      if(keep_columns[1] != "SDS")
          usethis::ui_warn("Vector of column names to keep not applicable when keep_all = TRUE. All columns were retained.")
      } else if(keep_all == FALSE) {
        if(keep_columns[1] == "SDS") {
          x.UniqueClientID <- which(colnames(data) == "UniqueClientID")
          x.CcmhID <- which(colnames(data) == "CcmhID")
          x.SDS <- grep("SDS_", names(data), fixed=TRUE)
          x.Age <- which(colnames(data) == "Age")
          x.list <- list(x.UniqueClientID, x.CcmhID, x.SDS, x.Age)
          x.list <- unlist(x.list, recursive = FALSE)
          data <- data[, x.list, with=F]
        } else {
          if(!all(keep_columns %in% names(data))) {
            usethis::ui_warn("All columns specified in keep_columns were not present in the data. Only present columns were retained.")
          }
          data <- data[, keep_columns, with=FALSE]
        }
      }

  #Return data frame
    return(as.data.frame(data))
}


#' @export
#' @rdname select_first_CCAPS

select_first_CLICC <- function(data,
                               order_by = "Date",
                               keep_all = FALSE,
                               keep_columns = "CLICC") {

  #Required Packages
    library(data.table)

  #Error messages presented if specific variables are missing
    #if order_by specified as an argument (quoted) is missing
      if (!any(names(data) == order_by)) {
        stop("order_by variable not present in data")
      }
    #If Has_CLICC is missing
      if (!"Has_CLICC" %in% names(data)) {
        stop("Has_CLICC variable not present in data")
      }

  #Setting data table
    data <- data.table::setDT(data)

  #Only Valid CLICC
    data <- data[Has_CLICC == 1,]

  #Order list of variables to be grouped
    orderlist<- c("UniqueClientID",
                  order_by)

  #Order data table by order list
    data <- data.table::setorderv(data,
                                  orderlist)

  #Obtain by first occurrences
    data <- data[, .SD[1], by=.(UniqueClientID)]

  #Specify keep_all and keep_columns argument (what columns need to be kept)
    if(keep_all == TRUE) {
      if(keep_columns[1] != "CLICC")
        usethis::ui_warn("Vector of column names to keep not applicable when keep_all = TRUE. All columns were retained.")
      } else if(keep_all == FALSE) {
        if (keep_columns[1] == "CLICC") {
          x.UniqueClientID <- which(colnames(data) == "UniqueClientID")
          x.CcmhID <- which(colnames(data) == "CcmhID")
          x.CLICC <- grep("CLICC_", names(data), fixed=TRUE)
          x.list <- list(x.UniqueClientID, x.CcmhID, x.CLICC)
          x.list <- unlist(x.list, recursive = FALSE)
          data <- data[, x.list, with=F]
        } else {
          if (!all(keep_columns %in% names(data))) {
            usethis::ui_warn("All columns specified in keep_columns were not present in the data. Only present columns were retained.")
          }
          data <- data[, keep_columns, with=FALSE]
        }
      }

    #Return as a data frame
      return(as.data.frame(data))
}





