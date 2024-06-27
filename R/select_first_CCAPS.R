#' Select each client's first administration of a specified survey
#'
#' @param data data frame
#' @param order_by variable to order by when selecting the first administration.
#' @param keep_all Columns to keep. `TRUE` will keep all columns, while `FALSE` will keep only IDs and CCAPS subscales, SDS items, or CLICC items.
#' @param keep_columns A string list of column names to retain. If not specified, and keep_all = FALSE, defaults to the relevent columns for that data form.
#' @param by_year A logical statement to indicate if first administration by year instead of overall. If `TRUE`, the first administration of each client will be selected by year. If `FALSE`, the first administration of each client will be selected regardless of year. Default = `FALSE`.
#'
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
                                                "Academics62"),
                               by_year = FALSE) {
  # Addressing "no visible binding for global variable" notes in R CMD check
  Is_ValidCCAPS = UniqueClientID = NULL

  #Error messages presented if specific variables are missing
    #if order_by specified as an argument (quoted) is missing
      if(!any(names(data) == order_by)) {
       stop("order_by variable not present in data")
      }
    #if Is_ValidCCAPS is missing
      if (!any(names(data) == "Is_ValidCCAPS")) {
       stop("Is_ValidCCAPS variable not present in data")
      }
    #if Data_year is missing when by_year = FALSE
      if (!any(names(data) == "Data_year") & by_year == TRUE) {
        stop("Data_year variable not present in data")
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
  datatable <- data.table::as.data.table(data)

  #Only Valid CCAPS
  datatable <- datatable[Is_ValidCCAPS == 1,]

  #Order list of variables to be grouped
    orderlist<- c("UniqueClientID",
                  order_by)

  #Order data table by order list
    datatable <- data.table::setorderv(datatable,
                                  orderlist)

  #First administration by year
    if(by_year == TRUE) {
      datatable <- datatable[, .SD[1], by=.(UniqueClientID, Data_year)][, ..keep_columns]
    } else {
      datatable <- datatable[, .SD[1], by=.(UniqueClientID)][, ..keep_columns]
    }

  #Return data table as a data frame
    return(as.data.frame(datatable))
}


#' @export
#' @rdname select_first_CCAPS

select_first_SDS <- function(data,
                             order_by = "Date",
                             keep_all = FALSE,
                             keep_columns = "SDS",
                             by_year = FALSE) {

  # Addressing "no visible binding for global variable" notes in R CMD check
  Has_SDS = UniqueClientID = NULL

  #Error messages presented if specific variables are missing
    #if order_by specified as an argument (quoted) is missing
      if (!any(names(data) == order_by)) {
        stop("order_by variable not present in data")
      }
    #if Has_SDS is missing
      if (!"Has_SDS" %in% names(data)) {
        stop("Has_SDS variable not present in data")
      }
    #if Data_year is missing when by_year = FALSE
      if (!any(names(data) == "Data_year") & by_year == TRUE) {
        stop("Data_year variable not present in data")
      }

  #Setting data table
  datatable <- data.table::as.data.table(data)

  #Only Valid SDS
  datatable <- datatable[Has_SDS == 1,]

  #Order list of variables to be grouped
    orderlist<- c("UniqueClientID",
                  order_by)

  #Order data table by order list
    datatable <- data.table::setorderv(datatable,
                                  orderlist)

  #First administration by year
    if(by_year == TRUE) {
      datatable <- datatable[, .SD[1], by=.(UniqueClientID, Data_year)]
    } else {
      datatable <- datatable[, .SD[1], by=.(UniqueClientID)]
    }

  #Specify keep_all and keep_columns argument (what columns need to be kept)
    if(keep_all == TRUE) {
      if(keep_columns[1] != "SDS")
          usethis::ui_warn("Vector of column names to keep not applicable when keep_all = TRUE. All columns were retained.")
      } else if(keep_all == FALSE) {
        if(keep_columns[1] == "SDS") {
          x.UniqueClientID <- which(colnames(datatable) == "UniqueClientID")
          x.CcmhID <- which(colnames(datatable) == "CcmhID")
          x.SDS <- grep("SDS_", names(datatable), fixed=TRUE)
          x.ClientAge <- which(colnames(datatable) == "ClientAge")
          x.list <- list(x.UniqueClientID, x.CcmhID, x.SDS, x.ClientAge)
          x.list <- unlist(x.list, recursive = FALSE)
          datatable <- datatable[, x.list, with=F]
        } else {
          if(!all(keep_columns %in% names(datatable))) {
            usethis::ui_warn("All columns specified in keep_columns were not present in the data. Only present columns were retained.")
          }
          datatable <- datatable[, keep_columns, with=FALSE]
        }
      }

  #Return data frame
    return(as.data.frame(datatable))
}


#' @export
#' @rdname select_first_CCAPS

select_first_CLICC <- function(data,
                               order_by = "Date",
                               keep_all = FALSE,
                               keep_columns = "CLICC",
                               by_year = FALSE) {

  # Addressing "no visible binding for global variable" notes in R CMD check
  Has_CLICC = UniqueClientID = NULL

  #Error messages presented if specific variables are missing
    #if order_by specified as an argument (quoted) is missing
      if (!any(names(data) == order_by)) {
        stop("order_by variable not present in data")
      }
    #If Has_CLICC is missing
      if (!"Has_CLICC" %in% names(data)) {
        stop("Has_CLICC variable not present in data")
      }
    #if Data_year is missing when by_year = FALSE
      if (!any(names(data) == "Data_year") & by_year == TRUE) {
        stop("Data_year variable not present in data")
      }

  #Setting data table
  datatable <- data.table::as.data.table(data)

  #Only Valid CLICC
  datatable <- datatable[Has_CLICC == 1,]

  #Order list of variables to be grouped
    orderlist<- c("UniqueClientID",
                  order_by)

  #Order data table by order list
    datatable <- data.table::setorderv(datatable,
                                  orderlist)

  #First administration by year
    if(by_year == TRUE) {
      datatable <- datatable[, .SD[1], by=.(UniqueClientID, Data_year)]
    } else {
      datatable <- datatable[, .SD[1], by=.(UniqueClientID)]
    }

  #Specify keep_all and keep_columns argument (what columns need to be kept)
    if(keep_all == TRUE) {
      if(keep_columns[1] != "CLICC")
        usethis::ui_warn("Vector of column names to keep not applicable when keep_all = TRUE. All columns were retained.")
      } else if(keep_all == FALSE) {
        if (keep_columns[1] == "CLICC") {
          x.UniqueClientID <- which(colnames(datatable) == "UniqueClientID")
          x.CcmhID <- which(colnames(datatable) == "CcmhID")
          x.CLICC <- grep("CLICC_", names(datatable), fixed=TRUE)
          x.list <- list(x.UniqueClientID, x.CcmhID, x.CLICC)
          x.list <- unlist(x.list, recursive = FALSE)
          datatable <- datatable[, x.list, with=F]
        } else {
          if (!all(keep_columns %in% names(datatable))) {
            usethis::ui_warn("All columns specified in keep_columns were not present in the data. Only present columns were retained.")
          }
          datatable <- datatable[, keep_columns, with=FALSE]
        }
      }

    #Return as a data frame
      return(as.data.frame(datatable))
}

#' @export
#' @rdname select_first_CCAPS

select_first_Closure <- function(data,
                                 order_by = "Date",
                                 keep_all = FALSE,
                                 keep_columns = "Closure",
                                 by_year = FALSE) {

  # Addressing "no visible binding for global variable" notes in R CMD check
    Has_Closure = UniqueClientID = NULL

  #Error messages presented if specific variables are missing
    #if order_by specified as an argument (quoted) is missing
    if (!any(names(data) == order_by)) {
      stop("order_by variable not present in data")
    }
  #If Has_CLOSURE is missing
    if (!"Has_Closure" %in% names(data)) {
      stop("Has_Closure variable not present in data")
    }
  #if Data_year is missing when by_year = FALSE
    if (!any(names(data) == "Data_year") & by_year == TRUE) {
      stop("Data_year variable not present in data")
    }

  #Setting data table
    datatable <- data.table::as.data.table(data)

  #Only Valid CLICC
    datatable <- datatable[Has_Closure == 1,]

  #Order list of variables to be grouped
    orderlist<- c("UniqueClientID",
                  order_by)

  #Order data table by order list
    datatable <- data.table::setorderv(datatable,
                                       orderlist)

  #First administration by year
    if(by_year == TRUE) {
      datatable <- datatable[, .SD[1], by=.(UniqueClientID, Data_year)]
    } else {
      datatable <- datatable[, .SD[1], by=.(UniqueClientID)]
    }

  #Specify keep_all and keep_columns argument (what columns need to be kept)
    if(keep_all == TRUE) {
      if(keep_columns[1] != "Closure")
        usethis::ui_warn("Vector of column names to keep not applicable when keep_all = TRUE. All columns were retained.")
    } else if(keep_all == FALSE) {
      if (keep_columns[1] == "Closure") {
        x.UniqueClientID <- which(colnames(datatable) == "UniqueClientID")
        x.CcmhID <- which(colnames(datatable) == "CcmhID")
        x.CLICC <- grep("Closure_", names(datatable), fixed=TRUE)
        x.list <- list(x.UniqueClientID, x.CcmhID, x.CLICC)
        x.list <- unlist(x.list, recursive = FALSE)
        datatable <- datatable[, x.list, with=F]
      } else {
        if (!all(keep_columns %in% names(datatable))) {
          usethis::ui_warn("All columns specified in keep_columns were not present in the data. Only present columns were retained.")
        }
        datatable <- datatable[, keep_columns, with=FALSE]
      }
    }

  #Return as a data frame
    return(as.data.frame(datatable))
}
