#' Extract the client's first CCAPS administration.
#'
#' @description The function extracts the client's first CCAPS administration. It also allows the programmer to pick specific items within CCAPS (e.g., DI, CCAPS_01).
#'
#' @param data A data frame that contains CCAPS data.
#' @param order_by A quoted string to indicate the variable used to determine the order of rows when selecting the first CCAPS administration (row). By default, `"Date"`.
#' @param keep_all A logical statement to indicate whether all columns should be kept. If `TRUE`, all columns are kept. If `FALSE`, variable names listed in keep_columns are kept. By default, `FALSE`.
#' @param keep_columns A quoted string or list of quoted strings of column CCAPS names to retain in the output data frame. By default, `c("UniqueClientID", "CcmhID", "Data_year", "Depression34", "Anxiety34", "Social_Anxiety34", "Academics34", "Eating34", "Hostility34", "Alcohol34", "DI", "Depression62", "Eating62", "Substance62", "Anxiety62", "Hostility62", "Social_Anxiety62", "Family62", "Academics62"`.
#' @param by_year A logical argument to indicate if the first CCAPS administration is by year instead of overall. If `TRUE,` the first CCAPS administration for each client will be extracted across each data year (e.g., multiple rows may correspond to a unique client across multiple years). If `FALSE`, the first CCAPS administration of each client will be selected regardless of year (e.g., each row should correspond to a single unique client). By default, `FALSE`.
#'
#' @return A data frame with the client's first CCAPS administration.
#'
#' @export

select_first_CCAPS <- function(data,
                               order_by = "Date",
                               keep_all = FALSE,
                               keep_columns = c("UniqueClientID", "CcmhID",
                                                "Data_year", "Depression34",
                                                "Anxiety34", "Social_Anxiety34",
                                                "Academics34", "Eating34",
                                                "Hostility34", "Alcohol34",
                                                "DI", "Depression62",
                                                "Eating62", "Substance62",
                                                "Anxiety62", "Hostility62",
                                                "Social_Anxiety62", "Family62",
                                                "Academics62"),
                               by_year = FALSE){

  # Addressing "no visible binding for global variable" notes in R CMD check
  Is_ValidCCAPS = UniqueClientID = NULL

  # Error messages presented if specific variables are missing
  if(!any(names(data) == order_by)){ # If order_by specified as an argument (quoted) is missing

    stop("The order_by variable is not present in the data.")

  } else if(!any(names(data) == "Is_ValidCCAPS")){ #if Is_ValidCCAPS is missing

    stop("The Is_ValidCCAPS variable is not present in the data.")

  } else if(!any(names(data) == "Data_year") &
            by_year == TRUE){ #if Data_year is missing when by_year = FALSE

    stop("The Data_year variable is not present in the data.")

  } else{

  }

  # Specify keep_all argument (what columns need to be kept)
  if(keep_all == TRUE){

    keep_columns <- names(data)

  } else if(keep_all == FALSE){

    if(!all(keep_columns %in% names(data))){

      usethis::ui_stop("At least one variable name specified in keep_columns were not present in the data.")

    } else{

    }

  } else{

  }

  # Setting data table
  datatable <- data.table::as.data.table(data)

  # Only Valid CCAPS
  datatable <- datatable[Is_ValidCCAPS == 1, ]

  # Order list of variables to be grouped
  orderlist<- c("UniqueClientID", order_by)

  # Order data table by order list
  datatable <- data.table::setorderv(datatable, orderlist)

  # Extract first administration by year
  if(by_year == TRUE){

    datatable <- datatable[, .SD[1], by=.(UniqueClientID, Data_year)][, ..keep_columns]

  } else{

    datatable <- datatable[, .SD[1], by=.(UniqueClientID)][, ..keep_columns]

  }

  # Return data table as a data frame
  return(as.data.frame(datatable))

}


#' Extract the client's first SDS administration.
#'
#' @description The function extracts the client's first SDS administration. It also allows the programmer to pick specific items within SDS (e.g., SDS_01, SDS_02).
#'
#' @param data A data frame that contains SDS data.
#' @param order_by A quoted string to indicate the variable used to determine the order of rows when selecting the first SDS administration (row). By default, "`Date`".
#' @param keep_all A logical statement to indicate whether all columns should be kept. If `TRUE`, all columns are kept. If `FALSE`, variable names listed in keep_columns are kept. By default, `FALSE`.
#' @param keep_columns A quoted string or list of quoted strings of column SDS names to retain in the output data frame. By default, `"SDS"` or all SDS columns.
#' @param by_year A logical argument to indicate if the first SDS administration is by year instead of overall. If `TRUE,` the first SDS administration for each client will be extracted across each data year (e.g., multiple rows may correspond to a unique client across multiple years). If `FALSE`, the first SDS administration of each client will be selected regardless of year (e.g., each row should correspond to a single unique client). By default, `FALSE`.
#'
#' @return A data frame with the client's first SDS administration.
#'
#' @export

select_first_SDS <- function(data,
                             order_by = "Date",
                             keep_all = FALSE,
                             keep_columns = "SDS",
                             by_year = FALSE){

  # Addressing "no visible binding for global variable" notes in R CMD check
  Has_SDS = UniqueClientID = NULL

  # Error messages presented if specific variables are missing
  if(!any(names(data) == order_by)){ # If order_by specified as an argument (quoted) is missing

    stop("The order_by variable is not present in the data.")

  } else if(!"Has_SDS" %in% names(data)){ #if Has_SDS is missing

    stop("The Has_SDS variable is not present in the data.")

  } else if(!any(names(data) == "Data_year") &
            by_year == TRUE){ #if Data_year is missing when by_year = FALSE

    stop("The Data_year variable is not present in the data.")

  } else{

  }

  # Setting data table
  datatable <- data.table::as.data.table(data)

  # Only valid SDS
  datatable <- datatable[Has_SDS == 1,]

  # Order list of variables to be grouped
  orderlist<- c("UniqueClientID", order_by)

  # Order data table by order list
  datatable <- data.table::setorderv(datatable, orderlist)

  # First administration by year
  if(by_year == TRUE){

    datatable <- datatable[, .SD[1], by=.(UniqueClientID, Data_year)]

  } else{

    datatable <- datatable[, .SD[1], by=.(UniqueClientID)]

  }

  # Specify what columns need to be kept
  if(keep_all == TRUE){

    if(keep_columns[1] != "SDS"){

      usethis::ui_warn("Vector of column names to keep not applicable when keep_all = TRUE. All columns were retained.")

    } else{

    }

  } else{

    if(keep_columns[1] == "SDS"){

      x.UniqueClientID <- which(colnames(datatable) == "UniqueClientID")
      x.CcmhID <- which(colnames(datatable) == "CcmhID")
      x.Data_year <- which(colnames(datatable) == "Data_year")
      x.SDS <- grep("SDS_", names(datatable), fixed = TRUE)
      x.ClientAge <- which(colnames(datatable) == "ClientAge")
      x.list <- list(x.UniqueClientID, x.CcmhID, x.Data_year, x.SDS, x.ClientAge)
      x.list <- unlist(x.list, recursive = FALSE)

      datatable <- datatable[, x.list, with = FALSE]

    } else{

    }

    if(!all(keep_columns %in% names(datatable))){

      usethis::ui_warn("All columns specified in keep_columns were not present in the data. Only present columns were retained.")

    } else{

    }

    datatable <- datatable[, keep_columns, with = FALSE]

  }

  # Return data frame
  return(as.data.frame(datatable))

}


#' Extract the client's first CLICC administration.
#'
#' @description The function extracts the client's first CLICC administration. It also allows the programmer to pick specific items within CLICC (e.g., CLICC_01_01, CLICC_01_02).
#'
#' @param data A data frame that contains CLICC data.
#' @param order_by A quoted string to indicate the variable used to determine the order of rows when selecting the first CLICC administration (row). By default, "`Date`".
#' @param keep_all A logical statement to indicate whether all columns should be kept. If `TRUE`, all columns are kept. If `FALSE`, variable names listed in keep_columns are kept. By default, `FALSE`.
#' @param keep_columns A quoted string or list of quoted strings of column CLICC names to retain in the output data frame. By default, `"CLICC"` or all CLICC columns.
#' @param by_year A logical argument to indicate if the first CLICC administration is by year instead of overall. If `TRUE,` the first CLICC administration for each client will be extracted across each data year (e.g., multiple rows may correspond to a unique client across multiple years). If `FALSE`, the first CLICC administration of each client will be selected regardless of year (e.g., each row should correspond to a single unique client). By default, `FALSE`.
#' @param recode_NA A logical argument to indicate if NA on CLICC should be recoded as 0. If `TRUE`, NA will be recoded as 0. By default, `FALSE`.
#'
#' @return A data frame with the client's first CLICC administration.
#'
#' @export

select_first_CLICC <- function(data,
                               order_by = "Date",
                               keep_all = FALSE,
                               keep_columns = "CLICC",
                               by_year = FALSE,
                               recode_NA = FALSE){

  # Addressing "no visible binding for global variable" notes in R CMD check
  Has_CLICC = UniqueClientID = NULL

  # Error messages presented if specific variables are missing
  if(!any(names(data) == order_by)){ # If order_by specified as an argument (quoted) is missing

    stop("The order_by variable is not present in the data.")

  } else if(!"Has_CLICC" %in% names(data)){ #if Has_CLICC is missing

    stop("The Has_CLICC variable is not present in the data.")

  } else if(!any(names(data) == "Data_year") &
            by_year == TRUE){ #if Data_year is missing when by_year = FALSE

    stop("The Data_year variable is not present in the data.")

  } else{

  }

  # Setting data table
  datatable <- data.table::as.data.table(data)

  # Only Valid CLICC
  datatable <- datatable[Has_CLICC == 1,]

  # Order list of variables to be grouped
  orderlist<- c("UniqueClientID", order_by)

  # Order data table by order list
  datatable <- data.table::setorderv(datatable, orderlist)

  # First administration by year
  if(by_year == TRUE){

    datatable <- datatable[, .SD[1], by=.(UniqueClientID, Data_year)]

  } else{

    datatable <- datatable[, .SD[1], by=.(UniqueClientID)]

  }

  # Specify what columns need to be kept
  if(keep_all == TRUE){

    if(keep_columns[1] != "CLICC"){

      usethis::ui_warn("Vector of column names to keep not applicable when keep_all = TRUE. All columns were retained.")

    } else{

    }

  } else{

    if(keep_columns[1] == "CLICC"){

      x.UniqueClientID <- which(colnames(datatable) == "UniqueClientID")
      x.CcmhID <- which(colnames(datatable) == "CcmhID")
      x.Data_year <- which(colnames(datatable) == "Data_year")
      x.CLICC <- grep("CLICC_", names(datatable), fixed = TRUE)
      x.list <- list(x.UniqueClientID, x.CcmhID, x.Data_year, x.CLICC)
      x.list <- unlist(x.list, recursive = FALSE)

      datatable <- datatable[, x.list, with = FALSE]

    } else{

    }

    if(!all(keep_columns %in% names(datatable))){

      usethis::ui_warn("All columns specified in keep_columns were not present in the data. Only present columns were retained.")

    } else{

    }

    datatable <- datatable[, keep_columns, with = FALSE]

  }

  # Recode NA to 0
  if(recode_NA == TRUE){

    datatable[is.na(datatable)] <- 0

  } else{

  }

  # Return as a data frame
  return(as.data.frame(datatable))

}

#' Extract the client's first Case Closure administration.
#'
#' @description The function extracts the client's first Case Closure administration. It also allows the programmer to pick specific items within Case Closure (e.g., CLOSURE_01_101, CLOSURE_01_102).
#'
#' @param data A data frame that contains Case Closure data.
#' @param order_by A quoted string to indicate the variable used to determine the order of rows when selecting the first Case Closure administration (row). By default, `"Date"`.
#' @param keep_all A logical statement to indicate whether all columns should be kept. If `TRUE`, all columns are kept. If `FALSE`, variable names listed in keep_columns are kept. By default, `FALSE`.
#' @param keep_columns A quoted string or list of quoted strings of column Case Closure names to retain in the output data frame. By default, `"Closure"` or all Case Closure columns.
#' @param by_year A logical argument to indicate if the first Case Closure administration is by year instead of overall. If `TRUE,` the first Case Closure administration for each client will be extracted across each data year (e.g., multiple rows may correspond to a unique client across multiple years). If `FALSE`, the first Case Closure administration of each client will be selected regardless of year (e.g., each row should correspond to a single unique client). By default, `FALSE`.
#' @param recode_NA A logical argument to indicate if NA on Case Closure should be recoded as 0. If `TRUE`, NA will be recoded as 0. By default, `FALSE`.
#'
#' @return A data frame with the client's first Case Closure administration.
#'
#' @export

select_first_Closure <- function(data,
                                 order_by = "Date",
                                 keep_all = FALSE,
                                 keep_columns = "Closure",
                                 by_year = FALSE,
                                 recode_NA = FALSE){

  # Addressing "no visible binding for global variable" notes in R CMD check
  Has_Closure = UniqueClientID = NULL

  # Error messages presented if specific variables are missing
  if(!any(names(data) == order_by)){ # If order_by specified as an argument (quoted) is missing

    stop("The order_by variable is not present in the data.")

  } else if(!"Has_Closure" %in% names(data)){ #if Has_Closure is missing

    stop("The Has_Closure variable is not present in the data.")

  } else if(!any(names(data) == "Data_year") &
            by_year == TRUE){ #if Data_year is missing when by_year = FALSE

    stop("The Data_year variable is not present in the data.")

  } else{

  }

  # Setting data table
  datatable <- data.table::as.data.table(data)

  # Only Valid Closure
  datatable <- datatable[Has_Closure == 1,]

  # Order list of variables to be grouped
  orderlist<- c("UniqueClientID", order_by)

  # Order data table by order list
  datatable <- data.table::setorderv(datatable, orderlist)

  # First administration by year
  if(by_year == TRUE){

    datatable <- datatable[, .SD[1], by=.(UniqueClientID, Data_year)]

  } else{

    datatable <- datatable[, .SD[1], by=.(UniqueClientID)]

  }

  # Specify what columns need to be kept
  if(keep_all == TRUE){

    if(keep_columns[1] != "Closure"){

      usethis::ui_warn("Vector of column names to keep not applicable when keep_all = TRUE. All columns were retained.")

    } else{

    }

  } else{

    if(keep_columns[1] == "Closure"){

      x.UniqueClientID <- which(colnames(datatable) == "UniqueClientID")
      x.CcmhID <- which(colnames(datatable) == "CcmhID")
      x.Data_year <- which(colnames(datatable) == "Data_year")
      x.CLOSURE <- grep("CLOSURE_", names(datatable), fixed = TRUE)
      x.list <- list(x.UniqueClientID, x.CcmhID, x.Data_year, x.CLOSURE)
      x.list <- unlist(x.list, recursive = FALSE)

      datatable <- datatable[, x.list, with = FALSE]

    } else{

    }

    if(!all(keep_columns %in% names(datatable))){

      usethis::ui_warn("All columns specified in keep_columns were not present in the data. Only present columns were retained.")

    } else{

    }

    datatable <- datatable[, keep_columns, with = FALSE]

  }

  # Recode NA to 0
  if(recode_NA == TRUE){

    datatable[is.na(datatable)] <- 0

  } else{

  }

  # Return as a data frame
  return(as.data.frame(datatable))

}
