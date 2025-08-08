#' Create Data_year variable.
#'
#' @name create_Data_year
#'
#' @description Creates the "Data_year" variable within a data frame. The "Data_year" variable refers to the academic year in which the data was collected. To run this function, the following must be true: a) the "Data_year" variable must be absent from the data frame, b) the "Date" variable must exist in the data frame, c) the "Date" variable must be classed as a date, and d) the "Date" variable must be in the following format "%Y/%m/%d" (ex. "2025-01-20"). To date, the function creates the "Data_year" variable for a date frame with dates between "2010-07-01" and "2035-07-01".
#'
#' @param data  A data frame with a valid Date variable.
#'
#' @return A data frame with a new variable, Data_year.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @importFrom lubridate is.Date
#' @importFrom lubridate year
#'
#' @export
#'

create_Data_year <- function(data){

  # Stop if Data_year already exists
  if("Data_year" %in% colnames(data)){

    stop("Data_year already exists")

  } else{

  }

  # Stop if Date does not exist
  if(!"Date" %in% colnames(data)){

    stop("Variable Date does not exist")

  } else{

  }

  # Stop if Date is not classified as a date
  if(!lubridate::is.Date(data$Date)){

    stop("Variable Date is not classified as a date")

  } else{

  }

  # Stop if Date format is incorrect
    # Function to check if date is in correct format
    IsDate <- function(mydate,
                       date.format = "%Y/%m/%d") {

      tryCatch(!is.na(as.Date(mydate, date.format)),
               error = function(err) {FALSE})

    }

    # Check if Date is correctly formatted
    check.date.list <- IsDate(data$Date)

    # Stop if date is not correctly formatted
    if(any(check.date.list == FALSE)){

        stop("Variable Date is not formatted correctly: %Y/%m/%d")

    } else{

    }

  # Create Data_year
  data <- data |>
    dplyr::mutate(Data_year = dplyr::if_else(month(Date) >= 7, lubridate::year(Date), lubridate::year(Date) - 1))

  # Return data
  return(data)

}

#' @rdname create_Data_year
#' @export
create_data_year <- create_Data_year

#' Format Data_year variable.
#'
#' @name format_Data_year
#'
#' @description Creates different formats of the "Data_year" variable within a data frame. This function will create three new variables within a data frame. The variables include Data_year_long (ex. "2020-2021"), Data_year_formal (ex. "2020-21"), and Data_year_formalshort (ex. "20-21"). These formatted variables are often used at CCMH for axis labels within trend plots. To run this function, a valid "Data_year" variable must exist within the data frame.
#'
#' @param data  A data frame with a valid Data_year variable.
#'
#' @return A data frame with new variables Data_year_long, Data_year_formal, and Data_year_formalshort.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_sub
#'
#' @export
#'

format_Data_year <- function(data){

  # Stop if Data_year does not exist
  if(!"Data_year" %in% colnames(data)){

    stop("Variable Data_year does not exist")

  } else{

  }

  # Stop if Data_year has more than less or more than 4 digits
  if(any(nchar(data$Data_year) != 4)){

    stop("Variable Data_year format is incorrect. Values within the Data_year vector should have 4 digits (for example, 2020)")

  } else{

  }

  # Making Data_year as a numeric
  data$Data_year <- as.numeric(data$Data_year)

  # Adding new Data_year variables
  data <- data |>
    dplyr::mutate(Data_year_long = paste0(Data_year, "-", Data_year + 1),
                  Data_year_formal = paste0(Data_year, "-", stringr::str_sub(Data_year + 1, start = -2)),
                  Data_year_formalshort = paste0(stringr::str_sub(Data_year, start = -2), "-", stringr::str_sub(Data_year + 1, start = -2)))

  # Return data
  return(data)

}

#' @rdname format_Data_year
#' @export
format_data_year <- format_Data_year
