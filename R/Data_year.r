#' Create Data_year variable
#'
#' @param data  A data file.
#'
#' @return A data frame with a new variable Data_year.
#' @export
#'
#' 

create_Data_year <- function(data){

    # Stop if Data_year already exists
    if("Data_year" %in% colnames(data)){
        stop("Data_year already exists")
    }

    # Stop if Date does not exist
    if(!"Date" %in% colnames(data)){
        stop("Variable Date does not exist")
    }

    # Stop if Date is not classified as a date
    if(!is.Date(data$Date)){
        stop("Variable Date is not classified as a date")
    }

    # Stop if Date format is incorrect
        # Function to check if date is in correct format
        IsDate <- function(mydate, date.format = "%Y/%m/%d") {
            tryCatch(!is.na(as.Date(mydate, date.format)),  
                    error = function(err) {FALSE})  
        }

        # Check if Date is in correct format
        check.date.list <- IsDate(data$Date)

    if(any(check.date.list == FALSE)){
        stop("Variable Date is not formatted correctly: %Y/%m/%d")
    }

    # Create Data_year
    data <- data %>%
        dplyr::mutate(Data_year = case_when(Date >= "2035-07-01" ~ 2035,
                                            Date >= "2034-07-01" ~ 2034,
                                            Date >= "2033-07-01" ~ 2033,
                                            Date >= "2032-07-01" ~ 2032,
                                            Date >= "2031-07-01" ~ 2031,
                                            Date >= "2030-07-01" ~ 2030,
                                            Date >= "2029-07-01" ~ 2029,
                                            Date >= "2028-07-01" ~ 2028,
                                            Date >= "2027-07-01" ~ 2027,
                                            Date >= "2026-07-01" ~ 2026,
                                            Date >= "2025-07-01" ~ 2025,
                                            Date >= "2024-07-01" ~ 2024,
                                            Date >= "2023-07-01" ~ 2023,
                                            Date >= "2022-07-01" ~ 2022,
                                            Date >= "2021-07-01" ~ 2021,
                                            Date >= "2020-07-01" ~ 2020,
                                            Date >= "2019-07-01" ~ 2019,
                                            Date >= "2018-07-01" ~ 2018,
                                            Date >= "2017-07-01" ~ 2017,
                                            Date >= "2016-07-01" ~ 2016,
                                            Date >= "2015-07-01" ~ 2015,
                                            Date >= "2014-07-01" ~ 2014,
                                            Date >= "2013-07-01" ~ 2013,
                                            Date >= "2012-07-01" ~ 2012,
                                            Date >= "2011-07-01" ~ 2011,
                                            Date >= "2010-07-01" ~ 2010))

    return(data)

}

#' Format Data_year variable
#'
#' @param data  A data file.
#'
#' @return A data frame with new variables: Data_year_long, Data_year_formal, and Data_year_formalshort.
#' @export
#'

format_Data_year <- function(data){

    # Stop if Data_year does not exist
    if(!"Data_year" %in% colnames(data)){
        stop("Variable Data_year does not exist")
    }

    # Stop if Data_year has more than less or more than 4 digits
    if(any(nchar(data$Data_year) != 4)){
        stop("Variable Data_year format is incorrect. Values within the Data_year vector should have 4 digits (for example, 2021)")
    }

    # Making Data_year as a numberic 
    data$Data_year <- as.numeric(data$Data_year)

    # Adding new Data_year variables
    data <- data %>%
        dplyr::mutate(Data_year_long = paste0(Data_year, "-", Data_year + 1),
                      Data_year_formal = paste0(Data_year, "-", stringr::str_sub(Data_year + 1, start = -2)),
                      Data_year_formalshort = paste0(stringr::str_sub(Data_year, start = -2), "-", stringr::str_sub(Data_year + 1, start = -2)))

    # Return data
    return(data)

}