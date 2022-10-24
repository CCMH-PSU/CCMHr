#' Detects and identifies centers with specific patterns of missing data
#'
#' @description Detects and identifies centers with missing data for at least one entire month on specified variables
#' @param data A data file
#' @note Checks for missing data in variables: Is_appointment, Has_CCAPS, Has_SDS, Has_CLICC, Has_Closure,
#' and all above variables at during one month
#' @return A data frame with centers that could have problematic missing data
#' @export

detect_missing_months <- function(data){

  #Check to see if variables are named correctly
    #List of variables required to run function
      var_names <- c("UniqueClientID", "Date", "Is_appointment", "Has_CCAPS",
                     "Has_SDS", "Has_CLICC", "Has_Closure")

    #Running Function to check for missing variables
      required_items(data,
                     var_names)

  #Checking to makes sure date is formatted correctly
    correct.date <- !is.na(lubridate::parse_date_time(data$date,
                                           orders = "ymd"))

  #Error message if data is not formatted correctly
    if(all(correct.date) == F) {
      stop('Date is not formatted correctly. Data needs to be formmated year/month/day.')
    } else {

    }

  #Recode date into a number
    data$month <- lubridate::month(data$Date)
    data$month <- as.numeric(data$month)

  #Grouping and summarizing missing data
    data <- data %>%
      dplyr::group_by(.data$CcmhID, .data$month) %>%
      dplyr::summarize(Is_appointment_sum = sum(.data$Is_appointment,
                                         na.rm = TRUE),
                Has_CCAPS_sum = sum(.data$Has_CCAPS,
                                    na.rm = TRUE),
                Has_SDS_sum = sum(.data$Has_SDS,
                                  na.rm = TRUE),
                Has_CLICC_sum = sum(.data$Has_CLICC,
                                    na.rm = TRUE),
                Has_Closure_sum = sum(.data$Has_Closure,
                                      na.rm = TRUE),
                .groups = "drop") %>%
      dplyr::ungroup()

  #Filling missing months back in with 0s
    data <- tidyr::expand(data, .data$CcmhID, .data$month) %>%
      dplyr::left_join(data) %>%
      dplyr::mutate(dplyr::across(.data$Is_appointment_sum:.data$Has_Closure_sum,
                    ~ifelse(is.na(.x),
                            0,
                            .x)))

  #Determing if data is problematic
    data <- dplyr::group_by(data, .data$CcmhID) %>%
      dplyr::summarize(dplyr::across(.data$Is_appointment_sum:.data$Has_Closure_sum,
                       ~ifelse(max(.x) != 0 & min(.x) == 0,
                               paste0(month[which(.x == 0)],
                                      collapse = ","),
                               NA)))

  #Diplaying only problematic cases in the output
    df.final<- data[c(!is.na(data$Is_appointment_sum) |
                       !is.na(data$Has_CCAPS_sum) |
                       !is.na(data$Has_SDS_sum) |
                       !is.na(data$Has_CLICC_sum)|
                       !is.na(data$Has_Closure_sum)),]

  #return final data frame
    return(df.final)

}
