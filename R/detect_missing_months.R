#' Identifies centers with monthly missing data across forms.
#'
#' @description Centers may not contribute data on forms for each month within a data year. This function identifies centers that had missing data for at least one entire month on specified forms.
#'
#' @param data A data frame that contains the following variables: "UniqueClientID", "Date", "Is_appointment", "Has_CCAPS", "Has_SDS", "Has_CLICC", and "Has_Closure".
#'
#' @return A data frame with the following variables: "CcmhID", "Is_appointment_sum", "Has_CCAPS_sum", "Has_SDS_sum", "Has_CLICC_sum", "Has_Closure_sum". Variables that end with "_sum" output the month(s) that had completely missing data on a specified form. Months are specified numerically (e.g., January is 1).
#'
#' @importFrom lubridate parse_date_time
#' @importFrom lubridate month
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr across
#' @importFrom dplyr ungroup
#' @importFrom tidyr expand
#'
#' @export

detect_missing_months <- function(data){

  # Check to see if variables are named correctly
  var_names <- c("UniqueClientID", "Date",
                 "Is_appointment", "Has_CCAPS",
                 "Has_SDS", "Has_CLICC",
                 "Has_Closure")

  CCMHr::required_items(data, var_names)

  # Checking if date is formatted correctly
  correct.date <- !is.na(lubridate::parse_date_time(data$date, orders = "ymd"))

  if(all(correct.date) == FALSE){

      stop('Date is not formatted correctly. Date needs to be formated year/month/day.')

  } else {

  }

  # Recode date into a number
  data$month <- lubridate::month(data$Date)
  data$month <- as.numeric(data$month)

  # Summarizing missing data
  data <- data |>
    dplyr::group_by(CcmhID, month) |>
    dplyr::summarize(Is_appointment_sum = sum(Is_appointment, na.rm = TRUE),
                     Has_CCAPS_sum = sum(Has_CCAPS, na.rm = TRUE),
                     Has_SDS_sum = sum(Has_SDS, na.rm = TRUE),
                     Has_CLICC_sum = sum(Has_CLICC, na.rm = TRUE),
                     Has_Closure_sum = sum(Has_Closure, na.rm = TRUE),
                     .groups = "drop") |>
      dplyr::ungroup()

  # Filling missing months back in with 0s
  data <- tidyr::expand(data, CcmhID, month) |>
    dplyr::left_join(data) |>
    dplyr::mutate(dplyr::across(Is_appointment_sum:Has_Closure_sum, ~ifelse(is.na(.x), 0, .x)))

  # Determining if data is problematic
  data <- data |>
    dplyr::group_by(CcmhID) |>
    dplyr::summarize(dplyr::across(Is_appointment_sum:Has_Closure_sum, ~ifelse(max(.x) != 0 & min(.x) == 0, paste0(month[which(.x == 0)], collapse = ","), NA)))

  # Displaying only problematic cases in the output
  df.final <- data[c(!is.na(data$Is_appointment_sum) |
                     !is.na(data$Has_CCAPS_sum) |
                     !is.na(data$Has_SDS_sum) |
                     !is.na(data$Has_CLICC_sum)|
                     !is.na(data$Has_Closure_sum)),]

  # Return final data frame
  df.final <- as.data.frame(df.final)

  return(df.final)

}
