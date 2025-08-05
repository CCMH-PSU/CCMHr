#' Summarize appointment utilization.
#'
#' @description This function cleans and summarizes data concerning individual and group therapy appointment utilization. The function returns a dataset containing summarized appointment utilization data of each client. The summarization process creates the following new variables: total.scheduled.appts (Total number of scheduled appointments), total.scheduled.ind.appts (Total number of scheduled individual appointments), total.scheduled.group.appts (Total number of scheduled group appointments), total.attended.appts (Total number of attended appointments), total.attended.ind.appts (Total number of attended individual appointments), and total.attended.group.appts (Total number of attended group appointments).
#'
#' @param data A data frame containing information on appointment data.
#' @param by_year A logical argument to indicate whether individual/group therapy appointment utilization should be summarized by year instead of overall. If `TRUE`, the appointment utilization of each client will be summarized by year. If `FALSE`, the appointment utilization of each client will be summarized across all years. By default, `FALSE`.
#' @param recode_NA A logical argument to indicate if NA should be recoded to 0. If `TRUE`, NA will be recoded to 0. By default, `FALSE`.
#'
#' @return A dataset containing details on clients' individual and group therapy appointment utilization. Variables in the data frame include UniqueClientID, CcmhID, Data_year, total.scheduled.appts, total.scheduled.ind.appts, total.scheduled.group.appts, total.attended.appts, total.attended.ind.appts, and total.attended.group.appts.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom dplyr across
#' @importFrom dplyr n
#' @importFrom dplyr starts_with
#'
#' @export

summarize_appointment_utilization <- function(data,
                                              by_year = FALSE,
                                              recode_NA = FALSE) {

  # Check if specified columns are present in the data
  if(!all(c("UniqueClientID", "CcmhID",
            "AppointID", "Data_year",
            "ClientAttendance", "AppointmentCategory",
            "Attended") %in% colnames(data))){

    stop("The data does not contain the required columns: UniqueClientID, CcmhID, AppointID, Data_year, ClientAttendance, AppointmentCategory, Attended.")

  } else{

  }

  # Clean the data
  data <- data |>
    dplyr::filter(Is_appointment == 1) |>
    dplyr::select(UniqueClientID, CcmhID,
                  AppointID, Data_year,
                  ClientAttendance, AppointmentCategory,
                  Attended) |>
    CCMHr::delete_duplicate_appointments() |>
    dplyr::filter(!ClientAttendance %in% c("Scheduled", "Counselor Cancelled",
                                           "Counselor Rescheduled", "Center Closed")) |>
    dplyr::filter(AppointmentCategory %in% c(2,3,13,14,15)) |>
    dplyr::mutate(appts_type = ifelse(AppointmentCategory %in% c(2,3), "Individual", "Group"))

  # By year argument
  if(by_year == TRUE){

    data <- data |>
      dplyr::summarise(total.scheduled.appts = dplyr::n(),
                       total.scheduled.ind.appts = length(which(appts_type == "Individual")),
                       total.scheduled.group.appts = length(which(appts_type == "Group")),
                       total.attended.appts = sum(Attended),
                       total.attended.ind.appts = length(which(appts_type == "Individual" & Attended == 1)),
                       total.attended.group.appts = length(which(appts_type == "Group" & Attended == 1)),
                       .by = c("UniqueClientID", "CcmhID", "Data_year"))
  } else{

    data <- data |>
      dplyr::summarise(total.scheduled.appts = dplyr::n(),
                       total.scheduled.ind.appts = length(which(appts_type == "Individual")),
                       total.scheduled.group.appts = length(which(appts_type == "Group")),
                       total.attended.appts = sum(Attended),
                       total.attended.ind.appts = length(which(appts_type == "Individual" & Attended == 1)),
                       total.attended.group.appts = length(which(appts_type == "Group" & Attended == 1)),
                       .by = c("UniqueClientID", "CcmhID"))

  }

  # Recode NA to 0
  if(recode_NA == TRUE){

    data <- data |>
      dplyr::mutate(dplyr::across(dplyr::starts_with("total"), ~ifelse(.x == 0, NA, .x)))

  } else{

  }

  # Return the cleaned data
  return(data)

}
