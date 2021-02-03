#' Delete Duplicate Appointments
#'
#' @param data A data frame with surveys and appointments.
#' @param client_identifier The column uniquely identifying each client. By default, `UniqueClientID`.
#' @param appointment_identifier The column uniquely identifying each appointment. By default `AppointID`, but if this is not available, use `Date` instead.
#'
#' @return A data frame with only the first instance of each duplicated appointment.
#' @export
#'
delete_duplicate_appointments <- function(data, client_identifier = "UniqueClientID", appointment_identifier = "AppointID") {

  if (!client_identifier %in% names(data)) {
    stop(glue::glue("client_identifier ({client_identifier}) not present in the data."))
  }

  if(!appointment_identifier %in% names(data)) {
    stop(glue::glue("appointment_identifier ({appointment_identifier}) not present in the data."))
  }

  appointment_identifier_temp <- rlang::sym(appointment_identifier)

  appt <- dplyr::filter(data, !is.na(!!appointment_identifier_temp)) # creates a data frame of only appointments
  survey <- dplyr::filter(data, is.na(!!appointment_identifier_temp))

  appt$duplicate <- duplicated(appt[c(client_identifier,appointment_identifier)])

  appt <- dplyr::filter(appt, .data$duplicate == FALSE) %>%
    dplyr::select(-.data$duplicate)

  if ("Date" %in% names(data)) {
    data <- rbind(appt, survey) %>%
      dplyr::arrange(.data$UniqueClientID, .data$Date)
  } else {
    data <- rbind(appt, survey) %>%
      dplyr::arrange(.data$UniqueClientID, !!appointment_identifier_temp)
  }

  data


}
