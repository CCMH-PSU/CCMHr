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

  appointment_identifier_temp <- sym(appointment_identifier)

  # TODO: confirm how unquote variable name in dplyr syntax
  appt <- dplyr::filter(data, !is.na(!!appointment_identifier_temp)) # creates a data frame of only appointments
  survey <- dplyr::filter(data, is.na(!!appointment_identifier_temp))

  appt$duplicate <- duplicated(appt[c(client_identifier,appointment_identifier)])

  appt <- dplyr::filter(appt, duplicate == FALSE) %>%
    dplyr::select(-duplicate)

  if ("Date" %in% names(data)) {
    data <- rbind(appt, survey) %>%
      dplyr::arrange(UniqueClientID, Date)
  } else {
    data <- rbind(appt, survey) %>%
      dplyr::arrange(UniqueClientID, !!appointment_identifier_temp)
  }


}
