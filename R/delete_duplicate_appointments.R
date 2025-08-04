#' Delete duplicate appointments.
#'
#' @description For reasons not discussed here, data on appointments may have duplicates (e.g., multiple rows with the same type of data). This function removes duplicate data from appointments.
#'
#' @param data A data frame containing rows related to appointments.
#' @param client_identifier A quoted string to indicate the column uniquely identifying each client. By default, `"UniqueClientID"`.
#' @param appointment_identifier A quoted string to indicate the column uniquely identifying each appointment. By default, `AppointID`. If `"AppointID"` is not available, use `"Date"` instead.
#'
#' @return A data frame with only the first instance of each appointment.
#'
#' @importFrom glue glue
#' @importFrom rlang sym
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#'
#' @export

delete_duplicate_appointments <- function(data,
                                          client_identifier = "UniqueClientID",
                                          appointment_identifier = "AppointID") {

  # Indicate if the client_identifier is present in the data
  if (!client_identifier %in% names(data)){

    stop(glue::glue("client_identifier ({client_identifier}) not present in the data."))

  } else{

  }

  # Indicate if the appointment_identifier is present in the data
  if(!appointment_identifier %in% names(data)){

    stop(glue::glue("appointment_identifier ({appointment_identifier}) not present in the data."))

  } else{

  }

  # Convert the appointment_identifier to a symbol
  appointment_identifier_temp <- rlang::sym(appointment_identifier)

  # Creates a data frame of only appointments
  appt <- dplyr::filter(data, !is.na(!!appointment_identifier_temp))

  # Creates a data frame of only survey
  survey <- dplyr::filter(data, is.na(!!appointment_identifier_temp))

  # Detect duplicates
  appt$duplicate <- duplicated(appt[c(client_identifier,appointment_identifier)])

  # Remove duplicates
  appt <- appt |>
    dplyr::filter(duplicate == FALSE) |>
    dplyr::select(-duplicate)

  # Recombine the data frames and arrange by client, date, or appointment
  if ("Date" %in% names(data)) {

    data <- rbind(appt, survey) |>
      dplyr::arrange(UniqueClientID, Date)

  } else {

    data <- rbind(appt, survey) |>
      dplyr::arrange(UniqueClientID, !!appointment_identifier_temp)

  }

  # Return data
  return(data)

}
