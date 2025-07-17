#' Identity the primary therapist.
#'
#' @description Clients may have multiple therapists. This function identifies the primary therapist based on a specified threshold of attended appointments. To run the function, "UniqueClientID", "CcmhID", "Date", "Data_year", "AppointID", "TherID", or "ClientAttendance" must be present in the data frame. The function returns a data frame with new variables. The returned data frame would include four CCMH variables (i.e., UniqueClientID, CcmhID, Data_year, TherID) and five new variables related to the primary therapist (i.e., primary.therapist, ther.n, ther.percent, single.therapist, and app.n.attended). If primary.therapist.only equals `TRUE`, only the rows pertaining to the primary therapist are returned.
#'
#' @param data A data frame that contains these variables: UniqueClientID, CcmhID, Date, Data_year, AppointID, TherID, and ClientAttendance.
#' @param percent.threshold A numeric value ranging from 0.50 (50%) to 1 (100%) indicates the percentage of appointments across UniqueClientID, CcmhID, and Data_year required to classify a therapist as the primary therapist. By default, `0.51`.
#' @param primary.therapist.only A logical argument to indicate whether only the primary therapist should be included in the returned data frame. If `TRUE`, only details about the primary therapist are returned. By default, `FALSE`.
#'
#' @return A data frame that contains some CCMH variables (i.e., UniqueClientID, CcmhID, Data_year, TherID) and new variables related to the primary therapist. The new variables include primary.therapist (indicates if the therapist is the primary therapist; 1 = Primary therapist, 0 = Not the primary therapist), ther.n (number of unique therapists), ther.percent (the percent of a client's total appointments conducted by a therapist), single.therapist (indicates if the client had only one therapist; 1 = One therapist, 0 = Multiple therapists), and app.n.attended (number of attended appointments).
#'
#' @export

identity_primary_therapist <- function(data,
                                       percent.threshold = 0.51,
                                       primary.therapist.only = FALSE){

  # Check if UniqueClientID, CcmhID, Date, AppointID, TherID, and ClientAttendance are present
  if(!any(names(data) == "UniqueClientID") |
     !any(names(data) == "CcmhID") |
     !any(names(data) == "Date") |
     !any(names(data) == "Data_year") |
     !any(names(data) == "AppointID")|
     !any(names(data) == "TherID")|
     !any(names(data) == "ClientAttendance")){

    stop("UniqueClientID, CcmhID, Date, Data_year, AppointID, TherID, or ClientAttendance are not present in data")

  } else{

  }

  # Process the data to extract therapist information
  data <- data |>
    dplyr::filter(!is.na(AppointID) &
                  !is.na(TherID) &
                  ClientAttendance == "Attended") |>
    dplyr::mutate(app.n.attended = dplyr::n_distinct(AppointID),
                  .by = c("UniqueClientID", "CcmhID",
                          "Data_year")) |>
    dplyr::mutate(mult.divider = 1/n_distinct(TherID),
                  .by = c("UniqueClientID", "CcmhID",
                          "Data_year", "AppointID")) |> # multiplier divides up appointments with more than one therapist among those therapists
    dplyr::mutate(ther.n = sum(mult.divider),
                  ther.percent = ther.n/app.n.attended,
                  primary.therapist = ifelse(ther.percent <= percent.threshold, 0, 1),
                  single.therapist = ifelse(ther.n == 1, 1, 0),
                  .by = c("UniqueClientID", "CcmhID",
                          "Data_year", "TherID")) |>
    dplyr::select(UniqueClientID, CcmhID,
                  Data_year, TherID,
                  primary.therapist, ther.n,
                  ther.percent, single.therapist,
                  app.n.attended)

  # Unique rows
  data <- unique(data)

  # Return only primary therapist
  if(primary.therapist.only == TRUE){

    data <- data |>
      dplyr::filter(primary.therapist == 1)

  } else{

  }

  # Return the data
  return(data)

}
