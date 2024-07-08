#' Identity primary therapist
#'
#' @param data A data file.
#' @param percent.threshold A numeric value ranging between .50 (50%) to 1 (%100) to indicate the percentage of appointments across the UniqueClientID, CcmhID, and Data_year required to classify a therapist as the primary therapist. For example, 0.75 would contend that atleast 75% of appointments should be conducted by a therapist to classify them as the primary therapist. Default is `0.50`.
#' @param primary.therapist.only A logical statement to indicate if only the primary therapist should be returned. Default is `FALSE`.
#'
#' @export
#'
#' 

identity_primary_therapist <- function(data, 
                                       percent.threshold = 0.50, 
                                       primary.therapist.only = FALSE){

    # Check if UniqueClientID, CcmhID, Date, AppointID, TherID, and ClientAttendance are present
    if (!any(names(data) == "UniqueClientID") | 
        !any(names(data) == "CcmhID") | 
        !any(names(data) == "Date") | 
        !any(names(data) == "Data_year") | 
        !any(names(data) == "AppointID")| 
        !any(names(data) == "TherID")| 
        !any(names(data) == "ClientAttendance")) {
      stop("UniqueClientID, CcmhID, Date, Data_year, AppointID, TherID, or ClientAttendance are not present in data")
    }

    # Clean data
    data <- data %>%
        dplyr::filter(!is.na(AppointID) & 
                      !is.na(TherID) & 
                      ClientAttendance == "Attended") %>%
        dplyr::mutate(app.n.attended = dplyr::n_distinct(AppointID), 
                      .by = c("UniqueClientID", "CcmhID", 
                              "Data_year")) %>%
        dplyr::mutate(mult.divider = 1/n_distinct(TherID), 
                      .by = c("UniqueClientID", "CcmhID", 
                              "Data_year", "AppointID")) %>% # multiplier divides up appointments with more than one therapist among those therapists
        dplyr::mutate(ther.n = sum(mult.divider), 
                      ther.percent = ther.n/app.n.attended, 
                      primary.therapist = ifelse(ther.percent <= percent.threshold, 0, 1),
                      single.therapist = ifelse(ther.n == 1, 1, 0),
                      .by = c("UniqueClientID", "CcmhID", 
                              "Data_year", "TherID")) %>%
        dplyr::select(UniqueClientID, CcmhID, 
                      Data_year, TherID, 
                      primary.therapist,
                      ther.n, ther.percent, 
                      single.therapist, app.n.attended)

    # Unique rows
    data <- unique(data)

    # return only primary therapist
    if (primary.therapist.only == TRUE) {
        data <- data %>%
            dplyr::filter(primary.therapist == 1)
    } else {
      # Skip
    }

    # Return the data
    return(data)

}