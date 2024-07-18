#' Summarize appointment utilization
#'
#' @description Clean and summarize individual and group therapy appointment utilization
#' @param data A data file containing information on individual and group therapy appointments.
#' @param by_year A logical statement to indicate if appointment utilization should be summarized by year instead of overall. If `TRUE`, the appointment utilization of each client will be summarized by year. If `FALSE`, the appointment utilization of each client will be summarized across years. Default = `FALSE`.
#' @param recode_NA A logical statement to indicate if NA should be recoded to 0. If `TRUE`, NA will be recoded to 0. Default = `FALSE`.
#' @return A data frame with cleaning appointment utilization data.
#' @export
#' 

summarize_appointment_utilization <- function(data, 
                                              by_year = FALSE, 
                                              recode_NA = FALSE) {

    # Check if specified columns are present in the data
    if (!all(c("UniqueClientID", "CcmhID", 
               "AppointID", "Data_year", 
               "ClientAttendance", "AppointmentCategory", 
               "Attended") %in% colnames(data))) {
        stop("The data does not contain the required columns: UniqueClientID, CcmhID, AppointID, Data_year, ClientAttendance, AppointmentCategory, Attended.")
    }

    # Clean the data
    data <- data %>%
        dplyr::filter(Is_appointment == 1) %>% 
        dplyr::select(UniqueClientID, CcmhID, 
                      AppointID, Data_year, 
                      ClientAttendance, AppointmentCategory, 
                      Attended) %>%
        CCMHr::delete_duplicate_appointments() %>% 
        dplyr::filter(!ClientAttendance %in% c("Scheduled", "Counselor Cancelled", 
                                                "Counselor Rescheduled", "Center Closed")) %>% 
        dplyr::filter(AppointmentCategory %in% c(2,3,13,14,15)) %>% 
        dplyr::mutate(appts_type = ifelse(AppointmentCategory %in% c(2,3), "Individual", "Group"))
        
    # By year
    if(by_year == TRUE) {
        data <- data %>%
            dplyr::summarise(total.scheduled.appts = dplyr::n(),
                             total.scheduled.ind.appts = length(which(appts_type == "Individual")),
                             total.scheduled.group.appts = length(which(appts_type == "Group")),
                             total.attended.appts = sum(Attended), 
                             total.attended.ind.appts = length(which(appts_type == "Individual" & Attended == 1)),
                             total.attended.group.appts = length(which(appts_type == "Group" & Attended == 1)),
                             .by = c("UniqueClientID", "CcmhID", "Data_year"))
    } else {
        data <- data %>%
            dplyr::summarise(total.scheduled.appts = dplyr::n(),
                             total.scheduled.ind.appts = length(which(appts_type == "Individual")),
                             total.scheduled.group.appts = length(which(appts_type == "Group")),
                             total.attended.appts = sum(Attended), 
                             total.attended.ind.appts = length(which(appts_type == "Individual" & Attended == 1)),
                             total.attended.group.appts = length(which(appts_type == "Group" & Attended == 1)),
                             .by = c("UniqueClientID", "CcmhID"))
    }

    # Recode NA to 0
    if(recode_NA == TRUE) {
        data <- data %>%
            dplyr::mutate(dplyr::across(dplyr::starts_with("total"), ~ifelse(.x == 0, NA, .x)))
    } else {
       # Skip
    }

  # Return the cleaned data
  return(data)

}