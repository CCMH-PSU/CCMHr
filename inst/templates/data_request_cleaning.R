# Data cleaning for {{{requester_name}}}

# List requested data here

require(tidyverse)

# Load data
data <- CCMHr::loadRDa("")

# Remove unnecessary columns
data <- CCMHr::remove_free_response(data)
data <- CCMHr::remove_empty(data)
data <- select(data, -contains("AUDIT"))
data <- select(data, -contains("Dx"))
data <- select(data, -c(ClientID, TherapistAge:Theory_Systems, CcapsVer2012StartDate:CaseNoteID, Enrollment:CCAPSFrequencyX))

# Removing duplicate appointments
appt <- dplyr::filter(data, !is.na(AppointID))
survey <- dplyr::filter(data, is.na(AppointID))

appt$duplicate <- duplicated(appt[c("UniqueClientID","AppointID")])

appt <- dplyr::filter(appt, duplicate == FALSE) %>%
  dplyr::select(-duplicate)

data <- rbind(appt, survey) %>%
  dplyr::arrange(UniqueClientID, Date)

# Save data into the "Data for {{{requester_name}}}" folder
write.csv(data, file = "")
