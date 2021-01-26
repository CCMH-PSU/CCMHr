# Data cleaning for {{{requester_last_name}}}

# List requested data here

require(tidyverse)

# Load data
data <- CCMHr::loadRDa("{{{cleaning_folder}}}/")

# Remove unnecessary columns
data <- CCMHr::remove_free_response(data)
data <- CCMHr::remove_empty(data)
data <- select(data, -contains("AUDIT"))
data <- select(data, -contains("Dx"))
data <- select(data, -c(ClientID, TherapistAge:Theory_Systems, CcapsVer2012StartDate:CaseNoteID, Enrollment:CCAPSFrequencyX))

# Removing duplicate appointments
data <- CCMHr::delete_duplicate_appointments(data)

# Scramble CcmhID to further deidentify it
data <- mutate(data, CcmhID = as.numeric(as.factor(CcmhID)))

# Save data into the "Data for {{{requester_last_name}}}" folder
write.csv(data, file = here::here("{{{final_folder}}}/"))
