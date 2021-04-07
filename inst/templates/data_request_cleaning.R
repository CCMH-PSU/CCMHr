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
data <- select(data,
               -tidyselect::any_of(c("ClientID", "TherapistAge", "TherAge",
                                   "Enrollment", "Public_Private", "Grading_Scale",
                                   "Athletic_Division", "APA_Accredited_Training",
                                   "IACS_Accredited", "Services_Career",
                                   "Services_Disability", "Services_Drug_Alcohol",
                                   "Services_EAP", "Services_Learning",
                                   "Services_Health", "Services_Testing",
                                   "Services_Other", "Psychiatric_Services",
                                   "Session_Limit", "Session_Limit_Amount",
                                   "ChargeFor_Intake", "ChargeFor_Individual",
                                   "ChargeFor_Group",
                                   "ChargeFor_PsychiatricEvaluation",
                                   "ChargeFor_PsychiatricFollowup",
                                   "ChargeFor_AssessmentPsychological",
                                   "ChargeFor_AssessmentCareer",
                                   "ChargeFor_AssessmentDisability",
                                   "ChargeFor_Other","UserID", "UserAttendance",
                                   "TherGender", "TherEthnicity", "Highest_Degree",
                                   "Highest_Degree_Discipline",
                                   "Highest_Degree_YearReceived",
                                   "Highest_Degree_Licensed", "Year_Licensed",
                                   "Position", "Theory_Analytic", "Theory_Behavioral",
                                   "Theory_Cognitive", "Theory_Humanistic",
                                   "Theory_Systems", "EnrollmentCount", "CCAPSFrequency",
                                   "CCAPSFrequencyX", "CcapsVer2012StartDate",
                                   "CcapsVer2015StartDate", "CenterWideConsent",
                                   "ClientID", "UserID.1", "Year",
                                   "Month", "Day", "OnMultipleSchedules",
                                   "DaysInTreatment", "TreatmentCourse",
                                   "UniquePreviousTherapist", "CaseNoteID",
                                   "HasSurvey", "HasCCAPS", "HasCCAPS34",
                                   "HasCCAPS62", "HasSDS", "HasCLICC",
                                   "HasCLOSURE", "UniqueScheduledTherapist")))

# Removing duplicate appointments
data <- CCMHr::delete_duplicate_appointments(data)

# Scramble CcmhID to further deidentify it
data <- mutate(data, CcmhID = as.numeric(as.factor(CcmhID)))

# Save data into the "Data for {{{requester_last_name}}}" folder
write.csv(data, file = here::here("{{{final_folder}}}/"))
