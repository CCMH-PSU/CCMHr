#' Filter rows and select columns specific to the chosen data years and forms.
#'
#' @name filter_select
#'
#' @description Filter rows to include only rows about specified data years and forms, and select only columns relevant to those years and forms. This simplifies and reduces the size of large datasets by removing unnecessary data.
#'
#' @param data A data frame or arrow table that contains CCMH data.
#' @param data.year A numeric value or list of numeric values that specifies the data years needed in the returned data frame. If `NULL`, all data years are included. By default, `NULL`.
#' @param forms A string or list of strings that specifies the forms needed in the returned data frame. Options include: `"ccaps"`, `"sds"`, `"clicc"`, `"closure"`, `"appointment"`, `"audit"`, `"provider"`, `"dx"`, `NULL`. If `NULL`, all forms are included. By default, `NULL`.
#'
#' @return Outputs a data frame with rows and columns about specified data years and forms. Please note that if the input is an arrow table, the output will be a data frame.
#' 
#' @export
#'

filter_select <- function(data,
                          data.year = NULL,
                          forms = NULL) {
  
  # Check for necessary criteria
    # Check if inputs on data year is numberic or list of numbers
    if (!is.null(data.year)){

      if (!is.numeric(data.year) & !is.list(data.year)) {

        stop("data.year must be numeric or a list of numbers.")

      } else {

      }

    } else {

    }
  
    # Check if Data_year column exists in data
    if (!is.null(data.year)) {

      if (!"Data_year" %in% colnames(data)) {

        stop("The data frame or arrow table must contain the column 'Data_year'.")

      } else {

      }

    } else {

    }
  
    # Check if data.year values exist in Data_year column
    if (!is.null(data.year)) {

      # Data processing 
      data.year.from.data <- data |>
        dplyr::select(Data_year) |>
        dplyr::distinct() |>
        dplyr::collect()

      requested_years <- sort(unique(as.integer(unlist(data.year))))
      available_years <- sort(unique(as.integer(data.year.from.data$Data_year)))
      missing_years <- setdiff(requested_years, available_years)

      # Message
      if (length(missing_years) > 0) {

        stop(sprintf("The following data.year values are not present in Data_year: %s",
                     paste(missing_years, collapse = ", ")))
        
      } else {

      }

    } else {

    }

    # Check inputs on forms
    if (!is.null(forms)) {

      # List valid forms 
      valid_forms <- c("CCAPS", "SDS", 
                       "CLICC", "Closure", 
                       "Appointment", "AUDIT", 
                       "Dx", "Provider")
      
      # Also include lower and upper case 
      valid_forms <- c(tolower(valid_forms))

      # Check
      if (!all(tolower(unlist(forms)) %in% valid_forms)) {

        stop(sprintf("forms must be one or more of the following: %s",
                     paste(valid_forms, collapse = ", ")))

      } else {

      }

    }
  
  # Filter rows based on data.year
  if (!is.null(data.year)) {

    data <- data |>
      dplyr::filter(Data_year %in% unlist(data.year))

  } else {

  }

  # Columns to keep 
    # across all froms 
    columns_all <- c("UniqueClientID", "CcmhID", 
                     "AppointID", "Date", 
                     "ClientID", "CaseNoteID", 
                     "TherID", "Data_year",
                     "CenterWideConsent", "RecordType", 
                     "UserID", "LinkedAppt")
  
    # CCAPS
    columns_ccaps <- c("Has_CCAPS", "Is_CCAPS62", 
                       "Is_CCAPS34", "Is_ValidCCAPS", 
                       "Is_ValidCCAPS62", "Is_ValidCCAPS34", 
                       "Depression34", "Anxiety34", 
                       "Social_Anxiety34", "Academics34",
                       "Eating34", "Hostility34",
                       "Alcohol34", "DI", 
                       "Depression62", "Eating62", 
                       "Substance62", "Anxiety62", 
                       "Hostility62", "Social_Anxiety62", 
                       "Family62", "Academics62", 
                       "Seq_ValidCCAPS", "Seq_ValidCCAPS34", 
                       "Seq_ValidCCAPS62", "CcapsVer2012StartDate", 
                       "CcapsVer2015StartDate", "CcapsVer2018StartDate", 
                       "CcapsVer2019StartDate")
  
    # SDS
    columns_sds <- c("Has_SDS", "ClientAge", 
                     "Seq_SDS")
  
    # CLICC
    columns_clicc <- c("Has_CLICC", "Seq_CLICC")
  
    # Closure
    columns_closure <- c("Has_Closure")
  
    # Appointment
    columns_appointment <- c("Is_appointment", "ClientAttendance", 
                             "AppointmentCategory", "Attended", 
                             "UserAttendance", "Length", 
                             "DaysSinceScheduled", "ModeId")
  
    # AUDIT
    columns_audit <- c("Has_Audit")
  
    # Dx
    columns_dx <- c("Has_Dx")
  
    # Provider
    columns_provider <- c("TherapistAge", "Gender", 
                          "SexualOrientation", "Ethnicity",
                          "Ethnicity_Other", "Ethnicity_More", 
                          "CountryOfOrigin", "RelationshipStatus", 
                          "Religion", "Religion_Other", 
                          "Religion_Importance", "Highest_Degree", 
                          "Highest_Degree_Other", "Highest_Degree_Discipline", 
                          "Highest_Degree_Discipline_Other", "Highest_Degree_YearReceived", 
                          "Highest_Degree_Licensed", "Year_Licensed", 
                          "Position", "Position_Other", 
                          "Theory_Analytic", "Theory_Behavioral", 
                          "Theory_Cognitive", "Theory_Humanistic", 
                          "Theory_Systems", "TherAge", 
                          "TherGender", "TherEthnicity", 
                          "Position", "Position_Other")

  # Filter rows and select columns based on forms 
  if (!is.null(forms)) {

    # CCAPS
    if (tolower("ccaps") %in% tolower(unlist(forms))) {

      # detect if Has_CCAPS column exists
      if (!"Has_CCAPS" %in% colnames(data)) {

        stop("The data frame or arrow table must contain the column 'Has_CCAPS' to filter for CCAPS form.")

      } else {

      }

      data.ccaps <- data |>
        dplyr::filter(Has_CCAPS == 1) |>
        dplyr::select(c(dplyr::any_of(columns_all), 
                        dplyr::any_of(columns_ccaps),
                        dplyr::starts_with("CCAPS_"))) |>
        dplyr::collect()

    } else {

    }

    # SDS
    if (tolower("sds") %in% tolower(unlist(forms))) {

      # detect if Has_SDS column exists
      if (!"Has_SDS" %in% colnames(data)) {

        stop("The data frame or arrow table must contain the column 'Has_SDS' to filter for SDS form.")

      } else {

      }

      data.sds <- data |>
        dplyr::filter(Has_SDS == 1) |>
        dplyr::select(c(dplyr::any_of(columns_all), 
                        dplyr::any_of(columns_sds),
                        dplyr::starts_with("SDS_"))) |>
        dplyr::collect()

    } else {

    }

    # CLICC
    if (tolower("clicc") %in% tolower(unlist(forms))) {

      # detect if Has_CLICC column exists
      if (!"Has_CLICC" %in% colnames(data)) {

        stop("The data frame or arrow table must contain the column 'Has_CLICC' to filter for CLICC form.")

      } else {

      }

      data.clicc <- data |>
        dplyr::filter(Has_CLICC == 1) |>
        dplyr::select(c(dplyr::any_of(columns_all), 
                        dplyr::any_of(columns_clicc),
                        dplyr::starts_with("CLICC_"))) |>
        dplyr::collect()

    } else {

    }

    # Closure
    if (tolower("closure") %in% tolower(unlist(forms))) {

      # detect if Has_Closure column exists
      if (!"Has_Closure" %in% colnames(data)) {

        stop("The data frame or arrow table must contain the column 'Has_Closure' to filter for Closure form.")

      } else {

      }

      data.closure <- data |>
        dplyr::filter(Has_Closure == 1) |>
        dplyr::select(c(dplyr::any_of(columns_all), 
                        dplyr::any_of(columns_closure),
                        dplyr::starts_with("Closure_"))) |>
        dplyr::collect()

    } else {

    }

    # Appointment
    if (tolower("appointment") %in% tolower(unlist(forms))) {

      # detect if Is_appointment column exists
      if (!"Is_appointment" %in% colnames(data)) {

        stop("The data frame or arrow table must contain the column 'Is_appointment' to filter for Appointment form.")

      } else {

      }

      data.app <- data |>
        dplyr::filter(Is_appointment == 1) |>
        dplyr::select(c(dplyr::any_of(columns_all), 
                        dplyr::any_of(columns_appointment))) |>
        dplyr::collect()

    } else {

    }

    # AUDIT
    if (tolower("audit") %in% tolower(unlist(forms))) {

      # detect if Has_Audit column exists
      if (!"Has_Audit" %in% colnames(data)) {

        stop("The data frame or arrow table must contain the column 'Has_Audit' to filter for AUDIT form.")

      } else {

      }

      data.audit <- data |>
        dplyr::filter(Has_Audit == 1) |>
        dplyr::select(c(dplyr::any_of(columns_all), 
                        dplyr::any_of(columns_audit),
                        dplyr::starts_with("AUDIT_"))) |>
        dplyr::collect()

    } else {

    }

    # Dx
    if (tolower("dx") %in% tolower(unlist(forms))) {

      # detect if Has_Dx column exists
      if (!"Has_Dx" %in% colnames(data)) {

        stop("The data frame or arrow table must contain the column 'Has_Dx' to filter for Dx form.")

      } else {

      }

      data.dx <- data |>
        dplyr::filter(Has_Dx == 1) |>
        dplyr::select(c(dplyr::any_of(columns_all), 
                        dplyr::any_of(columns_dx),
                        dplyr::starts_with("Dx_"))) |>
        dplyr::collect()

    } else {

    }

    # Provider
    if (tolower("provider") %in% tolower(unlist(forms))) {

      data.provider <- data |>
        dplyr::select(c(dplyr::any_of(columns_all), 
                        dplyr::any_of(columns_provider),
                        dplyr::starts_with("Provider_"))) |>
        dplyr::collect()

    } else {

      
    }

  } else {

  }

  # Combine data for selected forms
  if(!is.null(forms)){

    data.merged <- 
      dplyr::bind_rows(if (exists("data.ccaps")) data.ccaps,
                      if (exists("data.sds")) data.sds,
                      if (exists("data.clicc")) data.clicc,
                      if (exists("data.closure")) data.closure,
                      if (exists("data.app")) data.app,
                      if (exists("data.audit")) data.audit,
                      if (exists("data.dx")) data.dx,
                      if (exists("data.provider")) data.provider) |>
      dplyr::arrange(UniqueClientID, CcmhID, Date)

  } else{

    data.merged <- data |>
      dplyr::collect() |>
      dplyr::arrange(UniqueClientID, CcmhID, Date)

  }

  # Return data as data frame
  return(data.merged)

}