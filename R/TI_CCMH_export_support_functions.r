#' Cleans a Titanium (TI) appointment CCMH export file.
#'
#' @description This function cleans a TI appointment CCMH export file. The following cleaning procedures are conducted: a) rename variables (based on the key) to allow the data to be used with other CCMHr functions, b) format date, c) delete duplicate appointments, and d) return/save data frame.
#'
#' @param data A data frame of a Titanium (TI) appointment CCMH export file.
#' @param key A data frame with two columns (i.e., orginal_name, new_name) to rename the columns of the raw data. By default, `CCMHr::TI_CCMH_export_appointment_key`.
#' @param save A logical argument to indicate whether the data frame should be saved as a file under a local folder. If `TRUE`, the data frame would be saved as specified in the path argument. If `FALSE`, the data will be returned as an object. By default, `FALSE`.
#' @param path A quoted string to indicate the file type (e.g., CSV, RDA), file name, and the path within the local directory where the appointment data should be saved. By default, `"TI_appointment_clean.csv"`.
#'
#' @return A data frame with cleaned TI exported appointment data.
#'
#' @importFrom dplyr rename_with
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @export

clean_TI_export_appointment <- function(data,
                                        key = CCMHr::TI_CCMH_export_appointment_key,
                                        save = FALSE,
                                        path = "TI_appointment_clean.csv"){

  # Rename columns based on key
  data <- data |>
    dplyr::rename_with(~ key$new_name, key$orginal_name) |>
    dplyr::mutate(UniqueClientID = ClientID)

  # Format date variable
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

  # Delete duplicate appointments
  data <- CCMHr::delete_duplicate_appointments(data)

  # Exclude UniqueClientID variable
  data <- data |>
    dplyr::select(-UniqueClientID)

  # Save file
  if(save == TRUE){

    write.csv(data,
              file = path,
              row.names = FALSE)

  } else{

    return(data)

  }

}


#' Cleans a Titanium (TI) CCMH forms export file.
#'
#' @description This function cleans a TI CCMH forms export file. The following cleaning procedures are conducted: a) rename variables (based on the key) to allow the data to be used with other CCMHr functions, b) format date, c) recode TI-specific responses "<No Response>", d) clean/score CCAPS, e) clean SDS, f) clean CLICC, g) clean Case Closure, h) clean AUDIT, and i) return/save data frame.
#'
#' @param data A data frame from a Titanium (TI) CCMH forms export file.
#' @param id_var A quoted string or list of quoted strings to indicate the name of the ID variable(s) outside of "ClientID" if it/they exist. If `NULL`, there are no other ID variables. By default, `NULL`.
#' @param key A key to rename the columns in the data file. By default, `CCMHr::TI_CCMH_export_forms_key`.
#' @param included_forms A quoted string or list of quoted strings that indicates which forms should be cleaned and included in the data. Options include: "CCAPS", "SDS", "CLICC", "Closure", and "AUDIT". By default, `c("CCAPS", "SDS", "CLICC", "Closure", "AUDIT")`.
#' @param save_individual_form A logical argument to indicate whether the individual forms (e.g., "CCAPS", "SDS") should be saved in separate files under a local folder. If `TRUE`, each form is saved in a separate file under a local folder. If `FALSE`, the forms will be merged and saved under a single data frame. By default, `FALSE`.
#' @param path_individual_form A quoted string to indicate the path where the individual forms should be saved. If `NULL`, each form is saved under the current working directory. By default, `NULL`.
#' @param save A logical argument to indicate whether the data frame should be saved as a file under a local folder. If `TRUE`, the data frame would be saved as specified in the path argument. If `FALSE`, the data will be returned as an object. By default, `FALSE`.
#' @param path A quoted string to indicate the file type (e.g., CSV, RDA), file name, and the path within the local directory where data from the forms should be saved. By default, `"TI_CCMH_export_clean.csv"`.
#'
#' @return A data frame with cleaned TI CCMH exported forms data.
#'
#' @importFrom dplyr rename_with
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom dplyr all_of
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr arrange
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @importFrom dplyr everything
#' @importFrom dplyr contains
#' @importFrom dplyr vars
#' @importFrom dplyr mutate_at
#' @importFrom dplyr if_any
#' @importFrom plyr rbind.fill
#'
#' @export

clean_TI_CCMH_export <- function(data,
                                 id_var = NULL,
                                 key = CCMHr::TI_CCMH_export_forms_key,
                                 included_forms = c("CCAPS", "SDS", "CLICC", "Closure", "AUDIT"),
                                 save_individual_form = FALSE,
                                 path_individual_form = NULL,
                                 save = FALSE,
                                 path = "TI_CCMH_export_clean.csv"){

  # Remove the "/" symbol at the end of the path if it exists
  if(substr(path_individual_form, nchar(path_individual_form), nchar(path_individual_form)) == "/"){

    path_individual_form <- substr(path_individual_form, 1, nchar(path_individual_form) - 1)

  } else{

  }

  # Detect if the path_individual_form exists
  if(save_individual_form == TRUE &
     !is.null(path_individual_form)){

    path.individual.form.exist <- dir.exists(paste0(path_individual_form))

    # Stop function if the path does not exist
    if(path.individual.form.exist == FALSE){

      stop("The path_individual_form folder does not exist")

    } else{

    }

  } else{

  }

  # Clean data forms

    # Rename ID variables
    data <- data |>
      dplyr::rename("ClientID" = "clientid",
                    "CaseNoteID" = "casenoteid",
                    "RecordType" = "recordtype",
                    "Date" = "notedate",
                    "NoteType" = "notetype")

    # Extract the names of the data frame
    list.of.df.names <- names(data)

    # Filter key based on the names of the data frame
    key <- key |>
      dplyr::filter(standard_var_name %in% list.of.df.names)

    # Rename data frame based on key
    data <- data |>
      dplyr::rename_with(~ key$ccmh_var_name, key$standard_var_name)

    # Recoding "<No Response>" to NA
    data[] <- lapply(data,
                     function(x){

      x[x == "<No Response>"] <- NA

      return(x)

    })

    # Recoding "" to NA
    data[] <- lapply(data,
                     function(x){

      x[x == ""] <- NA

      return(x)

    })

    # Format date
    data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

    # Create empty data frame
    df.combine <- data.frame()

    # Process and select all unique ID variable combinations
    if(!is.null(id_var)){

      df.id <- data |>
        dplyr::select(ClientID, dplyr::all_of(id_var)) |>
        unique()

    } else{

    }

    # Clean CCAPS
    if("CCAPS" %in% included_forms){

      # Cleaning CCAPS62 administrations
      df.ccaps.62 <- data |>
        dplyr::select(ClientID, CaseNoteID,
                      RecordType, Date,
                      NoteType, dplyr::starts_with("CCAPS62"))

      # Renaming CCAPS62 variables
      names(df.ccaps.62) <- c('ClientID', 'CaseNoteID', 'RecordType', 'Date',
                              'NoteType', 'CCAPS_01',	'CCAPS_03', 'CCAPS_04',
                              'CCAPS_05',	'CCAPS_06',	'CCAPS_08', 'CCAPS_09',
                              'CCAPS_10',	'CCAPS_11',	'CCAPS_13', 'CCAPS_14',
                              'CCAPS_15',	'CCAPS_16',	'CCAPS_17', 'CCAPS_18',
                              'CCAPS_19',	'CCAPS_21',	'CCAPS_22', 'CCAPS_23',
                              'CCAPS_24',	'CCAPS_25',	'CCAPS_26', 'CCAPS_27',
                              'CCAPS_28',	'CCAPS_29',	'CCAPS_30', 'CCAPS_31',
                              'CCAPS_32',	'CCAPS_33',	'CCAPS_34', 'CCAPS_35',
                              'CCAPS_36',	'CCAPS_37',	'CCAPS_38', 'CCAPS_39',
                              'CCAPS_40',	'CCAPS_41',	'CCAPS_43', 'CCAPS_44',
                              'CCAPS_45',	'CCAPS_46',	'CCAPS_47', 'CCAPS_48',
                              'CCAPS_49',	'CCAPS_50',	'CCAPS_51', 'CCAPS_52',
                              'CCAPS_53',	'CCAPS_54',	'CCAPS_56', 'CCAPS_57',
                              'CCAPS_58',	'CCAPS_59',	'CCAPS_60', 'CCAPS_61',
                              'CCAPS_63',	'CCAPS_64',	'CCAPS_65', 'CCAPS_66',
                              'CCAPS_68',	'CCAPS_69',	'CCAPS_70')

      # Cleaning CCAPS34 administrations
      df.ccaps.34 <- data |>
        dplyr::select(ClientID, CaseNoteID,
                      RecordType, Date,
                      NoteType, dplyr::starts_with("CCAPS34"))

      # Renaming CCAPS34 variables
      names(df.ccaps.34) <- c('ClientID', 'CaseNoteID', 'RecordType', 'Date',
                              'NoteType', 'CCAPS_03','CCAPS_05', 'CCAPS_06',
                              'CCAPS_11',	'CCAPS_13',	'CCAPS_16', 'CCAPS_17',
                              'CCAPS_18',	'CCAPS_21',	'CCAPS_22', 'CCAPS_24',
                              'CCAPS_27',	'CCAPS_29',	'CCAPS_30', 'CCAPS_31',
                              'CCAPS_33',	'CCAPS_34',	'CCAPS_36', 'CCAPS_39',
                              'CCAPS_40',	'CCAPS_45',	'CCAPS_46', 'CCAPS_48',
                              'CCAPS_49',	'CCAPS_51',	'CCAPS_52', 'CCAPS_54',
                              'CCAPS_57',	'CCAPS_58',	'CCAPS_59', 'CCAPS_63',
                              'CCAPS_64',	'CCAPS_66',	'CCAPS_68')

      # Combine CCAPS62 and CCAPS34 administrations
      suppressWarnings(
        df.ccaps <- plyr::rbind.fill(df.ccaps.62, df.ccaps.34) |>
          dplyr::mutate(dplyr::across(CCAPS_01:CCAPS_70, as.numeric)) |>
          CCMHr::score_CCAPS() |>
          dplyr::filter(Is_ValidCCAPS == 1) |>
          dplyr::arrange(Date) |>
          dplyr::distinct(.keep_all = TRUE))

      # Specify ID variables
      if(!is.null(id_var)){

        df.ccaps <- df.ccaps |>
          dplyr::left_join(df.id, by = "ClientID") |>
          dplyr::select(dplyr::all_of(id_var), dplyr::everything())

      } else{

      }

      # Add CCAPS to the combined data frame
      df.combine <- plyr::rbind.fill(df.combine, df.ccaps)

      # Specify path and save individual CCAPS form
      if(save_individual_form == TRUE &
         is.null(path_individual_form)){

        write.csv(df.ccaps,
                  file = "TI_CCMH_export_ccaps_clean.csv",
                  row.names = FALSE)

      } else if(save_individual_form == TRUE &
                !is.null(path_individual_form)){

        write.csv(df.ccaps,
                  file = paste0(path_individual_form, "/", "TI_CCMH_export_ccaps_clean.csv"),
                  row.names = FALSE)

      } else{

      }

    } else{

    }

    # Clean SDS
    if("SDS" %in% included_forms){

      # Cleaning SDS administrations
      df.sds <- data |>
        dplyr::select(ClientID, CaseNoteID,
                      RecordType, Date,
                      NoteType, dplyr::contains("SDS")) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("SDS")), ~ replace(., . == 0, NA)) |>
        dplyr::filter(dplyr::if_any(dplyr::contains("SDS"), ~ !is.na(.))) |>
        dplyr::mutate(Has_SDS = 1) |>
        dplyr::arrange(Date) |>
        dplyr::distinct(.keep_all = TRUE)

      # Convert SDS variables to numeric
      df.sds <- CCMHr::sds_to_numeric(df.sds)

      # Specify ID variables
      if(!is.null(id_var)){

        df.sds <- df.sds |>
          dplyr::left_join(df.id, by = "ClientID") |>
          dplyr::select(dplyr::all_of(id_var), dplyr::everything())

      } else{

      }

      # Add SDS to the combined data frame
      df.combine <- plyr::rbind.fill(df.combine, df.sds)

      # Specify path and save individual SDS form
      if(save_individual_form == TRUE &
         is.null(path_individual_form)){

        write.csv(df.sds,
                  file = "TI_CCMH_export_sds_clean.csv",
                  row.names = FALSE)

      } else if(save_individual_form == TRUE &
                !is.null(path_individual_form)){

        write.csv(df.sds,
                  file = paste0(path_individual_form, "/", "TI_CCMH_export_sds_clean.csv"),
                  row.names = FALSE)

      } else{

      }

    } else{

    }

    # Clean CLICC
    if("CLICC" %in% included_forms){

      # Cleaning CLICC administrations
      df.clicc <- data |>
        dplyr::select(ClientID, CaseNoteID,
                      RecordType, Date,
                      NoteType, dplyr::contains("CLICC")) |>
        dplyr::mutate_at(dplyr::vars(CLICC_01_01:CLICC_03), ~ replace(., . == 0, NA)) |>
        dplyr::filter(dplyr::if_any(CLICC_01_01:CLICC_03, ~ !is.na(.))) |>
        dplyr::mutate(Has_CLICC = 1) |>
        dplyr::arrange(Date) |>
        dplyr::distinct(.keep_all = TRUE)

      # Specify ID variables
      if(!is.null(id_var)){

        df.clicc <- df.clicc |>
          dplyr::left_join(df.id, by = "ClientID") |>
          dplyr::select(dplyr::all_of(id_var), dplyr::everything())

      } else{

      }

      # Add CLICC to the combined data frame
      df.combine <- plyr::rbind.fill(df.combine, df.clicc)

      # Specify path and save individual CLICC form
      if(save_individual_form == TRUE &
         is.null(path_individual_form)){

        write.csv(df.clicc,
                  file = "TI_CCMH_export_clicc_clean.csv",
                  row.names = FALSE)

      } else if(save_individual_form == TRUE &
                !is.null(path_individual_form)){

        write.csv(df.clicc,
                  file = paste0(path_individual_form, "/", "TI_CCMH_export_clicc_clean.csv"),
                  row.names = FALSE)

      } else{

      }

    } else{

    }

    # Case Closure
    if("Closure" %in% included_forms){

      # Cleaning CLICC administrations
      df.caseclosure <- data |>
        dplyr::select(ClientID, CaseNoteID,
                      RecordType, Date,
                      NoteType, dplyr::contains("CLOSURE")) |>
        dplyr::mutate_at(dplyr::vars(CLOSURE_01_101:CLOSURE_04_107), ~ replace(., . == 0, NA)) |>
        dplyr::filter(dplyr::if_any(CLOSURE_01_101:CLOSURE_04_107, ~ !is.na(.))) |>
        dplyr::mutate(Has_Closure = 1) |>
        dplyr::arrange(Date) |>
        dplyr::distinct(.keep_all = TRUE)

      # Specify ID variables
      if(!is.null(id_var)){

        df.caseclosure <- df.caseclosure |>
                  dplyr::left_join(df.id, by = "ClientID") |>
                  dplyr::select(dplyr::all_of(id_var), dplyr::everything())

      } else{

      }

      # Add Case Closure to the combined data frame
      df.combine <- plyr::rbind.fill(df.combine, df.caseclosure)

      # Specify path and save individual Case Closure form
      if(save_individual_form == TRUE &
         is.null(path_individual_form)){

        write.csv(df.caseclosure,
                  file = "TI_CCMH_export_closure_clean.csv",
                  row.names = FALSE)

      } else if(save_individual_form == TRUE &
                !is.null(path_individual_form)){

        write.csv(df.caseclosure,
                  file = paste0(path_individual_form, "/", "TI_CCMH_export_closure_clean.csv"),
                  row.names = FALSE)

      } else{

      }

    } else{

    }

    # AUDIT
    if("AUDIT" %in% included_forms){

      # Cleaning AUDIT administrations
      df.audit <- data |>
        dplyr::select(ClientID, CaseNoteID,
                      RecordType, Date,
                      NoteType, dplyr::contains("AUDIT")) |>
        dplyr::rename(AUDIT_01  = AUDIT_1,
                      AUDIT_02  = AUDIT_2,
                      AUDIT_03  = AUDIT_3,
                      AUDIT_04  = AUDIT_4,
                      AUDIT_05  = AUDIT_5,
                      AUDIT_06  = AUDIT_6,
                      AUDIT_07  = AUDIT_7,
                      AUDIT_08  = AUDIT_8,
                      AUDIT_09  = AUDIT_9,
                      AUDIT_10  = AUDIT_10,
                      AUDIT_100 = AUDIT_100) |>
        dplyr::mutate_at(dplyr::vars(AUDIT_01:AUDIT_100), ~ replace(., . == 0, NA)) |>
        dplyr::filter(dplyr::if_any(AUDIT_01:AUDIT_100, ~ !is.na(.))) |>
        dplyr::mutate(Has_AUDIT = 1) |>
        dplyr::arrange(Date) |>
        dplyr::distinct(.keep_all = TRUE)

      # Specify ID variables
      if(!is.null(id_var)){

        df.audit <- df.audit |>
          dplyr::left_join(df.id, by = "ClientID") |>
          dplyr::select(dplyr::all_of(id_var), dplyr::everything())

      } else{

      }

      # Add AUDIT to the combined data frame
      df.combine <- plyr::rbind.fill(df.combine, df.audit)

      # Specify path and save individual AUDIT form
      if(save_individual_form == TRUE &
         is.null(path_individual_form)){

        write.csv(df.audit,
                  file = "TI_CCMH_export_audit_clean.csv",
                  row.names = FALSE)

      } else if(save_individual_form == TRUE &
                !is.null(path_individual_form)){

        write.csv(df.audit,
                  file = paste0(path_individual_form, "/", "TI_CCMH_export_audit_clean.csv"),
                  row.names = FALSE)

      } else{

      }

    } else{

    }

  # Sort by date
  df.combine <- df.combine |>
    dplyr::arrange(Date)

  # Save file
  if(save == TRUE){

    write.csv(df.combine,
              file = path,
              row.names = FALSE)

  } else{

    return(df.combine)

  }
}
