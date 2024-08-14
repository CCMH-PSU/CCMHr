# TI CCMH export support functions

#' Clean TI appointment export file
#'
#'  @param data A data frame.
#'  @param key A data frame with two columns, orginal_name and new_name, to rename the columns in the appointment data. Default is `CCMHr::TI_CCMH_export_appointment_key`
#'  @param save A logical statement indicates whether the data frame should be saved as a file under a local folder. If false, the data will be returned as an object. By default, `FALSE`
#'  @param path A quoted string to indicate the path where the appointment data should be saved. By default, `TI_appointment_clean.csv`
#' 
#' @return A data frame with cleaned TI exported appointment data.
#' @export

clean_TI_export_appointment <- function(data, 
                                        key = CCMHr::TI_CCMH_export_appointment_key, 
                                        save = FALSE, 
                                        path = "TI_appointment_clean.csv"){


    # Rename based on key
    data <- data %>%
        dplyr::rename_with(~ key$new_name, key$orginal_name) %>%
        dplyr::mutate(UniqueClientID = ClientID)

    # Format date
    data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

    # Delete duplicate appointment
    data <- CCMHr::delete_duplicate_appointments(data)

    # Exclude UniqueClientID
    data <- data %>%
        dplyr::select(-UniqueClientID)

    # Save file 
    if(save == TRUE){
        write.csv(data, 
                  file = path, 
                  row.names = FALSE)
    } else {
       return(data)
    }
    
}


#' Clean TI appointment export file
#'
#'  @param data A data frame.
#'  @param id_var A quoted string to indicate the name of the ID variable if it exist. By default, `NULL`. 
#'  @param key A key to rename the columns in the form data file. Default is `CCMHr::TI_CCMH_export_forms_key`
#'  @param included_forms A string or list of strings to indicate the forms that should be included in the data. Options include: "CCAPS", "SDS", "CLICC", "Closure", and "AUDIT". By default, `c("CCAPS", "SDS", "CLICC", "Closure", "AUDIT")`
#'  @param save_individual_form A logical statement indicates whether the individual forms should be saved seperate under a local folder. By default, `FALSE`
#'  @param path_individual_form A quoted string to indicate the path where the individual forms should be saved. By default, "NULL" or saved under the current working directory
#'  @param save A logical statement indicates whether the overall data frame should be saved as a file under a local folder. If false, the data will be returned as an object. By default, `FALSE`
#'  @param path A quoted string to indicate the path where the appointment data should be saved. By default, `TI_appointment_clean.csv`
#' 
#' @return A data frame with cleaned TI exported data forms.
#' @export

clean_TI_CCMH_export <- function(data, 
                                 id_var = NULL,
                                 key = CCMHr::TI_CCMH_export_forms_key,
                                 included_forms = c("CCAPS", "SDS", "CLICC", "Closure", "AUDIT"),
                                 save_individual_form = FALSE,
                                 path_individual_form = NULL,
                                 save = FALSE, 
                                 path = "TI_CCMH_export_clean.csv"){

    # If / exists at the end of the path, remove it
    if(substr(path_individual_form, nchar(path_individual_form), nchar(path_individual_form)) == "/"){
        path_individual_form <- substr(path_individual_form, 1, nchar(path_individual_form) - 1)
    } else {
        # Skip
    }

    # Does folder exist
    if(save_individual_form == TRUE & !is.null(path_individual_form)){
        exist.folder <- dir.exists(paste0(path_individual_form))
        if(exist.folder == FALSE){
            stop("The path_individual_form folder does not exist")
        }
    } else {
        # Skip
    }

    # Clean data forms
        # Rename ID variables
        data <- data %>%
            dplyr::rename("ClientID" = "clientid", 
                          "CaseNoteID" = "casenoteid", 
                          "RecordType" = "recordtype", 
                          "Date" = "notedate", 
                          "NoteType" = "notetype")

        # rename based on key
        data <- data %>% 
            dplyr::rename_with(~ key$ccmh_var_name, key$standard_var_name)

        # Recoding "<No Response>"
        data[] <- lapply(data, function(x) {
        x[x == "<No Response>"] <- NA
        return(x)
        })

        # Recoding ""
        data[] <- lapply(data, function(x) {
        x[x == ""] <- NA
        return(x)
        })

    # Format date
    data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

    # add data frame
    df.combine <- data.frame()

    # ID variable 
    if(!is.null(id_var)){
        df.id <- data %>% 
            dplyr::select(ClientID, dplyr::all_of(id_var)) %>%
            unique()

    } else {
        # Skip
    }

    # Clean ccaps
    if("CCAPS" %in% included_forms){ 
        # Cleaning CCAPS 62
        df.ccaps.62 <- data %>% 
            dplyr::select(ClientID, CaseNoteID, 
                          RecordType, Date, 
                          NoteType, dplyr::starts_with("CCAPS62")) 

        names(df.ccaps.62) <- c('ClientID', 'CaseNoteID', 'RecordType', 'Date', 
                                'NoteType', 'CCAPS_01',	'CCAPS_03',
                                'CCAPS_04',	'CCAPS_05',	'CCAPS_06',	'CCAPS_08',
                                'CCAPS_09',	'CCAPS_10',	'CCAPS_11',	'CCAPS_13',
                                'CCAPS_14',	'CCAPS_15',	'CCAPS_16',	'CCAPS_17',
                                'CCAPS_18',	'CCAPS_19',	'CCAPS_21',	'CCAPS_22',
                                'CCAPS_23',	'CCAPS_24',	'CCAPS_25',	'CCAPS_26',
                                'CCAPS_27',	'CCAPS_28',	'CCAPS_29',	'CCAPS_30',
                                'CCAPS_31',	'CCAPS_32',	'CCAPS_33',	'CCAPS_34',
                                'CCAPS_35',	'CCAPS_36',	'CCAPS_37',	'CCAPS_38',
                                'CCAPS_39',	'CCAPS_40',	'CCAPS_41',	'CCAPS_43',
                                'CCAPS_44',	'CCAPS_45',	'CCAPS_46',	'CCAPS_47',
                                'CCAPS_48',	'CCAPS_49',	'CCAPS_50',	'CCAPS_51',
                                'CCAPS_52',	'CCAPS_53',	'CCAPS_54',	'CCAPS_56',
                                'CCAPS_57',	'CCAPS_58',	'CCAPS_59',	'CCAPS_60',
                                'CCAPS_61',	'CCAPS_63',	'CCAPS_64',	'CCAPS_65',
                                'CCAPS_66',	'CCAPS_68',	'CCAPS_69',	'CCAPS_70')

        # Cleaning CCAPS 34
        df.ccaps.34 <- data %>% 
            dplyr::select(ClientID, CaseNoteID, 
                          RecordType, Date, 
                          NoteType, dplyr::starts_with("CCAPS34")) 
  
        names(df.ccaps.34) <- c('ClientID', 'CaseNoteID', 'RecordType', 'Date', 
                                'NoteType', 'CCAPS_03','CCAPS_05',
                                'CCAPS_06',	'CCAPS_11',	'CCAPS_13',	'CCAPS_16',
                                'CCAPS_17',	'CCAPS_18',	'CCAPS_21',	'CCAPS_22',
                                'CCAPS_24',	'CCAPS_27',	'CCAPS_29',	'CCAPS_30',
                                'CCAPS_31',	'CCAPS_33',	'CCAPS_34',	'CCAPS_36',
                                'CCAPS_39',	'CCAPS_40',	'CCAPS_45',	'CCAPS_46',
                                'CCAPS_48',	'CCAPS_49',	'CCAPS_51',	'CCAPS_52',
                                'CCAPS_54',	'CCAPS_57',	'CCAPS_58',	'CCAPS_59',
                                'CCAPS_63',	'CCAPS_64',	'CCAPS_66',	'CCAPS_68')
  
        # Combine CCAPS 62 and 34
        suppressWarnings(
            df.ccaps <- plyr::rbind.fill(df.ccaps.62, df.ccaps.34) %>%
            dplyr::mutate(dplyr::across(CCAPS_01:CCAPS_70, as.numeric)) %>%
            CCMHr::score_CCAPS() %>%
            dplyr::filter(Is_ValidCCAPS == 1) %>%
            dplyr::arrange(Date)%>%
            dplyr::distinct(.keep_all = TRUE))

        # ID variable 
        if(!is.null(id_var)){
            df.ccaps <- df.ccaps %>% 
                dplyr::left_join(df.id, by = "ClientID") %>%
                dplyr::select(dplyr::all_of(id_var), dplyr::everything())

        } else {
            # Skip
        }

        df.combine <- plyr::rbind.fill(df.combine, df.ccaps)

        if(save_individual_form == TRUE & is.null(path_individual_form)){
            write.csv(df.ccaps, 
                      file = "TI_CCMH_export_ccaps_clean.csv", 
                      row.names = FALSE)
        } else if(save_individual_form == TRUE & !is.null(path_individual_form)) {
            write.csv(df.ccaps, 
                      file = paste0(path_individual_form, "/", "TI_CCMH_export_ccaps_clean.csv"), 
                      row.names = FALSE)
        } else {
            # Skip CCAPS
        }

    } else {
        # Skip CCAPS
    }

    # Clean SDS
    if("SDS" %in% included_forms){ 

    df.sds <- data %>%
        dplyr::select(ClientID, CaseNoteID, 
                      RecordType, Date, 
                      NoteType, dplyr::contains("SDS")) %>%
        dplyr::mutate_at(vars(dplyr::contains("SDS")), ~ replace(., . == 0, NA)) %>%
        dplyr::filter(if_any(dplyr::contains("SDS"), ~ !is.na(.))) %>%
        dplyr::mutate(Has_SDS = 1)%>%
        dplyr::arrange(Date)%>%
        dplyr::distinct(.keep_all = TRUE)

    df.sds <- CCMHr::sds_to_numeric(df.sds)

    # ID variable 
        if(!is.null(id_var)){
            df.sds <- df.sds %>% 
                dplyr::left_join(df.id, by = "ClientID") %>%
                dplyr::select(dplyr::all_of(id_var), dplyr::everything())

        } else {
            # Skip
        }

    if(save_individual_form == TRUE & is.null(path_individual_form)){
            write.csv(df.sds, 
                      file = "TI_CCMH_export_sds_clean.csv", 
                      row.names = FALSE)
        } else if(save_individual_form == TRUE & !is.null(path_individual_form)) {
            write.csv(df.sds, 
                      file = paste0(path_individual_form, "/", "TI_CCMH_export_sds_clean.csv"), 
                      row.names = FALSE)
        } else {
            # Skip CCAPS
        }

    df.combine <- plyr::rbind.fill(df.combine, df.sds)

    } else {
        # Skip SDS
    }

    # Clean CLICC
    if("CLICC" %in% included_forms){ 
        df.clicc <- data %>%
            dplyr::select(ClientID, CaseNoteID, 
                        RecordType, Date, 
                        NoteType, dplyr::contains("CLICC")) %>%
            dplyr::mutate_at(vars(CLICC_01_01:CLICC_03), ~ replace(., . == 0, NA)) %>%
            dplyr::filter(if_any(CLICC_01_01:CLICC_03, ~ !is.na(.))) %>%
            dplyr::mutate(Has_CLICC = 1)%>%
            dplyr::arrange(Date)%>%
            dplyr::distinct(.keep_all = TRUE)

        # ID variable 
        if(!is.null(id_var)){
            df.clicc <- df.clicc %>% 
                dplyr::left_join(df.id, by = "ClientID") %>%
                dplyr::select(dplyr::all_of(id_var), dplyr::everything())

        } else {
            # Skip
        }

        if(save_individual_form == TRUE & is.null(path_individual_form)){
            write.csv(df.clicc, 
                      file = "TI_CCMH_export_clicc_clean.csv", 
                      row.names = FALSE)
        } else if(save_individual_form == TRUE & !is.null(path_individual_form)) {
            write.csv(df.clicc, 
                      file = paste0(path_individual_form, "/", "TI_CCMH_export_clicc_clean.csv"), 
                      row.names = FALSE)
        } else {
            # Skip CCAPS
        }

        df.combine <- plyr::rbind.fill(df.combine, df.clicc)
 
    } else {
        # Skip CLICC
    }

    # Case Closure
    if("Closure" %in% included_forms){ 
        df.caseclosure <- data %>%
            dplyr::select(ClientID, CaseNoteID, 
                        RecordType, Date, 
                        NoteType, dplyr::contains("CLOSURE")) %>%
            dplyr::mutate_at(vars(CLOSURE_01_101:CLOSURE_04_107), ~ replace(., . == 0, NA)) %>%
            dplyr::filter(if_any(CLOSURE_01_101:CLOSURE_04_107, ~ !is.na(.))) %>%
            dplyr::mutate(Has_Closure = 1) %>%
            dplyr::arrange(Date)%>%
            dplyr::distinct(.keep_all = TRUE)

        # ID variable 
        if(!is.null(id_var)){
            df.caseclosure <- df.caseclosure %>% 
                dplyr::left_join(df.id, by = "ClientID") %>%
                dplyr::select(dplyr::all_of(id_var), dplyr::everything())

        } else {
            # Skip
        }

        if(save_individual_form == TRUE & is.null(path_individual_form)){
            write.csv(df.ccaps, 
                      file = "TI_CCMH_export_closure_clean.csv", 
                      row.names = FALSE)
        } else if(save_individual_form == TRUE & !is.null(path_individual_form)) {
            write.csv(df.ccaps, 
                      file = paste0(path_individual_form, "/", "TI_CCMH_export_closure_clean.csv"), 
                      row.names = FALSE)
        } else {
            # Skip CCAPS
        }

        df.combine <- plyr::rbind.fill(df.combine, df.caseclosure)

    } else {
        # Skip Case Closure
    }

    # Clean Audit
    if("AUDIT" %in% included_forms){ 
        df.audit <- data %>%
            dplyr::select(ClientID, CaseNoteID, 
                        RecordType, Date, 
                        NoteType, dplyr::contains("AUDIT")) %>%
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
                          AUDIT_100 = AUDIT_100) %>%  
            dplyr::mutate_at(vars(AUDIT_01:AUDIT_100), ~ replace(., . == 0, NA)) %>%
            dplyr::filter(if_any(AUDIT_01:AUDIT_100, ~ !is.na(.))) %>%
            dplyr::mutate(Has_AUDIT = 1)%>%
            dplyr::arrange(Date)%>%
            dplyr::distinct(.keep_all = TRUE)

        # ID variable 
        if(!is.null(id_var)){
            df.audit <- df.audit %>% 
                dplyr::left_join(df.id, by = "ClientID") %>%
                dplyr::select(dplyr::all_of(id_var), dplyr::everything())

        } else {
            # Skip
        }

        if(save_individual_form == TRUE & is.null(path_individual_form)){
            write.csv(df.audit, 
                      file = "TI_CCMH_export_audit_clean.csv", 
                      row.names = FALSE)
        } else if(save_individual_form == TRUE & !is.null(path_individual_form)) {
            write.csv(df.audit, 
                      file = paste0(path_individual_form, "/", "TI_CCMH_export_audit_clean.csv"), 
                      row.names = FALSE)
        } else {
            # Skip CCAPS
        }

        df.combine <- plyr::rbind.fill(df.combine, df.audit)

    } else {
        # Skip CLICC
    }

    # Sort by date
    df.combine <- df.combine %>%
        dplyr::arrange(Date)

    # Save file 
    if(save == TRUE){
        write.csv(df.combine, 
                  file = path, 
                  row.names = FALSE)
    } else {
       return(df.combine)
    }
    
}