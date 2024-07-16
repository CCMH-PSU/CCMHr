# TI CCMH export support functions

#' Clean TI appointment export file
#'
#'  @param data A unquoted (when data file is an r object) or quoted string (when data file is located in a local directory) to specify the data file. If specified from directory, the data file should be a csv file.
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

    # Data
    if(is.character(data)){
        df.app <- read.csv(data)
    } else {
        df.app <- data
    }

    # rename based on key
    df.app <- df.app %>%
        dplyr::rename_with(~ key$new_name, key$orginal_name) %>%
        dplyr::mutate(UniqueClientID = ClientID)

    # Delete duplicate appointment
    df.app <- CCMHr::delete_duplicate_appointments(df.app)

    # Exclude UniqueClientID
    df.app <- df.app %>%
        dplyr::select(-UniqueClientID)

    # Save file
    if(save == TRUE){
        write.csv(df.app,
                  file = path,
                  row.names = FALSE)
    } else {
       return(df.app)
    }

}


#' Clean TI appointment export file
#'
#'  @param data.form A unquoted (when data file is an r object) or quoted string (when data file is located in a local directory) of the CCMH forms data file. If specified from directory, the data file should be a csv file.
#'  @param data.label A unquoted (when data file is an r object) or quoted string (when data file is located in a local directory) of the CCMH labels data file. If specified from directory, the data file should be a xls file. No precleaning is required.
#'  @param key A key to rename the columns in the form data file. Default is `CCMHr::TI_CCMH_export_forms_key`
#'  @param included_forms A string or list of strings to indicate the forms that should be included in the data. Options include: "CCAPS", "SDS", "CLICC", "Closure", and "Non-CCMH". By default, `c("CCAPS", "SDS", "CLICC", "Closure")`
#'  @param save_individual_form A logical statement indicates whether the individual forms should be saved seperate under a local folder. By default, `FALSE`
#'  @param path_individual_form A quoted string to indicate the path where the individual forms should be saved. By default, "NULL" or saved under the current working directory
#'  @param save A logical statement indicates whether the overall data frame should be saved as a file under a local folder. If false, the data will be returned as an object. By default, `FALSE`
#'  @param path A quoted string to indicate the path where the appointment data should be saved. By default, `TI_appointment_clean.csv`
#'
#' @return A data frame with cleaned TI exported data forms.
#' @export

clean_TI_CCMH_export <- function(data.form,
                                 data.label,
                                 key = CCMHr::TI_CCMH_export_forms_key,
                                 included_forms = c("CCAPS", "SDS", "CLICC", "Closure"),
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

    # Data forms
    if(is.character(data.form)){
        data.form <- read.csv(data.form)
    } else {
        data.form <- data.form
    }

    # Data labels
    suppressMessages(
    if(is.character(data.label)){
        data.label <- readxl::read_xls(data.label)
    } else {
        data.label <- data.label
    })

    # Clean data labels
        # Exclude the first three rows
        data.label <- data.label[-c(1:3), ]

        # Use first row as name
        names(data.label) <- data.label[1, ]

        # Remove first row
        data.label <- data.label[-1, ]

        # Make data frame
        data.label <- as.data.frame(data.label)

        # Clean data name
        names(data.label) <- c("ti_var_name", "standard_var_name", "description")

        # Recode NA in new_name to original_name
        data.label$standard_var_name[is.na(data.label$standard_var_name)] <- data.label$ti_var_name[is.na(data.label$standard_var_name)]

        # merge key
        suppressMessages(
        data.label <- data.label %>%
            dplyr::left_join(key))

        # Recode NA in new_name to original_name
        data.label$ccmh_var_name[is.na(data.label$ccmh_var_name)] <- data.label$ti_var_name[is.na(data.label$ccmh_var_name)]

        # Drop na
        data.label <- data.label %>%
            tidyr::drop_na()

    # Clean data forms
        # Rename ID variables
        data.form <- data.form %>%
            dplyr::rename("ClientID" = "clientid",
                          "CaseNoteID" = "casenoteid",
                          "RecordType" = "recordtype",
                          "Date" = "notedate",
                          "NoteType" = "notetype")

        # rename based on key
        data.form <- data.form %>%
            dplyr::rename_with(~ data.label$ccmh_var_name[which(data.label$ti_var_name == .x)], .cols = data.label$ti_var_name)

        # Date
        #data.form$Date <- as.character(data.form$Date)
        #data.form$Date <- as.Date(data.form$Date, format = "%m/%d/%Y")

        # Recoding "<No Response>"
        data.form[] <- lapply(data.form, function(x) {
        x[x == "<No Response>"] <- NA
        return(x)
        })

        # Recoding ""
        data.form[] <- lapply(data.form, function(x) {
        x[x == ""] <- NA
        return(x)
        })

    # add data frame
    df.combine <- data.frame()

    # Clean ccaps
    if("CCAPS" %in% included_forms){
        # Cleaning CCAPS 62
        df.ccaps.62 <- data.form %>%
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
        df.ccaps.34 <- data.form %>%
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

    df.sds <- data.form %>%
        dplyr::select(ClientID, CaseNoteID,
                      RecordType, Date,
                      NoteType, dplyr::contains("SDS")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("SDS")), ~ dplyr::replace(., . == 0, NA)) %>%
        dplyr::filter(dplyr::if_any(dplyr::contains("SDS"), ~ !is.na(.))) %>%
        dplyr::mutate(Has_SDS = 1)%>%
        dplyr::arrange(Date)%>%
        dplyr::distinct(.keep_all = TRUE)

    df.sds <- CCMHr::sds_to_numeric(df.sds)

    if(save_individual_form == TRUE & is.null(path_individual_form)){
            write.csv(df.ccaps,
                      file = "TI_CCMH_export_sds_clean.csv",
                      row.names = FALSE)
        } else if(save_individual_form == TRUE & !is.null(path_individual_form)) {
            write.csv(df.ccaps,
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
        df.clicc <- data.form %>%
            dplyr::select(ClientID, CaseNoteID,
                        RecordType, Date,
                        NoteType, dplyr::contains("CLICC")) %>%
            dplyr::mutate_at(dplyr::vars(CLICC_01_01:CLICC_03), ~ dplyr::replace(., . == 0, NA)) %>%
            dplyr::filter(dplyr::if_any(CLICC_01_01:CLICC_03, ~ !is.na(.))) %>%
            dplyr::mutate(Has_CLICC = 1)%>%
            dplyr::arrange(Date)%>%
            dplyr::distinct(.keep_all = TRUE)

        if(save_individual_form == TRUE & is.null(path_individual_form)){
            write.csv(df.ccaps,
                      file = "TI_CCMH_export_clicc_clean.csv",
                      row.names = FALSE)
        } else if(save_individual_form == TRUE & !is.null(path_individual_form)) {
            write.csv(df.ccaps,
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
        df.caseclosure <- data.form %>%
            dplyr::select(ClientID, CaseNoteID,
                        RecordType, Date,
                        NoteType, dplyr::contains("CLOSURE")) %>%
            dplyr::mutate_at(dplyr::vars(CLOSURE_01_101:CLOSURE_04_107), ~ dplyr::replace(., . == 0, NA)) %>%
            dplyr::filter(dplyr::if_any(CLOSURE_01_101:CLOSURE_04_107, ~ !is.na(.))) %>%
            dplyr::mutate(Has_Closure = 1) %>%
            dplyr::arrange(Date)%>%
            dplyr::distinct(.keep_all = TRUE)

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

    # Non CCMH data
    if("Non-CCMH" %in% included_forms){
    df.other <- data.form %>%
        dplyr::select(ClientID, CaseNoteID,
                      RecordType, Date,
                      NoteType, dplyr::contains("df"))  %>%
        dplyr::filter(dplyr::if_any(dplyr::contains("df"), ~ !is.na(.))) %>%
        dplyr::mutate(Non_CCMH = 1)%>%
        dplyr::distinct(.keep_all = TRUE)%>%
        dplyr::arrange(Date)%>%
        dplyr::distinct(.keep_all = TRUE)

    if(save_individual_form == TRUE & is.null(path_individual_form)){
            write.csv(df.ccaps,
                      file = "TI_CCMH_export_non-CCMH_clean.csv",
                      row.names = FALSE)
        } else if(save_individual_form == TRUE & !is.null(path_individual_form)) {
            write.csv(df.ccaps,
                      file = paste0(path_individual_form, "/", "TI_CCMH_export_non-CCMH_clean.csv"),
                      row.names = FALSE)
        } else {
            # Skip CCAPS
        }

    df.combine <- plyr::rbind.fill(df.combine, df.other)

    } else {
        # Skip Non-CCMH
    }

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
