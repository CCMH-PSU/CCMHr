#' Create a data frame with CCAPS change scores between first and last administration
#'
#' @param data A data frame with CCAPS data.
#' @param client_identifier The column uniquely identifying each client. By default, `UniqueClientID`.
#' @param center_identifier The column uniquely identifying each center. By default `CcmhID`.
#' @param add_items Specify the columns or CCAPS variables outside of CCAPS subscales to be included in the returned data frame. Default is `NA`, resulting in no additional CCAPS items being added to the returned data frame. Multiple items should be listed in the following manner: c("Variable1", "Variable2"). Option `"all"` selects all CCAPS 34 items to be in the returned data frame.
#' @param include_first A logical argument that specifies if subscale or item scores from the first completion of the CCAPS should be included in the returned data frame. By default `FALSE` resulting in CCAPS subscale or specified variable scores of the first administration not being included in the returned data frame. Option `"TRUE"` adds first administration scores of CCAPS subscales and specified items to the returned data frame.
#' @param include_last A logical argument that specifies if subscale or item scores from the last completion of the CCAPS should be included in the returned data frame. By default `FALSE` resulting in CCAPS subscale or specified variable scores of the last administration not being included in the returned data frame. Option `"TRUE"` adds last administration scores of CCAPS subscales and specified items to the returned data frame.
#'
#' @return A data frame with CCAPS change scores of subscales and specified CCAPS items. Based on additional arguments, scores from first and last administration will be included as well.
#' @export
#'

CCAPS_change <- function(data,
                          client_identifier = "UniqueClientID",
                          center_identifier = "CcmhID",
                         add_items = NA,
                         include_first = F,
                         include_last = F) {
  t1.st.01 <- Sys.time()

  #Check to see if variables are named correctly
    #List of variables required to run function
      var_names <- c("Is_ValidCCAPS",
                     "Date",
                     "Depression34",
                     "Anxiety34",
                     "Social_Anxiety34",
                     "Academics34",
                     "Eating34",
                     "Hostility34",
                     "Alcohol34",
                     "DI")

    #Running Function to check for missing variables
      CCMHr::required_items(data,
                            var_names)

  # Convert to data frame
  data <- as.data.frame(data)

  # Rename ids
  data <- data %>%
    dplyr::rename(UniqueClientID2 = {{client_identifier}},
                  CcmhID2 = {{center_identifier}})

  # Convert to data table
  data <- data.table::setDT(data)
  # Excluding data with no CCAPS data
  data <- data[Is_ValidCCAPS == 1,]

  # Excluding participants that didn't complete the CCAPS at least two times
  data <- dtplyr::lazy_dt(data)
  data <- data %>%
    dplyr::group_by(UniqueClientID2, CcmhID2) %>%
    dplyr::filter(dplyr::n() >= 2) %>%
    dplyr::ungroup() %>%
    as.data.table()

  # Excluding all data rows outside of first and last responses of the CCAPS
  data <- dtplyr::lazy_dt(data)
  data <- data %>%
    dplyr::group_by(UniqueClientID2, CcmhID2) %>%
    dplyr::arrange(Date) %>%
    dplyr::slice(1, dplyr::n()) %>%
    dplyr::ungroup() %>%
    as.data.table()

  #Variable that will detect if "all" is being specified in argument add_items
    add_items_all <- add_items == "all"[1]

  #Making CCAPS_data exist before running if else statements
    CCAPS_data <- NULL

  #Adding additional CCAPS items based on response to options in add_items
    if(is.na(add_items)[1]==T){ #If no items are added to add_items or NA
      #Returned data frame
      cols <- c("UniqueClientID2", "CcmhID2", "Depression34", "Anxiety34", "Social_Anxiety34",
                "Academics34", "Eating34", "Hostility34", "Alcohol34", "DI")

      CCAPS_data <- data[, .SD, .SDcols = cols]

    } else if(add_items_all[1] == T) { #If all CCAPS items are to be added or "all" is specified in add_items
      #Check to see if variables are named correctly to use this argument
        #List of variables required to run function
          var_names <- c("CCAPS_03", "CCAPS_05", "CCAPS_06", "CCAPS_11",
                         "CCAPS_13", "CCAPS_16", "CCAPS_17", "CCAPS_18",
                         "CCAPS_21", "CCAPS_22", "CCAPS_24", "CCAPS_27",
                         "CCAPS_29", "CCAPS_30", "CCAPS_31", "CCAPS_33",
                         "CCAPS_34", "CCAPS_36", "CCAPS_39", "CCAPS_40",
                         "CCAPS_45", "CCAPS_46", "CCAPS_48", "CCAPS_49",
                         "CCAPS_51", "CCAPS_52", "CCAPS_54", "CCAPS_57",
                         "CCAPS_58", "CCAPS_59", "CCAPS_63", "CCAPS_64",
                         "CCAPS_66", "CCAPS_68")

        #For loops to detect for missing variables
          #Parameters
            #Number of loops
              loop.num <- dim(data.frame(var_names))[1]
            #Data frame for missing variables
              df.detect.miss <- NULL
              df.detect.miss$var_names <- var_names
              df.detect.miss <- data.frame(df.detect.miss)
              df.detect.miss$missing <- NA

          #Loops
            for(i in 1:loop.num){
              df.detect.miss$missing[i] <- data.frame(var_names[[i]]) %in% names(data)
            }

        #Warning Message
          #Removing present variables
          df.detect.miss <- dplyr::filter(df.detect.miss,
                                   df.detect.miss$missing == F)
          #Listing out missing variables
          missing.vars <- toString(df.detect.miss$var_names)

        #Warning
          if(nrow(df.detect.miss) > 0){
            stop(paste0("The following variables are not present or properly named in data: ", missing.vars,". To use 'all' in the argument 'add_items', variable names listed in this error message need to be present in data."))
          } else{
          }

      #Returned data frame
      var_names_all <- c("UniqueClientID2", "CcmhID2", "Depression34", "Anxiety34", "Social_Anxiety34",
                         "Academics34", "Eating34", "Hostility34", "Alcohol34", "DI",
                         "CCAPS_03", "CCAPS_05", "CCAPS_06", "CCAPS_11",
                         "CCAPS_13", "CCAPS_16", "CCAPS_17", "CCAPS_18",
                         "CCAPS_21", "CCAPS_22", "CCAPS_24", "CCAPS_27",
                         "CCAPS_29", "CCAPS_30", "CCAPS_31", "CCAPS_33",
                         "CCAPS_34", "CCAPS_36", "CCAPS_39", "CCAPS_40",
                         "CCAPS_45", "CCAPS_46", "CCAPS_48", "CCAPS_49",
                         "CCAPS_51", "CCAPS_52", "CCAPS_54", "CCAPS_57",
                         "CCAPS_58", "CCAPS_59", "CCAPS_63", "CCAPS_64",
                         "CCAPS_66", "CCAPS_68")

        CCAPS_data <- data[, .SD, .SDcols = var_names_all]

    } else { #If specific items are added to add_items by listing out variable names
        #Specify R loop parameters
        data <- as.data.frame(data)
          #add items is a dataframe
            df_add_items <- data.frame(add_items)
          #Extract loop Number
            loop.num <- dim(df_add_items)[1]
          #Creating a blank df to insert data added by loops and making sure data.tmp is a data frame
            data.tmp<- matrix(nrow=dim(data)[1], ncol=dim(df_add_items)[1])
            data.tmp <- data.frame(data.tmp)

        #For loop that will insert each variable specified in add_items into data.tmp
          for(i in 1:loop.num){
            data.tmp[,i] <- data[,df_add_items[i,1]]
          }

        #Rename columns of data.tmp to match add_items
          colnames(data.tmp) <- add_items

        #Adding client_identifier and center_identifier data.tmp
          p1 <- dim(data.tmp)[2]+1
          p2 <- dim(data.tmp)[2]+2

          data.tmp[,p1:p2] <- data %>%
            dplyr::select(UniqueClientID2, CcmhID2)


        #Creating basic CCAPS_data and ensure it is a dataframe
          CCAPS_data <- data %>%
                          dplyr::select(UniqueClientID2, CcmhID2,
                                        Depression34:DI)
          CCAPS_data <- as.data.frame(CCAPS_data)

        #Merging data frames
          CCAPS_data <-  merge(CCAPS_data, data.tmp)
      }

  #Adding first and last completion of CCAPS scores
    #Specify the last variable in CCAPS_data
      last.var <- rev(names(CCAPS_data))[1]
    #CCAPS_data is a data table
      CCAPS_data <- data.table::setDT(CCAPS_data)

    #Different outcome depending on arguments include_first and include_last
      if(include_first == F & include_last == F){
        #Creating overall data frame
        CCAPS_data2 <- dtplyr::lazy_dt(CCAPS_data)
          data <- CCAPS_data2 %>%
                    dplyr::group_by(UniqueClientID2, CcmhID2) %>%
                    dplyr::select(names(CCAPS_data)) %>%
                    dplyr::summarize(dplyr::across(.data$Depression34:dplyr::all_of(last.var),
                                                   ~first(.x)-last(.x),
                                                   .names = "{col}_change"),
                                     .groups = "keep") %>%
                    as.data.table()

      } else if(include_first == T & include_last == F){
          #Creating overall data frame
          CCAPS_data2 <- dtplyr::lazy_dt(CCAPS_data)
            data <- CCAPS_data2 %>%
                        dplyr::group_by(UniqueClientID2, CcmhID2) %>%
                        dplyr::select(names(CCAPS_data)) %>%
                        dplyr::summarize(dplyr::across(.data$Depression34:dplyr::all_of(last.var),
                                                       first,
                                                       .names = "{.col}_first"),
                                         dplyr::across(.data$Depression34:dplyr::all_of(last.var),
                                                       ~first(.x)-last(.x),
                                                       .names = "{col}_change"),
                                         .groups = "keep")%>%
                    as.data.table()

      } else if(include_first == F & include_last == T){
          #Creating overall data frame
          CCAPS_data2 <- dtplyr::lazy_dt(CCAPS_data)
            data <- CCAPS_data2 %>%
                        dplyr::group_by(UniqueClientID2, CcmhID2) %>%
                        dplyr::select(names(CCAPS_data)) %>%
                        dplyr::summarize(dplyr::across(.data$Depression34:dplyr::all_of(last.var),
                                                       last,
                                                       .names = "{.col}_last"),
                                         dplyr::across(.data$Depression34:dplyr::all_of(last.var),
                                                       ~first(.x)-last(.x),
                                                       .names = "{col}_change"),
                                         .groups = "keep")%>%
                    as.data.table()

      } else {
        #Creating overall data frame
        CCAPS_data2 <- dtplyr::lazy_dt(CCAPS_data)
          data <- CCAPS_data2 %>%
                      dplyr::group_by(UniqueClientID2, CcmhID2) %>%
                      dplyr::select(names(CCAPS_data)) %>%
                      dplyr::summarize(dplyr::across(.data$Depression34:dplyr::all_of(last.var),
                                                     first,
                                                     .names = "{.col}_first"),
                                       dplyr::across(.data$Depression34:dplyr::all_of(last.var),
                                                     last,
                                                     .names = "{.col}_last"),
                                       dplyr::across(.data$Depression34:dplyr::all_of(last.var),
                                                     ~first(.x)-last(.x),
                                                     .names = "{col}_change"),
                                      .groups = "keep")%>%
                    as.data.table()
        }

  # Convert to data frame
  data <- data.table::setDF(data)
  data <- as.data.frame(data)

  # Rename ids
  data <- data %>%
   dplyr::rename(!!client_identifier := UniqueClientID2,
                 !!center_identifier:= CcmhID2)

# #Returns as data frame with CCAPS change scores of subscales and specified variables. Based on additional arguments, scores from first and last completion could be added to the data frame.
   data <- as.data.frame(data)
    return(data)

}
