#' Upload CCAPS data with change scores between first and last completion
#'
#' @param data A data frame with CCAPS data.
#' @param client_identifier The column uniquely identifying each client. By default, `UniqueClientID`.
#' @param center_identifier The column uniquely identifying each center. By default `CcmhID`.
#' @param add_items Specify the columns or CCAPS variables outside of CCAPS subscales to be included in the returned data frame. By default `NA` resulting in no additional CCAPS variables being added to the returned data frame. Multiple features should be listed in the following manner: c("Variable1", "Variable2"). Option `"all"` selects all CCAPS 34 variables to be in the returned data frame.
#' @param include_first A logical argument that specifies if subscale or variable scores from the first completion of the CCAPs should be included in the returned data frame. By default `FALSE` resulting in CCAPS subscale or specified variable scores of the first completion not being included in the returned data frame. Option `"TRUE"` adds first completion scores of CCAPS subscales and specified variables to the returned data frame.
#' @param include_last A logical argument that specifies if subscale or variable scores from the last completion of the CCAPs should be included in the returned data frame. By default `FALSE` resulting in CCAPS subscale or specified variable scores of the last completion not being included in the returned data frame. Option `"TRUE"` adds last completion scores of CCAPS subscales and specified variables to the returned data frame.
#'
#' @return A data frame with CCAPS change scores of subscales and specified variables. Based on additional arguments, scores from first and last completion could be added to the data frame.
#' @export
#'

CCAPS_change <- function(data,
                         client_identifier = UniqueClientID,
                         center_identifier = CcmhID,
                         add_items = NA,
                         include_first = F,
                         include_last = F) {

  #Packages required to run function
    library(tidyverse)
    library(CCMHr)

  #Check to see if variables are named correctly
    #List of variables required to run function
      var_names <- c("Is_ValidCCAPS",
                     "Depression34",
                     "Anxiety34",
                     "Social_Anxiety34",
                     "Academics34",
                     "Eating34",
                     "Hostility34",
                     "Alcohol34",
                     "DI")

    #Running Function to check for missing variables
      required_items(data,
                     var_names)

  #Excluding data with no CCAPS data
    data <- subset(data,
                   data$Is_ValidCCAPS == 1)

  #Excluding participants that didn't complete the CCAPS at least two times
    data <- data %>%
              dplyr::group_by({{client_identifier}},{{center_identifier}}) %>%
              dplyr::filter(dplyr::n() >= 2)

  #Excluding all data rows outside of first and last responses of the CCAPS
    data <- data %>%
              dplyr::slice(1, n())

  #Variable that will detect if "all" is being specified in argument add_items
    add_items_all <- add_items == "all"[1]

  #Making an CCAPS_data exist before running if else statements
    CCAPS_data <- NULL


  #Adding additional CCAPS items based on response to options in add_items
    if(is.na(add_items)[1]==T){ #If no items are added to add_items or NA
      #Returned data frame
        CCAPS_data <- data %>%
                        dplyr::select({{client_identifier}},
                                      {{center_identifier}},
                                      Depression34:DI)

    } else if(add_items_all[1] == T) { #If all CCAPS items are to be added or "all" is specified in add_items
      #Check to see if variables are named correctly to use this arguement
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
          df.detect.miss <- subset(df.detect.miss,
                                   df.detect.miss$missing == F)
          #Listing out missing variables
          missing.vars <- toString(df.detect.miss$var_names)

        #Warning
          if(nrow(df.detect.miss) > 0){
            stop(paste0("The following variables are not present or properly named in data: ", missing.vars,". To use 'all' in the argument 'add_items', variable names listed in this error message need to be present in data."))
          } else{
          }

      #Returned data frame
        CCAPS_data <- data %>%
                        dplyr::select({{client_identifier}}, {{center_identifier}},
                                      Depression34:DI, CCAPS_03, CCAPS_05,
                                      CCAPS_06, CCAPS_11, CCAPS_13, CCAPS_16,
                                      CCAPS_17, CCAPS_18, CCAPS_21, CCAPS_22,
                                      CCAPS_24, CCAPS_27, CCAPS_29, CCAPS_30,
                                      CCAPS_31, CCAPS_33, CCAPS_34, CCAPS_36,
                                      CCAPS_39, CCAPS_40, CCAPS_45,CCAPS_46,
                                      CCAPS_48, CCAPS_49, CCAPS_51, CCAPS_52,
                                      CCAPS_54, CCAPS_57, CCAPS_58, CCAPS_59,
                                      CCAPS_63, CCAPS_64, CCAPS_66, CCAPS_68)

    } else { #If specific items are added to add_items by listing out variable names
        #Specify R loop parameters
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
                                select({{client_identifier}}, {{center_identifier}})


        #Creating basic CCAPS_data and ensure it is a dataframe
          CCAPS_data <- data %>%
                          dplyr::select({{client_identifier}},
                                        {{center_identifier}},
                                        Depression34:DI)
          CCAPS_data <- as.data.frame(CCAPS_data)

        #Merging data frames
          CCAPS_data <-  merge(CCAPS_data, data.tmp)
      }


  #Adding first and last completion of CCAPS scores
    #Specify the last variable in CCAPS_data
      last.var <- rev(names(CCAPS_data))[1]

    #Different outcome depending on arguments include_first and include_last
      if(include_first == F & include_last == F){
        #Creating overall data frame
          data <- CCAPS_data %>%
                    dplyr::group_by({{client_identifier}}, {{center_identifier}},) %>%
                    dplyr::select(names(CCAPS_data)) %>%
                    dplyr::summarize(dplyr::across(Depression34:all_of(last.var),
                                                   ~first(.x)-last(.x),
                                                   .names = "{col}_change"),
                                     .groups = "keep")

      } else if(include_first == T & include_last == F){
          #Creating overall data frame
            data <- CCAPS_data %>%
                        dplyr::group_by({{client_identifier}}, {{center_identifier}},) %>%
                        dplyr::select(names(CCAPS_data)) %>%
                        dplyr::summarize(dplyr::across(Depression34:all_of(last.var),
                                                       first,
                                                       .names = "{.col}_first"),
                                         dplyr::across(Depression34:all_of(last.var),
                                                       ~first(.x)-last(.x),
                                                       .names = "{col}_change"),
                                         .groups = "keep")

      } else if(include_first == F & include_last == T){
          #Creating overall data frame
            data <- CCAPS_data %>%
                        dplyr::group_by({{client_identifier}}, {{center_identifier}},) %>%
                        dplyr::select(names(CCAPS_data)) %>%
                        dplyr::summarize(dplyr::across(Depression34:all_of(last.var),
                                                       last,
                                                       .names = "{.col}_last"),
                                         dplyr::across(Depression34:all_of(last.var),
                                                       ~first(.x)-last(.x),
                                                       .names = "{col}_change"),
                                         .groups = "keep")

      } else {
        #Creating overall data frame
          data <- CCAPS_data %>%
                      dplyr::group_by({{client_identifier}}, {{center_identifier}},) %>%
                      dplyr::select(names(CCAPS_data)) %>%
                      dplyr::summarize(dplyr::across(Depression34:all_of(last.var),
                                                     first,
                                                     .names = "{.col}_first"),
                                       dplyr::across(Depression34:all_of(last.var),
                                                     last,
                                                     .names = "{.col}_last"),
                                       dplyr::across(Depression34:all_of(last.var),
                                                     ~first(.x)-last(.x),
                                                     .names = "{col}_change"),
                                      .groups = "keep")
        }

#Returns as data frame with CCAPS change scores of subscales and specified variables. Based on additional arguments, scores from first and last completion could be added to the data frame.
  data
}
