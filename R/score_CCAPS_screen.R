#' Score the CCAPS subscales from CCAPS screen items
#'
#' @description Scores the CCAPS screen subscales for valid CCAPS administrations.
#' @param data A data file containing CCAPS screen items
#' @note `score_CCAPS_screen` calls on CCAPS items by name, so they must be named properly in
#' `data`. Variable naming convention is `CCAPS_01`... `CCAPS_70`.
#' If 'data does not contain the proper CCAPS variable names
#' (or the naming convention was changed in the new year's data), the
#' function will return an error.
#' Administrations are considered valid and will be scored if 1/3 or less of the items on any subscale are missing, and if less than
#' 1/2 of the data overall is missing, and if the variance of the items is > 0, indicating that the items were not all marked
#' as the same number.
#' Function does not overwrite CCAPS items to reverse score them. It creates new reverse scored version, which it then deletes.
#' Note that this takes several minutes to run.
#' @return A data frame with all the original data in `data`, and several additional columns: `Has_CCAPS`, `Is_ValidCCAPS`, `Depression`, `Anxiety`, `Social_Anxiety`, `Academics`, `Eating`, `Frustration`, `Alcohol`
#' @export

score_CCAPS_screen <- function(data) {

    #Check to see if variables are named correctly
      #List of variables required to run function
        var_names <- c("CCAPS_01", "CCAPS_03", "CCAPS_05", "CCAPS_06", "CCAPS_11",
                       "CCAPS_13", "CCAPS_14", "CCAPS_16", "CCAPS_17", "CCAPS_18",
                       "CCAPS_21", "CCAPS_22", "CCAPS_24", "CCAPS_25", "CCAPS_27",
                       "CCAPS_29", "CCAPS_30", "CCAPS_31", "CCAPS_33", "CCAPS_34",
                       "CCAPS_36", "CCAPS_39", "CCAPS_40", "CCAPS_45", "CCAPS_46",
                       "CCAPS_47", "CCAPS_48", "CCAPS_49", "CCAPS_51", "CCAPS_52",
                       "CCAPS_54", "CCAPS_57", "CCAPS_59", "CCAPS_63", "CCAPS_64",
                       "CCAPS_66", "UniqueClientID")

      #Running Function to check for missing variables
        required_items(data,
                       var_names)

    #Add variable Has_CCAPS, that detects if there is CCAPS Data
      data$Has_CCAPS <- rowSums(!is.na(dplyr::select(data, dplyr::starts_with("CCAPS_"))),
                                na.rm = T)
        #If any CCAPS data was completed, the data will display 1 as the score.
          data$Has_CCAPS[which(data$Has_CCAPS > 0)] <- 1

     #Reverse scoring
        data$CCAPS_18r <- abs(data$CCAPS_18-4)
        data$CCAPS_08r <- abs(data$CCAPS_08-4)
        data$CCAPS_61r <- abs(data$CCAPS_61-4)
        data$CCAPS_32r <- abs(data$CCAPS_32-4)
        data$CCAPS_23r <- abs(data$CCAPS_23-4)
        data$CCAPS_25r <- abs(data$CCAPS_25-4)
        data$CCAPS_09r <- abs(data$CCAPS_09-4)
        data$CCAPS_39r <- abs(data$CCAPS_39-4)
        data$CCAPS_60r <- abs(data$CCAPS_60-4)

    #Number of missing data points per subscale
      #For loop parameters
        #Variables
          Depression <- data.frame(data$CCAPS_11,
                                   data$CCAPS_13,
                                   data$CCAPS_24,
                                   data$CCAPS_27,
                                   data$CCAPS_45,
                                   data$CCAPS_51)

          Anxiety <- data.frame(data$CCAPS_05,
                                data$CCAPS_17,
                                data$CCAPS_21,
                                data$CCAPS_22,
                                data$CCAPS_31,
                                data$CCAPS_34)

          Social_Anxiety <- data.frame(data$CCAPS_03,
                                       data$CCAPS_39r,
                                       data$CCAPS_46,
                                       data$CCAPS_49,
                                       data$CCAPS_52)

          Academics <- data.frame(data$CCAPS_18r,
                                  data$CCAPS_57,
                                  data$CCAPS_59,
                                  data$CCAPS_66)

          Eating <- data.frame(data$CCAPS_06,
                               data$CCAPS_16,
                               data$CCAPS_29)

          Frustration <- data.frame(data$CCAPS_36,
                                    data$CCAPS_40,
                                    data$CCAPS_48,
                                    data$CCAPS_64)

          Alcohol <- data.frame(data$CCAPS_30,
                                data$CCAPS_33,
                                data$CCAPS_54,
                                data$CCAPS_63)

          Family <- data.frame(data$CCAPS_14,
                               data$CCAPS_25r,
                               data$CCAPS_47,
                               data$CCAPS_01)

        #Variable list
          var.list <- c("Depression",
                        "Anxiety",
                        "Social_Anxiety",
                        "Academics",
                        "Eating",
                        "Frustration",
                        "Alcohol",
                        "Family")

        #Loop number
          loop.num <- length(var.list)

        #Setting up df_missing
          df_missing <- matrix(nrow=dim(data)[1], length(var.list))
          df_missing <- data.frame(df_missing)

        #Setting up var.nam
          var.nam <- NULL

      #For loop to calculate missing data
          for(i in 1:loop.num){
            #Within parameters
              miss.var <- NULL
            #Variable name
              var.nam[i] <- paste0(var.list[i],"_MISS")
            #Calculate missing
              miss.var <- rowSums(is.na(eval(parse(text = var.list[i]))))
            #Missing dataframe
              df_missing[,i]<-miss.var
          }

        #Renaming columns
          colnames(df_missing)<-var.nam
        #Total Missing
          df_missing$CCAPS_Tmiss <- rowSums(df_missing)
        #Variance Score
          df_missing$variance <- apply(dplyr::select(data, .data$CCAPS_01:.data$CCAPS_70),
                                       1,
                                       stats::var,
                                       na.rm=T)

     #Calculating Is_ValidCCAPS
       #Add Unique identifer
         p <- dim(df_missing)[2]+1
         df_missing$UniqueClientID <- data$UniqueClientID
         colnames(df_missing)[p]<-"UniqueClientID"

        #Merge Dataframes
          data <- merge(data, df_missing)
        #Creating variables
          data$Is_ValidCCAPS <- 0
          data$Is_ValidCCAPS[which(data$Has_CCAPS == 1 &
                                   data$Depression_MISS < 3 &
                                   data$Anxiety_MISS < 3 &
                                   data$Social_Anxiety_MISS < 2 &
                                   data$Academics_MISS < 2 &
                                   data$Eating_MISS < 2 &
                                   data$Frustration_MISS < 2 &
                                   data$Alcohol_MISS < 2 &
                                   data$Family_MISS <2 &
                                   data$CCAPS_Tmiss <= 17 &
                                   data$variance >0)] <- 1

      #Calculate Subscale Scores
        #For Loop Parameters
          df_subscale <- matrix(nrow=dim(data)[1], length(var.list))
          df_subscale <- data.frame(df_subscale)
        #For loop
          for(i in 1:loop.num){
            #Within parameters
              subscale.var <- NULL
            #Calculate subscale
              subscale.var <- apply(eval(parse(text = var.list[i])),
                                    1,
                                    mean,
                                    na.rm=T)
              subscale.var[which(data$Is_ValidCCAPS==0)]=NA
            #Missing dataframe
              df_subscale[,i]<-subscale.var
          }
        #Renaming columns
          colnames(df_subscale)<-var.list
        #Add Unique identifer
          p <- dim(df_subscale)[2]+1
          df_subscale$UniqueClientID <- data$UniqueClientID
          colnames(df_subscale)[p]<-"UniqueClientID"

      #Merge Dataframes
         data <- merge(data, df_subscale)

    #Delete created variables after creating subscales
      data <- dplyr::select(data, -c(.data$CCAPS_18r:.data$CCAPS_60r,.data$Depression_MISS:.data$variance))

    return(data)
}
