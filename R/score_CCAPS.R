#' Score the CCAPS subscales from CCAPS items
#'
#' @description Scores the CCAPS subscales for valid CCAPS administrations.
#' @param data A data file containing CCAPS item data
#' @param CCAPS62 A logical value indicating whether the CCAPS62 should be scored. If FALSE, only the CCAPS34 will be scored.
#' @note `score_CCAPS` calls on CCAPS items by name, so they must be named properly in
#' `data`. Variable naming convention is `CCAPS_01`... `CCAPS_70`.
#' If 'data does not contain the proper CCAPS variable names
#' (or the naming convention was changed in the new year's data), the
#' function will return an error.
#' Administrations are considered valid and will be scored if 1/3 or less of the items on any subscale are missing, and if less than
#' 1/2 of the data overall is missing, and if the variance of the items is > 0, indicating that the items were not all marked
#' as the same number.
#' Function does not overwrite CCAPS items to reverse score them. It creates new reverse scored version, which it then deletes.
#' Note that this takes several minutes to run.
#' @return A data frame with all the original data in `data`, and several additional columns: `Has_CCAPS`, `Is_CCAPS62`, `Is_ValidCCAPS`, `Is_ValidCCAPS62`, `Is_CCAPS34`, `Is_ValidCCAPS34`, `Depression34`, `Anxiety34`, `Social_Anxiety34`, `Academics34`, `Eating34`, `Hostility34`, `Alcohol34`, `DI`, `Depression62`, `Eating62`, `Substance62`, `Anxiety62`, `Hostility62`, `Social_Anxiety62`, `Family62`, `Academics62`
#' @export

score_CCAPS <- function(data, CCAPS62 = T) {
  if(CCAPS62 == T) {
  if(!all(c("CCAPS_01", "CCAPS_03", "CCAPS_04", "CCAPS_05", "CCAPS_06", "CCAPS_08", "CCAPS_09", "CCAPS_10", "CCAPS_11", "CCAPS_13", "CCAPS_14", "CCAPS_15", "CCAPS_16", "CCAPS_17", "CCAPS_18", "CCAPS_19", "CCAPS_21", "CCAPS_22", "CCAPS_23", "CCAPS_24", "CCAPS_25", "CCAPS_26", "CCAPS_27", "CCAPS_28", "CCAPS_29", "CCAPS_30", "CCAPS_31", "CCAPS_32", "CCAPS_33", "CCAPS_34", "CCAPS_35", "CCAPS_36", "CCAPS_37", "CCAPS_38", "CCAPS_39", "CCAPS_40", "CCAPS_41", "CCAPS_43", "CCAPS_44", "CCAPS_45", "CCAPS_46", "CCAPS_47", "CCAPS_48", "CCAPS_49", "CCAPS_50", "CCAPS_51", "CCAPS_52", "CCAPS_53", "CCAPS_54", "CCAPS_56", "CCAPS_57", "CCAPS_58", "CCAPS_59", "CCAPS_60", "CCAPS_61", "CCAPS_63", "CCAPS_64", "CCAPS_65", "CCAPS_66", "CCAPS_68", "CCAPS_69", "CCAPS_70") %in% colnames(data))) stop('CCAPS items not present in data or not properly named. CCAPS items should be named CCAPS_01 through CCAPS_70.')

  # Has_CCAPS
  data$Has_CCAPS <- rowSums(!is.na(dplyr::select(data, CCAPS_01:CCAPS_70)), na.rm = T)
  data$Has_CCAPS[which(data$Has_CCAPS > 0)] <- 1

  # Is_CCAPS62 or Is_CCAPS34
  data$Is_CCAPS62 <- rowSums(!is.na(dplyr::select(data, CCAPS_01,CCAPS_04,CCAPS_08,CCAPS_09,CCAPS_10,CCAPS_14,CCAPS_15,CCAPS_19,CCAPS_23, CCAPS_25,CCAPS_26,CCAPS_28,CCAPS_32,CCAPS_35,CCAPS_37,CCAPS_38,CCAPS_41,CCAPS_43,CCAPS_44,CCAPS_47,CCAPS_50,CCAPS_53,CCAPS_56,CCAPS_60, CCAPS_61,CCAPS_65,CCAPS_69,CCAPS_70)), na.rm = T)
  data$Is_CCAPS62[which(data$Is_CCAPS62 > 0)] <- 1
  data$Is_CCAPS34 <- 0
  data$Is_CCAPS34[which(data$Has_CCAPS ==1 & data$Is_CCAPS62 == 0)] <- 1

  #Number missing per subscale and overall for 34 and 62
  data$Depression62_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_13, CCAPS_24, CCAPS_45, CCAPS_27, CCAPS_51, CCAPS_10, CCAPS_61, CCAPS_70, CCAPS_32, CCAPS_11, CCAPS_15, CCAPS_41, CCAPS_65)), na.rm = T)
  data$Eating62_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_29, CCAPS_16,CCAPS_35, CCAPS_06, CCAPS_26, CCAPS_23, CCAPS_69, CCAPS_38, CCAPS_53)), na.rm = T)
  data$Substance62_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_54, CCAPS_30, CCAPS_56, CCAPS_33, CCAPS_63, CCAPS_28)), na.rm = T)
  data$Anxiety62_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_31, CCAPS_17, CCAPS_05, CCAPS_22, CCAPS_34, CCAPS_37, CCAPS_04, CCAPS_21, CCAPS_44)), na.rm = T)
  data$Hostility62_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_36, CCAPS_48, CCAPS_64, CCAPS_58, CCAPS_40, CCAPS_68, CCAPS_50)), na.rm = T)
  data$Social_Anxiety62_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_03, CCAPS_49, CCAPS_39, CCAPS_60, CCAPS_52, CCAPS_19, CCAPS_46)), na.rm = T)
  data$Family62_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_25, CCAPS_47, CCAPS_09, CCAPS_01, CCAPS_14, CCAPS_43)), na.rm = T)
  data$Academics62_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_59, CCAPS_66, CCAPS_18, CCAPS_08, CCAPS_57)), na.rm = T)
  data$CCAPS62_Tmiss <- rowSums(is.na(dplyr::select(data, CCAPS_01:CCAPS_70)), na.rm = T)
  data$Depression34_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_11, CCAPS_13, CCAPS_24, CCAPS_27, CCAPS_45, CCAPS_51)), na.rm = T)
  data$Anxiety34_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_05, CCAPS_17, CCAPS_21, CCAPS_22, CCAPS_31, CCAPS_34)), na.rm = T)
  data$Social_Anxiety34_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_03, CCAPS_39, CCAPS_46, CCAPS_49, CCAPS_52)), na.rm = T)
  data$Academics34_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_18, CCAPS_57, CCAPS_59, CCAPS_66)), na.rm = T)
  data$Eating34_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_06, CCAPS_16, CCAPS_29)), na.rm = T)
  data$Hostility34_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_36, CCAPS_40, CCAPS_48, CCAPS_58, CCAPS_64, CCAPS_68)), na.rm = T)
  data$Alcohol34_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_30, CCAPS_33, CCAPS_54, CCAPS_63)), na.rm = T)
  data$CCAPS34_Tmiss <- rowSums(dplyr::select(data, Depression34_MISS, Anxiety34_MISS, Social_Anxiety34_MISS, Academics34_MISS, Eating34_MISS, Hostility34_MISS, Alcohol34_MISS), na.rm = T)

  # Variance
  data$variance <- matrixStats::rowVars(as.matrix(dplyr::select(data,CCAPS_01:CCAPS_70)), na.rm = T)

  # Indicators of valid/invalid administrations to score
  data$Is_ValidCCAPS <- 0
  data$Is_ValidCCAPS[which(data$Has_CCAPS == 1 & data$Depression34_MISS < 3 & data$Anxiety34_MISS < 3 & data$Social_Anxiety34_MISS < 2 & data$Academics34_MISS < 2 & data$Eating34_MISS < 2 & data$Hostility34_MISS < 3 & data$Alcohol34_MISS < 2 & data$CCAPS34_Tmiss <= 17 & data$variance >0)] <- 1
  data$Is_ValidCCAPS[which(data$Is_CCAPS62 == 1 & (data$Depression62_MISS > 4 | data$Anxiety62_MISS > 3 | data$Social_Anxiety62_MISS > 2 | data$Academics62_MISS > 1 | data$Eating62_MISS > 3 | data$Hostility62_MISS > 2 | data$Substance62_MISS > 2 | data$Family62_MISS > 2 | data$CCAPS62_Tmiss >= 32))] <- 0
  data$Is_ValidCCAPS62 <- 0
  data$Is_ValidCCAPS62[which(data$Is_CCAPS62 == 1 & data$Depression62_MISS < 5 & data$Anxiety62_MISS < 4 & data$Social_Anxiety62_MISS < 3 & data$Academics62_MISS < 2 & data$Eating62_MISS < 4 & data$Hostility62_MISS < 3 & data$Substance62_MISS < 3 & data$Family62_MISS < 3 & data$CCAPS62_Tmiss <= 31 & data$variance > 0)] <- 1
  data$Is_ValidCCAPS62[which(data$Is_ValidCCAPS == 0 )] <- 0
  data$Is_ValidCCAPS34 <- 0
  data$Is_ValidCCAPS34[which(data$Is_ValidCCAPS == 1 & data$Is_ValidCCAPS62 == 0)] <- 1

  # Reverse scoring
  data$CCAPS_18r <- abs(data$CCAPS_18-4)
  data$CCAPS_08r <- abs(data$CCAPS_08-4)
  data$CCAPS_61r <- abs(data$CCAPS_61-4)
  data$CCAPS_32r <- abs(data$CCAPS_32-4)
  data$CCAPS_23r <- abs(data$CCAPS_23-4)
  data$CCAPS_25r <- abs(data$CCAPS_25-4)
  data$CCAPS_09r <- abs(data$CCAPS_09-4)
  data$CCAPS_39r <- abs(data$CCAPS_39-4)
  data$CCAPS_60r <- abs(data$CCAPS_60-4)

  # Scoring subscales
  data$Depression34 <- apply(dplyr::select(data, c(CCAPS_11, CCAPS_13,CCAPS_24, CCAPS_27, CCAPS_45, CCAPS_51)), 1, mean, na.rm=T)
  data$Depression34[which(data$Is_ValidCCAPS==0)] <- NA

  data$Anxiety34 <- apply(dplyr::select(data, c(CCAPS_05, CCAPS_17,CCAPS_21, CCAPS_22, CCAPS_31, CCAPS_34)), 1, mean, na.rm=T)
  data$Anxiety34[which(data$Is_ValidCCAPS==0)]=NA

  data$Social_Anxiety34 <- apply(dplyr::select(data, c(CCAPS_03,CCAPS_39r, CCAPS_46, CCAPS_49, CCAPS_52)), 1, mean, na.rm=T)
  data$Social_Anxiety34[which(data$Is_ValidCCAPS==0)]=NA

  data$Academics34 <- apply(dplyr::select(data, c(CCAPS_18r, CCAPS_57, CCAPS_59, CCAPS_66)), 1, mean, na.rm=T)
  data$Academics34[which(data$Is_ValidCCAPS==0)]=NA

  data$Eating34 <- apply(dplyr::select(data, c(CCAPS_06, CCAPS_16, CCAPS_29)), 1, mean, na.rm=T)
  data$Eating34[which(data$Is_ValidCCAPS==0)]=NA

  data$Hostility34 <- apply(dplyr::select(data, c(CCAPS_36, CCAPS_40, CCAPS_48, CCAPS_58, CCAPS_64, CCAPS_68)), 1, mean, na.rm=T)
  data$Hostility34[which(data$Is_ValidCCAPS==0)]=NA

  data$Alcohol34 <- apply(dplyr::select(data, c(CCAPS_30, CCAPS_33,CCAPS_54, CCAPS_63)), 1, mean, na.rm=T)
  data$Alcohol34[which(data$Is_ValidCCAPS==0)]=NA

  data$DI <- apply(dplyr::select(data, c(CCAPS_05, CCAPS_11, CCAPS_13, CCAPS_17, CCAPS_21, CCAPS_22, CCAPS_24,CCAPS_27, CCAPS_31, CCAPS_34, CCAPS_40, CCAPS_45,CCAPS_46, CCAPS_48, CCAPS_51, CCAPS_52, CCAPS_57, CCAPS_58, CCAPS_59, CCAPS_66)), 1, mean, na.rm=T)
  data$DI[which(data$Is_ValidCCAPS==0)]=NA

  data$Depression62 <- apply(dplyr::select(data, c(CCAPS_13, CCAPS_24, CCAPS_45, CCAPS_27, CCAPS_51, CCAPS_10, CCAPS_61r,CCAPS_70, CCAPS_32r, CCAPS_11, CCAPS_15, CCAPS_41, CCAPS_65)), 1, mean, na.rm=T)
  data$Depression62[which(data$Is_ValidCCAPS62==0)]=NA

  data$Eating62 <- apply(dplyr::select(data, c(CCAPS_29, CCAPS_16,CCAPS_35, CCAPS_06, CCAPS_26, CCAPS_23r, CCAPS_69, CCAPS_38, CCAPS_53)), 1, mean, na.rm=T)
  data$Eating62[which(data$Is_ValidCCAPS62==0)]=NA

  data$Substance62 <- apply(dplyr::select(data, c(CCAPS_54, CCAPS_30,CCAPS_56, CCAPS_33, CCAPS_63, CCAPS_28)), 1, mean, na.rm=T)
  data$Substance62[which(data$Is_ValidCCAPS62==0)]=NA

  data$Anxiety62 <- apply(dplyr::select (data, c(CCAPS_31, CCAPS_17, CCAPS_05, CCAPS_22, CCAPS_34, CCAPS_37, CCAPS_04, CCAPS_21, CCAPS_44)), 1, mean, na.rm=T)
  data$Anxiety62[which(data$Is_ValidCCAPS62==0)]=NA

  data$Hostility62 <- apply(dplyr::select(data, c(CCAPS_36, CCAPS_48, CCAPS_64, CCAPS_58, CCAPS_40, CCAPS_68, CCAPS_50)), 1, mean, na.rm=T)
  data$Hostility62[which(data$Is_ValidCCAPS62==0)]=NA

  data$Social_Anxiety62 <- apply(dplyr::select (data, c(CCAPS_03, CCAPS_49, CCAPS_39r, CCAPS_60r, CCAPS_52, CCAPS_19, CCAPS_46)), 1, mean, na.rm=T)
  data$Social_Anxiety62[which(data$Is_ValidCCAPS62==0)]=NA

  data$Family62 <- apply(dplyr::select(data, c(CCAPS_25r, CCAPS_47, CCAPS_09r, CCAPS_01, CCAPS_14, CCAPS_43)), 1, mean, na.rm=T)
  data$Family62[which(data$Is_ValidCCAPS62==0)]=NA

  data$Academics62 <- apply(dplyr::select(data, c(CCAPS_59, CCAPS_66, CCAPS_18r, CCAPS_08r, CCAPS_57)), 1, mean, na.rm=T)
  data$Academics62[which(data$Is_ValidCCAPS62==0)]=NA

  # Delete created vars after creating subscales
  data <- dplyr::select(data, -c(Depression62_MISS:variance, CCAPS_18r:CCAPS_60r))
  return(data)
  } else if (CCAPS62 == F) {
    if(!all(c("CCAPS_03", "CCAPS_05", "CCAPS_06", "CCAPS_11", "CCAPS_13", "CCAPS_16", "CCAPS_17", "CCAPS_18", "CCAPS_21", "CCAPS_22", "CCAPS_24", "CCAPS_27", "CCAPS_29", "CCAPS_30", "CCAPS_31", "CCAPS_33", "CCAPS_34", "CCAPS_36", "CCAPS_39", "CCAPS_40", "CCAPS_45", "CCAPS_46", "CCAPS_48", "CCAPS_49", "CCAPS_51", "CCAPS_52", "CCAPS_54", "CCAPS_57", "CCAPS_58", "CCAPS_59", "CCAPS_63", "CCAPS_64", "CCAPS_66", "CCAPS_68") %in% colnames(data))) stop('CCAPS items not present in data or not properly named. CCAPS items should be named CCAPS_01 through CCAPS_70.')

    # Has_CCAPS
    data$Has_CCAPS <- rowSums(!is.na(dplyr::select(data,CCAPS_03,CCAPS_05,CCAPS_06,CCAPS_11,CCAPS_13,CCAPS_16,CCAPS_17,CCAPS_18,CCAPS_21,CCAPS_22,CCAPS_24,CCAPS_27,CCAPS_29,CCAPS_30,CCAPS_31,CCAPS_33,CCAPS_34,CCAPS_36,CCAPS_39,CCAPS_40,CCAPS_45,CCAPS_46,CCAPS_48,CCAPS_49,CCAPS_51,CCAPS_52,CCAPS_54,CCAPS_57,CCAPS_58,CCAPS_59,CCAPS_63,CCAPS_64,CCAPS_66,CCAPS_68)), na.rm = T)
    data$Has_CCAPS[which(data$Has_CCAPS > 0)] <- 1

    #Number missing per subscale
    data$Depression34_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_11, CCAPS_13, CCAPS_24, CCAPS_27, CCAPS_45, CCAPS_51)), na.rm = T)
    data$Anxiety34_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_05, CCAPS_17, CCAPS_21, CCAPS_22, CCAPS_31, CCAPS_34)), na.rm = T)
    data$Social_Anxiety34_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_03, CCAPS_39, CCAPS_46, CCAPS_49, CCAPS_52)), na.rm = T)
    data$Academics34_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_18, CCAPS_57, CCAPS_59, CCAPS_66)), na.rm = T)
    data$Eating34_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_06, CCAPS_16, CCAPS_29)), na.rm = T)
    data$Hostility34_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_36, CCAPS_40, CCAPS_48, CCAPS_58, CCAPS_64, CCAPS_68)), na.rm = T)
    data$Alcohol34_MISS <- rowSums(is.na(dplyr::select(data, CCAPS_30, CCAPS_33, CCAPS_54, CCAPS_63)), na.rm = T)
    data$CCAPS34_Tmiss <- rowSums(dplyr::select(data, Depression34_MISS, Anxiety34_MISS, Social_Anxiety34_MISS, Academics34_MISS, Eating34_MISS, Hostility34_MISS, Alcohol34_MISS), na.rm = T)

    # Variance
    data$variance <- matrixStats::rowVars(as.matrix(dplyr::select(data, CCAPS_03,CCAPS_05,CCAPS_06,CCAPS_11,CCAPS_13,CCAPS_16,CCAPS_17,CCAPS_18,CCAPS_21,CCAPS_22,CCAPS_24,CCAPS_27,CCAPS_29,CCAPS_30,CCAPS_31,CCAPS_33,CCAPS_34,CCAPS_36,CCAPS_39,CCAPS_40,CCAPS_45,CCAPS_46,CCAPS_48,CCAPS_49,CCAPS_51,CCAPS_52,CCAPS_54,CCAPS_57,CCAPS_58,CCAPS_59,CCAPS_63,CCAPS_64,CCAPS_66,CCAPS_68)), na.rm=T)

    # Indicators of valid/invalid administrations to score
    data$Is_ValidCCAPS <- 0
    data$Is_ValidCCAPS[which(data$Has_CCAPS == 1 & data$Depression34_MISS < 3 & data$Anxiety34_MISS < 3 & data$Social_Anxiety34_MISS < 2 & data$Academics34_MISS < 2 & data$Eating34_MISS < 2 & data$Hostility34_MISS < 3 & data$Alcohol34_MISS < 2 & data$CCAPS34_Tmiss <= 17 & data$variance >0)] <- 1

    # Reverse scoring
    data$CCAPS_18r <- abs(data$CCAPS_18-4)
    data$CCAPS_39r <- abs(data$CCAPS_39-4)

    # Scoring subscales
    data$Depression34 <- apply(dplyr::select(data, c(CCAPS_11, CCAPS_13,CCAPS_24, CCAPS_27, CCAPS_45, CCAPS_51)), 1, mean, na.rm=T)
    data$Depression34[which(data$Is_ValidCCAPS==0)] <- NA

    data$Anxiety34 <- apply(dplyr::select(data, c(CCAPS_05, CCAPS_17,CCAPS_21, CCAPS_22, CCAPS_31, CCAPS_34)), 1, mean, na.rm=T)
    data$Anxiety34[which(data$Is_ValidCCAPS==0)]=NA

    data$Social_Anxiety34 <- apply(dplyr::select(data, c(CCAPS_03,CCAPS_39r, CCAPS_46, CCAPS_49, CCAPS_52)), 1, mean, na.rm=T)
    data$Social_Anxiety34[which(data$Is_ValidCCAPS==0)]=NA

    data$Academics34 <- apply(dplyr::select(data, c(CCAPS_18r, CCAPS_57, CCAPS_59, CCAPS_66)), 1, mean, na.rm=T)
    data$Academics34[which(data$Is_ValidCCAPS==0)]=NA

    data$Eating34 <- apply(dplyr::select(data, c(CCAPS_06, CCAPS_16, CCAPS_29)), 1, mean, na.rm=T)
    data$Eating34[which(data$Is_ValidCCAPS==0)]=NA

    data$Hostility34 <- apply(dplyr::select(data, c(CCAPS_36, CCAPS_40, CCAPS_48, CCAPS_58, CCAPS_64, CCAPS_68)), 1, mean, na.rm=T)
    data$Hostility34[which(data$Is_ValidCCAPS==0)]=NA

    data$Alcohol34 <- apply(dplyr::select(data, c(CCAPS_30, CCAPS_33,CCAPS_54, CCAPS_63)), 1, mean, na.rm=T)
    data$Alcohol34[which(data$Is_ValidCCAPS==0)]=NA

    data$DI <- apply(dplyr::select(data, c(CCAPS_05, CCAPS_11, CCAPS_13, CCAPS_17, CCAPS_21, CCAPS_22, CCAPS_24,CCAPS_27, CCAPS_31, CCAPS_34, CCAPS_40, CCAPS_45,CCAPS_46, CCAPS_48, CCAPS_51, CCAPS_52, CCAPS_57, CCAPS_58, CCAPS_59, CCAPS_66)), 1, mean, na.rm=T)
    data$DI[which(data$Is_ValidCCAPS==0)]=NA

    # Delete created vars after creating subscales
    data <- dplyr::select(data, -c(Depression34_MISS:variance, CCAPS_18r:CCAPS_39r))
    return(data)
  } else {
    print("Please indicate whether to score the CCAPS62 or not by setting the CCAPS62 argument to TRUE or FALSE.")
  }
}

