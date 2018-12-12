#' A function to score the CCAPS subscales from CCAPS items
#'
#' @description Scores the CCAPS subscales for valid CCAPS administrations
#' @param \code{dat} A data file containing CCAPS item data
#' @note \code{Scoring_CCAPS} calls on CCAPS items by name, so they must be named properly in
#' \code{dat}. Variable naming convention is\code{CCAPS_01}... \code{CCAPS_70}.
#' If \code{dat} does not contain the proper CCAPS variable names
#' (or the naming convention was changed in the new year's data), the
#' function will return an error.
#' Administrations are considered valid and will be scored if 1/3 or less of the items on any subscale are missing, and if less than
#' 1/2 of the data overall is missing, and if the variance of the items is > 0, indicating that the items were not all marked
#' as the same number.
#' Note that this takes several minutes to run.
#' @return A data frame with all the original data in \code{dat}, and several additional columns: \code{Has_CCAPS}, \code{Is_CCAPS62}, \code{Is_ValidCCAPS}, \code{Is_ValidCCAPS62}, \code{Is_CCAPS34}, \code{Is_ValidCCAPS34}, \code{Depression34}, \code{Anxiety34}, \code{Social_Anxiety34}, \code{Academics34}, \code{Eating34}, \code{Hostility34}, \code{Alcohol34}, \code{DI}, \code{Depression62}, \code{Eating62}, \code{Substance62}, \code{Anxiety62}, \code{Hostility62}, \code{Social_Anxiety62}, \code{Family62}, \code{Academics62}
#' @export

score_CCAPS <- function(dat) {
  if(!all(c("CCAPS_01", "CCAPS_03", "CCAPS_04", "CCAPS_05", "CCAPS_06", "CCAPS_08", "CCAPS_09", "CCAPS_10", "CCAPS_11", "CCAPS_13", "CCAPS_14", "CCAPS_15", "CCAPS_16", "CCAPS_17", "CCAPS_18", "CCAPS_19", "CCAPS_21", "CCAPS_22", "CCAPS_23", "CCAPS_24", "CCAPS_25", "CCAPS_26", "CCAPS_27", "CCAPS_28", "CCAPS_29", "CCAPS_30", "CCAPS_31", "CCAPS_32", "CCAPS_33", "CCAPS_34", "CCAPS_35", "CCAPS_36", "CCAPS_37", "CCAPS_38", "CCAPS_39", "CCAPS_40", "CCAPS_41", "CCAPS_43", "CCAPS_44", "CCAPS_45", "CCAPS_46", "CCAPS_47", "CCAPS_48", "CCAPS_49", "CCAPS_50", "CCAPS_51", "CCAPS_52", "CCAPS_53", "CCAPS_54", "CCAPS_56", "CCAPS_57", "CCAPS_58", "CCAPS_59", "CCAPS_60", "CCAPS_61", "CCAPS_63", "CCAPS_64", "CCAPS_65", "CCAPS_66", "CCAPS_68", "CCAPS_69", "CCAPS_70") %in% colnames(dat))) stop('CCAPS items not present in data or not properly named. CCAPS items should be named CCAPS_01 through CCAPS_70.')

  # Has_CCAPS
  dat$Has_CCAPS <- rowSums(!is.na(select(dat, CCAPS_01:CCAPS_70)), na.rm = T)
  dat$Has_CCAPS[which(dat$Has_CCAPS > 0)] <- 1

  # Is_CCAPS62 or Is_CCAPS34
  dat$Is_CCAPS62 <- rowSums(!is.na(select(dat, CCAPS_01,CCAPS_04,CCAPS_08,CCAPS_09,CCAPS_10,CCAPS_14,CCAPS_15,CCAPS_19,CCAPS_23, CCAPS_25,CCAPS_26,CCAPS_28,CCAPS_32,CCAPS_35,CCAPS_37,CCAPS_38,CCAPS_41,CCAPS_43,CCAPS_44,CCAPS_47,CCAPS_50,CCAPS_53,CCAPS_56,CCAPS_60, CCAPS_61,CCAPS_65,CCAPS_69,CCAPS_70)), na.rm = T)
  dat$Is_CCAPS62[which(dat$Is_CCAPS62 > 0)] <- 1
  dat$Is_CCAPS34 <- 0
  dat$Is_CCAPS34[which(dat$Has_CCAPS ==1 & dat$Is_CCAPS62 == 0)] <- 1

  #Number missing per subscale and overall for 34 and 62
  dat$Depression62_MISS <- rowSums(is.na(select(dat, CCAPS_13, CCAPS_24, CCAPS_45, CCAPS_27, CCAPS_51, CCAPS_10, CCAPS_61, CCAPS_70, CCAPS_32, CCAPS_11, CCAPS_15, CCAPS_41, CCAPS_65)), na.rm = T)
  dat$Eating62_MISS <- rowSums(is.na(select(dat, CCAPS_29, CCAPS_16,CCAPS_35, CCAPS_06, CCAPS_26, CCAPS_23, CCAPS_69, CCAPS_38, CCAPS_53)), na.rm = T)
  dat$Substance62_MISS <- rowSums(is.na(select(dat, CCAPS_54, CCAPS_30, CCAPS_56, CCAPS_33, CCAPS_63, CCAPS_28)), na.rm = T)
  dat$Anxiety62_MISS <- rowSums(is.na(select(dat, CCAPS_31, CCAPS_17, CCAPS_05, CCAPS_22, CCAPS_34, CCAPS_37, CCAPS_04, CCAPS_21, CCAPS_44)), na.rm = T)
  dat$Hostility62_MISS <- rowSums(is.na(select(dat, CCAPS_36, CCAPS_48, CCAPS_64, CCAPS_58, CCAPS_40, CCAPS_68, CCAPS_50)), na.rm = T)
  dat$Social_Anxiety62_MISS <- rowSums(is.na(select(dat, CCAPS_03, CCAPS_49, CCAPS_39, CCAPS_60, CCAPS_52, CCAPS_19, CCAPS_46)), na.rm = T)
  dat$Family62_MISS <- rowSums(is.na(select(dat, CCAPS_25, CCAPS_47, CCAPS_09, CCAPS_01, CCAPS_14, CCAPS_43)), na.rm = T)
  dat$Academics62_MISS <- rowSums(is.na(select(dat, CCAPS_59, CCAPS_66, CCAPS_18, CCAPS_08, CCAPS_57)), na.rm = T)
  dat$CCAPS62_Tmiss <- rowSums(is.na(select(dat, CCAPS_01:CCAPS_70)), na.rm = T)
  dat$Depression34_MISS <- rowSums(is.na(select(dat, CCAPS_11, CCAPS_13, CCAPS_24, CCAPS_27, CCAPS_45, CCAPS_51)), na.rm = T)
  dat$Anxiety34_MISS <- rowSums(is.na(select(dat, CCAPS_05, CCAPS_17, CCAPS_21, CCAPS_22, CCAPS_31, CCAPS_34)), na.rm = T)
  dat$Social_Anxiety34_MISS <- rowSums(is.na(select(dat, CCAPS_03, CCAPS_39, CCAPS_46, CCAPS_49, CCAPS_52)), na.rm = T)
  dat$Academics34_MISS <- rowSums(is.na(select(dat, CCAPS_18, CCAPS_57, CCAPS_59, CCAPS_66)), na.rm = T)
  dat$Eating34_MISS <- rowSums(is.na(select(dat, CCAPS_06, CCAPS_16, CCAPS_29)), na.rm = T)
  dat$Hostility34_MISS <- rowSums(is.na(select(dat, CCAPS_36, CCAPS_40, CCAPS_48, CCAPS_58, CCAPS_64, CCAPS_68)), na.rm = T)
  dat$Alcohol34_MISS <- rowSums(is.na(select(dat, CCAPS_30, CCAPS_33, CCAPS_54, CCAPS_63)), na.rm = T)
  dat$CCAPS34_Tmiss <- rowSums(select(dat, Depression34_MISS, Anxiety34_MISS, Social_Anxiety34_MISS, Academics34_MISS, Eating34_MISS, Hostility34_MISS, Alcohol34_MISS), na.rm = T)

  # Variance
  dat$variance <- apply(select(dat,CCAPS_01:CCAPS_70), 1, var, na.rm=T)

  # Indicators of valid/invalid administrations to score
  dat$Is_ValidCCAPS <- 0
  dat$Is_ValidCCAPS[which(dat$Has_CCAPS == 1 & dat$Depression34_MISS < 3 & dat$Anxiety34_MISS < 3 & dat$Social_Anxiety34_MISS < 2 & dat$Academics34_MISS < 2 & dat$Eating34_MISS < 2 & dat$Hostility34_MISS < 3 & dat$Alcohol34_MISS < 2 & dat$CCAPS34_Tmiss <= 17 & dat$variance >0)] <- 1
  dat$Is_ValidCCAPS[which(dat$Is_CCAPS62 == 1 & (dat$Depression62_MISS > 4 | dat$Anxiety62_MISS > 3 | dat$Social_Anxiety62_MISS > 2 | dat$Academics62_MISS > 1 | dat$Eating62_MISS > 3 | dat$Hostility62_MISS > 2 | dat$Substance62_MISS > 2 | dat$Family62_MISS > 2 | dat$CCAPS62_Tmiss >= 32))] <- 0
  dat$Is_ValidCCAPS62 <- 0
  dat$Is_ValidCCAPS62[which(dat$Is_CCAPS62 == 1 & dat$Depression62_MISS < 5 & dat$Anxiety62_MISS < 4 & dat$Social_Anxiety62_MISS < 3 & dat$Academics62_MISS < 2 & dat$Eating62_MISS < 4 & dat$Hostility62_MISS < 3 & dat$Substance62_MISS < 3 & dat$Family62_MISS < 3 & dat$CCAPS62_Tmiss <= 31 & dat$variance > 0)] <- 1
  dat$Is_ValidCCAPS62[which(dat$Is_ValidCCAPS == 0 )] <- 0
  dat$Is_ValidCCAPS34 <- 0
  dat$Is_ValidCCAPS34[which(dat$Is_ValidCCAPS == 1 & dat$Is_ValidCCAPS62 == 0)] <- 1

  # Reverse scoring
  dat$CCAPS_18r <- abs(dat$CCAPS_18-4)
  dat$CCAPS_08r <- abs(dat$CCAPS_08-4)
  dat$CCAPS_61r <- abs(dat$CCAPS_61-4)
  dat$CCAPS_32r <- abs(dat$CCAPS_32-4)
  dat$CCAPS_23r <- abs(dat$CCAPS_23-4)
  dat$CCAPS_25r <- abs(dat$CCAPS_25-4)
  dat$CCAPS_09r <- abs(dat$CCAPS_09-4)
  dat$CCAPS_39r <- abs(dat$CCAPS_39-4)
  dat$CCAPS_60r <- abs(dat$CCAPS_60-4)

  # Scoring subscales
  dat$Depression34 <- apply(select(dat, c(CCAPS_11, CCAPS_13,CCAPS_24, CCAPS_27, CCAPS_45, CCAPS_51)), 1, mean, na.rm=T)
  dat$Depression34[which(dat$Is_ValidCCAPS==0)] <- NA

  dat$Anxiety34 <- apply(select(dat, c(CCAPS_05, CCAPS_17,CCAPS_21, CCAPS_22, CCAPS_31, CCAPS_34)), 1, mean, na.rm=T)
  dat$Anxiety34[which(dat$Is_ValidCCAPS==0)]=NA

  dat$Social_Anxiety34 <- apply(select(dat, c(CCAPS_03,CCAPS_39r, CCAPS_46, CCAPS_49, CCAPS_52)), 1, mean, na.rm=T)
  dat$Social_Anxiety34[which(dat$Is_ValidCCAPS==0)]=NA

  dat$Academics34 <- apply(select(dat, c(CCAPS_18r, CCAPS_57, CCAPS_59, CCAPS_66)), 1, mean, na.rm=T)
  dat$Academics34[which(dat$Is_ValidCCAPS==0)]=NA

  dat$Eating34 <- apply(select(dat, c(CCAPS_06, CCAPS_16, CCAPS_29)), 1, mean, na.rm=T)
  dat$Eating34[which(dat$Is_ValidCCAPS==0)]=NA

  dat$Hostility34 <- apply(select(dat, c(CCAPS_36, CCAPS_40, CCAPS_48, CCAPS_58, CCAPS_64, CCAPS_68)), 1, mean, na.rm=T)
  dat$Hostility34[which(dat$Is_ValidCCAPS==0)]=NA

  dat$Alcohol34 <- apply(select(dat, c(CCAPS_30, CCAPS_33,CCAPS_54, CCAPS_63)), 1, mean, na.rm=T)
  dat$Alcohol34[which(dat$Is_ValidCCAPS==0)]=NA

  dat$DI <- apply(select(dat, c(CCAPS_05, CCAPS_11, CCAPS_13, CCAPS_17, CCAPS_21, CCAPS_22, CCAPS_24,CCAPS_27, CCAPS_31, CCAPS_34, CCAPS_40, CCAPS_45,CCAPS_46, CCAPS_48, CCAPS_51, CCAPS_52, CCAPS_57, CCAPS_58, CCAPS_59, CCAPS_66)), 1, mean, na.rm=T)
  dat$DI[which(dat$Is_ValidCCAPS==0)]=NA

  dat$Depression62 <- apply(select (dat, c(CCAPS_13, CCAPS_24, CCAPS_45, CCAPS_27, CCAPS_51, CCAPS_10, CCAPS_61r,CCAPS_70, CCAPS_32r, CCAPS_11, CCAPS_15, CCAPS_41, CCAPS_65)), 1, mean, na.rm=T)
  dat$Depression62[which(dat$Is_ValidCCAPS62==0)]=NA

  dat$Eating62 <- apply(select (dat, c(CCAPS_29, CCAPS_16,CCAPS_35, CCAPS_06, CCAPS_26, CCAPS_23r, CCAPS_69, CCAPS_38, CCAPS_53)), 1, mean, na.rm=T)
  dat$Eating62[which(dat$Is_ValidCCAPS62==0)]=NA

  dat$Substance62 <- apply(select (dat, c(CCAPS_54, CCAPS_30,CCAPS_56, CCAPS_33, CCAPS_63, CCAPS_28)), 1, mean, na.rm=T)
  dat$Substance62[which(dat$Is_ValidCCAPS62==0)]=NA

  dat$Anxiety62 <- apply(select (dat, c(CCAPS_31, CCAPS_17, CCAPS_05, CCAPS_22, CCAPS_34, CCAPS_37, CCAPS_04, CCAPS_21, CCAPS_44)), 1, mean, na.rm=T)
  dat$Anxiety62[which(dat$Is_ValidCCAPS62==0)]=NA

  dat$Hostility62 <- apply(select (dat, c(CCAPS_36, CCAPS_48, CCAPS_64, CCAPS_58, CCAPS_40, CCAPS_68, CCAPS_50)), 1, mean, na.rm=T)
  dat$Hostility62[which(dat$Is_ValidCCAPS62==0)]=NA

  dat$Social_Anxiety62 <- apply(select (dat, c(CCAPS_03, CCAPS_49, CCAPS_39r, CCAPS_60r, CCAPS_52, CCAPS_19, CCAPS_46)), 1, mean, na.rm=T)
  dat$Social_Anxiety62[which(dat$Is_ValidCCAPS62==0)]=NA

  dat$Family62 <- apply(select (dat, c(CCAPS_25r, CCAPS_47, CCAPS_09r, CCAPS_01, CCAPS_14, CCAPS_43)), 1, mean, na.rm=T)
  dat$Family62[which(dat$Is_ValidCCAPS62==0)]=NA

  dat$Academics62 <- apply(select (dat, c(CCAPS_59, CCAPS_66, CCAPS_18r, CCAPS_08r, CCAPS_57)), 1, mean, na.rm=T)
  dat$Academics62[which(dat$Is_ValidCCAPS62==0)]=NA

  # Delete created vars after creating subscales
  dat <- select(dat, -c(Depression62_MISS:variance, CCAPS_18r:CCAPS_60r))
  return(dat)
}

