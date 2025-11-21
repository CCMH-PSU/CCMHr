#' Recode the multi-select responses for race/ethnicity into a single variable.
#'
#' @name recode_race_ethnicity
#'
#' @description Creates a single variable that recodes multi-select responses for race/ethnicity into a single variable. Those who selected multiple races/ethnicities are categorized as "Multi-racial".
#'
#' @param data A data file containing race/ethnicity variables (e.g., SDS_1095_01).
#'
#' @return Outputs the original dataset along with a new variable, SDS_1095, that recodes multi-select responses for race/ethnicity into a single variable.
#'
#' @export
#' 

recode_race_ethnicity <- function(data){

  # Check if required columns exist
    # Requared columns
    required_cols <- c("SDS_1095_01", "SDS_1095_02", 
                       "SDS_1095_03", "SDS_1095_04", 
                       "SDS_1095_05", "SDS_1095_06", 
                       "SDS_1095_07", "SDS_1095_08",
                       "SDS_1095_multi")
  
    # Missing columns
    missing_cols <- setdiff(required_cols, names(data))
  
    # Message for missing columns
    if(length(missing_cols) > 0){

      stop(paste("The following required columns are missing from the dataset:", paste(missing_cols, collapse = ", ")))

    } else {

    }
  
  # Check if SDS_1095 exists
  if("SDS_1095" %in% names(data)){
  
    stop("The variable 'SDS_1095' already exists in the dataset.")
  
  } else {
  
  }

  # Recode race/ethnicity
  data$SDS_1095 <- ifelse(data$SDS_1095_multi == 1, "Multi-racial", 
    ifelse(data$SDS_1095_01 == 1 & data$SDS_1095_multi != 1, "African American / Black",
      ifelse(data$SDS_1095_02 == 1 & data$SDS_1095_multi != 1, "American Indian or Alaska Native",
        ifelse(data$SDS_1095_03 == 1 & data$SDS_1095_multi != 1, "Asian American / Asian",
          ifelse(data$SDS_1095_04 == 1 & data$SDS_1095_multi != 1, "Hispanic / Latino/a/e",
            ifelse(data$SDS_1095_05 == 1 & data$SDS_1095_multi != 1, "Middle Eastern / North African",
              ifelse(data$SDS_1095_06 == 1 & data$SDS_1095_multi != 1, "Native Hawaiian or Pacific Islander",
                ifelse(data$SDS_1095_07 == 1 & data$SDS_1095_multi != 1, "White",
                  ifelse(data$SDS_1095_08 == 1 & data$SDS_1095_multi != 1, "Self-identify (please specify)", 
                    NA_character_)))))))))

  # Return data
  return(data)

}