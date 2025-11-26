#' Collapse data on case closure (select all that apply) across clients
#'
#' @name collapse_case_closure
#'
#' @description The case closure form could be filled out multiple times. This function ensures that if a client has multiple case closure records, all unique case closure reasons and events are captured in a single row for that client. This only applies to the select all that apply questions.
#'
#' @param data A data frame that contains data on Case Closure.
#' @param single.course A logical statement indicating whether data pertains to a single therapy course. If `TRUE`, the function assumes that each client has only one therapy course. If `FALSE`, the function will account for multiple therapy courses per client. By default, `TRUE`.
#'
#' @return Returns a data frame where data on Case Closure is collapsed across clients.
#' 
#' @export
#'

collapse_case_closure <- function(data, 
                                  single.course = TRUE){
  
  # Single course
  if(single.course == TRUE){

    # Check that required columns exist
    required_cols <- c("UniqueClientID", "CcmhID", 
                       "Has_Closure")
  
    missing_cols <- setdiff(required_cols, colnames(data))

    if(length(missing_cols) > 0){

      stop(paste("The following required columns are missing from the data:", paste(missing_cols, collapse = ", ")))

    } else{

    }

    # Collapse data on Case Closure across clients
    data_collapsed <- data |>
      dplyr::select(UniqueClientID, CcmhID, 
                    Has_Closure,
                    dplyr::starts_with("Closure_01"),
                    dplyr::starts_with("Closure_04")) |>
      dplyr::filter(Has_Closure == 1) |>
      dplyr::summarise(Has_Closure = 1,
                       dplyr::across(dplyr::starts_with("Closure_01"), ~ as.integer(any(. == 1, na.rm = TRUE))),
                       dplyr::across(dplyr::starts_with("Closure_04"), ~ as.integer(any(. == 1, na.rm = TRUE))), 
                       .by = c("UniqueClientID", "CcmhID"))
    
  } else{

    # Check that required columns exist
    required_cols <- c("UniqueClientID", "CcmhID", 
                      "Has_Closure", "RankCourse")
  
    missing_cols <- setdiff(required_cols, colnames(data))

    if(length(missing_cols) > 0){

      stop(paste("The following required columns are missing from the data:", paste(missing_cols, collapse = ", ")))

    } else{

    }

    # Collapse data on Case Closure across clients
    data_collapsed <- data |>
      dplyr::select(UniqueClientID, CcmhID, 
                    Has_Closure, RankCourse,
                    dplyr::starts_with("Closure_01"),
                    dplyr::starts_with("Closure_04")) |>
      dplyr::summarise(Has_Closure = 1,
                       dplyr::across(dplyr::starts_with("Closure_01"), ~ as.integer(any(. == 1, na.rm = TRUE))),
                       dplyr::across(dplyr::starts_with("Closure_04"), ~ as.integer(any(. == 1, na.rm = TRUE))), 
                       .by = c("UniqueClientID", "CcmhID", "RankCourse"))
    
  }

  # Recode 0 to NA
  data_collapsed <- data_collapsed |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Closure_01"), ~ dplyr::na_if(., 0))) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Closure_04"), ~ dplyr::na_if(., 0)))
  
  # Return data
  return(data_collapsed)
  
}