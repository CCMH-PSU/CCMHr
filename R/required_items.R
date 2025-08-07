#' Detects whether specified variables are present in the dataset.
#'
#' @description The function checks if specified variables are present in the dataset and presents an error message when specified variables are not present. This function is often used within other CCMHr functions when a specific variable is required to be present.
#'
#' @param data A data frame.
#' @param items A quoted string or list of quoted strings that specifies variable names required in a data frame.
#' @param message A quoted string of the returned error message when the specified variable(s) are missing from a data frame. By default, `"The following variable(s) need to be present in the data frame:"` and a list of the missing variables.
#'
#' @return When all the required variables are present, nothing is returned. When at least one required variable is not present, an error message is returned. The default error message is `"The following variable(s) need to be present in the data frame:"` and a list of the missing variables.
#'
#' @importFrom dplyr filter
#'
#' @export

required_items <- function(data,
                           items,
                           message = "The following variable(s) need to be present in data:") {

  # For loops to detect for missing variables in a data frame
    # Parameters
      # Number of loops
      loop.num <- dim(data.frame(items))[1]

      # Creating a data frame of specified variables
      df.detect.miss <- NULL
      df.detect.miss$var_names <- items
      df.detect.miss <- data.frame(df.detect.miss)

      # Making variable NA for loop
      df.detect.miss$missing <- NA

    # For Loop
    for(i in 1:loop.num){

      #Creating a vector to detect if there are missing variables
      df.detect.miss$missing[i] <- data.frame(items[[i]]) %in% names(data)

    }

  # Warning Message displayed if specified variable(s) are missing
    # Removing present variables
    df.detect.miss <- dplyr::filter(df.detect.miss,
                                    .data$missing == FALSE)

    # Cleaning up list of missing variables
    missing.vars <- toString(df.detect.miss$var_names)

    # Display message
    if(nrow(df.detect.miss) > 0){

      # Message
      stop(paste0(message, " ", missing.vars))

    } else{

    }

}
