#' Displays a error message when specified variables are not present in data
#'
#' @param data A data frame.
#' @param items Specify the variables that must be present in a data frame to use a function or run an R script. Variables should be listed using the c() function, and each variable name should be quoted. For example, "items = c("Var1", "Var2").
#' @param message The message that returns when specified variable(s) are missing. By default, "The following variable(s) need to be present in data:"
#'
#' @return A error message if specified variable is missing.
#'


required_items <- function(data,
                           items,
                           message = "The following variable(s) need to be present in data:") {

  #For loops to detect for missing variables in a data frame
    #Parameters
      #Number of loops
        loop.num <- dim(data.frame(items))[1]
      #Creating a data frame of specified variables
        df.detect.miss <- NULL
        df.detect.miss$var_names <- items
        df.detect.miss <- data.frame(df.detect.miss)
      #Making variable NA for loop
        df.detect.miss$missing <- NA

    #For Loop
      for(i in 1:loop.num){
        #Creating a vector to detect if there are missing variables
          df.detect.miss$missing[i] <- data.frame(items[[i]]) %in% names(data)
      }

  #Warning Message displayed if specified variable(s) are missing
    #Removing present variables
      df.detect.miss <- subset(df.detect.miss,
                               df.detect.miss$missing == F)
    #Cleaning up list of missing variables
      missing.vars <- toString(df.detect.miss$var_names)
    #Display message
      if(nrow(df.detect.miss) > 0){
        #Message
          stop(paste0(message, " ", missing.vars))
      } else{

        }

}
