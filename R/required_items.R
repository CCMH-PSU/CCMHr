#' Displays a error message when specifed variables are not present in data
#'
#' @param data A data frame.
#' @param items Specify the variables that must be present in a data frame to use a function or run an R script. Variables should be listed using the c() function and each variable name should be quoted. For example, "items = c("Var1", "Var2").
#' @param message The message that returns when specified variables are missing. By default, "The following variable(s) need to be present in data:"
#'
#' @return A data frame with CCAPS change scores of subscales and specified variables. Based on additional arguments, scores from first and last completion could be added to the data frame.
#' @export
#'


required_items <- function(data,
                           items,
                           message = "The following variable(s) need to be present in data:") {

  #For loops to detect for missing variables
    #Parameters
      #Number of loops
        loop.num <- dim(data.frame(items))[1]
      #Data frame for missing variables
        df.detect.miss <- NULL
        df.detect.miss$var_names <- items
        df.detect.miss <- data.frame(df.detect.miss)
        df.detect.miss$missing <- NA

  #Loops
    for(i in 1:loop.num){
      df.detect.miss$missing[i] <- data.frame(items[[i]]) %in% names(data)
    }

  #Warning Message
    #Removing present variables
      df.detect.miss <- subset(df.detect.miss,
                               df.detect.miss$missing == F)

      missing.vars <- toString(df.detect.miss$var_names)

  #Warning message displayed if there are missing variables
    if(nrow(df.detect.miss) > 0){
      stop(paste0(message, " ", missing.vars))
    } else{

      }

}
