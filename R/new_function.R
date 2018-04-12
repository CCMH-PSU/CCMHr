#' A New Function
#'
#' Just checking to see if github is properly linked to this project
#' @param n The number of standard normal random variates you want to generate.
#' @keywords Histogram, Normal Random Variable
#' @description Takes an integer-valued argument and generates that many standard normal random variables, then plots a histogram of these random variates.
#' @return A pretty vanilla looking histogram of standard normal random variables.



new_function <- function(n){
  stopifnot(n>0,is.integer(n))
  x <- rnorm(n)
  hist(x,xlab = "Binned Values",ylab = "Frequency",main = "A Generic Histogram")
}
