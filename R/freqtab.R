#' Frequency table
#'
#' @param x A vector
#'
#' @return A frequency table with percentages and cumulative percentages.
#' @export
#'
#' @examples
#' df <- data.frame(y = c(1,1,1,2,2,3))
#' freqtab(df$y)
#'
freqtab <- function(x) {
  sym <- deparse(substitute(x))
  descr::freq(ordered(x), plot = F) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = sym)
}

