#' Title
#'
#' @return
#' @export
#'
#' @examples
#'
sds_to_factor <- function(data){
  if ("a" %in% names(data)) {
    data$a <- dplyr::recode(data$a, `1` = "One", `2` = "Two", `3` = "Three") %>% as.factor() %>% forcats::fct_relevel("One", "Two", "Three")
  } else {
    return(data)
  }
}
