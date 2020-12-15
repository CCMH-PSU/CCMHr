#' CCMH Plot Theme
#'
#' @description Sets the font to Avenir and adds space between the axis titles and the plot.
#'
#' @export
#'
ccmh_theme <- function() {
  ggplot2::theme(text = ggplot2::element_text(family = "Avenir"),
                 axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 7, b = 0, l = 0)),
                 axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 7, r = 0, b = 0, l = 0)))
}
