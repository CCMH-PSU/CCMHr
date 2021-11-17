#' CCMH Plot Theme and Attribution Caption
#'
#' @description
#'   `ccmh_theme` Sets the font to Avenir and adds space between the axis titles and the plot. \cr
#'   `ccmh_caption` adds at attribution caption to the plot ("Source: The Center for Collegiate Mental Health")
#'
#' @param plot_title The font size of the plot title
#' @param axis_title The font size of the axis title
#' @param axis_text The font size of the axis labels
#' @param caption_text The font size for the caption
#'
#' @export
#'
ccmh_theme <- function(plot_title = 20, axis_title = 16, axis_text = 14, caption_text = 12) {
  ggplot2::theme_minimal() +
  ggplot2::theme(text = ggplot2::element_text(family = "Avenir"),
                 axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 7, b = 0, l = 0)),
                 axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 7, r = 0, b = 0, l = 0)),
                 plot.title = element_text(size = plot_title),
                 axis.title = element_text(size = axis_title),
                 axis.text = element_text(size = axis_text),
                 plot.caption = element_text(size = caption_text),
                 plot.background = element_rect(fill = 'white', color = NA))
}

#' @export
#' @rdname ccmh_theme

ccmh_caption <- function() {
  ggplot2::labs(caption = "Source: The Center for Collegiate Mental Health")
}
