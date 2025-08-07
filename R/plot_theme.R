#' Format a ggplot2 object with the CCMH theme.
#'
#' @description This function will reformat a plot created by ggplot2 with basic CCMH theming. The theming relates to formatting text (i.e., font type, size) and background.
#'
#' @param plot_title A numeric value to indicate the plot title text size. By default, `20`.
#' @param axis_title A numeric value to indicate the axis title text size. By default, `16`.
#' @param axis_text A numeric value to indicate the axis labels text size. By default, `14`.
#' @param caption_text A numeric value to indicate the caption text size. By default, `12`.
#' @param strip_text A numeric value to indicate the facet strip text size. By default, `14`.
#' @param text_font A quoted string to indicate the plot's text font. By default, `"Avenir"`.
#'
#' @return A ggplot2 object.
#'
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 margin
#' @importFrom ggplot2 element_rect
#'
#' @export

ccmh_theme <- function(plot_title = 20,
                       axis_title = 16,
                       axis_text = 14,
                       caption_text = 12,
                       strip_text = 14,
                       text_font = "Avenir"){

  # Setting the theme
  ggplot2::theme_minimal() +
  ggplot2::theme(text = ggplot2::element_text(family = text_font),
                 axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0,
                                                                               r = 7,
                                                                               b = 0,
                                                                               l = 0)),
                 axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 7,
                                                                               r = 0,
                                                                               b = 0,
                                                                               l = 0)),
                 plot.title = ggplot2::element_text(size = plot_title),
                 axis.title = ggplot2::element_text(size = axis_title),
                 axis.text = ggplot2::element_text(size = axis_text),
                 strip.text = ggplot2::element_text(size = strip_text),
                 plot.caption = ggplot2::element_text(size = caption_text),
                 plot.background = ggplot2::element_rect(fill = 'white',
                                                         color = NA))

}

#' Format a ggplot2 object to add CCMH caption.
#'
#' @description This function will add CCMH's caption to a ggplot2 object.
#'
#' @return A ggplot2 object.
#'
#' @importFrom ggplot2 labs
#'
#' @export

ccmh_caption <- function(){

  ggplot2::labs(caption = "Source: The Center for Collegiate Mental Health")

}
