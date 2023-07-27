#' A function to create standardized CCMH box plot's
#'
#' @description Creates standardized CCMH box plot's using ggplot2.
#' @param data A data file containing all variables that will be used in the plot
#' @param x.var A quoted string to indicate the variable that will be plotted on the x-axis. The variable class must be a character or factor.
#' @param y.var A quoted string to indicate the variable that will be plotted on the y axis. The variable class must be numeric.
#' @param group.var A quoted string to indicate the variable that will group the boxes on the x-axis and specify the color of the boxes. The x-axis variable could used to specify colors of each individual boxes. By default, `""`.
#' @param color A hex code or list of hex codes that indicates the color of the grouped boxes. See Note 1 for more details. By default, `"#838B8B"` or a shade of dark grey.
#' @param x.order.manual A list of quoted strings to indicate the order of the x axis. By default, `NULL`.
#' @param save A logical statement indicates whether the plot should be saved as a file under a local folder. If false, the plot will be returned as an object. By default, `FALSE`.
#' @param path A quoted string to indicate the file's pathway and name if `save = TRUE`. By default, `"plot.png"`.
#' @param plot.width A numeric value to indicate the plot's width. By default, `30`.
#' @param plot.height A numeric value to indicate the plot's height. By default, `20`.
#' @param plot.units A quoted string to indicate the plot's width and height size units. Options include `in`, `cm`, `mm`, and `px`. By default, `cm`.
#' @param plot.dpi A numeric value to indicate the plot's image resolution. By default, `320`.
#' @param mean.sym A logical statement specifies whether a point should be displayed to indicate the mean. By default, `FALSE`.
#' @param mean.sym.shape A numeric value to indicate the mean point shape. By default, `3` or Crosshair.
#' @param mean.sym.size A numeric value to indicate the mean point size. By default, `2`.
#' @param mean.sym.color A hex code to indicate the mean point color. By default, `"#000000"` or black.
#' @param mean.sym.position A numeric value to indicate the mean point position. By default, `0.75`.
#' @param plot.title A quoted string to indicate the title of the plot. By default, `""`.
#' @param x.title A quoted string to indicate the x axis title. By default, `""`.
#' @param y.title A quoted string to indicate the y axis title. By default, `""`.
#' @param plot.title.size A numeric value to indicate the plot title text size. By default, `20`.
#' @param axis.title.size A numeric value to indicate the axis title text size. By default, `16`.
#' @param axis.label.size A numeric value to indicate the axis labels text size. By default, `14`.
#' @param text.font A quoted string to indicate the plot's text font. By default, `Avenir`.
#' @param y.label.percent A logical statement to indicate whether the y axis labels are percentages. By default, `FALSE`.
#' @param y.min A numeric value to indicate the minimum number presented on the y axis. By default, `NULL`.
#' @param y.max A numeric value to indicate the maximum number presented on the y axis. By default, `NULL`.
#' @param x.wrap A numeric value to indicate the number of characters to wrap the x axis text. By default, `15`.
#' @param x.remove A logical statement to indicate whether x-axis title, labels, and ticks should be removed. By default, `FALSE`.
#' @param caption A logical statement to indicate whether the CCMH caption should be included in the plot. By default, `FALSE`.
#' @param caption.size A numeric value to indicate the caption text size. By default, `12`.
#' @param legend.position A quoted string or numeric vector to indicate the location of the legend. Options include `"left"`,`"top"`, `"right"`, `"bottom"`, `"none"`, or numeric vector c(x,y). By default, `"none"`.
#' @param legend.order.manual A list of quoted strings to indicate the order of the legend. By default, `NULL`.
#' @param legend.title A quoted string to indicate the legend title. By default, `""`.
#' @param legend.title.size A numeric value to indicate the legend title text size. By default, `16`.
#' @param legend.label.size A numeric value to indicate the legend label text size. By default, `14`.
#' @param coord.flip A logical statement to indicate if the plot's x and y axis should be flipped. By default, `FALSE`.
#' @param plot.element1 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' @param plot.element2 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' @param plot.element3 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' @param plot.element4 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' @param plot.element5 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#'
#' @note Note 1. The argument `color` specifies the color of items within the grouped variable. For example, the group variable items could be the name of automobile companies (i.e., Ford, Dodge, BMV). You can specify color randomly for each company using this list: `c("#21501b", "#c51329", "#074e67")`. Or you can specify color directly to each specific company using this list: `c("Ford" = "#21501b", "Dodge" = "#c51329", "BMV" = "#074e67")`.
#' @note The `plot.element` arguments specify additional ggplot2 graphical element(s) needed to complete a specific box plot, but are not specified as an argument in the `box_plot` function. For example, the axis label text will always be black unless specified in one of the `plot.element` arguments. To change the axis label text color in one of the `plot.element` arguments, you would first create an object to represent graphing element (e.g., green.axis.text <- ggplot2::theme(axis.text = ggplot2::element_text(color = "green"))). Then the object would be added to one of the `plot.element` arguments (e.g., `plot.element01 = green.axis.text`).
#' @return A box plot returned as a object or saved under a local file.
#' @export
#'

box_plot <- function(data,
                     x.var,
                     y.var,
                     group.var = "",
                     color = "#838B8B",
                     x.order.manual = NULL,
                     save = FALSE,
                     path = "plot.png",
                     plot.width = 20,
                     plot.height = 20,
                     plot.units = "cm",
                     plot.dpi = 320,
                     mean.sym = FALSE,
                     mean.sym.shape = 3,
                     mean.sym.size = 2,
                     mean.sym.color = "#000000",
                     mean.sym.position = 0.75,
                     plot.title = "",
                     x.title = "",
                     y.title = "",
                     plot.title.size = 20,
                     axis.title.size = 16,
                     axis.label.size = 14,
                     text.font = "Avenir",
                     y.label.percent = FALSE,
                     y.min = NULL,
                     y.max = NULL,
                     x.wrap = 15,
                     x.remove = FALSE,
                     caption = FALSE,
                     caption.size = 12,
                     legend.position = "none",
                     legend.order.manual = NULL,
                     legend.title = NULL,
                     legend.title.size= 16,
                     legend.label.size= 14,
                     coord.flip = FALSE,
                     plot.element1 = NULL,
                     plot.element2 = NULL,
                     plot.element3 = NULL,
                     plot.element4 = NULL,
                     plot.element5 = NULL){

  # Specify the x-axis variable as a symbol
    x.var1 <- rlang::sym(x.var)

  # Specify the y-axis variable as a symbol
    y.var1 <- rlang::sym(y.var)

    # Specify grouping variable
    if(group.var != ""){

      # Specify the fill variable as a symbol
        group.var1 <- rlang::sym(group.var)

      # Rename grouping variable
        data$group77d8214 <-  data[[{{group.var1}}]]

     } else{

     }

  # Remove rownames
    rownames(data) <- NULL

  # Error message to indicate the x.order.manual items does not match the items in x-axis variable
    if(!is.null(x.order.manual)){

      x.var <- unique(data[[{{x.var1}}]])
      test1 <- ifelse(x.var %in% x.order.manual, "TRUE", "FALSE")
      test2 <- ifelse(x.order.manual %in% x.var, "TRUE", "FALSE")
      test.final <- c(test1, test2)
      test.final <- FALSE %in% test.final

      if(test.final == TRUE){

        stop("Unique characters listed x.order.manual must match the characters of x-axis variable")

      } else{

      }
    } else{

    }

  # Error message to indicate the legend.order.manual items does not match the items in the filler variable
    if(!is.null(legend.order.manual) &
       group.var != ""){

      group.varz <- unique(data$group77d8214)
      test1 <- ifelse(group.varz %in% legend.order.manual, "TRUE", "FALSE")
      test2 <- ifelse(legend.order.manual %in% group.varz, "TRUE", "FALSE")
      test.final <- c(test1, test2)
      test.final <- FALSE %in% test.final

      if(test.final == TRUE){

        stop("Unique characters listed legend.order.manual must match the characters of fill variable")

      } else{

      }
    } else{

    }

  # Specify custom x axis order
    if(!is.null(x.order.manual)){

      data[[{{x.var1}}]] <- factor(data[[{{x.var1}}]],
                                   levels = c(x.order.manual))

    } else{

    }

  # Specify if the legend should be reordered
    if(!is.null(legend.order.manual) &
       group.var != ""){

      data$group77d8214 <- factor(data$group77d8214,
                                  levels = legend.order.manual)

    } else{

  }

  # Specify basic plot information and if grouping will occur
    plot.1 <- ggplot2::ggplot(data,
      {if(group.var == ""){

          aes(x = {{x.var1}},
              y = {{y.var1}},
              fill = color)

        } else{

          aes(x = {{x.var1}},
              y = {{y.var1}},
              fill = group77d8214)

        }})  +

  # Specify the plot as a box plot based on grouping
    {if(group.var == ""){

      ggplot2::geom_boxplot(fill = color)

     } else {

       ggplot2::geom_boxplot(ggplot2::aes(fill = group77d8214))

     }} +

    # Remove legend information based on alpha and specify fill color
      {if(group.var != "") {

        ggplot2::scale_fill_manual(values = color)

      } else{

      }
      } +

  # Specify if a mean point should be placed in the box plot
    {if(mean.sym == TRUE){

      stat_summary(fun = "mean",
                   position = position_dodge(width = mean.sym.position),
                   geom = "point",
                   shape = mean.sym.shape,
                   size = mean.sym.size,
                   color = mean.sym.color)}

      else {

    }}+

  # Specify legend information
    ggplot2::guides(color = ggplot2::guide_legend(byrow = TRUE,
                                                  override.aes = ggplot2::aes(label = ""))) +

  # Specify title information
    ggplot2::labs(title = plot.title,
                  y = y.title,
                  x = x.title,
                  fill = legend.title) +

  # Specify if the y-axis is a percentage
    {if(y.label.percent == TRUE){

      ggplot2::scale_y_continuous(labels = scales::percent,
                                  limits = c(y.min,
                                             y.max))

    } else {

      ggplot2::scale_y_continuous(limits = c(y.min,
                                             y.max))

    }} +

  # Specify the plots theme information
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = 'white',
                                                            color = NA),
                   text = ggplot2::element_text(family = text.font),
                   plot.title = ggplot2::element_text(size = plot.title.size,
                                                      hjust = 0.5),
                   axis.title = ggplot2::element_text(size = axis.title.size),
                   axis.text = ggplot2::element_text(size = axis.label.size),
                   axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0,
                                                                                 r = 7,
                                                                                 b = 0,
                                                                                 l = 0),
                                                        angle = 0,
                                                        vjust = 0.5),
                   axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 7,
                                                                                 r = 0,
                                                                                 b = 0,
                                                                                 l = 0)),
                   axis.line.x = ggplot2::element_line(colour = 'black',
                                                       linewidth=0.5,
                                                       linetype='solid'),
                   axis.line.y = ggplot2::element_line(colour = 'black',
                                                       linewidth=0.5,
                                                       linetype='solid'),
                   plot.caption = ggplot2::element_text(size = caption.size),
                   legend.position = legend.position,
                   legend.background = ggplot2::element_rect(fill = "white",
                                                             colour = "white",
                                                             inherit.blank = TRUE,
                                                             color = "white"),
                   legend.title = ggplot2::element_text(size= legend.title.size),
                   legend.text = ggplot2::element_text(size = legend.label.size)) +

  # Specify x axis label text wrap
    scale_x_discrete(labels = scales::wrap_format(x.wrap)) +

  # Specify if x axis should be removed
    {if(x.remove == TRUE){

      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank())

    } else{

    }} +

  # Specify if the coords should be flipped
    {if(coord.flip == TRUE){

      ggplot2::coord_flip()

      } else{

      }
     } +

  # Specify plot elements
    plot.element1 +
    plot.element2 +
    plot.element3 +
    plot.element4 +
    plot.element5

  # Specify caption
    {if(caption == TRUE){

      plot.1 <-plot.1 + CCMHr::ccmh_caption()

    } else{

    }}

  # Specify if the graph should be saved as file or returned as an object
    if(save == TRUE){

      ggsave(paste0(path),
             plot = plot.1,
             width = plot.width,
             height = plot.height,
             units = plot.units,
             dpi = plot.dpi)

    } else{

      return(plot.1)

    }

}
