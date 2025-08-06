#' Creates a box plot graph.
#'
#' @name plot_box
#'
#' @description This function creates a box plot graph.
#'
#' @param data A data file containing variables that will be used in the plot.
#' @param x.var A quoted or unquoted string indicates the variable that will be plotted on the x-axis. The variable class must be a character or factor.
#' @param y.var A quoted or unquoted string indicates the variable that will be plotted on the y-axis. The variable class must be numeric.
#' @param group.var A quoted string to indicate the variable that will group data/boxes on the x-axis. By default, `""`.
#' @param color A hex code or list of hex codes that indicates the color(s) of the boxes. If a variable groups the data, the argument specifies the color of the boxes or items within the grouped variable. For example, the group variable items could be the names of automobile companies (i.e., Ford, Dodge, BMW). You can specify a random color for each company using this list: `c("#21501b", "#c51329", "#074e67")`. Alternatively, you can specify the color directly for each company using this list: `c("Ford" = "#21501b", "Dodge" = "#c51329", "BMW" = "#074e67")`. The number of hex codes must be equal to or greater than the number of responses in the grouped data. If data is not grouped, all the boxes will have the same color. The default colors are color-blind safe. By default, `CCMHr::CCMH_categorical_palettes$tolmuted_category10`.
#' @param x.order.manual A list of quoted strings to indicate the order of the x-axis items. By default, `NULL`.
#' @param save A logical argument that indicates whether the plot should be saved as a file under a local folder. If `FALSE`, the plot will be returned as an object. By default, `FALSE`.
#' @param path A quoted string to indicate the file's pathway, name, and type if `save = TRUE`. By default, `"plot.png"`.
#' @param plot.width A numeric value to indicate the plot's width. By default, `12`.
#' @param plot.height A numeric value to indicate the plot's height. By default, `9`.
#' @param plot.units A quoted string to indicate the plot's width and height in size units. Options include `in`, `cm`, `mm`, and `px`. By default, `in`.
#' @param plot.dpi A numeric value to indicate the plot's image resolution. By default, `360`.
#' @param plot.device A quoted string or function to indicate the plot's device. If saving the plot as a PDF, use `cairo_pdf`. By default, `NULL`.
#' @param plot.scale A numeric value to indicate the plot's scale. By default, `1`.
#' @param mean.sym A logical argument that indicates whether a symbol should be displayed to indicate the mean within a box. If `TRUE`, the mean symbol is displayed. By default, `FALSE`.
#' @param mean.sym.shape A numeric value to indicate the mean symbol shape. By default, `3` or Crosshair.
#' @param mean.sym.size A numeric value to indicate the mean symbol size. By default, `2`.
#' @param mean.sym.color A hex code to indicate the mean symbol color. By default, `"#000000"` or black.
#' @param mean.sym.position A numeric value to indicate the mean symbol position. By default, `0.75`.
#' @param plot.title A quoted string to indicate the title of the plot. By default, `NULL`.
#' @param x.title A quoted string to indicate the x-axis title. By default, `NULL`.
#' @param y.title A quoted string to indicate the y-axis title. By default, `NULL`.
#' @param plot.title.size A numeric value to indicate the plot title text size. By default, `20`.
#' @param axis.title.size A numeric value to indicate the axis title text size. By default, `16`.
#' @param axis.label.size A numeric value to indicate the axis labels text size. By default, `14`.
#' @param text.font A quoted string to indicate the plot's text font. By default, `"Avenir"`.
#' @param y.label.type A quoted string or list of quoted strings to specify the format of the y-axis labels. Options include `"numeric"`, `"percent"`, `"comma"`, or `"sci"`. By default, `"numeric"`.
#' @param y.min A numeric value to indicate the minimum number presented on the y-axis. By default, `NULL`.
#' @param y.max A numeric value to indicate the maximum number presented on the y-axis. By default, `NULL`.
#' @param y.breaks Numeric values or a sequence of numeric values that indicate breaks on the y-axis. By default, `NULL`.
#' @param y.expand A list of two numeric values to indicate the expansion of the y-axis. By default, `ggplot2::waiver()`.
#' @param x.wrap A numeric value to indicate the number of characters to wrap the x-axis text. By default, `15`.
#' @param x.remove A logical argument to indicate whether y-axis information should be removed. If `TRUE`, the y-axis title, labels, and ticks will be removed. By default, `FALSE`.
#' @param caption A logical argument to indicate whether the CCMH caption should be displayed on the bottom right of the plot. If `TRUE`, the caption "Source: The Center for Collegiate Mental Health" is displayed on the bottom right of the plot. By default, `FALSE`.
#' @param caption.size A numeric value to indicate the caption text size. By default, `12`.
#' @param caption.vjust A numeric value to indicate the caption vertical adjustment. By default, `-3`.
#' @param legend.position A quoted string or numeric vector to indicate the location of the legend. Options include `"left"`,`"top"`, `"right"`, `"bottom"`, `"none"`, or numeric vector c(x,y). By default, `"none"`.
#' @param legend.order.manual A list of quoted strings to indicate the order of the legend. By default, `NULL`.
#' @param legend.title A quoted string to indicate the legend title. By default, `NULL`.
#' @param legend.title.size A numeric value to indicate the legend title text size. By default, `16`.
#' @param legend.label.size A numeric value to indicate the legend label text size. By default, `14`.
#' @param coord.flip A logical argument to indicate if the plot's x and y-axis should be flipped. If `TRUE`, the x and y axis is flipped. By default, `FALSE`.
#' @param y.grid.major A logical argument to indicate whether the y-axis major grid lines should be added to the plot. If `TRUE`, the y-axis major grid lines are displayed on the plot. By default, `FALSE`.
#' @param y.grid.major.color A hex code that indicates the color of the y-axis major grid lines. By default, `"#d3d3d3"` or a shade of light grey.
#' @param y.grid.major.size A numeric value to indicate the thickness of the y-axis major grid lines. By default, `0.5`.
#' @param y.grid.major.linetype A numeric value to indicate the line type of the y-axis major grid. By default, `1`.
#' @param y.grid.minor A logical argument to indicate whether the y-axis minor grid lines should be added to the plot. If `TRUE`, the y-axis minor grid lines are displayed on the plot. By default, `FALSE`.
#' @param y.grid.minor.color A hex code that indicates the color of the y-axis minor grid lines. By default, `"#d3d3d3"` or a shade of light grey.
#' @param y.grid.minor.size A numeric value to indicate the thickness of the y-axis minor grid lines. By default, `0.25`.
#' @param y.grid.minor.linetype A numeric value to indicate the line type of the y-axis minor grid. By default, `1`.
#' @param y.axis.line A logical argument to indicate whether the y-axis line should be included. If `FALSE`, the y-axis line will not be displayed. By default, `TRUE`.
#' @param x.axis.line A logical argument to indicate whether the x-axis line should be included. If `FALSE`, the x-axis line will not be displayed. By default, `TRUE`.
#' @param remove.axis.ticks A logical argument to indicate whether the axis ticks should be excluded. If `TRUE`, the axis ticks will not be displayed on the plot. By default, `FALSE`.
#' @param plot.element1 A ggplot2 plot function and arguments are needed to add graphical elements  to complete a specific task, but are not specified as arguments in the function. For example, the axis label text color will always be black unless specified in one of the plot.element arguments (i.e., plot.element1 to plot.element9). To change the axis label text color, one of the plot.element arguments should be specified as follows: `plot.element1 = ggplot2::theme(axis.text = ggplot2::element_text(color = "green"))`. By default, `NULL`.
#' @param plot.element2 A ggplot2 plot function and arguments are needed to add graphical elements  to complete a specific task, but are not specified as arguments in the function. By default, `NULL`.
#' @param plot.element3 A ggplot2 plot function and arguments are needed to add graphical elements  to complete a specific task, but are not specified as arguments in the function. By default, `NULL`.
#' @param plot.element4 A ggplot2 plot function and arguments are needed to add graphical elements  to complete a specific task, but are not specified as arguments in the function. By default, `NULL`.
#' @param plot.element5 A ggplot2 plot function and arguments are needed to add graphical elements  to complete a specific task, but are not specified as arguments in the function. By default, `NULL`.
#' @param plot.element6 A ggplot2 plot function and arguments are needed to add graphical elements  to complete a specific task, but are not specified as arguments in the function. By default, `NULL`.
#' @param plot.element7 A ggplot2 plot function and arguments are needed to add graphical elements  to complete a specific task, but are not specified as arguments in the function. By default, `NULL`.
#' @param plot.element8 A ggplot2 plot function and arguments are needed to add graphical elements  to complete a specific task, but are not specified as arguments in the function. By default, `NULL`.
#' @param plot.element9 A ggplot2 plot function and arguments are needed to add graphical elements  to complete a specific task, but are not specified as arguments in the function. By default, `NULL`.
#'
#' @return A box plot is returned as an object or saved as a file in a local directory.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rlang enquo
#' @importFrom rlang as_name
#' @importFrom rlang sym
#' @importFrom scales percent
#' @importFrom scales comma
#' @importFrom scales scientific
#' @importFrom scales dollar
#' @importFrom scales wrap_format
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 waiver
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 stat_summary
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 margin
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 ggsave
#'
#' @export

plot_box <- function(data,
                     x.var,
                     y.var,
                     group.var = "",
                     color = CCMHr::CCMH_categorical_palettes$tolmuted_category10,
                     x.order.manual = NULL,
                     save = FALSE,
                     path = "plot.png",
                     plot.width = 12,
                     plot.height = 9,
                     plot.units = "in",
                     plot.dpi = 360,
                     plot.device = NULL,
                     plot.scale = 1,
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
                     y.label.type = "numeric",
                     y.min = NULL,
                     y.max = NULL,
                     y.breaks = NULL,
                     y.expand = ggplot2::waiver(),
                     x.wrap = 15,
                     x.remove = FALSE,
                     caption = FALSE,
                     caption.size = 12,
                     caption.vjust = -3,
                     legend.position = "none",
                     legend.order.manual = NULL,
                     legend.title = NULL,
                     legend.title.size= 16,
                     legend.label.size= 14,
                     coord.flip = FALSE,
                     y.grid.major = FALSE,
                     y.grid.major.color = "#d3d3d3",
                     y.grid.major.size = 0.5,
                     y.grid.major.linetype = 1,
                     y.grid.minor = FALSE,
                     y.grid.minor.color = "#d3d3d3",
                     y.grid.minor.size = 0.25,
                     y.grid.minor.linetype = 1,
                     y.axis.line = TRUE,
                     x.axis.line = TRUE,
                     remove.axis.ticks = FALSE,
                     plot.element1 = NULL,
                     plot.element2 = NULL,
                     plot.element3 = NULL,
                     plot.element4 = NULL,
                     plot.element5 = NULL,
                     plot.element6 = NULL,
                     plot.element7 = NULL,
                     plot.element8 = NULL,
                     plot.element9 = NULL){

  # Specify data as a data frame
  data <- as.data.frame(data)

  # Specify the x-axis variable as a symbol
  x.var1 <- rlang::as_name(rlang::enquo(x.var))
  x.var1 <- rlang::sym(x.var1)

  # Specify the y-axis variable as a symbol
  y.var1 <- rlang::as_name(rlang::enquo(y.var))
  y.var1 <- rlang::sym(y.var1)

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

  # Specify Y Major Grid
  if(y.grid.major == TRUE){

    grid.maj <- ggplot2::element_line(color = y.grid.major.color,
                                      linewidth = y.grid.major.size,
                                      linetype = y.grid.major.linetype)

    } else{

    grid.maj <- ggplot2::element_blank()

  }

  # Specify Y breaks
  if(is.null(y.breaks)){

    y.breaks.c <- ggplot2::waiver()

    } else {

    y.breaks.c <- y.breaks

  }

  # Specify Y Minor Grid
  if(y.grid.minor == TRUE){

    grid.min <- ggplot2::element_line(color = y.grid.minor.color,
                                      linewidth = y.grid.minor.size,
                                      linetype = y.grid.minor.linetype)

    } else {

    grid.min <- ggplot2::element_blank()

  }

  # Specify y axis lables
  if(y.label.type == "percent"){

    y.label.typea <- scales::percent

    } else if(y.label.type == "comma"){

    y.label.typea <- scales::comma

    } else if(y.label.type == "numeric"){

    y.label.typea <- ggplot2::waiver()

    } else if(y.label.type == "sci"){

    y.label.typea <- scales::scientific

    } else if(y.label.type == "dollar"){

    y.label.typea <- scales::dollar

    } else {

    y.label.typea <- NULL

  }

  # Specify y axis line
  if(y.axis.line == TRUE){

    y.axis.line.c <- ggplot2::element_line(colour = 'black',
                                           linewidth=0.5,
                                           linetype='solid')

    } else{

    y.axis.line.c <- ggplot2::element_blank()

  }

  # Specify x axis line
  if(x.axis.line == TRUE){

    x.axis.line.c <- ggplot2::element_line(colour = 'black',
                                           linewidth=0.5,
                                           linetype='solid')

    } else{

    x.axis.line.c <- ggplot2::element_blank()

  }

  # Specify color
  if(group.var == ""){

    color <- color[1]

    } else{

  }

  # Specify basic plot information and if grouping will occur
  plot.1 <- ggplot2::ggplot(data,
    {if(group.var == ""){

      ggplot2::aes(x = {{x.var1}},
                   y = {{y.var1}},
                   fill = color)

      } else{

      ggplot2::aes(x = {{x.var1}},
                   y = {{y.var1}},
                   fill = group77d8214)

      }
    }) +

  # Specify the plot as a box plot based on grouping
  {if(group.var == ""){

    ggplot2::geom_boxplot(fill = color)

    } else{

    ggplot2::geom_boxplot(ggplot2::aes(fill = group77d8214))

    }
  } +

  # Remove legend information based on alpha and specify fill color
  {if(group.var != ""){

    ggplot2::scale_fill_manual(values = color)

    } else{

    }
  } +

  # Specify if a mean point should be placed in the box plot
  {if(mean.sym == TRUE){

    ggplot2::stat_summary(fun = "mean",
                          position = ggplot2::position_dodge(width = mean.sym.position),
                          geom = "point",
                          shape = mean.sym.shape,
                          size = mean.sym.size,
                          color = mean.sym.color)}

    else {

    }
  }+

  # Specify legend information
  ggplot2::guides(color = ggplot2::guide_legend(byrow = TRUE,
                                                override.aes = ggplot2::aes(label = ""))) +

  # Specify title information
  ggplot2::labs(title = plot.title,
                y = y.title,
                x = x.title,
                fill = legend.title) +

  # Specify y-axis scale
  ggplot2::scale_y_continuous(labels = y.label.typea,
                              limits = c(y.min,
                                         y.max),
                              expand = y.expand,
                              breaks = y.breaks.c)+

  # Specify the plots theme information
  ggplot2::theme(panel.grid = ggplot2::element_blank(),
                 panel.grid.major.y = grid.maj,
                 panel.grid.minor.y = grid.min,
                 panel.background = ggplot2::element_rect(fill = 'white',
                                                          color = NA),
                 plot.margin = ggplot2::margin(t = 0.5,  # Top margin
                                               r = 0.5,  # Right margin
                                               b = 0.5,  # Bottom margin
                                               l = 0.5,  # Left margin
                                               unit = "cm"),
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
                 axis.line.x = x.axis.line.c,
                 axis.line.y = y.axis.line.c,
                 plot.caption = ggplot2::element_text(size = caption.size,
                                                      vjust = caption.vjust),
                 legend.position = legend.position,
                 legend.background = ggplot2::element_rect(fill = "white",
                                                           colour = "white",
                                                           inherit.blank = TRUE,
                                                           color = "white"),
                 legend.key = ggplot2::element_blank(),
                 legend.title = ggplot2::element_text(size= legend.title.size),
                 legend.text = ggplot2::element_text(size = legend.label.size)) +

  # Specify x axis label text wrap
  ggplot2::scale_x_discrete(labels = scales::wrap_format(x.wrap)) +

  # Specify if x axis should be removed
  {if(x.remove == TRUE){

    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())

    } else{

    }
  } +

  # Specify to remove axis ticks
  {if(remove.axis.ticks == TRUE){

    ggplot2::theme(axis.ticks = ggplot2::element_blank())

    } else{

    }
  } +

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
  plot.element5 +
  plot.element6 +
  plot.element7 +
  plot.element8 +
  plot.element9

  # Specify caption
  {if(caption == TRUE){

    plot.1 <- plot.1 + CCMHr::ccmh_caption()

    } else{

    }
  }

  # Specify if the graph should be saved as file or returned as an object
  if(save == TRUE){

    ggplot2::ggsave(paste0(path),
                    plot = plot.1,
                    width = plot.width,
                    height = plot.height,
                    units = plot.units,
                    dpi = plot.dpi,
                    device = plot.device,
                    scale = plot.scale)

    } else{

    return(plot.1)

  }

}

#' @rdname plot_box
#' @export
box_plot <- plot_box
