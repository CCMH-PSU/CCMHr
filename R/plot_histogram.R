#' A function to create standardized CCMH histogram plot's
#'
#' @name plot_histogram
#' @description Creates standardized CCMH histogram plot's using ggplot2.
#' @param data A data file containing all variables that will be used in the plot
#' @param x.var A quoted string or unquoted characters to indicate the variable that will be plotted on the x-axis. The variable class must be a character or factor.
#' @param group.var A quoted string to indicate the variable that will used to specify grouping. By default, `""`.
#' @param color A hex code or list of hex codes that indicates the color based on grouping. See Note 1 for more details. By default, `c('#E6AB02', '#1B9E77', '#666666', '#D95F02', '#6db6ff', '#b66dff', '#ff6db6', '#920000')`.
#' @param y.axis.type A quoted string to indicate the type of information displayed on the y axis. Options include `count`, `density`, and `relative_freq`. By default, `count`.
#' @param hist.position A quoted string to indicate the histogram position adjustment. By default, `identity`.
#' @param save A logical statement indicates whether the plot should be saved as a file under a local folder. If false, the plot will be returned as an object. By default, `FALSE`.
#' @param path A quoted string to indicate the file's pathway and name if `save = TRUE`. By default, `"plot.png"`.
#' @param plot.width A numeric value to indicate the plot's width. By default, `1200`.
#' @param plot.height A numeric value to indicate the plot's height. By default, `900`.
#' @param plot.units A quoted string to indicate the plot's width and height size units. Options include `in`, `cm`, `mm`, and `px`. By default, `px`.
#' @param plot.dpi A numeric value to indicate the plot's image resolution. By default, `96`.
#' @param plot.device A quoted string or function to indicate the plot's device. By default, `NULL`.
#' @param plot.scale A numeric value to indicate the plot's scale. By default, `1`.
#' @param plot.title A quoted string to indicate the title of the plot. By default, `NULL`.
#' @param x.title A quoted string to indicate the x axis title. By default, `NULL`.
#' @param y.title A quoted string to indicate the y axis title. By default, `NULL`.
#' @param plot.title.size A numeric value to indicate the plot title text size. By default, `20`.
#' @param axis.title.size A numeric value to indicate the axis title text size. By default, `16`.
#' @param axis.label.size A numeric value to indicate the axis labels text size. By default, `14`.
#' @param text.font A quoted string to indicate the plot's text font. By default, `Avenir`.
#' @param y.label.type A quoted string or list to customize the y axis labels. Options include `"numeric"`, `"percent"`, `"comma"`, or `"sci`. By default, `numeric`.
#' @param y.min A numeric value to indicate the minimum number presented on the y axis. By default, `NULL`.
#' @param y.max A numeric value to indicate the maximum number presented on the y axis. By default, `NULL`.
#' @param y.breaks Numeric values or a sequence that indicates breaks on the y axis. By default, `NULL`.
#' @param x.label.type A quoted string or list to customize the x axis labels. Options include `"numeric"`, `"percent"`, `"comma"`, `"sci`, or `"dollar"` . By default, `numeric`.
#' @param x.min A numeric value to indicate the minimum number presented on the x axis. By default, `NULL`.
#' @param x.max A numeric value to indicate the maximum number presented on the x axis. By default, `NULL`.
#' @param x.breaks Numeric values or a sequence that indicates breaks on the x axis. By default, `NULL`.
#' @param caption A logical statement to indicate whether the CCMH caption should be included in the plot. By default, `FALSE`.
#' @param caption.size A numeric value to indicate the caption text size. By default, `12`.
#' @param caption.vjust A numeric value to indicate the caption vertical adjustment. By default, `0`.
#' @param legend.position A quoted string or numeric vector to indicate the location of the legend. Options include `"left"`,`"top"`, `"right"`, `"bottom"`, `"none"`, or numeric vector c(x,y). By default, `"none"`.
#' @param legend.order.manual A list of quoted strings to indicate the order of the legend. By default, `NULL`.
#' @param legend.title A quoted string to indicate the legend title. By default, `NULL`.
#' @param legend.title.size A numeric value to indicate the legend title text size. By default, `16`.
#' @param legend.label.size A numeric value to indicate the legend label text size. By default, `14`.
#' @param alpha A numeric value to indicate the histogram bars transparency. Values may range between 0 to 1. By default, `1`.
#' @param bin.width A numeric value to indicate the width of the bins. By default, `NULL`.
#' @param y.grid.major A logical statement to indicate if the y axis major grid lines should be added to the plot. By default, `FALSE`,
#' @param y.grid.major.color A hex code that indicates the color of the y axis major grid lines. By default, `"#d3d3d3"` or a shade of light grey.
#' @param y.grid.major.size A numeric value to indicate the thickness of the y axis major grid lines. By default, `0.5`.
#' @param y.grid.major.linetype A numeric value to indicate the line type of the y axis major grid. By default, `1`.
#' @param y.grid.minor A logical statement to indicate if the y axis minor grid lines should be added to the plot. By default, `FALSE`,
#' @param y.grid.minor.color A hex code that indicates the color of the y axis minor grid lines. By default, `"#d3d3d3"` or a shade of light grey.
#' @param y.grid.minor.size A numeric value to indicate the thickness of the y axis minor grid lines. By default, `0.25`.
#' @param y.grid.minor.linetype A numeric value to indicate the line type of the y axis minor grid. By default, `1`.
#' @param x.grid.major A logical statement to indicate if the y axis major grid lines should be added to the plot. By default, `FALSE`,
#' @param x.grid.major.color A hex code that indicates the color of the y axis major grid lines. By default, `"#d3d3d3"` or a shade of light grey.
#' @param x.grid.major.size A numeric value to indicate the thickness of the y axis major grid lines. By default, `0.5`.
#' @param x.grid.major.linetype A numeric value to indicate the line type of the y axis major grid. By default, `1`.
#' @param x.grid.minor A logical statement to indicate if the x axis minor grid lines should be added to the plot. By default, `FALSE`,
#' @param x.grid.minor.color A hex code that indicates the color of the x axis minor grid lines. By default, `"#d3d3d3"` or a shade of light grey.
#' @param x.grid.minor.size A numeric value to indicate the thickness of the x axis minor grid lines. By default, `0.25`.
#' @param x.grid.minor.linetype A numeric value to indicate the line type of the x axis minor grid. By default, `1`.
#' @param y.axis.line A logical statement to indicate if the y axis line should be included. By default, `TRUE`.
#' @param x.axis.line A logical statement to indicate if the x axis line should be included. By default, `TRUE`.
#' @param remove.axis.ticks A logical statement to indicate if the axis ticks should be excluded. By default, `FALSE`.
#' @param plot.element1 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' @param plot.element2 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' @param plot.element3 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' @param plot.element4 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' @param plot.element5 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' @param plot.element6 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' @param plot.element7 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' @param plot.element8 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' @param plot.element9 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#'
#' @note Note 1. The argument `color` specifies the color of items within the grouped variable. For example, the group variable items could be the name of automobile companies (i.e., Ford, Dodge, BMV). You can specify color randomly for each company using this list: `c("#21501b", "#c51329", "#074e67")`. Or you can specify color directly to each specific company using this list: `c("Ford" = "#21501b", "Dodge" = "#c51329", "BMV" = "#074e67")`. Default colors are color blind safe.
#' @note The `plot.element` arguments specify additional ggplot2 graphical element(s) needed to complete a specific task, but are not specified as an argument in the `hist_plot` function. For example, the axis label text will always be black unless specified in one of the `plot.element` arguments. To change the axis label text color in one of the `plot.element` arguments, you would first create an object to represent graphing element (e.g., green.axis.text <- ggplot2::theme(axis.text = ggplot2::element_text(color = "green"))). Then the object would be added to one of the `plot.element` arguments (e.g., `plot.element01 = green.axis.text`).
#' @return A histogram plot returned as a object or saved under a local file.
#' @export
#'

plot_histogram <- function(data,
                           x.var,
                           group.var = "",
                           color = c('#666666', '#1B9E77',
                                     '#E6AB02', '#D95F02',
                                     '#6db6ff', '#b66dff',
                                     '#ff6db6', '#920000'),
                           y.axis.type = "count",
                           hist.position = "identity",
                           save = FALSE,
                           path = "plot.png",
                           plot.width = 1200,
                           plot.height = 900,
                           plot.units = "px",
                           plot.dpi = 96,
                           plot.device = NULL,
                           plot.scale = 1,
                           plot.title = NULL,
                           x.title = NULL,
                           y.title = NULL,
                           plot.title.size = 20,
                           axis.title.size = 16,
                           axis.label.size = 14,
                           text.font = "Avenir",
                           y.label.type = "numeric",
                           y.min = NULL,
                           y.max = NULL,
                           y.breaks = NULL,
                           x.label.type = "numeric",
                           x.min = NULL,
                           x.max = NULL,
                           x.breaks = NULL,
                           caption = FALSE,
                           caption.size = 12,
                           caption.vjust = 0,
                           legend.position = "none",
                           legend.order.manual = NULL,
                           legend.title = NULL,
                           legend.title.size= 16,
                           legend.label.size= 14,
                           alpha = 1,
                           bin.width = NULL,
                           y.grid.major = FALSE,
                           y.grid.major.color = "#d3d3d3",
                           y.grid.major.size = 0.5,
                           y.grid.major.linetype = 1,
                           y.grid.minor = FALSE,
                           y.grid.minor.color = "#d3d3d3",
                           y.grid.minor.size = 0.25,
                           y.grid.minor.linetype = 1,
                           x.grid.major = FALSE,
                           x.grid.major.color = "#d3d3d3",
                           x.grid.major.size = 0.5,
                           x.grid.major.linetype = 1,
                           x.grid.minor = FALSE,
                           x.grid.minor.color = "#d3d3d3",
                           x.grid.minor.size = 0.25,
                           x.grid.minor.linetype = 1,
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

  # Specify the x-axis variable as a symbol
    x.var1 <- rlang::as_name(rlang::enquo(x.var))
    x.var1 <- rlang::sym(x.var1)

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

  # Specify if the legend should be reordered
    if(!is.null(legend.order.manual) &
       group.var != ""){

      data$group77d8214 <- factor(data$group77d8214,
                                  levels = legend.order.manual)

     } else{

     }

  # Specify Y breaks
    if(is.null(y.breaks)){
      y.breaks.c <- ggplot2::waiver()
    } else {
      y.breaks.c <- y.breaks
    }

  # Specify y axis labels
    if(y.label.type == "percent"){
      y.label.typea <- scales::percent

    } else if(y.label.type == "comma"){
      y.label.typea <- scales::comma

    } else if(y.label.type == "numeric"){
      y.label.typea <- ggplot2::waiver()

    } else if(y.label.type == "sci"){
      y.label.typea <- scales::scientific

    } else {
      y.label.typea <- NULL
    }

  # Specify X breaks
    if(is.null(x.breaks)){
      x.breaks.c <- ggplot2::waiver()
    } else {
      x.breaks.c <- x.breaks
    }

  # Specify x axis labels
    if(x.label.type == "percent"){
      x.label.typea <- scales::percent

    } else if(x.label.type == "comma"){
      x.label.typea <- scales::comma

    } else if(x.label.type == "numeric"){
      x.label.typea <- ggplot2::waiver()

    } else if(x.label.type == "sci"){
      x.label.typea <- scales::scientific

    } else if(x.label.type == "dollar"){
      x.label.typea <- scales::dollar

    } else {
      x.label.typea <- NULL
    }

  # Specify Y Major Grid
    if(y.grid.major == TRUE){
      grid.maj.y <- ggplot2::element_line(color = y.grid.major.color,
                                          linewidth = y.grid.major.size,
                                          linetype = y.grid.major.linetype)
    } else {
      grid.maj.y <- ggplot2::element_blank()
    }

  # Specify Y Minor Grid
    if(y.grid.minor == TRUE){
      grid.min.y <- ggplot2::element_line(color = y.grid.minor.color,
                                          linewidth = y.grid.minor.size,
                                          linetype = y.grid.minor.linetype)
    } else {
      grid.min.y <- ggplot2::element_blank()
    }

  # Specify X Major Grid
    if(x.grid.major == TRUE){
      grid.maj.x <- ggplot2::element_line(color = x.grid.major.color,
                                          linewidth = x.grid.major.size,
                                          linetype = x.grid.major.linetype)
    } else {
      grid.maj.x <- ggplot2::element_blank()
    }

  # Specify X Minor Grid
    if(x.grid.minor == TRUE){
      grid.min.x <- ggplot2::element_line(color = x.grid.minor.color,
                                          linewidth = x.grid.minor.size,
                                          linetype = x.grid.minor.linetype)
    } else {
      grid.min.x <- ggplot2::element_blank()
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

  # Specify the primary graph properties
    his.graph <- ggplot2::ggplot(data,
     {if(group.var != ""){

       ggplot2::aes(x = {{x.var1}},
                    fill = group77d8214)

      } else{

       ggplot2::aes(x = {{x.var1}},
                    fill = color)

       }
     }) +

  # Specify plot as geom_histogram
    {if(y.axis.type == "count"){

      ggplot2::geom_histogram(ggplot2::aes(x = {{x.var1}},
                                           y = ggplot2::after_stat(count)),
                              position = hist.position,
                              alpha = alpha,
                              binwidth = bin.width)

      } else if(y.axis.type == "density"){

        ggplot2::geom_histogram(ggplot2::aes(x = {{x.var1}},
                                             y = ggplot2::after_stat(density)),
                                position = hist.position,
                                alpha = alpha,
                                binwidth = bin.width)

      } else if(y.axis.type == "relative_freq"){

        ggplot2::geom_histogram(ggplot2::aes(x = {{x.var1}},
                                             y = ggplot2::after_stat(count/sum(count))),
                                position = hist.position,
                                alpha = alpha,
                                binwidth = bin.width)

      } else{

        stop("Specify y.axis.type from one of the following options: count, density, or relative frequency")

      }
    } +

  # Specify manual fill
    ggplot2::scale_fill_manual(values = color) +

  # Specify title information
    ggplot2::labs(title = plot.title,
                  y = y.title,
                  x = x.title,
                  fill = legend.title) +

    # Specify y-axis scale
      ggplot2::scale_y_continuous(labels = y.label.typea,
                                  limits = c(y.min,
                                             y.max),
                                  expand = c(0,0),
                                  breaks = y.breaks.c) +

    # Specify x-axis scale
      ggplot2::scale_x_continuous(labels = x.label.typea,
                                  limits = c(x.min,
                                             x.max),
                                  expand = c(0,0),
                                  breaks = x.breaks.c) +

  # Specify the plots theme information
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   panel.grid.major.y = grid.maj.y,
                   panel.grid.minor.y = grid.min.y,
                   panel.grid.major.x = grid.maj.x,
                   panel.grid.minor.x = grid.min.x,
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
                  legend.title = ggplot2::element_text(size= legend.title.size),
                  legend.text = ggplot2::element_text(size = legend.label.size)) +

  # Specify to remove axis ticks
    {if(remove.axis.ticks == TRUE){

      ggplot2::theme(axis.ticks = ggplot2::element_blank())

    } else {

    }} +

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

      his.graph <- his.graph + CCMHr::ccmh_caption()

     } else{

     }

    }

  # Specify if the graph should be saved as file or returned as an object
    if(save == TRUE){

      ggsave(paste0(path),
             plot = his.graph,
             width = plot.width,
             height = plot.height,
             units = plot.units,
             dpi = plot.dpi,
             device = plot.device,
             scale = plot.scale)

    } else{

       return(his.graph)

    }

}

#' @rdname plot_histogram
#' @export
  histogram_plot <- plot_histogram
