#' A function to create standardized CCMH scatter plot's
#'
#' @name plot_scatter
#' @description Creates standardized CCMH scatter plot's using ggplot2.
#' @param data A data file containing all variables that will be used in the plot
#' @param x.var A quoted string or unquoted characters to indicate the variable that will be plotted on the x-axis. The variable class must be numeric.
#' @param y.var A quoted string or unquoted characters to indicate the variable that will be plotted on the y axis. The variable class must be numeric.
#' @param group.var A quoted string to indicate the variable that will group the points on the x and y-axis and specify the color of the points. By default, `""`.
#' @param color A hex code or list of hex codes that indicates the color of the grouped points. See Note 1 for more details. By default, `c('#E6AB02', '#1B9E77', '#666666', '#D95F02', '#6db6ff', '#b66dff', '#ff6db6', '#920000')`.
#' @param save A logical statement indicates whether the plot should be saved as a file under a local folder. If false, the plot will be returned as an object. By default, `FALSE`.
#' @param path A quoted string to indicate the file's pathway and name if `save = TRUE`. By default, `"plot.png"`.
#' @param plot.width A numeric value to indicate the plot's width. By default, `15.24`.
#' @param plot.height A numeric value to indicate the plot's height. By default, `12.7`.
#' @param plot.units A quoted string to indicate the plot's width and height size units. Options include `in`, `cm`, `mm`, and `px`. By default, `cm`.
#' @param plot.dpi A numeric value to indicate the plot's image resolution. By default, `320`.
#' @param plot.device A quoted string or function to indicate the plot's device. By default, `NULL`.
#' @param hide.group.items A quoted string or list to indicate the group variable items to be hidden in the plot. See Note 2. By default, `NULL`.
#' @param plot.title A quoted string to indicate the title of the plot. By default, `NULL`.
#' @param x.title A quoted string to create the x axis title. By default, `NULL`.
#' @param y.title A quoted string to create the y axis title. By default, `NULL`.
#' @param plot.title.size A numeric value to indicate the plot title text size. By default, `20`.
#' @param axis.title.size A numeric value to indicate the axis title text size. By default, `16`.
#' @param axis.label.size A numeric value to indicate the axis labels text size. By default, `14`.
#' @param text.font A quoted string to indicate the plot's text font. By default, `Avenir`.
#' @param y.label.type A quoted string or list to customize the y axis labels. Options include `"numeric"`, `"percent"`, `"comma"`, `"sci`, or `"dollar"` . By default, `numeric`.
#' @param y.min A numeric value to indicate the minimum number presented on the y axis. By default, `NULL`.
#' @param y.max A numeric value to indicate the maximum number presented on the y axis. By default, `NULL`.
#' @param y.breaks Numeric values or a sequence that indicates breaks on the y axis. By default, `NULL`.
#' @param x.label.type A quoted string or list to customize the x axis labels. Options include `"numeric"`, `"percent"`, `"comma"`, `"sci`, or `"dollar"` . By default, `numeric`.
#' @param x.min A numeric value to indicate the minimum number presented on the x axis. By default, `NULL`.
#' @param x.max A numeric value to indicate the maximum number presented on the x axis. By default, `NULL`.
#' @param x.breaks Numeric values or a sequence that indicates breaks on the x axis. By default, `NULL`.
#' @param caption A logical statement to indicate whether the CCMH caption should be included in the plot. By default, `FALSE`.
#' @param caption.size A numeric value to indicate the caption text size. By default, `12`.
#' @param legend.position A quoted string or numeric vector to indicate the location of the legend. Options include `"left"`,`"top"`, `"right"`, `"bottom"`, `"none"`, or numeric vector c(x,y). By default, `"none"`.
#' @param legend.order.manual A list of quoted strings to indicate the order of the legend. By default, `NULL`.
#' @param legend.title A quoted string to indicate the legend title. By default, `NULL`.
#' @param legend.title.size A numeric value to indicate the legend title text size. By default, `16`.
#' @param legend.label.size A numeric value to indicate the legend label text size. By default, `14`.
#' @param coord.flip A logical statement to indicate if the plot's x and y axis should be flipped. By default, `FALSE`.
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
#' @param fit.line A logical statement to indicate if the plot should have a fitted line. By default, `FALSE`.
#' @param fit.line.type A quoted string to indicate the method of fitting a line. See information on geom_smooth for more information. By default, `NULL`.
#' @param fit.line.ci A logical statement to indicate if the plotted line should have confidence intervals. By default, `FALSE`.
#' @param fit.line.fullrange A logical statement to indicate if the plotted line should cross the entire plot. By default, `FALSE`.
#' @param point_size A numeric value to indicate the point size. By default, `1.5`.
#' @param point_shape A numeric value to indicate the point shape. By default, `19`.
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
#' @note Note 1. The argument `color` specifies the color of points within the grouped variable. For example, the group variable items could be the name of automobile companies (i.e., Ford, Dodge, BMV). You can specify color randomly for each company using this list: `c("#21501b", "#c51329", "#074e67")`. Or you can specify color directly to each specific company using this list: `c("Ford" = "#21501b", "Dodge" = "#c51329", "BMV" = "#074e67")`. Default colors are color blind safe.
#' @note Note 2. The `hide.group.items` function hides points in the plot while keeping the hidden points position. This argument was intended to be used when preparing plots for presentation, where the presenter wants to present data in a specified order instead of displaying all the data at once. Fit lines will not be removed.
#' @note The `plot.element` arguments specify additional ggplot2 graphical element(s) needed to complete a specific task, but are not specified as an argument in the `scatter_plot` function. For example, the axis label text will always be black unless specified in one of the `plot.element` arguments. To change the axis label text color in one of the `plot.element` arguments, you would first create an object to represent graphing element (e.g., green.axis.text <- ggplot2::theme(axis.text = ggplot2::element_text(color = "green"))). Then the object would be added to one of the `plot.element` arguments (e.g., `plot.element01 = green.axis.text`).
#' @return A column plot returned as a object or saved under a local file.
#' @export
#'

plot_scatter <- function(data,
                         x.var,
                         y.var,
                         group.var = "",
                         color = c('#666666', '#1B9E77',
                                   '#E6AB02', '#D95F02',
                                   '#6db6ff', '#b66dff',
                                   '#ff6db6', '#920000'),
                         save = FALSE,
                         path = "plot.png",
                         plot.width = 15.24,
                         plot.height = 12.7,
                         plot.units = "cm",
                         plot.dpi = 320,
                         plot.device = NULL,
                         hide.group.items = NULL,
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
                         legend.position = "none",
                         legend.order.manual = NULL,
                         legend.title = NULL,
                         legend.title.size = 16,
                         legend.label.size = 14,
                         coord.flip = FALSE,
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
                         fit.line = FALSE,
                         fit.line.type = NULL,
                         fit.line.ci = FALSE,
                         fit.line.fullrange = FALSE,
                         point_size = 1.5,
                         point_shape = 19,
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

  # Specify the y-axis variable as a symbol
    y.var1 <- rlang::as_name(rlang::enquo(y.var))
    y.var1 <- rlang::sym(y.var1)

  # Specify the fill variable as a symbol
    group.var1 <- rlang::sym(group.var)

  # Remove rownames
    rownames(data) <- NULL

  # Detect and display error messages
    # Error message to indicate the legend.order.manual items does not match the items in the filler variable
      if(!is.null(legend.order.manual) &
         group.var != ""){

        group.varz <- unique(data[[{{group.var1}}]])
        test1 <- ifelse(group.varz %in% legend.order.manual, "TRUE", "FALSE")
        test2 <- ifelse(legend.order.manual %in% group.varz, "TRUE", "FALSE")
        test.final <- c(test1, test2)
        test.final <- FALSE %in% test.final

       if(test.final == TRUE){

          stop("Unique characters listed legend.order.manual must match the characters of fill variable")

       } else{

       }
      }

  # Specify to remove legend or x axis information from the plot
    if(!is.null(hide.group.items) |
       group.var != ""){

     # Remove column based on fill information
       if(!is.null(hide.group.items)){

          data <- data %>%
            dplyr::mutate(alpha = ifelse({{group.var1}} %in% hide.group.items, "b", "a"))

       } else{

          data$alpha <- "a"

       }

     # Specify if the legend should be reordered
       if(!is.null(legend.order.manual) &
          group.var != ""){

         data[[{{group.var1}}]] <- factor(data[[{{group.var1}}]],
                                         levels = legend.order.manual)

         data.temp <- data %>%
           dplyr::arrange({{group.var1}})%>%
           dplyr::filter(alpha == "a") %>%
           dplyr::select({{group.var1}})

         names(data.temp) <- "break1"
         break1 <- as.character(unique(data.temp$break1))

       } else if(is.null(legend.order.manual) &
                 group.var != ""){

         data.temp <- data %>%
           dplyr::filter(alpha == "a") %>%
           dplyr::select({{group.var1}})

         names(data.temp) <- "break1"

         break1 <- as.character(unique(data.temp$break1))

       } else{

       }

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

    } else if(y.label.type == "dollar"){
      y.label.typea <- scales::dollar

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

  # Specify the primary graph properties and x axis order
    sca.graph <- ggplot2::ggplot(data,
      {if(group.var == ""){

        ggplot2::aes(x = {{x.var1}},
                     y = {{y.var1}},
                     color = color)

       } else {

         ggplot2::aes(x = {{x.var1}},
                      y = {{y.var1}},
                      color = {{group.var1}})

       }}) +

  # Specify plot as geom_point and alpha
    {if(!is.null(hide.group.items)){

      ggplot2::geom_point(ggplot2::aes(alpha = alpha),
                          size = point_size,
                          shape = point_shape)

     } else{

      ggplot2::geom_point(size = point_size,
                          shape = point_shape)

     }
    } +

  # Specify fit line
    {if(fit.line == TRUE){

      ggplot2::geom_smooth(method = fit.line.type,
                           fullrange = fit.line.fullrange,
                           se = fit.line.ci)

      } else{

     }
    } +

  # Remove points based on alpha
    {if(!is.null(hide.group.items)){

        ggplot2::scale_alpha_manual(values = c("a" = 1, "b" = 0),
                                    guide = 'none')

      } else{

      }
    } +

  # Remove legend information based on alpha and specify fill color
    {if(group.var != ""){

      ggplot2::scale_color_manual(values = color,
                                  breaks = break1)

     } else{

      ggplot2::scale_color_manual(values = color)

     }
    } +

  # Specify legend information
    ggplot2::guides(color = ggplot2::guide_legend(byrow = TRUE,
                                                  override.aes = ggplot2::aes(label = "")),
                    alpha = "none") +

  # Specify title information
    ggplot2::labs(title = plot.title,
                  y = y.title,
                  x = x.title,
                  color = legend.title) +

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
                   plot.caption = ggplot2::element_text(size = caption.size),
                   legend.position = legend.position,
                   legend.key = ggplot2::element_blank(),
                   legend.background = ggplot2::element_rect(fill = "white",
                                                             colour = "white",
                                                             inherit.blank = TRUE,
                                                             color = "white"),
                   legend.title = ggplot2::element_text(size= legend.title.size),
                   legend.text = ggplot2::element_text(size = legend.label.size)) +

  # Specify if the coords should be flipped
    {if(coord.flip == TRUE){

      ggplot2::coord_flip()

     } else{

     }
    } +

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

  # Specify caption information
    {if(caption == TRUE){

      sca.graph <- sca.graph +
                   CCMHr::ccmh_caption()

     } else{

     }
    }

  # Specify if the graph should be saved as file or returned as an object
    if(save == TRUE){

      ggsave(paste0(path),
             plot = sca.graph,
             width = plot.width,
             height = plot.height,
             units = plot.units,
             dpi = plot.dpi,
             device = plot.device)

     } else{

      return(sca.graph)

    }

  }

#' @rdname plot_scatter
#' @export
scatter_plot <- plot_scatter
