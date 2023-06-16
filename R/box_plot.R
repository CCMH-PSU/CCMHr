#' A function to create standardized CCMH box plot's
#'
#' @description Creates standardized CCMH box plot's using ggplot2.
#' @param data A data file containing all variables that will be used in the plot
#' @param x.var A quoted string to indicate the variable that will be plotted on the x-axis. The variable could be a string, numeric, or factor.
#' @param y.var A quoted string to indicate the variable that will be plotted on the y axis. The variable must be numeric.
#' @param grouping A logical statement specifies if a grouping variable will be used in the plot. By default, `FALSE`.
#' @param group.var A quoted string to indicate the variable that will act as a grouping variable if `grouping = TRUE`. The grouping variable would be represented in the legend. By default, `""`.
#' @param save A logical statement specifies whether the plot should be saved as a file under a local folder. By default, `FALSE`.
#' @param path A quoted string to indicate the file's pathway and name if `save = TRUE`. By default, `"plot.png"`.
#' @param plot.width A numeric value to indicate the plot's width. By default, `20`.
#' @param plot.height A numeric value to indicate the plot's height. By default, `20`.
#' @param plot.units A quoted string to indicate the plot's width and height size units. Options include `in`, `cm`, `mm`, and `px`. By default, `cm`.
#' @param plot.dpi A numeric value to indicate the plot's image resolution. By default, `320`.
#' @param box.colors A hex code or list of hex codes that indicates the fill color of the box plots. By default, `"##838B8B"` or dark grey.
#' @param mean.sym A logical statement specifies whether a point should be displayed to indicate the mean. By default, `FALSE`.
#' @param mean.sym.shape A numeric value to indicate the mean point shape. By default, `3` or Crosshair.
#' @param mean.sym.size A numeric value to indicate the mean point size. By default, `2`.
#' @param mean.sym.color A hex code to indicate the mean point color. By default, `"#000000"` or black.
#' @param mean.sym.position A numeric value to indicate the mean point position. By default, `0.75`.
#' @param plot.title A quoted string to create the title of the plot. By default, `""`.
#' @param x.axis.title A quoted string to create the x axis title. By default, `""`.
#' @param y.axis.title A quoted string to create the y axis title. By default, `""`.
#' @param plot.title.size A numeric value to indicate the plot title text size. By default, `20`.
#' @param axis.title.size A numeric value to indicate the axis title text size. By default, `16`.
#' @param axis.text.size A numeric value to indicate the axis labels text size. By default, `14`.
#' @param text.font A quoted string to indicate the plot's text font. By default, `Avenir`.
#' @param y.axis.percent A logical statement to indicate whether the y axis labels are percentages. By default, `FALSE`.
#' @param y.axis.min A numeric value to indicate the minimum number presented on the y axis. By default, `NULL`.
#' @param y.axis.max A numeric value to indicate the maximum number presented on the y axis. By default, `NULL`.
#' @param x.axis.text.wrap A numeric value to indicate the number of characters to wrap the x axis text. By default, `15`.
#' @param x.axis.remove A logical statement to indicate whether y-axis information should be removed. By default, `FALSE`.
#' @param caption.text A logical statement to indicate whether the CCMH caption should be included in the plot. By default, `FALSE`.
#' @param caption.text.size A numeric value to indicate the caption text size. By default, `12`.
#' @param legend.position A quoted string or numeric vector to indicate the location of the legend. Options include `"left"`,`"top"`, `"right"`, `"bottom"`, `"none"`, or numeric vector c(x,y). By default, `"none"`.
#' @param legend.title A quoted string to indicate the legend title. By default, `NULL`.
#' @param legend.title.size A numeric value to indicate the legend title text size. By default, `16`.
#' @param legend.text.size A numeric value to indicate the legend label text size. By default, `14`.
#' @param plot.element1 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' @param plot.element2 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' @param plot.element3 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' @param plot.element4 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' @param plot.element5 A ggplot plot function and arguments needed for the plot and specified as an object. By default, `NULL`.
#' 
#' @note The `plot.element` arguments specify additional ggplot2 graphical element(s) needed to complete a specific box plot, but are not specified as an argument in the `box_plot` function. For example, the axis label text will always be black unless specified in one of the `plot.element` arguments. To change the axis label text color in one of the `plot.element` arguments, you would first create an object to represent graphing element (e.g., green.axis.text <- ggplot2::theme(axis.text = ggplot2::element_text(color = "green"))). Then the object would be added to one of the `plot.element` arguments (e.g., `plot.element01 = green.axis.text`).
#' @return A box plot returned as a object or saved under a local file.  
#' @export
#'

box_plot <- function(data,
                     x.var,
                     y.var,
                     grouping = FALSE,
                     group.var = "",
                     save = FALSE,
                     path = "plot.png",
                     plot.width = 20,
                     plot.height = 20,
                     plot.units = "cm",
                     plot.dpi = 320,
                     box.colors = "#838B8B",
                     mean.sym = FALSE,
                     mean.sym.shape = 3,
                     mean.sym.size = 2,
                     mean.sym.color = "#000000",
                     mean.sym.position = 0.75,
                     plot.title = "",
                     x.axis.title = "",
                     y.axis.title = "", 
                     plot.title.size = 20,
                     axis.title.size = 16,
                     axis.text.size = 14,
                     text.font = "Avenir",
                     y.axis.percent = FALSE,
                     y.axis.min = NULL,
                     y.axis.max = NULL,
                     x.axis.text.wrap = 15,
                     x.axis.remove = FALSE,
                     caption.text = FALSE,
                     caption.text.size = 12,
                     legend.position = "none",
                     legend.title = NULL,
                     legend.title.size= 16, 
                     legend.text.size= 14,
                     plot.element1 = NULL,
                     plot.element2 = NULL,
                     plot.element3 = NULL,
                     plot.element4 = NULL,
                     plot.element5 = NULL){
  
  # Specify the x-axis variable as a symbol
    x.var1 <- rlang::sym(x.var)
    
  # Specify the y-axis variable as a symbol
    y.var1 <- rlang::sym(y.var)
    
  # Specify the grouping variable as a symbol
    group.var1 <- rlang::sym(group.var)

  # Specify basic plot information and if grouping will occur
    plot.1 <- ggplot2::ggplot(data,
      {if(grouping == FALSE) {
        aes(x = {{x.var1}},
            y = {{y.var1}})
      } else{
        aes(x = {{x.var1}},
            y = {{y.var1}},
            fill = {{group.var1}}) 
      }}) +

  # Specify the plot as a box plot based on grouping  
    {if(grouping == FALSE) {
        geom_boxplot(fill = box.colors) 
      } else{
        geom_boxplot()
     }}+
    
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
    
  # Specify the plot and axis titles 
    ggplot2::labs(title = paste0(plot.title),
                  y = paste0(y.axis.title),
                  x = paste0(x.axis.title)) +

    
  # Specify if the y-axis is a percentage
    {if(y.axis.percent == TRUE){
      ggplot2::scale_y_continuous(labels = scales::percent,
                                  limits = c(y.axis.min,
                                             y.axis.max))
    } else {
      ggplot2::scale_y_continuous(limits = c(y.axis.min,
                                             y.axis.max))
    }} +
    
  # Specify box plot fill colors of grouped data
    {if(grouping == TRUE){
      ggplot2::scale_fill_manual(values=box.colors)
    } else {
      
    }} +
    
  # Specify the legend titles
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title)) +
      
  # Specify the plots theme information
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = 'white',
                                                            color = NA),
                   text = ggplot2::element_text(family = text.font),
                   plot.title = ggplot2::element_text(size = plot.title.size,
                                                      hjust = 0.5),
                   axis.title = ggplot2::element_text(size = axis.title.size),
                   axis.text = ggplot2::element_text(size = axis.text.size),
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
                   plot.caption = ggplot2::element_text(size = caption.text.size),
                   legend.position = legend.position,
                   legend.background = ggplot2::element_rect(fill = "white",
                                                             colour = "white",
                                                             inherit.blank = TRUE,
                                                             color = "white"),
                   legend.title = ggplot2::element_text(size= legend.title.size),
                   legend.text = ggplot2::element_text(size = legend.text.size)) +
    
  # Specify x axis label text wrap
    scale_x_discrete(labels = scales::wrap_format(x.axis.text.wrap)) +
      
  # Specify if x axis should be removed
    {if(x.axis.remove == TRUE){
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank())
    } else{
    }} + 
      
  # Specify plot elements
    plot.element1 +
    plot.element2 +
    plot.element3 +
    plot.element4 +
    plot.element5
    
  # Specify caption
    {if(caption.text == TRUE){
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