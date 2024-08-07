#' A function to create standardized CCMH column plot's
#'
#' @name plot_column
#' @description Creates standardized CCMH column plot's using ggplot2.
#' @param data A data file containing all variables that will be used in the plot
#' @param x.var A quoted string or unquoted characters to indicate the variable that will be plotted on the x-axis. The variable class must be a character or factor.
#' @param y.var A quoted string or unquoted characters to indicate the variable that will be plotted on the y axis. The variable class must be numeric.
#' @param group.var A quoted string to indicate the variable that will group the columns on the x-axis and specify the color of the columns. The x-axis variable could used to specify colors of each individual column. By default, `""`.
#' @param color A hex code or list of hex codes that indicates the color of the grouped columns. See Note 1 for more details. By default, the 10 colors from `CCMHr::CCMH_categorical_palettes$tolmuted_category10` palette.
#' @param x.ascend A logical statement to indicate if the columns should be arranged in ascending order on the x axis. By default, `FALSE`.
#' @param x.descend A logical statement to indicate if the columns should be arrange descending on the x axis. By default, `FALSE`.
#' @param x.order.manual A list of quoted strings to indicate the order of the x axis. By default, `NULL`.
#' @param col.position A quoted string to indicate the column position adjustment based on geom_col. By default, `dodge`.
#' @param save A logical statement indicates whether the plot should be saved as a file under a local folder. If false, the plot will be returned as an object. By default, `FALSE`.
#' @param path A quoted string to indicate the file's pathway and name if `save = TRUE`. By default, `"plot.png"`.
#' @param plot.width A numeric value to indicate the plot's width. By default, `1200`.
#' @param plot.height A numeric value to indicate the plot's height. By default, `900`.
#' @param plot.units A quoted string to indicate the plot's width and height size units. Options include `in`, `cm`, `mm`, and `px`. By default, `px`.
#' @param plot.dpi A numeric value to indicate the plot's image resolution. By default, `96`.
#' @param plot.device A quoted string or function to indicate the plot's device. By default, `NULL`.
#' @param plot.scale A numeric value to indicate the plot's scale. By default, `1`.
#' @param hide.x.items A quoted string or list to indicate the x-axis variable items to be hidden in the plot. See Note 2. By default, `NULL`.
#' @param hide.group.items A quoted string or list to indicate the group variable items to be hidden in the plot. See Note 2. By default, `NULL`.
#' @param plot.title A quoted string to indicate the title of the plot. By default, `NULL`.
#' @param x.title A quoted string to indicate the x axis title. By default, `NULL`.
#' @param y.title A quoted string to indicate the y axis title. By default, `NULL`.
#' @param plot.title.size A numeric value to indicate the plot title text size. By default, `20`.
#' @param axis.title.size A numeric value to indicate the axis title text size. By default, `16`.
#' @param axis.label.size A numeric value to indicate the axis labels text size. By default, `14`.
#' @param text.font A quoted string to indicate the plot's text font. By default, `Avenir`.
#' @param y.label.type A quoted string or list to customize the y axis labels. Options include `"numeric"`, `"percent"`, `"comma"`, `"sci`, or `"dollar"` . By default, `numeric`.
#' @param y.min A numeric value to indicate the minimum number presented on the y axis. By default, `NULL`.
#' @param y.max A numeric value to indicate the maximum number presented on the y axis. By default, `NULL`.
#' @param y.breaks Numeric values or a sequence that indicates breaks on the y axis. By default, `NULL`.
#' @param y.expand A list of two numeric values to indicate the expansion of the y axis. By default, `ggplot2::waiver()`.
#' @param x.wrap A numeric value to indicate the number of characters to wrap the x axis text. By default, `15`.
#' @param x.remove A logical statement to indicate whether x-axis title, labels, and ticks should be removed. By default, `FALSE`.
#' @param caption A logical statement to indicate whether the CCMH caption should be included in the plot. By default, `FALSE`.
#' @param caption.size A numeric value to indicate the caption text size. By default, `12`.
#' @param caption.vjust A numeric value to indicate the caption vertical adjustment. By default, `0`.
#' @param legend.position A quoted string or numeric vector to indicate the location of the legend. Options include `"left"`,`"top"`, `"right"`, `"bottom"`, `"none"`, or numeric vector c(x,y). By default, `"none"`.
#' @param legend.order.manual A list of quoted strings to indicate the order of the legend. By default, `NULL`.
#' @param legend.title A quoted string to indicate the legend title. By default, `NULL`.
#' @param legend.title.size A numeric value to indicate the legend title text size. By default, `16`.
#' @param legend.label.size A numeric value to indicate the legend label text size. By default, `14`.
#' @param column.width A numeric value to indicate the columns width. By default, `NULL`.
#' @param coord.flip A logical statement to indicate if the plot's x and y axis should be flipped. By default, `FALSE`.
#' @param y.grid.major A logical statement to indicate if the y axis major grid lines should be added to the plot. By default, `FALSE`,
#' @param y.grid.major.color A hex code that indicates the color of the y axis major grid lines. By default, `"#d3d3d3"` or a shade of light grey.
#' @param y.grid.major.size A numeric value to indicate the thickness of the y axis major grid lines. By default, `0.5`.
#' @param y.grid.major.linetype A numeric value to indicate the line type of the y axis major grid. By default, `1`.
#' @param y.grid.minor A logical statement to indicate if the y axis minor grid lines should be added to the plot. By default, `FALSE`,
#' @param y.grid.minor.color A hex code that indicates the color of the y axis minor grid lines. By default, `"#d3d3d3"` or a shade of light grey.
#' @param y.grid.minor.size A numeric value to indicate the thickness of the y axis minor grid lines. By default, `0.25`.
#' @param y.grid.minor.linetype A numeric value to indicate the line type of the y axis minor grid. By default, `1`.
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
#' @note Note 2. The `hide.x.items` and `hide.group.items` function hides columns in the plot while keeping the hidden columns position. This argument was intended to be used when preparing plots for presentation, where the presenter wants to present data in a specified order instead of displaying all the data at once.
#' @note The `plot.element` arguments specify additional ggplot2 graphical element(s) needed to complete a specific task, but are not specified as an argument in the `column_plot` function. For example, the axis label text will always be black unless specified in one of the `plot.element` arguments. To change the axis label text color in one of the `plot.element` arguments, you would first create an object to represent graphing element (e.g., green.axis.text <- ggplot2::theme(axis.text = ggplot2::element_text(color = "green"))). Then the object would be added to one of the `plot.element` arguments (e.g., `plot.element01 = green.axis.text`).
#' @return A column plot returned as a object or saved under a local file.
#' @export
#'

plot_column <- function(data,
                        x.var,
                        y.var,
                        group.var = "",
                        color = CCMHr::CCMH_categorical_palettes$tolmuted_category10,
                        x.ascend = FALSE,
                        x.descend = FALSE,
                        x.order.manual = NULL,
                        col.position = "dodge",
                        save = FALSE,
                        path = "plot.png",
                        plot.width = 1200,
                        plot.height = 900,
                        plot.units = "px",
                        plot.dpi = 96,
                        plot.device = NULL,
                        plot.scale = 1,
                        hide.x.items = NULL,
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
                        y.expand = ggplot2::waiver(),
                        x.wrap = 15,
                        x.remove = FALSE,
                        caption = FALSE,
                        caption.size = 12,
                        caption.vjust = 0,
                        legend.position = "none",
                        legend.order.manual = NULL,
                        legend.title = NULL,
                        legend.title.size= 16,
                        legend.label.size= 14,
                        column.width = NULL,
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

  # Detect and display error messages
    # Error message to indicate that no more than one X axis order technique can be specified
      if(x.ascend == TRUE |
         x.descend == TRUE |
         !is.null(x.order.manual)){

          x.order.manual.2 <- !is.null(x.order.manual)
          df.order <- c(x.ascend, x.descend, x.order.manual.2)
          df.order <- as.data.frame(table(df.order))
          df.order <- dplyr::filter(df.order, df.order == "TRUE")

        if(as.numeric(df.order$Freq) > 1){

            stop("Only one of the following arguments could be used: x.ascend, x.decend, x.order.manual")

        } else{

        }
      } else{

      }

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

  # Specify descending x axis order
    if(x.descend == TRUE){

      data2 <- data %>%
        dplyr::arrange(desc({{y.var1}})) %>%
        dplyr::select({{x.var1}})

      names(data2) <- "x.var"
      x.var <- unique(data2$x.var)
      x.var <- as.character(x.var)

      data[[{{x.var1}}]] <- factor(data[[{{x.var1}}]],
                                   levels = c(x.var))

    } else{

    }

  # Specify ascending x axis order
    if(x.ascend == TRUE) {

      data2 <- data %>%
        dplyr::arrange({{y.var1}}) %>%
        dplyr::select({{x.var1}})

      names(data2) <- "x.var"
      x.var <- unique(data2$x.var)
      x.var <- as.character(x.var)

      data[[{{x.var1}}]] <- factor(data[[{{x.var1}}]],
                                   levels = c(x.var))

    } else{

    }

  # Specify to remove legend or x axis information from the plot
    if(!is.null(hide.group.items) |
       !is.null(hide.x.items)){

     # Add hide.x.items if null
       if(is.null(hide.x.items)){
         hide.x.items <- ""
       } else{

       }

     # Add hide.group.items if null
       if(is.null(hide.group.items)){
         hide.group.items <- ""
       } else{

       }

     # Remove column based on fill information
       if(!is.null(hide.group.items)){

          data <- data %>%
            dplyr::mutate(alpha.fill = ifelse(group77d8214 %in% hide.group.items, "b", "a"))

       } else{

          data$alpha.fill <- "a"

       }

     # Remove column based on x axis information
       if(!is.null(hide.x.items)){

          data <- data %>%
            dplyr::mutate(alpha.varx = ifelse({{x.var1}} %in% hide.x.items, "b", "a"))

       } else{

          data$alpha.varx <- "a"

       }

     # Specify an overall alpha variable
       data <- data %>%
         dplyr::mutate(alpha = ifelse(alpha.fill == "b" | alpha.varx == "b", "b", "a"))

     # Specify if the legend should be reordered
       if(!is.null(legend.order.manual) &
          group.var != ""){

         data$group77d8214 <- factor(data$group77d8214,
                                         levels = legend.order.manual)

         data.temp <- data %>%
           dplyr::arrange(group77d8214)%>%
           dplyr::filter(alpha == "a") %>%
           dplyr::select(group77d8214)

         names(data.temp) <- "break1"
         break1 <- as.character(unique(data.temp$break1))

       } else if(is.null(legend.order.manual) &
                 group.var != ""){

         data.temp <- data %>%
           dplyr::filter(alpha == "a") %>%
           dplyr::select(group77d8214)

         names(data.temp) <- "break1"
         break1 <- as.character(unique(data.temp$break1))

       } else{

       }

    } else{

    }

  # Specify Y Major Grid
    if(y.grid.major == TRUE){
      grid.maj <- ggplot2::element_line(color = y.grid.major.color,
                                        linewidth = y.grid.major.size,
                                        linetype = y.grid.major.linetype)
    } else {
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

  # Specify color
    if(group.var == ""){
      color <- color[1]
    } else{

    }

  # Specify the primary graph properties and x axis order
    col.graph <- ggplot2::ggplot(data,
      {if(x.ascend  == TRUE &
          group.var == ""){

        ggplot2::aes(x = forcats::fct_reorder({{x.var1}}, {{y.var1}}),
                     y = {{y.var1}},
                     fill = color)

       } else if(x.ascend  == TRUE &
                 group.var != ""){

         ggplot2::aes(x = forcats::fct_reorder({{x.var1}}, {{y.var1}}),
                      y = {{y.var1}},
                      fill = group77d8214)

       } else if(x.descend  == TRUE &
                 group.var == ""){

         ggplot2::aes(x = forcats::fct_rev(forcats::fct_reorder({{x.var1}}, {{y.var1}})),
                      y = {{y.var1}},
                      fill = color)

       } else if(x.descend  == TRUE &
                 group.var != ""){

         ggplot2::aes(x = forcats::fct_rev(forcats::fct_reorder({{x.var1}}, {{y.var1}})),
                      y = {{y.var1}},
                      fill = group77d8214)

       } else if(!is.null(x.order.manual) &
                 group.var == ""){

         ggplot2::aes(x = {{x.var1}},
                      y = {{y.var1}},
                      fill = color)

       } else if(!is.null(x.order.manual) &
                 group.var != ""){

         ggplot2::aes(x = {{x.var1}},
                      y = {{y.var1}},
                      fill = group77d8214)

       } else if(x.descend  == FALSE &
                 x.ascend  == FALSE &
                 is.null(x.order.manual) &
                 group.var != ""){

         ggplot2::aes(x = {{x.var1}},
                      y = {{y.var1}},
                      fill = group77d8214)

       } else{

         ggplot2::aes(x = {{x.var1}},
                      y = {{y.var1}},
                      fill = color)

      }}) +

  # Specify plot as geom_col and alpha
    {if(!is.null(hide.x.items) |
        !is.null(hide.group.items)){

      ggplot2::geom_col(position = col.position,
                        ggplot2::aes(alpha = alpha),
                        width = column.width)

     } else{

      ggplot2::geom_col(position = col.position,
                        width = column.width)
     }
    } +

  # Remove columns based on alpha
    {if(!is.null(hide.x.items)){

      ggplot2::scale_alpha_manual(values = c("a" = 1, "b" = 0),
                                  guide = 'none')

     } else{

     }
    } +

  # Remove legend information based on alpha and specify fill color
    {if(!is.null(hide.x.items) &
        group.var != ""){

      ggplot2::scale_fill_manual(values = color,
                                 breaks = break1)

     } else{

      ggplot2::scale_fill_manual(values = color)

     }
    } +

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
                   legend.title = ggplot2::element_text(size= legend.title.size),
                   legend.text = ggplot2::element_text(size = legend.label.size)) +

  # Specify x axis label text wrap
    ggplot2::scale_x_discrete(labels = scales::wrap_format(x.wrap)) +

  # Specify if the coords should be flipped
    {if(coord.flip == TRUE){

      ggplot2::coord_flip()

     } else{

     }
    } +

  # Specify if x axis information should be removed
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

      col.graph <- col.graph +
                   CCMHr::ccmh_caption()

     } else{

     }
    }

  # Specify if the graph should be saved as file or returned as an object
    if(save == TRUE){

      ggplot2::ggsave(paste0(path),
                      plot = col.graph,
                      width = plot.width,
                      height = plot.height,
                      units = plot.units,
                      dpi = plot.dpi,
                      device = plot.device,
                      scale = plot.scale)

     } else{

      return(col.graph)

     }

  }

#' @rdname plot_column
#' @export
  column_plot <- plot_column
