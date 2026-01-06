#' Creates a column graph.
#'
#' @name plot_column
#'
#' @description This function creates a column graph. Column graphs visually resemble bar graphs; however, there are critical differences (see plot_bar for details).
#'
#' @param data A data file containing variables that will be used in the plot.
#' @param x.var A quoted or unquoted string indicates the variable that will be plotted on the x-axis. The variable class must be a character or factor.
#' @param y.var A quoted or unquoted string indicates the variable that will be plotted on the y-axis. The variable class must be numeric.
#' @param group.var A quoted string to indicate the variable that will group data/columns on the x-axis. By default, `""`.
#' @param color A hex code or list of hex codes that indicates the color(s) of the columns. If a variable groups the data, the argument specifies the color of the columns or items within the grouped variable. For example, the group variable items could be the names of automobile companies (i.e., Ford, Dodge, BMW). You can specify a random color for each company using this list: `c("#21501b", "#c51329", "#074e67")`. Alternatively, you can specify the color directly for each company using this list: `c("Ford" = "#21501b", "Dodge" = "#c51329", "BMW" = "#074e67")`. The number of hex codes must be equal to or greater than the number of responses in the grouped data. If data is not grouped, all the bars will have the same color. The default colors are color-blind safe. By default, `CCMHr::CCMH_categorical_palettes$tolmuted_category10`.
#' @param x.ascend A logical argument to indicate whether the items should be arranged in ascending order on the x-axis. If `TRUE`, the x-axis is ordered ascending. By default, `FALSE`.
#' @param x.descend A logical argument to indicate if the items should be arranged in descending order on the x-axis. If `TRUE`, the x-axis is ordered descending. By default, `FALSE`.
#' @param x.order.manual A list of quoted strings to indicate the order of the x-axis items. By default, `NULL`.
#' @param col.position A quoted string to indicate the column position adjustment. By default, `"dodge"`.
#' @param save A logical argument that indicates whether the plot should be saved as a file under a local folder. If `FALSE`, the plot will be returned as an object. Note: Returning plot object is not supported when fixed.legend.size = TRUE or multiple plots are created based on hide.group.items and/or hide.x.items. By default, `FALSE`.
#' @param path A quoted string to indicate the file's pathway, name, and type if `save = TRUE`. By default, `"plot.png"`.
#' @param plot.width A numeric value to indicate the plot's width. By default, `12`.
#' @param plot.height A numeric value to indicate the plot's height. By default, `9`.
#' @param plot.units A quoted string to indicate the plot's width and height in size units. Options include `in`, `cm`, `mm`, and `px`. By default, `in`.
#' @param plot.dpi A numeric value to indicate the plot's image resolution. By default, `360`.
#' @param plot.device A quoted string or function to indicate the plot's device. If saving the plot as a PDF, use `cairo_pdf`. By default, `NULL`.
#' @param plot.scale A numeric value to indicate the plot's scale. By default, `1`.
#' @param hide.x.items A quoted string, a character vector, or a list to indicate the x-axis variable items to be hidden in the plot. The argument hides the x-axis item bars in the plot while maintaining the position of the hidden bars. This argument is intended for use when preparing plots for presentation, where the presenter wants to present data in a specified order instead of displaying all the data at once. When a quoted string or a character vector is used, a single plot is saved/returned. When a list is provided, multiple plots are saved/returned based on its length. See notes and codebook for more details and examples. By default, `NULL`.
#' @param hide.group.items A quoted string, a character vector, or a list to indicate the group variable items to be hidden in the plot. The argument hides the grouped item bars in the plot while maintaining the position of the hidden bars. This argument is intended for use when preparing plots for presentation, where the presenter wants to present data in a specified order instead of displaying all the data at once. When a quoted string or a character vector is used, a single plot is saved/returned. When a list is provided, multiple plots are saved/returned based on its length. See notes and codebook for more details and examples. By default, `NULL`.
#' @param plot.title A quoted string to indicate the title of the plot. By default, `NULL`.
#' @param x.title A quoted string to indicate the x-axis title. By default, `NULL`.
#' @param y.title A quoted string to indicate the y-axis title. By default, `NULL`.
#' @param plot.title.size A numeric value to indicate the plot title text size. By default, `20`.
#' @param axis.title.size A numeric value to indicate the axis title text size. By default, `16`.
#' @param axis.label.size A numeric value to indicate the axis labels text size. By default, `14`.
#' @param text.font A quoted string to indicate the plot's text font. By default, `"Avenir"`.
#' @param y.label.type A quoted string or list of quoted strings to specify the format of the y-axis labels. Options include `"numeric"`, `"percent"`, `"comma"`, `"sci"`, or `"dollar"`. By default, `"numeric"`.
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
#' @param fixed.legend.size A logical argument to indicate whether the legend key size should be fixed. If `TRUE`, the legend key size is fixed. By default, `FALSE`.
#' @param column.width A numeric value to indicate the column's width. By default, `0.90`.
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
#' @param reference.line A numeric value or list of numeric values to indicate where reference line(s) should be placed on the y-axis. If NULL, no reference line(s) are added. By default, `NULL`.
#' @param reference.line.color A hex code or list of A hex codes to indicate the color(s) of each reference line. By default, `"#1f2022"` or a dark grey.
#' @param reference.line.size A numeric value or list of numeric values to indicate the thickness of each reference line. By default, `1`.
#' @param reference.line.linetype A numeric value or list of numeric values to indicate the line type(s) of each reference line. By default, `2`.
#' @param plot.element1 A ggplot2 plot function and arguments needed to add graphical element(s) needed to complete a specific task, but are not specified as an argument in the function. For example, the axis label text color will always be black unless specified in one of the plot.element arguments (i.e., plot.element1 to plot.element9). To change the axis label text color, one of the plot.element arguments should be specified as follows: `plot.element1 = ggplot2::theme(axis.text = ggplot2::element_text(color = "green"))`. By default, `NULL`.
#' @param plot.element2 A ggplot2 plot function and arguments needed to add graphical element(s) required to complete a specific task, but are not specified as an argument in the function. By default, `NULL`.
#' @param plot.element3 A ggplot2 plot function and arguments needed to add graphical element(s) required to complete a specific task, but are not specified as an argument in the function. By default, `NULL`.
#' @param plot.element4 A ggplot2 plot function and arguments needed to add graphical element(s) required to complete a specific task, but are not specified as an argument in the function. By default, `NULL`.
#' @param plot.element5 A ggplot2 plot function and arguments needed to add graphical element(s) required to complete a specific task, but are not specified as an argument in the function. By default, `NULL`.
#' @param plot.element6 A ggplot2 plot function and arguments needed to add graphical element(s) required to complete a specific task, but are not specified as an argument in the function. By default, `NULL`.
#' @param plot.element7 A ggplot2 plot function and arguments needed to add graphical element(s) required to complete a specific task, but are not specified as an argument in the function. By default, `NULL`.
#' @param plot.element8 A ggplot2 plot function and arguments needed to add graphical element(s) required to complete a specific task, but are not specified as an argument in the function. By default, `NULL`.
#' @param plot.element9 A ggplot2 plot function and arguments needed to add graphical element(s) required to complete a specific task, but are not specified as an argument in the function. By default, `NULL`.
#'
#' @note When using a list for the `hide.x.items` and/or `hide.group.items` arguments, the order of the list determines what is presented in the graph in each transition and file name. The file name would be the original path, with `_Transition-0n` appended. For example, if the path is `"plot.png"` and the maximum number of lists between `hide.x.items` and `hide.group.items` is 3, the following three files would be saved: `plot_Transition-01.png`, `plot_Transition-02.png`, and `plot_Transition-03.png`. When using both `hide.x.items` and `hide.group.items` as lists, the length of both lists does not have to be equal. For example, if `hide.x.items` has three items and `hide.group.items` has two items, three plots will be created. In the last plot, no group items are hidden. Also, `NULL` could be used in a list to ensure no x or group items are hidden.
#' 
#' @return A column plot is returned as an object or saved as a file in a local directory.
#'
#' @importFrom forcats fct_reorder
#' @importFrom forcats fct_rev
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
#' @importFrom ggplot2 geom_col
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
#' @importFrom ggplot2 scale_alpha_manual
#' @importFrom ggplot2 ggplotGrob
#' @importFrom grid grid.force
#' @importFrom grid grid.newpage
#' @importFrom grid grid.draw
#' @importFrom grid grid.ls
#' @importFrom grid grid.edit
#' @importFrom grid grid.grab
#' @importFrom grid gpar
#'
#' @export

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
                        plot.width = 12,
                        plot.height = 9,
                        plot.units = "in",
                        plot.dpi = 360,
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
                        caption.vjust = -3,
                        legend.position = "none",
                        legend.order.manual = NULL,
                        legend.title = NULL,
                        legend.title.size = 16,
                        legend.label.size = 14,
                        fixed.legend.size = FALSE,
                        column.width = 0.90,
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
                        reference.line = NULL,
                        reference.line.color = "#1f2022",
                        reference.line.size = 1,
                        reference.line.linetype = 2,
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

    data2 <- data |>
      dplyr::arrange(desc({{y.var1}})) |>
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

    data2 <- data |>
      dplyr::arrange({{y.var1}}) |>
      dplyr::select({{x.var1}})

    names(data2) <- "x.var"
    x.var <- unique(data2$x.var)
    x.var <- as.character(x.var)

    data[[{{x.var1}}]] <- factor(data[[{{x.var1}}]],
                                 levels = c(x.var))

  } else{

  }

  # Ensure hide.x.items and hide.group.items are lists
  if(!is.list(hide.x.items)) {

    hide.x.items <- list(hide.x.items)

  } else {

  }
    
  if(!is.list(hide.group.items)) {
    
    hide.group.items <- list(hide.group.items)

  } else {

  }

  # Determine the maximum number of loops needed
  max.loops <- max(length(hide.x.items), length(hide.group.items), 1L)

  # Pad shorter list(s) with NULL
  if(length(hide.x.items) < max.loops){

    hide.x.items <- c(hide.x.items, rep(list(NULL), max.loops - length(hide.x.items)))

  } else {

  }

  if(length(hide.group.items) < max.loops){

    hide.group.items <- c(hide.group.items, rep(list(NULL), max.loops - length(hide.group.items)))

  } else{

  }

  # For loop 
  for(i in 1:max.loops){

    # Initialize temporary variables
    hide.x.items.temp <- NULL
    hide.group.items.temp <- NULL
    path.temp <- NULL
    col.color <- NULL

    # Specify to remove legend or x axis information from the plot
    if(!is.null(hide.group.items[[i]]) |
       !is.null(hide.x.items[[i]])){

      # Add hide.x.items if null
      if(is.null(hide.x.items[[i]])){

        hide.x.items[[i]] <- ""

      } else{

      }

      # Add hide.group.items if null
      if(is.null(hide.group.items[[i]])){

        hide.group.items[[i]] <- ""

      } else{

      }

    } else{

      hide.x.items[[i]] <- ""
      hide.group.items[[i]] <- ""

    }

    # Unlist hide variables
    hide.x.items.temp <- unlist(hide.x.items[[i]])
    hide.group.items.temp <- unlist(hide.group.items[[i]])

    # Remove column based on fill information
    if(all(hide.group.items.temp != "")){

      data <- data |>
        dplyr::mutate(alpha.fill = ifelse(group77d8214 %in% hide.group.items.temp, "b", "a"))

    } else{

      data$alpha.fill <- "a"

    }

    # Remove column based on x axis information
    if(all(hide.x.items.temp != "")){

      data <- data |>
        dplyr::mutate(alpha.varx = ifelse({{x.var1}} %in% hide.x.items.temp, "b", "a"))

    } else{

      data$alpha.varx <- "a"

    }

    # Specify an overall alpha variable
    data <- data |>
      dplyr::mutate(alpha = ifelse(alpha.fill == "b" | alpha.varx == "b", "b", "a"))

    # Specify if the legend should be reordered
    if(!is.null(legend.order.manual) &
       group.var != ""){

      data$group77d8214 <- factor(data$group77d8214,
                                  levels = legend.order.manual)

      data.temp <- data |>
        dplyr::arrange(group77d8214)|>
        dplyr::filter(alpha == "a") |>
        dplyr::select(group77d8214)

      names(data.temp) <- "break1"
      break1 <- as.character(unique(data.temp$break1))

    } else if(is.null(legend.order.manual) &
              group.var != ""){

      data.temp <- data |>
        dplyr::filter(alpha == "a") |>
        dplyr::select(group77d8214)

      names(data.temp) <- "break1"
      break1 <- as.character(unique(data.temp$break1))

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

    # Specify color of text and columns
    if(group.var == ""){

      col.color <- color[1]

    } else{

      if(all(hide.group.items.temp == "")){

        col.color <- rep(color,
                         length.out = length(unique(data$group77d8214)))

      } else{

        df.color <- data.frame(unique(data$group77d8214), 
                               rep(color,
                                   length.out = length(unique(data$group77d8214))))
        
        names(df.color) <- c("group77d8214", "color")

        df.data <- data |>
          dplyr::select(group77d8214, alpha.fill) |>
          unique()

        df.color <- df.color |>
          dplyr::inner_join(df.data, by = "group77d8214") |>
          dplyr::mutate(color = ifelse(alpha.fill == "b", "#FFFFFF", color))

        if(!is.null(legend.order.manual)){

          df.color$group77d8214 <- factor(df.color$group77d8214,
                                          levels = legend.order.manual)

          df.color <- df.color |>
            dplyr::arrange(group77d8214)

        } else{

        }

        df.color$group77d8214 <- factor(df.color$group77d8214,
                                        levels = sort(unique(data$group77d8214)))

        df.color <- df.color |>
          dplyr::arrange(group77d8214)
        
        col.color <- df.color$color

        legend_labels <- df.color$group77d8214

      }

    }

    # Specify the primary graph properties and x axis order
    col.graph <- ggplot2::ggplot(data,
      {if(x.ascend  == TRUE &
          group.var == ""){

        ggplot2::aes(x = forcats::fct_reorder({{x.var1}}, {{y.var1}}),
                    y = {{y.var1}},
                    fill = col.color)

      } else if(x.ascend  == TRUE &
                group.var != ""){

        ggplot2::aes(x = forcats::fct_reorder({{x.var1}}, {{y.var1}}),
                    y = {{y.var1}},
                    fill = group77d8214)

      } else if(x.descend  == TRUE &
                group.var == ""){

        ggplot2::aes(x = forcats::fct_rev(forcats::fct_reorder({{x.var1}}, {{y.var1}})),
                    y = {{y.var1}},
                    fill = col.color)

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
                    fill = col.color)

      }
    }) +

    # Specify plot as geom_col and alpha
    {if(!is.null(hide.x.items.temp) |
        !is.null(hide.group.items.temp)){

      ggplot2::geom_col(position = col.position,
                        ggplot2::aes(alpha = alpha),
                        width = column.width)

    } else{

      ggplot2::geom_col(position = col.position,
                        width = column.width)
      }
    } +

    # Remove columns based on alpha
    {if(!is.null(hide.x.items.temp)){

      ggplot2::scale_alpha_manual(values = c("a" = 1, "b" = 0),
                                  guide = 'none')

    } else{

      }
    } +

    # Remove legend information based on alpha and specify fill color
    {if(all(hide.group.items.temp != "") &
        fixed.legend.size == FALSE){

        ggplot2::scale_fill_manual(values = col.color,
                                   breaks = break1)

      } else if(all(hide.group.items.temp == "") &
                fixed.legend.size == FALSE){
      
        ggplot2::scale_fill_manual(values = col.color)

       } else if(all(hide.group.items.temp != "") &
                 fixed.legend.size == TRUE){
      
         ggplot2::scale_fill_manual(values = col.color, 
                                    labels = legend_labels)

        } else{

          ggplot2::scale_fill_manual(values = col.color)

         }
    } +

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

    # Specify caption information
    {if(caption == TRUE){

      col.graph <- col.graph +
        CCMHr::ccmh_caption()

    } else{

      }
    }

    # Specify reference line information
    if(!is.null(reference.line)){

      # Determine the length of reference line properties
      reference.length <- length(reference.line) == length(reference.line.color) &
                          length(reference.line) == length(reference.line.size) &
                          length(reference.line) == length(reference.line.linetype)

      # If the lengths are not equal, adjust the lengths
      if(reference.length == FALSE){

        # Error message
        stop("The length of reference.line, reference.line.color, reference.line.size, and reference.line.linetype must be equal.")

      } else{

        for(i in 1:length(reference.line)){

        col.graph <- col.graph +
          ggplot2::geom_hline(yintercept = reference.line[i],
                              color = reference.line.color[i],
                              linewidth = reference.line.size[i],
                              linetype = reference.line.linetype[i])

        }

      }

    } else{

    }

    # Specify fixed legend based on hide.group.items.temp
    if(any(data$group77d8214 %in% hide.group.items.temp) == TRUE &
       group.var != "" &
       fixed.legend.size == TRUE){

      # Convert plot to grob
      g <- grid::grid.force(ggplot2::ggplotGrob(col.graph))

      # Draw the plot grob so grid.edit() can target drawn grobs
      grid::grid.newpage()
      grid::grid.draw(g)

      # Locate the legend index based on legend position
      if(legend.position == "right") {

        legend_index <- which(g$layout$name == "guide-box-right" | grepl("guide-box-right", g$layout$name))

      } else if(legend.position == "left") {

        legend_index <- which(g$layout$name == "guide-box-left" | grepl("guide-box-left", g$layout$name))

      } else if(legend.position == "top") {

        legend_index <- which(g$layout$name == "guide-box-top" | grepl("guide-box-top", g$layout$name))

      } else if(legend.position == "bottom") {

        legend_index <- which(g$layout$name == "guide-box-bottom" | grepl("guide-box-bottom", g$layout$name))

      } else {

      }

      # Inspect legend-forced to find the text grob names
      legend_forced <- grid::grid.force(g$grobs[[legend_index]])

      # Choose target text grob names
      legend_names <- grid::grid.ls(legend_forced, print = FALSE)$name
      text_names   <- legend_names[grepl("^GRID.text", legend_names)]
      label_text_names <- tail(text_names, length(col.color))  

      # Now edit the drawn grobs' colours (acts on the plotted device)
      for (j in seq_along(label_text_names)) {

        grid::grid.edit(label_text_names[j], 
                        gp = grid::gpar(col = col.color[j]), 
                        grep = TRUE)
          
      }

      # Grab the modified plot as a grob
      col.graph <- grid::grid.grab()
    
    } else{

    }

    # Specify if the graph should be saved as file or returned as an object
    if(save == TRUE){

      # Specify path for each transition if multiple loops
      if(max.loops > 1){

        path.temp <- substr(path, 1, nchar(path) - 4)
        path.end <- substr(path, pmax(nchar(path) - 3, 1), nchar(path))
        path.temp <- paste0(path.temp, "_Transition-", sprintf("%02d", as.integer(i)), path.end)

      } else{

        path.temp <- path

      }

      # Save the plot
      ggplot2::ggsave(paste0(path.temp),
                      plot = col.graph,
                      width = plot.width,
                      height = plot.height,
                      units = plot.units,
                      dpi = plot.dpi,
                      device = plot.device,
                      scale = plot.scale)

    } else{

      # Render the plot to the active device while the function runs
      if (interactive()) print(col.graph)
     
    }

  }

  # Warning message and return graph
  if(save == FALSE){

    # Warning message
    if(max.loops > 1 |
       fixed.legend.size == TRUE &
       save == FALSE){

        warning("Returning plot object is not supported when fixed.legend.size = TRUE or multiple plots are created based on hide.group.items and/or hide.x.items. To save the plot(s), set save = TRUE and specify a path.")

      } else{

      }

    if (interactive()) return(col.graph)

  } else{

  }

}

#' @rdname plot_column
#' @export
column_plot <- plot_column
