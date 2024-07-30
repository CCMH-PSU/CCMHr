#' Create APA formatted tables
#'
#' @description Create create APA formatted tables. 
#' @param data A data file.
#' @param header.names A vector of header names to change columns names. Default is `NULL`.
#' @param title The title of the table. Default is `Table #`.
#' @param subtitle The subtitle of the table. Default is `Add subtitle`.
#' @param save A logical statement indicates whether the table should be saved as a file under a local folder. If false, the table will be returned as an object. By default, `FALSE`.
#' @param path A quoted string to indicate the file's pathway and name if `save = TRUE`. By default, `"table.png"`.
#' @param width A numeric value between 0 to 1 to indicate the width of the table. By default, `NULL`.
#' @param note A quoted string to add a note at the bottom of the table. By default, `NULL`.
#' 
#' @return Print and saves APA formatted tables.
#' @export
#' 
#' 

apa_table <- function(data, 
                      header.names = NULL, 
                      title = "Table #",
                      subtitle = "Add subtitle", 
                      save = FALSE, 
                      path = "table.png", 
                      width = NULL, 
                      note = NULL) {

    # Extract the number of columns in data and header names listed
    ncol <- ncol(data)
    header.n <- length(header.names)

    # Substract the number of columns in data and header names listed
    ncol.header.diff <- ncol - header.n

    # Header names warning
    if(!is.null(header.names) & 
       ncol.header.diff != 0){ 
        stop("The number of header names listed must match the number of columns in the data.")
    } else {
       #Skip
    }

    # Change column names
    if(!is.null(header.names)){
        colnames(data) <- header.names
    } else {
       #Skip
    }

    # Create title 
    title_html <- sprintf("<div style='line-height: 2;'>%s</div>", title)

    # Create subtitle
    subtitle_html <- sprintf("<div style='line-height: 2;'>%s</div>", subtitle)

    # Create GT table
    data <- data %>%
       gt::gt() %>% 
       gt::tab_header(title = gt::html(paste0("<b>", title_html, "</b>")), 
                      subtitle = gt::html(paste0("<i>", subtitle_html, "</i>"))) %>%
       gt::cols_align(align = "center",
                      columns = gt::everything()) %>%
       gt::tab_options(table.font.names = "Times New Roman",
                       table.font.size = 16,
                       table.border.top.style = "solid",
                       table.border.top.width = gt::px(0.667),
                       table.border.top.color = "white",
                       table.border.right.style = "solid",
                       table.border.right.width = gt::px(0.667),
                       table.border.right.color = "white",
                       table.border.bottom.style = "solid",
                       table.border.bottom.width = gt::px(0.667),
                       table.border.bottom.color = "black",
                       table.border.left.style = "solid",
                       table.border.left.width = gt::px(0.667),
                       table.border.left.color = "white",
                       heading.title.font.size = 16,
                       heading.subtitle.font.size = 16, 
                       heading.align = "left", 
                       heading.title.font.weight = "bolder", 
                       heading.border.bottom.style = "solid",
                       heading.border.bottom.width = gt::px(0.667),
                       heading.border.bottom.color = "black",
                       column_labels.padding = gt::px(6),
                       column_labels.border.top.style = "solid",
                       column_labels.border.top.width = gt::px(0.667),
                       column_labels.border.top.color = "black",
                       column_labels.border.bottom.style = "solid",
                       column_labels.border.bottom.width = gt::px(0.667),
                       column_labels.border.bottom.color = "black", 
                       row_group.border.top.style = "solid",
                       row_group.border.top.width = gt::px(0.667),
                       row_group.border.top.color = "black",
                       row_group.border.bottom.style = "solid",
                       row_group.border.bottom.width = gt::px(0.667),
                       row_group.border.bottom.color = "black",
                       table_body.border.top.style = "solid",
                       table_body.border.top.width = gt::px(0.667),
                       table_body.border.top.color = "black",
                       table_body.border.bottom.style = "solid",
                       table_body.border.bottom.width = gt::px(0.667),
                       table_body.border.bottom.color = "black",
                       table_body.hlines.color = "white", 
                       table.font.color = "black", 
                       stub.border.style = "solid",
                       stub.border.width = gt::px(0.667),
                       stub.border.color = "black",
                       stub_row_group.border.style = "solid",
                       stub_row_group.border.width = gt::px(0.667),
                       stub_row_group.border.color = "black",
                       data_row.padding = gt::px(4))  %>%
       gt::tab_style(
           style = gt::cell_borders(
           sides = c("top", "bottom"),
           color = "black",
           weight = gt::px(0.667),
           style = "solid"),
        locations = gt::cells_column_labels())

    # Add note
    if(!is.null(note)){
        data <- data %>%
            gt::tab_footnote(footnote = gt::html(paste0("<i>", "Note. ", "</i>", note)))%>%
            gt::tab_options(footnotes.font.size = 16,
                            footnotes.padding = gt::px(6),
                            footnotes.border.lr.color = "white",
                            table.border.bottom.color = "white",
                            table.font.names = "Times New Roman", 
                            table.font.color = "black")
    } else {
        #Skip
    }

    # table width
    data <- data %>%
        gt::tab_options(table.width = gt::pct(width))

    # Save tableS
    if(save == TRUE){
        data %>% 
          gt::gtsave(filename = path)
    } else {
        return(data)
    }

}