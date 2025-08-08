#' Format tables into APA.
#'
#' @name apa_table
#'
#' @description This function takes in a table and reformats the table into APA.
#'
#' @param data A data file that contains details of the table.
#' @param header.names A list of quoted strings that specifies column or header names. By default, `NULL`.
#' @param title A quoted string to indicate the title of the table. By default, `"Table #"`.
#' @param subtitle A quoted string to indicate the subtitle of the table. By default, `"Add subtitle"`.
#' @param save A logical argument that indicates whether the table should be saved as a file under a local folder. If `FALSE`, the table will be returned as an object. By default, `FALSE`.
#' @param path A quoted string to indicate the file's pathway, name, and type if `save = TRUE`. By default, `"table.png"`.
#' @param width A numeric value to indicate the width of the table. By default, `1`.
#' @param note A quoted string to specify a note at the bottom of the table. By default, `NULL`.
#'
#' @return Print and saves APA formatted tables.
#'
#' @importFrom gt gt
#' @importFrom gt tab_header
#' @importFrom gt html
#' @importFrom gt cols_align
#' @importFrom gt tab_options
#' @importFrom gt tab_footnote
#' @importFrom gt everything
#' @importFrom gt px
#' @importFrom gt tab_style
#' @importFrom gt cell_borders
#' @importFrom gt cells_column_labels
#' @importFrom gt pct
#' @importFrom gt gtsave
#'
#' @export

apa_table <- function(data,
                      header.names = NULL,
                      title = "Table #",
                      subtitle = "Add subtitle",
                      save = FALSE,
                      path = "table.png",
                      width = 1,
                      note = NULL) {

  # Extract the number of columns in data and header names listed
  ncol <- ncol(data)
  header.n <- length(header.names)

  # Subtract the number of columns in data and header names listed
  ncol.header.diff <- ncol - header.n

  # Header names warning
  if(!is.null(header.names) &
     ncol.header.diff != 0){

    stop("The number of header names listed must match the number of columns in the data.")

  } else {

  }

  # Change column names
  if(!is.null(header.names)){

    colnames(data) <- header.names

  } else {

  }

  # Create title
  title_html <- sprintf("<div style='line-height: 2;'>%s</div>", title)

  # Create subtitle
  subtitle_html <- sprintf("<div style='line-height: 2;'>%s</div>", subtitle)

  # Create GT table
  data <- data |>
    gt::gt() |>
    gt::tab_header(title = gt::html(paste0("<b>", title_html, "</b>")),
                   subtitle = gt::html(paste0("<i>", subtitle_html, "</i>"))) |>
    gt::cols_align(align = "center",
                   columns = gt::everything()) |>
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
                    data_row.padding = gt::px(4))  |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("top", "bottom"),
        color = "black",
        weight = gt::px(0.667),
        style = "solid"),
      locations = gt::cells_column_labels())

  # Add note
  if(!is.null(note)){

    data <- data |>
      gt::tab_footnote(footnote = gt::html(paste0("<i>", "Note. ", "</i>", note))) |>
      gt::tab_options(footnotes.font.size = 16,
                      footnotes.padding = gt::px(6),
                      footnotes.border.lr.color = "white",
                      table.border.bottom.color = "white",
                      table.font.names = "Times New Roman",
                      table.font.color = "black")

  } else{

  }

  # table width
  data <- data |>
    gt::tab_options(table.width = gt::pct(width))

  # Save table
  if(save == TRUE){

    data |>
      gt::gtsave(filename = path)

  } else {

    return(data)

  }

}

#' @rdname apa_table
#' @export
APA_table <- apa_table
