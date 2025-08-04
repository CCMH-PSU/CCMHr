#' Create CCMH demographic tables.
#'
#' @description CCMH collected various types of demographic variables within the SDS. This function creates tables that summarize various demographic variables. Demographic variables include: Prior Counseling (SDS_01), Prior Medication (SDS_02), Prior Hospitalization (SDS_64), Gender Identity (SDS_88), Sexual Orientation (SDS_91), Race/Ethnicity (SDS_95), Academic Year/Status (SDS_37), Academic Year/Status (SDS_1037), International Student Status (SDS_32), and First Generation Student Status (SDS_56).
#'
#' @param data A data frame that contains demographic-related variables.
#' @param decimal_place A numeric value to indicate the number of decimal places to round numbers within the tables. By default, `2`.
#' @param text.font A quoted string to specify the text font type used in the table. By default, `"Avenir"`.
#' @param print.tables A logical argument that indicates whether the table should be printed in the console. If `TRUE`, the tables will be printed in the console. By default, `TRUE`.
#' @param save A logical argument that indicates whether the table should be saved as a file under a local folder. If `FALSE`, the table will be returned as an object. By default, `FALSE`.
#' @param path A quoted string to indicate the file's pathway and name if save = `TRUE`. The file must be an HTML file. By default, `"table.html"`.
#'
#' @return Print demographic tables in the console and/or save the tables within the working directory or a local folder.
#'
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom gt gt
#' @importFrom gt cols_label
#' @importFrom gt cols_align
#' @importFrom gt tab_options
#' @importFrom gt md
#' @importFrom gt tab_header
#' @importFrom htmltools tagList
#' @importFrom htmltools as.tags
#' @importFrom htmltools save_html
#'
#' @export

demographic_tables <- function(data,
                               decimal_place = 2,
                               text.font = "Avenir",
                               print.tables = TRUE,
                               save = FALSE,
                               path = "demograpics.xlsx") {

  # Change SDS into factor
  data <- CCMHr::sds_to_factor(data)

  # A function to create tables
  func1 <- function(data) {

    nrows.n <- nrow(data)

    if (nrows.n > 1) {

      data <- data |>
        dplyr::rename("Cumulative Percent" = "Cum Percent") |>
        dplyr::mutate(Percent = round(Percent, digits = decimal_place),
                      `Valid Percent` = round(`Valid Percent`, digits = decimal_place),
                      `Cumulative Percent` = round(`Cumulative Percent`, digits = decimal_place),
                      `Valid Percent` = ifelse(is.na(`Valid Percent`), "--", `Valid Percent`),
                      `Cumulative Percent` = ifelse(is.na(`Cumulative Percent`), "--", `Cumulative Percent`),
                      Frequency = format(Frequency, format = "d", big.mark = ",")) |>
        gt::gt() |>
        gt::cols_label(`Valid Percent` = "Valid<br>Percent",
                       `Cumulative Percent` = "Cumulative<br>Percent",
                       .fn = gt::md) |>
        gt::cols_align(align = "center") |>
        gt::tab_options(table.font.names = "Avenir",
                        table.border.top.color = "white",
                        table.border.bottom.color = "white",
                        table_body.hlines.color = "white")

    } else {

      data <- data |>
        gt::gt()

    }

    return(data)

  }

  # create tables for each demographic variable
  couns <- CCMHr::freqtab(data$SDS_01) |>
    dplyr::rename(" " = "data$SDS_01") |>
    func1() |>
    gt::tab_header(title = 'Prior Counseling (SDS_01)')

  med <- CCMHr::freqtab(data$SDS_02) |>
    dplyr::rename(" " = "data$SDS_02") |>
    func1() |>
    gt::tab_header(title = 'Prior Medication (SDS_02)')

  hosp <- CCMHr::freqtab(data$SDS_64) |>
    dplyr::rename(" " = "data$SDS_64") |>
    func1() |>
    gt::tab_header(title = 'Prior Hospitalization (SDS_64)')

  gender <- CCMHr::freqtab(data$SDS_88) |>
    dplyr::rename(" " = "data$SDS_88") |>
    func1() |>
    gt::tab_header(title = 'Gender Identity (SDS_88)')

  sexual.orientation <- CCMHr::freqtab(data$SDS_91) |>
    dplyr::rename(" " = "data$SDS_91") |>
    func1() |>
    gt::tab_header(title = 'Sexual Orientation (SDS_91)')

  race <- CCMHr::freqtab(data$SDS_95) |>
    dplyr::rename(" " = "data$SDS_95") |>
    func1() |>
    gt::tab_header(title = 'Race/Ethnicity (SDS_95)')

  cohort1 <- CCMHr::freqtab(data$SDS_37) |>
    dplyr::rename(" " = "data$SDS_37") |>
    func1() |>
    gt::tab_header(title = 'Academic Year/Status (SDS_37)')

  cohort2 <- CCMHr::freqtab(data$SDS_1037) |>
    dplyr::rename(" " = "data$SDS_1037") |>
    func1() |>
    gt::tab_header(title = 'Academic Year/Status (SDS_1037)')

  international <- CCMHr::freqtab(data$SDS_32) |>
    dplyr::rename(" " = "data$SDS_32") |>
    func1() |>
    gt::tab_header(title = 'International Student Status (SDS_32)')

  firstgen <- CCMHr::freqtab(data$SDS_56) |>
    dplyr::rename(" " = "data$SDS_56") |>
    func1() |>
    gt::tab_header(title = 'First Generation Student Status (SDS_56)')

  # Print tables
  if(print.tables == TRUE){
    table.list <- list(couns, med,
                       hosp, gender,
                       sexual.orientation, race,
                       cohort1, cohort2,
                       international, firstgen)

    print(table.list)
  } else {

  }

  # Save tables
  if(save == TRUE) {

    html_file <- htmltools::tagList(htmltools::as.tags(couns),
                                    htmltools::as.tags(med),
                                    htmltools::as.tags(hosp),
                                    htmltools::as.tags(gender),
                                    htmltools::as.tags(sexual.orientation),
                                    htmltools::as.tags(race),
                                    htmltools::as.tags(cohort1),
                                    htmltools::as.tags(cohort2),
                                    htmltools::as.tags(international),
                                    htmltools::as.tags(firstgen))

    htmltools::save_html(html_file,
                         file = path)

  } else {

  }

}
