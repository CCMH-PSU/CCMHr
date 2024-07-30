#' Create demographic tables
#'
#' @description Create CCMH demographic tables. 
#' @param data A data file containing demographic information.
#' @param decimal_place The number of decimal places to round to. Default is `2`.
#' @param text.font The font type of the text in the table. Default is `Avenir`.
#' @param print.tables A logical statement indicates whether the table should be printed in the console. By default, `TRUE`.
#' @param save A logical statement indicates whether the table should be saved as a file under a local folder. If false, the table will be returned as an object. By default, `FALSE`.
#' @param path A quoted string to indicate the file's pathway and name if `save = TRUE`. By default, `"table.html"`. File must be an html file.
#' 
#' @return Prints demographic tables in concole and/or saves them in under the working directory.
#' @export
#' 
#' 

demographic_tables <- function(data, 
                               decimal_place = 2, 
                               text.font = "Avenir", 
                               print.tables = TRUE,
                               save = FALSE, 
                               path = "demograpics.xlsx") {

    # SDS to factor
    data <- CCMHr::sds_to_factor(data)

    # function to create tables
    func1 <- function(data) {

      nrows.n <- nrow(data)

      if (nrows.n > 1) {
        data <- data %>% 
            dplyr::rename("Cumulative Percent" = "Cum Percent") %>%
            dplyr::mutate(Percent = round(Percent, digits = decimal_place), 
                            `Valid Percent` = round(`Valid Percent`, digits = decimal_place), 
                            `Cumulative Percent` = round(`Cumulative Percent`, digits = decimal_place), 
                            `Valid Percent` = ifelse(is.na(`Valid Percent`), "--", `Valid Percent`), 
                            `Cumulative Percent` = ifelse(is.na(`Cumulative Percent`), "--", `Cumulative Percent`), 
                            Frequency = format(Frequency, format = "d", big.mark = ",")) %>%
            gt::gt() %>%
            gt::cols_label(`Valid Percent` = "Valid<br>Percent",
                        `Cumulative Percent` = "Cumulative<br>Percent",
                        .fn = md) %>%
            gt::cols_align(align = "center") %>%
            gt::tab_options(table.font.names = "Avenir",
                            table.border.top.color = "white", 
                            table.border.bottom.color = "white", 
                            table_body.hlines.color = "white") 
      } else {
      data <- data %>% 
          gt::gt()
      }

       return(data)

    }

    # create tables
    couns <- CCMHr::freqtab(data$SDS_01) %>%
      dplyr::rename(" " = "data$SDS_01") %>%
      func1() %>%
      gt::tab_header(title = 'Prior Counseling (SDS_01)')

    med <- CCMHr::freqtab(data$SDS_02) %>%
      dplyr::rename(" " = "data$SDS_02") %>%
      func1() %>%
      gt::tab_header(title = 'Prior Medication (SDS_02)')

    hosp <- CCMHr::freqtab(data$SDS_64) %>%
      dplyr::rename(" " = "data$SDS_64") %>%
      func1() %>%
      gt::tab_header(title = 'Prior Hospitalization (SDS_64)')

    gender <- CCMHr::freqtab(data$SDS_88) %>%
      dplyr::rename(" " = "data$SDS_88") %>%
      func1() %>%
      gt::tab_header(title = 'Gender Identity (SDS_88)')

    sexual.orientation <- CCMHr::freqtab(data$SDS_91) %>%
      dplyr::rename(" " = "data$SDS_91") %>%
      func1() %>%
      gt::tab_header(title = 'Sexual Orientation (SDS_91)')

    race <- CCMHr::freqtab(data$SDS_95) %>%
      dplyr::rename(" " = "data$SDS_95") %>%
      func1() %>%
      gt::tab_header(title = 'Race/Ethnicity (SDS_95)')

    cohort1 <- CCMHr::freqtab(data$SDS_37) %>%
      dplyr::rename(" " = "data$SDS_37") %>%
      func1() %>%
      gt::tab_header(title = 'Academic Year/Status (SDS_37)')

    cohort2 <- CCMHr::freqtab(data$SDS_1037) %>%
      dplyr::rename(" " = "data$SDS_1037") %>%
      func1() %>%
      gt::tab_header(title = 'Academic Year/Status (SDS_1037)')

    international <- CCMHr::freqtab(data$SDS_32) %>%
      dplyr::rename(" " = "data$SDS_32") %>%
      func1() %>%
      gt::tab_header(title = 'International Student Status (SDS_32)')

    firstgen <- CCMHr::freqtab(data$SDS_56) %>%
      dplyr::rename(" " = "data$SDS_56") %>%
      func1() %>%
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
      # skip
    }

  # Save tables
  if(save == TRUE) {

    html_file <- htmltools::tagList(
      htmltools::as.tags(couns),
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
    # skip
  }

}