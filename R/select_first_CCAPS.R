#' Select each client's first administration of a specified survey
#'
#' @param data data frame
#' @param order_by variable to order by when selecting the first CCAPS
#' @param keep_all Columns to keep. `TRUE` will keep all columns, while `FALSE` will keep only IDs and CCAPS subscales, SDS items, or CLICC items.
#' @param keep_columns A string list of column names to retain. If not specified, and keep_all = FALSE, defaults to the relevent columns for that data form.
#'
#' @return
#' @export
#'

select_first_CCAPS <- function(data, order_by = Date, keep_all = FALSE, keep_columns = c("UniqueClientID", "CcmhID", "Depression34", "Anxiety34",
                                                                                         "Social_Anxiety34", "Academics34", "Eating34", "Hostility34",
                                                                                         "Alcohol34", "DI", "Depression62", "Eating62", "Substance62",
                                                                                         "Anxiety62", "Hostility62", "Social_Anxiety62", "Family62", "Academics62")) {

  if (!deparse(substitute(order_by)) %in% names(data)) {
    stop("order_by variable not present in data")
  }

  if (!"Is_ValidCCAPS" %in% names(data)) {
    stop("Is_ValidCCAPS variable not present in data")
  }

  ccaps <- dplyr::filter(data, .data$Is_ValidCCAPS ==1) %>%
    dplyr::arrange(.data$UniqueClientID, {{order_by}}) %>%
    dplyr::group_by(.data$UniqueClientID) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()


  if (keep_all == TRUE) {
    if (!setequal(keep_columns,
        c("Depression34", "Anxiety34",
                          "Social_Anxiety34", "Academics34", "Eating34", "Hostility34",
                          "Alcohol34", "DI", "Depression62", "Eating62", "Substance62",
                          "Anxiety62", "Hostility62", "Social_Anxiety62", "Family62", "Academics62"))) {
      usethis::ui_warn("Vector of column names to keep not used when keep_all = TRUE. All columns were retained.")
    }

    return(ccaps)

  } else if (keep_all == FALSE) {
    if (!all(keep_columns %in% names(data))) {
      usethis::ui_warn("All columns specified in keep_columns were not present in the data. Only present columns were retained.")
    }
      dplyr::select(ccaps, tidyselect::any_of(keep_columns))
  }

}

#' @export
#' @rdname select_first_CCAPS

select_first_SDS <- function(data,
                             order_by = Date,
                             keep_all = FALSE,
                             keep_columns = "SDS") {

  if (!deparse(substitute(order_by)) %in% names(data)) {
    stop("order_by variable not present in data")
  }

  if (!"Has_SDS" %in% names(data)) {
    stop("Has_SDS variable not present in data")
  }

  sds <- dplyr::filter(data, .data$Has_SDS ==1) %>%
    dplyr::arrange(.data$UniqueClientID, {{order_by}}) %>%
    dplyr::group_by(.data$UniqueClientID) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  if (keep_all == TRUE) {
    if (keep_columns[1] != "SDS")
      usethis::ui_warn("Vector of column names to keep not applicable when keep_all = TRUE. All columns were retained.")

    return(sds)

  } else if (keep_all == FALSE) {
    if (keep_columns[1] == "SDS") {
      sds %>%
        dplyr::select(.data$UniqueClientID, .data$CcmhID, dplyr::starts_with("SDS_"))
    } else {
      if (!all(keep_columns %in% names(data))) {
        usethis::ui_warn("All columns specified in keep_columns were not present in the data. Only present columns were retained.")
      }
        dplyr::select(sds, tidyselect::any_of(keep_columns))
    }

  }
}

#' @export
#' @rdname select_first_CCAPS

select_first_CLICC <- function(data,
                               order_by = Date,
                               keep_all = FALSE,
                               keep_columns = "CLICC") {

  if (!deparse(substitute(order_by)) %in% names(data)) {
    stop("order_by variable not present in data")
  }

  if (!"Has_CLICC" %in% names(data)) {
    stop("Has_CLICC variable not present in data")
  }

  clicc <- dplyr::filter(data, .data$Has_CLICC ==1) %>%
    dplyr::arrange(.data$UniqueClientID, {{order_by}}) %>%
    dplyr::group_by(.data$UniqueClientID) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  if (keep_all == TRUE) {
    if (keep_columns[1] != "CLICC")
      usethis::ui_warn("Vector of column names to keep not applicable when keep_all = TRUE. All columns were retained.")

    return(clicc)

  } else if (keep_all == FALSE) {
    if (keep_columns[1] == "CLICC") {
      clicc %>%
        dplyr::select(.data$UniqueClientID, .data$CcmhID, dplyr::starts_with("CLICC_"))
    } else {
      if (!all(keep_columns %in% names(data))) {
        usethis::ui_warn("All columns specified in keep_columns were not present in the data. Only present columns were retained.")
      }
      dplyr::select(clicc, tidyselect::any_of(keep_columns))
    }
    }

}
