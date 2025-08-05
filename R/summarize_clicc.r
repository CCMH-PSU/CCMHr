#' Summarize CLICC data.
#'
#' @description This function cleans and summarizes data from the CLICC. The function returns a dataset containing summarized CLICC data. The summarization process creates the following new variables: sample.n (The number of each CLICC concern being endorsed) and percent (The percentage that a CLICC item is endorsed). The clicc_type argument allows the researcher to specify whether the summary should be based on checking all that apply (CLICC_01\_##) or the top concern (CLICC_03). The summary could also be in relation to specific grouping variables using the group_vars argument.
#'
#' @param data A data frame containing CLICC.
#' @param clicc_type A quoted string to indicate whether the summary should be based on checking all that apply (CLICC_01\_##) or the top concern (CLICC_03). Options include: `"checkall"` or `"topconcern"`. By default, `"checkall"`.
#' @param group_vars A quoted string or list of quoted strings to specify if grouping variables should influence the summary. By default, `NULL`.
#' @param save A logical argument that indicates whether the summarized data should be saved as a file under a local folder. If `FALSE`, the summarized data will be returned as an object. By default, `FALSE`.
#' @param path A quoted string to indicate the file's type, pathway, and name if save = `TRUE`. By default, `"clicc_summary.csv"`.
#'
#' @return A data frame containing details on checking all that apply or top CLICC concerns. Variables returned in the dataset include variables in the CLICC key (Item, Concern, and Number), sample.n, and percent.
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom dplyr contains
#' @importFrom dplyr n_distinct
#' @importFrom dplyr first
#' @importFrom dplyr arrange
#' @importFrom dplyr inner_join
#' @importFrom dplyr desc
#' @importFrom dplyr group_by
#' @importFrom dplyr all_of
#' @importFrom dplyr ungroup
#' @importFrom dplyr recode
#' @importFrom dplyr case_when
#' @importFrom dplyr across
#' @importFrom dplyr rename
#' @importFrom dplyr n
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr drop_na
#'
#' @export

summarize_clicc <- function(data,
                            clicc_type = "checkall",
                            group_vars = NULL,
                            save = FALSE,
                            path = "clicc_summary.csv"){

  # Check if variables are in data
  if(!all(c("UniqueClientID") %in% colnames(data))){

    stop("The data does not contain UniqueClientID.")

  } else{

  }

  # Checking group_vars
  if(!all(group_vars %in% colnames(data))){

    stop("The data does not contain the grouping variables specified in group_vars.")

  } else {

  }

  # Check if clicc_type is "checkall" or "topconcern"
  if(!clicc_type %in% c("checkall", "topconcern")){

    stop("The clicc_type must be either 'checkall' or 'topconcern'.")

  } else {

  }

  # Check all
  if("checkall" == clicc_type){

    # Mutate data
    data$CLICC_01_1006[which(data$CLICC_01_06 == 1)] <- 1
    data$CLICC_01_1028[which(data$CLICC_01_28 == 1)] <- 1
    data$CLICC_01_06 <- NULL
    data$CLICC_01_28 <- NULL

    data <- data |>
      dplyr::mutate(CLICC_01_01 = dplyr::case_when(CLICC_01_1101 == 1 ~ 1,
                                                   CLICC_01_1102 == 1 ~ 1,
                                                   CLICC_01_1103 == 1 ~ 1,
                                                   CLICC_01_1104 == 1 ~ 1,
                                                   CLICC_01_1105 == 1 ~ 1,
                                                   CLICC_01_1106 == 1 ~ 1,
                                                   CLICC_01_01 == 1 ~ 1))

    # Specify group_vars
    if(is.null(group_vars)){

      # Data filter
      df.clicc.check.all <- data |>
        dplyr::select(UniqueClientID,
                      dplyr::contains("CLICC_01")) |>
        tidyr::pivot_longer(c(CLICC_01_01:CLICC_01_1106),
                            names_to = "Item",
                            values_to = "value") |>
        tidyr::drop_na() |>
        dplyr::mutate(sample.n = dplyr::n_distinct(UniqueClientID)) |>
        dplyr::summarise(percent = sum(value == 1, na.rm = TRUE)/sample.n,
                         sample.n = dplyr::first(sample.n),
                         .by = c("Item")) |>
        dplyr::arrange(dplyr::desc(percent)) |>
        dplyr::inner_join(CCMHr::clicc_key)

      df.clicc.check.all <- unique(df.clicc.check.all)

    } else {

      # Data filter
      df.clicc.check.all <- data |>
        dplyr::select(UniqueClientID, group_vars,
                      dplyr::contains("CLICC_01")) |>
        tidyr::pivot_longer(c(CLICC_01_01:CLICC_01_1106),
                            names_to = "Item",
                            values_to = "value") |>
        tidyr::drop_na() |>
        dplyr::mutate(sample.n = dplyr::n_distinct(UniqueClientID),
                      .by = group_vars) |>
        dplyr::summarise(percent = sum(value == 1, na.rm = TRUE)/sample.n,
                         .by = c("Item", group_vars))  |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
        dplyr::arrange(dplyr::desc(percent), .by_group = TRUE) |>
        dplyr::ungroup() |>
        dplyr::inner_join(CCMHr::clicc_key)

      df.clicc.check.all <- unique(df.clicc.check.all)

    }

    # Save file
    if(save == TRUE){

      write.csv(df.clicc.check.all,
                file = path,
                row.names = FALSE)

    } else{

      return(df.clicc.check.all)

    }

  } else {

  }

  # Top concern
  if("topconcern" == clicc_type){

    # Data processing
    data$CLICC_03 <- data |>
      dplyr::recode(CLICC_03,
                    `1101` = 1L,
                    `1102` = 1L,
                    `1103` = 1L,
                    `1104` = 1L,
                    `1105` = 1L,
                    `1106` = 1L)

    data$CLICC_03[which(data$CLICC_03 == 6)] <- 1006
    data$CLICC_03[which(data$CLICC_03 == 28)] <- 1028

    # Specify group_vars
    if(is.null(group_vars)){

      # Data filter
      df.clicc.check.top <- data |>
        dplyr::select(UniqueClientID,
                      dplyr::contains("CLICC_03")) |>
        tidyr::drop_na() |>
        dplyr::mutate(sample.n = dplyr::n_distinct(UniqueClientID)) |>
        dplyr::rename("Number" = "CLICC_03") |>
        dplyr::summarise(percent = dplyr::n()/sample.n,
                         percent = format(percent, scientific = FALSE),
                         .by = c("Number")) |>
        dplyr::arrange(dplyr::desc(percent)) |>
        dplyr::inner_join(CCMHr::clicc_key)

      df.clicc.check.top <- unique(df.clicc.check.top)

    } else{

      # Data filter
      df.clicc.check.top <- data |>
        dplyr::select(UniqueClientID, group_vars,
                      dplyr::contains("CLICC_03")) |>
        tidyr::drop_na()|>
        dplyr::mutate(sample.n = dplyr::n_distinct(UniqueClientID),
                      .by = group_vars) |>
        dplyr::rename("Number" = "CLICC_03") |>
        dplyr::summarise(percent = dplyr::n()/sample.n,
                         percent = format(percent, scientific = FALSE),
                         .by = c("Number", group_vars)) |>
        dplyr::group_by(across(all_of(group_vars))) |>
        dplyr::arrange(dplyr::desc(percent), .by_group = TRUE) |>
        dplyr::ungroup() |>
        dplyr::inner_join(CCMHr::clicc_key)

      df.clicc.check.top <- unique(df.clicc.check.top)

    }

    # Save file
    if(save == TRUE){

      write.csv(df.clicc.check.top,
                file = path,
                row.names = FALSE)

    } else{

      return(df.clicc.check.top)

    }

  } else {

  }

}
