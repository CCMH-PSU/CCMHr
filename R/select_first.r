#' Extract the client's first administration and merge multiple CCMH forms.
#'
#' @name select_first
#'
#' @description Many analyses regard a client's first administration on specified forms. For example, analyses may require extracting data from a client's first CCAPS and SDS administration. This function encompasses four processes within a single framework. First, the function allows extracting the first administration on the following forms: CCAPS, SDS, CLICC, and Closure. The extracted forms are determined by the forms listed in the inner_join_list and left_join_list. Second, the function allows the programmer to choose how the forms are merged. Forms listed under inner_join_list are forms required (e.g., if inner_join_list = `c("CCAPS", "SDS")`, the client must have both a CCAPS and SDS to be included in the output data frame). Forms listed under left_join_list are forms included but not required (e.g., if `left_join_list = "CLICC"`, data on the first administration of the CLICC will be merged, but clients do not need the data on the CLICC to be included in the output data frame). Third, the function allows the programmer to pick specific items within different forms (e.g., the programmer could only extract SDS_01 instead of every SDS item). Lastly, the function allows the recording of NA for the CLICC and Case Closure forms to streamline analyses.
#'
#' @param data A data frame that contains CCMH forms.
#' @param inner_join_list A quoted string or list of quoted strings to indicate which data forms should be inner joined in the final merged data frame. Options include: "CCAPS", "SDS", "CLICC", "Closure", and "NULL". By default, `NULL`.
#' @param left_join_list A quoted string or list of quoted strings to indicate which data forms should be left joined in the final merged data frame. Options include: "CCAPS", "SDS", "CLICC", "Closure", and "NULL". By default, `NULL`. Data forms listed under inner_join_list can not be listed under left_join_list.
#' @param order_by A quoted string to indicate the variable used to determine the order of rows when selecting the first administration (row) across data forms. By default, "Date".
#' @param keep_columns_CCAPS A quoted string or list of quoted strings of column CCAPS names to retain in the output data frame. By default, `c("UniqueClientID", "CcmhID", "Data_year", "Depression34", "Anxiety34", "Social_Anxiety34", "Academics34", "Eating34", "Hostility34", "Alcohol34", "DI", "Depression62", "Eating62", "Substance62", "Anxiety62", "Hostility62", "Social_Anxiety62", "Family62", "Academics62")`.
#' @param keep_columns_SDS A quoted string or list of quoted strings of SDS column names to retain in the output data frame. By default, `"SDS"` or all SDS columns.
#' @param keep_columns_CLICC A quoted string or list of quoted strings of CLICC column names to retain in the output data frame. By default, `"CLICC"` or all CLICC columns.
#' @param keep_columns_Closure A quoted string or list of quoted strings of Case Closure column names to retain in the output data frame. By default, `"Closure"` or all Case Closure columns.
#' @param by_year A logical argument to indicate if the first administration is by year instead of overall. If `TRUE,` the first form administration for each client will be extracted across each data year (e.g., multiple rows may correspond to a unique client across multiple years). If `FALSE`, the first administration of each client will be selected regardless of year (e.g., each row should correspond to a single unique client). By default, `FALSE`.
#' @param recode_NA_CLICC A logical argument to indicate if NA on CLICC should be recoded as 0. If `TRUE`, NA will be recoded as 0. By default, `FALSE`.
#' @param recode_NA_Closure A logical argument to indicate if NA on CLICC should be recoded as 0. If `TRUE`, NA will be recoded as 0. By default, `FALSE`.
#'
#' @return A data frame with the first administration of multiple forms (based on inner_join_list and left_join_list arguments) and items within forms (based on keep_columns_CCAPS, keep_columns_SDS, keep_columns_CLICC, and keep_columns_Closure arguments).
#'
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#'
#' @export

select_first <- function(data,
                         inner_join_list = NULL,
                         left_join_list = NULL,
                         order_by = "Date",
                         keep_columns_CCAPS = c("UniqueClientID", "CcmhID",
                                                "Data_year", "Depression34",
                                                "Anxiety34", "Social_Anxiety34",
                                                "Academics34", "Eating34",
                                                "Hostility34", "Alcohol34",
                                                "DI", "Depression62",
                                                "Eating62", "Substance62",
                                                "Anxiety62", "Hostility62",
                                                "Social_Anxiety62", "Family62",
                                                "Academics62"),
                         keep_columns_SDS = "SDS",
                         keep_columns_CLICC = "CLICC",
                         keep_columns_Closure = "Closure",
                         by_year = FALSE,
                         recode_NA_CLICC = FALSE,
                         recode_NA_Closure = FALSE){

  # Error message if inner_join_list and left_join_list are not mutually exclusive
  if(length(intersect(inner_join_list, left_join_list)) > 0){

    stop("inner_join_list and left_join_list must be mutually exclusive.")

  } else{

  }

  # Error message if both inner_join_list and left_join_list are NULL
  if(is.null(inner_join_list) & is.null(left_join_list)) {

    stop("inner_join_list and left_join_list cannot both be NULL. Specify at least one data form in inner_join_list or left_join_list.")

  } else{

  }

  # Error message if inner_join_list or left_join_list are not CCAPS, SDS, CLICC, or Closure
  if(!all(inner_join_list %in% c("CCAPS", "SDS",
                                 "CLICC", "Closure")) |
     !all(left_join_list %in% c("CCAPS", "SDS",
                                "CLICC", "Closure"))){

    stop("inner_join_list and left_join_list must be a list of data forms to be merged. Options include: 'CCAPS', 'SDS', 'CLICC', 'Closure'.")

  } else{

  }

  # Select first administration across data from
    # CCAPS
    if("CCAPS" %in% inner_join_list |
       "CCAPS" %in% left_join_list){

      df.ccaps <- CCMHr::select_first_CCAPS(data = data,
                                            order_by = order_by,
                                            keep_all = FALSE,
                                            keep_columns = keep_columns_CCAPS,
                                            by_year = by_year)

    } else{

    }

    # SDS
    if("SDS" %in% inner_join_list |
       "SDS" %in% left_join_list){

      df.sds <- CCMHr::select_first_SDS(data = data,
                                        order_by = order_by,
                                        keep_all = FALSE,
                                        keep_columns = keep_columns_SDS,
                                        by_year = by_year)

    } else{

    }

    # CLICC
    if("CLICC" %in% inner_join_list |
       "CLICC" %in% left_join_list){

      df.clicc <- CCMHr::select_first_CLICC(data = data,
                                            order_by = order_by,
                                            keep_all = FALSE,
                                            keep_columns = keep_columns_CLICC,
                                            recode_NA = recode_NA_CLICC,
                                            by_year = by_year)

    } else{

    }

    # Closure
    if("Closure" %in% inner_join_list |
       "Closure" %in% left_join_list){

      df.closure <- CCMHr::select_first_Closure(data = data,
                                                order_by = order_by,
                                                keep_all = FALSE,
                                                keep_columns = keep_columns_Closure,
                                                recode_NA = recode_NA_Closure,
                                                by_year = by_year)

    } else{

    }

  # Merge data forms
    # Setup data frame
    df.merge <- data.frame()

    # Specify inner joins
      # CCAPS
      if("CCAPS" %in% inner_join_list){

        df.merge <- df.ccaps

      } else{

      }

      # SDS
      if("SDS" %in% inner_join_list){

        if(nrow(df.merge) == 0){

          df.merge <- df.sds

        } else{

          df.merge <- dplyr::inner_join(df.merge, df.sds)

        }

      } else {

      }

      # CLICC
      if("CLICC" %in% inner_join_list){

        if(nrow(df.merge) == 0){

          df.merge <- df.clicc

        } else{

          df.merge <- dplyr::inner_join(df.merge, df.clicc)

        }

      } else{

      }

      # Closure
      if("Closure" %in% inner_join_list){

        if(nrow(df.merge) == 0){

          df.merge <- df.closure

        } else{

          df.merge <- dplyr::inner_join(df.merge, df.closure)

        }

      } else{

      }

    # Specify left joins
      # CCAPS
      if("CCAPS" %in% left_join_list){

        if(nrow(df.merge) == 0){

          df.merge <- df.ccaps

        } else{

          df.merge <- dplyr::left_join(df.merge, df.ccaps)

        }

      } else{

      }

      # SDS
      if("SDS" %in% left_join_list){

        if(nrow(df.merge) == 0){

          df.merge <- df.sds

        } else{

          df.merge <- dplyr::left_join(df.merge, df.sds)

        }

      } else{

      }

      # CLICC
      if("CLICC" %in% left_join_list){

        if(nrow(df.merge) == 0){

          df.merge <- df.clicc

        } else{

          df.merge <- dplyr::left_join(df.merge, df.clicc)

        }

      } else{

      }

      # Closure
      if("Closure" %in% left_join_list){

        if(nrow(df.merge) == 0){

          df.merge <- df.closure

        } else{

          df.merge <- dplyr::left_join(df.merge, df.closure)

        }

      } else {

      }

  # Return merged data frame
  return(df.merge)

}
