#' Select each client's first administration and merge specified CCMH forms
#'
#' @param data An unquoted string indicating the data frame object name.
#' @param inner_join_list A list of data forms to be inner joined in the final merged data set. Options include: "CCAPS", "SDS", "CLICC", "Closure".
#' @param left_join_list A list of data forms to be left joined in the final merged data set. Options include: "CCAPS", "SDS", "CLICC", "Closure". Data forms listed under inner_join_list must not be listed under left_join_list.
#' @param order_by A quoted string to indicate the variable used to order by when selecting the first administration across data forms.
#' @param keep_columns_CCAPS A string list of column CCAPS names to retain. If not specified, defaults to the relevent columns for CCAPS.
#' @param keep_columns_SDS A string list of column SDS names to retain. If not specified, defaults to the relevent columns for SDS.
#' @param keep_columns_CLICC A string list of column CLICC names to retain. If not specified, defaults to the relevent columns for CLICC.
#' @param keep_columns_Closure A string list of column closure names to retain. If not specified, defaults to the relevent columns for Closure.
#' @param by_year A logical statement to indicate if first administration by year instead of overall. If `TRUE`, the first administration of each client will be selected by year. If `FALSE`, the first administration of each client will be selected regardless of year. Default = `FALSE`.
#' @param recode_NA_CLICC A logical statement to indicate if NA on CLICC should be recoded to 0. If `TRUE`, NA will be recoded to 0. Default = `FALSE`.
#' @param recode_NA_Closure A logical statement to indicate if NA on Case Closure should be recoded to 0. If `TRUE`, NA will be recoded to 0. Default = `FALSE`.
#'
#' @export
#'

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
                         recode_NA_Closure = FALSE) {

    # Warning if inner_join_list and left_join_list are not mutually exclusive
    if(length(intersect(inner_join_list, left_join_list)) > 0) {
        stop("inner_join_list and left_join_list must be mutually exclusive.")
    } else {
       
    }

    # Warning if both inner_join_list and left_join_list are NULL
    if(is.null(inner_join_list) & is.null(left_join_list)) {
        stop("inner_join_list and left_join_list cannot both be NULL. Specify at least one data form in inner_join_list or left_join_list.")
    } else {
        
    }

    # Warning if inner_join_list or left_join_list are not CCAPS, SDS, CLICC, or Closure
    if(!all(inner_join_list %in% c("CCAPS", "SDS", "CLICC", "Closure")) | 
       !all(left_join_list %in% c("CCAPS", "SDS", "CLICC", "Closure"))) {
        stop("inner_join_list and left_join_list must be a list of data forms to be merged. Options include: 'CCAPS', 'SDS', 'CLICC', 'Closure'.")
    } else {
        
    }

    # Select first administration across data from 
        # CCAPS
        if("CCAPS" %in% inner_join_list | "CCAPS" %in% left_join_list) {
            df.ccaps <- CCMHr::select_first_CCAPS(data = data,
                                                  order_by = order_by,
                                                  keep_all = FALSE,
                                                  keep_columns = keep_columns_CCAPS,
                                                  by_year = by_year)

        } else {
         
        }

        # SDS
        if("SDS" %in% inner_join_list | "SDS" %in% left_join_list) {
            df.sds <- CCMHr::select_first_SDS(data = data,
                                              order_by = order_by,
                                              keep_all = FALSE,
                                              keep_columns = keep_columns_SDS,
                                              by_year = by_year)

        } else {
         
        }

        # CLICC
        if("CLICC" %in% inner_join_list | "CLICC" %in% left_join_list) {
            df.clicc <- CCMHr::select_first_CLICC(data = data,
                                                  order_by = order_by,
                                                  keep_all = FALSE,
                                                  keep_columns = keep_columns_CLICC,
                                                  recode_NA = recode_NA_CLICC,
                                                  by_year = by_year)

        } else {
          
        }

        # Closure
        if("Closure" %in% inner_join_list | "Closure" %in% left_join_list) {
            df.closure <- CCMHr::select_first_Closure(data = data,
                                                      order_by = order_by,
                                                      keep_all = FALSE,
                                                      keep_columns = keep_columns_Closure,
                                                      recode_NA = recode_NA_Closure,
                                                      by_year = by_year)

        } else {
         
        }

    # Merge data forms
        # Setup data frame
        df.merge <- data.frame()

        # Specify inner joins
            # CCAPS
            if("CCAPS" %in% inner_join_list) {
                df.merge <- df.ccaps
            } else {
                
            }

            # SDS
            if("SDS" %in% inner_join_list) {
                if(nrow(df.merge) == 0) {
                    df.merge <- df.sds
                } else {
                    df.merge <- dplyr::inner_join(df.merge, df.sds)
                }
            } else {
                
            }

            # CLICC
            if("CLICC" %in% inner_join_list) {
                if(nrow(df.merge) == 0) {
                    df.merge <- df.clicc
                } else {
                    df.merge <- dplyr::inner_join(df.merge, df.clicc)
                }
            } else {
                
            }

            # Closure
            if("Closure" %in% inner_join_list) {
                if(nrow(df.merge) == 0) {
                    df.merge <- df.closure
                } else {
                    df.merge <- dplyr::inner_join(df.merge, df.closure)
                }
            } else {
                
            }

        # Specify left joins
            # CCAPS
            if("CCAPS" %in% left_join_list) {
                if(nrow(df.merge) == 0) {
                    df.merge <- df.ccaps
                } else {
                    df.merge <- dplyr::left_join(df.merge, df.ccaps)
                }
            } else {
                
            }

            # SDS
            if("SDS" %in% left_join_list) {
                if(nrow(df.merge) == 0) {
                    df.merge <- df.sds
                } else {
                    df.merge <- dplyr::left_join(df.merge, df.sds)
                }
            } else {
                
            }

            # CLICC
            if("CLICC" %in% left_join_list) {
                if(nrow(df.merge) == 0) {
                    df.merge <- df.clicc
                } else {
                    df.merge <- dplyr::left_join(df.merge, df.clicc)
                }
            } else {
                
            }

            # Closure
            if("Closure" %in% left_join_list) {
                if(nrow(df.merge) == 0) {
                    df.merge <- df.closure
                } else {
                    df.merge <- dplyr::left_join(df.merge, df.closure)
                }
            } else {
                
            }

    # Return merged data frame
    return(df.merge)

}
