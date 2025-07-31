#' Create change variables on CCAPS subscale scores between a client's first and last CCAPS administration.
#'
#' @description This function could create three different types of variables. The function will always return variables that end with "\_change" (e.g., Depression34_change). These variables depict the change in CCAPs subscale scores between the first and last administration (i.e., first admin - last admin). Here, a positive score indicates symptom improvement, a zero score indicates no change, and a negative score indicates symptom degradation. When include_first = TRUE, the CCAPs subscale scores from the first administration will be included in the returned data frame (e.g., the variable names will end with "\_first"). When include_last = TRUE, the CCAPs subscale scores from the last administration will be included in the returned data frame (e.g., the variable names will end with "\_last").
#'
#' @param data A data file containing CCAPS subscales scores.
#' @param client_identifier The column that uniquely identifies each client. By default, `UniqueClientID`.
#' @param center_identifier The column that uniquely identifies each center. By default, `CcmhID`.
#' @param add_items Specify the columns outside CCAPS subscales to be included in the returned data frame. Options include `NA` for no additional variables, a single quoted string (e.g., `"Variable1"`) for a single variable, a list of quoted strings (e.g., `c("Variable1", "Variable2")`) for a list of variables, or `"all"` for selecting all CCAPS-34 items. By default, `NA`.
#' @param include_first A logical argument that specifies if scores and/or responses from the first completion of the CCAPS should be included in the returned data frame. If `TRUE`, responses of the first administration is included in the returned data frame. By default, `FALSE`.
#' @param include_last A logical argument that specifies if scores and/or responses from the last completion of the CCAPS should be included in the returned data frame. If `TRUE`, responses of the last administration is included in the returned data frame. By default, `FALSE`.
#'
#' @return A data frame with CCAPS change score of subscales and specified columns. Depending on the include_first and include_last endorsement, scores and/or responses from the first and last CCAPS administration could also be included.
#'
#' @export

CCAPS_change <- function(data,
                         client_identifier = "UniqueClientID",
                         center_identifier = "CcmhID",
                         add_items = NA,
                         include_first = FALSE,
                         include_last = FALSE){

  # Checking for required variables
    # List of variables
    var_names <- c("Is_ValidCCAPS", "Date",
                   "Depression34", "Anxiety34",
                   "Social_Anxiety34", "Academics34",
                   "Eating34", "Hostility34",
                   "Alcohol34", "DI")
print("Work 1")
    # Check for missing variables
    CCMHr::required_items(data, var_names)
    print("Work 2")
  # Convert data into data frame
  data <- as.data.frame(data)
  print("Work 3")
  # Rename ids
  data <- data |>
    dplyr::rename(UniqueClientID2 = {{client_identifier}},
                  CcmhID2 = {{center_identifier}})
  print("Work 4")
  # Convert to data table
  data <- data.table::setDT(data)
  print("Work 5")
  # Excluding data with no CCAPS data
  data <- data[data$Is_ValidCCAPS == 1,]
  print("Work 6")
  # Convert to lazy data table
  data <- dtplyr::lazy_dt(data)
  print("Work 7")
  # Excluding participants that didn't complete the CCAPS at least two times.
  data <- data |>
    dplyr::group_by(UniqueClientID2, CcmhID2) |>
    dplyr::filter(dplyr::n() >= 2) |>
    dplyr::ungroup() |>
    data.table::as.data.table()
  print("Work 8")
  print("Work 9")
  # Excluding all rows outside of first and last CCAPS administrations.
  data <- data |>
    dplyr::group_by(UniqueClientID2, CcmhID2) |>
    dplyr::arrange(Date) |>
    dplyr::slice(1, dplyr::n()) |>
    dplyr::ungroup() |>
    data.table::as.data.table()
  print("Work 9")
  # Variable that will detect if "all" is being specified in the add_items argument
  add_items_all <- add_items == "all"[1]
  print("Work 10")
  # Creating an empty data frame
  CCAPS_data <- NULL
  print("Work 11")
  # Adding additional CCAPS items based on add_items
  if(is.na(add_items)[1] == TRUE){ #If no items are added to add_items or NA
    print("Work 12")
    # Returned data frame with specified columns
    cols <- c("UniqueClientID2", "CcmhID2",
              "Depression34", "Anxiety34",
              "Social_Anxiety34", "Academics34",
              "Eating34", "Hostility34",
              "Alcohol34", "DI")
    print("Work 13")
    CCAPS_data <- data[, .SD, .SDcols = cols]
    print("Work 14")
  } else if(add_items_all[1] == TRUE){ # If all CCAPS items are to be added or "all" is specified in add_items
    print("Work 15")
    # Check to see if variables are named correctly to use this argument
      # List of variables required to run function
      var_names <- c("CCAPS_03", "CCAPS_05",
                     "CCAPS_06", "CCAPS_11",
                     "CCAPS_13", "CCAPS_16",
                     "CCAPS_17", "CCAPS_18",
                     "CCAPS_21", "CCAPS_22",
                     "CCAPS_24", "CCAPS_27",
                     "CCAPS_29", "CCAPS_30",
                     "CCAPS_31", "CCAPS_33",
                     "CCAPS_34", "CCAPS_36",
                     "CCAPS_39", "CCAPS_40",
                     "CCAPS_45", "CCAPS_46",
                     "CCAPS_48", "CCAPS_49",
                     "CCAPS_51", "CCAPS_52",
                     "CCAPS_54", "CCAPS_57",
                     "CCAPS_58", "CCAPS_59",
                     "CCAPS_63", "CCAPS_64",
                     "CCAPS_66", "CCAPS_68")
      print("Work 16")
      # For loops to detect for missing variables
        # Parameters
          # Number of loops
          loop.num <- dim(data.frame(var_names))[1]
          print("Work 16")
          # Data frame for missing variables
          df.detect.miss <- NULL
          df.detect.miss$var_names <- var_names
          df.detect.miss <- data.frame(df.detect.miss)
          df.detect.miss$missing <- NA
          print("Work 17")
          # Loops
          for(i in 1:loop.num){
            print("Work 18")
            df.detect.miss$missing[i] <- data.frame(var_names[[i]]) %in% names(data)

          }
          print("Work 19")
      # Error Message
        # Removing present variables
        df.detect.miss <- dplyr::filter(df.detect.miss, df.detect.miss$missing == FALSE)
        print("Work 20")
        # Listing out missing variables
        missing.vars <- toString(df.detect.miss$var_names)
        print("Work 21")
        # Release error message
        if(nrow(df.detect.miss) > 0){

          stop(paste0("The following variables are not present or named adequately in the data: ", missing.vars,". To use 'all' in the argument 'add_items', variable names listed in this error message need to be present in the data."))

        } else{

        }
        print("Work 22")
    # Returned data frame
      # Specify variables
      var_names_all <- c("UniqueClientID2", "CcmhID2",
                         "Depression34", "Anxiety34",
                         "Social_Anxiety34", "Academics34",
                         "Eating34", "Hostility34",
                         "Alcohol34", "DI",
                         "CCAPS_03", "CCAPS_05",
                         "CCAPS_06", "CCAPS_11",
                         "CCAPS_13", "CCAPS_16",
                         "CCAPS_17", "CCAPS_18",
                         "CCAPS_21", "CCAPS_22",
                         "CCAPS_24", "CCAPS_27",
                         "CCAPS_29", "CCAPS_30",
                         "CCAPS_31", "CCAPS_33",
                         "CCAPS_34", "CCAPS_36",
                         "CCAPS_39", "CCAPS_40",
                         "CCAPS_45", "CCAPS_46",
                         "CCAPS_48", "CCAPS_49",
                         "CCAPS_51", "CCAPS_52",
                         "CCAPS_54", "CCAPS_57",
                         "CCAPS_58", "CCAPS_59",
                         "CCAPS_63", "CCAPS_64",
                         "CCAPS_66", "CCAPS_68")
      print("Work 23")
      # Creating data set
      CCAPS_data <- data[, .SD, .SDcols = var_names_all]
      print("Work 24")
  } else{ # If specific items are added to add_items by listing out variable names
    print("Work 25")
    # Set as data frame
    data <- as.data.frame(data)
    print("Work 26")
    # Specify R loop parameters
      # Add items is a data frame
      df_add_items <- data.frame(add_items)
      print("Work 27")
      # Extract the loop Number
      loop.num <- dim(df_add_items)[1]
      print("Work 28")
      # Creating a blank df to insert data added by loops and making sure data.tmp is a data frame
      data.tmp <- matrix(nrow = dim(data)[1], ncol = dim(df_add_items)[1])
      data.tmp <- data.frame(data.tmp)
      print("Work 29")
    # For loop that will insert each variable specified in add_items into data.tmp
    for(i in 1:loop.num){
      print("Work 30")
      data.tmp[,i] <- data[,df_add_items[i,1]]

    }
      print("Work 31")
    # Rename columns of data.tmp to match add_items
    colnames(data.tmp) <- add_items
    print("Work 32")
    # Adding client_identifier and center_identifier to data.tmp
    p1 <- dim(data.tmp)[2]+1
    p2 <- dim(data.tmp)[2]+2
    print("Work 33")
    data.tmp[,p1:p2] <- data |>
      dplyr::select(UniqueClientID2, CcmhID2)
    print("Work 33")
    # Selecting CCAPS variables
    CCAPS_data <- data |>
      dplyr::select(UniqueClientID2, CcmhID2,
                    Depression34:DI)
    print("Work 34")
    CCAPS_data <- as.data.frame(CCAPS_data)
    print("Work 35")
    # Merging data frames
    CCAPS_data <-  merge(CCAPS_data, data.tmp)
  }
  print("Work 36")
  # Adding first and last completion of CCAPS scores
    # Specify the last variable in CCAPS_data
    last.var <- rev(names(CCAPS_data))[1]
    print("Work 37")
    # CCAPS_data is a data table
    CCAPS_data <- data.table::setDT(CCAPS_data)
    print("Work 38")
    # Specify the different outcomes depending on arguments include_first and include_last
    if(include_first == FALSE &
       include_last == FALSE){
      print("Work 39")
      # Creating overall data frame
      CCAPS_data2 <- dtplyr::lazy_dt(CCAPS_data)
      print("Work 40")
      data <- CCAPS_data2 |>
        dplyr::group_by(UniqueClientID2, CcmhID2) |>
        dplyr::select(names(CCAPS_data)) |>
        dplyr::summarize(dplyr::across(Depression34:dplyr::all_of(last.var),
                                       ~dplyr::first(.x)-dplyr::last(.x),
                                       .names = "{col}_change"),
                                       .groups = "keep") |>
        data.table::as.data.table()
      print("Work 41")
    } else if(include_first == TRUE &
              include_last == FALSE){
      print("Work 42")
      # Creating an overall data frame
      CCAPS_data2 <- dtplyr::lazy_dt(CCAPS_data)
      print("Work 43")
      data <- CCAPS_data2 |>
        dplyr::group_by(UniqueClientID2, CcmhID2) |>
        dplyr::select(names(CCAPS_data)) |>
        dplyr::summarize(dplyr::across(Depression34:dplyr::all_of(last.var), dplyr::first, .names = "{.col}_first"),
                         dplyr::across(Depression34:dplyr::all_of(last.var), ~dplyr::first(.x)-dplyr::last(.x), .names = "{col}_change"),
                         .groups = "keep") |>
        data.table::as.data.table()
      print("Work 44")
    } else if(include_first == FALSE &
              include_last == TRUE){
      print("Work 45")
      # Creating overall data frame
      CCAPS_data2 <- dtplyr::lazy_dt(CCAPS_data)
      print("Work 46")
      data <- CCAPS_data2 |>
        dplyr::group_by(UniqueClientID2, CcmhID2) |>
        dplyr::select(names(CCAPS_data)) |>
        dplyr::summarize(dplyr::across(Depression34:dplyr::all_of(last.var), dplyr::last, .names = "{.col}_last"),
                         dplyr::across(Depression34:dplyr::all_of(last.var), ~dplyr::first(.x)-dplyr::last(.x), .names = "{col}_change"),
                         .groups = "keep") |>
        data.table::as.data.table()
      print("Work 47")
    } else {
      print("Work 48")
      # Creating overall data frame
      CCAPS_data2 <- dtplyr::lazy_dt(CCAPS_data)
      print("Work 49")
      data <- CCAPS_data2 |>
        dplyr::group_by(UniqueClientID2, CcmhID2) |>
        dplyr::select(names(CCAPS_data)) |>
        dplyr::summarize(dplyr::across(Depression34:dplyr::all_of(last.var), dplyr::first, .names = "{.col}_first"),
                         dplyr::across(Depression34:dplyr::all_of(last.var), dplyr::last, .names = "{.col}_last"),
                         dplyr::across(Depression34:dplyr::all_of(last.var), ~dplyr::first(.x)-dplyr::last(.x), .names = "{col}_change"),
                         .groups = "keep") |>
        data.table::as.data.table()
    }
    print("Work 50")
  # Convert to data frame
  data <- data.table::setDF(data)
  data <- as.data.frame(data)
  data <- data.table::setDT(data)
  data <- dtplyr::lazy_dt(data)
  print("Work 51")
  # Rename ids
  data <- data |>
    dplyr::rename(!!client_identifier := UniqueClientID2,
                  !!center_identifier := CcmhID2) |>
    data.table::as.data.table()
  print("Work 52")
  # Returns as data frame
  data <- as.data.frame(data)
  print("Work 53")
  return(data)
  print("Work 54")
}
