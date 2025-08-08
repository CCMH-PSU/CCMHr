#' Check the response validity of the CLICC items.
#'
#' @name check_CLICC
#'
#' @description Check a data frame to ensure all CLICC_01 items have 1 or NA values or CLICC_03 responses corresponding to possible CLICC numbers (See SDS codebook for possible CLICC_03 values). If these constraints are not satisfied, the function returns a message indicating which CLICC_01 variables had invalid responses and/or the CLICC_03 invalid responses. To run this function, CLICC variable names must be correctly named.
#'
#' @param dat A data file containing CLICC items.
#'
#' @return If CLICC_01 and CLICC_03 item responses are valid, the function returns the message, "CLICC looks good!" If CLICC_01 and CLICC_03 item responses are invalid, the function returns a message indicating which CLICC_01 variables had invalid responses and/or the CLICC_03 invalid responses.
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr summarise
#' @importFrom dplyr ends_with
#' @importFrom tidyr drop_na
#'
#' @export

check_CLICC <- function(dat){

  # Function to specify that only valid values are 1 or NA
  one_or_na <- function(x){

    ifelse(is.na(x), TRUE, ifelse(x == 1, TRUE, FALSE))

  }

  # Sum violations for CLICC_01_01 to CLICC_01_1106
  sum_violate_01 <- dat |>
    dplyr::select(CLICC_01_01:CLICC_01_1106) |>
    dplyr::mutate(dplyr::across(CLICC_01_01:CLICC_01_1106, ~ !one_or_na(.), .names = "{.col}_viol")) |>
    dplyr::summarise(dplyr::across(dplyr::ends_with("_viol"), ~ sum(., na.rm = TRUE), .names = "{.col}_sum"))

  # Sum violations for CLICC_03
    # Specify valid responses
    CLICC_03_levels <- c(1, 1101,
                         1102, 1103,
                         1104, 1105,
                         1106, 2,
                         3, 4,
                         5, 1006,
                         46, 7,
                         6, 8,
                         9, 10,
                         11, 12,
                         13, 14,
                         15, 16,
                         17, 18,
                         19, 20,
                         21, 22,
                         23, 24,
                         25, 50,
                         26, 27,
                         1028, 47,
                         48, 28,
                         29, 30,
                         31, 32,
                         33, 34,
                         35, 49,
                         36, 37,
                         38, 39,
                         40, 41,
                         42, 43,
                         44)

    # Summarize violations
    sum_violate_03 <- dat |>
      dplyr::select(CLICC_03) |>
      tidyr::drop_na() |>
      dplyr::mutate(CLICC_03 = ifelse(CLICC_03 %in% CLICC_03_levels, 1, CLICC_03)) |>
      unique()

    # Set up violations
    violate_03 <- length(unique(sum_violate_03$CLICC_03)) - 1

  # Total violations
  total <- sum(sum_violate_01) + violate_03

  # Messaging
  if(total==0){

    message("CLICC looks good!")

  } else{

    if(sum(sum_violate_01) > 0 &
       violate_03 == 0){

      # Extract CLICC_01 variable names with violations
      violations.CLICC.01 <- colnames(dplyr::select(dat, CLICC_01_01:CLICC_01_1106))[sum_violate_01 > 0]

      # Return message
      return(paste0("Invalid response options for CLICC_01: ", list(as.character(violations.CLICC.01)), "."))

    } else if(sum(sum_violate_01) == 0 &
              violate_03 > 0){

      # Extract CLICC_03 response options with violations
      violations.CLICC.03 <- unique(sum_violate_03$CLICC_03)
      violations.CLICC.03 <- violations.CLICC.03[violations.CLICC.03 != 1]

      # Return message
      return(paste0("Invalid response options for CLICC_03: ", list(violations.CLICC.03), "."))

    } else{

      # Extract CLICC_01 variable names with violations
      violations.CLICC.01 <- colnames(dplyr::select(dat, CLICC_01_01:CLICC_01_1106))[sum_violate_01 > 0]
      violations.CLICC.01 <- as.vector(unique(violations.CLICC.01))

      # Extract CLICC_03 response options with violations
      violations.CLICC.03 <- unique(sum_violate_03$CLICC_03)
      violations.CLICC.03 <- violations.CLICC.03[violations.CLICC.03 != 1]

      # Return message
      return(paste0("Invalid response options for CLICC_01: ", list(as.character(violations.CLICC.01)), ". Invalid response options for CLICC_03: ", list(violations.CLICC.03), "."))

    }

  }

}

#' @rdname check_CLICC
#' @export
check_clicc <- check_CLICC
