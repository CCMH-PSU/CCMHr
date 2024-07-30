#' Summarize CLICC data
#'
#' @description Summarize CLICC data
#' @param data A data file containing clicc.
#' @param clicc_type A vector containing the CLICC type. Options include: "checkall" or "topconcern". By default, `"checkall"`.
#' @param group_vars A vector of grouping variables. By default, `NULL`.
#' @param save A logical statement indicates whether the table should be saved as a file under a local folder. If false, the table will be returned as an object. By default, `FALSE`.
#' @param path A quoted string to indicate the file's pathway and name if `save = TRUE`. By default, `"clicc_summary.csv"`. 
#' 
#' @return Prints demographic tables in concole and/or saves them in under the working directory.
#' @export
#' 
#' 

summarize_clicc <- function(data, 
                            clicc_type = "checkall",
                            group_vars = NULL,
                            save = FALSE, 
                            path = "clicc_summary.csv") {

    # Check if variables are in data
    if(!all(c("UniqueClientID") %in% colnames(data))) {
        stop("The data does not contain UniqueClientID.")
    } else {
       # Skip
    }

    # group_vars
    if(!all(group_vars %in% colnames(data))){
        stop("The data does not contain the grouping variables specified in group_vars.")
    } else {
       # Skip
    }

    # check if clicc_type is "checkall" or "topconcern"
    if(!clicc_type %in% c("checkall", "topconcern")){
        stop("The clicc_type must be either 'checkall' or 'topconcern'.")
    } else {
       # Skip
    }
    
    # Check all
    if("checkall" == clicc_type){
        
        data$CLICC_01_1006[which(data$CLICC_01_06 == 1)] <- 1
        data$CLICC_01_1028[which(data$CLICC_01_28 == 1)] <- 1
        data$CLICC_01_06 <- NULL
        data$CLICC_01_28 <- NULL

        data <- mutate(data, 
                       CLICC_01_01 = case_when(CLICC_01_1101 == 1 ~ 1,
                                               CLICC_01_1102 == 1 ~ 1,
                                               CLICC_01_1103 == 1 ~ 1,
                                               CLICC_01_1104 == 1 ~ 1,
                                               CLICC_01_1105 == 1 ~ 1,
                                               CLICC_01_1106 == 1 ~ 1,
                                               CLICC_01_01 == 1 ~ 1))

        if(is.null(group_vars)){
        
        # Data filter
        df.clicc.check.all <- data %>%
            dplyr::select(UniqueClientID,
                          dplyr::contains("CLICC_01")) %>%
            tidyr::pivot_longer(c(CLICC_01_01:CLICC_01_1106), 
                                names_to = "Item", 
                                values_to = "value") %>%
            tidyr::drop_na() %>%
            dplyr::mutate(sample.n = dplyr::n_distinct(UniqueClientID)) %>%
            dplyr::summarise(percent = sum(value == 1, na.rm = TRUE)/sample.n, 
                             sample.n = dplyr::first(sample.n),
                             .by = c("Item")) %>%
            dplyr::arrange(dplyr::desc(percent)) %>%
            dplyr::inner_join(CCMHr::clicc_key)

           df.clicc.check.all <- unique(df.clicc.check.all)

        } else {
        
        # Data filter
        df.clicc.check.all <- data %>%
            dplyr::select(UniqueClientID, group_vars, 
                          dplyr::contains("CLICC_01")) %>%
            tidyr::pivot_longer(c(CLICC_01_01:CLICC_01_1106), 
                                names_to = "Item", 
                                values_to = "value") %>%
            tidyr::drop_na() %>%
            dplyr::mutate(sample.n = dplyr::n_distinct(UniqueClientID), 
                          .by = group_vars) %>%
            dplyr::summarise(percent = sum(value == 1, na.rm = TRUE)/sample.n, 
                             .by = c("Item", group_vars))  %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
            dplyr::arrange(dplyr::desc(percent), .by_group = TRUE) %>%
            dplyr::ungroup() %>%
            dplyr::inner_join(CCMHr::clicc_key)
            
           df.clicc.check.all <- unique(df.clicc.check.all)

        }

        # Save file 
        if(save == TRUE){
            write.csv(df.clicc.check.all, 
                      file = path, 
                      row.names = FALSE)
        } else {
            return(df.clicc.check.all)
        }

    } else {
       #Skip
    }

    # Top concern
    if("topconcern" == clicc_type){

        data$CLICC_03 <- dplyr::recode(data$CLICC_03, 
                                       `1101` = 1L, 
                                       `1102` = 1L, 
                                       `1103` = 1L, 
                                       `1104` = 1L, 
                                       `1105` = 1L, 
                                       `1106` = 1L)

        data$CLICC_03[which(data$CLICC_03 == 6)] <- 1006
        data$CLICC_03[which(data$CLICC_03 == 28)] <- 1028

        if(is.null(group_vars)){
        
        # Data filter
        df.clicc.check.top <- data %>%
            dplyr::select(UniqueClientID,
                          dplyr::contains("CLICC_03")) %>%
            tidyr::drop_na() %>%
            dplyr::mutate(sample.n = dplyr::n_distinct(UniqueClientID)) %>%
            dplyr::rename("Number" = "CLICC_03") %>%
            dplyr::summarise(percent = dplyr::n()/sample.n,  
                             percent = format(percent, scientific = FALSE),
                             .by = c("Number")) %>%
            dplyr::arrange(dplyr::desc(percent)) %>%
            dplyr::inner_join(CCMHr::clicc_key)

            df.clicc.check.top <- unique(df.clicc.check.top)

        } else {
        
        # Data filter
        df.clicc.check.top <- data %>%
            dplyr::select(UniqueClientID, group_vars, 
                          dplyr::contains("CLICC_03")) %>%
            tidyr::drop_na()%>%
            dplyr::mutate(sample.n = dplyr::n_distinct(UniqueClientID), 
                          .by = group_vars) %>%
            dplyr::rename("Number" = "CLICC_03") %>%
            dplyr::summarise(percent = dplyr::n()/sample.n, 
                             percent = format(percent, scientific = FALSE),
                             .by = c("Number", group_vars)) %>%
            dplyr::group_by(across(all_of(group_vars))) %>%
            dplyr::arrange(dplyr::desc(percent), .by_group = TRUE) %>%
            dplyr::ungroup() %>%
            dplyr::inner_join(CCMHr::clicc_key)
            
            df.clicc.check.top <- unique(df.clicc.check.top)


        }

        # Save file 
        if(save == TRUE){
            write.csv(df.clicc.check.top, 
                      file = path, 
                      row.names = FALSE)
        } else {
            return(df.clicc.check.top)
        }

    } else {
       #Skip
    }

}