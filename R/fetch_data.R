#' Fetches a data set from a CCMH - Documents repository
#'
#' @name fetch_data
#'
#' @description Fetches a data set from a CCMH - Documents repository. Each single year data frame can be fetched, along with the complete data frame. To run this function, the programmer must set the working directory within the CCMH - Documents folder. Typically, only CCMH staff have access to the CCMH - Documents folder.
#'
#' @param data.year An unquoted integer, unquoted number, or "all". The unquoted integer or number will fetch data from a single data year (i.e., data from 2010 to the most recent data year). If "all", the complete data set with all data years is fetched from a parquet file. 
#'
#' @return If data.year equals an unquoted integer or number, a data frame containing a single data year will be returned. If data.year equals "all", an arrow table containing data from all data years is returned.
#' 
#' @importFrom lubridate year
#' @importFrom arrow open_dataset
#' @importFrom CCMHr loadRDa
#'
#' @export

fetch_data <- function(data.year){

  # Error message for valid data.year inputs
    # Set up 
    vector.valid.data.year <- c(is.numeric(data.year), 
                                is.integer(data.year), 
                                data.year == "all")
  
  if(any(vector.valid.data.year) != TRUE) {
    
    # Error message 
    stop("The data.year argument must be an unquoted number, unquoted integer, or equal \"all\".")
  
  } else{

  }
  
  # Error message for data.year range
  if(data.year != "all"){

    # Extract current year
    current.year <- lubridate::year(Sys.Date())
    previous.year <- current.year - 1

    if(!data.year %in% c(2010:previous.year)){

      # Error message 
      stop(paste0("The data.year argument must be between 2010 and ", current.year - 1, "."))
    
    } else{
 
    }

  } else{

  }

  # Extract current working directory
  working.dir <- getwd()

  # Error message for determine if working directory is accepted 
  if(!grepl("CCMH - Documents", working.dir)){

    stop("Your working directory must be within the CCMH - Documents folder. This function can only be used by individuals (typically CCMH staff) who have access to the CCMH - Documents folder.")

  } else{

  }

  # Root directory 
  root.working.dir <- sub("^(.*?CCMH - Documents/).*", "\\1", working.dir)

  # Fetch data 
  if(data.year == "all"){

    # Directory for all data years
    all.working.dir <- paste0(root.working.dir, "Data Repository/Data/Multiple years/Complete Research Dataset")
    
    # Extract files within folder
    all.files <- list.files(all.working.dir, 
                            full.names = TRUE)

    # Extract parquet files
    parquet_files <- list.files(path = all.working.dir, 
                                pattern = "\\.parquet$", 
                                full.names = FALSE, 
                                ignore.case = TRUE)
    
    # Determine the number of parquet files
    num_parquet_files <- length(parquet_files)

    # Error message for multiple parquet files
    if(num_parquet_files != 1){

      # Error message
      stop(paste0("There must be one parquet file in: ", all.working.dir, ". There are ", num_parquet_files, " rda files."))

    } else{

    }

    # File path 
    file.path <- paste0(all.working.dir, "/", parquet_files)

    # Load file 
    df <- arrow::open_dataset(file.path)

  } else{

    # Directory for a single year 
    single.working.dir <- paste0(root.working.dir, "Data Repository/Data/Single years/", data.year, "-", data.year+1, "/TI")
    
    # Detect if folder exist
    single.working.dir.exist <- dir.exists(single.working.dir)

    # Error message if folder does not exist
    if(single.working.dir.exist != TRUE){

      stop("The ", single.working.dir, " does not exist.")

    } else{

    }

    # Extract files within folder
    single.files <- list.files(single.working.dir, 
                               full.names = TRUE)
    
    # Extract rda files
    rda_files <- list.files(path = single.working.dir, 
                            pattern = "\\.rda$", 
                            full.names = FALSE, 
                            ignore.case = TRUE)

    # Determine the number of parquet files
    num_rda_files <- length(rda_files)

    # Error message for multiple parquet files
    if(num_rda_files != 1){

      # Error message
      stop(paste0("There must be one rda file in: ", single.working.dir, ". There are ", num_rda_files, " rda files."))

    } else{

    }

    # File path 
    file.path <- paste0(single.working.dir, "/", rda_files)

    # Load file 
    df <- CCMHr::loadRDa(file.path)

  }

  # Return data
  return(df)

}
