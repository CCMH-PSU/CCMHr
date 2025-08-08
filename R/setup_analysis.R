#' Sets up a folder to house a set of analyses.
#'
#' @name setup_analysis
#'
#' @description The function creates folders associated with a specific set of analyses. Primarily, this function is used to create an ad hoc data analysis folder and its corresponding subfolders. To run the function and create a new analysis folder, the programmer must first set their working directory to a specified folder where the analysis folder will be saved. Typically, the working directory is located under the following path: "CCMH - Documents/Data Repository/Ad hoc projects/####", where #### represents the data year. The newly created folder will be saved under this working directory. The main folder name must not contain the symbols "-" or "/" (hyphens or forward slashes). Most subfolder names are standardized (e.g., data, visuals); however, the programmer may add custom subfolders using the add.folder argument.
#'
#' @param name A quoted string to indicate the name of the main folder.
#' @param data.folder A logical argument to add a data subfolder to the main folder. If `TRUE`, the subfolder named "data" is added. By default, `TRUE`.
#' @param data.folder.rawclean A logical argument to add a raw and clean subfolder within the data subfolder. If `TRUE`, the subfolders "clean" and "raw" are added. By default, `TRUE`.
#' @param visuals.folder A logical argument to add a visuals subfolder to the main folder. If `TRUE`, the subfolder named "visuals" is added. By default, `TRUE`.
#' @param R.folder A logical argument to add a R subfolder to the main subfolder. If `TRUE`, the subfolder named "R" is added. By default, `TRUE`.
#' @param add.date A logical argument to add the date to the beginning of the main folder name. Format is Year-Month ("2025_01"). If `TRUE`, the date is added to the beginning of the main folder name. By default, `TRUE`.
#' @param add.folder A quoted string or list of quoted strings with the names of additional folders to be added to the main and subfolders. When creating subfolder(s), make sure the parent folder is listed/created before the subfolder(s). For example, the folder "figures" should be listed before "figures/histograms". By default, `NULL`.
#' @param create.rproj A logical argument to create a .Rproj file in the main folder. If `TRUE`, a R project is added to the folder. By default, `TRUE`.
#'
#' @return The function will create a new analysis folder that contains various subfolders. Most subfolder names are standardized (e.g., data, visuals); however, custom subfolders could be created using the add.folder argument.
#'
#' @export

setup_analysis <- function(name,
                           data.folder = TRUE,
                           data.folder.rawclean = TRUE,
                           visuals.folder = TRUE,
                           R.folder = TRUE,
                           add.date = TRUE,
                           add.folder = NULL,
                           create.rproj = TRUE) {

  # Stop if data.folder is FALSE and data.folder.rawclean is TRUE
  if(data.folder == FALSE &
     data.folder.rawclean == TRUE){

    stop("data.folder.rawclean cannot be TRUE if data.folder is FALSE")

  } else{

  }

  # Create the main folder
    # Rework name
    name <- gsub("-", "/", name)

    # Get year
    year <- format(Sys.Date(), "%Y")

    # Get month
    month <- format(Sys.Date(), "%m")

    # Set up folder name
    if(add.date == TRUE){

      main.folder.name <- paste0(year, "-", month, "_", name)

    } else {

      main.folder.name <- name

    }

    # Create directory
    dir.create(main.folder.name)

  # Create subfolders
    # data
    if(data.folder == TRUE){

      dir.create(file.path(main.folder.name, "data"))

    } else{

    }

    # data/rawclean
    if(data.folder.rawclean == TRUE){

      dir.create(file.path(main.folder.name, "data/raw"))
      dir.create(file.path(main.folder.name, "data/clean"))

    } else{

    }

    # visual
    if(visuals.folder == TRUE){

      dir.create(file.path(main.folder.name, "visuals"))

    } else {

    }

    # R
    if(R.folder == TRUE){

      dir.create(file.path(main.folder.name, "R"))

    } else {

    }

    # Additional folders
    if(!is.null(add.folder)){

      for(i in 1:length(add.folder)){

        dir.create(file.path(main.folder.name, add.folder[i]))

      }

    } else{

    }

  # Create .Rproj file content
  if(create.rproj == TRUE){

    rproj_content <-
      sprintf('Version: 1.0\nProject: %s\n', main.folder.name) |>
      paste('RStudio.project.context: "Version": "1.0"\n', collapse = "") |>
      paste('RStudio.cloud.project.name: ""\n', collapse = "") |>
      paste('RStudio.cloud.project.url: ""\n', collapse = "") |>
      write(paste0(main.folder.name, "/", main.folder.name, ".Rproj"))

    } else {

    }

}
