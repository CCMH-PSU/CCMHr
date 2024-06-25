#' Setup data analysis folder
#'
#' @param name A string to indicate the name of the main folder.
#' @param data.folder A logical statement to add data subfolder to the main folder. Default is `TRUE`.
#' @param data.folder.rawclean A logical statement to add raw and clean subfolder within the data subfolder. Default is `TRUE`.
#' @param visuals.folder A logical statement to add visuals subfolder to the main folder. Default is `TRUE`.
#' @param R.folder A logical statement to add R subfolder to the main subfolder. Default is `TRUE`.
#' @param add.date A logical statement to add the date to the main folder name. Format is Year-Month (numeric). Default is `TRUE`.
#' @param add.folder A string or vector list with the names of additional folders to be added to the main and subfolders. When creating a folder that will contain subfolder(s), make sure the main folder is listed before the subfolder(s). Default is `NULL`.
#' @param add.rfiles A logical statement to add R files based on templates to the R folder. Default is `TRUE`.
#' @param create.rproj A logical statement to create a .Rproj file in the main folder. Default is `TRUE`.
#'
#'
#' @return A analysis folder and R syntax files to conduct data analysis.
#' @export
#'

setup_analysis <- function(name,
                           data.folder = TRUE,
                           data.folder.rawclean = TRUE,
                           visuals.folder = TRUE,
                           R.folder = TRUE,
                           add.date = TRUE,
                           add.folder = NULL,
                           add.rfiles = TRUE,
                           create.rproj = TRUE) {

    # Stop if data.folder is FALSE and data.folder.rawclean is TRUE
      if(data.folder == FALSE & data.folder.rawclean == TRUE) {
        stop("data.folder.rawclean cannot be TRUE if data.folder is FALSE")
      } else{
        # Continue
      }

    # Create the main folder
      # Rework name
        name <- gsub("-", "/", name)
      # Get year
        year <- format(Sys.Date(), "%Y")
      # Get month
        month <- format(Sys.Date(), "%m")
      # Set up folder name
        if(add.date == TRUE) {
          main.folder.name <- paste0(year, "-", month, "_", name)
        } else {
          main.folder.name <- name
        }
      # Create main folder
        dir.create(main.folder.name)

    # Create subfolders
      # data
        if(data.folder == TRUE) {
          dir.create(file.path(main.folder.name, "data"))
        } else {
          # Skip
        }

      # data/rawclean
        if(data.folder.rawclean == TRUE) {
          dir.create(file.path(main.folder.name, "data/raw"))
          dir.create(file.path(main.folder.name, "data/clean"))
        } else {
          # Skip
        }

      # visual
        if(visuals.folder == TRUE) {
          dir.create(file.path(main.folder.name, "visuals"))
        } else {
          # Skip
        }

      # R
        if(R.folder == TRUE) {
          dir.create(file.path(main.folder.name, "R"))
        } else {
          # Skip
        }

      # Additional folders
        if(!is.null(add.folder)) {
          for(i in 1:length(add.folder)) {
            dir.create(file.path(main.folder.name, add.folder[i]))
          }
        } else {
          # Skip
        }

      # Add templates
        if(R.folder == TRUE & add.rfiles == TRUE){

            usethis::use_template(template = "data_processing.R",
                                  save_as = paste0("R/01_process.R"),
                                  package = "CCMHr",
                                  data = list(main.folder.name = main.folder.name),
                                  open = TRUE)

            usethis::use_template(template = "create_visuals.R",
                                  save_as = paste0("R/02_visuals.R"),
                                  package = "CCMHr",
                                  data = list(main.folder.name = main.folder.name),
                                  open = TRUE)

        } else {
           # Skip
        }

    # Create .Rproj file content
      if(create.rproj == TRUE){
        rproj_content <-
            sprintf('Version: 1.0\nProject: %s\n', main.folder.name) %>%
            paste('RStudio.project.context: "Version": "1.0"\n', collapse = "") %>%
            paste('RStudio.cloud.project.name: ""\n', collapse = "") %>%
            paste('RStudio.cloud.project.url: ""\n', collapse = "") %>%
            write(paste0(main.folder.name, "/", main.folder.name, ".Rproj"))

       } else {
         # Skip
       }

}
