
#' Get .Rmd template
#'
#' The function copies an .Rmd template file from the APACo package to put it in a new directory
#'     called 'out' created  at the root of the current directory when running the analytical
#'     pipeline.
#'
#' @param dir A character string for the path to the directory.
#' @param name A character value for the name of the template.
#'
#' @return The path to the template.
#' @export
#'
get_rmd_template <- function(dir = "./out", name){

  # Copy APACo package .Rmd file
  file.copy(from = system.file("templates", name, package = "APACo"),
            to = dir)

  # Get path to new template
  path_to_file <- paste0(dir, "/", name)

  # Return path
  return(path_to_file)

}
