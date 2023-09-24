
#' Read dataset from a package when using a {target} analytical pipeline
#'
#'This function is from the book by Bruno Rodrigues entitled 'Building reproducible
#'     analytical pipelines with R' (pp 410-411).
#'
#' @param data_name A character value naming the dataset to be loaded.
#' @param package_name A character value naming the package from which data should be loaded.
#'
#' @return A dataset.
#' @export
#' @importFrom utils data
#'
read_data <- function(data_name, package_name) {
  temp <- new.env(parent = emptyenv())

  data(list = data_name,
       package = package_name,
       envir = temp)

  get(data_name, envir = temp)
}
