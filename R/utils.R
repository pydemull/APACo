#' Define a custom + operator allowing to handle NAs
#'
#' @param x A numeric value.
#' @param y A numeric value.
#'
#' @return A numeric value.
#' @export
#'
`%+%` <- function(x, y){
  mapply(sum, x, y, MoreArgs = list(na.rm = TRUE))
}
