
#' Get plot for all IPAQ MET-min/week data (APA & Co project)
#'
#' @param data 'IPAQ' dataframe (from APA & Co project analysis pipeline).
#'
#' @return A ggplot object.
#' @export
#'
get_plot_IPAQ_all <- function(data) {
  view_rainclouds(
    data = data,
    id = "patient",
    x = "MONTH",
    y = "MET_MIN_WK",
    color_fill = "#BFD61F",
    color_stat = "black",
    labs_x = "Months post-program",
    labs_y = "MET-min/week"
  )
}
