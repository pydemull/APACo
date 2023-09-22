
#' Get plot for all IPAQ MET-min/week data
#'
#' @param data 'IPAQ' dataframe (from APA & Co project analytical pipeline).
#'
#' @return A ggplot object.
#' @export
#'
get_plot_IPAQ_all <- function(data) {

  ggplot2::theme_set(theme_bw())

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
