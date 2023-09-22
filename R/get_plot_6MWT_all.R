
#' Get plot for all 6MWT distance data
#'
#' @param data 'DB_6MWT' dataframe (from APA & Co project analytical pipeline).
#'
#' @return A ggplot object.
#' @export
#'
get_plot_6MWT_all <- function(data) {

  ggplot2::theme_set(theme_bw())

  view_rainclouds(
    data = data,
    id = "patient",
    x = "MONTH",
    y = "DIST_M",
    color_fill = "#0089C6",
    color_stat = "black",
    labs_x = "Months post-program",
    labs_y = "6-min walking test distance (m)"
  )
}
