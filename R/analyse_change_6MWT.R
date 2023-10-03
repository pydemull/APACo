
#' Get results for the change in 6MWT distance between 0 and 12 months
#'
#' @param data 'DB_6MWT_0_12' data frame (from APA&Co project analytical pipeline).
#'
#' @return A list with one figure and two tables (one for the shift function, and one for
#'     the difference asymmetry function).
#' @export
#'
analyse_change_6MWT <- function(data) {
  analyse_change(
    data = data,
    id = "patient",
    x = "MONTH",
    y = "DIST_M",
    rain_side = "f1x1",
    nudge_y = 6,
    color_fill = "#0089C6",
    color_stat = "black",
    labs_1x = "Months post-program",
    labs_1y = "6MWT distance (m)",
    labs_2x = "",
    labs_2y = "6MWT distance [Month 12 - Month 0] (m)",
    labs_3x = "6MWT distance at Month 0 (m)",
    labs_3y = "6MWT distance at Month 12 (m)",
    labs_4x = "Months post-program",
    labs_4y = "6MWT distance (m)",
    labs_5x = "Deciles of 6MWT distance at Month 0 (m)",
    labs_5y = "Decile Month 12 - Decile Month 0 (m)",
    labs_6x = "Quantiles",
    labs_6y = "Quantile sum = q + 1-q"
  )
}
