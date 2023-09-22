
#' Get results for the change in IPAQ MET-min/week between 6 and 12 months (APA & Co project)
#'
#' @param data 'DB_IPAQ_6_12' dataframe (from APA & Co project analysis pipeline).
#'
#' @return A list with one figure and two tables (one for the shift function, and one for
#'     the difference asymmetry function).
#' @export
#'
analyse_change_IPAQ <- function(data) {
  analyse_change(
    data = data,
    id = "patient",
    x = "MONTH",
    y = "MET_MIN_WK",
    rain_side = "f1x1",
    nudge_y = 6,
    color_fill = "#BFD61F",
    color_stat = "black",
    labs_1x = "Months post-program",
    labs_1y = "MET-min/week",
    labs_2x = "",
    labs_2y = "MET-min/week [Month 12 - Month 0]",
    labs_3x = "MET-min/week at Month 0",
    labs_3y = "MET-min/week at Month 12",
    labs_4x = "Months post-program",
    labs_4y = "MET-min/week",
    labs_5x = "Deciles of MET-min/week at Month 0",
    labs_5y = "Decile Month 12 - Decile Month 0 (MET-min/week)",
    labs_6x = "Quantiles",
    labs_6y = "Quantile sum = q + 1-q"
  )
}
