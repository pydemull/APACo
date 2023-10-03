
#' Get results for the change in EMAPS scores between 0 and 12 months
#'
#' @param data 'DB_EMAPS_0_12' data frame (from APA&Co project analytical pipeline).
#'
#' @return A list of results lists relating to each EMAPS score. Each list includes one figure
#'    and two tables (one for the shift function, and one for the difference asymmetry function).
#' @export
#'
analyse_change_EMAPS <- function(data) {
  purrr::map(names(data)[3:8], function(x) {
    if (x == "INTRINSIC") {
      x_lab = "Intrinsic motivation"
      color_fill <- "chartreuse4"
    }

    if (x == "INTEGRATED")
    {
      x_lab = "Integrated regulation"
      color_fill <- "chartreuse4"
    }

    if (x == "IDENTIFIED")
    {
      x_lab = "Identified regulation"
      color_fill <- "chartreuse4"
    }
    if (x == "INTROJECTED")
    {
      x_lab = "Introjected regulation"
      color_fill <- "grey50"
    }

    if (x == "EXTERNAL")
    {
      x_lab = "External regulation"
      color_fill <- "#FAC090"
    }

    if (x == "AMOTIVATION")
    {
      x_lab = "Amotivation"
      color_fill <- "#E46C0A"
    }

    list_emaps <-
      analyse_change(
        data = data |> dplyr::select(patient, MONTH, x),
        id = "patient",
        x = "MONTH",
        y = x,
        rain_side = "f1x1",
        nudge_y = 0.1,
        color_fill = color_fill,
        color_stat = "black",
        labs_1x = "Months post-program",
        labs_1y = paste0(x_lab, " score"),
        labs_2x = "",
        labs_2y = paste0(x_lab, " score [Month 12 - Month 0]"),
        labs_3x = paste0(x_lab, " score at Month 0"),
        labs_3y = paste0(x_lab, " score at Month 12"),
        labs_4x = "Months post-program",
        labs_4y = paste0(x_lab, " score"),
        labs_5x = paste0("Deciles of ", x_lab, " score at Month 0"),
        labs_5y = "Decile Month 12 - Decile Month 0",
        labs_6x = "Quantiles",
        labs_6y = "Quantile sum = q + 1-q"
      )

    return(list_emaps)

  })
}
