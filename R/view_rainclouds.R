#' Make a polished raincloud plot for visualising the change
#'
#' @param data A dataframe.
#' @param id A character value to indicate the name of the variable designating the
#'     identities of the observations.
#' @param x A character value to indicate the name of a factor variable (2 levels) to be plotted on the X axis.
#' @param y A character value to indicate the name of a numeric variable to be plotted on the Y axis.
#' @param rain_side A character value to indicate how you want the rainclouds displayed,
#'     right ("r"), left ("l") or flanking ("f"), for a 1-by-1 flanking raincloud use ("f1x1")
#'     and for a 2-by-2 use ("f2x2").
#' @param color_fill A character value to set the filling color.
#' @param color_stat A character value to set the color of the statistical summaries.
#' @param labs_x A character string to name the X axis.
#' @param labs_y A character string to name the Y axis.
#'
#' @return A ggplot object.
#' @export
#' @import ggplot2
#'

view_rainclouds <- function(data,
                            id,
                            x,
                            y,
                            rain_side = NULL,
                            color_fill = NULL,
                            color_stat = NULL,
                            labs_x = NULL,
                            labs_y = NULL) {
  # Set the theme for the graphics
  theme_set(theme_bw())

  # Set colors
  if (is.null(color_fill))
    color_fill <- "grey50"
  if (is.null(color_stat))
    color_stat <- "black"

  # Set labels
  if (is.null(labs_x))
    labs_x <- x
  if (is.null(labs_y))
    labs_y <- y


  # Make plot
  p <-
    ggplot(data = data, aes(x = .data[[x]], y = .data[[y]])) +
    ggrain::geom_rain(
      rain.side = rain_side,
      fill = color_fill,
      id.long.var = id,
      point.args = rlang::list2(
        alpha = 0.3,
        color = color_fill,
        size = 4
      ),
      line.args = rlang::list2(
        alpha = 0.2,
        color = color_fill,
        linewidth = 1
      ),
      line.args.pos = rlang::list2(position = position_jitter(
        width = .04,
        height = 0,
        seed = 42
      )),
      point.args.pos = rlang::list2(position = position_jitter(
        width = .04,
        height = 0,
        seed = 42
      ))
    ) +
    stat_summary(
      aes(group = 1),
      fun = "mean",
      geom = "line",
      size = 1,
      color = color_stat
    ) +
    stat_summary(
      aes(group = .data[[x]]),
      fun = "mean",
      geom = "point",
      size = 3,
      color = color_stat
    ) +
    stat_summary(
      aes(group = .data[[x]]),
      fun.data = "mean_sdl",
      geom = "errorbar",
      fun.args = list(mult = 1),
      width = 0.05,
      linewidth = 0.7,
      color = color_stat
    ) +
    labs(title = "Marginal distributions",
         x = labs_x,
         y = labs_y) +
    theme(legend.position = "none")

  # Return plot
  p

}
