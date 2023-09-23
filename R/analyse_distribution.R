#' Plot distribution
#'
#' Function to graphically analyse the distribution of a continuous variable
#'     using a raincloud plot and a qq-plot
#'
#' @param data A data frame.
#' @param var A character value to indicate the name of a numeric variable to be plotted.
#'
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @import patchwork
#' @import Hmisc
#'

analyse_distribution <- function(data,
                                 var) {
  # Raincloud plot
  raincloud <-
    ggplot(data = data, aes(x = 0, y = .data[[var]])) +
    ggrain::geom_rain(
      point.args = rlang::list2(alpha = 0.4),
      point.args.pos = rlang::list2(position = position_jitter(
        width = .04,
        height = 0,
        seed = 42
      ))
    ) +
    stat_summary(
      fun = "mean",
      geom = "point",
      color = "red",
      size = 3
    ) +
    coord_flip() +
    labs(x = "") +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

  # Q-Q plot
  qqplot <-
    ggplot(data = data, aes(sample = .data[[var]])) +
    stat_qq() +
    stat_qq_line()

  # Combine plots
  p <- raincloud / qqplot

  # Return final plot
  p

}
