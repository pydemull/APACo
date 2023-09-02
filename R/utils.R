
#' Functions to easily reproduce the results and graphics
#'
#' @param data A dataframe object.
#' @param var If required, a character string to indicate the name of the variable to be plotted.
#' @param id If required, a character string to indicate the name of the variable designating the
#'     identities of the observations.
#' @param x If required, a character string to indicate the name of the variable to be plotted on the X axis.
#' @param y If required, a character string to indicate the name of the variable to be plotted on the Y axis.
#' @param rain_side If required, 	a character string to indicate how you want the rainclouds displayed,
#'     right ("r"), left ("l") or flanking ("f"), for a 1-by-1 flanking raincloud use ("f1x1") and for a 2-by-2 use ("f2x2").
#' @param color_fill If required, a character string to indicate the filling color.
#' @param color_stat If required, a character string to indicate the color of the statistics summaries.
#' @param labs_1x If required, a character string to name the X axis.
#' @param labs_1y If required, a character string to name the Y axis.
#' @param nudge_y If required, a numeric value to move vertically the label of the graphics (in units of the
#'     Y axis)

#'
#' @export
#' @import ggplot2
#' @noRd

# ------------------------------
# Set the theme for the graphics ----
# ------------------------------
theme_set(theme_bw())


# -------------------------------------------------------------------------
# Function to graphically analyse the distribution of a continuous variable ----
# (raincloud plot and a qq-plot)
# -------------------------------------------------------------------------

analyse_distribution <- function(
    data,
    var,
    id
) {

# Raincloud plot
raincloud <-
  ggplot(data = data, aes(x = 0, y = .data[[var]])) +
  ggrain::geom_rain(
    id.long.var = id,
    point.args = rlang::list2(alpha = 0.4),
    point.args.pos = rlang::list2(position = position_jitter(width = .04, height = 0, seed = 42))
  ) +
  stat_summary(fun = "mean", geom = "point",  color = "red", size = 3) +
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


# ---------------------------------------------------------------------
# Function to make a polished raincloud plot for visualising the change ----
# ---------------------------------------------------------------------

view_rainclouds <- function(
  data,
  id,
  x,
  y,
  rain_side = NULL,
  color_fill = NULL,
  color_stat = NULL,
  labs_1x = NULL,
  labs_1y = NULL
) {

# Set colors
if(is.null(color_fill)) color_fill <- "grey50"
if(is.null(color_stat)) color_stat <- "black"

# Set labels
if(is.null(labs_1x)) labs_1x <- x
if(is.null(labs_1y)) labs_1y <- y


# Make plot
p <-
  ggplot(data = data, aes(x = .data[[x]], y = .data[[y]])) +
  ggrain::geom_rain(
    rain.side = rain_side,
    fill = color_fill,
    id.long.var = id,
    point.args = rlang::list2(alpha = 0.3, color = color_fill, size = 4),
    line.args = rlang::list2(alpha = 0.2, color = color_fill, size = 1),
    line.args.pos = rlang::list2(position = position_jitter(width = .04, height = 0, seed = 42)),
    point.args.pos = rlang::list2(position = position_jitter(width = .04, height = 0, seed = 42))
  ) +
  stat_summary(aes(group = 1), fun = "mean", geom = "line", size = 1, color = color_stat) +
  stat_summary(aes(group = .data[[x]]), fun = "mean", geom = "point", size = 3, color = color_stat) +
  stat_summary(aes(group = .data[[x]]), fun.data = "mean_sdl", geom = "errorbar", fun.args = list(mult = 1), width = 0.05, linewidth = 0.7, color = color_stat) +
  labs(
    title = "Data at each time of measurement",
    x = labs_1x,
    y = labs_1y
  ) +
  theme(legend.position = "none")

# Return plot
p

}


# -----------------------------------------------------------------------------------
# Function to build a complex figure analysing the change in the variable of interest ----
# -----------------------------------------------------------------------------------

analyse_change <- function(
  data,
  id,
  x,
  y,
  rain_side = NULL,
  nudge_y = NULL,
  color_fill = NULL,
  color_stat = NULL,
  labs_1x = NULL,
  labs_1y = NULL,
  labs_2x = NULL,
  labs_2y = NULL,
  labs_3x = NULL,
  labs_3y = NULL,
  labs_4x = NULL,
  labs_4y = NULL,
  labs_5x = NULL,
  labs_5y = NULL,
  labs_6x = NULL,
  labs_6y = NULL
){

# Set colors
if(is.null(color_fill)) color_fill <- "grey50"
if(is.null(color_stat)) color_stat <- "black"

# Set labels
if(is.null(labs_1x)) labs_1x <- x
if(is.null(labs_1y)) labs_1y <- y
if(is.null(labs_2x)) labs_1x <- x
if(is.null(labs_2y)) labs_1y <- y
if(is.null(labs_3x)) labs_1x <- x
if(is.null(labs_3y)) labs_1y <- y
if(is.null(labs_4x)) labs_1x <- x
if(is.null(labs_4y)) labs_1y <- y
if(is.null(labs_5x)) labs_1x <- x
if(is.null(labs_5y)) labs_1y <- y
if(is.null(labs_6x)) labs_1x <- x
if(is.null(labs_6y)) labs_1y <- y

# Set vertical move of the labels
if(is.null(nudge_y)) nudge_y <- 0

# Make a raincloud plot to visualize individual changes
p1 <-
  ggplot(data = data, aes(x = .data[[x]], y = .data[[y]])) +
  ggrain::geom_rain(
    rain.side = rain_side,
    fill = color_fill,
    id.long.var = id,
    point.args = rlang::list2(alpha = 0.3, color = color_fill, size = 4),
    line.args = rlang::list2(alpha = 0.2, color = color_fill, size = 1),
    line.args.pos = rlang::list2(position = position_jitter(width = .04, height = 0, seed = 42)),
    point.args.pos = rlang::list2(position = position_jitter(width = .04, height = 0, seed = 42))
  ) +
  stat_summary(aes(group = 1), fun = "mean", geom = "line", size = 1, color = color_stat) +
  stat_summary(aes(group = .data[[x]]), fun = "mean", geom = "point", size = 3, color = color_stat) +
  stat_summary(aes(group = .data[[x]]), fun.data = "mean_sdl", geom = "errorbar", fun.args = list(mult = 1), width = 0.05, linewidth = 0.7, color = color_stat) +
  labs(
    title = "Data at each time of measurement",
    x = labs_1x,
    y = labs_1y
  ) +
  theme(legend.position = "none")


# Make a raincloud plot to visualize pairwise differences

  # Get levels of interest
  level1 <- levels((data |> dplyr::arrange(id, x) |> dplyr::pull(x)))[[1]]
  level2 <- levels((data |> dplyr::arrange(id, x) |> dplyr::pull(x)))[[2]]

  # Rearrange data
  data2 <-
    data |>
    tidyr::pivot_wider(names_from = .data[[x]], values_from = .data[[y]]) |>
    dplyr::mutate(diff = .data[[level2]] - .data[[level1]])

  # Make plot
  p2 <-
  ggplot(data = data2, aes(x = "", y = diff)) +
  ggrain::geom_rain(
    fill = color_fill,
    point.args = rlang::list2(alpha = 0.3, color = color_fill, size = 4),
    point.args.pos = rlang::list2(position = position_jitter(width = .04, height = 0, seed = 42))
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = "mean", geom = "point", size = 3, color = color_stat) +
  stat_summary(fun.data = "mean_sdl", geom = "errorbar", fun.args = list(mult = 1), width = 0.05, linewidth = 0.7, color = color_stat) +
  labs(
    title = "Pairwise differences",
    x = labs_2x,
    y = labs_2y
  ) +
  theme(axis.ticks.x = element_blank())

# Make a scatter plot
p3 <-
  ggplot(data = data2, aes(x = .data[[level1]], y = .data[[level2]])) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(alpha = 0.3, color = color_fill, size = 4) +
  labs(
    title = "Relationship between the measurements",
    x = labs_3x,
    y = labs_3y
  )

# Make a plot showing the shift of the distribution

  # Compute shift function
  sf <- rogme::shiftdhd_pbci(
    data |> dplyr::mutate({{x}} := forcats::fct_relevel(.data[[x]], level2, level1)),
    formula = as.formula(paste(y, x, sep = "~")),
    nboot = 200
    )

  # Set colors
  sf <-
    sf[[1]] |>
    dplyr::mutate(
      diff_sign = ifelse(difference >= 0, "positive", "negative"),
      alpha = dplyr::case_when(
        q == 0.5           ~ 5,
        q %in% c(0.4, 0.6) ~ 4,
        q > 0.2 & q < 0.4  ~ 3,
        q > 0.6 & q < 0.8  ~ 3,
        q %in% c(0.2, 0.8) ~ 2,
        q %in% c(0.1, 0.9) ~ 1
      )
      )
  # Make plot
  p5 <-
    ggplot(data = sf, aes(x = .data[[names(sf)[2]]])) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = diff_sign, alpha = alpha),  width = 0) +
    geom_line(aes(group = 1, y = difference), color = "grey80", linewidth = 2) +
    geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey50") +
    geom_vline(aes(xintercept = sf[q == 0.5, names(sf)[2]]), linetype = "dashed", color = "grey50") +
    geom_point(aes(y = difference), fill = "white", shape = 21, size = 6) +
    geom_point(aes(y = difference, fill = diff_sign, alpha = alpha), shape = 21, size = 6) +
    ggrepel::geom_label_repel(aes(
      y = ifelse(diff_sign == "positive", ci_upper, ci_lower),
      label = round(difference, 2),
      fill = diff_sign,
      alpha = alpha,
      ),
      direction = "y",
      nudge_y = ifelse(sf$diff_sign == "positive", nudge_y, -nudge_y),
      color = "white", fontface = "bold", size = 4) +
    scale_fill_manual(values = c("grey60", "#0089C6")) +
    scale_color_manual(values = c("grey60", "#0089C6")) +
    scale_alpha(range = c(0.6, 1)) +
    labs(
      fill = NULL,
      title = "Shift function",
      x = labs_5x,
      y = labs_5y
      ) +
    theme(legend.position = "none")


# Build figure
p <- (p1) / (p2 | p5) / (p3)

# Return figure
p

}
