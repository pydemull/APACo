#' Get a set of figures and tables for analysing the change in the variable of interest
#'
#' @param data A data frame.
#' @param id A character value to indicate the name of the variable designating the
#'     identities of the observations.
#' @param x A character value to indicate the name of a factor variable (2 levels) to be plotted on the X axis.
#' @param y A character value to indicate the name of a numeric variable to be plotted on the Y axis.
#' @param rain_side A character value to indicate how you want the rainclouds displayed,
#'     right ("r"), left ("l") or flanking ("f"), for a 1-by-1 flanking raincloud use ("f1x1")
#'     and for a 2-by-2 use ("f2x2").
#' @param color_fill A character value to set the filling color.
#' @param color_stat A character value to set the color of the statistical summaries.
#' @param nudge_y A numeric value to vertically move the labels of the graphic showing the shift function.
#' @param labs_1x A character string to name the X axis of the 1rst graphic (marginal distributions).
#' @param labs_1y A character string to name the Y axis of the 1rst graphic (marginal distributions).
#' @param labs_2x A character string to name the X axis of the 2nd graphic (pairwise differences).
#' @param labs_2y A character string to name the Y axis of the 2nd graphic (pairwise differences).
#' @param labs_3x A character string to name the X axis of the 3rd graphic (bivariate relationship).
#' @param labs_3y A character string to name the Y axis of the 3rd graphic (bivariate relationship).
#' @param labs_4x A character string to name the X axis of the 4th graphic (quantile shifts).
#' @param labs_4y A character string to name the Y axis of the 4th graphic (quantile shifts).
#' @param labs_5x A character string to name the X axis of the 5th graphic (shift function).
#' @param labs_5y A character string to name the Y axis of the 5th graphic (shift function).
#' @param labs_6x A character string to name the X axis of the 6th graphic (difference asymmetry function).
#' @param labs_6y A character string to name the Y axis of the 6th graphic (difference asymmetry function).
#'
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @import Hmisc
#' @import patchwork
#' @importFrom stats p.adjust

analyse_change <- function(data,
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
                           labs_6y = NULL) {
  # Set the theme for the graphics
  theme_set(theme_bw())

  # Set colors
  if (is.null(color_fill))
    color_fill <- "grey50"
  if (is.null(color_stat))
    color_stat <- "black"

  # Set labels
  if (is.null(labs_1x))
    labs_1x <- x
  if (is.null(labs_1y))
    labs_1y <- y
  if (is.null(labs_2x))
    labs_2x <- ""
  if (is.null(labs_2y))
    labs_2y <- y
  if (is.null(labs_3x))
    labs_3x <- levels((data |> dplyr::pull(x)))[[1]]
  if (is.null(labs_3y))
    labs_3y <- levels((data |> dplyr::pull(x)))[[2]]
  if (is.null(labs_4x))
    labs_4x <- x
  if (is.null(labs_4y))
    labs_4y <- y
  if (is.null(labs_5x))
    labs_5x <- "Quantile of group 1"
  if (is.null(labs_5y))
    labs_5y <- "Quantile difference (group 2 - group 1)"
  if (is.null(labs_6x))
    labs_6x <- "Quantile"
  if (is.null(labs_6y))
    labs_6y <- "Quantile sum = q + 1-q"

  # Set the vertical move of the labels of the graphic relating to the shift function
  if (is.null(nudge_y))
    nudge_y <- 0

  # Make a raincloud plot to visualize individual changes
  p1 <-
    suppressWarnings(
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
          )),
          boxplot.args = rlang::list2(
            width = 0.05,
            fill = color_fill,
            outlier.alpha = 0
          )
        ) +
        stat_summary(
          aes(group = 1),
          fun = "mean",
          geom = "line",
          linewidth = 1,
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
             x = labs_1x,
             y = labs_1y) +
        theme(legend.position = "none")
    )

  # Make a raincloud plot to visualize pairwise differences

  ## Get the levels of interest
  level1 <- levels((data |> dplyr::pull(x)))[[1]]
  level2 <- levels((data |> dplyr::pull(x)))[[2]]

  ## Rearrange data (a wide format is needed to compute the differences)
  data2 <-
    data |>
    tidyr::pivot_wider(names_from = {{ x }}, values_from = {{ y }}) |>
    dplyr::mutate(diff = .data[[level2]] - .data[[level1]])

  ## Make plot
  p2 <-
    ggplot(data = data2, aes(x = "", y = diff)) +
    ggrain::geom_rain(
      fill = color_fill,
      point.args = rlang::list2(
        alpha = 0.3,
        color = color_fill,
        size = 4
      ),
      point.args.pos = rlang::list2(position = position_jitter(
        width = .04,
        height = 0,
        seed = 42
      ))
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    stat_summary(
      fun = "mean",
      geom = "point",
      size = 3,
      color = color_stat
    ) +
    stat_summary(
      fun.data = "mean_sdl",
      geom = "errorbar",
      fun.args = list(mult = 1),
      width = 0.05,
      linewidth = 0.7,
      color = color_stat
    ) +
    labs(title = "Pairwise differences",
         x = labs_2x,
         y = labs_2y) +
    coord_flip() +
    theme(axis.ticks.y = element_line(size = 0))

  # Make a scatter plot
  p3 <-
    ggplot(data = data2, aes(x = .data[[level1]], y = .data[[level2]])) +
    geom_abline(slope = 1, intercept = 0) +
    geom_point(
      alpha = 0.3,
      color = color_fill,
      position = position_jitter(
        width = 0.03,
        height = 0.03,
        seed = 123
      ),
      size = 4
    ) +
    labs(title = "Bivariate relationship",
         x = labs_3x,
         y = labs_3y)

  # Make a plot showing the shift function

  ##  Compute shift function

  ### Note from G. Rousselet : "The confidence intervals are not corrected for multiple
  ### comparisons, the p values are." (`rogme::shiftdhd_pbci` documentation)

  set.seed(123)
  sf <- rogme::shiftdhd_pbci(
    # The levels of the factor variable x have to be rearranged to have the expected sign when computing the
    # difference between the quantiles of the two marginal distributions considered
    data |> dplyr::mutate({
      {
        x
      }
    } := forcats::fct_relevel(.data[[x]], level2, level1)),
    formula = as.formula(paste(y, x, sep = "~")),
    nboot = 200
  )
  set.seed(NULL)

  ##  Update the shift function table to allow its future use in a graphic with customized colors
  sf <-
    sf[[1]] |>
    dplyr::mutate(
      diff_sign = ifelse(difference >= 0, "positive", "negative"),
      alpha = dplyr::case_when(
        q == 0.5           ~ 1,
        q %in% c(0.4, 0.6) ~ 0.9,
        q > 0.2 &
          q < 0.4  ~ 0.8,
        # using q == 0.3 does not work for unclear reasons
        q > 0.6 &
          q < 0.8  ~ 0.8,
        # using q == 0.7 does not work for unclear reasons
        q %in% c(0.2, 0.8) ~ 0.7,
        q %in% c(0.1, 0.9) ~ 0.6
      )
    )

  ## Add p-values adjusted using the Benjamini-Hochbergs False Discovery Rate method
  sf$adj_p_value_bh <- round(p.adjust(sf$p_value, method = "BH"), 2)

  ##  Make plot
  p5 <-
    ggplot(data = sf, aes(x = .data[[names(sf)[3]]])) + # level1 is used for the X axis
    geom_errorbar(aes(
      ymin = ci_lower,
      ymax = ci_upper,
      color = diff_sign,
      alpha = alpha
    ),
    width = 0) +
    geom_line(aes(group = 1, y = difference),
              color = "grey80",
              linewidth = 2) +
    geom_hline(aes(yintercept = 0),
               linetype = "dashed",
               color = "grey50") +
    geom_vline(aes(xintercept = sf[q == 0.5, names(sf)[3]]),
               linetype = "dashed",
               color = "grey50") +
    geom_point(
      aes(y = difference),
      fill = "white",
      shape = 21,
      size = 6
    ) +
    geom_point(
      aes(y = difference,
          fill = diff_sign,
          alpha = alpha),
      shape = 21,
      size = 6
    ) +
    geom_point(
      aes(y = difference),
      shape = 1,
      size = 5,
      color = ifelse(sf$adj_p_value_bh <= 0.05, "red", "black"),
      stroke = ifelse(sf$adj_p_value_bh <= 0.05, 2, 0)
    ) +
    ggrepel::geom_label_repel(
      aes(
        y = ifelse(diff_sign == "positive", ci_upper, ci_lower),
        label = round(difference, 2),
        fill = diff_sign,
        alpha = alpha
      ),
      color = "white",
      direction = "y",
      force = 0.2,
      nudge_y = ifelse(sf$diff_sign == "positive", nudge_y, -nudge_y),
      fontface = "bold",
      size = 6,
      seed = 123
    ) +
    ggrepel::geom_label_repel(
      aes(
        y = ifelse(diff_sign == "positive", ci_upper, ci_lower),
        label = round(difference, 2),
      ),
      fill = NA,
      color = "white",
      direction = "y",
      force = 0.2,
      nudge_y = ifelse(sf$diff_sign == "positive", nudge_y, -nudge_y),
      fontface = "bold",
      size = 6,
      seed = 123
    ) +
    scale_fill_manual(values = c("grey60", color_fill)) +
    scale_color_manual(values = c("grey60", color_fill)) +
    scale_alpha(range = c(0.5, 1)) +
    labs(
      fill = NULL,
      title = "Shift function",
      x = labs_5x,
      y = labs_5y
    ) +
    theme(legend.position = "none")

  # Make a plot tracking the shift of the quantiles while keeping the raw data

  ## Arrange shift function information
  table_sf <-
    sf |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(c(level2, level1)),
      names_to = "group",
      values_to = "vals"
    )

  ## Make plot
  p4 <-
    ggplot(data = data |>
             dplyr::mutate({{x}} := forcats::fct_relevel(.data[[x]], level2, level1)),
           aes(x = .data[[x]],
               y = .data[[y]])) +
    ggbeeswarm::geom_quasirandom(
      shape = 21,
      size = 4,
      color = "grey10",
      fill = "grey90",
      alpha = 0.3
    ) +
    geom_segment(
      data = table_sf,
      aes(
        x = stage(start = group, after_scale(x - 0.2)),
        xend = stage(start = group, after_scale(x + 0.2)),
        y = vals,
        yend = vals
      ),
      linewidth = c(rep(0.6, 8), rep(1.2, 2), rep(0.6, 8))
    ) +
    geom_line(
      data = table_sf,
      aes(
        x = ifelse(group == level2, 1.2, 1.8),
        y = vals,
        group = q,
        color = diff_sign,
        alpha = alpha
      ),
      linewidth = c(rep(0.6, 8), rep(1.2, 2), rep(0.6, 8))
    ) +
    geom_label(
      data = sf |>
        dplyr::mutate(pos_label = .data[[names(sf)[3]]] + (.data[[names(sf)[2]]] -
                                                             .data[[names(sf)[3]]]) / 2) |>
        dplyr::filter(q == 0.1 | q == 0.9),
      aes(
        x = 1.5,
        y = pos_label,
        label = round(difference, 2),
        fill = diff_sign,
        alpha = alpha
      ),
      color = "white",
      fontface = "bold",
      size = 6
    ) +
    scale_color_manual(values = c("grey60", color_fill)) +
    scale_fill_manual(values = c("grey60", color_fill)) +
    scale_alpha(range = c(0.5, 1)) +
    labs(
      fill = NULL,
      title = "Change in the deciles",
      x = labs_4x,
      y = labs_4y
    ) +
    theme(legend.position = "none") +
    coord_flip()


  # Make a plot showing difference asymmetry function

  ## Compute difference asymmetry function
  set.seed(123)
  daf <-
    (
      rogme::asymdhd(
        data |>
          tidyr::pivot_wider(names_from = {{ x }}, values_from = {{ y }}) |>
          dplyr::mutate(diff = .data[[level2]] - .data[[level1]],
                        gr = as.factor("group1")),
        formula = diff ~ gr,
        nboot = 200
      )
    )[[1]]
  set.seed(NULL)

  ## Add p-values adjusted using the Benjamini-Hochberg False Discovery Rate method
  daf$adj_p_value_bh <-
    round(p.adjust(daf$p.value, method = "BH"), 2)

  ## Make plot
  p6 <-
    ggplot(daf, aes(x = quantile, y = SUM)) +
    geom_errorbar(aes(ymin = ci.low, ymax =  ci.up),  width = 0) +
    geom_line(aes(group = 1),
              color = "grey80",
              linewidth = 2) +
    geom_hline(aes(yintercept = 0),
               linetype = "dashed",
               color = "grey50") +
    geom_point(aes(y = SUM),
               fill = "white",
               shape = 21,
               size = 6) +
    geom_point(
      aes(y = SUM, fill = quantile),
      shape = 21,
      size = 6,
      color = ifelse(daf$adj_p_value_bh <= 0.05, "red", "black"),
      stroke = ifelse(daf$adj_p_value_bh <= 0.05, 2, 0.8)
    ) +
    scale_x_continuous(breaks = seq(0, 0.4, 0.05)) +
    scale_fill_gradient(low = "white", high = "grey35") +
    labs(
      fill = NULL,
      title = "Difference asymmetry function",
      x = labs_6x,
      y = labs_6y
    ) +
    theme(legend.position = "none")



  # Build figure
  layout <- "
  AAAAABBB
  CCCCCDDD
  EEEEEFFF
  "
  p <-
    p1 + p3 + p4 + p2 + p5 + p6 +
    patchwork::plot_annotation(tag_levels = 'A') +
    patchwork::plot_layout(design = layout) & theme(
      plot.title = element_text(size = 20),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 15),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 15),
      plot.tag = element_text(size = 20)
    )

  # Return a list with the figures and the shift/difference asymmetry functions information
  objects <-
    list(
      variable = labs_1y,
      p = p,
      sf = sf |> dplyr::select(-c(diff_sign, alpha, adj_p_value)),
      daf = daf |> dplyr::select(-p_crit)
    )
  return(objects)

}
