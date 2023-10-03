
#' Get plot for all EMAPS scores data
#'
#' @param data 'DB_EMAPS' data frame (from APA&Co project analytical pipeline).
#'
#' @return A ggplot object.
#' @export
#'
get_plot_EMAPS_all <- function(data){

  ggplot2::theme_set(theme_bw())

  data |>
    dplyr::rename(
      "Intrinsic motivation" = INTRINSIC,
      "Integrated regulation" = INTEGRATED,
      "Identified regulation" = IDENTIFIED,
      "Introjected regulation" = INTROJECTED,
      "External regulation" = EXTERNAL,
      "Amotivation" = AMOTIVATION
    ) |>
    tidyr::pivot_longer(
      cols = -c(patient, MONTH),
      names_to = "type_motiv",
      values_to = "val"
    ) |>
    dplyr::mutate(dplyr::across(type_motiv, \(x) factor(
      x,
      levels = c(
        "Intrinsic motivation",
        "Integrated regulation",
        "Identified regulation",
        "Introjected regulation",
        "External regulation",
        "Amotivation"
      )
    ))) |>
    ggplot(aes(x = MONTH, y = val, fill = type_motiv)) +
    ggrain::geom_rain(
      id.long.var = "patient",
      cov = "type_motiv",
      point.args = rlang::list2(alpha = 0.3, size = 3),
      line.args = rlang::list2(alpha = 0, linewidth = 0),
      point.args.pos = rlang::list2(position = position_jitter(
        width = .04,
        height = 0,
        seed = 42
      ))
    ) +
    geom_line(
      aes(group = patient, color = type_motiv),
      alpha = 0.2,
      position = position_jitter(
        width = .04,
        height = 0,
        seed = 42
      )
    ) +
    stat_summary(
      aes(group = 1),
      fun = "mean",
      geom = "line",
      linewidth = 0.5,
      color = "black"
    ) +
    stat_summary(
      aes(group = MONTH),
      fun = "mean",
      geom = "point",
      size = 2,
      color = "black"
    ) +
    stat_summary(
      aes(group = MONTH),
      fun.data = "mean_sdl",
      geom = "errorbar",
      fun.args = list(mult = 1),
      width = 0.05,
      linewidth = 0.5,
      color = "black"
    ) +
    scale_y_continuous(breaks = seq(1, 7, 1)) +
    scale_fill_manual(values = c(
      "chartreuse4",
      "chartreuse4",
      "chartreuse4",
      "grey50",
      "#FAC090",
      "#E46C0A"
    )) +
    scale_color_manual(values = c(
      "chartreuse4",
      "chartreuse4",
      "chartreuse4",
      "grey50",
      "#FAC090",
      "#E46C0A"
    )) +
    labs(x = "Months post-program",
         y = "Score") +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      strip.text.x = element_text(size = 12)
    ) +
    facet_wrap(. ~ type_motiv)
}
