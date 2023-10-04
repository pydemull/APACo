
#' Get the plot showing the results relating to the barriers to physical activity
#'     at 12 months
#'
#' @param data 'BARRIERS_cleaned' data frame (from APA&Co project analytical pipeline).
#'
#' @return A ggplot object.
#' @export
#' @importFrom stats binom.test
#'
get_plot_BARRIERS <- function(data) {

  ggplot2::theme_set(theme_bw())

  data |>
    dplyr::select(patient:isolement_faible_RS) |>
    tidyr::pivot_longer(cols = c(-patient),
                        names_to = "var",
                        values_to = "rep") |>
    dplyr::mutate(
      rep  = as.factor(rep),
      var = forcats::fct_recode(
        var,
        "Unfavourable weather" = "meteo_defavorable",
        "Lack of time" = "manque_temps",
        "Heavy effort / too tired" = "effort_import_fatig",
        "Fear of injury / pain" = "crainte_blessures_douleurs",
        "Lack of interest" = "manque_interet",
        "Difficulty to move" = "deplacements_diff",
        "Too old" = "trop_vieux",
        "Social isolation / weak social network" = "isolement_faible_RS",
        "Too costly" = "cout_trop_eleve"
      )
    ) |>
    dplyr::count(var, rep) |>
    dplyr::filter(rep == 1) |>
    dplyr::mutate(
      n_tot = 77,
      prop_estimate = purrr::map2_dbl(n, n_tot, function(n, n_tot) {
        ((
          binom.test(
            n,
            n_tot,
            0.5,
            alternative = "two.sided",
            conf.level = 0.95
          )
        )$estimate[[1]]) * 100
      }),
      prop_ci_low = purrr::map2_dbl(n, n_tot, function(n, n_tot) {
        ((
          binom.test(
            n,
            n_tot,
            0.5,
            alternative = "two.sided",
            conf.level = 0.95
          )
        )$conf.int[[1]]) * 100
      }),
      prop_ci_up = purrr::map2_dbl(n, n_tot, function(n, n_tot) {
        ((
          binom.test(
            n,
            n_tot,
            0.5,
            alternative = "two.sided",
            conf.level = 0.95
          )
        )$conf.int[[2]]) * 100
      }),
      magnitude = ifelse(prop_estimate > 15, "high", "low")
    ) |>
    ggplot(aes(
      x = forcats::fct_reorder(var, n),
      y = prop_estimate,
      color = magnitude
    )) +
    ggrepel::geom_text_repel(
      aes(label = paste0(
        round(prop_estimate, 1),
        "% [",
        round(prop_ci_low, 1),
        "%; ",
        round(prop_ci_up, 1),
        "%]"
      )),
      hjust = 0,
      nudge_x = 0.3,
      nudge_y = 1.5,
      size = 7,
      direction = "y",
      force = 0.2
    ) +
    geom_segment(
      aes(
        x = forcats::fct_reorder(var, n),
        y = 0,
        xend = forcats::fct_reorder(var, n),
        yend = 60
      ),
      linewidth  = 0.5,
      color = "grey80"
    ) +
    geom_errorbar(
      aes(
        x = forcats::fct_reorder(var, n),
        ymin = prop_ci_low,
        ymax = prop_ci_up
      ),
      width = 0.2,
      linewidth = 1
    ) +
    geom_point(
      shape = 21,
      stroke = 2,
      fill = "grey90",
      size = 4
    ) +
    labs(y = NULL, x = NULL) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_color_manual(values = c("grey30", "grey60")) +
    coord_flip(ylim = c(0, 60)) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 20),
      legend.position = "none",
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
}
