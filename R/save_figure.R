
#' Save figure
#'
#' @param filename Cf. ?ragg::agg_tiff.
#' @param ... The object to be plotted.
#' @param scaling Cf. ?ragg::agg_tiff.
#' @param height Cf. ?ragg::agg_tiff.
#' @param units Cf. ?ragg::agg_tiff.
#' @param res Cf. ?ragg::agg_tiff.
#' @param width Cf. ?ragg::agg_tiff.
#'
#' @return A character string indicating the path to the saved figure.
#' @export
#' @importFrom grDevices dev.off
#'
save_figure <- function(filename, ..., scaling = 0.4, height = 16, width = 16, units = "cm", res = 400) {

  theme_set(theme_bw())

  ragg::agg_tiff(
    filename = filename,
    scaling = scaling,
    height = height,
    width = width,
    units = units,
    res = res
  )
  plot(...)
  dev.off()

  return(filename)
}
