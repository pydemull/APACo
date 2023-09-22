
#' Filtering DB_6MWT: Keep the rows for months 0 and 12, and keep the participants with data
#'     at both 0 and 12 months (APA & Co project)
#'
#' @param data 'DB_6MWT' dataframe (from APA & Co project analysis pipeline).
#'
#' @return A dataframe.
#' @export
#'
get_DB_6MWT_0_12 <- function(data) {
  data |>
    dplyr::filter(MONTH != "6") |>
    dplyr::mutate(MONTH = factor(MONTH, levels = c("0", "12"))) |>
    tidyr::drop_na() |>
    dplyr::group_by(patient) |>
    tidyr::nest() |>
    dplyr::mutate(n_visits = purrr::map_dbl(data, ~ nrow(.x))) |>
    dplyr::filter(n_visits == 2) |>
    dplyr::ungroup() |>
    tidyr::unnest(data)
}

