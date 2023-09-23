
#' Filtering DB_IPAQ
#'
#' Keep the rows for months 6 and 12, and keep the participants with data
#'     at both 6 and 12 months.
#'
#' @param data 'DB_IPAQ' data frame (from APA & Co project analytical pipeline).
#'
#' @return A data frame.
#' @export
#'
get_DB_IPAQ_6_12 <- function(data) {
  data |>
    dplyr::filter(MONTH != "0") |>
    dplyr::mutate(MONTH = factor(MONTH, levels = c("6", "12"))) |>
    dplyr::select(patient, MONTH, MET_MIN_WK) |>
    tidyr::drop_na() |>
    dplyr::group_by(patient) |>
    tidyr::nest() |>
    dplyr::mutate(n_visits = purrr::map_dbl(data, ~ nrow(.x))) |>
    dplyr::filter(n_visits == 2) |>
    dplyr::ungroup() |>
    tidyr::unnest(data)
}
