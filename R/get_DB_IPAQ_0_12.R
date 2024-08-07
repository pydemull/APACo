
#' Filtering DB_IPAQ to exclude data at 6 months
#'
#' Keep the rows for months 0 and 12, and keep the participants with data
#'     at both 0 and 12 months.
#'
#' @param data 'DB_IPAQ' data frame (from APA&Co project analytical pipeline).
#'
#' @return A data frame.
#' @export
#'
get_DB_IPAQ_0_12 <- function(data) {
  data |>
    dplyr::filter(MONTH != "6") |>
    dplyr::mutate(MONTH = factor(MONTH, levels = c("0", "12"))) |>
    dplyr::select(patient, MONTH, MET_MIN_WK) |>
    tidyr::drop_na() |>
    dplyr::group_by(patient) |>
    tidyr::nest() |>
    dplyr::mutate(n_visits = purrr::map_dbl(data, ~ nrow(.x))) |>
    dplyr::filter(n_visits == 2) |>
    dplyr::ungroup() |>
    tidyr::unnest(data)
}
