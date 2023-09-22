
#' Get final 6MWT database (APA & Co project)
#'
#' @param data1 'INCLUSION_cleaned' dataframe (from APA & Co project analysis pipeline).
#' @param data2 'VISIT_6M_cleaned' dataframe (from APA & Co project analysis pipeline).
#' @param data3 'VISIT_12M_cleaned' dataframe (from APA & Co project analysis pipeline).
#'
#' @return A dataframe.
#' @export
#'
get_DB_6MWT <- function(data1, data2, data3) {
  data1 |>
    dplyr::left_join(data2, by = "patient") |>
    dplyr::left_join(data3, by = "patient") |>
    dplyr::select(patient, DIST_6WT_M0, DIST_6WT_M6, DIST_6WT_M12) |>
    dplyr::rename(MONTH_0 = DIST_6WT_M0,
                  MONTH_6 = DIST_6WT_M6,
                  MONTH_12 = DIST_6WT_M12) |>
    tidyr::pivot_longer(
      cols = c(MONTH_0, MONTH_6, MONTH_12),
      names_to = "MONTH",
      values_to = "DIST_M"
    ) |>
    dplyr::mutate(
      MONTH = forcats::fct_relevel(MONTH, "MONTH_0", "MONTH_6", "MONTH_12"),
      MONTH = forcats::fct_recode(
        MONTH,
        "0" = "MONTH_0",
        "6" = "MONTH_6",
        "12" = "MONTH_12"
      )
    )
}
