
#' Get final EMAPS database
#'
#' @param data 'EMAPS' data frame (from APA & Co project analytical pipeline).
#'
#' @return A data frame.
#' @export
#'
get_DB_EMAPS <- function(data) {
  data |>
    dplyr::rename(MONTH = num_visit) |>
    dplyr::mutate(
      patient = as.factor(patient),
      MONTH = forcats::fct_recode(
        as.factor(MONTH),
        "0" = "1",
        "6" = "2",
        "12" = "3"
      )
    ) |>
    dplyr::group_by(MONTH) |>
    dplyr::mutate(
      INTRINSIC    = (AP_q1  %+% AP_q6  %+%  AP_q11) / 3,
      INTEGRATED   = (AP_q7  %+% AP_q10 %+%  AP_q13) / 3,
      IDENTIFIED   = (AP_q4  %+% AP_q12 %+%  AP_q16) / 3,
      INTROJECTED  = (AP_q3  %+% AP_14  %+%  AP_q18) / 3,
      EXTERNAL     = (AP_q9  %+% AP_q15 %+%  AP_q17) / 3,
      AMOTIVATION  = (AP_q2  %+% AP_q5  %+%  AP_q8)  / 3
    ) |>
    dplyr::select(patient, MONTH, INTRINSIC:AMOTIVATION)
}
