
#' Get final IPAQ database
#'
#' @param data 'IPAQ' dataframe (from APA & Co project analytical pipeline).
#'
#' @return A dataframe.
#' @export
#'
get_DB_IPAQ <- function(data) {
  data |>
    dplyr::mutate(
      patient = as.factor(patient),
      total_hours_heavy = ifelse(
        is.na(total_hours_heavy) |
          total_hours_heavy == 999,
        0,
        total_hours_heavy
      ),
      total_minutes_heavy = ifelse(
        is.na(total_minutes_heavy) |
          total_minutes_heavy == 999,
        0,
        total_minutes_heavy
      ),
      total_hours_moderate = ifelse(
        is.na(total_hours_moderate) |
          total_hours_moderate == 999,
        0,
        total_hours_moderate
      ),
      total_minutes_moderate = ifelse(
        is.na(total_minutes_moderate) |
          total_minutes_moderate == 999,
        0,
        total_minutes_moderate
      ),
      bouts_walk_7days = ifelse(
        is.na(bouts_walk_7days) |
          bouts_walk_7days == 9999,
        0,
        bouts_walk_7days
      ),

      MINUTES_VPA_WK = total_hours_heavy * 60 + total_minutes_heavy,
      MINUTES_MPA_WK = total_hours_moderate * 60 + total_minutes_moderate,
      MINUTES_WALK_WK = bouts_walk_7days * 10,
      MINUTES_TOT_WK = MINUTES_VPA_WK + MINUTES_MPA_WK + MINUTES_WALK_WK,
      MET_MIN_WK = MINUTES_VPA_WK * 8 + MINUTES_MPA_WK * 4 + MINUTES_WALK_WK * 3.3,

      MONTH = forcats::fct_recode(
        as.factor(num_visit),
        "0" = "1",
        "6" = "2",
        "12" = "3"
      )
    )
}
