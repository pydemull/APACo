
#' Get a table of descriptive statistics for all measurements relating to 6MWT, IPAQ-SF, and EMAPS
#'
#' @param data1  'DB_6MWT' dataframe (from APA & Co project analytical pipeline).
#' @param data2  'DB_IPAQ' dataframe (from APA & Co project analytical pipeline).
#' @param data3  'DB_EMAPS' dataframe (from APA & Co project analytical pipeline).
#'
#' @return A reactable object.
#' @export
#'
get_table_all_measurements <- function(data1, data2, data3) {

  # Build the table

  all_desc_stat <-
    data1 |>
    dplyr::left_join(data2 |> dplyr::select(c(patient, MONTH, MET_MIN_WK)), by = c("patient", "MONTH")) |>
    dplyr::left_join(data3, by = c("patient", "MONTH")) |>
    dplyr::rename(
      "Intrinsic motivation" = INTRINSIC,
      "Integrated regulation" = INTEGRATED,
      "Identified regulation" = IDENTIFIED,
      "Introjected regulation" = INTROJECTED,
      "External regulation" = EXTERNAL,
      "Amotivation" = AMOTIVATION
    ) |>
    dplyr::group_by(MONTH) |>
    dplyr::summarise(dplyr::across(
      DIST_M:Amotivation,
      list(
        PARAM_n = ~ sum(!is.na(.x)),
        PARAM_mean = ~ mean(.x, na.rm = TRUE),
        PARAM_sd = ~ sd(.x, na.rm = TRUE),
        PARAM_median = ~ median(.x, na.rm = TRUE),
        PARAM_Q1 =  ~ quantile(., probs = 0.25, na.rm = TRUE),
        PARAM_Q3 =  ~ quantile(., probs = 0.75, na.rm = TRUE)
      )
    )) |>

    tidyr::pivot_longer(
      cols = c(-MONTH),
      names_to = c("Variable", ".value"),
      names_sep = "_PARAM_",
    ) |>
    tidyr::pivot_longer(cols = n:Q3,
                        names_to = "Parameter",
                        values_to = "stat") |>
    tidyr::pivot_wider(names_from = MONTH, values_from = stat) |>
    dplyr::rename("MONTH 0" = "0",
                  "MONTH 6" = "6",
                  "MONTH 12" = "12") |>
    dplyr::mutate(
      Variable = forcats::fct_recode(
        Variable,
        "6MWT distance (m)" = "DIST_M",
        "IPAQ-SF (MET-min) / week" = "MET_MIN_WK"
      ),
      dplyr::across(`MONTH 0`:`MONTH 12`, ~ round(.x, 2))
    )

  # Formating the table for .html format
  all_desc_stat_react <-
    reactable::reactable(
      all_desc_stat,
      columns = list(Variable = reactable::colDef(
        style = reactable::JS(
          "function(rowInfo, colInfo, state) {
        var firstSorted = state.sorted[0]
        // Merge cells if unsorted or sorting by Variable
        if (!firstSorted || firstSorted.id === 'Variable') {
          var prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.row['Variable'] === prevRow['Variable']) {
            return { visibility: 'hidden' }
          }
        }
      }"
        )
      )),
      outlined = TRUE,
      striped = TRUE,
      defaultPageSize = 12
    )
}

