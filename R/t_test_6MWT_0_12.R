
#' T-test for comparing means of 6MWT distances at 0 and 12 months
#'
#' @param data 'DB_6MWT_0_12' dataframe (from APA & Co project analytical pipeline).
#'
#' @return An object of class 'htest'.
#' @export
#' @importFrom stats t.test
#'
t_test_6MWT_0_12 <- function(data) {
  t.test(
    formula = DIST_M ~ MONTH,
    data = data |> dplyr::mutate(MONTH = forcats::fct_relevel(MONTH, "12", "0")),
    mu = 0,
    paired = TRUE,
    var.equal = FALSE
  )
}
