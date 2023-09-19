testthat::test_that("Sum across columns works with NAs", {

  # Define basic dataframe
  df <-
    data.frame(
    x = c(1, 2, 3),
    y = c(1, NA, 3),
    z = c(NA, 2, 3)
    )

  # Compute truth
  ref_sum <- c(2, 4, 9)

  # Compute sum
  test_sum <-
    df |>
    dplyr::mutate(sum = x %+% y %+% z) |>
    dplyr::pull(sum)

  # Do test
  testthat::expect_equal(test_sum, ref_sum)

})
