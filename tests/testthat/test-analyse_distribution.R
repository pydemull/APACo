testthat::test_that("The plot is correctly generated", {

  # Build plot
  p <-
    analyse_distribution(
    data = iris,
    var = "Sepal.Length"
  )

  # Do test
  testthat::expect_s3_class(p, "ggplot")

})
