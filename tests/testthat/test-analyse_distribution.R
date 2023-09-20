test_that("The raincloud plot and the q-q plot showing the distribution are correctly generated", {

  # Build plot
  p <-
    analyse_distribution(data = iris,
                         var = "Sepal.Length")

  # Do test
  expect_s3_class(p, "ggplot")

})
