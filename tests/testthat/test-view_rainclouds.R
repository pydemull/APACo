test_that("The rainclouds are correctly generated", {


  # Prepare data
  new_iris <-
    iris |>
    dplyr::filter(Species != "versicolor") |>
    dplyr::mutate(id = as.factor(rep(1:50, 2)),
                  Species = factor(Species, levels = c("setosa", "virginica"))) |>
    dplyr::select(id, Species, Sepal.Length)

  # Get results
  p <-
    view_rainclouds(
      data = new_iris,
      id = "id",
      x = "Species",
      y = "Sepal.Length",
      rain_side = "f1x1",
      color_fill = NULL,
      color_stat = NULL
    )

  # Do test
  expect_s3_class(p, "ggplot")

})
