test_that("The figure is correctly generated", {


  # Prepare data
  new_iris <-
    iris |>
    dplyr::filter(Species != "versicolor") |>
    dplyr::mutate(id = as.factor(rep(1:50, 2)),
                  Species = factor(Species, levels = c("setosa", "virginica"))) |>
    dplyr::select(id, Species, Sepal.Length)

  # Get results
  p <-
      analyse_change(
        data = new_iris,
        id = "id",
        x = "Species",
        y = "Sepal.Length",
        rain_side = "f1x1",
        nudge_y = NULL,
        color_fill = NULL,
        color_stat = NULL,
        labs_1x = NULL,
        labs_1y = NULL,
        labs_2x = NULL,
        labs_2y = NULL,
        labs_3x = NULL,
        labs_3y = NULL,
        labs_4x = NULL,
        labs_4y = NULL,
        labs_5x = NULL,
        labs_5y = NULL,
        labs_6x = NULL,
        labs_6y = NULL
      )

  # Do test
  testthat::expect_s3_class(p$p, "ggplot")

})
