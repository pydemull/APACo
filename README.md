
<!-- README.md is generated from README.Rmd. Please edit that file -->

# APACo

<!-- badges: start -->
<!-- badges: end -->

## Reproduce the analytical pipeline of the APA&Co project

The analytical pipeline implemented to the get the results of the
project APA&Co is based on packages that have been designed to make all
analyses reproducible with R programming language: the `{renv}` package,
and the `{targets}` package. `{renv}` will allow you to restore all the
exact package dependencies that have been used to perform analyses and
to get the final materials of the project. `{target}` will provide you
the structure to automatically drive the analyses. Please follow the
steps below if you want to reproduce the analyses of the project.

- Step 1: Install R (version \>= 4.1.0) and RStudio on your machine.
- Step 2: Make sure you are on the [‘pipeline’ branch of the APACo
  repository](https://github.com/pydemull/APACo/tree/pipeline) and click
  on the green button called `Code`. Then click on `Download ZIP`.
- Step 3: Open the zipped folder on your machine and double-click on the
  `APACo.Rproj` file to open the project in RStudio.
- Step 4: Restore the package dependencies of the project with `{renv}`
  using the following command line in the Console:

``` r
renv::restore()
```

- Step 5: Run the analytical pipeline with `{targets}` using the
  following command line in the Console:

``` r
targets::tar_make()
```

This last step will produce all the objects relating to the analytical
process. It also will generate the materials (a .html report with the
main results, .tiff figures, and .html file-based supplementary
materials) that are deemed to be used for a scientific publication. Some
materials will take time to be built because they include several high
resolution figures.

The list of the objects created during the analytical process is shown
in the Console after each ‘target’ expression. Once the analytical
pipeline ended, you will can read any created object running
`targets::tar_read(OBJECT_NAME)` in the Console. You also will can load
any object in the global environment running
`targets::tar_load(OBJECT_NAME)` in the Console. You can load all the
created objects in one go running `targets::tar_load_everything()` in
the Console. All the figures and .html files generated during the run of
the analytical pipeline will appear in the `out/` folder placed at the
root of the project.
