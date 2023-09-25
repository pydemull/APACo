
<!-- README.md is generated from README.Rmd. Please edit that file -->

# APACo

<!-- badges: start -->
<!-- badges: end -->

## Reproduce the analytical pipeline of the APA&Co project

The reproduction of the analytical pipeline implemented to the get the
results of the APA&Co project is based on packages that have been
designed to make all analyses reproducible with R programming language:
the `{renv}` package, and the `{targets}` package. `{renv}` will allow
you to restore all the exact package dependencies that have been used to
perform analyses and to get the final materials of the project.
`{target}` will provide you the structure to automatically drive the
analyses. Please follow the steps below if you want to reproduce the
analyses of the project.

- Step 1: [Install R (version \>= 4.1.0)](https://cran.rstudio.com/),
  [RStudio](https://posit.co/download/rstudio-desktop/), and the [latest
  version of Rtools (if your are a Windows
  user)](https://cran.r-project.org/bin/windows/Rtools/) on your
  machine.
- Step 2: Go to the [‘pipeline’ branch of the APACo
  repository](https://github.com/pydemull/APACo/tree/pipeline) and click
  on the green button called `Code`. Then click on `Download ZIP`.
- Step 3: Unzip the zipped folder on your machine, open it, and
  double-click on the `APACo.Rproj` file to open the project in RStudio.
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

## Verify the code

The main interest of running the analytical pipeline is to allow an
exact reproduction of the analysis process. However, you will not see
what are the computations actually performed behind the scene. To have
more information about it, you can open the `_targets.R` file placed at
the root of the project. This file essentially includes a list of
`targets::tar_target()` functions, with the name of the object created
as first argument, and the operations performed to get that object as
second argument. A quick way of looking at the code could be to copy the
name of a function of interest and paste it in the Console. Another way,
which will provide you a more confortable view of the code, is to go to
the [Github repository of the {APACo}
package](https://github.com/pydemull/APACo). The files placed in the
`R/` folder contain the code of each of the functions developped for the
project.
